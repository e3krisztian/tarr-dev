class UndefinedLabelError(Exception):
    pass


class MissingEndIfError(Exception):
    pass


class MultipleElseError(Exception):
    pass


class ElIfAfterElseError(Exception):
    pass


class Compilable(object):

    def compile(self, compiler):
        pass


class InstructionBase(Compilable):

    index = None

    # run time
    def run(self, runner, state):
        return state

    def next_instruction(self, exit_status):
        return None

    # compile time
    def set_next_instruction(self, instruction):
        pass

    def compile(self, compiler):
        compiler.add_instruction(self.clone())

    def clone(self):
        return self.__class__()

    # visitor
    def accept(self, visitor):
        pass


class Instruction(InstructionBase):

    _next_instruction = None

    def next_instruction(self, exit_status):
        return self._next_instruction

    def set_next_instruction(self, instruction):
        self._next_instruction = instruction

    def accept(self, visitor):
        visitor.visit_instruction(self)


class Return(InstructionBase):

    return_value = None

    def __init__(self, return_value=True):
        self.return_value = bool(return_value)

    def next_instruction(self, exit_status):
        return None

    def run(self, runner, state):
        if self.return_value is not None:
            runner.set_exit_status(self.return_value)

        return state

    def compile(self, compiler):
        super(Return, self).compile(compiler)
        compiler.path.close()

    def clone(self):
        return self.__class__(self.return_value)

    def accept(self, visitor):
        visitor.visit_return(self)


RETURN_TRUE = Return(return_value=True)
RETURN_FALSE = Return(return_value=False)


class BranchingInstruction(InstructionBase):

    instruction_on_yes = None
    instruction_on_no = None

    def next_instruction(self, exit_status):
        if exit_status:
            return self.instruction_on_yes
        return self.instruction_on_no

    def set_next_instruction(self, instruction):
        if self.instruction_on_yes is None:
            self.instruction_on_yes = instruction
        if self.instruction_on_no is None:
            self.instruction_on_no = instruction

    def set_instruction_on_yes(self, instruction):
        self.instruction_on_yes = instruction

    def set_instruction_on_no(self, instruction):
        self.instruction_on_no = instruction

    def accept(self, visitor):
        visitor.visit_branch(self)


class Runner(object):

    exit_status = None

    def set_exit_status(self, value):
        self.exit_status = value

    def run_instruction(self, instruction, state):
        return instruction.run(self, state)

    def run(self, start_instruction, state):
        instruction = start_instruction

        while instruction:
            state = self.run_instruction(instruction, state)
            instruction = instruction.next_instruction(self.exit_status)

        return state


class Call(BranchingInstruction):

    label = None
    start_instruction = None

    def __init__(self, label):
        self.label = label

    def run(self, runner, state):
        return runner.run(self.start_instruction, state)

    def compile(self, compiler):
        super(Call, self).compile(compiler)

    def set_start_instruction(self, instruction):
        self.start_instruction = instruction

    def clone(self):
        return self.__class__(self.label)

    def accept(self, visitor):
        visitor.visit_call(self)


class CompileIf(Compilable):

    def __init__(self, branch_instruction):
        self.branch_instruction = branch_instruction

    def compile(self, compiler):
        compiler.compile_instruction(self.branch_instruction)

        if_path, else_path = compiler.path.split(compiler.last_instruction)
        compiler.control_stack.append(
            IfElseControlFrame(compiler.path, if_path, else_path))

        compiler.path = if_path

IF = CompileIf


class CompileIfNot(CompileIf):

    def compile(self, compiler):
        super(CompileIfNot, self).compile(compiler)
        frame = compiler.control_stack.pop()

        # swap if_path and else_path
        frame.if_path, frame.else_path = frame.else_path, frame.if_path

        compiler.path = frame.if_path
        compiler.control_stack.append(frame)

IF_NOT = CompileIfNot


class CompileElIf(Compilable):

    def __init__(self, branch_instruction):
        self.branch_instruction = branch_instruction

    def compile(self, compiler):
        frame = compiler.control_stack.pop()

        if frame.else_used:
            raise ElIfAfterElseError
        if frame.elif_path is not None:
            frame.if_path.join(frame.elif_path)

        compiler.path = frame.else_path
        compiler.compile_instruction(self.branch_instruction)

        frame.elif_path, frame.else_path = frame.else_path.split(
            compiler.last_instruction)
        compiler.path = frame.elif_path

        compiler.control_stack.append(frame)

ELIF = CompileElIf


class CompileElIfNot(CompileElIf):

    def compile(self, compiler):
        super(CompileElIfNot, self).compile(compiler)
        frame = compiler.control_stack.pop()

        # swap elif_path and else_path
        frame.elif_path, frame.else_path = frame.else_path, frame.elif_path
        compiler.path = frame.elif_path

        compiler.control_stack.append(frame)

ELIF_NOT = CompileElIfNot


class CompileElse(Compilable):

    def compile(self, compiler):
        frame = compiler.control_stack.pop()

        if frame.else_used:
            raise MultipleElseError

        compiler.path = frame.else_path
        frame.else_used = True

        compiler.control_stack.append(frame)

ELSE = CompileElse()


class CompileEndIf(Compilable):

    def compile(self, compiler):
        frame = compiler.control_stack.pop()

        frame.main_path.join(frame.if_path)
        if frame.elif_path is not None:
            frame.main_path.join(frame.elif_path)
        frame.main_path.join(frame.else_path)

        compiler.path = frame.main_path

ENDIF = CompileEndIf()


class Appender(object):
    '''Knows how to continue a path

    Continue = how to append a new instruction to
    '''

    def append(self, first_instruction, last_instruction):
        pass


class NoopAppender(Appender):
    pass


class InstructionAppender(Appender):
    '''Appends to previous instruction
    '''

    def __init__(self, instruction):
        self.last_instruction = instruction

    def append(self, first_instruction, last_instruction):
        self.last_instruction.set_next_instruction(first_instruction)
        self.last_instruction = last_instruction


class NewPathAppender(Appender):
    '''Appends to empty path
    '''

    def __init__(self, path):
        self.path = path

    def append(self, first_instruction, last_instruction):
        self.path.set_appender(InstructionAppender(last_instruction))


class YesBranchAppender(Appender):
    '''Appends to `yes` side of a branch instruction
    '''

    def __init__(self, path, branch_instruction):
        self.path = path
        self.branch_instruction = branch_instruction

    def append(self, first_instruction, last_instruction):
        self.branch_instruction.set_instruction_on_yes(first_instruction)
        self.path.set_appender(InstructionAppender(last_instruction))


class NoBranchAppender(Appender):
    '''Appends to `no` side of a branch instruction
    '''

    def __init__(self, path, branch_instruction):
        self.path = path
        self.branch_instruction = branch_instruction

    def append(self, first_instruction, last_instruction):
        self.branch_instruction.set_instruction_on_no(first_instruction)
        self.path.set_appender(InstructionAppender(last_instruction))


class JoinAppender(Appender):

    def __init__(self, path, merged_path):
        self.path = path
        self.orig_appender = path.appender
        self.merged_path = merged_path

    def append(self, first_instruction, last_instruction):
        self.merged_path.append(first_instruction, last_instruction)
        self.orig_appender.append(first_instruction, last_instruction)
        self.path.set_appender(InstructionAppender(last_instruction))


class Path(object):
    '''
    An execution path.

    Instructions can be appended to it and other paths can be joined in.
    Real work happens in appenders, which are changed as needed.
    '''

    def __init__(self, appender=None):
        self.appender = appender or NewPathAppender(self)

    def append(self, first_instruction, last_instruction):
        self.appender.append(first_instruction, last_instruction)

    def split(self, branch_instruction):
        self.close()
        yes_path = Path()
        yes_path.set_appender(
            YesBranchAppender(yes_path, branch_instruction))
        no_path = Path()
        no_path.set_appender(
            NoBranchAppender(no_path, branch_instruction))
        return yes_path, no_path

    def join(self, path):
        self.appender = JoinAppender(self, path)

    def set_appender(self, appender):
        self.appender = appender

    def close(self):
        self.set_appender(NoopAppender())


class IfElseControlFrame(object):

    '''
    Records paths for IF, IF_NOT, ELIF, ELIF_NOT, ELSE, ENDIF

    * main_path: stored when entering IF or IF_NOT, restored on ENDIF,
                 not used in between
    * if_path:   the first conditional path to define
    * elif_path: optional, used by ELIF to keep the current conditional path
    * else_path: ELSE branch goes here

    ENDIF merges if_path, elif_path, else_path back to main_path,
    before restoring
    '''

    def __init__(self, main_path, if_path, else_path):
        self.main_path = main_path
        self.if_path = if_path
        self.elif_path = None
        self.else_path = else_path
        self.else_used = False


class Compiler(object):

    instructions = None
    control_stack = None
    path = None

    @property
    def last_instruction(self):
        return self.instructions[-1]

    def __init__(self, program):
        '''
        Create a compiler for the [end-]user defined `program`.
        '''
        self.program = program
        self.control_stack = []
        self.path = Path()
        self.instructions = list()

    def compile(self, sub_program_name):
        try:
            sub_program = self.program[sub_program_name]
        except KeyError:
            raise UndefinedLabelError(sub_program_name)

        for instruction in sub_program:
            self.compile_instruction(instruction)

        if self.control_stack:
            raise MissingEndIfError

        self.set_indices()

    def set_indices(self):
        # indices = labels to match instructions with run-time statistics
        for i, instruction in enumerate(self.instructions):
            instruction.index = i

    def compile_instruction(self, instruction):
        if isinstance(instruction, basestring):
            self.compile_subprogram(instruction)
        else:
            instruction.compile(self)

    def compile_subprogram(self, sub_program_name):
        compiler = Compiler(self.program)
        compiler.compile(sub_program_name)
        self.path.append(compiler.instructions[0], compiler.last_instruction)
        self.instructions.extend(compiler.instructions)
        Call(sub_program_name).compile(self)

    def add_instruction(self, instruction):
        self.path.append(instruction, instruction)
        self.instructions.append(instruction)


class ProgramVisitor(object):

    def enter_subprogram(self, label, instructions):
        pass

    def leave_subprogram(self, label):
        pass

    def visit_call(self, i_call):
        pass

    def visit_return(self, i_return):
        pass

    def visit_instruction(self, instruction):
        pass

    def visit_branch(self, i_branch):
        pass


class Program(object):

    instructions = None
    runner = None

    def __init__(self, program):
        self.compile(program)

    def run(self, state):
        return self.runner.run(self.start_instruction, state)

    def compile(self, program):
        compiler = Compiler(program)
        compiler.compile('main')
        self.init(compiler.instructions)

    def init(self, instructions):
        self.instructions = instructions
        self.runner = self.make_runner()

    @property
    def start_instruction(self):
        return self.instructions[0]

    def make_runner(self):
        return Runner()

    def sub_programs(self):
        # raise AssertionError('Broken due to new macro implementation')
        label = None
        yield (label, self.instructions[:])

    def accept(self, visitor):
        for (label, instructions) in self.sub_programs():
            visitor.enter_subprogram(label, instructions)
            for i in instructions:
                i.accept(visitor)
            visitor.leave_subprogram(label)
