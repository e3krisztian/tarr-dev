import unittest
import tarr.compiler_base as m
from tarr.compiler_base import (
    Instruction, BranchingInstruction,
    RETURN_TRUE, RETURN_FALSE,
    IF, ELSE, ELIF, ENDIF,
    IF_NOT, ELIF_NOT,
    UndefinedLabelError, MissingEndIfError,
    MultipleElseError, ElIfAfterElseError)


class _Div2(Instruction):

    def run(self, flag, data):
        return super(_Div2, self).run(flag, data / 2)

Div2 = _Div2()


class _IsOdd(BranchingInstruction):

    def run(self, flag, data):
        return super(_IsOdd, self).run(data % 2 == 1, data)

IsOdd = _IsOdd()


class Noop(Instruction):
    pass

Noop = Noop()


class ValueMixin(object):

    def __init__(self, value):
        self.value = value

    def clone(self):
        return self.__class__(self.value)

    @property
    def instruction_name(self):
        return '{}({})'.format(self.__class__.__name__, self.value)


class Eq(ValueMixin, BranchingInstruction):

    def run(self, flag, data):
        return super(Eq, self).run(data == self.value, data)


class Const(ValueMixin, Instruction):

    def run(self, flag, data):
        return super(Const, self).run(flag, self.value)


class Add(ValueMixin, Instruction):

    def run(self, flag, data):
        return super(Add, self).run(flag, data + self.value)


Add1 = Add(1)


class Test_Path(unittest.TestCase):

    def test_NewPathAppender(self):
        # append
        i1 = m.Instruction()
        i2 = m.Instruction()

        path = m.Path()
        path.append(i1, i1)
        path.append(i2, i2)

        self.assertEqual(i2, i1.next_instruction)

    def test_InstructionAppender(self):
        # append
        i0 = m.Instruction()
        i1 = m.Instruction()
        i2 = m.Instruction()

        path = m.Path(m.InstructionAppender(i0))
        path.append(i1, i1)
        path.append(i2, i2)

        self.assertEqual(i1, i0.next_instruction)

    def test_join(self):
        # p1 p1i1   p1i2
        # p2 p2i1 /
        p1i1 = m.Instruction()
        p1i2 = m.Instruction()
        p2i1 = m.Instruction()

        path1 = m.Path()
        path1.append(p1i1, p1i1)
        path2 = m.Path()
        path2.append(p2i1, p2i1)

        path1.join(path2)

        path1.append(p1i2, p1i2)

        self.assertEqual(p1i2, p1i1.next_instruction)
        self.assertEqual(p1i2, p2i1.next_instruction)

    def test_join_a_joined_path(self):
        # p1 p1i1   p1i2
        # p2 p2i1 /
        # p3 p3i1 /
        # or
        # p2.join(p3)
        # p1.join(p2)
        p1i1 = m.Instruction()
        p1i2 = m.Instruction()
        p2i1 = m.Instruction()
        p3i1 = m.Instruction()

        path1 = m.Path()
        path1.append(p1i1, p1i1)
        path2 = m.Path()
        path2.append(p2i1, p2i1)
        path3 = m.Path()
        path3.append(p3i1, p3i1)

        path2.join(path3)
        path1.join(path2)

        path1.append(p1i2, p1i2)

        self.assertEqual(p1i2, p1i1.next_instruction)
        self.assertEqual(p1i2, p2i1.next_instruction)
        self.assertEqual(p1i2, p3i1.next_instruction)

    def test_join_to_a_closed_path(self):
        # p1 RETURN   p1i2
        # p2 p2i1 /
        p2i1 = m.Instruction()
        p1i2 = m.Instruction()

        path1 = m.Path()
        iret = m.Return()
        path1.append(iret, iret)
        path1.close()
        path2 = m.Path()
        path2.append(p2i1, p2i1)

        path1.join(path2)

        path1.append(p1i2, p1i2)

        self.assertEqual(p1i2, p2i1.next_instruction)

    def test_YesBranchAppender(self):
        bi = m.BranchingInstruction()
        i1 = m.Instruction()

        path = m.Path()
        path.set_appender(m.YesBranchAppender(path, bi))
        path.append(i1, i1)

        self.assertEqual(i1, bi.instruction_on_yes)

    def test_YesBranchAppender_does_not_touch_no_path(self):
        sentinel = object()
        bi = m.BranchingInstruction()
        bi.instruction_on_no = sentinel
        i1 = m.Instruction()
        i2 = m.Instruction()

        path = m.Path()
        path.set_appender(m.YesBranchAppender(path, bi))
        path.append(i1, i1)
        path.append(i2, i2)

        self.assertEqual(sentinel, bi.instruction_on_no)

    def test_YesBranchAppender_resets_appender(self):
        bi = m.BranchingInstruction()
        i1 = m.Instruction()
        i2 = m.Instruction()

        path = m.Path()
        path.set_appender(m.YesBranchAppender(path, bi))
        path.append(i1, i1)
        path.append(i2, i2)

        self.assertEqual(i1, bi.instruction_on_yes)
        self.assertEqual(i2, i1.next_instruction)

    def test_NoBranchAppender(self):
        bi = m.BranchingInstruction()
        i1 = m.Instruction()

        path = m.Path()
        path.set_appender(m.NoBranchAppender(path, bi))
        path.append(i1, i1)

        self.assertEqual(i1, bi.instruction_on_no)

    def test_NoBranchAppender_does_not_touch_yes_path(self):
        sentinel = object()
        bi = m.BranchingInstruction()
        bi.instruction_on_yes = sentinel
        i1 = m.Instruction()
        i2 = m.Instruction()

        path = m.Path()
        path.set_appender(m.NoBranchAppender(path, bi))
        path.append(i1, i1)
        path.append(i2, i2)

        self.assertEqual(sentinel, bi.instruction_on_yes)

    def test_NoBranchAppender_resets_appender(self):
        bi = m.BranchingInstruction()
        i1 = m.Instruction()
        i2 = m.Instruction()

        path = m.Path()
        path.set_appender(m.NoBranchAppender(path, bi))
        path.append(i1, i1)
        path.append(i2, i2)

        self.assertEqual(i1, bi.instruction_on_no)
        self.assertEqual(i2, i1.next_instruction)


class Test_Program(unittest.TestCase):

    PROGRAM_CLASS = m.Program

    def program(self, program_spec):
        return self.PROGRAM_CLASS(program_spec)

    def test_instruction_sequence(self):
        prog = self.program({'main': [Add1, Div2, RETURN_TRUE]})
        self.assertEqual(2, prog.run(3))

    def test_incomplete_program_is_not_compilable(self):
        with self.assertRaises(UndefinedLabelError):
            self.program({'main': ['label', RETURN_TRUE]})

    # TODO: add test against recursion

    def test_branch_on_yes(self):
        prog = self.program({
            'main': [
                IF (IsOdd),
                    Add1,
                ELSE,
                    'add2',
                ENDIF,
                RETURN_TRUE
            ],

            'add2': [
                Add1,
                Add1,
                RETURN_TRUE
            ]
        })
        self.assertEqual(4, prog.run(3))
        self.assertEqual(6, prog.run(4))

    def test_branch_on_no(self):
        prog = self.program({
            'main': [
                IF (IsOdd),
                    Add1,
                    Add1,
                ELSE,
                    'add1',
                ENDIF,
                RETURN_TRUE
            ],

            'add1': [
                Add1,
                RETURN_TRUE
            ]
        })
        self.assertEqual(5, prog.run(4))
        self.assertEqual(5, prog.run(3))

    def test_string_as_call_symbol(self):
        prog = self.program({
            'main': ['+1', '+2', RETURN_TRUE],
            '+2': ['+1', '+1', RETURN_TRUE],
            '+1': [Add1, RETURN_TRUE]
        })
        self.assertEqual(3, prog.run(0))

    def test_compilation_with_missing_ENDIF_is_not_possible(self):
        with self.assertRaises(MissingEndIfError):
            self.program({'main': [IF (IsOdd), RETURN_TRUE]})

    def test_compilation_with_multiple_ELSE_is_not_possible(self):
        with self.assertRaises(MultipleElseError):
            self.program(
                {'main': [IF (IsOdd), ELSE, ELSE, ENDIF, RETURN_TRUE]})

    def test_IF(self):
        prog = self.program({
            'main': [
                IF (IsOdd),
                    Add1,
                ENDIF,
                Add1,
                RETURN_TRUE
            ]
        })

        self.assertEqual(1, prog.run(0))
        self.assertEqual(3, prog.run(1))
        self.assertEqual(3, prog.run(2))

    def test_IF_NOT(self):
        prog = self.program({
            'main': [
                IF_NOT (Eq('value')),
                    Const('IF_NOT'),
                    IF_NOT (Eq('value')),
                        Const('IF_NOT'),
                        # here we have the bug: this path is merged to here,
                        # but outside this path is *ignored*,
                        # its ELSE path is merged
                    ENDIF,
                ENDIF,
                Add('.'),
                RETURN_TRUE
            ]
        })

        self.assertEqual('value.', prog.run('value'))
        self.assertEqual('IF_NOT.', prog.run('?'))

    def test_ELSE(self):
        prog = self.program({
            'main': [
                IF (IsOdd),
                ELSE,
                    Add1,
                ENDIF,
                Add1,
                RETURN_TRUE
            ]
        })

        self.assertEqual(2, prog.run(0))
        self.assertEqual(2, prog.run(1))
        self.assertEqual(4, prog.run(2))

    def test_IF_ELSE(self):
        prog = self.program({
            'main': [
                IF (IsOdd),
                    Add1,
                ELSE,
                    Add1,
                    Add1,
                ENDIF,
                Add1,
                RETURN_TRUE
            ]
        })

        self.assertEqual(3, prog.run(0))
        self.assertEqual(3, prog.run(1))
        self.assertEqual(5, prog.run(2))

    def test_IF_ELIF_ELSE(self):
        prog = self.program({
            'main': [
                IF (Eq('value')),
                    Const('IF'),
                ELIF (Eq('variant1')),
                    Const('ELIF1'),
                ELIF (Eq('variant2')),
                    Const('ELIF2'),
                ELSE,
                    Const('ELSE'),
                ENDIF,
                Add('.'),
                RETURN_TRUE
            ]
        })

        self.assertEqual('IF.', prog.run('value'))
        self.assertEqual('ELIF1.', prog.run('variant1'))
        self.assertEqual('ELIF2.', prog.run('variant2'))
        self.assertEqual('ELSE.', prog.run('unknown'))

    def test_IF_ELIF_NOT_ELSE(self):
        prog = self.program({
            'main': [
                IF (Eq('value')),
                    Const('IF'),
                ELIF_NOT (Eq('variant')),
                    # not variant
                    Const('ELIF_NOT'),
                ELSE,
                    # variant
                    Const('ELSE'),
                ENDIF,
                Add('.'),
                RETURN_TRUE
            ]
        })

        self.assertEqual('IF.', prog.run('value'))
        self.assertEqual('ELIF_NOT.', prog.run('not_variant'))
        self.assertEqual('ELSE.', prog.run('variant'))

    def test_IF_NOT_ELSE(self):
        prog = self.program({
            'main': [
                IF_NOT (Eq('value')),
                    # not value
                    Const('IF_NOT'),
                ELSE,
                    # value
                    Const('ELSE'),
                ENDIF,
                Add('.'),
                RETURN_TRUE
            ]
        })

        self.assertEqual('IF_NOT.', prog.run('unkown'))
        self.assertEqual('ELSE.', prog.run('value'))

    def test_compilation_with_ELIF_after_ELSE_is_not_possible(self):
        with self.assertRaises(ElIfAfterElseError):
            self.program({
                'main': [
                    IF (IsOdd),
                    ELSE,
                    ELIF (IsOdd),
                    ENDIF,
                    RETURN_TRUE
                ]
            })

    def test_embedded_IFs(self):
        prog = self.program({
            'main': [
                IF (IsOdd),
                    Add1,
                    Div2,
                    IF (IsOdd),
                        Add1,
                    ENDIF,
                ELSE,
                    Add1,
                    Add1,
                ENDIF,
                Div2,
                RETURN_TRUE
            ]
        })

        self.assertEqual(1, prog.run(0))
        self.assertEqual(1, prog.run(1))
        self.assertEqual(2, prog.run(2))
        self.assertEqual(1, prog.run(3))
        self.assertEqual(3, prog.run(4))
        self.assertEqual(2, prog.run(5))

    def test_macro_return_yes(self):
        prog = self.program({
            'main': [
                IF ('odd?'),
                    Add1,
                ENDIF,
                RETURN_TRUE
            ],

            'odd?': [
                IF (IsOdd),
                    RETURN_TRUE,
                ELSE,
                    RETURN_FALSE,
                ENDIF
            ]
        })

        self.assertEqual(2, prog.run(1))
        self.assertEqual(4, prog.run(3))

    def test_macro_return_no(self):
        prog = self.program({
            'main': [
                IF ('odd?'),
                    Add1,
                ENDIF,
                RETURN_TRUE
            ],

            'odd?': [
                IF (IsOdd),
                    RETURN_TRUE,
                ELSE,
                    RETURN_FALSE,
                ENDIF
            ]
        })

        self.assertEqual(2, prog.run(2))
        self.assertEqual(4, prog.run(4))

    # used in the next 2 tests
    complex_prog_spec = {
        'main': [
            IF ('even?'),
                RETURN_TRUE,
            ELSE,
                Add1,
            ENDIF,
            RETURN_TRUE
        ],

        'even?': [
            IF (IsOdd),
                RETURN_FALSE,
            ELSE,
                RETURN_TRUE,
            ENDIF
        ]
    }

    def test_macro_return(self):
        prog = self.program(self.complex_prog_spec)

        self.assertEqual(2, prog.run(1))
        self.assertEqual(2, prog.run(2))
        self.assertEqual(4, prog.run(3))
        self.assertEqual(4, prog.run(4))

    def test_instruction_index(self):
        prog = self.program(self.complex_prog_spec)

        indices = [i.index for i in prog.instructions]
        self.assertEqual(range(len(prog.instructions)), indices)

    def test_sub_programs(self):
        prog = self.program({
            'main': [
                Add1,
                m.RETURN_TRUE
            ],

            'x1': [m.RETURN_TRUE],
            'x2': [m.RETURN_TRUE],

            'x3': [
                Add1,
                Add1,
                m.RETURN_TRUE
            ]
        })

        self.assertEqual(3, len(prog.instructions))  # Add1, RETURN, _END_

    def program_for_visiting_with_all_features(self):
        return self.program({
            'main': ['x', m.RETURN_TRUE],

            'x': [
                m.IF (IsOdd),
                    Add1,
                m.ENDIF,
                m.RETURN_TRUE
            ]
        })

    def check_visitor(self, visitor):
        prog = self.program_for_visiting_with_all_features()
        prog.accept(visitor)

    def test_remembering_visitor_is_an_accepted_visitor(self):
        self.check_visitor(RememberingVisitor())

    def test_visit(self):
        prog = self.program_for_visiting_with_all_features()

        remembering_visitor = RememberingVisitor()

        prog.accept(remembering_visitor)

        i = prog.instructions
        self.assertEqual(
            [
            ('branch', i[0]),
            ('instruction', i[1]),
            ('return', i[2]),
            ('branch', i[3]),
            ('return', i[4]),
            ('branch', i[5]),
            ('end')
            ], remembering_visitor.calls)


class RememberingVisitor(m.ProgramVisitor):

    calls = None

    def __init__(self):
        self.calls = []

    def visit_return(self, i_return):
        self.calls.append(('return', i_return))

    def visit_instruction(self, instruction):
        self.calls.append(('instruction', instruction))

    def visit_branch(self, i_branch):
        self.calls.append(('branch', i_branch))

    def end_program(self):
        self.calls.append('end')