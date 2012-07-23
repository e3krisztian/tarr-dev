import unittest
import mock
import tarr.application as m # odule
import tarr.db
from datetime import datetime

def make_app(cls=m.Application):
    app = cls()
    app.session = mock.Mock()
    app.dag_config_hash = mock.Mock(app.dag_config_hash, return_value=mock.sentinel.dag_config_hash)
    return app


def create_job(app, dag_config='', source='', partitioning_name='', description=''):
    app.create_job(dag_config=dag_config, source=source, partitioning_name=partitioning_name, description=description)


def uncompleted_batch(source):
    batch = tarr.db.Batch()
    batch.source = source
    return batch

def completed_batch(source, time_completed=mock.sentinel.time_completed, dag_config_hash=mock.sentinel.dag_config_hash):
    batch = uncompleted_batch(source=source)
    batch.time_completed = time_completed
    batch.dag_config_hash = dag_config_hash
    return batch


class Bpplication(m.Application):

    pass


class Test_create_job(unittest.TestCase):

    def test_created(self):
        app = make_app()

        app.create_job(dag_config='', source='', partitioning_name='', description='')

        self.assertIsNotNone(app.job)

    def test_application(self):
        app = make_app()

        app.create_job(dag_config='', source='', partitioning_name='', description='')

        self.assertEqual('tarr.application.Application', app.job.application)

    def test_bpplication(self):
        app = make_app(cls=Bpplication)

        app.create_job(dag_config='', source='', partitioning_name='', description='')

        self.assertEqual('tarr.test_application.Bpplication', app.job.application)

    def test_added_to_session(self):
        app = make_app()
        app.session.add = mock.Mock()

        app.create_job(dag_config='', source='', partitioning_name='', description='')

        app.session.add.assert_called_once_with(app.job)

    def test_creates_batches(self):
        app = make_app()
        app.create_batches = mock.Mock()

        app.create_job(dag_config='', source='', partitioning_name='', description='')

        app.create_batches.assert_called_once_with()

    def test_changes_committed(self):
        app = make_app()

        app.create_job(dag_config='', source='', partitioning_name='', description='')

        app.session.commit.assert_called_once_with()

    def test_parameters_stored(self):
        app = make_app()
        app.dag_config_hash = mock.Mock(app.dag_config_hash)

        ms = mock.sentinel
        app.create_job(dag_config=ms.dag_config, source=ms.source, partitioning_name=ms.partitioning_name, description=ms.description)

        self.assertEqual(ms.dag_config, app.job.dag_config)
        self.assertEqual(ms.source, app.job.source)
        self.assertEqual(ms.partitioning_name, app.job.partitioning_name)
        self.assertEqual(ms.description, app.job.description)

    def test_dag_config_hash_called_for_hash(self):
        app = make_app()

        app.create_job(dag_config='tarr.test_dag_config_for_hash', source='', partitioning_name='', description='')

        self.assertEqual(mock.sentinel.dag_config_hash, app.job.dag_config_hash)


class Test_dag_config_file(unittest.TestCase):

    def test(self):
        app = make_app()
        dag_config = 'asd/x'
        app.create_job(dag_config=dag_config, source='', partitioning_name='', description='')

        self.assertRegexpMatches(app.dag_config_file(), '/asd/x')


class Test_dag_config_hash(unittest.TestCase):

    def test(self):
        app = m.Application()
        app.job = mock.Mock()
        app.job.dag_config = 'fixtures/test_dag_config_for_hash'

        self.assertEqual('e01b0562e55d417eb14b6646d14fc9e5a879ab02', app.dag_config_hash())


class Test_load_dag(unittest.TestCase):

    def test_dag_is_available(self):
        app = make_app()
        app.job = mock.Mock()
        app.job.dag_config = 'fixtures/test_dag_config'

        app.load_dag()

        self.assertIsNotNone(app.dag.node_by_name('id'))


class Test_process_job(unittest.TestCase):

    def test(self):
        processed_batches = []
        class Application(m.Application):
            def process_batch(self):
                processed_batches.append(self.batch.source)

        app = make_app(cls=Application)
        create_job(app)

        app.job.batches.append(uncompleted_batch(source='1'))
        app.job.batches.append(completed_batch(source='2'))
        app.job.batches.append(uncompleted_batch(source='3'))

        app.process_job()

        self.assertEqual(['1', '3'], processed_batches)
