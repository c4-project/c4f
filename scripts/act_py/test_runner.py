# The Automagic Compiler Tormentor
#
# Copyright (c) 2018--2019 Matt Windsor and contributors
#
# ACT itself is licensed under the MIT License. See the LICENSE file in the
# project root for more information.
#
# ACT is based in part on code from the Herdtools7 project
# (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
# project root for more information.
"""The main part of the ACT high-level test runner."""
import logging
import subprocess
import typing
from dataclasses import dataclass

from act_py import act_id, test_common, io_utils

logger = logging.getLogger(__name__)


@dataclass
class RunPhase(test_common.Phase):
    """Object representing a single run of the tester, with a given machine, compiler, and subject."""

    def __init__(self, instance: test_common.Instance):
        test_common.Phase.__init__(self, instance)

    @property
    def driver_dict(self) -> typing.Mapping[str, typing.Any]:
        """Returns the dictionary of values used to populate driver templates.

        This doesn't include any of the environment's values, as they're
        auto-populated when we ask for the populated driver.

        :return: The dictionary used for driver templates.
        """
        return {
            "backend": self.backend,
            "compiler": self.compiler,
            "dir": self.output_dir,
            **self.subject.driver_dict,
        }

    def run(self) -> None:
        """Runs this test instance."""
        self.prepare_instance()

        logger.info("%s: running driver", str(self))
        proc: subprocess.CompletedProcess = subprocess.run(
            self.driver_command, shell=True, text=True, capture_output=True
        )
        self.log_result(proc)
        self.write_files(proc)

    def prepare_instance(self) -> None:
        """Performs instance-specific preparations for the test run."""
        self.cleanup_error_file()
        self.output_dir.mkdir(exist_ok=True)

    def cleanup_error_file(self) -> None:
        """Removes the error file if it exists.

        This mainly serves to make sure that, if we're re-running tests in the
        same scratch area, the test checker doesn't accidentally mistake
        previous errors for ones in the current run.
        """
        try:
            self.error_file.unlink()
        except FileNotFoundError:
            pass

    def log_result(self, proc: subprocess.CompletedProcess) -> None:
        """Logs the exit code of a driver.

        :param proc: The completed process corresponding to the driver.
        """
        if proc.returncode == 0:
            logger.info("%s: success", str(self))
        else:
            logger.error("%s: failed: see %s for details", str(self), self.error_file)

    def write_files(self, proc: subprocess.CompletedProcess) -> None:
        """Writes the output of a driver to specific files in the output
        directory.

        :param proc: The completed process corresponding to the driver.
        """
        io_utils.write_text(self.error_file, proc.stderr)
        if proc.returncode == 0:
            io_utils.write_text(self.state_file, proc.stdout)

    @property
    def driver_command(self) -> str:
        """Gets the fully populated driver shell command for this instance.
        :return: The populated driver template.
        """
        return self.env.populate_driver(**self.driver_dict)


def run_phase(instance: test_common.Instance) -> test_common.Phase:
    """Function wrapper for `RunPhase`.

    :param instance: The instance to use for the check.
    :return: A `test_common.Phase` that, when run, computes the test result for
        `instance`.
    """
    return RunPhase(instance)
