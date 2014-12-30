import os
from os import path
import subprocess

from sphinxcontrib.cldomain import cl_launch_args, which


cl_launch_exe = [which("cl-launch")[0]]
cl_launch_command = cl_launch_args(
    package="sphinxcontrib.cldomain/test",
    main_function="sphinxcontrib.cldomain/test::run-tests")

env = os.environ.copy()
if path.exists(path.expanduser("~/.quicklisp")):
    env['QUICKLISP'] = path.expanduser("~/.quicklisp")
elif path.exists(path.expanduser("~/quicklisp")):
    env['QUICKLISP'] = path.expanduser("~/quicklisp")
else:
    raise Exception("Can't find Quicklisp.")

env.update({"CLDOMAIN": path.abspath(path.dirname(__file__)) + "/"})

subprocess.check_call(cl_launch_exe + cl_launch_command, env=env)
