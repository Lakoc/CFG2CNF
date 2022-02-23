import random
import string
from subprocess import Popen, PIPE
import argparse

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Process argument tests of executable.')
    parser.add_argument('executable', type=str, help="Executable path.")
    args = parser.parse_args()

    program_args = ["",  # no arguments
                    "-i -i -i",  # multiple arguments
                    f"-i {''.join(random.choices(string.ascii_uppercase + string.digits, k=30))}",  # not existing file
                    "-i /tmp",  # folder
                    "-i -1",  # two valid args
                    "-i /dev/null"  # empty file
                    "-i "
                    ]
    for arg in program_args:
        cmd = [f'./{args.executable}']
        cmd.extend(arg.split())

        p = Popen(cmd, stdin=PIPE, stdout=PIPE, stderr=PIPE,
                  bufsize=-1)
        output, error = p.communicate()

        if p.returncode == 0:
            raise Exception(f"Test with arguments: <{arg}> did not terminated with ret code != 0.")

    print("All tests passed.")
