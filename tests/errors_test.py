import os
from subprocess import Popen, PIPE
import argparse

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Process tests in specified folder and compare return code to 0.')
    parser.add_argument('executable', type=str, help="Executable path.")
    parser.add_argument('folder', type=str, help="Folder with tests.")
    args = parser.parse_args()

    tests = os.listdir(args.folder)
    for test in tests:
        cmd = [f'./{args.executable}', '-i', os.path.join(args.folder, test)]
        p = Popen(cmd, stdin=PIPE, stdout=PIPE, stderr=PIPE,
                  bufsize=-1)
        output, error = p.communicate()

        if p.returncode == 0:
            raise Exception(f"Test file: <{test}> did not terminated with ret code != 0.")

    print("All tests passed.")
