import os
from subprocess import Popen, PIPE

if __name__ == '__main__':
    tests = os.listdir('tests/errors')
    for test in tests:
        cmd = ['./flp21-fun', '-i', f'tests/errors/{test}']

        p = Popen(cmd, stdin=PIPE, stdout=PIPE, stderr=PIPE,
                  bufsize=-1)
        output, error = p.communicate()

        if p.returncode == 0:
            raise Exception(f"Test file: <{test}> did not terminated with ret code != 0.")

    print("All tests passed.")
