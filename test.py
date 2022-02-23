import os
from subprocess import Popen, PIPE
import argparse
import collections


def parse_grammar(grammar):
    rows = grammar.split('\n')
    non_terminals = rows[0].split(',')
    terminals = rows[1].split(',')
    starting_symbol = rows[2]
    rules = [x for x in rows[3:] if x]
    return non_terminals, terminals, starting_symbol, rules


def same_grammars(g1, g2):
    return collections.Counter(g1[0]) == collections.Counter(g2[0]) and collections.Counter(
        g1[1]) == collections.Counter(g2[1]) and g1[2] == g2[2] and collections.Counter(g1[3]) == collections.Counter(
        g2[3])


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Process tests in specified folder and compare to provided output.')
    parser.add_argument('executable', type=str, help="Executable path.")
    parser.add_argument('folder', type=str, help="Folder with tests.")

    args = parser.parse_args()

    tests = list(set([file.split('.')[0] for file in os.listdir(args.folder)]))

    for test in tests:
        cmd = [f'./{args.executable}', '-i', f'{os.path.join(args.folder, test)}.in']
        p = Popen(cmd, stdin=PIPE, stdout=PIPE, stderr=PIPE,
                  bufsize=-1)
        with open(f'{os.path.join(args.folder, test)}.out') as f:
            ref_output = f.read()
            ref_g = parse_grammar(ref_output)
            output, error = p.communicate()
            hyp_g = parse_grammar(output.decode("utf-8"))
            if not same_grammars(ref_g, hyp_g):
                raise Exception(f"Test file: <{test}> failed.")
    print("All tests passed.")
