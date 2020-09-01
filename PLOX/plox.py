import sys


def run(line):
    tokens = str(line).split()
    for token in tokens:
        print(token)

def run_file(file_path):
    file = open(file_path, 'r')
    lines = file.readlines()
    for line in lines:
        print(line)
        run(file)

def run_prompt():
    guard = True
    while guard:
        line = input("> ")
        if line is None:
            guard = False
        run(line)


def main():
    print("# args {}".format(len(sys.argv)))
    print(sys.argv)
    if len(sys.argv) == 2:
        run_file(sys.argv[1])
    else:
        run_prompt()


main()
