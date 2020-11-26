from json import load
import sys
import matplotlib.pyplot as plt
from matplotlib.ticker import FuncFormatter



def calcAverages(ds):
    c = 0
    for i in ds["runtimes"]:
        c += int(i)
    avg = c // len(ds["runtimes"])
    return avg

def formattick(val):
    # Dirty hack, don't shame me
    print(val)
    return val.split("[")[1].split("]")[0]

def main(basename, entrypoint, output):
    op = load(open("{}-opencl.json".format(basename)))["{}.fut:{}"
            .format(basename, entrypoint)]
    c = load(open("{}-c.json".format(basename)))["{}.fut:{}"
            .format(basename, entrypoint)]
    # Extract relevant data
    op_labels = sorted(op["datasets"].keys(), reverse=True)
    op_run = [calcAverages(op["datasets"][ds]) for ds in op_labels]
    c_labels = sorted(c["datasets"].keys(), reverse=True)
    c_run = [calcAverages(c["datasets"][ds]) for ds in c_labels]
    # Get speedups
    diffs = [(c_i / op_i) for (c_i, op_i) in zip (c_run, op_run)]
    # Do the plotty plotty stuff
    text_labels_op = [formattick(t) for t in op_labels]
    text_labels_c = [formattick(t) for t in c_labels]
    fig, ax = plt.subplots()
    ax.plot(text_labels_op, op_run, label="Opencl Runtime")
    ax.plot(text_labels_c, c_run, label="C Runtime")
    ax.plot(diffs, label="Diffs")
    ax.set_ylabel("Runtime [ms]")
    ax.set_xlabel("Dataset Size")
    fig.legend()

    fig.savefig(output)

if __name__ == "__main__":
    # Load data
    if len(sys.argv) != 4:
        print("Usage: python makeGraphs.py <basename> <entrypoint> <output>")
    else:
        main(*sys.argv[1:])

