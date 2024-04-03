#!/usr/bin/env python3

import os
import sys
import subprocess
import time
import matplotlib.pyplot as plt

directory = "tests/"
initial = int(sys.argv[1])
final = int(sys.argv[2])

for executable in ["gerarquia.py", "gerarquia"]:
    times = []
    for x in range(initial, final + 1):
        point_time = 0
        for y in range(10):
            print("##############")
            input_file = os.path.join(directory, f"input{x}-{y}.txt")
            print(f"Testing {input_file}")
            initial_time = time.time()
            process = subprocess.run(
                ['./' + executable, input_file],
                stdout=subprocess.PIPE,
                stderr=subprocess.DEVNULL,
            )
            final_time = time.time()
            output_data = process.stdout.decode("utf-8")
            with open("output.txt", "w") as output_file:
                output_file.write(output_data)

            try:
                output_file = os.path.join(directory, f"output{x}-{y}.txt")
                subprocess.run(["diff", "output.txt", output_file], check=True)
            except subprocess.CalledProcessError:
                print("Error in ", output_file)
                raise

            point_time += final_time - initial_time

        times.append( (x, point_time) )

    xvalues, yvalues = zip(*times)

    plt.clf()
    plt.plot(xvalues, yvalues)
    plt.xticks(range(initial, final + 1))
    plt.title("Time to solve the hierarchy problem with " + executable)
    plt.xlabel("Number of Levels")
    plt.ylabel("Time (s)")
    #plt.show()
    plt.savefig(executable + ".time.png")
