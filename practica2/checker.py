#!/usr/bin/env python

import os
import sys
import subprocess
import time
import matplotlib.pyplot as plt

directory = "tests/"
xinitial = 3
xfinal = int(sys.argv[1])

for algorithm in ["backtracking", "improved", "greedy"]:
    times = []
    print("Algorithm: ", algorithm)
    for x in range(xinitial, xfinal + 1):
        point_time = 0
        for instance in range(10):
            print("##############")
            input_file = os.path.join(directory, f"input3-{x}-{instance}.txt")
            print(f"Testing {input_file}")
            initial_time = time.time()
            process = subprocess.run(
                ['python3',  './tunnel.py', '--algorithm', algorithm, input_file],
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
            )
            final_time = time.time()
            output_data = process.stdout.decode("utf-8")
            error_data = process.stderr.decode("utf-8")
            with open("output.txt", "w") as output_file:
                output_file.write(output_data + error_data)

            try:
                output_file = os.path.join(directory, f"output3-{x}-{instance}.txt")
                subprocess.run(["diff", "output.txt", output_file], check=True)
            except subprocess.CalledProcessError:
                print("#### Error in ", output_file)
                raise

            point_time += final_time - initial_time

        times.append( (x, point_time) )

    xvalues, yvalues = zip(*times)

    #plt.clf()
    plt.plot(xvalues, yvalues, label = algorithm)

plt.legend(loc="upper left")
plt.xticks(range(xinitial, xfinal + 1))
plt.title("Time to solve the tunnel problem")
plt.xlabel("Number of Umpalumpas")
plt.ylabel("Time (s)")
#plt.show()
plt.savefig("time.png")
