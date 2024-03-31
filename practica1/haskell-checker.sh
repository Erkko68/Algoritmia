#!/usr/bin/env bash

directory="tests/"
total_time=0
num_runs=0

for x in {2..4}; do
    for y in {0..9}; do
        echo "##############"
        inputfile="$directory/input$x-$y.txt"
        echo "Testing $inputfile"

        start=$(date +%s%N)
        ./gerarquia "$inputfile" > output.txt
        endt=$(date +%s%N)
        runtime=$((endt-start))
        total_time=$((total_time + runtime))
        num_runs=$((num_runs + 1))

        echo "Execution time for run $num_runs: $runtime ns"

        outputfile="$directory/output$x-$y.txt"
        diff output.txt "$outputfile"
    done
done

average_time=$((total_time / num_runs))
echo "Average execution time: $average_time ns"
