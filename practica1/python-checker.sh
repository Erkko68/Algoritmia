#!/usr/bin/env bash

directory="tests/"
echo $directory
for x in {2..4}
do
   for y in {0..9}
   do
      echo "##############"
      inputfile=$directory'input'$x'-'$y'.txt'
      echo Testing $inputfile
      python3 gerarquia.py $inputfile > output.txt
      outputfile=$directory'output'$x'-'$y'.txt'
      diff output.txt $outputfile
   done
done
