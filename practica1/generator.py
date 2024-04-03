#!/usr/bin/env python3

from random import randint, choice, shuffle
from string import ascii_uppercase
from itertools import product
from math import log, ceil

def create_instance(infilename, outfilename, branches, levels):
  name_length = ceil(log(levels, branches))+1
  if branches**levels > len(ascii_uppercase)**name_length:
    name_length += 1
     
  all_nodes = [''.join(name) for name in product(ascii_uppercase, repeat=name_length)]
  shuffle(all_nodes)
  all_nodes = all_nodes[:branches**levels]

  queue, all_nodes = all_nodes[branches**(levels-1):], all_nodes[:(branches**(levels-1)+1)]
  leaves = queue[:]

  nodes = {}
  parent = ""
  relations = []
  while len(queue) > 1:
      children, queue = queue[:branches], queue[branches:]
      parent = all_nodes[0]
      all_nodes = all_nodes[1:]
      nodes[ parent ] = children
      relations += [(parent,child) for child in children]
      queue.append(parent)

  with open(infilename, 'w') as f:
    print(parent, file=f)
    for parent, child in relations:
      print( parent, child, file=f )

  with open(outfilename, 'w') as f:
    print(' '.join(sorted(leaves)), file=f)

def main():
    import sys
    max_leaves_per_node = 3
    first_level, last_level = int(sys.argv[1]), int(sys.argv[2])
    number_of_instances = 10
    directory = "tests/"

    for max_levels in range(first_level, last_level+1):
        print("Creating instances for", max_levels, "levels")
        for instance in range(number_of_instances):
            input_filename = directory + "input"+str(max_levels)+"-"+str(instance)+".txt" 
            output_filename = directory + "output"+str(max_levels)+"-"+str(instance)+".txt"
            arbre = create_instance(input_filename, output_filename, max_leaves_per_node, max_levels)    

if __name__ == "__main__":
    main()
