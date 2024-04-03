#!/usr/bin/env python3

"""
Module to analyze hierarchical structures.
"""

import sys

def parse_file(filename):
    """
    Parses the file and returns a list of edges and the parent node.

    Args:
    filename (str): The name of the file to parse.

    Returns:
    tuple: A tuple containing a list of edges (as tuples) and the parent node.
    """
    parent_node = None  # Initialize parent_node
    edges = []
    with open(filename, 'r', encoding='utf-8') as file:
        for line in file:
            if len(line.strip().split()) == 1:
                parent_node = str(line.strip().split()[0])
            else:
                parent, child = line.strip().split()
                edges.append((parent, child))
    return edges, parent_node

def construct_tree(edges):
    """
    Constructs a tree dictionary from a list of edges.

    Args:
    edges (list): A list of edges (as tuples).

    Returns:
    dict: A dictionary representing the tree structure.
    """
    root_value = edges[0][0]
    tree_dict = {root_value: []}
    for parent, child in edges:
        if parent not in tree_dict:
            tree_dict[parent] = []
        tree_dict[parent].append(child)
    return tree_dict

def search_deepest_childs(tree_dict, node_value):
    """
    Searches for the deepest children in the tree.

    Args:
    tree_dict (dict): A dictionary representing the tree structure.
    node_value (str): The value of the node from which to start the search.

    Yields:
    tuple: A tuple containing the deepest node and its depth.
    """
    stack = [(node_value, 0)]
    while stack:
        node, depth = stack.pop()
        children = tree_dict.get(node, [])
        if not children:
            yield node, depth
        else:
            for child in children:
                stack.append((child, depth + 1))

def main(filename):
    """
    Main function to execute the program.

    Args:
    filename (str): The name of the file to process.
    """
    edges, parent_node = parse_file(filename)
    tree = construct_tree(edges)
    generator = search_deepest_childs(tree, parent_node)

    result_dict = {}
    for letter, number in generator:
        if number in result_dict:
            result_dict[number].append(letter)
        else:
            result_dict[number] = [letter]

    result = sorted(result_dict[max(result_dict.keys())])
    print(' '.join(result))

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python script.py filename")
        sys.exit(1)
    main(sys.argv[1])