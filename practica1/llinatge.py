import sys


def parse_file(filename):
    ''' Returns a list of tuples (parent,child) created while reading the provided file name '''
    global node
    edges = []
    with open(filename, 'r') as file:
        for line in file:
            if(len(line.strip().split()) == 1):
                node = str(line.strip().split()[0])
            else:
                parent, child = line.strip().split()
                edges.append((parent, child))
    return edges

def construct_tree(edges):
    # Extract the root value from the first pair of parent and child in the list of edges
    root_value = edges[0][0]

    # Create a dictionary to map node values to their children
    tree_dict = {root_value: []}

    # Iterate over each pair of parent and child in the list of edges
    for parent, child in edges:
        # If the parent node is not already in the tree_dict, add it with an empty list of children
        if parent not in tree_dict:
            tree_dict[parent] = []
        # Add the child node to the list of children of the parent node
        tree_dict[parent].append(child)

    # Return the dictionary representing the tree
    return tree_dict

dic={}

def search_deepest_childs(tree_dict, node_value, depth=0):
    # Print the current node value with appropriate indentation

    # Get the children of the current node
    children = tree_dict.get(node_value, [])

    # Recursively print each child
    if children:
        dic[depth] = dic.get(depth, []) + children
    for child in children:
        search_deepest_childs(tree_dict, child, depth + 1)

def main(filename):
    edges = parse_file(filename)
    tree = construct_tree(edges)
    search_deepest_childs(tree,node)
    print(dic[max(dic.keys())])

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python script.py filename")
        sys.exit(1)
    filename = sys.argv[1]
    main(filename)
