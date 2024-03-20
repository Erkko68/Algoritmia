import sys

def parse_file(filename):
    #Returns a list of tuples (parent,child) created while reading the provided file name
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

def search_deepest_childs(tree_dict, node_value):
    stack = [(node_value, 0)]  # Use a stack to keep track of nodes and their depths

    while stack:
        node, depth = stack.pop()  # Pop the top node and its depth

        children = tree_dict.get(node, [])  # Get children of the current node

        if not children:
            yield node, depth  # Yield the deepest node and its depth
        else:
            for child in children:
                stack.append((child, depth + 1))  # Add children to the stack with their updated depths


def main(filename):
    edges = parse_file(filename)
    tree = construct_tree(edges)
    list = search_deepest_childs(tree,node)

    # Convert list into a dictionary
    result_dict = {}
    for letter, number in list:
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
    filename = sys.argv[1]
    main(filename)
