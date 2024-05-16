
import random
import tunnel 

def create_instance( input_filename, output_filename, capacitat, num_elements):
    with open(input_filename, 'w') as f:
        print(num_elements, capacitat, file=f)
        print( " ".join(map(str, random.sample(range(1,100), num_elements))), file=f)

    with open(output_filename, 'w') as f:
        capacitat, umpalumpes = tunnel.llegir(input_filename)
        solucions = tunnel.backtracking( capacitat, umpalumpes )
        if not solucions:
            return print(0, file=f)
        else:
            print( min( tunnel.backtracking( capacitat, umpalumpes ) ), file=f)

def main():
    import sys

    if len(sys.argv) > 2:
        capacitat = int(sys.argv[1])
    else:
        capacitat = 3
    directory = "tests/"
    number_of_instances = 10
    min_elements = capacitat
    max_elements = capacitat * 2

    for num_elements in range(min_elements, max_elements):
        print("Creating instances for", num_elements, "elements")
        for instance in range(number_of_instances):
            input_filename = directory + "input"+str(capacitat)+"-"+str(num_elements)+"-"+str(instance)+".txt" 
            output_filename = directory + "output"+str(capacitat)+"-"+str(num_elements)+"-"+str(instance)+".txt"
            create_instance(input_filename, output_filename, capacitat, num_elements)

if __name__ == "__main__":
    main()
