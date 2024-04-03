
"""WARNING: This code has not been extensively checked"""

def first_empty( elements ):
    """Find the first empty space when the elements are sorted.
    Algorithm with nested loop."""
    def exists_previous( position ):
        for j in range( len(elements)):
            if j == position:
                continue
            if elements[j] == elements[position]-1:
                return True
        return False
    
    solution = len(elements)
    for i in range(len(elements)):
        if elements[i] == 1:
            continue
        if not exists_previous( i ):
            solution = min(solution, elements[i])

    return solution-1

def first_empty1( elements ):
    """Find the first empty space when the elements are sorted.
    Algorithm with sorting."""
    elements = sorted(elements)
    for i, j in enumerate(elements):
        if i+1 != j:
            return i+1
    return len(elements)

def first_empty2( elements, solution = 0):
    """Find the first empty space when the elements are sorted.
    Divide and conquer."""
    def partition():
        pivot = elements[0]
        left = []
        right = []
        for element in elements[1:]:
            if element < pivot:
                left.append( element )
            else:
                right.append( element )
        return pivot, left, right
    
    if not elements:
        return solution+1
    x, left, right = partition()
    if x == len(left) + solution + 1:
        return first_empty2( right, x )
    else:
        return first_empty2( left, solution )

a = [3,5,1,7,2]
print( first_empty(a) )
print( first_empty1(a) )
print( first_empty2(a) )
