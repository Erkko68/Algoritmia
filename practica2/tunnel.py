#!/usr/bin/env python3

# Used to count recursive calls
def counted(f):
    def wrapped(*args, **kwargs):
        wrapped.calls += 1
        return f(*args, **kwargs)
    wrapped.calls = 0
    return wrapped

def llegir( filename ):
    capacitat, llistat = None, None
    with open(filename) as f:
        nombre, capacitat = list(map( int, f.readline().split()))
        llistat = list(map( int, f.readline().split()))
        if len(llistat) != nombre:
            raise Exception("Does not match", len(llistat), nombre)
    return capacitat, llistat

def rem( llista, element ):
    index = llista.index(element)
    return llista[:index] + llista[index+1:]

def rem_list( llista, elements ):
    resultat = llista[:]
    for element in elements:
        resultat.pop( resultat.index(element))
    return resultat

def combinacions( llista, mida, solucio = []):
    if len(solucio) == mida:
        return [solucio]
    solucions = []
    for element in llista:
        s = combinacions( rem(llista, element), mida, solucio + [element])
        solucions += s
    return solucions

@counted
def backtracking( capacitat, fora, dins = [], entrem = True, temps = 0):
    if fora == []:
        return [ temps ]
    solucions = []
    if entrem:
        for esquadra in combinacions( fora, capacitat ):
            s = backtracking( capacitat, rem_list(fora, esquadra), dins + esquadra, not entrem, temps + max(esquadra))
            solucions += s 
    else:
        for umpalumpa in dins :
            s = backtracking( capacitat, fora + [umpalumpa], rem(dins, umpalumpa), not entrem, temps+umpalumpa)
            solucions += s 
    return solucions


@counted
def improved( capacitat, fora, dins = [], entrem = True, temps = 0, millor_resultat = float('inf')):
    if fora == []:
        return temps

    if entrem:
        for esquadra in combinacions( fora, capacitat ):
            esquadra_time = max(esquadra)
            if temps + esquadra_time >= millor_resultat:
                return millor_resultat
            s = improved( capacitat, rem_list(fora, esquadra), dins + esquadra, not entrem, temps + esquadra_time, millor_resultat)
            millor_resultat = min(millor_resultat, s)
    else:
        for umpalumpa in dins :
            if temps + umpalumpa >= millor_resultat:
                return millor_resultat
            s = improved( capacitat, fora + [umpalumpa], rem(dins, umpalumpa), not entrem, temps+umpalumpa, millor_resultat)
            millor_resultat = min(millor_resultat, s)
    return millor_resultat


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description='Tunnel')
    parser.add_argument('input', type=str, help='Input file')
    parser.add_argument('--algorithm', type=str, help='Type of algorithm to use', default='backtracking')
    args = parser.parse_args()

    capacitat, umpalumpes = llegir(args.input)
    solucio = None
    if args.algorithm == 'backtracking':
        solucions = backtracking( capacitat, umpalumpes )
        solucio = 0 if not solucions else min(solucions)
        # print("CALLS:", backtracking.calls)
    elif args.algorithm == 'improved':
        solucio = improved( capacitat, umpalumpes )
        # print("CALLS:", improved.calls)
    else:
        raise Exception("Unknown algorithm", args.algorithm)
    print(solucio)


# HASKELL:
# lletres abecedari llargada solucio 
#    | length solucio == llargada = [solucio]
#    | otherwise = foldl (++) [] (map (\x -> lletres abecedari llargada (x:solucio)) abecedari)
   
# main = do 
#    mapM_ (putStrLn . show) (lletres "abcde" 2 [])