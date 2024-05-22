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
    if len(llista) < mida: # He afegit aquesta condició
        return [llista]
    if len(solucio) == mida:
        return [solucio]
    solucions = []
    for element in [x for x in llista if x not in solucio]:
        s = combinacions( llista, mida, solucio + [element])
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
            if temps + esquadra_time < millor_resultat:
                s = improved( capacitat, rem_list(fora, esquadra), dins + esquadra, not entrem, temps + esquadra_time, millor_resultat)
                millor_resultat = min(millor_resultat, s)
    else:
        for umpalumpa in dins :
            if temps + umpalumpa < millor_resultat:
                s = improved( capacitat, fora + [umpalumpa], rem(dins, umpalumpa), not entrem, temps+umpalumpa, millor_resultat)
                millor_resultat = min(millor_resultat, s)
    return millor_resultat

@counted
def greedy(capacitat, fora, dins = [], entrem = True, temps = 0):
    if not fora:
        return temps

    # Ordenem la llista d'elements fora per processar de forma voraç
    fora.sort()

    # Estratègia voraç: sempre seleccionar els més ràpids
    if entrem:
        # Comprovar si hi ha suficients elements per formar una esquadra
        if len(fora) < capacitat:
            return temps + max(fora)
        # Formar la millor esquadra possible (els més ràpids) amb la capacitat permesa
        esquadra = fora[:capacitat]
        # Calcular el temps per aquesta esquadra
        esquadra_time = max(esquadra)
        # Eliminar els elements de la esquadra dels elements de fora
        fora = fora[capacitat:]
        # Afegir la esquadra als elements de dins
        dins.extend(esquadra)
        # Actualizar el temps total acumulat
        temps += esquadra_time
    else:
        # Seleccionar l'element més ràpid dins per a que torni
        umpalumpa = min(dins)
        # Eliminar aquest element de la llista de dins
        dins.remove(umpalumpa)
        # Afegir aquest element a la llista de fora
        fora.append(umpalumpa)
        # Actualitzar el temps total acumulat
        temps += umpalumpa

    # Continuar el procés recursivament canviant l'estat d'entrem
    return greedy(capacitat, fora, dins, not entrem, temps)

@counted
def greedy2(capacitat, fora, dins = [], entrem = True, rapids = True, temps = 0):
    if not fora:
        return temps

    # Ordenem la llista d'elements fora per processar de forma voraç
    fora.sort()

    # Estratègia voraç: sempre seleccionar els més ràpids
    if entrem:
        # Comprovar si hi ha suficients elements per formar una esquadra
        if len(fora) < capacitat:
            return temps + max(fora)
        if rapids:
            # Formar la millor esquadra possible (els més ràpids) amb la capacitat permesa
            esquadra = fora[:capacitat]
        else:
            esquadra = fora[-capacitat:]
        rapids = not rapids
        # Calcular el temps per aquesta esquadra
        esquadra_time = max(esquadra)
        # Eliminar els elements de la esquadra dels elements de fora
        fora = fora[capacitat:]
        # Afegir la esquadra als elements de dins
        dins.extend(esquadra)
        # Actualizar el temps total acumulat
        temps += esquadra_time
    else:
        # Seleccionar l'element més ràpid dins per a que torni
        umpalumpa = min(dins)
        # Eliminar aquest element de la llista de dins
        dins.remove(umpalumpa)
        # Afegir aquest element a la llista de fora
        fora.append(umpalumpa)
        # Actualitzar el temps total acumulat
        temps += umpalumpa

    # Continuar el procés recursivament canviant l'estat d'entrem
    return greedy2(capacitat, fora, dins, not entrem, rapids, temps)

@counted
def greedy3(capacitat, fora, dins = [], entrem = True, temps = 0):
    if not fora:
        return temps

    # Ordenem la llista d'elements fora per processar de forma voraç
    fora = sorted(fora, reverse=True)

    # Estratègia voraç: sempre seleccionar els més ràpids
    if entrem:
        # Comprovar si hi ha suficients elements per formar una esquadra
        if len(fora) < capacitat:
            return temps + max(fora)
        # Formar la millor esquadra possible (els més ràpids) amb la capacitat permesa
        esquadra = fora[:capacitat]
        # Calcular el temps per aquesta esquadra
        esquadra_time = max(esquadra)
        # Eliminar els elements de la esquadra dels elements de fora
        fora = fora[capacitat:]
        # Afegir la esquadra als elements de dins
        dins.extend(esquadra)
        # Actualizar el temps total acumulat
        temps += esquadra_time
    else:
        # Seleccionar l'element més ràpid dins per a que torni
        umpalumpa = min(dins)
        # Eliminar aquest element de la llista de dins
        dins.remove(umpalumpa)
        # Afegir aquest element a la llista de fora
        fora.append(umpalumpa)
        # Actualitzar el temps total acumulat
        temps += umpalumpa

    # Continuar el procés recursivament canviant l'estat d'entrem
    return greedy3(capacitat, fora, dins, not entrem, temps)

@counted
def greedy4(capacitat, fora, dins = [], entrem = True, temps = 0):
    if not fora:
        return temps

    # Ordenem la llista d'elements fora per processar de forma voraç
    fora = sorted(fora, reverse=True)

    # Estratègia voraç: sempre seleccionar els més ràpids
    if entrem:
        # Comprovar si hi ha suficients elements per formar una esquadra
        if len(fora) < capacitat:
            return temps + max(fora)
        # Formar la millor esquadra possible (els més ràpids) amb la capacitat permesa
        esquadra = [fora[-1]] + fora[:capacitat-1]
        # Calcular el temps per aquesta esquadra
        esquadra_time = max(esquadra)
        # Eliminar els elements de la esquadra dels elements de fora
        fora = fora[capacitat-1:-1]
        # Afegir la esquadra als elements de dins
        dins.extend(esquadra)
        # Actualizar el temps total acumulat
        temps += esquadra_time
    else:
        # Seleccionar l'element més ràpid dins per a que torni
        umpalumpa = min(dins)
        # Eliminar aquest element de la llista de dins
        dins.remove(umpalumpa)
        # Afegir aquest element a la llista de fora
        fora.append(umpalumpa)
        # Actualitzar el temps total acumulat
        temps += umpalumpa

    # Continuar el procés recursivament canviant l'estat d'entrem
    return greedy4(capacitat, fora, dins, not entrem, temps)

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
        solucio = 0 if solucio == float('inf') else solucio
        # print("CALLS:", improved.calls)
    elif args.algorithm == 'greedy':
        solucio = greedy( capacitat, umpalumpes )
        solucio = 0 if solucio == float('inf') else solucio
    elif args.algorithm == 'greedy2':
        solucio = greedy2( capacitat, umpalumpes )
        solucio = 0 if solucio == float('inf') else solucio
        # print("CALLS:", greedy2.calls)
    elif args.algorithm == 'greedy3':
        solucio = greedy3( capacitat, umpalumpes )
        solucio = 0 if solucio == float('inf') else solucio
        # print("CALLS:", greedy3.calls)
    elif args.algorithm == 'greedy4':
        solucio = greedy4( capacitat, umpalumpes )
        solucio = 0 if solucio == float('inf') else solucio
        # print("CALLS:", greedy4.calls)
    else:
        raise Exception("Unknown algorithm", args.algorithm)
    print(solucio)
