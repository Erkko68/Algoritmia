# Llinatges
El problema abordat en aquesta pràctica és la cerca dels nodes fills més allunyats donat un node d'un arbre n-ari. L'objectiu és implementar aquesta funcionalitat en dos llenguatges de programació: Python i Haskell, cadascun enfocat a resoldre el problema de manera diferent, de manera iterativa amb Python i de forma recursiva amb Haskell.
Al final, totes dues implementacions haurien de tenir el mateix cost computacional, i això és el que analitzarem en aquest document.


## Anàlisi: Implementació en Python
En aquest apartat ens centrem solament en les funcions principals considerant que ja tenim carregades a memòria les dades obtingudes del fitxer.

La primera part del nostre algorisme es centra en crear un diccionari que representarà els nodes del arbre, el format que seguirà aquest diccionari serà de la forma:
```
{'Parent-1':[Childs],'Parent-2':[Childs],'Parent-N':[Childs]}
```
La funció encarregada de crear aquesta conversió és la mostrada a continuació i rebrà parelles Pare-Fill que anirà afegint al diccionari a retornar:
```
def construct_tree(edges):                Cost   Times
    root_value = edges[0][0]              C1     1
    tree_dict = {root_value: []}          C2     1
    for parent, child in edges:           C3     n
        if parent not in tree_dict:       C4     n
            tree_dict[parent] = []        C5     n
        tree_dict[parent].append(child)   C6     n
    return tree_dict                      C7     1 
```
Les dues primeres línies solament serveixen per inicialitzar el diccionari i s'executen en temps constant, el bucle `for` té cost `n` en funció del nombre de parelles que rebi la funció. Per cada parella haurà de comprobar si la clau es troba en el diccionari, encara que en diccionaris, al estar implementats amb hash tables, el cost mitjà de cerca dins d'un diccionari es constant.

Així de forma general i considerant que operacions d'inserció i cerca en un diccionari estan implementades amb hash tables, i per tant amb un cost mitjà constant, aquesta funció tindria una complexitat O(n) en funció del nombre de parelles que rebi ja que necessàriament ha d'iterar per totes elles en el bucle `for`.

La següent funció és la encarregada de realitzar la cerca dels nodes fill més allunyats donat un node del arbre en concret:
```
def search_deepest_childs(tree_dict, node_value):   Cost  Times
    stack = [(node_value, 0)]                       C1    1
    while stack:                                    C2    n    
        node, depth = stack.pop()                   C3    n-1
        children = tree_dict.get(node, [])          C4    n-1
        if not children:                            C5    n-1
            yield node, depth                       C6    n-1
        else:                                      
            for child in children:                  C7    n-1*n
                stack.append((child, depth + 1))    C8    n-1*n
```
La inicialització del stack (C1) serà de cost constant O(1). El bucle encarregat de realitzar les operacions s'executarà sempre i que existeixen elements dins del stack, en el pitjor dels casos contindrà tots els elements i per tant el seu cost serà O(n) en funció del nombre d'elements.
L'operació pop és de cost constant O(1) i el get com hem comentat amb anterioritat tendeix a una mitjana constant O(1).

El primer condicional comproba si existeixen nodes fills després d'obtenir-los del diccionari, comprobar si existeix algun valor es cost constant i executar el `yield` també.
En cas que existeixi algun node fill els haurem de recòrrer per afegir-los al stack, aquesta operació s'executaria en el pitjor dels casos O(n) en cas que un sol node contingués tots els fills de la llista. I finalment afegir cada nou node és simplement cost constant.

Per tant podem aproximar el cost com:
```
T(d) = C1 + C2 * (C3 + C4 + C5 + C6 + (C7 * C8))
     = 1 + n * (n-1 + n-1 + n-1 + n-1 + (n * 1))
     = 1 + 5n^2 - 4n
     ≈ n^2
```
Obtenint un cost exponencial: O(n^2).

Aquest algorisme ens permet obtenir els fills del node i la profunditat en la que es troben, a continuació requerim d'una funció que ens permeti classificar tots els nodes trobats en funció de la seva profunditat:
```                                              Cost Times
result_dict = {}                                 C1   1
for letter, number in generator:                 C2   n
    if number in result_dict:                    C3   n
        result_dict[number].append(letter)       C4   n
    else:
        result_dict[number] = [letter]           C5   n
```

Aquest bucle iterarà sobre tots els elements retornats per la funció anterior i agruparà cada node en funció de la seva profunditat, el cost depen del bucle `for` i la resta d'opreacions son de cost constant, per aquest motiu el cost és O(n).

I finalment agafarem els nodes més profunds del diccionari, aquells que tinguin la profunditat més gran. Per aixó requerim de trobar el màxim entre les diferents claus del diccionari:
```
result_dict[max(result_dict.keys())]
```

El cost de la funció max() és O(n), podriam pensar que obtenir les claus del diccionari també es lineal però amb Python3 s'utilitza una vista, operació que resulta molt efficient tenint un cost mitjà de O(1).

Aleshores podem calcular el cost total d'aquesta implementació:
```
T(d) = construct_tree(n) + search_deepest_childs(n) + result_dict(n) + max(n) =
     ≈ n + n^2 + n + n 
     ≈ n^2 + 3n
     ≈ n^2
```
Sent la funció de cerca la predominant mostrant un cost O(n^2).
