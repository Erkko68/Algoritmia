# Llinatges
El problema abordat en aquesta pràctica és la cerca dels nodes fills més allunyats donat un node d'un arbre n-ari. L'objectiu és implementar aquesta funcionalitat en dos llenguatges de programació: Python i Haskell, cadascun enfocat a resoldre el problema de manera diferent, de manera iterativa amb Python i de forma recursiva amb Haskell.
Al final, totes dues implementacions haurien de tenir el mateix cost computacional, i això és el que analitzarem en aquest document.


## Anàlisi: Implementació en Python
En aquest apartat ens centrem solament en les funcions principals considerant que ja tenim carregades a memòria les dades obtingudes del fitxer.

La primera part del nostre algorisme se centra a crear un diccionari que representarà els nodes de l'arbre, el format que seguirà aquest diccionari serà de la forma:
```
{'Parent-1':[Childs],'Parent-2':[Childs], ... ,'Parent-N':[Childs]}
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
Les dues primeres línies solament serveixen per inicialitzar el diccionari i s'executen en temps constant, el bucle `for` té cost `n` en funció del nombre de parelles que rebi la funció. Per cada parella haurà de comprovar si la clau es troba en el diccionari, encara que en diccionaris, en estar implementats amb hash tables, el cost mitjà de cerca dins d'un diccionari és constant.

Així de forma general i considerant que operacions d'inserció i cerca en un diccionari estan implementades amb hash tables, i per tant amb un cost mitjà constant, aquesta funció tindria una complexitat: O(n) en funció del nombre de parelles que rebi, ja que necessàriament ha d'iterar per totes elles en el bucle `for`.

La següent funció és l'encarregada de realitzar la cerca dels nodes fill més allunyat donat un node de l'arbre en concret:
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
La inicialització del stack (C1) serà de cost constant O(1). El bucle encarregat d'efectuar les operacions s'executarà sempre i que existeixen elements dins del stack, en el pitjor dels casos contindrà tots els elements i, per tant, el seu cost serà O(n) en funció del nombre d'elements.
L'operació pop és de cost constant O(1) i el get com hem comentat amb anterioritat tendeix a una mitjana constant O(1).

El primer condicional comprova si existeixen nodes fills després d'obtenir-los del diccionari, comprovar si existeix algun valor és cost constant i executar el `yield` també.
En cas que existeixi algun node fill els haurem de recórrer per afegir-los al stack, aquesta operació s'executaria en el pitjor dels casos O(n) en cas que un sol node contingués tots els fills de la llista. I finalment afegir cada nou node és simplement cost constant.

Per tant, podem aproximar el cost com:
```
T(n) = C1 + C2 * (C3 + C4 + C5 + C6 + (C7 * C8))
     = 1 + n * (n-1 + n-1 + n-1 + n-1 + (n * 1))
     = 1 + 5n^2 - 4n
     ≈ n^2
```
Obtenint un cost exponencial: O(n^2).

Aquest algorisme ens permet obtenir els fills del node i la profunditat en la qual es troben, a continuació requerim una funció que ens permeti classificar tots els nodes trobats en funció de la seva profunditat:
```                                              Cost Times
result_dict = {}                                 C1   1
for letter, number in generator:                 C2   n
    if number in result_dict:                    C3   n
        result_dict[number].append(letter)       C4   n
    else:
        result_dict[number] = [letter]           C5   n
```

Aquest bucle iterarà sobre tots els elements retornats per la funció anterior i agruparà cada node en funció de la seva profunditat, el cost depèn del bucle `for` i la resta d'operacions són de cost constant, per aquest motiu el cost és O(n).

I finalment agafarem els nodes més profunds del diccionari, aquells que tinguin la profunditat més gran. Per això requerim trobar el màxim entre les diferents claus del diccionari:
```
result_dict[max(result_dict.keys())]
```

El cost de la funció max() és O(n), podríem pensar que obtenir les claus del diccionari també és lineal, però amb Python3 s'utilitza una vista, operació que resulta molt eficient tenint un cost mitjà de O(1).

Aleshores podem calcular el cost total d'aquesta implementació:
```
T(n) = construct_tree(n) + search_deepest_childs(n) + result_dict(n) + max(n) =
     ≈ n + n^2 + n + n 
     ≈ n^2 + 3n
     ≈ n^2
```
Sent la funció de cerca la predominant mostrant un cost O(n^2).

## Anàlisi: Implementació en Haskell
La implementació en haskell s'ha de realitzar de forma recursiva, ja que aquesta és la gràcia del llenguatge. La implementació realitzada segueix el mateix procediment que el realitzat amb Python.

Primer convertirem les dades obtingudes del fitxer a una llista de parelles amb el format (Pare:Fill) :
```
createPairs :: [String] -> [(String, String)]   Cost Times
createPairs = map createPair                    C1    n

createPair :: String -> (String, String)
createPair str = case words str of              C2    1
    (father:child:_) -> (father, child)
    _ -> error "Invalid format. Expected 'Father child'."
```
Aquesta funció aprofita el `map` per executar la funció `createPair` per cada element de la llista. La funció `createPair` simplement converteix el string al format parella i, per tant, en un temps constant O(1). La complexitat de la funció en conjunt és de O(n) en haver d'efectuar aquesta operació constant per cada element de la llista inicial: 1*n = n -> O(n).

A continuació hem de construir un diccionari representant l'arbre que estem tractant:
```
buildTree :: [(String, String)] -> Map.Map String [String]
buildTree = foldl insertPair Map.empty
  where
    -- Inserting pairs into the tree dictionary
    insertPair tree (parent, child) = Map.insertWith (++) parent [child] tree
```
Centrant-nos en incertar les parelles al diccionari foldl `insertPair Map.empty` ha d'aplicar `insertPair` a cada parell de la llista obtenint un cost O(n) a causa de la iteració sobre la llista de n parelles.
La funció `insertPair` simplement afegeix una nova entrada al diccionari o amplia el nombre de fills d'un determinat node, el temps serà d'O(1) per a la majoria d'operacions en diccionaris moderns basats en hash. En casos rars amb col·lisions de hash o rehashing, podria ser lleugerament superior, però considerem un temps mitjà. Obtenint finalment un cost O(n) sent `map` el cost predominant.

Finalment, amb les dades creades i organitzades en un diccionari podem realitzar la cerca dels nodes:
```
deepestChildren :: Map.Map String [String] -> String -> Maybe [String]          Cost Times
deepestChildren tree key = sort <$> deepestHelper tree [key]                    C1   1

deepestHelper :: Map.Map String [String] -> [String] -> Maybe [String]
deepestHelper _ [] = Nothing                                                    C2   1
deepestHelper tree parents =
    let children = concatMap (\p -> fromMaybe [] (Map.lookup p tree)) parents   C3   n
    in if null children                                                         C4   n
       then Just parents                                                        C5   n
       else deepestHelper tree children                                         C6   n
```
La nostra funció depèn de la crida a una funció auxiliar `deepestHelper`, aquesta crida solament s'executarà un cop i per tant cost constant O(1). La funció `deepestHelper` es la que executa les crides recursives i insercions al diccionari final. El cas base és quan la parella es buida i tindrà cost constant el fet de retornar `nothing`. 
La crida recursiva es pot resumir en una iteració sobre la llista de claus representant els pares. Per a cada clau, comprova si ja existeix com a clau al diccionari d'arbre.
Si el pare existeix, obté la llista dels seus fills del diccionari.
Si el pare no existeix, considera que el pare no té fills (llista buida).
Finalment, concatena totes aquestes llistes de fills en una única llista i l'assigna a la variable fills. El cost d'aquesta operació es redueix a O(n) en concret amb l'operació `concatMap` en existir la possibilitat de concatenar tots els fills concentrats en aquell nivell, ja que la cerca `Map.looup` en taules de hash actuals sol ser un cost constant.
Finalment, aquesta operació s'executarà `d` vegades segons la profunditat de l'arbre obtenint una complexitat O(n*d), si tenim en compte el pitjor cas possible on l'estructura de l'arbre seria:
```
A -> B -> C -> D -> ... -> Z
```
I, per tant, `d = n` el cost d'aquesta funció es podria considerar O(n^2).

Si tornem a calcular el cost en conjunt del programa obtenim que:
```
T(n) = createPairs(n) + buildTree(n) + deepestChildren(n) + sort(n) = 
     ≈ n + n + n^2 + n
     ≈ n^2 + 3n
     ≈ O(n^2)
```
Que és el mateix cost obtingut en la implementació en Python.