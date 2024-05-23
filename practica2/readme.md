# Pràctica 2: Travessant el túnel

## Improved
L'esquema de la funció "improved" segueix l'estructura de backtracking amb la millora de branch-and-bound. Per implementar aquesta millora, és necessari tenir una variable que mantingui el millor resultat trobat fins al moment. Per aquest motiu, inicialitzem aquesta variable amb el valor infinit, i es va reduint a mesura que es troben noves solucions.

A continuació, podem comparar el valor de la branca que estem tractant amb el millor valor trobat fins al moment. D'aquesta manera, podem evitar continuar per aquell camí en cas que el temps excedeixi el millor valor actual.
```
if temps + umpalumpa < millor_resultat:
```
En el nostre cas, ho hem fet a la inversa i només continuem per aquella branca si el temps és menor. El resultat es compara amb el millor actual i s'actualitza.
```
millor_resultat = min(millor_resultat, s)
```
## Greedy
Per la implementació del greedy s'han realitzat 4 codis cadascún amb un enfocament diferent.
### Greedy 1
Segueix la estratègia de sempre seleccionar els més ràpids.
```
    if len(fora) < capacitat:
        return temps + max(fora)
    esquadra = fora[:capacitat]
```
### Greedy 2
Segueix la estartègia d'anar alternant entre els més ràpids i els més lents.
```
if rapids:
    esquadra = fora[:capacitat]
else:
    esquadra = fora[-capacitat:]
```
### Greedy 3
Van els més lents.
```
if len(fora) < capacitat:
    return temps + max(fora)
esquadra = fora[:capacitat]
```
### Greedy 4
Va el més ràpid anant i tornant portant amb ells els més lents fins completar la capacitat.
```
if len(fora) < capacitat:
    return temps + max(fora)
esquadra = [fora[-1]] + fora[:capacitat-1]
```

## Backtracking: Haskell
La implementació de backtracking en Haskell segueix la mateixa implementació que en python. Utilitza tres funcions auxiliars:

### remEle
S'encarrega d'eliminar un element de la llista que se li envia per paràmetre. Utilitzada quan tenim que eliminar un umpalumpa de la llista que conté els umpalumpa a fora o a dins.
```
remEle :: Eq a => [a] -> a -> [a]
remEle llista element = filter (/= element) llista
```

### remList
Utilitzada quan tenim que eliminar una esquadra de umpalumpes, simplement s'encarrega d'eliminar els elements que es troben en una llista d'un altra.
```
remList :: Eq a => [a] -> [a] -> [a]
remList llista elements = foldl (\acc element -> remEle acc element) llista elements
```
Aplica la funció remEle als elements de la llista.

### Combinacions
Es la funció que s'encarrega de generar totes les combinacions possibles donades una llista d'elements.
```
combinacions llista mida solucio
    | length llista < mida = [llista]
    | length solucio == mida = [solucio]
    | otherwise = foldr (\element acc -> acc ++ combinacions llista mida (solucio ++ [element])) [] filteredList
        where filteredList = [x | x <- llista, x `notElem` solucio]
```

### Tunel
Sent la funció principal, és la que s'encarrega de generar el backtracking. A efectes pràctics fa exàctament els mateixos processos que la implementació de Python. En funció del valor booleá en que ens trobem executarem la part del codi per entrar o per sortir.
```
 entrem = foldl (++) [] (map (\esquadra ->
      let novaFora = remList fora esquadra
          nousDins = dins ++ esquadra
          nouTemps = temps + maximum esquadra
      in tunel capacitat novaFora nousDins (not entrem) nouTemps) (combinacions fora capacitat []))
```
```
otherwise = foldl (++) [] (map (\umpalumpa ->
      let novaFora = fora ++ [umpalumpa]
          nousDins = remEle dins umpalumpa
          nouTemps = temps + umpalumpa
      in tunel capacitat novaFora nousDins (not entrem) nouTemps) (dins))
```
Cada apartat tractarà les dues llistes adientment executant les funcions per eliminar elements de cada llista i generar totes les possibles combinacions.