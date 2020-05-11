# Interpreter Języka Reptile

## Ogólny Opis Kodu/Rozwiązania

W chwili obecnej niestety nie da się tego nazwać rozwiązaniem/interpreterem.
Napisany kod potrafi sparsować bardzo proste programy do drzewa składni. 
Poza tym jest w repozytorium trochę kodu, który docelowo ma być odpowiedzialny 
za uruchamianie funkcji, ale ta część jest raczej jeszcze w powijakach.

| Plik Źródłowy |                             Opis                             |
|===============|==============================================================|
|`Tokenizer.hs`|Zapewnia trochę abstrakcji jeżeli chodzi o czytanie danych z pliku|
|`ExpressionParser.hs`|Funkcje związane z parsowaniem wyrażeń do obliczenia|
|`Parser.hs`|Używa Tokenizera i parsera wyrażeń do zbudowania drzewa składni z kodu pliku|
|`Intermediate.hs`|Definicje związane z formą programu, która jest rozumiana przez interpreter (nie chcę operować bezpośrednio na drzewie składni, ale też nie mam na razie ochoty pisać obsługi bytecodu w Haskellu)|
|`Main.hs`|Spina wszystko w jedną całość, na razie tylko parsuje|

Po uruchomieniu program na razie wypisze sparsowane drzewo składni:

```bash
ghc Main.hs -o reptile
# lub z innym argumentem
./reptile examples/P01.txt 
```

## Poszczególne Części Interpretera

### Parsowanie

Parser pozwala na definicje funkcji w postaci:

```
def nazwaFunkcji([(val|ref|const) argName as argType]) as retType:
  ...
```

Wewnątrz funkcji mogą być definicje kolejnych funkcji zagnieżdżonych, deklaracje
zmiennych lokalnych, instrukcje `if` oraz wyrażenia. Cały plik jest traktowany 
tak, jak ciało funkcji.

Chyba najciekawsza (być może jedyna ciekawa, poza walką z monadą `State`) część
parsera to ta odpowiedzialna za wyrażenia. Wszystkie operatory są zdefiniowane
w mapie `operators` w pliku `ExpressionParser.hs` i można nie psując parsera 
dopisać tam więcej operatorów, także dodając dodatkowe poziomy priorytetów.
Definiując operatory unarne należy pamiętać tylko, że parser pozwala na najwyżej
jeden operator unarny z danego poziomu przed wyrażeniem (więc na przykład bitowa
negacja powinna mieć inny priorytet niż unarny `-`).

### Wywoływanie Funkcji (Wykonywanie Programu)

Pomysł jest taki, że w stanie programu jest sobie mapa `Integer -> VData`, która
przedstawia **pamięć** programu i wszystkie jej elementy oprócz wartości 
zawierają także licznik referencji. Funkcje używają **ramek**, które są mapami 
`String -> Integer` do odwoływania się do zmiennych w pamięci.

Ponieważ chciałbym pozwalać na dowolnie zagnieżdżone funkcje, które można 
przypisać do zmiennych, to pomysł na wywoływanie funkcji jest trochę 
skomplikowany. Z drugiej strony chciałbym to zrobić w taki sposób, żeby
zarówno instrukcje warunkowe, jak i pętle dawały się w miarę łatwo 
zaimplementować za pomocą tych funkcji.

Reprezentacja funkcji w run-time zawiera nie tylko jej kod, ale też informacje 
potrzebne do utworzenia środowiska dla niej. Środowisko powinno być 
tworzone przed wywołaniem funkcji, jak i przed przypisaniem jej do zmiennej.
Jest realizowane przez funkcję `instantiateFunction` i polega na utworzeniu
dla funkcji **ramki** zawierającej referencje do wszystkich zmiennych wolnych 
tej funkcji.

Następnie gdy funkcja dostanie argumenty, ramka ze środowiskiem jest wzbogacana
o mapowania do wartości argumentów, które są umieszczane w pamięci 
(chyba, że to referencje). To robi `applyArgs`.

Nie wiem niestety jeszcze, jak w tym wszystkim odnajdą się planowane
funkcje–generatory.
