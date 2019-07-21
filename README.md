# Zawartość katalogu i kompilacja

Katalog zawiera źródła interpretera, które można skompilować poleceniem `make`. Utworzy to dwa wykonywalne pliki: 

* **interpreter** - jego znaczenie jest zgodne ze specyfikacją zadania

* **typeTest** - pomocny plik przy debugowaniu, wynik kompilacji pliku **TypeTets.hs**. Wypisuje środowisko typowe, które zostało wydedukowane w fazie inferencji typów. Uruchamia się tak samo jak **interpreter**. Przepraszam za sposób wypisywania typów - inne rzeczy wydały mi się ważniejsze.

Poza tym w katalogu gównym znajdują się trzy katalogi:

* **good** - poprawne przykłady kontrukcji językowych. Pliki **types.zak** i **test.zak** zawierają annotacje do mojego mini frameworka testowego, o którym za chwile. **dijkstra.zak** to w pełni działający przykładowy program.

* **bad**

* **test** - dwa skrypty pythonnowe: **runTest.py** uruchamia testy z pliku **good/test.zak**. **printTypes** wypisuje wyinferowane typy z plików **good/types.zak** i **bad/types.zak**. Funkcja z annotacją *// @Test: Description* powinna mieć sygnaturę `() => Bool` i w wyniku sukcesu zwracać `true`. Parsowanie pliku testowego w poszukiwaniu annotacji jest prymitywne, ważne są np. niekóre białe znaki.

Rozwiązanie było kompilowane i uruchamiane na studentsie. Może na przykład nie działać z innymi wersjami GHC.

# Krótki opis rozwiązania

Gramatyka języka została zdefiniowana w bnfc, różni się nieznacznie od gramatyki zadeklarowanej wcześniej. Gramatyka jest zdefiniowana w pliku **zaklang.cf**. Zawiera ona jeden konflikt shift/reduce - napis **foo(x)(y)** możemy potraktować jako aplikacja **foo(x)**, a później wyrażenie w nawiasach **(y)** (reduce), albo aplikacja **foo(x)(y)** (shift). Chodzi oczywiście o drugą interpretację. Na szczęście bnfc rozwiązuje ten konflikt na korzyść shift i tak też jest to obsługiwane prez interpreter

Po sprasowaniu napisu przez lexer następuje faza kontroli typów. Jest ona oparta na algorytmie Hindleya-Milnera i zdefiniowana w pliku **TypeTest.hs**. Następnie denotowany jest program. Wynikiem programu jest wynik funkcji **main**, która musi mieć sygnaturę `() => int`.
