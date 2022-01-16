###########################################################
#             Zaawansowane programowanie w R              #
#                 profilowanie i benchmarking             #
#                       Zajęcia 4                         #
#             Piotr Ćwiakowski, Piotr Wójcik              #
###########################################################     

# Dzisiaj nauczymy sie w jaki sposob mozna sprawdzac i porownywac
# szybkosc wykonywania kodow w R.
# Te sama rzecz (analiza, przetworzenie danych) mozna zwykle zrobic 
# w R na co najmniej kilka sposobow - jedne sa szybsze, inne wiolniejsze

# Narzedzia, ktore dzis poznamy pozwola zidentyfikowac fragmenty kodu,
# ktore wykonuja sie najdluzej, a takze porownac czas wykonania roznych
# sposobow wykonania tego samego zadania.

# nowe pakiety, ktorych bedziemy uzywac
install.packages("tictoc")
install.packages("rbenchmark")
install.packages("microbenchmark")
install.packages("profvis")

library(tictoc)
library(rbenchmark)
library(microbenchmark)
library(profvis)


#-------------------------------------------------------------------------------
# Sys.time()
# funkcja Sys.time() zwraca aktualny czas - jesli wiec wywolany ja 
# bezposrednio przed i po wlasciwym kodzie R, mozemy policzyc czas
# jego wykonania

# jak dlugo (ile sekund) trwa generowanie 10 mln liczb losowych?

# aby wynik byl wiarygodny, musimy wywolac ponisze 3 polecenia 
# JEDNOCZESNIE

poczatek <- Sys.time()
los <- runif(1e7)
koniec <- Sys.time()

# sprawdzmy roznice
koniec - poczatek


#-------------------------------------------------------------------------------
# tic() i toc() - pakiet tictoc
# funkcje tic() i toc() moga byc uzywane w podobny sposob, jak Sys.time()
# Daja jednak znacznie wieksza elastycznosc.

# tic("etykieta") - wlacza pomiar czasu, zapamietuje start i etykiete pomiaru
# toc() - wylacza pomiar czasu (ostatnio wlaczony sposrod niezamknietych), 
#         oblicza i wyswietla czas wykonania wraz z odpowiednia etykieta


# ponizsze 3 polecenia takze wywolajmy JEDNOCZESNIE

tic("generowanie") # nadajemy etykiete analizowanemu kodowi
los <- runif(1e7)
toc()

# w tym przypadku mozemy uwzglednic kilka pomiarow
# - takze zagniezdzonych w sobie, 
# UWAGA! dla pomiarow zagniezdzonych funkcja toc() zamyka
# je zawsze w odwrotnej kolejnosci niz byly otwierane

tic("calosc")
  tic("generowanie")
    x <- runif(1e6)
    y <- ifelse(x > 0.5, 1, 0)
  toc()
  tic("rysowanie")
    layout(matrix(1:2, nrow = 2, ncol = 1))
      hist(x)
      hist(y, breaks = c(0, 0.5, 1))
    layout(matrix(1))
  toc()
  tic("koncowe")
    print(summary(as.factor(y)))
    rm(x, y)
  toc()
toc()


#-------------------------------------------------------------------------------
# system.time()

# Czas wykonania polecenia badz calego bloku kodu R mozemy latwo zmierzyc
# za pomoca funkcji system.time(), ktora bezposrednio mierzy jak dlugo 
# wykonuje sie kod R, podany jako jej argument

system.time(runif(1e7))

# funkcja zwraca trzy wartosci
# użytkownik/user - czas wykonywania przez procesor kodu R
# system - ile trwaly dodatkowe operacje systemu
#          (np. dostep do plikow, przydzielanie pamieci, itp.)
# upłynęło/elapsed - lacznie oba powyzsze czasy

# za pomoca system.time() mozna zmierzyc czas trwania dluzszego 
# programu - wtedy argument trzeba wziaz w nawias klamrowy {}

system.time({
  x <- runif(1e6)
  y <- ifelse(x > 0.5, 1, 0)
  layout(matrix(1:2, nrow = 2, ncol = 1))
    hist(x)
    hist(y, breaks = c(0, 0.5, 1))
  layout(matrix(1))
  print(summary(as.factor(y)))
  rm(x, y)
  })

# sprobujmy porownac czas policzenia sredniej z numerycznej
# kolumny ramki danych na kilka sposobow

dane <- data.frame(x = rnorm(1e7))

moja_srednia1 <- function(x) {
  result <- sum(x) / length(x)
  return(result)
}

moja_srednia2 <- function(x) {
  result <- NA
  n <- length(x)
  for(i in 1:n)
   result <- sum(result, x[i], na.rm = T)
  result <- result/n
  return(result)
}

system.time(m1 <- moja_srednia1(dane$x))

system.time(m2 <- moja_srednia2(dane$x))

system.time(m3 <- mean(dane$x))

system.time(m4 <- mean(as.numeric(dane$x)))

# widac na pewno, ze najwolniej dziala wykonanie obliczen w petli, 
# ale co do pozostalych - czasy sa porownywalne.
# poza tym kazde obliczenie wykonalismy tylko raz, co nie jest
# miarodajne, bo podczas uruchamiania jednego z kodow system mogl
# akurat byc bardziej obciazony

# zawsze warto sprawdzic czy rozne podejscia daja ten sam wynik

# zalozmy, ze naszym benchmarkiem jest m3 - wynik funkcji mean()
identical(m1, m3)
identical(m1, m2)
identical(m1, m4)

# wyswwietlmy obliczone wartosci 
m1
m2
m3
m4

# wydaja sie takie same, ale...
m1 - m3
m1 - m2
m1 - m4 

# nie sa takie same - dlaczego? 
# To kwestia minimalnych roznic przy zaokraglaniu wynikow 
# czastkowych obliczen - trzeba miec tego swiadomosc 
# i nie wymagac dokladnej rownosci, ale przyjac akceptowalny
# poziom dokladnosci

(m1 == m3)
(m1 - m3 < 1e-15)


# porownanie czasow wykonania roznych kodow jest bardziej wiarygodne,
# gdy powtorzymy je wiecej niz 1 raz

# tu z pomoca przychodza pakiety rbenchmark i microbenchmark


#-------------------------------------------------------------------------------
# funkcja benchmark() z pakietu rbenchmark


# wybierzmy pierwsze 100000 obserwacji w ramce dane
# (na calym zbiorze porownanie dzialaloby zbyt dlugo)

benchmark(m1 <- moja_srednia1(dane$x[1:100000]),
          m2 <- moja_srednia2(dane$x[1:100000]),
          m3 <- mean(dane$x[1:100000]),
          m4 <- mean(as.numeric(dane$x[1:100000]))
          )

# zeby etykiety (kolumna test) byly czytelniejsze, mozemy 
# je zdefiniowac podczas wywolywania funkcji
# zapiszmy wynik porownania jako nowy obiekt

(porownaj_mean <- benchmark("moja_srednia1" = {m1 <- moja_srednia1(dane$x[1:100000])},
                           "moja_srednia2" = {m2 <- moja_srednia2(dane$x[1:100000])},
                           "mean" = {m3 <- mean(dane$x[1:100000])},
                           "mean_on_num" = {m4 <- mean(as.numeric(dane$x[1:100000]))}
                           )
  )

# znaczenie elapsed, user.self i sys.self jest identyczne 
# jak opisane powyzej dla funkcji system.time() - jest to 
# sredni czas na podstawie wszystkich powtorzen

# relative - iloraz średniego czasu wykonania danego wariantu 
#            w stosunku do najszybszego
# replications - liczba powtorzen
# user.child i system.child dotycza procesow pochodnych,
#    ktore tu nie wystapily

# wyswietlmy wynik posortowany po kolumnie relative

porownaj_mean[order(porownaj_mean$relative), c(1, 3, 4)]

# funkcja z petla jest ok. 40 razy wolniejsza od pozostalych...

# liczbe replikacji (a takze dodawanych do wyniku kolumn) mozemy 
# zmienic za pomoca odpowiednich argumentow

# zrobmy 500 powtorzen dla 10000 obserwacji

(porownaj_mean1a <- benchmark("moja_srednia1" = {m1 <- moja_srednia1(dane$x[1:10000])},
                              "moja_srednia2" = {m2 <- moja_srednia2(dane$x[1:10000])},
                              "mean" = {m3 <- mean(dane$x[1:10000])},
                              "mean_on_num" = {m4 <- mean(as.numeric(dane$x[1:10000]))},
                              columns = c("test", "replications", "elapsed", "relative"),
                              order = "relative",
                              replications = 500
                              )
  )
# Jeszcze wiecej mozliwosci oferuje pakiet microbenchmark
#-------------------------------------------------------------------------------
# funkcja microbenchmark() z pakietu microbenchmark

# funkcja jest bardzo podobna do omownionej powyzej - rowniez 
# pozwala porownywac czas wykonania kilku roznych kodow
# przy zadanej liczbie powtorzen (domyslnie 100, jak poprzednio)

# ponownie wybierzmy pierwsze 100000 obserwacji w ramce dane
# (na calym zbiorze porownanie dzialaloby zbyt dlugo)

(porownaj_mean2 <- microbenchmark("moja_srednia1" = {m1 <- moja_srednia1(dane$x[1:10000])},
                                  "moja_srednia2" = {m2 <- moja_srednia2(dane$x[1:10000])},
                                  "mean" = {m3 <- mean(dane$x[1:10000])},
                                  "mean_on_num" = {m4 <- mean(as.numeric(dane$x[1:10000]))}
                                  )
)

# wynikiem nie sa jedynie srednie czasy wykonania kodu, ale
# zestaw statystyk opisowych (min, lq, mean, median, uq oraz max)

# expr - testowane wyrazenie
# neval - liczba powtorzen (domyslnie 100)
# cld - ranking statystyczny (istotnosc roznic zaznaczona literami, jesli 
#       zainstalowany jest pakiet multcomp) - podobnie jak w testach
#       porownan wielokrotnych - wiersze z ta sama litera NIE roznia
#       sie od siebie istotnie

# czasy sa przedstawione w jednostkach dopasowanych do profilowanego
# kodu, co ulatwia ich porownanie (tu mikrosekundy).

# co wiecej, poniewaz sam proces oceny czasu wykonania funkcji trwa, 
# funkcja microbenchmark koryguje wynik o ten nawias czasowy uruchamiajac
# pewna liczbe “rozpedzajacych” iteracji (domyslnie 2) przed wlasciwymi 
# iteracjami, sluzacymi sprawdzeniu czasu wykonania kodu


# mozemy zmienic liczbe powtorzen i zamiast absolutnych czasow uzyc relatywne

(porownaj_mean2a <- microbenchmark("moja_srednia1" = {m1 <- moja_srednia1(dane$x[1:1000])},
                                  "moja_srednia2" = {m2 <- moja_srednia2(dane$x[1:1000])},
                                  "mean" = {m3 <- mean(dane$x[1:1000])},
                                  "mean_on_num" = {m4 <- mean(as.numeric(dane$x[1:1000]))},
                                  times = 500, 
                                  unit = "relative" # tez: ns, us, ms, s
                                  )
  )


# wynik funkcji microbenchmark jest zapisany jako obiekt klasy "microbenchmark"

class(porownaj_mean2a)

# mozemy go wykorzystac do narysowania wykresu
# (dwie metody: autoplot.microbenchmark oraz boxplot.microbenchmark)

# aby uzywac metody autoplot, nalezy zaladowac pakiet ggplot2

library(ggplot2)

autoplot(porownaj_mean2a)

# domyslnie os czasu jest w skali logarytmicznej
# mozemy to zmienic

autoplot(porownaj_mean2a, log = F)

# ale wtedy wykres nie staje sie bardziej czytelny...

# mozemy takze narysowac boxplot - rowniez standardowo
# os czasu ma skale logarytmiczna

boxplot(porownaj_mean2a)


# funkcja microbenchmark umozliwia tez automatyczne sprawdzenie
# wynikow porownywanych kodow za pomoca funkcji zdefiniowanej
# przez uzytkownika

# Mozemy na przyklad dodac sprawdzenie, czy wyniki uzyskiwane 
# przez poszczegolne porownywane kody sa takie same
# (na przyjetym poziomie precyzji)

# sprawdzmy czy policzone srednie roznia sie o mniej niz 1e-20

# napiszmy funkcje porownujaca wyniki (poszczegolne wartosci
# z listy 4, bo tyle kodow liczacych srednia porownujemy)
# funkcja powinna zwracac wartosc T/F - T jesli wyniki sa OK

sprawdz_rownosc_srednich <- function(wyniki) {
  dokladnosc <- 1e-20
  max_blad <- max(c(abs(wyniki[[1]] - wyniki[[2]]),
                    abs(wyniki[[1]] - wyniki[[3]]),
                    abs(wyniki[[1]] - wyniki[[4]]),
                    abs(wyniki[[2]] - wyniki[[3]]),
                    abs(wyniki[[2]] - wyniki[[4]]),
                    abs(wyniki[[3]] - wyniki[[4]])
                    )
                  )
  max_blad < dokladnosc
}

# .. i wykorzystajmy ja w funkcji microbenchmark

(porownaj_mean2 <- microbenchmark("moja_srednia1" = {m1 <- moja_srednia1(dane$x[1:10000])},
                                  "moja_srednia2" = {m2 <- moja_srednia2(dane$x[1:10000])},
                                  "mean" = {m3 <- mean(dane$x[1:10000])},
                                  "mean_on_num" = {m4 <- mean(as.numeric(dane$x[1:10000]))},
                                  check = sprawdz_rownosc_srednich
                                  )
  )

# zalozona dokladnosc jest zbyt duza...

# zobaczmy dla 1e-15

sprawdz_rownosc_srednich <- function(wyniki) {
  dokladnosc <- 1e-15
  max_blad <- max(c(abs(wyniki[[1]] - wyniki[[2]]),
                    abs(wyniki[[1]] - wyniki[[3]]),
                    abs(wyniki[[1]] - wyniki[[4]]),
                    abs(wyniki[[2]] - wyniki[[3]]),
                    abs(wyniki[[2]] - wyniki[[4]]),
                    abs(wyniki[[3]] - wyniki[[4]])
  )
  )
  max_blad < dokladnosc
}

(porownaj_mean2 <- microbenchmark("moja_srednia1" = {m1 <- moja_srednia1(dane$x[1:10000])},
                                  "moja_srednia2" = {m2 <- moja_srednia2(dane$x[1:10000])},
                                  "mean" = {m3 <- mean(dane$x[1:10000])},
                                  "mean_on_num" = {m4 <- mean(as.numeric(dane$x[1:10000]))},
                                  check = sprawdz_rownosc_srednich
)
)

# tym razem nie ma problemu


#-------------------------------------------------------------------------------
# byte compiler

# Od wersji 2.13.0 do srodowiska R wlaczony jest pakiet compiler,
# pozwalajacy na wstepna kompilacje kodu zrodlowego do kodu
# posredniego (bajtowego).

# Kod bajtowy sklada sie z ciagu instrukcji, ktore nie odpowiadaja
# bezposrednio instrukcjom procesora i moga zawierac instrukcje 
# wysokiego poziomu, jednak w przeciwienstwie do kodu zrodlowego 
# wymagaja analizy tylko pojedynczych poszczegolnych operacji.
# Powoduje to szybsze wykonywanie prekompilowanego kodu.
# https://pl.wikipedia.org/wiki/Kod_bajtowy

# Od R 2.14.0 wszystkie standardowe funkcje i pakiety z base R
# sa prekompilowane do kodu bajtowego.

# zobaczmy to na przykladzie funkcji median()

median

# Trzecia linia zawiera informacje o kodzie bajtowym funkcji.
# Oznacza to, ze pakiet compiler przetlumaczyl kod zrodlowy 
# funkcji median() z jezyka R function na inny jezyk, ktory
# moze byc zrozumiany przez bardzo szybki interpreter.

# Nie zawsze przyrost efektywnosci jest znaczny, ale
# prekompilacja jest prostym sposobem na przyspieszenie 
# wykonywanie funkcji - zwlaszcza zawierajacych pretwarzanie
# w petlach.

# pakiet compiler pozwala kompilowac pakiety podczas instalacji,
# i pojedyncze funkcje R na zyczenie.
# Moze tez kompilowac petle i funkcje podczas ich wykonania (just-in-time).

# Wiecej szczegolow:
# https://channel9.msdn.com/Events/useR-international-R-User-conferences/useR-International-R-User-2017-Conference/Taking-Advantage-of-the-Byte-Code-Compiler
# http://www.divms.uiowa.edu/~luke/R/compiler/compiler.pdf

# wczytajmy pakiet do pamieci

library(compiler)

# podstawowe funkcje: compile(), cmpfun() oraz cmpfile()

# funkcja compile() kompiluje wyrazenie i zwraca obiekt kod bajtowy,
# ktory nastepnie moze byc wykorzystany w funkcji eval()

# Na przyklad:

( a <- compile(sum(1:10)) )

eval(a)

# cmpfun() sluzy do kompilowania funkcji

# zdefiniujmy ponownie funkcje moja_srednia2

moja_srednia2 <- function(x) {
  result <- NA
  n <- length(x)
  for(i in 1:n)
    result <- sum(result, x[i], na.rm = T)
  result <- result/n
  return(result)
}

# zobaczmy jej kod
moja_srednia2

# dokonajmy wstepnej kompilacji
moja_srednia2_cmp <- cmpfun(moja_srednia2)

# zobaczmy kod prekompiloanej funkcji
moja_srednia2_cmp

# zawiera bytecode !

# aktualnie R standardowo prekompiluje wszystkie funkcje 
# podczas ich pierwszego uzycia (just-in-time, JIT)

# wylaczmy to na chwile za pomoca funkcji enableJIT()
# jedyny jej argument to poziom kompilacji
# 0 - brak prekompilacji JIT
# 1, 2, 3 = 3 najwyzszy poziom kompilacji (domyslny)

enableJIT(0)

# wywolanie funkcji wyswietla poprzednia wartosc poziomu kompilacji

benchmark("moja_srednia2" = {m1 <- moja_srednia2(dane$x[1:10000])},
          "moja_srednia2_cmp" = {m2 <- moja_srednia2_cmp(dane$x[1:10000])}
          )

# po skompilowaniu funkcja jest dwukrotnie szybsza

# przywrocmy domyslna prekompilacje

enableJIT(3)

# i porownajmy efektywnosc jeszcze raz

benchmark("moja_srednia2" = {m1 <- moja_srednia2(dane$x[1:10000])},
          "moja_srednia2_cmp" = {m2 <- moja_srednia2_cmp(dane$x[1:10000])}
          )

# tym razem nie ma wielkiej roznicy, dlatego ze funkcja moja_srednia2()
# zostala automatycznie prekompilowana przy pierwszym wywolaniu


# funkcja cmpfile() sluzy do prekompilowania kodu zapisanego
# w zewnetrznym pliku. Prekompilowany kod moze byc nastepnie
# wczytany do R przez funkcje loadcmp()

# Np. plik "moja_srednia2.R" zawiera nieskompilowany kod R

cmpfile(infile = "moja_srednia2.R",  # plik zrodlowy
        outfil = "moja_srednia2_cmp.R") # plik wynikowy

# zajrzyjmy do pliku wynikowego

# skasujmy funkcje moja_srednia2()

rm(moja_srednia2)

# wczytajmy funkcje z pliku

source("moja_srednia2.R")

moja_srednia2

# to wersja nieskompilowana

rm(moja_srednia2)

loadcmp("moja_srednia2_cmp.R")

moja_srednia2

# a to juz wersja wstepnie skompilowana



#-------------------------------------------------------------------------------
# profilowanie kodu: pakiet profvis

# Po namierzeniu kodu, ktory wykonuje sie wolno, warto zindetyfikowac
# ktore jego czesci stanowia waskie gardla - najbardziej spowalniaja
# calosc. 
# Mozemy do tego wykorzystac funkcje profvis() z pakietu profvis. 
# Wykorzystuje ona funkcje RProf() z base R do profilowania kodu
# i wyswietla podsumowanie w formie graficznej.

# Podstawowym argumentem funkcji profvis() jest profilowane wyrazenie.

profvis(
  {
    x <- rnorm(1e6)
    result <- NA
    n <- length(x)
    for(i in 1:n)
      result <- sum(result, x[i], na.rm = T)
    result <- result/n
    hist(x)
    }
  )

# Wynik przedstawiony jest w dwoch zakladkach: “Flame Graph” i “Data”.

# Zakladka "Flame graph" (wykres plomieniowy) podzielona jest na 2 panele

# Gorny panel pokazuje profilowany kod wraz z czytelnym podsumowaniem
# wskazujacym, ile czasu trwalo wykonanie poszczegolnych jego czesci
# oraz ile pamieci bylo zajete/zwolnione w kolejnych krokach.

# Dolny panel na osi poziomej ma czas w millisekundach, a wymiar 
# pionowy reprezentuje stos wywolan (ang. call stack), pokazujacy
# czy przywolane funkcje wywolywaly inne funkcje (jakie).

# Wizualizacja jest interaktywna - klikanie na poszczegolne 
# elementy wyswietla dodatkowe informacje dla danego bloku
# programowego.

# W naszym przypadku:
# pierwsze 60 ms zajelo generowanie liczb losowych za pomoca funkcji 
# rnorm(), potem R wywoluje funkcje tryCompile() z pakietu compiler, ta 
# z kolei tryCatch() -> tryCatchList(), etc. (wysoki slupek)

# najwiecej trwalo wykonywanie petli, a na koncu funkcja hist()
# wywolala kilka innych funkcji, co zajelo podobna ilosc czasu,
# jak generowanie danych


# Zakladka “Data” pokazuje czas wykonania kazdej wywolanej funkcji
# (pierwszego poziomu) - mozemy rozwinac liste wywolan kolejnych
# poziomow i zobaczyc czas wykonania poszczegolnych funkcji 
# na danym stosie.


# UWAGA!
# Funkcja profvis moze nie byc w stanie profilowac kodu, ktory wykonuje
# sie zbyt szybko - moze wtedy wystapic nastepujacy blad: 
#  Error in parse_rprof(prof_output, expr_source) : 
#  No parsing data available. Maybe your function was too fast?

# mozna wtedy zmienic domyslna wartosc argumentu interval w funkcji
# profvis() - jest to czestosc probkowania (domyslnie 0.01 = co 10 milisekund)
# Autorzy pakietu zalecaja, zeby unikac interwalow mniejszych niz 5 ms, 
# bo moze to prowadzic do niepoprawnych wynikow profilowania. 

# W przypadku bardzo szybkiego kodu profilowanie nalezy ograniczyc
# do analizy z wykorzystaniem pakietu microbenchmark, omowionego
# powyzej.

# Wiecej:
# https://www.rstudio.com/resources/webinars/profvis-profiling-tools-for-faster-r-code/
# https://rstudio.github.io/profvis/
# https://bookdown.org/rdpeng/RProgDA/profiling-and-benchmarking.html


#--------------------------------------------------------------------------
# Cwiczenia 4.

# Cwiczenie 4.1.
# Sprawdz efektywnosc czasowa roznych formul liczacych pierwiastek
# kwadratowy z wektora wartosci. Porownaj wyniki dla wektorow
# roznej dlugosci i typu (integer vs double).
# Sprawdz czy rozne formuly daja dokladnie ten sam wynik.

# przyklad
x <- 1:10000000
sqrt(x)
x**(0.5)
exp(log(x) / 2)

benchmark("sqrt1" = {sqrt(x)},
          "sqrt2" = {x**(0.5)},
          "sqrt3" = {exp(log(x) / 2)}
)



# Cwiczenie 4.2.
# Porownaj efektywnosc czasowa roznych wariantow odwolywania sie
# do pojedynczego elementu z data.frame (patrz ponizej)

dane <- data.frame(x = rnorm(1e9))
benchmark(
"1" = dane$x[100000],
"2" = dane[100000, 1],
"3" = dane[100000, "x"],
"4" = dane[[c(1, 100000)]],
"5" = dane[[1]][100000],
"6" = .subset2(dane, select = 1)[100000]
)


# https://stat.ethz.ch/R-manual/R-devel/library/base/html/base-internal.html





# Cwiczenie 4.3.
# Napisz dwa (albo wiecej) warianty funkcji znajdujacej i wyswietlajacej
# wszystkie liczby pierwsze z przedzialu [2, n], gdzie n bedzie
# jedynym argumentem funkcji.

# W jednym z wariantow wykorzystaj petle po calym wektorze,
# w drugim ,ozesz wykorzystac algorytm znany jako sito Erastotenesa:
# https://pl.wikipedia.org/wiki/Sito_Eratostenesa

# Porownaj efektywnosc czasowa obu funkcji PRZED i PO skompilowaniu
# do kodu bajtowego. Sprawdz identycznosc uzyskanych wynikow.
# Dokonaj profilowania kodu obu funkcji i opisz, ktore ich
# czesci zajmuja najwiecej czasu.





# Cwiczenie 4.4.
# Przeanalizuj funkcje zapisana w pliku cwiczenie4_4, 
# aby zrozumiec co robi.
# w razie potrzeby zainstaluj odpowiednie pakiety.
# Sprawdz efektywnosc czasowa kodu i dokonaj jego profilowania.
# Nastepnie napisz bardziej efektywna wersje funkcji.
# Ile razy szybciej sie wykonuje? Czy wynik jest identyczny?

source("cwiczenie4_4.R")

co_ja_robie(100)




