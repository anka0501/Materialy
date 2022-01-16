###########################################################
#             Zaawansowane programowanie w R              #
#               przetwarzanie równoległe                  #
#                       Zajęcia 6                         #
#             Piotr Ćwiakowski, Piotr Wójcik              #
###########################################################     

# Dziś poznamy kolejny sposób na przyspieszenie wykonywania 
# kodów w R - wykorzystanie przetwarzania równoległego/wielowątkowego, 
# czyli równoczesnej pracy na kilku rdzeniach procesora.

# oczywiście jest to możliwe tylko w przypadku komputera
# wyposażonego w procesor wielordzeniowy, co aktualnie
# jest w zasadzie standardem.

# Teoretycznie można wykorzystywać wszystkie rdzenie do obliczeń w R,
# jednak dobrą praktyką jest pozostawienie jednego lub dwóch rdzeni
# na procesy systemowe wykonywane w tle.

# Przetwarzanie równoległe można podzielić na:
# - explicit parallelism - pozwala na kontrolę procesu 
#   tworzenia klastra obliczeniowego
# - implicit parallelism - tu przetwarzanie równoległe jest 
#   wbudowane i użytkownik może nawet nie być jego świadomy
#   (zajmuje się tym kompilator lub zewnętrzna biblioteka)

# Poniżej omówimy tylko wybrane sposoby jawnego (explicit) 
# przetwarzania równoległego.

# Dostępność sposobów zrównoleglania zależy od systemu operacyjnego,
# na którym pracujemy - najmniej elastyczny jest Windows, ale
# ze względu na jego popularność omówimy przykłady, które będą
# działały na każdym systemie operacyjnym (także na Windows)

# Niezależnie od stosowanego podejścia, przetwarzanie równoległe
# zwykle składa się z następujących kroków:
# 1. budowa klastra - inicjalizacja procesu R na różnych rdzeniach/procesorach
# 2. prześlij kod/dane z węzła głównego do pozostałych
# 3. wykonuj kod równolegle
# 4. zbierz wyniki ze wszystkich węzłów
# 5. zwiń klaster i zwolnij zasoby


# utworzmy przykladowe dane: 
# - 6 zmiennych z rozkladu N(0,1)
# - 1 mln obserwacji dla każdej

dane <- data.frame(matrix(rnorm(6e6), ncol = 6))

str(dane)

head(dane)

# dla każdej kolumny policzmy 
# - pierwiastek sumy kwadratów 
# - sumę wartości absolutnych


# zacznijmy od przypomnienia funkcji lapply(), sapply()

# lapply() bierze pierwszy parametr (wektor albo listę), 
# do każdego elementu stosuje podaną funkcję i zwraca listę.
# W przypadku data.frame stosuje obliczenia do każdej kolumny osobno

lapply(dane, 
       function(x) c(sqrt(sum(x^2)),
                     sum(abs(x)))
       )


# sapply() analogicznie, ale nie zwróci listy tylko wektor lub macierz

sapply(dane, 
       function(x) c(sqrt(sum(x^2)),
                     sum(abs(x)))
       )

# funkcje lapply() i sapply() wykonują obliczenia bardzo szybko,
# ale wykonanie ich na kilku procesorach/rdzeniach może ten proces
# jeszcze przyspieszyć.


# jedną z bibliotek umożliwiających przetwarzanie równoległe
# jest biblioteka parallel.

# Zawiera ona zróznoleglone odpowiedniki funkcji lapply(), sapply()

install.packages("parallel")
library(parallel)

# sprawdzmy liczbę dostępnych rdzeni w naszym komputerze
detectCores()

# Załóżmy, że chcemy wykorzystać wszystkie poza jednym
liczba_rdzeni <- detectCores() - 1

# Inicjujmy klaster
klaster <- makeCluster(liczba_rdzeni)

# co chwilę trwa...

# powtórzmy teraz obliczenia z użyciem równoległego wariantu
# funkcji laplly(), czyli parLapply().
# Jej pierwszym argumentem jest wykorzystywany klaster

parLapply(klaster,
          dane, 
          function(x) c(sqrt(sum(x^2)),
                        sum(abs(x))))


# albo funkcji parSapply()

parSapply(klaster,
          dane, 
          function(x) c(sqrt(sum(x^2)),
                        sum(abs(x))))

# po zakończeniu obliczeń zwijamy klaster i zwalniamy zasoby (pamięć)

stopCluster(klaster)

# domyślnym sposobem budowania klastra w makeCluster() jest PSOCK
# (Parallel Socket Cluster), w którym poszczególne rdzenie są
# inicjalizowane z pustą przestrzenią nazw. Oznacza to, że nie są
# do nich kopiowane zbiory danych czy dodatkowe pakiety wczytane
# do pamięci od momentu uruchomienia R (dodatkowe rdzenie są więc jak
# nowe sesje R - dostępne są jedynie podstawowe pakiety i pusta przestrzeń
# robocza). 

# Oznacza to, że należy do nich skopiować potrzebne dodatkowe dane
# oraz niestandardowe biblioteki.

# poniższy przykład:

potega <- 2
lapply(dane, 
       function(x) c(sqrt(sum(x^potega)),
                     sum(abs(x))))

# nie zadziała w podejściu równoległym

klaster <- makeCluster(liczba_rdzeni)
   potega <- 2
   parLapply(klaster,
             dane, 
             function(x) c(sqrt(sum(x^potega)),
                           sum(abs(x))))
stopCluster(klaster)

# ze względu na to, że obiekt potęga nie został przekazany
# do dodatkowych rdzeni (obiekt wektor przekazujemy wywołując
# na nim funkcję parLapply())

# należy po inicjalizacji klastra wyeksportować potrzebne
# obiekty do wszystkich rdzeni - służy do tego funkcja 
# clusterExport()

klaster <- makeCluster(liczba_rdzeni)
   potega <- 2
   clusterExport(klaster, 
                 "potega") # wektor nazw obiektów do eksportu
   parSapply(klaster,
             dane, 
             function(x) c(sqrt(sum(x^potega)),
                           sum(abs(x))))
stopCluster(klaster)

# teraz zadziałało bez problemu

# UWAGA! Jakiekolwiek zmiany przekazanych obiektów
# po wykonaniu eksportu są ignorowane w obliczeniach
# w klastrze, ale zmiany są widoczne po zamknięciu klastra

klaster <- makeCluster(liczba_rdzeni)
   potega <- 2
   clusterExport(klaster, 
                 "potega") # wektor nazw obiektów do eksportu
   potega <- 4 # zmieniamy wartość potęgi
   parSapply(klaster,
             dane, 
             function(x) c(sqrt(sum(x^potega)),
                           sum(abs(x))))
stopCluster(klaster)

# wynik jest identyczny jak poprzednio

# chociaż obiekt został ostatecznie zmieniony
potega

# przekazanie niestandardowej biblioteki albo funkcji 
# do klastra wymaga użycia funkcji clusterEvalQ()

# zdefiniujmy własną funkcję dla pierwiastka
pierwiastek <- function(x) sqrt(x)

# i zobaczmy czy zadziała w klastrze
klaster <- makeCluster(liczba_rdzeni)
   potega <- 2
   # powtórzmy definicję funkcji po inicjalizacji klastra
   pierwiastek <- function(x) sqrt(x)
   clusterExport(klaster, 
                 "potega") # wektor nazw obiektów do eksportu
   parSapply(klaster,
             dane, 
             function(x) c(pierwiastek(sum(x^potega)),
                           sum(abs(x))))
stopCluster(klaster)

# nie zadziałało

# musimy funkcję wyeksportować świadomie
# i zobaczmy czy zadziała w klastrze

klaster <- makeCluster(liczba_rdzeni)
   potega <- 2
   clusterEvalQ(klaster, 
                pierwiastek <- function(x) sqrt(x)) # wyrażenie, które chcemy wykonać
                                                    # w całym klastrze
   clusterExport(klaster, 
                 "potega") # wektor nazw obiektów do eksportu
   parSapply(klaster,
             dane, 
             function(x) c(pierwiastek(sum(x^potega)),
                           sum(abs(x))))
stopCluster(klaster)

# teraz działa

# podobnie jest z wykorzystaniem niestandardowych pakietów
# załóżmy, że chcemy wykorzystać funkcję basicStats()
# z pakietu fBasics

# install.packages("fBasics")

library(fBasics)

basicStats(dane$X1)

# wynik jest data.frame (de facto specjalnym typem listy) 
str(basicStats(dane$X1))

# jeśli chcemy uzyskać po prostu zestaw wartości
# możemy użyć funkcji unlist()

unlist(basicStats(dane$X1))

# wykorzystajmy tę funkcję w przetwarzaniu równoległym

klaster <- makeCluster(liczba_rdzeni)
   potega <- 2
   # wykonujemy wszystkie niezbędne polecenia w klastrze
   clusterEvalQ(klaster, 
             {pierwiastek <- function(x) sqrt(x)
              library(fBasics)
             }) 
   # w całym klastrze
   clusterExport(klaster, 
                 "potega") # wektor nazw obiektów do eksportu
   parSapply(klaster,
             dane, 
             function(x) c(pierwiastek(sum(x^potega)),
                           sum(abs(x)),
                           unlist(basicStats(x))))
stopCluster(klaster)

# Alternatywnym rozwiązaniem problemu przekazywania obiektów do rdzeni
# równoległych jest użycie klastrów typu FORK, które automatycznie
# zawierają pełną kopię przestrzeni roboczej podstawowego procesu.
# Niestety nie są one dostępne w systemie Windows.
# Na Linuxie czy Macu można je włączyć opcją: 
# makeCluster(liczba_rdzeni, type = "FORK")


#--------------------------------------------------------------------
# Nie każdą pętlę da się zastąpić za pomocą lapply() czy sapply()
# Dlatego drugim ważnym pakietem przy przetwarzaniu równoległym 
# jest foreach. 

install.packages("foreach")

library(foreach)

# Pakiet ten oferuje coś w rodzaju hybrydy między standardową pętlą
# i funkcją lapply() 

# aby zrównoleglić proces obliczeń, pakiet foreach wymaga również
# pakietu doParallel

install.packages("doParallel")
library(doParallel)

# Tu również wymagana jest inicjalizacja klastra obliczeniowego
# za pomocą funkcji registerDoParallel()

klaster <- makeCluster(liczba_rdzeni)

registerDoParallel(klaster)

# klaster zamykany jest funkcją stopImplicitCluster()

stopImplicitCluster()

# argumentem funkcji registerDoParallel() może być obiekt zwrócony
# przez funkcję makeCluster() albo liczba rdzeni do wykorzystania w klastrze

registerDoParallel(liczba_rdzeni)

stopImplicitCluster()

# Wykorzystajmy teraz funkcję foreach()
# Może być ona postrzegana jako rozszerzenie funkcji parSapply()
# pozwalająca na większą kontrolę nad tym jak zbierać wyniki
# obliczeń z poszczególnych klastrów (odpowiada za to 
# argument .combine). 

# W odróżnieniu od tradycyjnej pętli for, która wykonuje zapisany
# w niej kod kolejno dla każdej wartości "licznika" pętli (np. i),
# pętla foreach wykonuje zapisany w niej kod równolegle na podanej 
# liczbie rdzeni.

# UWAGA! funkcja foreach() ma specjalny operator %dopar% pozwalający
# na przetwarzanie równoległe (można też użyć operatora %do%, gdy
# nie chcemy zrównoleglać procesu)

registerDoParallel(liczba_rdzeni)
   foreach(kolumna = 1:6) %dopar% # licznik pętli (numer kolumny danych)
     c(pierwiastek(sum(dane[, kolumna]^2)), # wykonywane obliczenia
       sum(abs(dane[, kolumna])))
stopImplicitCluster()

# domyślnie funkcja zwraca wynik w postaci listy

# możemy kontrolować jak wyniki z poszczególnych obliczeń
# są łączone - odtwórzmy domyślne łączenie w listę (za pomocą list())

registerDoParallel(liczba_rdzeni)
   foreach(kolumna = 1:6, # licznik pętli (numer kolumny danych)
           .combine = list,
           .multicombine = TRUE) %dopar% # sposób łączenia wyników
                                         # i operator %dopar% 
           # argument .multicombine = T pozwala uniknąć uzyskania
           # zagnieżdżonej listy
  c(pierwiastek(sum(dane[, kolumna]^2)), # wykonywane obliczenia
    sum(abs(dane[, kolumna])))
stopImplicitCluster()

# wynik identyczny jak poprzednio

# alternatywnie złączmy wyniki w macierz (za pomocą rbind())

registerDoParallel(liczba_rdzeni)
      foreach(kolumna = 1:6, 
              .combine = rbind) %dopar% 
        c(pierwiastek(sum(dane[, kolumna]^2)), 
          sum(abs(dane[, kolumna])))
stopImplicitCluster()

# albo w wektor (funkcja c())
registerDoParallel(liczba_rdzeni)
   foreach(kolumna = 1:6, 
           .combine = c) %dopar% 
  c(pierwiastek(sum(dane[, kolumna]^2)),
    sum(abs(dane[, kolumna])))
stopImplicitCluster()

# zauważmy, że inaczej niż w przypadku pakietu parallel, aby wykorzystać
# w dodatkowych rdzeniach własne funkcje (tu: pierwiastek) nie musimy 
# ich eksportować

# podobnie w przypadku dodatkowych obiektów

potega <- 2

registerDoParallel(liczba_rdzeni)
   foreach(kolumna = 1:6,
           .combine = rbind) %dopar%
     c(pierwiastek(sum(dane[, kolumna]^potega)),
       sum(abs(dane[, kolumna])))
stopImplicitCluster()

# co innego, jeśli będziemy chcieli wykorzystać je do obliczeń
# równoległych wewnątrz funkcji

obliczaj <- function() {
  foreach(kolumna = 1:6,
          .combine = rbind) %dopar%
    c(pierwiastek(sum(dane[, kolumna]^potega)),
      sum(abs(dane[, kolumna])))
  }

registerDoParallel(liczba_rdzeni)
   obliczaj()
stopImplicitCluster()

# wewnątrz funkcji foreach() nie widzi funkcji pierwiastek(),
# nie zobaczy też obiektów dane czy potega

# użyjmy standardowej funkcji sqrt(), aby to sprawdzić

obliczaj <- function() {
  foreach(kolumna = 1:6,
          .combine = rbind) %dopar%
    c(sqrt(sum(dane[, kolumna]^potega)),
      sum(abs(dane[, kolumna])))
  }

registerDoParallel(liczba_rdzeni)
   obliczaj()
stopImplicitCluster()

# musimy je tam wyeksportować - odpowiada za to argument
# .export funkcji foreach()

obliczaj <- function() {
  foreach(kolumna = 1:6,
          .combine = rbind,
          # eksportujemy wszystkie potrzebne obiekty
          .export = c("dane", "potega", "pierwiastek")) %dopar%
    c(pierwiastek(sum(dane[, kolumna]^potega)),
      sum(abs(dane[, kolumna])))
}

registerDoParallel(liczba_rdzeni)
   obliczaj()
stopImplicitCluster()

# podobnie w przypadku wykorzystania funkcji z dodatkowych pakietów...

obliczaj <- function() {
  foreach(kolumna = 1:6,
          .combine = rbind,
          # eksportujemy wszystkie potrzebne obiekty
          .export = c("dane", "potega", "pierwiastek")) %dopar%
    c(pierwiastek(sum(dane[, kolumna]^potega)),
      sum(abs(dane[, kolumna])),
      # dodajemy basicStats()
      unlist(basicStats(dane[, kolumna])))
}

registerDoParallel(liczba_rdzeni)
   obliczaj()
stopImplicitCluster()

# nie działa

# dodatkowe pakiety przekażemy do lokalnego środowiska wewnątrz funkcji
# za pomocą argumentu .packages

obliczaj <- function() {
  foreach(kolumna = 1:6,
          .combine = rbind,
          # eksportujemy wszystkie potrzebne obiekty
          .export = c("dane", "potega", "pierwiastek"),
          # wczytujemy potrzebne dodatkowe pakiety
          .packages = "fBasics") %dopar%
    c(pierwiastek(sum(dane[, kolumna]^potega)),
      sum(abs(dane[, kolumna])),
      # dodajemy basicStats()
      unlist(basicStats(dane[, kolumna])))
}

registerDoParallel(liczba_rdzeni)
   obliczaj()
stopImplicitCluster()

# wykorzystanie foreach() poza funkcją nie wymaga 
# przekazywania danych czy własnych funkcji, ale
# wymaga "włączenia" niestamdardowych pakietów:

registerDoParallel(liczba_rdzeni)
   foreach(kolumna = 1:6,
           .combine = rbind,
           .packages = "fBasics") %dopar%
     c(pierwiastek(sum(dane[, kolumna]^potega)),
       sum(abs(dane[, kolumna])),
       unlist(basicStats(dane[, kolumna])))
stopImplicitCluster()

# UWAGA! sekwencja przekazana do funkcji foreach() może być
# ograniczona do elementów spełniających pewien warunek
# - wtedy wyrażenie po %do%/%dopar% jest wykonywane tylko
# dla wybranych elementów sekwencji. Służy do tego kolejny 
# specjalny operator funkcji foreach() %:% 

# np. wykonajmy obliczenia tylko dla parzystych kolumn

foreach(kolumna = 1:6,
        .combine = rbind,
        .packages = "fBasics") %:% when(kolumna %% 2 == 0) %do% # tu NIE przetwarzamy równolegle
  c(pierwiastek(sum(dane[, kolumna]^potega)),
    sum(abs(dane[, kolumna])),
    unlist(basicStats(dane[, kolumna])))

# UWAGA! nagłówki wierszy NIE zawierają informacji o numerze
# przetwarzaniej kolumny - dodajmy ją do zwracanego wyniku

foreach(kolumna = 1:6,
        .combine = rbind,
        .packages = "fBasics") %:% when(kolumna %% 2 == 0) %do% 
        # tu NIE przetwarzamy równolegle, ale sekwencyjnie 
  c(kolumna = kolumna,
    pierwiastek(sum(dane[, kolumna]^potega)),
    sum(abs(dane[, kolumna])),
    unlist(basicStats(dane[, kolumna])))


#---------------------------------------------------------------------
# Ćwiczenia 6

# podstawą dla poszczególnych ćwiczeń
# będzie poniższy model regresji

model1 <- lm(X1 ~ X2 + X3 + X4 + X5 + X6, 
             data = dane)

summary(model1)

# współczynniki i p-value uzyskaj za pomocą funkcji
# coeftest() z pakietu lmtest

library(lmtest)
coeftest(model1)

# współczynniki
as.matrix(coeftest(model1))[,1]
# p-value
as.matrix(coeftest(model1))[,4]


# Ćwiczenie 6.1
# Wykorzystując funkcje parS/Lapply() z pakietu parallel
# przeprowadź estymację modelu regresji na popróbach składających
# się z 1000 obserwacji (1-1000, 1001-2000, 2001-3000, itd.)
# zapisując wyniki każdej estymacji.

# UWAGA! dane możesz podzielić na listę mniejszych podzbiorów
# za pomocą funkcji split()

dane_lista_po1000 <- split(dane,
                           rep(1:1000, each = 1000))




# Ćwiczenie 6.2
# Analogicznie jak w ćwiczeniu 6.1. przeprowadź estymację 
# powyższego modelu na popróbach składających się z 1000 obserwacji
# (1-1000, 1001-2000, 2001-3000, itd.) korzystając z funkcji foreach()
# i wykorzystując przetwarzanie równoległe.




# Ćwiczenie 6.3
# Napisz funkcję, która będzie estymowała powyższy model
# (i zapisywała wszystkie wyniki estymacji) na kolejnych 
# rozłącznych podpróbach składających się z n obserwacji,
# gdzie n będzie jedynym argumentem funcji (ostatnia podpróba
# może mieć mniej obserwacji niż n, jeśli 1e6 nie jest podzielne
# przez n bez reszty).
# Razem z wynikami estymacji zapisuj wielkość próbki, na której
# model jest szacowany.




# Ćwiczenie 6.4(*)
# Porównaj efektywność czasową poszczególnych podejść
# z ćwiczeń 6.1, 6.2 i 6.3 - możesz w tym celu wykorzystać
# mniejszy zbiór danych - np. pierwsze 100000 obserwacji.
# Wykorzystaj funkcję benchmark() lub microbenchmark()





#---------------------------------------------------------------------
# Źródła i dodatki:
# Biecek (2017) Przewodnik po programie R, wyd. 4
# https://cran.r-project.org/web/packages/doParallel/vignettes/gettingstartedParallel.pdf
# http://gforge.se/2015/02/how-to-go-parallel-in-r-basics-tips/
# https://www.r-bloggers.com/implementing-parallel-processing-in-r/
# https://www.r-exercises.com/2017/07/06/parallel-computing-exercises-snowfall-part-1/
# https://www.r-exercises.com/2017/07/13/parallel-computing-exercises-foreach-and-doparallel-part-2/
# https://www.r-exercises.com/2017/08/03/parallel-computing-exercises-snow-and-rmpi-part-3-solutions/
