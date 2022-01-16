#############################
#      Praca domowa 2       #
#      Anna Głowacka        #
#############################

install.packages("tictoc")
library(tictoc)
install.packages("rbenchmark")
library(rbenchmark)
install.packages("microbenchmark")
library(microbenchmark)
install.packages("profvis")
library(profvis)
install.packages("compiler")
library(compiler)


# Zadanie 4.1
#Sprawdź efektywność czasową różnych formuł liczących pierwiastek kwadratowy z wektora wartości. 
#Porównaj wyniki dla wektorów różnej długości i typu (integer vs double). Sprawdź czy różne 
# formuły dają dokładnie ten sam wynik.

x <- 1:1000000
sqrt(x)
x**(0.5)
exp(log(x) / 2)
benchmark(
  "1" = sqrt(x),
  "2" = x**(0.5),
  "3" = exp(log(x) / 2)
)

# Zadanie 4.2
# Porównaj efektywność czasową różnych wariantów odwoływania się do pojedynczego elementu z 
# `data.frame` (patrz poniżej)
# https://stat.ethz.ch/R-manual/R-devel/library/base/html/base-internal.html

dane <- data.frame(x = rnorm(1e5))
benchmark(
  "1"=dane$x[10000],
  "2"=dane[10000, 1],
  "3"=dane[10000, "x"],
  "4"=dane[[c(1, 10000)]],
  "5"=dane[[1]][10000],
  "6"=.subset2(dane, select = 1)[10000]
)

# Zadanie 4.3

# Napisz dwa (albo więcej) warianty funkcji znajdującej i wyświetlającej wszystkie liczby
# pierwsze z przedziału [2, n], gdzie n będzie jedynym argumentem funkcji.
# W jednym z wariantów wykorzystaj pętle po całym wektorze, w drugim możesz wykorzystać 
# algorytm znany jako sito Erastotenesa: https://pl.wikipedia.org/wiki/Sito_Eratostene
# Porównaj efektywność czasową obu funkcji PRZED i PO skompilowaniu do kodu bajtowego. Sprawdź 
# identyczność uzyskanych wyników. Dokonaj profilowania kodu obu funkcji i opisz, które ich części 
# zajmują najwięcej czasu.

czy_p <- function(n){
  if(!(sum(n %% (1:n) == 0) > 2))
    n
}
pierwsze <- function (n){
  v <- c()
  for(i in 2:n)
    v <- c(v,czy_p(i))
  print(v)
}
pierwsze(10)


sito <- function(n) {
  c <- c(2:n)
  p <- 2
  r <- c()
  while (p*p < n) {
    r <- c(r,c[1])
    c <- c[-(which(c %% p ==0))]
    p <- c[1]
  }
  c(r,c)
}

sito(20)


ef_przed<-benchmark(
          "1" = pierwsze(20),
          "2" = sito(20)
        )

ef_przed

# Funkcja sito Erastotenesa działa duzo szybciej

skompilowane_pierwsze <- cmpfun(pierwsze)
skompilowane_sito <- cmpfun(sito)


enableJIT(0)

# Wywolanie funkcji wyawietla poprzednia wartosc poziomu kompilacji

benchmark("ef_przed_pierwsze" = {"1" <-pierwsze(20)},
          "ef_przed_sito" = {"2" <-sito(20)},
          "ef_po_pierwsze" = {"3" <- skompilowane_pierwsze},
          "ef_po_sito" = {"4" <- skompilowane_sito}
)

# Po skompilowaniu widać, że obie funkcje działają w tym samym czasie (0.00)
# Nieco wolniej działa funkcja pierwsze przed kompilacją (0.02)


# Przywracam domyslną prekompilacje

enableJIT(3)

benchmark("ef_przed_pierwsze" = {"1" <-pierwsze(20)},
          "ef_przed_sito" = {"2" <-sito(20)},
          "ef_po_pierwsze" = {"3" <- skompilowane_pierwsze},
          "ef_po_sito" = {"4" <- skompilowane_sito}
)

# Efektywność jest na podobnym poziomie, jedynie dla nieskompilowanej funkcji pierwsze, efektywność
# jest trochę gorsza

# Ponieważ wypisywanie liczb pierwszych opiera się na 2 funkcjach (funkcja pierwsze i czy_p) 
# dokonałam profilowania kodu w 2 etapach

profvis({
    if(!(sum(n %% (1:n) == 0) > 2))
      n
  })
# 50 ms zajęło sprawdzenie warunku if

# Nie udało mi się sprawdzić
profvis({
    v <- c()
    for(i in 2:n)
      v <- c(v,{if (!(sum(i %% (1:n) == 0) > 2 ){i}})
        
    print(v)
 })

profvis({
  c <- c(2:n)
  p <- 2
  r <- c()
  while (p*p < n) {
    r <- c(r,c[1])
    c <- c[-(which(c %% p ==0))]
    p <- c[1]
  }
  c(r,c)
})

# Najdłużej trwa przypisanie do c w pętli while (630 ms), następnie z while i przypisanie do 
# c wartości (c<-c(2:n)) - po 10 ms

# Zadanie 4.4
# Przeanalizuj funkcje zapisaną w pliku *cwiczenie4_4.R*, aby zrozumieć co robi.
# W razie potrzeby zainstaluj odpowiednie pakiety. Sprawdź efektywność czasową kodu 
# i dokonaj jego profilowania. Następnie napisz bardziej efektywną wersję funkcji. 
# Ile razy szybciej się wykonuje? Czy wynik jest identyczny?

source("cwiczenie4_4.R")

co_ja_robie(100)

install.packages("gridExtra")
library(ggplot2)
library(gridExtra)

co_ja_robie <- function(n) {
  
  require(ggplot2)
  require(gridExtra)
  
  wektor <- c()
  ma5 <- c()
  wynik <- c()
  
  for(i in 1:n) {
    wektor <- c(wektor, rnorm(1))
    
    if (i < 5) 
      ma5 <- c(ma5, 0) else { 
        ma5_i <- 0
        for (j in (i-4):i) 
          ma5_i <- ma5_i + wektor[j]
        ma5_i <- ma5_i / (i - (i-4) + 1)
        ma5 <- c(ma5, ma5_i)
      }
    if (i >= 5) {
      if (ma5[i] > wektor[i]) wynik[i] <- 1
      if (ma5[i] < wektor[i]) wynik[i] <- -1
      if (ma5[i] == wektor[i]) wynik[i] <- wynik[i-1]
    }
  }
  dane <- data.frame(nr = 1:n, wektor, ma5, wynik)
  
  wykres1 <- ggplot(data = dane,
                    aes(x = nr, y = wektor)) + 
    geom_line(color = "dark gray") +
    geom_line(aes(x = nr, y = ma5),
              size = 1.5, 
              colour = "red") + 
    theme_bw()
  wykres2 <- ggplot(data = dane, 
                    aes(x = nr, ymax = wynik, ymin = 0)) + 
    geom_linerange() +
    theme_bw()
  
  grid.arrange(wykres1, wykres2, nrow = 2)  
}

benchmark(m1 <- co_ja_robie(100))
# Efektywność jest dosyć słaba (elapsed=34.7)

profvis({
  require(ggplot2)
  require(gridExtra)
  
  wektor <- c()
  ma5 <- c()
  wynik <- c()
  
  for(i in 1:n) {
    wektor <- c(wektor, rnorm(1))
    
    if (i < 5) 
      ma5 <- c(ma5, 0) else { 
        ma5_i <- 0
        for (j in (i-4):i) 
          ma5_i <- ma5_i + wektor[j]
        ma5_i <- ma5_i / (i - (i-4) + 1)
        ma5 <- c(ma5, ma5_i)
      }
    if (i >= 5) {
      if (ma5[i] > wektor[i]) wynik[i] <- 1
      if (ma5[i] < wektor[i]) wynik[i] <- -1
      if (ma5[i] == wektor[i]) wynik[i] <- wynik[i-1]
    }
  }
  dane <- data.frame(nr = 1:n, wektor, ma5, wynik)
  
  wykres1 <- ggplot(data = dane,
                    aes(x = nr, y = wektor)) + 
    geom_line(color = "dark gray") +
    geom_line(aes(x = nr, y = ma5),
              size = 1.5, 
              colour = "red") + 
    theme_bw()
  wykres2 <- ggplot(data = dane, 
                    aes(x = nr, ymax = wynik, ymin = 0)) + 
    geom_linerange() +
    theme_bw()
  
  grid.arrange(wykres1, wykres2, nrow = 2)  
  
})

# Niestety nie udało mi się wykonać profilowania- kod wykonywał się dosyć długo.




