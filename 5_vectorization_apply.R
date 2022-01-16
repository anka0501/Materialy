###########################################################
#             Zaawansowane programowanie w R              #
#      Wektoryzacja operacji i rodzina funkcji apply      #
#                       Zajęcia 5                         #
#             Piotr Ćwiakowski, Piotr Wójcik              #
###########################################################  


# 1. Rodzina funkcji apply
# 
# a. W R jest bardzo często chcemy napisać pętlę, która będzie działa na elementach
# obiektu - tablicy, listy, wektora. Żeby programowanie takich poleceń było schlud -
# niejsze i bardziej przejrzyste, stosujemy rodzinę funkcji apply. Poszczególne 
# funkcje różnią się od siebie funkcjonalnością (jedne działają na tablicy, inne
# na liście), czy typem obiektu który zwracają (ta ostatnia właściwość również 
# przyczynia się do skrócenia pisanego kodu). Podstawowe funkcje to:

# 1. apply
# 2. lapply
# 3. sapply
# 4. vapply
# 5. tapply
# 6. mapplt


# 1. apply
# Wykonuje operacje na tablicach przebiegając jeden z jej wymiarów (1,2).

# Argumenty:
# apply(X=tablica, MARGIN=numer_wymiaru, FUN=funkcja, ...=dodatkowe_parametry_FUN)
# gdzie numer wymiaru to 1 - dla wierszy, 2 dla kolumny.

# Obiektem wynikowym zawsze jest wektor lub macierz.

# Przykład:
tablica <- cbind(c(1:8),c(10:17),c(15:8)) # Przykładowa tablica

apply(tablica,1,mean) # zwraca wektor średnich wartości w wierszach
apply(tablica,2,max)  # zwraca wektor maksymalnych wartości w kolumnach

# Jako parametru FUN możemy użyć także funkcji użytkownika, pisanej w locie - 'on the run'.
tablica2 <- apply(tablica, 1:2, function(x) x^2-x/exp(x)) 

tablica2 # wynik

# Pamiętajmy oczywiście, że można było po prostu dokonać wektoryzacji działania:
tablica3 <- tablica^2-tablica/exp(tablica)

# Porówniae obiektów
tablica2 == tablica3
identical(tablica2, tablica3)

# taka instrukcja zwróci macierz
apply(tablica,1,range)


# 2. lapply
# Funkcja lapply wykonuje następującą serię operacji:
# 1. Przebiega listę iterując po każdym elemencie tej listy
# 2. Stosuje podaną w argumencie funkcję do każdego elementu listy
# 3. Zwraca listę

# Argumenty:
# lapply(lista,funkcja,...)
# gdzie ... odpowiada za parametry podanej funkcji
# Możemy podać istniejącą już funkcję, bądź zastosować funkcję anonimową.

# Przykład:
# zdefiniujmy listę:
L <- list(a=c(1,2),b=c(3,5,7))
# Policzmy funkcję sinus:
L2 <- lapply(L,sin)
# obejrzyjmy obiekt
L2

# zastosowanie pewnej nowo zdefiniowanej funkcji do wszystkich wartości tablicy
L3 <- lapply(L, function(x){x^2}) 
L3

# A co będzie jak policzymy średnią?
L4<-lapply(L,mean)
L4 # również lista!

# Możemy skorzystać z polecenia:
unlist(L4)

# W poniższym przykładzie argumenty min i max odnoszą się do funkcji runif:
lapply(1:4, runif, min=0, max=10) 

# 3. sapply
# Funkcja sapply jest to uproszczona wersja lapply. Różnica polega na tym, że sapply:
# 1. Zwraca wektor jeśli wszystkie elementy listy były jednoelementowe
# 2. Zwraca macierz jeśli wszystkie elementy listy były tego samego wymiaru
# 3. Zwraca listę w przeciwnym wypadku

# Obiekty do przykładów:
L <- list(a=1,b=3)
L1 <- list(a=c(1,2), b=c(5,6))
L2 <- list(a=c(1,2,3), b=c(5,6))

# Przykładowe zastosowanie funckji
W <- sapply(L, function(x){x^3})
W1 <- sapply(L1, function(x){x^3})
W2 <- sapply(L2, function(x){x^3})

# Przeanalizujmy obiekty:
is.vector(W);W
is.matrix(W1);W1
is.list(W2);W2

# Teraz jeszcze z parametrem simplify ustawionym na FALSE
W <- sapply(L, function(x){x^3}, simplify = F); W # zawsze lista - zwykłe lapply
W1 <- sapply(L1, function(x){x^3}, simplify = F); W1 # zawsze lista - zwykłe lapply
W2 <- sapply(L2, function(x){x^3}, simplify = F); W2 # zawsze lista - zwykłe lapply

# 4. vapply - bezpieczniejsza i szybsza wersja lapply:

# Przykładowe dane
lista.wektorow <- sapply(3:9, seq) 

# Z nadaniem tytułów wierszom:
vapply(i39, fivenum,
       c(Min. = 0, "1st Qu." = 0, Median = 0, "3rd Qu." = 0, Max. = 0))

# 5. tapply
# Funkcja wykonująca operacje na wektorach, z możliwością ich grupowania.

# Argumenty:
# tapply(wektor, indeksy, funkcja,...)
# gdzie indeksy określa factor zawierający grupowanie elementów wektora.

# Przykład:
x <- c(rnorm(10), runif(10), rnorm(10, 1)) # tworzymy 30-elementowy wektor
kategorie <- gl(3, 10) # tworzymy 30 elementowy factor o trzech poziomach

tapply(x, kategorie, sum)

# 6. mapply
# Wielowymiarowa wersja funkcji sapply()

# mapply(FUN, ..., MoreArgs = NULL, SIMPLIFY = TRUE,
       # USE.NAMES = TRUE)

# Przykłady:
mapply(rep, 1:4, 4:1)
mapply(rep, times = 1:4, x = 4:1)
mapply(rep, times = 1:4, MoreArgs = list(x = 42))
mapply(function(x, y) seq_len(x) + y,
       c(a =  1, b = 2, c = 3),
       c(A = 10, B = 0, C = -10))


# Inne pętle, które warto znać:
# 
# do.call()

# Bardzo przydatna funkcja - wyobraźmy sobie, że chcemy wykorzystać wszystkie elementy 
# listy w jednej funckji:

przyklad <- mapply(rep, 1:4, 4:1)

do.call(c, przyklad)

# Albo przykład z pomocy:
tmp <- expand.grid(letters[1:2], 1:3, c("+", "-"))
tmp

do.call("paste", c(tmp, sep = ""))

# replicate()
# Replikuje wynik funkcji zadaną liczbę razy

library(ggplot2)
wykres <- ggplot(data = diamonds) + geom_point(aes(x = carat, y = price))

ex <- replicate(5, wykres, simplify = 'array')
ex[1,1]
ex1 <- replicate(5, wykres, simplify = F)
ex1[[1]]$data
ex1[[1]]$layers

################################################################################
# 2. Wektoryzacja operacji.
# 
# Mimo tego, że pętle są bardzo wygodne, zwykle najefektywniejszym (najszybszym) 
# i najschludniejszym sposobem programowania w R jest pisanie zwektoryzowanego,
# kodu. 

# Co to oznacza? Rozważmy na przykładach:

# 1. Sumowanie

# Tradycyjnie sumowanie liczb z wektora x:
x <- c(16, 5, 79, 18, 22, 33) 

# Wykonalibyśmy tak:
suma = 0
for (i in 1:length(x)){
  suma = suma + x[i]
}

# Ale po co, skoro można:
sum(x)

# Pomyślmy nad sumą iloczynów wartości x i y :
y <- c(12, 3, 45, 67, 19, 2)

suma.i = 0
for (i in 1:length(y)){
  suma.i = suma + x[i]*y[i]
}

# Ale po co, skoro można:
sum(x*y)

# Wektoryzacja ma znaczenie dla szybkości operacji:
# (źródło: http://alyssafrazee.com/2014/01/29/vectorization.html)

duzy.wektor = runif(1000000, 10, 100)

# funkcja logarytmująca każdy element osobno:
log_iter <- function(n){
   wynik = rep(NA, length(n))
   for(i in seq_along(n)){
     wynik[i] = log(n[i])
   }
   return(wynik)
}

# Zauważmy dużą różnicę w czasie:
system.time(log_iter(duzy.wektor))
system.time(log(duzy.wektor))

# Sprawdźmy jeszcze apply:
system.time(sapply(duzy.wektor, log))


# Kiedy pętle w R są zatem przydatne?
# 1. Kiedy niezbędne obliczenie wyników dla różnych elementów/grup (rodzina apply)
# 2. Kiedy kolejne iteracje zależą od siebie i kolejność ma znaczenie (algorytm Gibbsa)
# 3. Kiedy używamy pakietu Rcpp do pisania funkcji - będziemy jeszcze o tym mówić 
# pod koniec kursu. 


# Przeanalizujmy jeszcze poniższe przykłady z dokumentacji:
vrep <- Vectorize(rep.int)
rep.int(1:4, 4:1)
vrep(1:4, 4:1)
vrep(times = 1:4, x = 4:1)



# Albo przykład z instrukcją switch:
centre <- function(x, type){
  switch(type,
         mean = mean(x),
         median = median(x)
         )
}

# Przykładowe dane:
wektor <- rnorm(100, 10)

# Można wywołać pojedynczy wynik:
centre(wektor, 'mean')
centre(wektor, 'median')

# Ale..
centre(wektor, c('mean','median'))

# Więc:
centrev <- Vectorize(centre, vectorize.args = 'type')
centrev(wektor, c('mean','median'))

# Czasem przydatne.

################################################################################
# Ćwiczenia 
# 
# 1. Na podstawie bazy state.x77 znajdziemy średnią, medianę, max i min dla:
# liczby ludności, dochodu, oczekiwanej długości życia oraz powierzchni we wszystkich
# stanach. Wyniki zwrócimy w jednej, przejrzystej tabeli.


# 2. Przypuśćmy, że mamy bazę danych złożoną z informacji o płci badanej osoby 
# oraz wzroście. Proszę obliczyć średnią w zależności od płci (funkcją tapply). 

# Kod generujący dane:
Wzrost <- rnorm(100, mean=170, sd=10)
Kobieta <- factor(floor(2*runif(100)))
d <- data.frame(Kobieta, Wzrost)

# 3. Utwórz listę zawierającą 100 wektorów: c(1), c(1,2), c(1,2,3), ... , c(1,...,100)

# 4. Wygeneruj 10 zbiorów z parami liczb x i y. Następnie dla każdego zbioru policz 
# równanie regresji liniowej i wynik zapisz do listy.

# 5. Wyniki regresji z punktu piątego pobierz z każdego elementu listy funkcją 
# coef() i zapisz do macierzy - w kolejnych wierszach wyniki kolejnych modeli.

# 6.
# Za pomocą funkcji switch napisz formułę, która na podstawie zmiennej name stwierdzi,
# czy imię jest męskie, czy żeńskie (na podstawie ostatniej litery z wyjątkiem imion
# Kuba, Bonawentura, Barnaba).
# Następnie zwektoryzuj funkcję.
# Podpowiedź: Funkcję switch można zagnieżdżać.
# Podpowiedź: Aby dowiedzieć, się jaki jest ostatni znak w danym ciągu wykorzystaj funkcję:
# substr()

# Przykładowy ciąg imion:
# imiona=c("anTek","BAsIa","czaREK","Daria","EWELINA","filip")