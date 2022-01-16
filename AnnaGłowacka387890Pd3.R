#############################
#      Praca domowa 3       #
#      Anna Głoowacka       #
#         387890            #
#############################

# Zadanie 5.1
# Na podstawie bazy state.x77 znajdziemy średnią, medianę, max i min dla: liczby ludności, dochodu, 
# oczekiwanej długości życia oraz powierzchni we wszystkich stanach. Wyniki zwrócimy w jednej, 
# przejrzystej tabeli

data(state)
head(state.x77)
baza <-state.x77
names(baza) <- cbind("Popul", "Income", "Illit", "LifeExp", "Murder", "HSGrad", "Frost", "Area")
baza <- baza[, c(1,2,4,8)]

# Statystyki dla poszczególnych kolumn
średnia<-apply(baza,2,mean)
średnia
mediana<-apply(baza,2,median)
mediana
minimum<-apply(baza,2,min)
minimum
maximum<-apply(baza,2,max)
maximum

#Statystyki zbite do jednej tabeli
tabela<-cbind(średnia,mediana, minimum, maximum )
tabela

# Zadanie 5.2
# Przypuśćmy, że mamy bazę danych złożoną z informacji o płci badanej osoby oraz wzroście.
# Proszę obliczyć średnią w zależności od płci (funkcją tapply). Kod generujący dane: 

Wzrost <- rnorm(100, mean=170, sd=10) 
Kobieta <- factor(floor(2*runif(100))) 
d <- data.frame(Kobieta, Wzrost)
tapply(d$Wzrost, Kobieta, mean)

# Zadanie 5.3
# Utwórz listę zawierającą 100 wektorów: c(1), c(1,2), c(1,2,3), ... , c(1,...,100)

c <- sapply(1:100, seq)
c

# Zadanie 5.4
# Wygeneruj 10 zbiorów z parami liczb x i y. Następnie dla każdego zbioru policz równanie 
# regresji liniowej i wynik zapisz do listy.

x <- rnorm(1000, mean=170, sd=10) 
y <- 2*runif(1000)
zbior<-data.frame(x,y)
zbior_ile<-seq(1,10, by=1)
# Tworzę 10 zbiorów
ile_zb<-split(zbior,zbior_ile)
lapply(ile_zb, function(ile_zb) lm(y~x, data=ile_zb))

# Zadanie 5.5
# Wyniki regresji z punktu piątego pobierz z każdego elementu listy funkcją coef() i zapisz 
# do macierzy - w kolejnych wierszach wyniki kolejnych modeli.

reg<-lapply(ile_zb, function(ile_zb) lm(y~x, data=ile_zb))
wektor<-sapply(reg, function(reg) coef(reg), simplify="array")
matrix(wektor, nrow=10, ncol=2, byrow = TRUE)

# Zadanie 5.6
# Za pomocą funkcji switch napisz formułę, która na podstawie zmiennej name stwierdzi, czy
# imię jest męskie, czy żeńskie (na podstawie ostatniej litery z wyjątkiem imion Kuba, 
# Bonawentura, Barnaba). Następnie zwektoryzuj funkcję. Przykładowy ciąg imion: 
# imiona <- c("anTek","BAsIa","czaREK","Daria","EWELINA","filip") 
# Podpowiedź: Funkcję switch można zagnieżdżać. Podpowiedź: Aby dowiedzieć, się jaki jest 
# ostatni znak w danym ciągu wykorzystaj funkcję: substr()

imiona <- c("boNawentura","anTek","BAsIa","czaREK","Daria","EWELINA","filip","CZesio") 
# nchar(imiona[1]) - liczy ile jest liter w imieniu anTek
#casefold(imiona[5]) - zamienia duże znaki na małe


plec<-function(i, type){
ostatnia<-substr(casefold(i),nchar(i),nchar(i))
switch(type, plec= ifelse(casefold(i)=="kuba" | casefold(i)=="bonawentura" |
                            casefold(i)=="barnaba","male",
                           ifelse(ostatnia=="a","female","male")))
}


vplec<-Vectorize(plec,vectorize.args ='type')
vplec(imiona,'plec')
plec(imiona,'plec')

# Zadanie 6.1
# Wykorzystując funkcje parS/Lapply() z pakietu parallel przeprowadź estymację modelu regresji
# na popróbach składających się z 1000 obserwacji (1-1000, 1001-2000, 2001-3000, itd.) 
# zapisując wyniki każdej estymacji. UWAGA! dane możesz podzielić na listę mniejszych podzbiorów
# za pomocą funkcji split() 

install.packages("parallel")
library(parallel)

dane <- data.frame(matrix(rnorm(6e6), ncol = 6))
typeof(dane)
dane_lista_po1000 <- split(dane, rep(1:1000, each = 1000))

# Liczenie regresji na liście podzbiorów dla lapply
lapply(dane_lista_po1000,function(dane_lista_po1000) {lm(X1 ~ X2 + X3 + X4 + X5 + X6, 
                                                        data = dane_lista_po1000)})

liczba_rdzeni <- detectCores() - 1
klaster <- makeCluster(liczba_rdzeni)
parLapply(klaster, dane_lista_po1000, function(dane_lista_po1000) {lm(X1 ~ X2 + X3 + X4 + X5 + X6, 
                                                            data = dane_lista_po1000)})
stopCluster(klaster)

# Zadanie 6.2
# Analogicznie jak w ćwiczeniu 6.1. przeprowadź estymację powyższego modelu na popróbach 
# składających się z 1000 obserwacji (1-1000, 1001-2000, 2001-3000, itd.) korzystając z 
# funkcji foreach() i wykorzystując przetwarzanie równoległe.

install.packages("doParallel")
library(doParallel)
# install.packages("lmtest")
# library(lmtest)

registerDoParallel(liczba_rdzeni)
foreach(i=dane_lista_po1000, .combine=list, .multicombine=TRUE) %dopar%{
lm(i$X1~i$X2+i$X3+i$X4+i$X5+i$X6)}
stopImplicitCluster()

# Zadanie 6.3
# Napisz funkcję, która będzie estymowała powyższy model (i zapisywała wszystkie wyniki estymacji) 
# na kolejnych rozłącznych podpróbach składających się z n obserwacji, gdzie n będzie jedynym 
# argumentem funcji (ostatnia podpróba może mieć mniej obserwacji niż n, jeśli 1e6 nie jest 
# podzielne przez n bez reszty). Razem z wynikami estymacji zapisuj wielkość próbki, na której 
# model jest szacowany.

dane <- data.frame(matrix(rnorm(6e6), ncol = 6))
typeof(dane)


proba<-function(n){
  foreach(i = dane, .combine=list, .multicombine=TRUE) %:% 
    when(1e6 %% n == 0) %dopar% {
      c(n ,model=lm(i$X1~i$X2+i$X3+i$X4+i$X5+i$X6))
    } 
  foreach(i = dane, .combine=list, .multicombine=TRUE) %:%
    when(1e6 %% n != 0) %dopar% {
      c(1e6 %% n ,model=lm(i$X1~i$X2+i$X3+i$X4+i$X5+i$X6))
    } 
}

proba<-function(n){
  
  foreach(dane = data.frame(matrix(rnorm(6e6), ncol = 6)), .combine=list, .multicombine=TRUE) %:% 
    when(1e6 %% n == 0) %dopar% {
      c(n ,model=lm(dane[,1]~dane[,2]+dane[,3]+dane[,4]+dane[,5]+dane[,6]))
    } 
  foreach(i = 1:6) %:%
    when(1e6 %% n != 0) %dopar% {
      c(1e6 %% n ,model=lm(dane[,1]~dane[,2]+dane[,3]+dane[,4]+dane[,5]+dane[,6]))
    } 
}

registerDoParallel(liczba_rdzeni)
proba(1000)
stopImplicitCluster()


n<-1000
dane<-dane[1:9999,]
dane$s<-c(rep(1:floor(nrow(dane)/n), each = n),
          rep(floor(nrow(dane)/n)+1,nrow(dane)-floor(nrow(dane)/n)*n))

dane_lista_rowne <- split(dane,dane$s)

n<-527
dane_lista_po1000 <- split(dane,
                           rep(1:floor(nrow(dane)/n)+1, each = n))

dane <- data.frame(matrix(rnorm(6e6), ncol = 6))
dane_lista_po1000 <- split(dane, rep(1:1000, each = 1000))
str(dane_lista_po1000)

n<-527
dane_lista_po1000 <- split(dane,
                           rep(1:n, each = n))
########################################

dane <- data.frame(matrix(rnorm(6e6), ncol = 6))
splt <- rep_len( 1:n , nrow(dane) )
splt
str(dane)
n<-527
dane_lista_pon<-split(dane ,f=rep_len( 1:n , nrow(dane))
  
 

