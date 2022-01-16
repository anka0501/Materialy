# Notatka:
# Szymon Lesiak 348768
# 
################################################################################
# Ćwiczenia 
# 
# 1. Na podstawie bazy state.x77 znajdziemy średnią, medianę, max i min dla:
# liczby ludności, dochodu, oczekiwanej długości życia oraz powierzchni we wszystkich
# stanach. Wyniki zwrócimy w jednej, przejrzystej tabeli.
b<-as.data.frame(state.x77)
t(do.call(data.frame, 
          list(mean = apply(b[,c(1,2,4,8)], 2, mean),
               median = apply(b[,c(1,2,4,8)], 2, median),
               min = apply(b[,c(1,2,4,8)], 2, min),
               max = apply(b[,c(1,2,4,8)], 2, max))))
lub
install.packages("fBasics")
library(fBasics)
basicStats(b[,c(1,2,4,8)])[c("Mean", "Median", "Minimum", "Maximum"),]
# 2. Przypuśćmy, że mamy bazę danych złożoną z informacji o płci badanej osoby 
# oraz wzroście. Proszę obliczyć średnią w zależności od płci (funkcją tapply). 

# Kod generujący dane:
Wzrost <- rnorm(100, mean=170, sd=10)
Kobieta <- factor(floor(2*runif(100)))
d <- data.frame(Kobieta, Wzrost)
tapply(d$Wzrost,Kobieta,mean)

# 3. Utwórz listę zawierającą 100 wektorów: c(1), c(1,2), c(1,2,3), ... , c(1,...,100)
lista1_100 <- sapply(1:100, seq)
# 4. Wygeneruj 10 zbiorów z parami liczb x i y. Następnie dla każdego zbioru policz 
# równanie regresji liniowej i wynik zapisz do listy.
x<-rnorm(10000,mean=5,sd=2)
y<-3*x+rnorm(10000)
df<-data.frame(x,y)
df$s<-seq(1,10, by=1)
#bardziej zyciowe jest ze bede mial jedna baz danych i bde musial podzielic ja na mniejsze
lsplit<-split(df,df$s)
#wyciagam elmenty listy data frameow
for(i in 1:length(lsplit)){
  assign(paste0("df", i), lsplit[[i]])
}
#do wielokrotnych regresji nawet wygodniej ze podzielone zbiorki są listą
reg<-lapply(lsplit,function(lsplit) lm(y~x,data=lsplit))

# 5. Wyniki regresji z punktu piątego pobierz z każdego elementu listy funkcją 
# coef() i zapisz do macierzy - w kolejnych wierszach wyniki kolejnych modeli.
test<-sapply(reg,function(reg) coef(reg),simplify = "array")
matrix(test, nrow=10, ncol=2, byrow = TRUE)

# 6.
# Za pomocą funkcji switch napisz formułę, która na podstawie zmiennej name stwierdzi,
# czy imię jest męskie, czy żeńskie (na podstawie ostatniej litery z wyjątkiem imion
# Kuba, Bonawentura, Barnaba).
# Następnie zwektoryzuj funkcję.
# Podpowiedź: Funkcję switch można zagnieżdżać.
# Podpowiedź: Aby dowiedzieć, się jaki jest ostatni znak w danym ciągu wykorzystaj funkcję:
# substr()
# Przykładowy ciąg imion:


imiona=c("Bonawentura","BAsIa","czaREk","Daria","EWELINA","filip")
gender <- function(var, type){
  switch(type,
         gender=  ifelse(toupper(var)=="KUBA"|
                           toupper(var)=="BONAWENTURA"|
                           toupper(var)=="BARNABA","male",
                         ifelse(substr(toupper(var),nchar(var),nchar(var))=="A"
                                ,"female","male")),
         first_letter=toupper(substr(var,1,1)),
         last_letter=toupper(substr(toupper(var),nchar(var),nchar(var))))
}

vgender<-Vectorize(gender,vectorize.args =c('type'))
gender(imiona,'last_letter')
gender(imiona,'gender')
gender(imiona,c('last_letter','gender'))

vgender(imiona,'first_letter')
vgender(imiona,'last_letter')
vgender(imiona,'gender')
vgender(imiona2,c('last_letter','gender'))


# �wiczenie 6.1
# Wykorzystuj�c funkcje parS/Lapply() z pakietu parallel
# przeprowad� estymacj� modelu regresji na popr�bach sk�adaj�cych
# si� z 1000 obserwacji (1-1000, 1001-2000, 2001-3000, itd.)
# zapisuj�c wyniki ka�dej estymacji.

# UWAGA! dane mo�esz podzieli� na list� mniejszych podzbior�w
# za pomoc� funkcji split()

dane_lista_po1000 <- split(dane,
                           rep(1:100, each = 1000))

klaster <- makeCluster(liczba_rdzeni)
reg61<-parLapply(klaster,
                 dane_lista_po1000, 
                 function(dane_lista_po1000) lm(X1 ~ X2 + X3 + X4 + X5 + X6,
                                                data=dane_lista_po1000 ))
stopCluster(klaster)
# �wiczenie 6.2
# Analogicznie jak w �wiczeniu 6.1. przeprowad� estymacj� 
# powy�szego modelu na popr�bach sk�adaj�cych si� z 1000 obserwacji
# (1-1000, 1001-2000, 2001-3000, itd.) korzystaj�c z funkcji foreach()
# i wykorzystuj�c przetwarzanie r�wnoleg�e.

registerDoParallel(liczba_rdzeni)
reg62<-foreach(i=dane_lista_po1000,
               .combine = list,
               .multicombine = T) %dopar% {
                 
                 lm(i$X1 ~ 
                      i$X2 + 
                      i$X3 + 
                      i$X4 +
                      i$X5 + 
                      i$X6
                 )}
stopImplicitCluster()

# Zadanie 6.3

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

