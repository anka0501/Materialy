###########################################################
#             Zaawansowane programowanie w R              #
#                 R jako język obiektowy                  #
#                       Zajęcia 1                         #
#             Piotr Ćwiakowski, Piotr Wójcik              #
###########################################################     


# Każdy obiekt w programie R jest strukturą, na zasadzie struktury w języku C.
# Obiekt w programowaniu ma dwa atrybuty: 
# - pola (właściwości, informacje, dane które przechowuje )
# - metody (funkcje, które można na nim wykonać)
# 
# Każdy obiekt musi mieć swój typ zwany klasą - w ten sposób porządkujemy obiekty,
# ponieważ wszystkie obiekty tej samej klasy będą miały takie same funkcje i metody,
# a klasy podrzędne będą dziedziczyły pola i metody klas nadrzędnych (ogólnych).
# Ale więcej o tym już za chwilę.
# 
# Przydatnym narzędziem dla świadomych użytkowników jest pakiet pryr (nie mylić z plyr!!)
# który zawiera zestaw przydatnych funkcji np. do zarządzania pamięcią w R czy rozpoznawania 
# klas:
install.packages('pryr')
library(pryr)
 
# A teraz już do rzeczy. Obiekty w R zorganizowane są w pięć systemów. 
# 
# A. Base
# 
# B. S3
# 
# C. S4
# 
# D. RC lub R5
# 
# E. R6
# 
# Zostaną one szczegółowo opisane poniżej.
# 
#### A. Base (obiekty w tym systemie tworzą tylko programiści odpowiedzialni za jądro R - ostanie zmiany 
#dokonano w 2011 r.). Pełną listę tych obiektów można znaleźć np. tutaj:
#  https://kasperdanielhansen.github.io/genbioconductor/html/R_Base_Types.html

# Przyjrzyjmy się teraz najczęściej wykorzystywanym obiektom z tego systemu.
# W R najczęściej pracujemy na:
#
# Zmiennych liczbowych:
# 1. numeric (rzeczywistych)
#  a. integer - liczba całkowita
#  b. double - liczba o podwójnej precyzji
# 2. complex (zespolonych)
#
# Zmiennych dyskretnych
# 1. factor
# 2. ordered
#
# Zmiennych tekstowych
# 1. character
#
# Zmiennych logicznych
# 1. logical

## 1. Rozpoznawanie i zmiana typu zmiennych

# Do rozpoznania typu zmiennej służy komenda typeof(),
# zaś do zamiany typu (w określonych sytuacjach), czy wyświetlenia jako inny typ:

# as.{nazwa typu}]({zmienna}).

# Możemy również użyć pytania o typ: is.{nazwa typu}({zmienna}).

n = 10
typeof(n)

n = as.integer(n)
typeof(n)
is.double(n)

n<-as.numeric(n)
typeof(n)

## 2. Atomowe typy danych

### Liczby rzeczywiste
# Liczby rzeczywiste (numeric) domyślnie przechowywane są jako typ double
# (podwójnej precyzji); czasem jednak jest potrzeba użycia typu integer.
# Do Klasy numeric należą także znaki specjalne:
Inf # nieskończoność
is.numeric(Inf)
-Inf # minus nieskończoność
is.numeric(-Inf)
NaN # nie liczba (liczba nieoznaczona)
is.numeric(NaN)

# Przykład wyprodukowania stałej NaN:
0/0
Inf-Inf
Inf/Inf

### Liczby zespolone
# Zawierają część rzeczywistą i urojoną.
1+2i
typeof(1+2i)

sqrt(-1)
sqrt(-1+0i)

# Podstawowe funkcje dotyczące liczb zespolonych:
# Re
# Im
# Mod
# Arg
# Conj

### Typ logiczny
# Szczególnym typem jest typ logiczny (logical), który ma trzy możliwe wartości:
# TRUE (prawda), FALSE (fałsz) (reprezentowane także odpowiednio przez
# T lub 1 oraz F lub 0.) oraz NA (wartość brakująca). NaN jest szczególnym przypadkiem NA.
# Do wykonywania operacji na wartościach logicznych służą operatory
# & (,,i''), | (,,lub'') oraz ! (,,negacja'').
# Występuje też operator && oraz ||, który jest przeznaczony dla wartości jednowymiarowych.

T | F
TRUE & 0
c(T,F,T,F) & c(1,1,0,0)
NA & TRUE
!NA

# Wartości logiczne mogą powstawać w wyniku operacji relacyjnych na obiektach, np.
1<2
(1+2)>2^2

# Lista operatorów relacyjnych na obiektach:
# < - mniejsze niż,
# <= - mniejsze lub równe niż,
# > - większe niż,
# >= - większe lub równe niż,
# == - równe,
# != - nierówne.

### Wartość pusta
NULL
typeof(NULL)
# Oznacza ona wartość która ma być zingonorowana.

c(1,NULL,2,3)

# NULL może posłużyć do kasowania elementów listy.
# Na przykład:

d<-list(a=1,b=c(1,2))
d$b<-NULL
d

### Ciągi znaków
# Ważnym typem jest typ znakowy (character) --- przechowuje poszczególne znaki
# lub ciągi ,,sklejonych'' ze sobą znaków. Zmienne tekstowe umieszczamy w cudzysłowach.
a = 10
typeof(a)
typeof("a")
as.character(a)


## 3. Atomowe struktury danych w R.
# Są podstawą konstrukcji wszystkich obiektów w R

### Wektor - podstawowa struktura atomowa w R.
## Tworzenie wektora:
# Operator konkatenacji c():
wektor<-c(1,2,3,4,5) #Definiowanie wektora 5-elementowego
c(1,2,NULL,4) # działanie NULL
# Operator pomocniczy ':':
wektor<-1:5 #ten sam wynik
# Operator sekwencji seq():
wektor<-seq(1,100,by=2) # by określa krok sekwencji
wektor

# Wektor może zawierać dane tego samego typu atomowego.
# Możliwe jest więc uproszone tworzenie nowego wektora danego typu:
# typ(dlugosc wektora)

wektor <- numeric(6) # równoważnie double(6)
wektor
wektor <- character(3)
wektor

## Ćwiczenie 1 ##
# Porównaj wektory otrzymane w wyniku dodanie do operatora c() zmiennych
# o różnych typach atomowych.




## Ćwiczenie 2 ##
# Porównaj działania na wektorach o różnych długościach.






## Indeksowanie wektorów.
# - Poprzez podanie indeksu(ów) elementu w nawiasie kwadratowym []
w<-c("a","b","c")
w[2]

# - Poprzez maskę - w nawiasie kwadratowym podajemy wektor logiczny.
# Jesli na danej pozycji mamy wartość TRUE - element zostanie wyświetlony, wpp nie zostanie.
w<-1:5
w[c(TRUE,FALSE,TRUE,TRUE,FALSE)]
w[w<4]
w[c(TRUE,NA,NA,FALSE,FALSE)]

## Ćwiczenie 3 ##
# Sprawdź jakie działanie zajdzie w wyniku podania do wektora niepełnej maski.





### Tablice
# Są to uogólnione wektory posiadające informacje o wymiarach (dim).

## Przykłady implementacji:

# - Wymuszenie wymiarów:
w <- c("a","b","c","d","e","f")
dim(w) <- c(2,3)
w

# Możemy sprawzić atrybuty:
attributes(w)

# Wymiarom możemy przypisać nazwy:
dimnames(w) <- list(c("w1","w2"),c("k1","k2","k3"))
w

# - Tworzenie macierzy:
macierz <- matrix(0, #wartości
                  5, #liczba wierszy
                  6 #liczba kolumn
)
macierz
macierz<-matrix(1,
                5,
                6
)
macierz

macierz<-matrix(c(1,2),
                5,
                6
)
macierz

macierz<-matrix(c(1,2),
                5,
                6,
                byrow = TRUE
)
macierz

# Gdzie to się może przydać? Chociażby w funkcji layout():
layout


# Ćwiczenie 
# Podziel okno graficzne na 4 części i wstaw tam cztery wykresy (np. za pomocą funkcji plot())

# - Łączenie wektorów w wierszach (rbind) lub kolumnach (cbind)
k1 <- c(2:4)
k2 <- c(5:7)

w<-cbind(k1,k2)
w

w<-rbind(k1,k2)
w


# Ponieważ w R działania są wykonywane po elementach, warto wiedzieć w jaki sposób wykonywać
# działania na pełnych macierzach. Podtsawowe operacje:

A <- matrix(1:4,2,2)
B <- matrix(1:6,2,3)

# Mnożenie macierzy: %*%
A%*%B

# Odwracanie macierzy: solve()
solve(A)

# Transpozycja macierzy: t()
t(B)


## Ćwiczenie 4 ##
# Zbadaj jakie wyniki otrzymamy przy wykonywaniu działań na macierzach niezgodnych
# wymiarów, zarówno dla działań po elementach jak i macierzowych.

# Listy - są to wektory zawierające na współrzędnych elementy niejednorodnego typu.
# W szczególności elementem listy może być dowolny obiekt, np. inna lista.

# Tworzenie listy:
przyklad <- list(nazwa1 = c(2, 3, 5), nazwa2 = c("aa", "bb"), nazwa3 = c(T,F,F))

#wyświetlenie elementów listy
przyklad
#wyświetlenie elementu listy
przyklad["nazwa1"]
przyklad[1]
#wyświetlenie kilku elementów
przyklad[c("nazwa1","nazwa2")]
przyklad[c(1,2)]
#Odwoływanie się do wartości
przyklad[[2]]
przyklad[["nazwa1"]]
#Odwołanie się do konkretnego elementu:
przyklad[[1]][2]
#Zmiana konkretnego elementu
przyklad[[1]][1]<-4
przyklad[[1]]

# Czy czegoś to nie przypomina?
przyklad$nazwa1

# Możemy przerobić wektor na listę:
c(1,2,3)
as.list(c(1,2,3))


# Możemy też stworzyć pustą listę:
a <- list()

# Na następnych zajęciach nauczymy się zapełniać puste listy elementami, z wykorzystaniem
# pętli for oraz funkcji z rodziny apply()

# Przykład skomplikowanej listy
install.packages('ggplot2')

x<-as.numeric(1:100)
y<-x^2

d<-data.frame(x=x,y=y)

w1 <- ggplot(data=d,aes(x=x,y=y))+geom_point()

names(w1)
attributes(w1)

w1$data$x

# Przeanalizujmy teraz obiekt regresji liniowej:
data(diamonds)

fit <- lm(price~.,data=diamonds)

# Jaka to klasa?
class(fit)

# A może jednak..
is.list(fit)

# Łatwo nie da się wypisać elementów:
fit

# Ale spróbujmy wypróbowanej komendy names()
names(fit)

# A więc...

fit[[1]]

## Działanie na elementach listy:
lista<-list(Imie=LETTERS[1:6], Wzrost=166:171, Waga=55:60, Mezczyzna=c(F,F,T,F,T,T))

# Zbadajmy wenetrzną strukturę obiektu:
str(lista)

# Dokonajmy kilku operacji na liście w danej liście. Funkcja with:
lista$BMI <- lista$Waga/(lista$Wzrost^2)*100^2
lista$BMI2 <- with(lista, Waga/(Wzrost^2)*100^2)
BMI <- with(lista, Waga/(Wzrost^2)*100^2)
lista;BMI

# Ramki danych (data.frame) - są szczególnym typem listy, której elementami są wektory równej długości.
# W związku z tym mają strukturę macierzową. Przeanalizujmy przykład:

Name<-LETTERS[1:10]
Value<-1:10
Male<-c(T,F,F,T,T,T,T,F,T,F)
D<-data.frame(Name, Value, Male)

# Odwoływanie do elementów jak w listach:
D[["Name"]]
D$Value

# Uwaga przy tworzeniu ramki wektory typu character są przekonwertowywane na typ factor (o którym za chwilę).
is.factor(Name)
is.factor(D$Name)

# W celu uniknięcia konwersji, stosujemy operator I():
D2 <- data.frame(I(Name), Value, Male)
is.factor(D2$Name)
# ... lub odpowiedni argument funkcji data.frame
D3 <- data.frame(Name, Value, Male, stringsAsFactors = F)
is.factor(D3$Name)


# Factor

# Szczególny typ wektora, z dodatkową informacją o zbiorze wartości wektora, oraz
# liczności występowania każdego z elementów.

# Posiada dwa dodatkowe atrybuty:
# levels - informacja o wartościach
# table - informacja o licznościach (tablica kontyngencji)

# Przykład:
fact <- factor(c("Ciastko","Ciastko","Karmel","Czekolada","Karmel","Ciastko","Czekolada","Czekolada","Ciastko"))
levels(fact)
table(fact)

# Factor jest przechowywany jako typ integer, lecz wszelkie operacje liczbowe na factorze zwracają NA.
fact < 2 
fact - 1

# Po konwersji na typ liczbowy otrzymamy wektor liczb całkowitych,
# będących przyporządkowaniem każdej wartości pozycji w levels.
# Konwersja na tekst umożliwi ekstrakcję etykiet z factora
as.numeric(fact)
as.integer(fact)
as.character(fact)


################################################################################
### B. S3 - najprostszy i najpowszechniejszy system, najczęściej wykorzystywany
# w pakietach R-owych. 

df <- data.frame(x = seq(1,100,by = 4), y = letters[1:25])
pryr::otype(df) 

# bez pakietu R-owego trzeba by napisać:
is.object(df) & !isS4(df)

# w systemie S3 właścicielami metod nie są obiekty tylko funkcje, zwane funkcjami generycznymi.
# Żeby sie dowiedzieć, czy funkcja jest funkcją generyczną, wystarczy dokonać wyświetlenia jej 
# kody źródłowego w konsoli celem sprawdzenia, czy wywoływana jest funkcja UseMethod(), która
# odwołuje się do odpowiedniej metody,:
mean


pryr::ftype(mean) # funkcja generyczna
pryr::ftype(mean.Date) # metoda dla obiektów typu date
methods('mean') # wszystkie metody funkcji generycznych

ftype(t) # funkcja generyczna
methods('t')
ftype(t.data.frame) # transpozycja data.frame'u

ftype(t.test) # funkcja generyczna

# można również wyświetlić wszystkie metody dla danej klasy
methods(class = "data.frame")

# Dla metod można wyświetlić kod źródlowy, funkcja generyczna zawiera tylko nośnik - funkcję 
# UseMethod

print()
print.default()
print.data.frame()
print.summary.table()

summary()
summary.lm()
# Tworzenie obiektów w systemie S3
# 
# Stworzenie nowej klasy obiektów w S# odbywa się przez przypisanie obiektowi z systemu base
# atrybutu w polu klasa, co można zrobić na dwa sposoby

a <- structure(list(), class = "nowa_klasa")
class(a)

a <- list();class(a)
class(a) <- 'nowa_klasa'; class(a)

# Dziedziczenie można ustalić funkcją inherits:
inherits(a, "nowa_klasa")

# POlecenie structure() warto wykorzystywać w swoich funkcjach, żeby być pewnym jaki typ obiektu
# zostanie zwrócony przez funkcję:
nowa_funkcja <- function(x) {
  if (!is.numeric(x)) stop("X musi być numeryczne")
  structure(list(x), class = "nowa_klasa")
}

a <- nowa_funkcja(5)
a
# Tworzenie nowych metod i funkcji generycznych
# Dla danej klasy można stworzyć funkcję (metodę), która będzie pracować tylko z tą klasą.

f <- function(x) UseMethod("f")
f.a <- function(x) "Class a" # funkcja UseMethod traktuje wyrażenie po kropce jako nazwę klasy,

# a przed klasą jako nazwę funkcji generycznej (dlatego odradza się obecnie używanie w nazwach funkcji kropek)

a <- structure(list(), class = "a")
class(a)

f(a)


#################
# Ćwiczenie
# Sprawdź co się stanie, jak zmienisz w  powyższym kodzie nazwę klasy obiektu a 
# na jakąkolwiek inną.

a <- structure(list(), class = "b")
f(a)



#################

# Można zdefiniować metodę dla dowolnej klasy
f.default <- function(x) "Unknown class" 

a <- structure(list(), class = "b")
f(a)



################################################################################
### Zadania
### 

### 1. Znajdź klasy poniższych funkcji:
# floor
# +
# sum
# all
# any
# Arg





### 2. Stwórz funkcję generyczną sumuj, która będzie odwoływała się do dwóch metod:
# a) sumuj.numeric (suma liczb dla wektora numerycznego)
# b) sumuj.character (sklejenie znaków wektora tekstowego``)





### 3. Jakie metody posiada funkcja generyczna predict?






######################################################################################
### C. S4 - system bardzo podobny do S3, ale:
# a) klasy mają formalne definicje, które opisują ich pola i strukturę dziedziczenia
# b) Pojawia się specjalny operator @, który pozwala na odwoływanie się do slotów obiektów S4
# c) metody mogą być wieloargumentowe.

# S4 to bardzo skomplikowany system, na dzisiejszych zajęciach chcielibyśmy przedstawić tylko
# najważniejsze jego właściwości. W podstawowych pakietach R-owych nie ma klas S4, żeby je 
# wytłumaczyć musimy się odwołać sie np. do pakietu stats4:

library(stats4)

# W pomocy funkcji mle() możemy znaleźć:
y <- c(26, 17, 13, 12, 20, 5, 9, 8, 5, 4, 8) # tworzymy przykładowy wektor danych
nLL <- function(lambda) - sum(dpois(y, lambda, log = TRUE)) # definiujemy jednolinijkową funkcję wiarygodności
fit <- mle(nLL, start = list(lambda = 5), nobs = length(y)) # za pomocą algorytmu mle znajdujemy maksimum 
# funkcji dla parametru lambda
isS4(fit)

# Wszystkie funkcje generyczne S4
getGenerics()
getClasses()

# W S4 aby stworzyć klasę, należy ją zdefiniować, tj. nadać nazwę, sloty (pola) i opcjonalnie określić
# z jakich klas dziedziczy. Poniżej przykład:

setClass('WNE',
         slots = list(adres = 'character', dziekan = 'character'))
setClass("Sala",
         slots = list(numer = "numeric", rezerwacja = 'logical'), contains = "WNE")

# obiekty tworzymy poleceniem new:
Wydzial <- new('WNE', adres = 'Długa 44/50', dziekan = 'J.J. Michałek')
Wydzial@adres

B201 <- new('Sala', numer = 201)
B201@numer
B201@rezerwacja
B201@adres

# Definiowanie metod w systemie S4 (na przykładzie funkcji union():
union(1:10, 5:15)

df1 <- data.frame(x=1:10, y=101:110)
df2 <- data.frame(x=5:15, y=105:115)

union(df1,df2)

# Swtorzenie funkcji generycznej z funkcji union()
setGeneric("union")

union(1:10, 5:15)

# Utworzenie metidy union dla data.frame'ów
setMethod("union",
          c(x = "data.frame", y = "data.frame"),
          function(x, y) {
            unique(rbind(x, y))
          }
)

union(1:10, 5:15)
union(df1,df2)

union <- NULL
rownames(df)
# Gdzie można spotkać klasę S4? Np w pakiecie poświęconym analizie Market Basket Analysis:
# install.packages('arules')
# install.packages('arulesViz')
library(arules)
library(arulesViz)

data(Groceries)

# Obejrzyjmy strukturę danych
# Ważne przed rozpoczęciem analizy!!
tibble::glimpse(Groceries)
head(Groceries)

Groceries

# To nie jest zwykły data.frame...
Groceries@data
Groceries@itemInfo
Groceries@itemsetInfo


# Zapisanie do data.frame
a <- as(Groceries, "data.frame")

# Prawie 10 tys. reguł. Ale przecież nie o to chodziło...

# Jak stworzyć taki obiekt?
# Odpowiedź znajduje się w dokumentacji
# https://www.rdocumentation.org/packages/arules/versions/1.5-3/topics/transactions-class


##############################################################################
### Zadania

### 1. Stwórz klasę typu S4. Zdefiniuj dla niej metodę i 3 pola




### D. RC (reference class, także R5)
# najnowszy system, pojawił się w wersji R 2.1 Najważniejsze różnice:
# a) metody należą do obiektów, a nie funkcji
# b) obiekty sa modyfikowalne (mogą być zmieniane po ich utworzeniu)

# Definiowanie klas

Wydzial <- setRefClass("Wydzial",
                       fields = list(n_stud = 'numeric'))

WNE <- Wydzial$new(n_stud = 100)
WNE$n

# ponieważ obiekty można zmieniać
WNE$n_stud <- 200
WNE$n_stud

# jest specjalna metoda do kopiowania, której nie trzeba definiować
WNE_UW <- WNE$copy()

# Jednak bez dodatkowych metod ten obiekt nie jest zby użyteczny. Stwórzmy jakieś:
Wydzial <- setRefClass("Wydzial",
                       fields = list(n_stud = 'numeric'),
                       methods = list(
                         rejestracja = function(x){
                           n_stud <<- n_stud + x
                         },
                         skreslenie = function(x){
                           n_stud <<- n_stud - x
                         }
                       )
)

# I jeszcze stwórzmy klase dziedziczaca

Rocznik <- setRefClass("Rocznik",
                       contains = "Wydzial",
                       fields = list(rok_studiow = 'numeric', l_grup = 'numeric'),
                       methods = list(
                         ile_grup = function(x) {
                           if (n_stud < 20) stop('za mało studentów na roku')
                           l_grup <<- n_stud %/% 20 + 1
                         } 
                       )
)

I_rok<- Rocznik$new(rok_studiow=1, n_stud=200)
I_rok$rok_studiow
I_rok$n_stud
I_rok$ile_grup()
I_rok$l_grup

# Najnowszym i odrębnym typem obiektów, ale na razie mało popularnym jest klasa R6 dostępna w pakiecie R6:
# Więcej informacji znajdziecie tutaj:
# https://cran.r-project.org/web/packages/R6/vignettes/Introduction.html
# https://rpubs.com/wch/24456


##############################################################################
### Zadania 

### 1. Stwórz klasę typu RC. Zdefiniuj dla niej metodę i 3 pola








# Źródła internetowe:
BrowseURL('http://biecek.pl/R/RC/Jakub%20Derbisz%20R%20reference%20card%20classes.pdf')
BrowseURL('http://adv-r.had.co.nz/OO-essentials.html')
BrowseURL('http://adv-r.had.co.nz/S3.html')
BrowseURL('https://rstudio-pubs-static.s3.amazonaws.com/150296_904158e070594471864e89c10c0d14f9.html')
BrowseURL('http://www.cyclismo.org/tutorial/R/s3Classes.html')