###########################################################
#             Zaawansowane programowanie w R              #
#                 R jako język funkcyjny                  #
#                       Zajęcia 2                         #
#             Piotr Ćwiakowski, Piotr Wójcik              #
###########################################################     

# Na poprzednich zajęciach poznaliśmy podstawowe obiekty w R.
# U swojego zarania, w swoim jądrze, R jest jednak językiem funkcyjnym.
# Dzięki temu kod można organizować w funkcje, które w R można traktować 
# jak obiekty (można tworzyć z nich listy, przypisywać do obiektów, przenosić
# pomiędzy różnymi środowiskami, podawać jako argumenty innych funkcji). 

# Po co używamy funkcji w programowaniu?
# a) żeby unikać wielokrotnego powtarzania kodu wykonującego takie same operacje,
# b) żeby organizować kod w spójne i wyraźne sekcje (funkcja przyjmuje argumenty
# i zwraca wartości)
# c) biorąc pod uwagę a) i b) funkcje poprawiają czytelność naszego kodu.


# Na dzisiejszych zajęciach nauczymy się pisać proste funkcje. Ale zanim to
# nastąpi, upewnijmy się że znamy dwa podstawowe polecenia używane w programowaniu:
# * instrukcja if
# * instrukcja for


####################################################################################
## Instrukcja warunkowa:

# Jednym z podstawowych elementów programowania (w dowolnym języku) jest stosowanie
# instrukcji warunkowej. W zależności od wartości logicznej danego wyrażenia chcemy,
# aby nasz program wykonał albo jeden, albo drugi blok instrukcji.

## 1.1 Instrukcja if.
# Składnia: if ( WARUNEK ) { co jeśli TAK } else { co jeśli NIE }
# Składnia rozbudowana dla wielu warunków:
# if ( WARUNEK1 ) { co jeśli TAK } else if ( WARUNEK2 ) { co jeśli TAK} else if ... else { co w pozostałym przypadku}

###  Wyrażenia relacyjne i logiczne - przypomnienie:
# == przypisanie
# !=
# <=
# <
# >=
# >
# & koniunkcja
# | alternatywa łączna

# Przypomnijmy, że w R alternatywę warunków deklaruje się symbolem | (dla wektorów)
# lub || (dla pojedynczych wartości), a koniunkcję odpowiednio symbolami & oraz &&.
# Oczywiście dla jednoelementowych wektorów symboli można używać zamiennie.
# Dodatkowo możemy używać funkcji all() oraz any() oznaczających odpowiednio koniunkcję
# i alternatywę dla więcej niż dwóch warunków.

c(TRUE,FALSE) || c(FALSE,FALSE)
c(TRUE,FALSE) && c(FALSE,TRUE)

all(any(1+2 == 3, 2+3 == 4, 3+4 == 5),2+3 == 5,3+4 == 7)


if (2+2 == 4 | 1+3 == 5) {"Hello"} else {"World"}
if (2+2 == 4 & 1+3 == 5) {"Hello"} else {"World"}

# Inne przykłady
a = 3
b = 1
c = 4

#############################
macierz <- matrix(c(1,2),
                  5,
                  6,
                  byrow = TRUE
)
macierz
############################

if(a == 3){
  macierz[a, 1] = 100
}
macierz

if(b != 2){
  macierz[a,1] = 101
}

macierz

if(a == 4 | (b == 1 & c == 4) ){
  macierz[a,1] = 105
}
macierz


####################################################################################
## Ćwiczenie 1 ##
# Napisz instrukcję warunkową, która dla danych zmiennych a, b zwróci ich sumę,
# jeżeli były to liczby; ich konkatenację (sklejenie), jeśli były to zmienne tekstowe
# oraz wyraz ,,błąd'' w innych przypadkach.

# Funkcja pozwalająca na konkatenację: paste0





####################################################################################
## Funkcja ifelse - wektorowy odpowiednik instrukcji if
# ifelse({warunek na wektorach}, {co jeśli TAK}, {co jeśli NIE})
# W wyniku powstaje wektor o wymiarach takich jak ten w warunku.

# Przykład:
ifelse(c(1:5)%%2 == 1,"nieparzysta","parzysta")
# Symbol %% oznacza resztę z dzielenia:
8%%2
7%%3


####################################################################################
## Pętla for

wektor <- 1:10

#Składnia:
# for (i in {zbiór indeksów}){ POLECENIE }

#Dosłownie: dla i=1 do 10 wykonaj polecenia wewnątrz pętli (ograniczone nawiasami klamrowymi)
for(i in 1:10){
  #polecenia wewnątrz pętli np.:
  print(wektor)
}

for(i in 1:10){
  #polecenia wewnątrz pętli np.:
  print(wektor[i])
}
#Można też indeksować
wek<-seq(1,20)
wek
for(i in 1:20){
  print(wek[i])
}
#jako granice przedziałów można wybrać wynik funkcji
for(i in 1:length(wek)){
  print(wek[i])
}
#można wybrane wartości
for(i in c(2,5,10,20)) {
  print(wek[i])
}
#można też co drugi
for(i in seq(1,length(wek),by=2)){
  print(wek[i])
}

#Przykład. stwórzmy macierz i wypełnijmy ją wartościami:

macierz<-matrix(0,10,2)

for(i in 1:10){
  macierz[i,1]<-i/5
  macierz[i,2]<-i*2
}
macierz

#Przykład 2
#Pętle można zagnieżdżać.
liczba <- 1

for(i in 1:10){
  for(j in 1:2){
    macierz[i,j] <- liczba
    liczba <- liczba + 1
  }
}
macierz


####################################################################################
# 3. Pętla while
# Składnia:
# while( WARUNEK ){ POLECENIE }
# Słownie: dopóki WARUNEK jest spełniony wykonuj POLECENIE.
# W pętli while, aby była ona skończona POLECENIE musi zawierać fragment, który wpływa na WARUNEK.
# Inaczej otrzymamy pętlę nieskończoną (o ile początkowy warunek był spełniony).

# Przykład:

n <- 0
while(n < 10) {
  print(n)
  n <- n + 1
}

# Przykład pętli nieskoczonej:
while(TRUE){print("wciśnij ESC")}


####################################################################################
# 4. Pętla repeat
# Nieskończona, może być rozwiązana jedynie przez polecenie break.

# Przykład:
x <- 1

repeat {
  print(x)
  x = x+1
  if (x == 6){
    break
  }
}

# Działanie pętli repeat bardzo przypomina działanie pętli while. Różnica polega na tym, że w repeat warunek
# sprawdzany jest dopiero po wykonaniu operacji. Oznacza to, że pętla zawsze zostanie wykonana co najmniej raz.


# Przykłady z użyciem poleceń next i break:

# next - zatrzymuje obecną iterację w pętli i zwiększa indeks iteracyjny.
# break - wychodzi z pętli (for, while lub repeat)


for(i in 1:22) {
  if(i <= 20) {
    ## Omin 20 pierwszych iteracji
    next
  }
  print("a") ## Wykonaj dla niezignorowanych indeksów
}

for(i in 1:5){print("a")
  next
  print("b")}


for(i in 1:100) {
  print(i)
  if(i > 20) {
    ## Zatrzymaj pętlę po 20 operacjach
    break
  }
}


#### III. Funkcje.
# Pisanie własnych komend (funkcji)

# Uproszczona składnia:

# nazwa_funkcji <- function(argumenty){
# OBLICZENIA
# return(wartośc_wynikowa)
# }

# Gdy opuścimy polecenie return zostanie zwrócona ostatnia zwracana wartość w obliczeniach.

wartosc1 <- function(n){
  return(n)
  2
}
wartosc1(3)

wartosc2 <- function(n){
  n
  2
}
wartosc2(3)


moja.funkcja <- function(a,b,c){
  a+b+c
}

moja.funkcja(1,2,3)

wynik <- moja.funkcja(1,2,3)
wynik

moja.funkcja2 <- function(a,b,c){
  a*b*c
}

class(a)
moja.funkcja3 <- function(a,b,c){
  if(class(a)=="numeric" & class(b)=="numeric" & class(c)=="numeric"){
    a*b*c
  }
  else {
    print("argumenty funkcji nie sa numeryczne")
  }
}

moja.funkcja3(2,2,2)

moja.funkcja3(2,2,'2')

wektor1<-1:5
wektor2<-1:5
wektor3<-1:5

# Funkcje jeśli mogą, działają na wektorach:
moja.funkcja2(wektor1,wektor2,wektor3)

# Pamiętajmy że argumenty mają nazwy:

f.przyklad <- function(a1,a2,a3){
  print(a2);print(a3)
}

# Że mogą mieć wartości domyślne:

f.przyklad <- function(a1=1,a2=5,a3=7){
  print(a2);print(a3);print(a1)
}

f.przyklad()

# .. i że mogą być nieobowiązkowe:


# Tak deklarujemy argumenty nieobowiązkowe:
f.przyklad <- function(a1=NULL,a2=5,a3=7){
  
}

#f.przyklad <- function(a2=3,a4=8){
#  if(a1 == NULL){a1+a2+a3}
 # else{a2+a3}
#}
f.przyklad()

# Argumenty funkcji można wypisać
args(lm)

# A u nas:
args(f.przyklad)

# Można tworzyć funkcje bez zdefiniowanej liczby parametrów:
myplot <- function(x, y, type = "l", ...) {
  plot(x, y, type = type, ...) ## Podanie 
}

myplot(x,y,lwd=4)

# Nie ma x i y, zdefiniujmy (przy okazji możemy obejrzeć sobie debuggera, więcej o nim niebawem)

x <- 1:100
y <- x^2

myplot(x,y,lwd=4)

# Tworzenie funkcji o dowolnej liczbie parametrów:
# Aby stworzyć funkcję o dowolnej liczbie parametrów, podajemy w argumencie funkcji 'ellipsis' tj. '...'
# Przykład:

suma <- function(...){
  s=0
  argumenty<-list(...) # pobranie argumentów do listy
  for (i in 1:length(argumenty)) {
    s=s+argumenty[[i]]
  }
  return(s)
}

suma(1,2)
suma(1,2,3)

# Możemy także dołączać parametry stałe:

suma_ntych_poteg <- function(n, ...){
  s<-0
  lista<-list(...)
  for (i in 1:length(lista)){
    s=s+(lista[[i]])^n
  }
  return(s)
}

suma_ntych_poteg(2,1,2,3)

## Zadania ##

## Zadanie 1. (łatwe)
#
# Napisz funkcję, która liczy odchylenie przeciętne
# Źródło: http://www.naukowiec.org/wzory/statystyka/odchylenie-przecietne_10.html
#
## Zadanie 2. (łatwe)
#
# Napisz funkcję, która liczy współczynnik zmienności
#
# Źródło: https://pl.wikipedia.org/wiki/Wsp%C3%B3%C5%82czynnik_zmienno%C5%9Bci
#
## Zadanie 3. (przeciętne)
#
# Stwórz funkcję liczącą dominantę
#
## Zadanie 4. (przeciętne)
#
# Stwórz funkcję która liczy średnią ważoną.
#
## Zadanie 5. (przeciętne)
#
# Napisz funkcję, która będzie zamieniała wektor tekstowy na liczbowy, w zależności
# od występujących wartości tekstowych. Przykładowo, wektor:
# c('mysz','mysz','kot',mysz','kot','mysz','mysz') ma zamienić na c(1,1,2,1,2,1,1). Funkcja
# niech pozwala na maksimum 10 wartości unikalnych

## Zadanie 6. (średnie)
# Napisz funkcję, która danej liczbie naturalnej przyporządkuje ilość jej cyfr.
# Wsk. skorzystaj z funkcji log i floor.
# Wsk2. Przypomnij sobie o notacji wykładniczej liczb rzeczywistych.
#
## Zadanie 7. (średnie)
#  Napisz funkcję która sprawdza czy dana liczba naturalna jest liczbą pierwszą.
#
## Zadanie 8. (przeciętne)
#  Korzystając z zadania z części I, napisz funkcję która splata
#  dwa wektory tej samej długości.
#
## Zadanie 9. (trudne)
#  Napisz funkcję, która danej liczbie naturalnej przyporządkuje wektor, zawierający
#  reprezentację danej liczby w systemie dwójkowym.
#
## Zadanie 10.
# Stwórz funkcję o dowolnej liczbie argumentów będących liczbamy rzeczywistymi, która utworzy z nich
# listę. Co więcej nazwy elementów listy mają być kolejnymi literami alfabetu.
