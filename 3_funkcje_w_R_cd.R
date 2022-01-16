###########################################################
#             Zaawansowane programowanie w R              #
#                 R jako język funkcyjny - cd.            #
#                       Zajęcia 3                         #
#             Piotr Ćwiakowski, Piotr Wójcik              #
###########################################################     

# Dzisiaj poznamy kolejne funkcjonalnosci mozliwe do wykorzystania
# przy pisaniu wlasnych funkcji, w tym podejscie okreslane jako 
# programowanie defensywne.

# nowy pakiet, ktorego bedziemy uzywac
install.packages("testit")

library(testit)


#-------------------------------------------------------------------------------
# programowanie defensywne, komunikacja z uzytkownikiem

# Wywolywane przez uzytkownika funkcje moga dawac niespodziewane rezultaty.
# Programowanie defensywne polega na pisaniu funkcji "odpornych" 
# na ich uzytkownika. Zaleca sie czeste sprawdzanie wymaganych warunkow
# (np. odpowiedniego typu danych) i w razie ich niespelnienia zwrocenie 
# uzytkownikowi informacje o rodzaju popelnionego bledu.

# Te sprawdzenia nazywane sa w programowaniu asercjami. 
# Asercja wskazuje, ze programista zaklada, ze pewien warunek
# jest prawdziwy. Jesli jest on falszywy (czyli niespelnione sa warunki 
# postawione przez programiste) asercja powoduje przerwanie wykonania programu. 
# Asercja ma szczegolne zastosowanie w trakcie testowania tworzonego 
# oprogramowania, np. dla sprawdzenia luk lub jego odpornosci na bledy. 
# Zaleta stosowania asercji jest mozliwosc sprawdzenia, w ktorym fragmencie 
# kodu zrodlowego programu nastapil blad.
# Zrodlo: https://pl.wikipedia.org/wiki/Asercja_(informatyka)

# Oszczedza to czas poswiecany na poszukiwanie bledow w kodzie przez
# uzytkownikow, ktorym funkcja zwraca niespodziewany wynik.

#-------------------------------------------------------------------------------
# 3.1. sprawdzanie typow danych wejsciowych

# napiszmy prosta funkcje liczaca pierwiastek stopnia n 
# z podanej liczby x i wyswietlajaca wynik
# w postaci komunikatu

pierwiastek <- function(x, n) {
  wynik <- x**(1/n)
  return(paste0("pierwiastek stopnia ", n, 
                " z ", x, " wynosi ", wynik))
}

# sprawdzmy jej dzialanie

pierwiastek(100, 2)

pierwiastek(1024, 10)

# czy pierwiastek moze byc stopnia:
# ujemnego?
pierwiastek(100, -1)

# zerowego?
pierwiastek(100, 0)

# ulamkowego?
pierwiastek(100, 0.5)

# NIE - pierwiastek algebraiczny moze miec stopien bedacy
# dodatnia liczba naturalna!

# uzupelnijmy wiec nasza funkcje o dodatkowe funkcjonalnosci

# Mozemy zasymulowac tzw. fatal error za pomoca polecenia stop(), 
# ktore wstrzymuje wykonanie programu i wyswietla komunikat podany
# jako argument funkcji.

# Jesli stwierdzimy blad w kodzie warto o nim zakomunikowac
# tak szybko jak to mozliwe i przerwac dzialanie funkcji, 
# aby uniknac jalowego wykorzystywania zasobow.
# Tak wiec pierwsze linie w definicji funkcji zwykle 
# sprawdzaja typy danych i inne konieczne warunki.

pierwiastek2 <- function(x, n) {
  if (!is.integer(n) | n < 1) 
    stop("n powinna byc dodatnia liczba naturalna!")
  
  wynik <- x**(1/n)
  return(paste0("pierwiastek stopnia ", n, 
                " z ", x, " wynosi ", wynik))
}

pierwiastek2(100, -1)
pierwiastek2(100, 0)
pierwiastek2(100, 0.5)

# sprawdzmy czy dziala dla 10
pierwiastek2(1024, 10)

# i tu niespodzianka..., double liczba podwójnej precyzji zapamiętuje przed i po przecinku

typeof(10)
is.numeric(10)

# dla wartosci calkowitych powinnismy uzyc L
pierwiastek2(1024, 10L)

# ale skad uzytkownik ma to wiedziec?

# zrobmy to sami w funkcji, ale poinformujmy o tym uzytkownika
# za pomoca ostrzezenia (warning)

# ostrzezenie mozemy wygenerowac za pomoca funkcji warning(). 
# Wyswietlenie ostrzezenia moze oznaczac potencjalny problem. 

# Na przyklad

mean(NULL) 
# zwraca NA ale takze wyswietla warning.

c(1,1,1)+c(2,2,2,2,2) #błąd

c(1,1,1)+c(2,2,2,2,2,2) #nie ma błędu, bo wektor 1 jest wielokrotnością 2

library(ggplot2)
ggplot(data=diamonds, aes(x=price))+geom_histogram()
prop.test(c(1,2),c(4,6)) #test odsetka 

# kiedy wykonany kod zwraca warning, wazne jest sprawdzenie
# z czego ono wynika - nie nalezy go ignorowac. 

# Zignorowanie ostrzezen na krotka mete oszczedza czas, ale 
# czesto warningi odslaniaja jakis glebszy problem, ktory
# moze byc sie pojawic przy nieuwaznym wykonywaniu kodu.

pierwiastek3 <- function(x, n) {
  if (!is.integer(n)) {
    n.int <- as.integer(n)
    # komunikat wyswietlimy tylko gdy obcieto czesc dziesietna
    if (n - n.int != 0) 
      warning(paste0("Wartosc ", n, 
                     " zaokraglona do calkowitej: ", n.int))
  }
  if (n.int < 1) 
    stop("n powinna byc liczba naturalna !")
  
  wynik <- x**(1/n.int)
  return(paste0("pierwiastek stopnia ", n.int, 
                " z ", x, " wynosi ", wynik))
}

pierwiastek3(1024, 10.5)

pierwiastek3(1024, 10)

pierwiastek3(1024, -10)

# ostrzezenia mozna wylaczyc za pomoca suppressWarnings().

suppressWarnings(pierwiastek3(1024, 10.5))

# ale generalnie nie jest to zalecane

# zalozmy uzycie funkcji przez kompletnego ignoranta

pierwiastek3(T, 10)
pierwiastek3(F, 10)
pierwiastek3(1024, T)

pierwiastek3("T", 10)

# w ostatnim przypadku funkcja nie zadziala, ale 
# w trzech wczesniejszych jak najbardziej (T = 1, F = 0)

# zamiast if() stop() mozemy uzyc funkcji stopifnot()

# z kolei zamiast warning() mozemy uzyc polecenia message(), 
# ktore po prostu wyswietla informacje (nie bedaca ostrzezeniem)
# w trakcie wykonania funkcji.
# messages takze moga byc wylaczone za pomoca suppressMessages().

pierwiastek4 <- function(x, n) {
  stopifnot(is.numeric(x), is.numeric(n))
  if (!is.integer(n)) {
    n.int <- as.integer(n)
    # komunikat wyswieylimy tylko gdy obcieto czesc dziesietna
    if (n - n.int != 0) 
      message(paste0("Wartosc ", n, 
                     " zaokraglona do calkowitej: ", n.int))
  } # zamiast warning() mozna uzyc tez message()
  
  if (n.int < 1) 
    stop("n powinna byc liczba dodatnia!")
  
  wynik <- x**(1/n.int)
  return(paste0("pierwiastek stopnia ", n.int, 
                " z ", x, " wynosi ", wynik))
}

pierwiastek4(T, 10)
pierwiastek4(F, 10)
pierwiastek4(1024, T)
pierwiastek4("T", 10)

# tym razem funkcja wyraznie wskazuje, gdzie lezy problem,
# ktory warunek jest niespelniony, ale wyswietlanego 
# przez nia komunikatu nie mozna zmienic - i dla uzytkownika,
# ktory nie zna wywolywanej funkcji, moze nie byc zrozumialy

# lepiej wtedy wykorzystac funkcje assert() z pakietu testit.
# funkcja pozwala dodac wlasny komunikat, gdy warunek nie 
# jest spelnoiny, co ulatwie identyfikacje bledu, np.

assert("dziesiec jest rowne dziesiec", 10L == 10) # (komunikat, warunek)
assert("Wynik nie jest taki sam", 
       pierwiastek4(1024, 10) == 1024**0.1)

# podobnie jak w stopifnot(), mozemy wpisac 
# kilka warunkow do spelnienia

assert("dane wejsciowe nie sa numeryczne", 
       is.numeric(1024), is.numeric(10))

# jesli jeden z warunkow nie jest spelniony.

assert("dane wejsciowe nie sa numeryczne", 
       is.numeric(1024), is.numeric(F))


# zamienmy stopifnot() na assert() w funkcji pierwiastek
# obsługa pakietu, gdy nie ma shiny

zmienna<-require(Shiny)
if(suppressWarnings(require(shiny))) install.packages('shiny')
library(shiny)

pierwiastek5 <- function(x, n) {
  require(testit) # nasza funkcja wymaga funkcji z tego pakietu
  assert("Oba parametry wejsciowe powinny byc numeryczne",
         is.numeric(x), is.numeric(n))
  
  if (!is.integer(n)) {
    n.int <- as.integer(n)
    # komunikat wyswieylimy tylko gdy obcieto czesc dziesietna
    if (n - n.int != 0) 
      message(paste0("Wartosc ", n, 
                     " zaokraglona do calkowitej: ", n.int))
  } # zamiast warning() mozna uzyc tez message()
  
  if (n.int < 1) 
    stop("n powinna byc liczba dodatnia!")
  
  wynik <- x**(1/n.int)
  return(paste0("pierwiastek stopnia ", n.int, 
                " z ", x, " wynosi ", wynik))
}

pierwiastek5(T, 10)
pierwiastek5(F, 10)
pierwiastek5(1024, T)
pierwiastek5("T", 10)

pierwiastek5(1024, 10)


# pakiet testit zawiera tez funkcje pozwalajace wykryc,
# czy wystapil blad albo zostal wygenerowany warning

has_error(10 + 10)

has_error(10 + "10")

has_warning(10 + 10)

has_warning(mean(NULL))

# ktore mozna wykorzystac w asercjach

#-----------------------------------------------------------
# polecenie on.exit()
# nasza funkcje mozna zastosowac do wektora

pierwiastek5(1:20, 5)

# sprobujmy narysowac 

plot(1:20, pierwiastek5(1:20, 5))

# nie da sie, bo drugi argument jest wartoscia tekstowa

# napiszmy prosta funkcje rysujaca wykres 
# i zapisujaca go do pliku o podanej nazwie

zapisz_wykres_do_pliku <- function(x, y, nazwa_pliku,
                                   height = 800, width = 600) {
  png(filename = paste0(nazwa_pliku, ".png"), 
      width = width, height = height)
     plot(x, y)
  dev.off() # zamkniecie device png
}

zapisz_wykres_do_pliku(1:20, pierwiastek5(1:20, 5),
                       nazwa_pliku = "wykres1")


# z powodu bledu w poleceniu plot()
# w biezacej sciezce mamy NIE ZAMKNIETY plik "wykres1.png"
# (poniewaz nie wykonalo sie polecenie dev.off())

# musimy zamknac ten kanal zapisu recznie
dev.off()

# mozemy tez wykorzystac wewnatrz funkcji funkcje on.exit()
# pozwala ona zdefiniowac jakie polecenie(a) jest wykonywane 
# w momencie wyjscia z wywolanej funkcji, NAWET JESLI podczas 
# jej wykonania wystapil blad i funkcja nie wykonala sie do konca

# Dlatego najczestszym jej zastosowaniem jest "czyszczenie" po 
# "ryzykownych" operacjach - np. polaczeniu z baza danych
# (ktore musi byc zamkniete), zapisywaniu wykresu do pliku
# (kiedy trzeba zamknac kanal zapisu po utworzeniu pliku)

# # UWAGA! polecenie on.exit() musi byc przed linia z ewentualnym bledem 
# przy okazji dodajmy asercje za pomoca polecenia assert()

zapisz_wykres_do_pliku2 <- function(x, y, nazwa_pliku,
                                   height = 800, width = 600) {
  png(filename = paste0(nazwa_pliku, ".png"), width = width, height = height)
    on.exit(dev.off()) # przed linia z ewentualnym bledem
    assert("Dane do wykresu sa nieprawidlowe", 
           !has_error(plot(x, y)))
   # zamkniecie device png zawsze przy wychodzeniu z funkcji
}

zapisz_wykres_do_pliku2(1:20, pierwiastek5(1:20, 5),
                       nazwa_pliku = "wykres1")

# teraz nie ma juz co zamykac
dev.off()


#-------------------------------------------------------------------------------
# 3.2. obsluga bledow

# Zalozmy, ze wywolujemy funkcje, ktora zglasza blad. Co wtedy? 
# Efektywny, odporny kod funkcji powinien namierzyc blad
# i odpowiednio zareagowac.

# Bledy moga byc namierzone/przechwycone przze funkcje
# try(wyrazenie) oraz tryCatch(wyrazenie)
# dodatkowa opcja silent = TRUE (tylko dla try()!!!) wylacza 
# wyswietlanie komunikatow o bledzie podczas wykonania blednego kodu

# Na przyklad:
try(2 + sqrt(2))

tryCatch(2 + "2")

try(2 + "2", silent = T)

# wynik mozemy zapisac do obiektu

dobrze = try(2 + sqrt(2))

zle = try(2 + pierwiastek5(2, 2))

zle = try(2 + pierwiastek5(2, 2), silent = TRUE)

# jesli wyrazenie w try() jest poprawne, funkcja zwroci 
# po prostu wynik tego wyrazenia

dobrze

# jesli jednak wystapi blad, otrzymujemy obiekt
# klasy try-error oraz artybut `condition`` zawierajacy 
# komunikat o bledzie

zle

class(zle)

zle$condition

# zidentyfikowanie bledu mozna nastepnie wykorzystac 
# w przetwarzaniu warunkowym

if(class(zle) == "try-error") print("Tu bedzie blad")


#-------------------------------------------------------------------------------  
# 3.3. zwracanie niewidocznych obiektow: invisible()

# Czasem wygodnie jest zwrocic jako wynik funkcji obiekt, ktory 
# jest czasowo niewodoczny - sluzy do tego funkcja invisible()
# Jest to szczegolnie uzyteczne dla funkcji, ktore zwracaja wynik,
# ktory moze byc przypisany do jakiegos obiektu, ale jesli nie jest 
# przypisany, nie bedzie wyswietlony w konsoli.

# Przyklad: funkcja, ktora rysuje wykres rozrzutu
# i naklada na niego linie regresji z modelu liniowego,
# ktory jest obliczany wewnatrz funkcji, ale nie jest 
# wyswietlany

wykres_regresji = function(x, y) {
  # narysuj wykres rozrzutu miedzy x i y
  plot(x, y) 
  # oszacuj model regresji
  model = lm(y ~ x)
  # na wykrsie dodaj linie regresji
  abline(model)
  # zwroc model jako obiekt "niewidzialny"
  invisible(model)
}

# sprobujmy ja zastosowac dla przykladowych danych

x <- 1:100
y <- x + 20*rnorm(100)

# wywolajmy funkcje bez przypisania wyniku do obiektu

wykres_regresji(x, y)

# wykres jest wygenerowany, ale wynik modelu 
# nie jest wyswietlany w konsoli (jest "niewidzialny")

# teraz zapiszmy wynik modelu jako obiekt
# (uzyjmy innych danych dla zmiennej zaleznej)

y1 <- x + 25*rnorm(100)

model1 <- wykres_regresji(x, y1)

# wykres zostal wyswietlony, w konsoli nie ma wyniku,
# ale to dlatego, ze uzylismy polecenia, ktore jest 
# przypisaniem

# zobaczmy co zawiera wynik
summary(model1)

str(model1)

# w tym przypadku zawiera on takze zapisane
# wyniki samego modelu


# klasycznym przykladem "niewidocznego" wyniku
# jest funkcja hist().

# zwykle po prostu wyswietlamy histogram
hist(y)

# ale mozemy tez przypisac wynik do obiektu...
histogram_y <- hist(y1)

# ... ktory jest lista zawierajaca miedzy innymi
# informacje o srodkach slupkow histogramu, granicach
# przedzialow czy liczebnosciach

str(histogram_y)

# nota bene - te informacje o wykresie sa wykorzystywane
# do "zobaczenia" histogramu (czy innych wykresow) przez
# osoby niewidome, ktore takze pracuja z R i grafika !!!
# patrz: https://cran.r-project.org/web/packages/BrailleR/BrailleR.pdf
# https://channel9.msdn.com/Events/useR-international-R-User-conferences/useR-International-R-User-2017-Conference/Interactive-graphs-for-blind-and-print-disabled-people




#-------------------------------------------------------------------------------
# funkcje anonimowe
# jesli funkcji nie nadamy nazwy, tworzona jest funkcja anonimowa.
# Jak to mozliwe?
# W R funkcja (jak kazdy inny obiekt) jest wykonywana bez koniecznosci
# przypisywania go albo jego wyniku do jakiegokolwiek obiektu majacego nazwe

# zobadamy prosta funkcje

kwadrat <- function(x) x ** 2

kwadrat(10)

# wywolanie odpowiedniej funkcji anonimowej wyglada tak:

(function(x) x ** 2)(10)

macierz <-matrix(1:100, ncol=10, byrow=T)
macierz^2
apply(macierz, 2, kwadrat) #zamiast ^2
apply(macierz, 1, function(x) {x==2}) #pętla ktora moze isc po kolumnach i wierszach


# kod funkcji nalezy wziac w nawias, ale nie nadajemy jej nazwy
# (drugi nawias standardowo zawiera argument funkcji)

# Gdzie mozna wykorzystac funkcje anonimowe?

# Funkcja anonimowa jest zdefiniowana i uzyta w tym samym poleceniu.
# Nie jest to wiec przejrzysty sposob kodowania, ale moze byc
# uzyteczne tam, gdzie nie chcemy tworzyc nowej funkcji, ktora nie bedzie
# wykorzystana nigdzie indziej.


#-------------------------------------------------------------------------------
# specjalne funkcje - tworzenie wlasnych operatorow binarnych

# wykorzystujac kod powobny do funkcji mozemy tworzyc wlasne operatory

# operatory binarne (np. %%, %in%, %*% - odpowiednio modulo, match 
# i mnozenie macierzy) to de facto funkcje, ktore dzialaja na dwoch
# argumentach, a ktore R rozpoznaje jako specjalne funkcje po nazwie,
# ktora zaczyna i konczy sie %.

# Mozna ich wtedy uzywac w formie:
# argument1 %nazwa_funkcji% argument2

# zamiast:
# nazwa_funkcji(argument1, argument2)

# definiujac operator (specjalna funkcje), jego nazwa oprocz
# %nazwa% umieszczamy takze w apostrofach ``

# przyklad - operator bedacy odpowiednikiem funkcji head()

`%head%` <- function(a, b) head(x = a, n = b)

head(1:10, 4)

1:10 %head% 4

# wynik jest identyczny
`%+%`<-function(x,y) sum(x,y)
(function(x,y) sum(x,y))(x,y)
x%+%y
###############################################blad
iris
unique(iris$Sepal.Length)

install.packages('tidyverse')
library(tidyverse)

iris<-iris
iris %>% filter(Sepal.Length %in% c(5.1, 4.9, 4.7, 4.6, 5.0, 5.4, 4.4, 4.8, 4.3, 5.8, 5.7, 5.2, 5.5))
'%lin%' <-function(x,y)!('%in%'(x,y))
iris %>% filter(Sepal.Length %lin% c(5.1, 4.9, 4.7, 4.6, 5.0, 5.4, 4.4, 4.8, 4.3, 5.8, 5.7, 5.2, 5.5))

###########################################
c(5,NA,7) %na.sum% 2

library(ggplot2)
dplyr::mutate(diamonds, nazwa=x %+% y %+% z)
diamonds$x+diamonds$y+diamonds$z

#sumowanie wierszy
obiekt<-mutate(baza_danych, nowa_zmienna=rowSums(as.matrix(diamonds[,c('x','y','z')]), na.rm=T))

#--------------------------------------------------------------------------
# Cwiczenia 3.

# Cwiczenie 3.1.
# Przerob ponizsza funkcje tak, aby byla odporna na niewlasciwe dane
# wejsciowe:
  
  moja_srednia <- function(x) {
    result <- sum(x) / length(x)
    return(result)
    }

# - dodaj asercje sprawdzajace poprawnosc danych wejsciowych,
# - dodaj osbluge brakow danych (wyswietl informacje o liczbie brakow, 
#   wyeliminuj je z obliczen),
# - wyswietlaj ostrzezenie, jesli liczba niebrakujacych obserwacji
#   jest mniejsza niz 5.
# - sprawdz na gotowej funkcji za pomoca has_error(), has_warning(),
#   czy zwraca blad/warning w przypadku podania niepoprawnych danych
#   wejsciowych





# Cwiczenie 3.2.
# Biorac za punkt wyjscia ponizszy kod, napisz funkcje, ktora
# przeprowadza prosta walidacje krzyzowa modelu regresji
# po kolei pomija po jednej obserwacji z danych, wykonuje dla
# niej oszacowanie modelu, zapisuje wartosci parametrow i wyniki
# testow istotnosci, a takze przeprowadza prognoze dla obserwacji
# zostawionej poza modelem i zapisuje blad MAE tej prognozy.
# - uzyj petli,
# - wyswietlaj numer iteracji (pominietej obserwacji)
# - dodaj obsluge brakow danych
# - dodaj sprawdzanie poprawnosci danych wejsiowych
# - niech tabela z wynikami modelu bedzie niewidzialnym wynikiem funkcji
# - niech drugim wynikiem funkcji bedzie wykres pokazujacy histogram
#   wielkosci bledu MAE, zapisywany do pliku o nazwie bedacej argumentem
#   funkcji,
# - upewnij sie, ze plik z hstogramem bedzie zamkniety niezaleznie 
#   od poprawnosci danych wykorzystanych w funkcji hist()





# Cwiczenie 3.3.
# Zdefiniuj operator %probka%, ktory bedzie zwracal n elementowa 
# (argument2) probe z podanego wektora (argument1).
# (wykorzystaj funkcje sample())





# Cwiczenie 3.4.
# Zdefiniuj operator %MA%, ktory bedzie zwracal srednia
# ruchoma z wektora (argument1) liczona na podstawie
# n (argument2) wartosci




