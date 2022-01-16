#############################
#      Praca domowa 1       #
#      Anna Głowacka        #
#############################
# Zadanie 1.2 wyskakuje error
# Zadane 2.1 wyniki mojej funkcji z wbudowaną do R-a funkcją nie pokrywają się
# Zadanie 3.2 zostało nie działa- prośba o ewentualne wskazówki lub poprawę kodu


# Zadanie 1.1

sumuj <- function(x) UseMethod("sumuj")

sumuj.numeric<-function(x)
{
  suma<-sum(x)
  return (noquote(paste0("Suma elementów obiektu numeric to ", suma)))
  
  structure(list(x), class = "numeric")
}
sumuj.character<-function(x)
{
  sklejanie<-paste(x,collapse="")
  return (noquote(paste0("Sklejone elementy obiektu character to ", sklejanie)))
  structure(list(x), class = "character")
}

sumuj.default<-function(x){
  return(noquote(paste0(("Obiekt nie jest klasy numeric lub character"))))
}

# Sprawdzam poprawność
numeric<-c(4,6,3)
character<-c("a","b","c")
factor<-as.factor(numeric)

sumuj(numeric)
sumuj(character)
sumuj(factor)

# Zadanie 1.2

setClass("Osoba", representation(imie = "character", wiek = "numeric", plec = "character"))
setClass("MiejscePracy", representation(nazwa = "character"), contains = "Osoba")
OsobaTestowa <- new("Osoba", imie = "Test", wiek = 18, plec = "M")

fun.Osoba <- function(x)
  print("Osoba")
fun.MiejscePracy <- function(x)
  print("MiejscePracy")
setGeneric("fun")
getGeneric("fun")


# Zadanie 1.3

osoba <- setRefClass("osoba",fields = list(name = "character", age = "numeric", sex = "numeric"))
s <- osoba(name = "Aneta", age = 45, sex = 1)

osoba$methods(
  wyswietl=function(x) {
    n<-x*(x+1)
    print(n)
  }
)
s$wyswietl(1)
s$wyswietl(2)

# Zadanie 2.1 

odchylenie<-function(x){
  sum(abs(x-sum(x) / length(x)))/length(x)
}

odchylenie<-function(x){
  sum(abs(x-mean(x)))/length(x)
}

# Sprawdzenie
odchylenie(1:9)
mad(1:9)
# Nie daje tych samych wyników

# Zadanie 2.2

install.packages("raster")
library(raster)

punkty <- c(12, 56, 89, 0, 23, 90, 45, 47, 100, 23)
cv(punkty)


wspolczynnik <- function(x){
  (sd(x)/mean(x))*100
}

wspolczynnik(punkty)

# Zadanie 2.3

x<-c(1,4,5,4,2,1,3,4,6,6,6,6,8,8,8,8,7,7,7,7)

dominanta <-function(x){
  y<-factor(x)
  w<-table(y)
  s<-sort(w,decreasing = T)
  
    if(s[[1]]>s[[2]]){
      return(paste0("Dominantą jest liczba: ", names(sort(w,decreasing = T))[1]))}
      else{print("Brak dominanty")}
}

dominanta(x)

# Zadanie 2.4

ilosc<-c(1,23,45,7,12,9)
ocena<-c(2,5,4.5,3.5,4,3)

wazona<-function(x,y){
  sum(x*y)/sum(y)
}


wazona(ocena,ilosc)

weighted.mean(ocena,ilosc)


# Zadanie 3.1

install.packages("testit")
library(testit)

x<-c(1,2,3,NA)
moja_srednia<-function(x)
{
  require(testit)
  if(!is.numeric(x))
    assert("dane wejsciowe nie sa numeryczne", is.numeric(x))
  if(is.numeric(x))
  {
    suma_bledow<-sum(is.na(x))
    
    if(is.numeric(x))
    {
      y<-na.omit(x)
      result <- sum(y) / length(y)
      if(sum(is.numeric(x))<5)
      {
        message(paste0("Mniej niż 5 obserwacji zostalo użytych do liczenia sredniej"))
      }
      
      return(paste0("Średnia wynosi: ",result," Liczba błędów ", suma_bledow))}
  }
}
moja_srednia(x)
has_error(x)
has_warning(x)


## Zadanie 3.2

# Zadanie nie zostało poprawnie wykonane

walidacja<-function(x1,x2,nazwa_pliku){
  if(!is.numeric(x1) | !is.numeric(x2) ){
    x1<-as.numeric(x1)
    x2<-as.numeric(x2)
    
  }
  
  
  x1<-na.omit(x1)
  x2<-na.omit(x2)
  
  
  x1 <- runif(1000)
  x2 <- runif(1000)
  y <- 0.4 * x1 - 0.3 * x2 + rnorm(1000)/5
  dane <- data.frame(x1, x2, y)
  
  for(i in 1:1000){
    
    model_1 <- lm(y ~ x1 + x2, data = dane[-i,])
    print(i)
    coef_ <- (summary(model_1))$coefficients[, 1]
    pval_ <- (summary(model_1))$coefficients[, 4]
    prognoza <- predict(model_1, newdata = dane[1,])
    
    model_summary <- data.frame(obs = 1,
                                t(coef_),
                                t(pval_),
                                prognoza)
    
    names(model_summary)[2:7] <- c("coef_int",
                                   "coef_x1",
                                   "coef_x2",
                                   "p_int",
                                   "p_x1",
                                   "p_x2")
    
  }
  
  # tu bedzie po prostu blad prognozy - MAE mozna policzyc na calym zbiorze
  model_summary$blad_prognozy <- abs(dane$y[1] - prognoza)
  wynik<-rm(model_1, coef_, pval_, prognoza)
  invisible(rm(model_1, coef_, pval_, prognoza))
  
  
  png(filename = paste0(nazwa_pliku, ".png"), 
      width = 600, height = 800)
  assert("Dane do wykresu sa nieprawidlowe", 
         !has_error(hist(model_summary$blad_prognozy)))
  hist(model_summary$blad_prognozy)
}

# Szymona


# Zadanie 3.2


tekst<-function(x, ...){
  
  print(noquote(paste(x, ...)))
  
}

x1 <- runif(1000)
x2 <- runif(1000)
x3 <- as.character(c(c(1:999),NA))
y <- 0.4 * x1 - 0.3 * x2 + rnorm(1000)/5
dane <- data.frame(x1, x2, x3, y)
dane$x3<-as.character(dane$x3)

crosvalid<-function(dane,y,nazwa_pliku){
  model_summarytotal<-data.frame()
  if(!(is.numeric(y) | is.integer(y) | is.double(y))) stop("zmienna ma zły typ")
  ifelse(is.element(c("character","string","date"),sapply(dane,class))!=c(FALSE,FALSE,FALSE),
         warning("Zmienna ma zły typ"),
         print(""))
  if(anyNA(dane)) tekst("wykluczono ", sum(is.na(dane)), "brakow danych.")
  dane<-na.omit(dane)
  for (i in 1:nrow(dane)){
    if(i%%100==0)tekst("Iteracja ", i)
    # model z pominieciem pierwszej obserwacji
    model<- lm(y ~ x1 + x2, data = dane[-i,])
    
    coef_ <- (summary(model))$coefficients[, 1]
    pval_ <- (summary(model))$coefficients[, 4]
    prognoza <- predict(model, newdata = dane[i,])
    
    model_summary <- data.frame(obs = i,
                                t(coef_),
                                t(pval_),
                                prognoza)
    
    names(model_summary)[2:7] <- c("coef_int",
                                   "coef_x1",
                                   "coef_x2",
                                   "p_int",
                                   "p_x1",
                                   "p_x2"
    )
    
    # tu bedzie po prostu blad prognozy - MAE mozna policzyc na calym zbiorze
    model_summary$blad_prognozy <- abs(dane$y[i] - prognoza)
    model_summarytotal<-rbind(model_summarytotal,model_summary)
    rm(model, coef_, pval_, prognoza, model_summary)
  }
  invisible(model_summarytotal)
  png(filename = paste0(nazwa_pliku, ".png"), 
      width = 800, height = 800)
  hist(model_summarytotal$blad_prognozy)
  on.exit(dev.off())
  assert("Bląd przy generowaniu wykresu", 
         !has_error(hist(model_summarytotal$blad_prognozy)))
}
model_summarytotal<-as.data.frame(crosvalid(dane,y,"histogram"))
crosvalid(dane,y,"histogram")
tekst<-function(x, ...){
  
  print(noquote(paste(x, ...)))
  
}

# Zadanie 3.3

`%probka%` <-function(a, b ) sample(a, size = b, replace = TRUE)

probka(1:20,7)
1:20 %probka% 7

# Zadanie 3.4

  
`%MA%` <- function(x, n){
  y = x
  for(i in n:length(x)){
    y[i] = mean(x[(i-n+1):i])
  }
  y[[length(x)]]
}


c(9, 2, 10, 8, 12) %MA% 4
(1:14) %MA% 4



