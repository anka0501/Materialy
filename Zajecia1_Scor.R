########################################################

# WST?PNA ANALIZA DANYCH

########################################################
#bill amount t-->pay amount t-1

# Ustalenie ?cie?ki docelowej

dir() # Lists files in the working directory

# dir.create("D:/Pulpit/WNE/Przedmioty/Budowa karty scoringowej/wyniki") 
getwd()
setwd("D:/mks/zaj1")

# Zaczytywanie danych z CSV
?read.csv
UCI<-read.csv2(file="UCI_Credit_Card.csv")
View(UCI)
#Zmiana nazwy kolumny z flag? GB
colnames(UCI)[25]<-"DEF"


# BILL_AMT6 - ile by?o do zap?acenia na koniec 6M
# PAY_AMT5 - ile zaplacono w 5 miesiacy (czyli ile splacono z 6M)
# PAY_5 - jaki to spowodowalo status

#UWAGA: sa ma?e b??dy, lub proces jest bardziej skomplikowny, cho? nie da sie tego potweirdzi? na dost?pnych danych

colnames(UCI)[8]<-"PAY_1"
colnames(UCI)[9]<-"PAY_2"
colnames(UCI)[10]<-"PAY_3"
colnames(UCI)[11]<-"PAY_4"
colnames(UCI)[12]<-"PAY_5"

# zapisanie danych do formatu R
save(UCI,file="UCI.RData")

# wstepna analiza bazy danych
?summary
summary(UCI) # Podstawowe stats opisowe.

# PROBLEM -> R wczyta? LIMIT_BAL jako FACTOR a chcielibysmy jako NUMERIC
# ROZWI?ZANIE: gsub() <- zamiana znaku na inny
?gsub
UCI$LIMIT_BAL<-as.numeric(gsub(" ", "", UCI$LIMIT_BAL, fixed = TRUE))

#stats opisowe
summary(UCI)
str(UCI)
#sko?no?? i kurtoza
install.packages("moments")
install.packages("DescTools")
library(DescTools)
library(moments)
Desc(UCI$LIMIT_BAL)


skewness(UCI$LIMIT_BAL)
kurtosis(UCI$LIMIT_BAL)
hist(UCI$LIMIT_BAL, breaks="Scott")

# Struktura danych
str(UCI) 

# Nazwy zmiennych
names(UCI) # Lists variables in the dataset

#Pierwsze 10 wierszy
head(UCI, n=10)# First 10 rows of dataset

# Ostatnie 10 wiersz
tail(UCI, n=10) # Last 10 rows

# Liczebnosc brakow danych - czy problem brak?w danych w og?le wyst?puje?
colSums(is.na(UCI)) # Number of missing per column/variable
# w tej bazie danych nie ma brak?w danych, ale normalnie to du?y problem


# zmiana zmiennej na FACTOR - bedzie potrzebne do przeprowadzenia kolejnych przeksztalce?
?factor

#Zmienne SEX
UCI$SEXF <- factor(UCI$SEX, levels = c(1,2), labels = c("male", "female"))
#levels poziomy zmiennej oryginalnej
#labels nadanie etykiet warto?ciom zmiennych

#Zmienna Education
#Przeksztalcenie zmiennej w taki spos?b, ?e warto?ci 0, 4,5,6 s? przekszta?ocone w jeden poziom 0
#funkcja ifelse() warunkowa zmiana wartosci zmiennej 
hist(UCI$EDUCATION)
table(UCI$EDUCATION)
UCI$EDUCATION<-ifelse(UCI$EDUCATION %in% c(0,4:6),0,UCI$EDUCATION)
UCI$EDUCATIONF <- factor(UCI$EDUCATION, levels = c(0,1,2,3), labels = c("others","graduate school", "university", "high school"))
table(UCI$EDUCATIONF)

# Zmienna MARRIAGE
UCI$MARRIAGEF <- factor(UCI$MARRIAGE, levels = c(0,1,2,3), labels = c("others","married", "single", "divorce"))


# Zmienne PAY_0 - PAY_6
#Zmiana za pomoc? p?tli for
#paste() - funcja s?u??ca do "sklejania" tekstu - w tym przypadku utworzenia nazw zmiennych
  #sep="" brak znak?w separuj?cych kolejne elementy tekstu
for (i in c(0:5))
{
  UCI[,paste("PAY_",i,"F",sep="")]<-factor(UCI[,paste("PAY_",i,sep="")], levels = c(-2,-1,0,1,2,3,4,5,6,7,8,9), labels = c("no consumption","fully paid", "revolving", "1d", "2d", "3d", "4d", "5d", "6d", "7d", "8d", "9d"))
}


# Wst?pna analiza danych - Jaki typ danych? Ile poziom?w? Jakie rozk?ady?
  # liczebnosci
  table(UCI$EDUCATIONF)
  # liczebnosci - tabelka 2 wymiarowa
  tabelka<-table(UCI$EDUCATIONF,UCI$DEF)
  # table() - tabela cz?sto??i 
  
  # dodanie sum
  addmargins(tabelka)
  
  #tabelka procentowa (,1) - udzia? w wierszach, (,2) - udzia? w kolumenach, ()  udzia? we wszystkich obserwacjach
  prop.table(tabelka,1) 
  #zaokraglenie do 2 znak?w po przecinku
  round(prop.table(tabelka,1), 2)
options(scipen=999)
  #testy na niezalezno?? miedzy zmiennymi 
  #H0:wejscie w default nie zalezy od wyksztalcenia
  chisq.test(tabelka) #WNIOSEK?
  fisher.test(tabelka) 
  #giniego sie nie uzyje bo powinno sie uporzadkowac wedlug ryzyka
  
  #miara V-Cramera
  install.packages("vcd")
  library(vcd)
  assocstats(tabelka) # First two are assoc measures, last three show degree of association.
  ?assocstats
  #likelihood ratio to tet G
  #V cramera bliskie zero-bardzo mala wspolzaleznosc, ale duzy zbior i cos daje
  #ta slaba informacj
  # 3-way crosstabs
  table3 <- xtabs(~SEXF+EDUCATIONF+DEF, data=UCI)
  ftable(table3)
  
  #Wykres udzialu defaultow 
  barplot(t(prop.table(tabelka,1)))
  
  table3 <- xtabs(~EDUCATIONF+DEF, data=UCI)
  ftable(table3)
  
# PRZYGOTOWANIE WYKRESóW DO ANALIZA ZALE?NO?CI ZMIENNYCH
  
# Funkcja s?u??ca do resetowania ustawie? rysunk?w
resetPar <- function() {
  dev.new()
  op <- par(no.readonly = TRUE)
  dev.off()
  op
}

#podzial zmiennych na zmienne numeryczne i zmienne typu factor
colnames(UCI)

kolumny<-c("SEXF","EDUCATIONF","MARRIAGEF","LIMIT_BAL","PAY_0F","PAY_1F","PAY_2F","PAY_3F","PAY_4F","PAY_5F","BILL_AMT1","BILL_AMT2","BILL_AMT3","BILL_AMT4","BILL_AMT5","BILL_AMT6","PAY_AMT1","PAY_AMT2","PAY_AMT3","PAY_AMT4","PAY_AMT5","PAY_AMT6")
baza<-UCI[,kolumny]

nums <- sapply(baza, is.numeric) 
#aspply() przypisanie funkcji {is.numeric} do kazdego elementu zbioru {baza}
baza.n<-baza[,nums]

fact <- sapply(baza, is.factor)
baza.f<-baza[,fact]



# DYSKRETYZACJA - FINE CLASSING



percentile<-apply(X=baza.n, MARGIN=2, FUN=function(x) round(quantile(x, seq(0.1,1,0.1), na.rm=TRUE),2))
# apply() - funkcja apply wykonuje funkcj? na ka?dym wierszu lub kolumnie zbioru
# x= obiekt
# MARGIN= 1 - dzia?anie na wierszach, 2 dzialanie na kolumnach, c(1,2) na wierszach i kolumnach
# FUN= definicja funkcji 
# function(x) round(quantile(x, seq(0.1,1,0.1), na.rm=TRUE),2) definicja w?asnej funkcji

# a co je?li poziom?w zmiennej numerycznej jest mniej ni? 10, czy liczenie perecentyli ma sens?
# to ma znaczenie dla funkcji smbinning, o kt?rej p??niej

# wyliczenie liczby unikalnych wartosci per kolumna 
unique<-apply(baza.n, MARGIN=2, function(x) length(unique(x)))

# wybranie tylko tych kolumn, dla ktorych liczba unikalnych wartosci jest wieksza lub rowna 10
numeric<-colnames(baza.n[which(unique>=10)])
#zmienne o mniejszej liczbie ni? 10 unikalnych warto?ci
num_as_fact<-colnames(baza.n[which(unique<10 & unique>1 )])
#wybranie zmiennych tylko o jednej warto?ci nie ma zbytnio sensu, dlaczego?
#mo?e si? jednak zdarzy?, ?e czasem ma sens, dlaczego?

# kategoryzacja zmiennych wg percentyli
options(scipen=999) 
#ustawienie sposobu wy?witalnia warto?ci numerycznych - wy?aczenie notacji naukowej


# Dla zmiennych z wi?ksz? liczb? unikalnych warto?ci ni? 10 przygotowanie zmiennej z kategoriami



#dla wektora zmienych numeric stworz zienna_fine 
#potnij ja tak zeby wlozylo unikalne wartosci percentyli
#etykiety jako <= od wartoci percentyla
for (m in numeric)
  {
  baza.n[,paste(m,"_fine", sep="")]<-cut(
    x=as.matrix(baza.n[m]), 
    breaks=c(-Inf,unique(percentile[,m])), 
    labels =c(paste("<=",unique(percentile[,m])))
    ) 
  }

# funkcja cut()
# x - wektor numeryczny, kt?ry ma zosta? zamieniony na faktor przez poci?cie zmiennej
# breaks - punkty uciecia
# labels - nadanie etykiet (przydatne do rysunk?w)

# zamiana na factory tam, gdzie liczba unikalnych wartosci <10 - dla takich zmiennych to jest proces kategoryzacji 

num_as_fact[,paste(num_as_fact,"_fine",sep="_")]<-lapply(num_as_fact,as.factor)
fact<-cbind(fact, num_as_fact)
# cbind() - laczenie obiekt?w kolumnami


# wyliczenie Weight of Evidence za pomoc? pakietu smbinning

# w tym pakiecie przyjmujete jest, ze 0 to zly klient --> nale?y zmieni? definicj? w zmiennej zaleznej
baza.n$def_woe<-(1- UCI$DEF)
baza.n$def<-UCI$DEF

#Utworzenie listy do wypelnienia odpowiednimi danymi
WOE<-list()

#Utworzenie data frame na warto?ci Information Value
IV<-data.frame(VAR=character(), IV=integer())

#Wybranie zmiennych z konc?wk? _fine 
baza.n_fine<-baza.n[ ,grepl(pattern="_fine" , x=names(baza.n))]
?grepl

#pattern - czego szukamy
#x - wektor, kt?ry przeszukujemy

# install.packages("smbinning")
library(smbinning)

smbinning.eda(baza.n, rounding = 3, pbar = 1)
#podstawowe miary opisuj?ce zmienne

# Otwarcie pliku PDF w celu zapisywania kolejnych wykres?w
pdf(file="WoE_numeric1.pdf",paper="a4")

# Wyb?r zmiennych do analizy
names.n<-colnames(baza.n[,!names(baza.n) %in% c(colnames(baza.n_fine),"def","def_woe")])

#Ustanownienie paska post?pu

total_pb <- length(names.n)
pb<- txtProgressBar(min = 0, max = total_pb, style = 3)
# min, max - wartosci ekstremalne paska postepu
# style=3 pokazuje procentowy udzial ukonczonych zadan
i<-names.n[1]
for (i in names.n){
  
  # ustawienie liczby kolumn i wierszy przy laczeniu wykres?w
  par(mfrow=c(2,2))
  
  # smbinning.custom(df, y, x, cuts)
  # df - data frame
  # y - flaga GB
  # x - czynnik ryzyka
  # cuts - punkty ci?cia
  
  results<- smbinning.custom(df=baza.n, y="def_woe", x=i, cuts=unique(percentile[,i]))
  
  # Relevant plots (2x2 Page) 
  
  # BOXPLOT
  #w rozbiciu na klientow w defaulcie i nie w defaulcie
  boxplot(baza.n[,i]~baza.n$def, 
          horizontal=T, frame=F, col="lightgray",main="Distribution") 
  mtext(i,3) 
  # Rozk?ad cz?sto?ci 
  smbinning.plot(results,option="dist",sub=i)
  # Udzia? z?ych klient?w
  smbinning.plot(results,option="badrate",sub=i) 
  # WoE
  smbinning.plot(results,option="WoE",sub=i)
  
  

  # Doklejenie wiersza do tabeli IV z wyniami dla danej zmiennej
  #frakcja poszzegolnych grup w zmiennej
  IV<-rbind(IV, as.data.frame(cbind("VAR"=i, "IV"=results$ivtable[results$ivtable$Cutpoint=="Total", "IV"])))
  
  # Zapisanie do tabeli Kategorii, WoE i udzia?u procentowego 
  d<-results$ivtable[,c("Cutpoint","WoE","PctRec")]
  # Usuniecie wiersza Total nic nie wnosi  
  d<-d[d$Cutpoint!="Total",]
  # Uszeregowanie tabeli od najwi?kszego do najmniejszego WoE - potrzebne m.in. do policzenia Giniego, K-S
  d<-d[with(d, order(d$WoE)),]
  # NUmer porz?dkowy  
  d$numer<-11:(nrow(d)+10)
  # Zapisanie wynik?w w li?cie 
  WOE[[i]]<-d
  
  #Update paska post?pu  
  setTxtProgressBar(pb,  min(grep(i, names.n)))
} 
close(pb)
dev.off()








# Zmienne Factorowe
smbinning.eda(baza.f, rounding = 3, pbar = 1)

#wektor zmiennych do analizy
names.f<-colnames(baza.f[,!names(baza.f) %in% c("def","def_woe")])

#zmienne GB i odwrocone GB
baza.f$def_woe<-(1- UCI$DEF)
baza.f$def<-UCI$DEF

#wyszukanie poziom?w gdzie zawsze s? tylko 0 lub tylko 1 - dlaczego?
for (i in names.f){
temp<-table(baza.f[,i],baza.f$def)
temp<-temp[temp[,1]==0|temp[,2]==0,,drop=FALSE]
#drop=FALSE - nie usuwa nazw wierszy/kolumn gdy obiekt redukuje si? do wektora
print(i)
print(temp)
}

for (i in names.f){
baza.f[,i]<-gsub(pattern="8d", replacement = "7d", x=baza.f[,i])
baza.f[,i]<-gsub("9d", "7d", baza.f[,i])
#gsub() - zamiana warto?ci 
  # pattern - szukana warto??
  # replacement - warto?? na kt?ra nale?y zamieni? 
  # x - obiekt przeszukiwany
}

baza.f$PAY_5F<-gsub("1d", "2d", baza.f$PAY_5F)

# na wszelki wypadek, gdyby jakas operacja zmieni?a na inny typ zmienne
baza.f <- data.frame(apply(baza.f[names.f], 2, as.factor),as.numeric(baza.f$def_woe),as.numeric(baza.f$def))
colnames(baza.f)[10:11]<-c("def_woe","def")


# Otwarcie PDF
pdf(file="WoE_factor.pdf",paper="a4")

# pasek post?pu
total_pb <- length(names.f)
pb<- txtProgressBar(min = 0, max = total_pb, style = 3)

for (i in names.f){
  
  #potrzebne do ujednolicenia wyliczenie statystyk dla zmiennych numeric i factor
  baza.f[,paste(i,"_fine", sep="")]<-baza.f[,i]
  
  par(mfrow=c(2,2))
  
  # smbinning.custom(df, y, x, cuts)
  # df - data frame
  # y - flaga GB
  # x - czynnik ryzyka
  # cuts - punkty ci?cia
  
  results<- smbinning.factor(df=baza.f, y="def_woe", x=i, maxcat=length(unique(baza.f[,i])))
  # df - data frame
  # y - flaga GB
  # x - zmienna 
  # maxcat - liczba maksymalna kategorii

  smbinning.plot(results,option="dist",sub=i) 
  smbinning.plot(results,option="badrate",sub=i) 
  smbinning.plot(results,option="WoE",sub=i)
  
  IV<-rbind(IV, as.data.frame(cbind("VAR"=i, "IV"=results$ivtable[results$ivtable$Cutpoint=="Total", "IV"])))

  d<-results$ivtable[,c("Cutpoint","WoE","PctRec")]
 
  d<-d[d$Cutpoint!="Total",]
 
  d<-d[with(d, order(d$WoE)),]
  
  d$numer<-11:(nrow(d)+10)
  
  WOE[[i]]<-d
  
  setTxtProgressBar(pb,  min(grep(i, names.f)))
} 
close(pb)
dev.off()

# tu koniec
# Wyliczenie statystyk oceniajacych jakosc poszczegolnych znmiennych


install.packages("forcats")
library(forcats)
install.packages("pROC")
library(pROC)

# polaczenie baz numeric i factor
baza<-cbind(baza.f,baza.n)[,1:46]


# progress bar
total_pb <- length(names(WOE))
pb<- txtProgressBar(min = 0, max = total_pb, style = 3)
stats<-cbind(IV, Gini=NA, miss=NA)


for (l in names(WOE)){
  # Zamiana wartosci w zmiennej po fine classingu z NA na 'Missing'  
  baza[,paste(l,"_fine", sep="")]<-fct_explicit_na(baza[,paste(l,"_fine", sep="")], na_level="Missing")
  
  # obiekty potrzebne do wyliczenia Giniego
  zmienna<-baza[,c("def_woe", paste(l,"_fine", sep=""))]
  woe<- WOE[[l]][c("Cutpoint", "WoE")]
  
  #zmiana wartosci w zmiennej Cutpoint je?eli zmienna oryginalna by?a Factorem
  if (is.character(woe$Cutpoint)==TRUE) 
  { 
    woe$Cutpoint<-as.factor(gsub("= '|'", "", woe$Cutpoint))
    woe$Cutpoint<-as.factor(woe$Cutpoint)
  }
  
  # laczenie poziom?w zmiennej z warto?ciami WoE
    # merge()
    # x,y - tabele
    # by.x .y klucz
    # all.x=T - left join
  
  zbior_temp<-merge(zmienna, woe, by.x=paste(l,"_fine", sep=""), by.y="Cutpoint", all.x=T)
 
  # zmiana nazwy zmiennej
  colnames(zbior_temp)[which(names(zbior_temp) == "WoE")] <- paste(l, "_woe", sep="")  
  
  # dodanie zmiennej z wartosciami WoE do oryginalnego zbioru
  baza<-merge(baza, woe, by.x=paste(l,"_fine", sep=""), by.y="Cutpoint", all.x=T)
  colnames(baza)[which(names(baza) == "WoE")] <- paste(l, "_woe", sep="")
  
  #check, czy wszystkie obserwacje maja przypisana wartosc WoE
  print(c(any(is.na(zbior_temp[,paste(l, "_woe", sep="")])), l))
  
  
  # wyliczenie wartosci giniego
  #   gini = 2*AUROC-1
  #   auc() funkcja licz?ca giniego
  #   ?pROC
  
  gini<- c(2*auc(zbior_temp$def_woe,zbior_temp[,paste(l, "_woe", sep="") ])-1)
  
  stats[stats$VAR==l, "Gini"]<-gini
  
  
  miss<-1-c(nrow(zbior_temp[zbior_temp[,paste(l,"_fine", sep="")]!='Missing', ])/nrow(zbior_temp))
  
  stats[stats$VAR==l, "miss"]<-miss
  
  
  setTxtProgressBar(pb,  min(grep(l, names(WOE))))
  
}

close(pb)

write.csv(stats, "stats.csv")
dir()



###########################  ANALIZA KORELACJI ###############################

# wyb?r tylko zmiennych z _woe
zm_do_analizy<-colnames(baza)[grep("_woe", colnames(baza))]
# wylaczenie zmiennej def_woe
zm_do_analizy<-zm_do_analizy[-1]
baza_kor<-baza[,zm_do_analizy]


# Por?wnanie dw?ch metod liczenia korelacji Kendalla
#   Czemu Kendall?
#   The distribution of Kendall?s tau has better statistical properties.
#   The interpretation of Kendall?s tau in terms of the probabilities of observing the agreeable (concordant) and non-agreeable (discordant) pairs is very direct.
#   In most of the situations, the interpretations of Kendall?s tau and Spearman?s rank correlation coefficient are very similar and thus invariably lead to the same inferences.


# pobranie informcaji o czasie przed startem zadania

# tim <- proc.time ()[1]	## applying cor (standard R implementation)
#   # cor()
#   # x- baza
#   # method - typ miary korelacji
#   # use - spos?b postepowania przy brakach danych "pairwise" - dla ka?dej pary zmiennych usuwa te obserwacje,
#   # w kt?rych wystepuje przynajmniej jeden missing
# 
# kendall <-cor (baza_kor, method = "kendall",use="pairwise")
# 
#   # algorytm rz?du o(n^2)
# cat ("cor runtime [s]:", proc.time ()[1] - tim, "(n =", nrow (baza_kor), ")\n")
# 
#   #cor runtime [s]: 3645.02 (n = 30000 )
# 
#   # zapisanie zbioru korelacji
# save(kendall,file="kendall.RData")

  # wczytanie
load(file="kendall.RData")

  #wykorzystanie algorytmu rz?du o(n*log(n))
  # cor runtime [s]: 0.67 (n = 30000 )
  
  #install.packages("pcaPP")
library(pcaPP)
tim <- proc.time ()[1]	
kendall2 <-cor.fk (baza_kor)
cat ("cor runtime [s]:", proc.time ()[1] - tim, "(n =", nrow (baza_kor), ")\n")
save(kendall2,file="kendall2.RData")


# Wyb?r zmiennych, kt?re zostan? wybrane do coarse classingu

kor_list<-data.frame()

# uporzadkowanie macierzy korelacji zgodnie z wartosci Gini (wiersze i kolumny)
    #utworzenie zmiennej z woe
  stats$VARW<-paste(stats$VAR,"_woe",sep="")
    # ?aczenie statystyk i wynik?w korelacji zmiennych - ?eby uzyska? Gini
  stat<-merge(x = stats, y = kendall2, by.x = "VARW",by.y="row.names",all.y=T)
    #posortowanie zbioru po Gini od najwi?kszych wartosci
  stat<-stat[ order(-stat[,"Gini"]), ]
    #nadanie wierszom nazw jak zmienne
  row.names(stat)<-stat[,1]
    #wyb?r tylko zmiennych do macierzy korelacji
  stat<-stat[6:length(stat[1,])]
    #wektor nazw zmiennych z wierszy
  cols<-row.names(stat[,])
    #uporzadkowanie kolumn wedlug Gini
  stat<-stat[cols]
    
    #w sytuacji, gdy brakuje wartosci korelacji przypisanie minimalnej wartosci, 
    #ktora nie wyelimuje zdnej zmiennej z analizy
  temp_k<-stat
  temp_k<-replace(temp_k, is.na(temp_k), 0.000001000)

  # 
  # petla reapet()
  # wykonuje zadania a? nie zostanie przerwana (w tym przypadku warunek w petli if)
  
  # install.packages("plyr")
  library(plyr)
  threshold<-0.5
  
repeat{
  # pomimo, ze ten warunek jest na poczatku, to ma on charakter dodatkowy
  # w tym przypadku warunkiem przerwania petli jest redukcja macierzy do zbioru pustego
  # to moze sie wydarzyc, jezeli w ostatnim wykonaniu petli wszystkie pozostale zmienne okazaly sie spelniac warunek
  # > threshold 
  if (length(temp_k)<1) {
    break
  }
  
    # wyb?r wszystkich zmiennych dla kt?rych wsp korelacji z pierwsz zmienna w zbiorze wi?kszy od threshold
    # Uwaga: wsp korelacji dla analizowanej zmiennej rowna sie 1 wiec tez bedzia ta zmienna wybrana
  wiersz_k<-abs(temp_k[1,]) > threshold
  wiersz_k[1]<-TRUE
  
    # warunek sprawdza czy nie jestesmy juz na koncu analizy i zostala nam ostatnie zmienna do analizy
    # jezeli nie to wybieram tylke te zmienne dla ktorych warunek > threshold jest spelniony
    # jezeli tak to biore tylko jedna zmienna ktora zostala
  if(length(wiersz_k)>1){
    wiersz_k2<-wiersz_k[,wiersz_k]
  }else{wiersz_k2<-wiersz_k}
    # z temp_k wycinam wiersze i kolumny, ktore  zostaly zapisane w wiersz_k2
    # , zeby w dalszej analize juz ich nie uwzgledniac
  temp_k<-as.data.frame(temp_k[!t(wiersz_k),!wiersz_k])
  
    #jak zostanie jedna zmienna to R zredukuje DF do wektora i trzeba nadac nazwy wierszom i kolumnom
  if(length(temp_k)==1){ 
    colnames(temp_k)<- dimnames(wiersz_k)[[2]][-which(wiersz_k)]
    rownames(temp_k)<- dimnames(wiersz_k)[[2]][-which(wiersz_k)]
  }
  
  
    #przygotowanie tabeli z nazwami zmiennych, ktroe spelniaja warunek >threshold
  if(length(wiersz_k2)==1) {
    zmienna<-row.names(wiersz_k)
    nazwy<-as.data.frame(zmienna)
  }
  
  if(length(wiersz_k2)>1) {
    wiersz_k2<-wiersz_k2[2:length(wiersz_k2)]
    wiersz_k2<-as.data.frame(t(wiersz_k2))
    nazwy<-colnames(wiersz_k2)
    nazwy<-as.data.frame(t(nazwy))
    row.names(nazwy) <- row.names(wiersz_k)
    zmienna<-row.names(wiersz_k)
    zmienna<-as.data.frame(zmienna)
    nazwy<-merge(zmienna,nazwy)
  }
  
  
    #doklejenie do kor_list wiersza z informacja o skorelowanych zmiennych z analizowana zmienna
    #rbind.fill() - filling missing columns with NA
  kor_list<-rbind.fill(kor_list,nazwy)
  
    # warunek wyjscia z petli - gdy ropatrywana jest ostatnia zmienna
  if(length(temp_k)==1){ 
    zmienna<-dimnames(wiersz_k)[[2]][-which(wiersz_k)]
    nazwy<-as.data.frame(zmienna)
    kor_list<-rbind.fill(kor_list,nazwy)
    break}
  
}

    # ?aczenie zbioru ze statystykami i wynikami korelacji
  all<-merge(x = stats, y = kor_list, by.x = "VARW",by.y="zmienna",all.y=T)
    # sortowanie po Gini
  all<-all[ order(-all[,"Gini"]), ]
   # wylaczenie zmiennych z gini ponizej 0.1
  all<-all[all$Gini>0.08,]
  write.csv(all, "wyniki_fine_classing.csv")
  
  
  ################################################################################################
  
  baza_coarse<-baza[-grep("_fine",colnames(baza))]
  kols<-c(as.character(all$VAR),"def","def_woe")
  baza_coarse<-baza_coarse[kols]
  
  # PODZIAL PROBY NA PROBE UCZ?C?/TRENINGOW? i WALIDACYJN?/TESTOW?
  size<-0.7
    # losowanie proste
      # ustawienie ziarna losowania pozwala odtworzyc dokladnie podzial (losowanie)
      set.seed(1916)
      smpl<-sample(x=(1:nrow(baza_coarse)), size=size*nrow(baza_coarse))
      train_simple<-baza_coarse[smpl,]
      test_simple<-baza_coarse[-smpl,]
      #przy losowaniu prostym nie ma mo?liwo?ci kotroli ?adnej miary (np. r?nwego DR)
      mean(train_simple$def)
      mean(test_simple$def)
    
    # to samo za pomoca fukcji sample.spli()
     install.packages("caTools")
       library(caTools)
       set.seed(1916)
       smp_split2<-sample.split(baza_coarse$def, SplitRatio = size)
       #SplitRatio je?eli 0<SR<1 to udzial zbioru
       train_simple2<-baza_coarse[smp_split2==T,]
       test_simple2<-baza_coarse[smp_split2==F,]
       #przy losowaniu prostym nie ma mo?liwo?ci kotroli ?adnej miary (np. r?nwego DR)
       mean(train_simple2$def)
       mean(test_simple2$def)
   
   #losowanie ze stratyfikacj?    
       
      #podzial na table dla ka?dej starty osobno
      # lista z liczba elemnt?w r?wn? liczbie pozi?w startyfikacyjnych
      # f - podzial na przekroje
       d_split<-split(x=baza_coarse, f=baza_coarse$def)
      # paste(baza_coarse$def,baza_coarse$def) je?eli dla wi?cej ni? jednego elemetu
  
      #progress bar
      total <- length(d_split)
      pb<- txtProgressBar(min = 0, max = total, style = 3)
  
      # tabele train i test
      train = list()
      test=list()

      # petla for z licza loop?w r?wn? licznie poziom?w straty
      
    for (i in 1:length(d_split)){
      set.seed(1916);
      
      # utworzenie wektora dzielacego na proby train i test dla i-tej straty
      assign(
        paste("sample",i,sep="_"),sample.split(d_split[[i]][,1], SplitRatio = .7))
      
      # utworzenie zbioru trenuj?cego dla i-tej straty
      assign(
        paste("train",i,sep="_"),subset(d_split[[i]],  get(paste("sample",i,sep="_")) == TRUE))
      
      # utworzenie zbioru testuj?cego dla i-tej straty
      assign(
        paste("test",i,sep="_"),subset(d_split[[i]], get(paste("sample",i,sep="_")) == FALSE))
      
      train[[i]]<-get(paste("train",i,sep="_"))
      test[[i]]<-get(paste("test",i,sep="_"))
      setTxtProgressBar(pb, i)}
  
  # sklejenie wierszowe elemet?w list train i test
  train = do.call(rbind, train)
  test = do.call(rbind, test)
  
  # usuniecie niepotrzebnych tabel
  rm( test_1, test_2
       , train_1, train_2
       , sample_1, sample_2)
  
  save(train,file="train")
  save(test,file="test")
  
  
  

  
  
  
  
  
  
