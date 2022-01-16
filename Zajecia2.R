# WCZYTYWANIE PAKIETOW -----------------------------------------------------------------
library(zoo)
library(lattice) #rysunki

library(pROC) # gini itp
library(forcats) #NA na Missing
library(RColorBrewer) #paleta kolor?w

library(smbinning) #smbinning

 #install.packages("devtools") #install_github
library(devtools)

# install_github("riv","tomasgreif") #iv.mult
library(woe)

#install.packages("woeBinning") #woeBinning
library(woeBinning)

#install.packages("plyr") #revalue
library(plyr)

setwd("C:/Users/Anna/Desktop/UW/Modelowanie Ryzyka Kredytowego")

# Reset parametr?w do wykres?w
resetPar <- function() {
  dev.new()
  op <- par(no.readonly = TRUE)
  dev.off()
  op
}


par(resetPar()) 
par(xpd = T, mfrow=c(1,1))


# COARSE CLASSING --------------------------------------------------------
# ladowanie zbioru trenujacego
load("train")
load("test")
dir()

kolumny<-colnames(train)[-which(names(train) %in% c("def","def_woe"))]


nums <- sapply(train[,kolumny], is.numeric) 
#aspply() przypisanie funkcji {is.numeric} do kazdego elementu zbioru {baza}
kolumny.n<-kolumny[nums==T]

# progress bar
total <- length(kolumny.n)
pasek<- txtProgressBar(min = 0, max = total, style = 3)

temp<-data.frame(VAR=character(), VALUE=integer())
stats<-data.frame(VAR=character(), IV=integer(),Gini=integer(), MISS=integer(),IVw=integer(),Giniw=integer(), MISSw=integer())
#zbieranie punktow odciecia na potrzeby zbioru testowego
cut_offs<-data.frame(VAR=character(), cuts=integer())

zmienna_c<-kolumny.n[2]

pdf("WoE_coarse.pdf")
for (zmienna_c in kolumny.n){
  
  
  par(xpd = T, mar = par()$mar, mfrow=c(2,2))
  
  #smbinning wg drzewa decyzyjnego
    # Optimal Binning categorizes a numeric characteristic into bins for ulterior usage in scoring modeling. 
    # This process, also known as supervised discretization, utilizes Recursive Partitioning to 
    # categorize the numeric characteristic.
    # The especific algorithm is Conditional Inference Trees which initially excludes missing values (NA) 
    # to compute the cutpoints, adding them back later in the process for the calculation of the Information Value.
  
  result<-smbinning(train[,c("def_woe", zmienna_c)], y="def_woe", x=zmienna_c, p=0.05)
  
  #zadania dla ktorych udalo si? wykona? funkcj?
  if(length(result)>1){
    points<-list(result$cuts)
    cut_offs<-rbind(cut_offs,as.data.frame(cbind("VAR"=zmienna_c,"cuts"=points)))
    
    IV<-result$ivtable
    
    
    #przypisaie zmienne po Coarse Classingu do zbioru  
    train[,paste(zmienna_c, "_coarse", sep="")]<- cut(train[,zmienna_c], breaks=c(-Inf,unique(result$cuts),Inf), 
                                                 labels=c(paste("<=", unique(result$cuts)),"<= Inf"),
                                                 include.lowest = T)
    
    train[,paste(zmienna_c, "_coarse", sep="")]<-fct_explicit_na(train[,paste(zmienna_c, "_coarse", sep="")], na_level="Missing")
    
    test[,paste(zmienna_c, "_coarse", sep="")]<- cut(test[,zmienna_c], breaks=c(-Inf,unique(result$cuts),Inf), 
                                                      labels=c(paste("<=", unique(result$cuts)),"<= Inf"),
                                                      include.lowest = T)
    test[,paste(zmienna_c, "_coarse", sep="")]<-fct_explicit_na(test[,paste(zmienna_c, "_coarse", sep="")], na_level="Missing")
    
    
    IVw<-sum(iv.mult(test,"def",vars=paste(zmienna_c,"_coarse",sep=""))[[1]][,"miv"])
    IV$Cutpoint<-ifelse(grepl(">",IV$Cutpoint)==T,"<= Inf",IV$Cutpoint)
    
      #doklejenie zmiennej z WoE i zmiana nazwy
    train<-merge(train,IV[,c("Cutpoint", "WoE")],by.x= paste(zmienna_c, "_coarse", sep=""), by.y="Cutpoint", all.x=T, sort=F)
    colnames(train)[which(names(train) == "WoE")] <- paste(zmienna_c, "_woe", sep="") 
    
    test<-merge(test,IV[,c("Cutpoint", "WoE")],by.x= paste(zmienna_c, "_coarse", sep=""), by.y="Cutpoint", all.x=T, sort=F)
    colnames(test)[which(names(test) == "WoE")] <- paste(zmienna_c, "_woe", sep="")
    
      #GINI
    gini<- 2*auc(train$def_woe,train[,paste(zmienna_c, "_woe", sep="") ])-1
    giniw<- 2*auc(test$def_woe,test[,paste(zmienna_c, "_woe", sep="") ])-1
      #policz % missing?w
    miss<-1-nrow(train[!(is.na(train[,zmienna_c])),])/nrow(train)
    missw<-1-nrow(test[!(is.na(test[,zmienna_c])),])/nrow(test)
    stats<-rbind(stats, as.data.frame(cbind("VAR"=zmienna_c, "IV"=IV[IV$Cutpoint=="Total", "IV"],"Gini"=gini,  "MISS"=miss,"IVw"=IVw, "Giniw"=giniw,  "MISSw"=missw)))
    
    
    wykres<-IV[!is.na(IV$WoE) & IV$Cutpoint!="Total",]
    wykres$WoE<-ifelse(wykres$WoE==Inf, 8, ifelse(wykres$WoE==-Inf, -8,wykres$WoE))
    g<-barplot(wykres$WoE,names.arg=wykres$Cutpoint, cex.names=0.5, main=zmienna_c, xaxt="n")
    print(g)
    axis(1,g,wykres$Cutpoint, tick=F, las=2, cex.axis=0.7)
    text(g,wykres$WoE, labels=paste("WoE=",format(wykres$WoE,digits=1)," \n Fill=",round(wykres$PctRec,3)*100, "%", sep=""), xpd = T, col = "black", pos=3, cex=0.7)
    
    # Stabilnosc
    
    freq.test<-as.data.frame(table(test[,paste(zmienna_c, "_coarse", sep="")])/ nrow(test))
    colnames(freq.test)<-c("Cutpoint", "PctRec")
    freq.train<-wykres[,c("Cutpoint","PctRec")]
    freq<-merge(freq.train,freq.test, all.x=T, by="Cutpoint")
    colnames(freq)<- c("Cutpoint","Train","Test")
    rownames(freq)<-freq[,1]
    freq<-freq[,-1]
    library(ggplot2)
    
  
    stacked<-barplot(as.matrix(freq), beside=F, main=zmienna_c)
    print(stacked)
    legend("topright", legend=row.names(freq), cex=0.6, bty="o",pch=21)

    # Rozk?ad cz?sto?ci 
    smbinning.plot(result,option="dist",sub=zmienna_c)
    # Udzia? z?ych klient?w
    smbinning.plot(result,option="badrate",sub=zmienna_c) 
    
    
    }
  #pokaz dla jakich zmiennych policzyl woe
  temp<-rbind(temp, as.data.frame(cbind(VAR=zmienna_c,VALUE=length(result)>1 )))
  names(temp)<-c("VAR","VALUE")
  
  setTxtProgressBar(pasek,  min(grep(zmienna_c, kolumny.n)))
}
dev.off()


# Zmienne factorowe

    # woe.bin_f<-woe.binning(train, target.var="def", pred.var=c("EDUCATIONF","PAY_0F","PAY_3F","PAY_6F"), min.perc.total=0.05,min.perc.class=0, stop.limit=0.1)
    # woe.binning.plot(woe.bin_f, multiple.plots=FALSE)
    # woe.binning.table(woe.bin_f)
    # train<-woe.binning.deploy(train, woe.bin_f,add.woe.or.dum.var='woe')
    # test<-woe.binning.deploy(test, woe.bin_f,add.woe.or.dum.var='woe')

# df	= Name of data frame with input data.
# target.var	= Name of dichotomous target 
# pred.var	= Name of predictor variable(s) 
# min.perc.total =	
#   For numeric variables this parameter defines the number of initial classes before any merging is applied. 
#   For example min.perc.total=0.05 (5%) will result in 20 initial classes. 
#   For factors the original levels with a percentage below this limit are collected in a ?miscellaneous? level before the merging
#   based on the min.perc.class and on the WOE starts. 
#   Accepted range: 0.01-0.2; default: 0.05.
# min.perc.class =
#   If a column percentage of one of the target classes within a bin is below this limit (e.g. below 0.01=1%) then the respective bin 
#   will be joined with others. In case of numeric variables adjacent predictor classes are merged. 
#   For factors respective levels (including sparse NAs) are assigned to a ?miscellaneous? level. 
#   Accepted range: 0-0.2; default: 0, i.e. no merging with respect to sparse target classes is applied.
# stop.limit	
# Stops WOE based merging of the predictor's classes/levels in case the resulting information value (IV) 
# decreases more than x% (e.g. 0.05 = 5%) compared to the preceding binning step. stop.limit=0 will skip any WOE based merging. 
# Increasing the stop.limit will simplify the binning solution and may avoid overfitting. 
# Accepted range: 0-0.5; default: 0.1.
# abbrev.fact.levels	

# laczenie kategorii segment new

train$EDUCATIONF_coarse<-revalue(train$EDUCATIONF, c('university'='univ/hs', "high school"='univ/hs'))
test$EDUCATIONF_coarse<-revalue(test$EDUCATIONF, c('university'='univ/hs', "high school"='univ/hs'))

train$PAY_0F_coarse<-revalue(train$PAY_0F, c('2d'="2d+", "3d"="2d+", "4d"="2d+", "5d"="2d+", "6d"="2d+", "7d"="2d+"))
test$PAY_0F_coarse<-revalue(test$PAY_0F, c('2d'="2d+", "3d"="2d+", "4d"="2d+", "5d"="2d+", "6d"="2d+", "7d"="2d+"))

train$PAY_2F_coarse<-revalue(train$PAY_2F, c('1d'="1d+", '2d'="1d+", "3d"="1d+", "4d"="1d+", "5d"="1d+", "6d"="1d+", "7d"="1d+"))
test$PAY_2F_coarse<-revalue(test$PAY_2F, c('1d'="1d+",'2d'="1d+", "3d"="1d+", "4d"="1d+", "5d"="1d+", "6d"="1d+", "7d"="1d+"))

train$PAY_5F_coarse<-revalue(train$PAY_5F, c('2d'="2d+", "3d"="2d+", "4d"="2d+", "5d"="2d+", "6d"="2d+", "7d"="2d+"))
test$PAY_5F_coarse<-revalue(test$PAY_5F, c('2d'="2d+", "3d"="2d+", "4d"="2d+", "5d"="2d+", "6d"="2d+", "7d"="2d+"))


# table(train[,"EDUCATIONF_coarse"],train$def,useNA = "always")
kolumny.f <- c("PAY_0F_coarse","PAY_2F_coarse","PAY_5F_coarse","EDUCATIONF_coarse") 

# progress bar
total <- length(kolumny.f)
pasek<- txtProgressBar(min = 0, max = total, style = 3)



zmienna_c<-kolumny.f[1]

resetPar <- function() {
  dev.new()
  op <- par(no.readonly = TRUE)
  dev.off()
  op
}

par(resetPar()) 


zmienna_c

pdf("WoE_coarse_fact12.pdf")
for (zmienna_c in kolumny.f){
  
  
  par(xpd = T, mfrow=c(2,2))

  result<-smbinning.factor(train[,c("def_woe", zmienna_c)], y="def_woe", x=zmienna_c)


if(length(result)>1){
  points<-list(result$cuts)
  cut_offs<-rbind(cut_offs,as.data.frame(cbind("VAR"=zmienna_c,"cuts"=points)))
  
  IV<-result$ivtable
  
  
  #NA na Missing  
  train[,zmienna_c]<-fct_explicit_na(train[,zmienna_c], na_level="Missing")
  test[,zmienna_c]<-fct_explicit_na(test[,zmienna_c], na_level="Missing")
  
  
  IVw<-sum(iv.mult(test,"def",vars=zmienna_c)[[1]][,"miv"])
  IV$Cutpoint<-as.factor(gsub("= |'","",IV$Cutpoint))

  #doklejenie zmiennej z WoE i zmiana nazwy
  train<-merge(train,IV[,c("Cutpoint", "WoE")],by.x= zmienna_c, by.y="Cutpoint", all.x=T, sort=F)
  colnames(train)[which(names(train) == "WoE")] <- gsub("_coarse","_woe",zmienna_c)
  
  test<-merge(test,IV[,c("Cutpoint", "WoE")],by.x= zmienna_c, by.y="Cutpoint", all.x=T, sort=F)
  colnames(test)[which(names(test) == "WoE")] <- gsub("_coarse","_woe",zmienna_c)
  
  #GINI
  gini<- 2*auc(train$def_woe,train[,gsub("_coarse","_woe",zmienna_c) ])-1
  giniw<- 2*auc(test$def_woe,test[,gsub("_coarse","_woe",zmienna_c)])-1
  #policz % missing?w
  miss<-1-nrow(train[!(is.na(train[,zmienna_c])),])/nrow(train)
  missw<-1-nrow(test[!(is.na(test[,zmienna_c])),])/nrow(test)
  stats<-rbind(stats, as.data.frame(cbind("VAR"=zmienna_c, "IV"=IV[IV$Cutpoint=="Total", "IV"],"Gini"=gini,  "MISS"=miss,"IVw"=IVw, "Giniw"=giniw,  "MISSw"=missw)))
  
  #wykres WoE
  wykres<-IV[!is.na(IV$WoE) & IV$Cutpoint!="Total",]
  wykres$WoE<-ifelse(wykres$WoE==Inf, 8, ifelse(wykres$WoE==-Inf, -8,wykres$WoE))
  g<-barplot(wykres$WoE,names.arg=wykres$Cutpoint, cex.names=0.5, main=zmienna_c, xaxt="n")
  print(g)
  axis(1,g,wykres$Cutpoint, tick=F, las=2, cex.axis=0.7)
  text(g,wykres$WoE, labels=paste("WoE=",format(wykres$WoE,digits=1)," \n Fill=",round(wykres$PctRec,3)*100, "%", sep=""), xpd = T, col = "black", pos=3, cex=0.7)
  
  # Stabilnosc
  
  freq.test<-as.data.frame(table(test[,zmienna_c])/ nrow(test))
  colnames(freq.test)<-c("Cutpoint", "PctRec")
  freq.train<-wykres[,c("Cutpoint","PctRec")]
  freq<-merge(freq.train,freq.test, all.x=T, by="Cutpoint")
  colnames(freq)<- c("Cutpoint","Train","Test")
  rownames(freq)<-freq[,1]
  freq<-freq[,-1]
  
  stacked<-barplot(as.matrix(freq), beside=F, main=zmienna_c)
  print(stacked)
  
  
  # Rozk?ad cz?sto?ci 
  smbinning.plot(result,option="dist",sub=zmienna_c)
  # Udzia? z?ych klient?w
  smbinning.plot(result,option="badrate",sub=zmienna_c) 
  
  
}
#pokaz dla jakich zmiennych policzyl woe
temp<-rbind(temp, as.data.frame(cbind(VAR=zmienna_c,VALUE=length(result)>1 )))
names(temp)<-c("VAR","VALUE")

setTxtProgressBar(pasek,  min(grep(zmienna_c, kolumny.f)))
}

dev.off()
warnings()

#ZAPISYWANIE -----------------------------------

write.csv(stats, file="stats.csv")
save(train,file="trainm")
save(test,file="testm")
