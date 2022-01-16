getwd()
setwd("D:/mks/zaj1")

install.packages("LogisticDx")
install.packages("gtools")

library(LogisticDx) # - gof()
library(pROC) # - auc(), roc.test itp.
library(gtools) # smartbind()



load(file="trainm")
load(file="testm")


kolsy<-colnames(train)[grep("woe", colnames(train))]


# inf<-c()
# minf<-c()
# nan<-c()

# for (zmienna_c in kolsy){
#   if(any(train[,zmienna_c]==Inf,na.rm=T)){
#     inf<-cbind(inf,zmienna_c)}
#   if(any(train[,zmienna_c]==-Inf,na.rm=T)){
#     minf<-cbind(-inf,zmienna_c)}
#   if(any(is.nan(train[,zmienna_c]))){
#     minf<-cbind(-inf,zmienna_c)}
# }



############################

#FUNKCJE POTRZEBNE DO PRZEPROWADZENIA OCENY JAKOSCI KARTY


hosmerlem = function(y, yhat, g=20) {
  cutyhat = cut(yhat,breaks = quantile(yhat, probs=seq(0,1, 1/g)), include.lowest=TRUE)  
  obs = xtabs(cbind(1 - y, y) ~ cutyhat)  
  expect = xtabs(cbind(1 - yhat, yhat) ~ cutyhat)  
  chisq = sum((obs - expect)^2/expect)  
  P = 1 - pchisq(chisq, g - 2)  
  return(list(chisq=chisq,p.value=P))
  hr=P
}

cal_psi <- function(data1,data2, bench, target, bin)
{
  ben<-sort(data1[,bench]);
  tar<-sort(data2[,target]);
  # get and sort benchmark and target variable
  ttl_bench<-length(ben);
  ttl_target<-length(tar);
  # get total num obs for benchmark and target
  n<-ttl_bench%/%bin; #Num of obs per bin
  psi_bin<-rep(0,times=bin) #initialize PSI=0 for each bin
  
  for (i in 1:bin) # calculate PSI for ith bin
  {
    
    lower_cut<-ben[(i-1)*n+1];
    if(i!=bin){upper_cut<-ben[(i-1)*n+n]; pct_ben<-n/ttl_bench;} else
    {upper_cut<-ben[ttl_bench];
    pct_ben<(ttl_bench-n*(bin-1))/ttl_bench;}
    #last bin should have all remaining obs
    
    pct_tar<-length(tar[tar>lower_cut&tar<=upper_cut])/ttl_target;
    psi_bin[i]<-(pct_tar-pct_ben)*log(pct_tar/pct_ben);
  }
  psi<-sum(psi_bin);
  return(psi);
}


cal_psi_zm <- function(data1,data2, bench, target)
{
  ben<-sort(data1[,bench]);
  tar<-sort(data2[,target]);
  bin<-length(unique(ben))
  bin_tar<-length(unique(tar))
  # get and sort benchmark and target variable
  ttl_bench<-length(ben);
  ttl_target<-length(tar);
  # get total num obs for benchmark and target
  tab_ben<-table(ben)
  pct_ben<-tab_ben/ttl_bench
  names<-names(tab_ben)
  tab_tar<-table(tar)
  
  if (ttl_target!=ttl_bench) {
    tab_tar<-smartbind(tab_ben,tab_tar)
    tab_tar<-tab_tar[2,]
    tab_tar[,is.na(tab_tar)]<-0
  }
  pct_tar<-tab_tar/ttl_target
  psi_bin<-rep(0,times=bin) #initialize PSI=0 for each bin
  
  psi_bin<-(pct_tar-pct_ben)*log(pct_tar/pct_ben);
  psi<-sum(psi_bin);
  return(psi);
}



###########################################################

# ESTYMACJA MODELU

kolsy<-kolsy[-grep("def_woe", kolsy)]
data<-train[,c("def",kolsy)]

# estymacja modelu tylko ze sta?? - do por?wna?
baza<-glm(def ~ 1,data=data, family=binomial("logit"))
summary(baza)
# wart dop = ln(p/(1-p)) -> p=e^wd/(1+e^wd) -> e^-1.259/(1+e^-1.259) -> 22,11%
# sta?a -> wartosc oczekiwana w grupie bazowej -> ca?a pr?ba
#stala w modelu estymowanym na woe zostanie taka sama-nawet po dodaniu nowych zmiennych
mean(train$def)

table(train$PAY_2F_woe)
train$PAY_2F_woe<-ifelse(train$PAY_2F_woe==Inf,0.0201,train$PAY_2F_woe)
test$PAY_2F_woe<-ifelse(test$PAY_2F_woe==Inf,0.0201,test$PAY_2F_woe)

max<-glm(def ~ .-PAY_2F_woe,data=data, family=binomial("logit"))#bo na zajeciach sie spsuła
summary(max)
model<-glm(def ~ LIMIT_BAL_woe + PAY_AMT1_woe + PAY_AMT3_woe + PAY_AMT4_woe + PAY_0F_woe + PAY_2F_woe + EDUCATIONF_woe-PAY_2F_woe,data=data, family=binomial("logit"))
summary(model)



################################################################################

### Analiza jako?ci dopasowania


#test na jako?? dopasowania modelu - zupelnie podstawowy
# za?ozenie por?wnujemy uzyskany model z modele "idealnym" i sprawdzamy, czy uzyskana wartosc max wirogodno?ci jest statystycznie bliska 0
# H0: model jest dobrze dopasowany do danych
gf<-pchisq(model$deviance, model$df.residual,lower.tail = F)
gf
# wniosek?
#model szczegolowy jest tak samo ok jak model ogolny
# test LR na istotono?? zmiennych
# sprawdzamy czy maxW dla modelu jest istotnie wi?ksze ni? dla modelu tylko ze sta?a - test na laczna istotnosc modelu
# H0  zmienne s? ?acznie nieistotne
ist<-pchisq(model$null.deviance-model$deviance, model$df.null-model$df.residual,lower.tail = F)
ist
# wniosek?
#odrzucic H0 o nieistotnosci zmiennych pominietych w modelu
# test Hosmera - Lemeshowa - podstawowy test na jakosc dopasowania w modelach dla binarnej zmiennej zaleznej
# H0: model jest dobrze dopasowany do danych
# ma wiele wad - przede wszystkim jest bardzo wra?liwy na liczbe przedzia??w

hr<-hosmerlem(y=data$def, yhat=fitted(model),g=10)
hr
#model nie ma poprawnej formy funkcyjnej, nie jest dobrze dopasowany do danych
hosmerlem(y=data$def, yhat=fitted(model),g=7)
hosmerlem(y=data$def, yhat=fitted(model),g=8)
hosmerlem(y=data$def, yhat=fitted(model),g=9)
#hr$p.value

#Inne testy dopasowania
# generalnie powinny by? interpretowane ?aczeni, ka?dy z nich analizuje nieco inn? specyfik? dopasowania
# je?eli jeden to OR

gof<-gof(model, g=10)
gof
#drugie testy dotycza poprawnosci jakosci dopasowania
#
# https://cran.r-project.org/web/packages/gof/gof.pdf
# HL <- Hosmer-Lemeshow test
# mHL <- modified Hosmer-Lemeshow test
# OsRo <- Osius - Rojek of the link function test
# 
# S Stukel's tests:
#   SstPgeq0.5	 score test for addition of vector z1
#   SstPl0.5	 score test for addition of vector z2
#   SstBoth	 score test for addition of vector z1 and z2
#   SllPgeq0.5	 log-likelihood test for addition of vector z1
#   SllPl0.5	 log-likelihood test for addition of vector z2
#   SllBoth	 log-likelihood test for addition of vectors z1 and z2



#przypisamie PD do zbior?w 
  # fitted.values - PD
  # linear.predictors - ln(p/(1-p))

data$baza<-baza$fitted.values
data$model<-model$fitted.values
data$max<-max$fitted.values

# przeskalowanie wartosci dopasowana na wybrana skale
  # 660 punkt?w oznacza ODDS = 72 a ODDS si? dubuluje co 40 punkt?w

data$score<-(660-40/log(1/2)*log(1/72))+40/log(1/2)*model$linear.predictors

# Przypisanie PD i SCORE do zbioru testowego i treningowego ca?ego
test$model<-predict(model, newdata=test, type="response") 
test$score<-(660-40/log(1/2)*log(1/72))+40/log(1/2)*predict(model, newdata=test, type="link") 

train$model<-predict(model, newdata=train, type="response") 
train$score<-(660-40/log(1/2)*log(1/72))+40/log(1/2)*predict(model, newdata=train, type="link") 

#test roc - sprawdza czy krzywa ROC jest istotnie lepsza dla dw?ch modeli
#H0 krzywe ROC s? r?wnie dobre
roc_test_baza<-roc.test(data$def, data$model, data$baza,method="d")$p.value
roc_test_baza
#czy model daje takie same prognozy jak model bazowy (ze stala)
roc_test_og<-roc.test(data$def, data$max, data$model,method="d")$p.value
roc_test_og
#czy model jest taki sam jak model ogolny
# wniosek?
#wszystkie modele sa rozne
summary(model)


# mg<-mean(data[data$def==0,c("score")])
# mb<-mean(data[data$def==1,c("score")])
# vg<-var(data[data$def==0,c("score")])
# vb<-var(data[data$def==1,c("score")])
# ng<-length(data[data$def==0,c("score")])
# nb<-length(data[data$def==1,c("score")])
# n<-ng+nb
# s<-sqrt(((ng-1)*vg+(nb-1)*vb)/(n-2))
# 
# u<-mg-mb+qt(0.975,n-2)*s*sqrt(1/ng+1/nb)
# l<-mg-mb-qt(0.975,n-2)*s*sqrt(1/ng+1/nb)
# 
# g<-2*pnorm((mg-mb)/(s*sqrt(2)))-1
# gu<-2*pnorm(u/(s*sqrt(2)))-1
# gl<-2*pnorm(l/(s*sqrt(2)))-1
#mean(data[data$def==0,c("model")])
#mean(data[data$def==1,c("model")])

hist(data[data$def==0,c("score")])
hist(data[data$def==1,c("score")])

# wsp??czynnik gini 
# im wy?szy tym rozk?ady scor?w u z?ych i dobrych klient?w si? od siebie r??ni?
gini_t<-2*auc(data$def,data$model,direction="<")-1
gini_w<-2*auc(test$def,test$model,direction="<")-1
#parametr direction-czy wraz ze wzrostem/spadkiem zmiennej niezleznej rosnie prawdopodbnienstwo defaultu
#czyli czy wraz ze wzrostem estymacji prawdopodbienwta bedzie rosl udzial porazek
#robi przy okazji tst na to czy model nie jest tak przekoszony ze odwraca predykcje
# a model max?
2*auc(data$def,data$max,direction="<")-1



# policzenie przedzia??w ufno?ci dla gini_t
  # method "delong" - analiztyczna postac; "bootstrap" szacowanie za pomc? bootstrap
ci_delong_t<-2*ci.auc(data$def, data$model,method="d",direction="<")-1
# 0.5205047 0.5366548 0.5528049
ci_delong_w<-2*ci.auc(test$def, test$model,method="d",direction="<")-1

  # bootstrap liczy sie dlugo
  # tim <- proc.time ()[1]	## applying cor (standard R implementation)
  # ci_bootstrap<-2*ci.auc(data$def, data$model,method="b",boot.n=500)-1
  # cat ("cor runtime [s]:", proc.time ()[1] - tim)
  
  # cor runtime [s]: 129.51
  # 2.5%       50%     97.5% 
  # 0.5202285 0.5370040 0.5527152 


#statystyka K-S
  # statystyka testu Kolmogorova - Smirnova na podobie?stwo dw?ch rozk?ad?w
  # por?wnywany s? rozk??dy scor?w u dobrych i u z?ych klient?w,
  # im bardziej si? od siebie r??ni? tym lepiej
ks_score_t<-ks.test(data[data$def==0,c("score")],data[data$def==1,c("score")])$statistic
ks_score_w<-ks.test(test[test$def==0,c("score")],test[test$def==1,c("score")])$statistic


########################################################################################


# stabilnosc modelu

#PSI - sprawdza na ile dwa rozk?ady r??ni? si? od siebie - to jest IV tylko tym razem chcemy by by?o ma?e
psi<-cal_psi(data1=data, data2=test, bench="score",target="score",bin=20)
#jesli psi<0.25 to jest ok
# test Kolmogorowa Sminrnova na podobienstwo dw?ch rozkladow 
# por?wnywane s? rozk?ady scora w pr?bie trenuj?cej i testuj?cej
# H0 dwa analizowane rozk?ady s? statystycznie nierozr??nialne

ks<-ks.test(data$score,test$score)$p.value

#najczestsze scory - je?eli powtarza si? cz?sto jeden score, to ma?e zmiany generuj? ryzyko zmiany oceny jako?ci modelu
t<-as.data.frame(sort(table(data$score)/length(data$score),decreasing=T))[1:3,1:2]
w<-as.data.frame(sort(table(test$score)/length(test$score),decreasing=T))[1:3,1:2]


######################################################################################


# Analiza jako?ci w innych wymiarach


#gini tylko dobrzy
table(train$PAY_0F_coarse)
train_g<-train[train$PAY_0F_coarse %in% c("revolving","no consumption", "fully paid"),]
test_g<-test[test$PAY_0F_coarse %in% c("revolving","no consumption", "fully paid"),]
gini_t_g<-2*auc(train_g$def,train_g$model,direction="<")-1
gini_w_g<-2*auc(test_g$def,test_g$model,direction="<")-1

ks_score_t_g<-ks.test(train_g[train_g$def==0,c("score")],train_g[train_g$def==1,c("score")])$statistic
ks_score_w_g<-ks.test(test_g[test_g$def==0,c("score")],test_g[test_g$def==1,c("score")])$statistic

psi_g<-cal_psi(data1=train_g, data2=test_g, bench="score",target="score",bin=20)
ks_g<-ks.test(train_g$score,test_g$score)$p.value
rm(train_g,test_g)



mdl<-"model_og"
zmienne<-names(model$coefficients)[2:length(model$coefficients)]

ocena_zmienne<-NULL
ocena_modeli<-NULL
zmienne_tab<-NULL


for (i in 1:length(zmienne)) {
  tab<-NULL 
  tab$model<-mdl
  tab$v<-zmienne[i]
  tab$gini_t<-2*ci.auc(data[!is.na(data[,zmienne[i]]),c("def")], data[!is.na(data[,zmienne[i]]),zmienne[i]],direction=">",method="d")[2]-1
  tab$gini_w<-2*ci.auc(test[!is.na(test[,zmienne[i]]),c("def")], test[!is.na(test[,zmienne[i]]),zmienne[i]],direction=">",method="d")[2]-1

  tab$psi<-cal_psi_zm(data1=data[!is.na(data[,zmienne[i]]),zmienne], data2=test[!is.na(test[,zmienne[i]]),zmienne], bench=zmienne[i],target=zmienne[i])
  tab<-as.data.frame(tab)
  zmienne_tab<-rbind(zmienne_tab, tab)
}
#pay_amt2 pay_of5 dodac zeby dobic do tego co było na zajeciach



temp_tab<-as.data.frame(cbind("Model"="model_og",
                              
                              'ist_param'=ist,
                              "roc_test_baza"=roc_test_baza,
                              "gof"=gof$gof$pVal[3],
                              "hosmer"=hr$p.value,
                              "gf"=gf,
                              "ist_ogr"=ist,
                              "roc_test_og"=roc_test_og,
                              "gini_t_cil"=ci_delong_t[1],
                              "gini_t"=gini_t,
                              "gini_t_ciu"=ci_delong_t[3],
                              "gini_w_cil"=ci_delong_w[1],
                              "gini_w"=gini_w,
                              "gini_w_ciu"=ci_delong_w[3],
                              "ks_score_t"=ks_score_t,
                              "ks_score_w"=ks_score_w,
                              "psi"=psi,
                              "ks_test"=ks,
                              "gini_t_g"=gini_t_g,
                              "gini_w_g"=gini_w_g,
                              "ks_score_t_g"=ks_score_t_g,
                              "ks_score_w_g"=ks_score_w_g,
                              "psi_g"=psi_g,
                              "ks_test_g"=ks_g,
                              
                              "t_1_n"=t[1,1],
                              "t_1"=t[1,2],
                              "t_2_n"=t[2,1],
                              "t_2"=t[2,2],
                              "t_3_n"=t[3,1],
                              "t_3"=t[3,2],
                              "w_1_n"=w[1,1],
                              "w_1"=w[1,2],
                              "w_2_n"=w[2,1],
                              "w_2"=w[2,2],
                              "w_3_n"=w[3,1],
                              "w_3"=w[3,2]
                             ))


ocena_modeli<-rbind(ocena_modeli,temp_tab)
ocena_zmienne<-rbind(ocena_zmienne,zmienne_tab)

save(ocena_modeli,file="ocena_modeli.rdata")
save(ocena_zmienne,file="ocena_zmienne.rdata")
save(train,file="trainm")
save(test,file="testm")
