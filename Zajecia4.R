getwd()
setwd("D:/mks/zaj1")
setwd("C:\\Users\\Anna\\Desktop\\UW\\Modelowanie Ryzyka Kredytowego")
install.packages("leaps")
library(leaps) #subsetting selection
install.packages("VSURF")
library("VSURF")
install.packages("LogisticDx")
library(LogisticDx) # - gof()
# install.packages("pROC")
library(pROC) # - auc(), roc.test itp.
# install.packages("gtools")
library(gtools) # smartbind()

load(file="trainm")
load(file="testm")

#lista nazw zmiennych z _woe
kolsy<-colnames(train)[grep("woe", colnames(train))]
kolsy<-kolsy[-grep("def_woe", kolsy)]
data<-train[,c("def",kolsy)]

###########################################################

# ESTYMACJA MODELU


############ STEPWISE

tim <- proc.time ()[1]	## applying cor (standard R implementation)
model_stepwise_both<-step(baza, scope = list(upper=max, lower=baza ), direction = "both", trace=T,steps=30,k=4)
#czynnik karzacy k=4 we wzorze na aic-4*k/n
cat ("cor runtime [s]:", proc.time ()[1] - tim, "(n =", ncol (baza)-3, ")\n")
save(model_stepwise_both,file="model_stepwise_both.rdata")

summary(model_stepwise_both)

############ FORWARD
tim <- proc.time ()[1]	## applying cor (standard R implementation)
model_stepwise_for<-step(baza, scope = list(upper=model, lower=~1 ), direction = "forward", trace=T,steps=30,k=4)
cat ("cor runtime [s]:", proc.time ()[1] - tim, "(n =", ncol (baza)-3, ")\n")
save(model_stepwise_for,file="model_stepwise_for.rdata")

summary(model_stepwise_for)


########### BACKWARD
tim <- proc.time ()[1]	## applying cor (standard R implementation)
model_stepwise_b<-step(max,  direction = "backward", trace=T,steps=30,k=4)
cat ("cor runtime [s]:", proc.time ()[1] - tim, "(n =", ncol (baza)-3, ")\n")
save(model_stepwise_b,file="model_stepwise_bck.rdata")

summary(model_stepwise_b)

########## SUBSETING - Analiza mniej zautoamtyzowana
# analiza najlepszych modeli z okre?lon? liczb? zmiennych 
# nbest = liczba najlepszych modeli dla kazdej liczby zmiennych
# nvmax= malsymalana liczba zmiennych w modelu

tim <- proc.time ()[1]	## applying cor (standard R implementation)
model_nbest<-regsubsets(data[,2:12], data=data, y=data$def, nbest=1, nvmax=15)
cat ("cor runtime [s]:", proc.time ()[1] - tim, "(n =", ncol (baza)-3, ")\n")
save(model_nbest,file="model_nbest.rdata")
#zrob model dla 15 zmiennych,wybierz najlepszy,zrob modele dla 14 zmiennych, wybierz najlepszy oitd...
summary(model_nbest)$adjr2

plot(model_nbest, scale="adjr2")
#majlepszy model pomija amt2,amt6 i amt5
plot(model_nbest, scale="bic")
#najlepszy pomija amtwoe2 amt6 amt5

########## variable selection using Random Forrest

# Details
# 
# First step ("thresholding step"): first, nfor.thres random forests are computed using the function randomForest with arguments 
# importance=TRUE, and our choice of default values for ntree and mtry 
# (which are higher than default in randomForest to get a more stable variable importance measure). 
# Then variables are sorted according to their mean variable importance (VI), in decreasing order. 
# This order is kept all along the procedure. Next, a threshold is computed: min.thres, 
# the minimum predicted value of a pruned CART tree fitted to the curve of the standard deviations of VI. 
# Finally, the actual "thresholding step" is performed: only variables with a mean VI larger than nmin * min.thres are kept.
# 
# Second step ("intepretation step"): the variables selected by the first step are considered. 
# nfor.interp embedded random forests models are grown, starting with the random forest build with only the most important variable 
# and ending with all variables selected in the first step. Then, err.min the minimum mean out-of-bag (OOB) error of these models 
# and its associated standard deviation sd.min are computed. Finally, the smallest model 
# (and hence its corresponding variables) having a mean OOB error less than err.min + nsd * sd.min is selected.
# 
# Note that for this step (and the next one), the mtry parameter of randomForest is set to its default value (see randomForest) if nvm,
# the number of variables in the model, is not greater than the number of observations, while it is set to nvm/3 otherwise. 
# This is to ensure quality of OOB error estimations along embedded RF models.
# 
# Third step ("prediction step"): the starting point is the same than in the second step. 
# However, now the variables are added to the model in a stepwise manner. mean.jump, 
# the mean jump value is calculated using variables that have been left out by the second step, 
# and is set as the mean absolute difference between mean OOB errors of one model and its first following model. 
# Hence a variable is included in the model if the mean OOB error decrease is larger than nmj * mean.jump.
# 
# As for interpretation step, the mtry parameter of randomForest is set to its default value if nvm, 
# the number of variables in the model, is not greater than the number of observations, while it is set to nvm/3 otherwise.

    # model_vsurf <- VSURF(x =data[,2:16], y=data$def, data = data,na.action = na.omit, parallel=T, nmin=20)
    # save(model_vsurf,file="model_vsurf.rdata")
    
    # load(file="model_vsurf.rdata")
    # summary(model_vsurf)


  # VSURF computation time: 1.1 days 
  # 
  # VSURF selected: 
  # 15 variables at thresholding step (in 23.2 hours)
  # 7 variables at interpretation step (in 3.4 hours)
  # 2 variables at prediction step (in 11.2 mins)
  # 
  # VSURF ran in parallel on a PSOCK cluster and used 3 cores
  
    # model_vsurf$varselect.thres
    # model_vsurf$varselect.interp
    # colnames(data)[model_vsurf$varselect.interp]
    # 
    # model_vsurf$mean.per
  
# estymacja modelu tylko ze sta?? - do por?wna?
baza<-glm(def ~ 1,data=data, family=binomial("logit"))

max<-glm(def ~ .,data=data, family=binomial("logit"))
summary(max)



mdl<-"model_stepwise_both"
model<-model_stepwise_both


################################################################################

### Analiza jako?ci dopasowania


#test na jako?? dopasowania modelu - zupelnie podstawowy
# za?ozenie por?wnujemy uzyskany model z modele "idealnym" i sprawdzamy, czy uzyskana wartosc max wirogodno?ci jest statystycznie bliska 0
# H0: model jest dobrze dopasowany do danych
gf<-pchisq(model$deviance, model$df.residual,lower.tail = F)
# wniosek?

# test LR na istotono?? zmiennych
# sprawdzamy czy maxW dla modelu jest istotnie wi?ksze ni? dla modelu tylko ze sta?a - test na laczna istotnosc modelu
# H0  zmienne s? ?acznie nieistotne
ist<-pchisq(model$null.deviance-model$deviance, model$df.null-model$df.residual,lower.tail = F)
# wniosek?

# test Hosmera - Lemeshowa - podstawowy test na jakosc dopasowania w modelach dla binarnej zmiennej zaleznej
# H0: model jest dobrze dopasowany do danych
# ma wiele wad - przede wszystkim jest bardzo wra?liwy na liczbe przedzia??w

hr<-hosmerlem(y=data$def, yhat=fitted(model),g=10)
hosmerlem(y=data$def, yhat=fitted(model),g=7)
hosmerlem(y=data$def, yhat=fitted(model),g=8)
hosmerlem(y=data$def, yhat=fitted(model),g=9)
#hr$p.value

#Inne testy dopasowania
# generalnie powinny by? interpretowane ?aczeni, ka?dy z nich analizuje nieco inn? specyfik? dopasowania
# je?eli jeden to OR

gof<-gof(model, g=10)

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
roc_test_og<-roc.test(data$def, data$max, data$model,method="d")$p.value
# wniosek?



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




zmienne<-names(model$coefficients)[2:length(model$coefficients)]

# ocena_zmienne<-NULL
# ocena_modeli<-NULL
# zmienne_tab<-NULL


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




temp_tab<-as.data.frame(cbind("Model"=mdl,
                              
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
