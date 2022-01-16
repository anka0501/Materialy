#install.packages("forcats")
library(forcats)

model<-model_stepwise_both
# Przygotowanie karty scoringowej i mapowanie na klasy ratingowe

coeff<-as.data.frame(model$coefficients)
coeff$Zmienna<-rownames(coeff)
names(coeff)[1]<-"Parametr"
names(coeff)[2]<-"Zmienna"

# ustawienie parametrow do skalowania scora
score_points<-660
log_odds<-1/72
ptd<-40#liczba punktow do zdublowania ryzyka

#przypisanie PD do scora modelowego - ostatecznie wybrany model
train$fpd<-model$fitted.values
#prawdopodbienstwa defaultu uzusyakne dla zbioru treningowego
train$score_mod<-(score_points-ptd/log(1/2)*log(log_odds))+ptd/log(1/2)*model$linear.predictors

test$fpd<-predict(model, newdata=test, type="response") 
test$score_mod<-(score_points-ptd/log(1/2)*log(log_odds))+ptd/log(1/2)*predict(model, newdata=test, type="link") 

#histogramy rozk?adu dobrych i zlych klient?w
hist(train[train$def==0,c("score_mod")],col="green")
hist(train[train$def==1,c("score_mod")],col="red",add=T)

# Przygotownaie karty scoringowej
zmienne<-names(model$coefficients)[-grep("Intercept",names(model$coefficients))]
zmienne_k<-c(1:(length(names(model$coefficients))-1))
names(zmienne_k)<-gsub("_woe","_coarse",zmienne)
zm_k<-colnames(train)[which(colnames(train) %in% (names(zmienne_k)))]
zmienne_k<-names(zmienne_k)
zmienne_k<-zmienne_k[order(zmienne_k)]
zmienne<-zmienne[order(zmienne)]
zmienne_c<-cbind(zmienne_k,zmienne)

podzial<-data.frame(Zmienna=character(),Zmienna_c=character(),Groups=character(),WoE=character(),NR=numeric())


for (i in 1:(length(zmienne))) {
  
  # liczebnosc bin?w
  tempa<-cbind(table(train[,zmienne_c[i]],useNA="always"))
  # zastapienie missingow na pozotale
  rownames(tempa)<-fct_explicit_na(rownames(tempa),na_level="OTHER_P")
  tempa<-as.data.frame(tempa)
  tempa$Groups<-rownames(tempa)
  colnames(tempa)[1]<-"NR"
  
  #doklejenie WoE
  tempb<-as.data.frame(cbind(table(train[,zmienne[i]])))
  tempb$WoE<-rownames(tempb)
  colnames(tempb)[1]<-"NR"
  podz<-merge(tempa, tempb,by="NR")
  podz<-cbind(zmienne[i],zmienne_c[i],podz)
  
  colnames(podz)[1]<-"Zmienna"
  colnames(podz)[2]<-"Zmienna_c"
  
  #doklejenie dla kazdej ze zmiennych 
  podzial<-rbind(podzial,podz)
  print(i)
}

podzial$Zmienna_org<-gsub("_woe","",podzial$Zmienna,ignore.case=T)

#Utworzenie karty scoringowej
karta<-merge(x=coeff,y=podzial,by="Zmienna",all.x=T)
#wyb?r zmiennych
karta<-karta[,c("Zmienna_org","Zmienna_c","Zmienna","Groups","WoE","Parametr")]
karta[,5]<-as.numeric(karta[,5])

# Wyliczenie punkt?w scoringowych
karta$points<-round(((log(1/exp(karta[,6]*karta[,5]+karta[1,6]))+log(log_odds)+score_points/ptd*log(2))/(log(2)/ptd))-((log(1/exp(karta[1,6]))+log(log_odds)+score_points/ptd*log(2))/(log(2)/ptd)),digit=4)
karta$points[1]<-round((log(1/exp(karta[1,6]))+log(log_odds)+score_points/ptd*log(2))/(log(2)/ptd),digit=4) 

#zapisanie karty do pliku csv.
write.csv(karta,file="C:\\Users\\Anna\\Desktop\\UW\\Modelowanie Ryzyka Kredytowego\\karta.csv")


# Kalibracja
model_calib<-glm(def	~ score_mod,data=train, family=binomial("logit"))

train$int_calib<-model_calib$coefficients[1]
train$slope_calib<-model_calib$coefficients[2]
train$ln_odds_calib<-train$int_calib+train$slope_calib*train$score_mod
train$score_calib<-(score_points-ptd/log(1/2)*log(log_odds))+ptd/log(1/2)*train$ln_odds_calib
train$prob_calib<-exp(train$ln_odds_calib)/(exp(train$ln_odds_calib)+1)


#install.packages("sqldf")
library(sqldf)
train<-sqldf("select *
                ,	case when	prob_calib	IS		NULL	then		NULL		
                when	prob_calib	>=	0.0000	and	prob_calib	<	0.002	then	1
                when	prob_calib	>=	0.002	and	prob_calib	<	0.003	then	2
                when	prob_calib	>=	0.003	and	prob_calib	<	0.004	then	3
                when	prob_calib	>=	0.004	and	prob_calib	<	0.006	then	4
                when	prob_calib	>=	0.006	and	prob_calib	<	0.009	then	5
                when	prob_calib	>=	0.009	and	prob_calib	<	0.015	then	6
                when	prob_calib	>=	0.015	and	prob_calib	<	0.022	then	7
                when	prob_calib	>=	0.022	and	prob_calib	<	0.033	then	8
                when	prob_calib	>=	0.033	and	prob_calib	<	0.049	then	9
                when	prob_calib	>=	0.049	and	prob_calib	<	0.074	then	10
                when	prob_calib	>=	0.074	and	prob_calib	<	0.11	then	11
                when	prob_calib	>=	0.11	and	prob_calib	<	0.17	then	12
                when	prob_calib	>=	0.17	and	prob_calib	<	0.25	then	13
                when	prob_calib	>=	0.25	and	prob_calib	<	0.5	then	14
                when	prob_calib	>=	0.5			then	15
                else NULL	end as	KL_RAT							
                from train")

train<-sqldf("select *
                ,	case when	KL_RAT	IS		NULL	then		NULL		
                when	KL_RAT	in (1,2,3,4)			then		'A'		
                when	KL_RAT	in (5,6,7,8)			then		'B'		
                when	KL_RAT	in (9,10,11)			then		'c'	
                when	KL_RAT	in (12)			then		'D'		
                when	KL_RAT	in (13)			then		'E'		
                when	KL_RAT	in (14,15)			then	'F'		
                else NULL	end as	GR_RYZ							
                
                from train") 
colnames(train)<-c("installment_plan1_coarse", "housing1_coarse", "employment_length1_coarse",
                   "property1_coarse", "savings_balance1_coarse", "purpose1_coarse",
                   "checking_balance1_coarse", "credit_history1_coarse", "age_coarse",
                   "amount_coarse", "months_loan_duration_coarse", "months_loan_duration",
                   "credit_history1", "checking_balance1","purpose1",
                   "amount", "age","property1", "employment_length1",
                   "housing1","installment_plan1", "savings_balance1",
                   "default", "default_woe", "months_loan_duration_woe", 
                   "amount_woe", "age_woe" , "credit_history1_woe" ,
                   "checking_balance1_woe", "purpose1_woe", "savings_balance1_woe",
                   "property1_woe", "employment_length1_woe", "housing1_woe", "installment_plan1_woe",
                   "model", "score", "fpd", "score_mod", "int_calib", "slope_calib", "ln_odds_calib",
                  "prob_calib", "KL_RAT" , "GR_RYZ")  
                  


write.table(train, "colnames_train.csv",  row.names = FALSE ,col.names = TRUE, sep = ",")

#install.packages("utiml")
library(utiml)
br(prop.table(table(train$GR_RYZ,train$def),1))
