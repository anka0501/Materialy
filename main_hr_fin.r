## USER DEFINED FUNCTIONS ##

plot_factor = function(data,xcol,ycol = "defaulted"){
  
  
  temp.DF = data[,c(xcol,ycol)] %>% na.omit()
  temp.DF = aggregate(temp.DF[,2], list(temp.DF[,1], temp.DF[,2]), FUN = length)
  colnames(temp.DF) = c(xcol,ycol, "Freq")
  
  levels(temp.DF[,1]) <- gsub("/", "\n", levels(temp.DF[,1]))
  
  plot = ggplot(temp.DF, aes(x = temp.DF[,1], y = temp.DF[,3], fill = as.factor(temp.DF[,2]))) +
    geom_bar(stat = "identity" , color = "black") +
    theme_minimal(base_size = 18) +
    scale_fill_discrete(name="Loan Status", labels = c("Default", "Nondefault")) +
    labs(x = xcol, y = 'Freq') + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 12))
  
  return(plot)
  
}
plot_cont = function(data,xcol,ycol = "defaulted", xlim = 0){
  
  temp.DF = data[,c(xcol,ycol)]
  colnames(temp.DF) = c(xcol,ycol)
  
  if(xlim == 0){
    ggplot(temp.DF, aes( x = temp.DF[,1])) + geom_density(fill = '#99d6ff', alpha=0.4) +
      facet_grid(defaulted ~ .) +
      scale_x_continuous(breaks=pretty_breaks(n = 10)) +
      scale_y_continuous( breaks=pretty_breaks(n = 5)) + 
      theme_minimal(base_size = 18) +
      labs(x = xcol) -> p1
    
    p1 = p1 + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    ggplot(temp.DF, aes( y = temp.DF[,1], x = as.factor(defaulted))) + geom_violin(fill = '#99d6ff', alpha=0.4) +
      stat_boxplot(geom ='errorbar') +
      scale_y_continuous( breaks=pretty_breaks(n = 10)) + 
      theme_minimal(base_size = 18) + 
      labs(x = paste(ycol), y = paste(xcol)) -> p2
    
    plot = ggarrange(p1,p2)
  }else{
    
    ggplot(temp.DF, aes( x = temp.DF[,1])) + geom_density(fill = '#99d6ff', alpha=0.4) +
      facet_grid(defaulted ~ .) +
      scale_x_continuous(limits = c(0, xlim), breaks=pretty_breaks(n = 10)) +
      scale_y_continuous( breaks=pretty_breaks(n = 5)) + 
      theme_minimal(base_size = 18) +
      labs(x = xcol) -> p1
    
    p1 = p1 + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    ggplot(temp.DF, aes( y = temp.DF[,1], x = as.factor(defaulted))) + geom_violin(fill = '#99d6ff', alpha=0.4) +
      stat_boxplot(geom ='errorbar') +
      scale_y_continuous(limits = c(0, xlim), breaks=pretty_breaks(n = 10)) + 
      theme_minimal(base_size = 18) + 
      labs(x = paste(ycol), y = paste(xcol)) -> p2
    
    plot = ggarrange(p1,p2)
    
    
  }
  
  return(plot)
  
}

make_bin_grid = function(xcol){
  
  binning_grid = expand.grid(min.perc.total = seq(0.05,0.2,0.01), min.perc.class = seq(0,0.2,0.05), stop.limit = seq(0,0.5,0.1))
  
  temp.DF = Main.DF[,c(xcol,"defaulted")]
  
  #temp.DF$defaulted = as.numeric(temp.DF$defaulted) - 1
  
  binning_grid$IV =  mapply(function(x, y,z) {
    binning = woe.binning(temp.DF, 'defaulted', xcol,
                          min.perc.total=x, min.perc.class=y,
                          stop.limit=z, event.class='Default')
    
    binning[[3]]
    
  }, binning_grid[,1], binning_grid[,2], binning_grid[,3])
  
  return(binning_grid)
  
}

woe_deploy = function(xcol, x,y,z){
  
  temp.DF = Main.DF[,c(xcol,"defaulted")]
  
  binning <- woe.binning(temp.DF, 'defaulted', xcol,
                         min.perc.total=x, min.perc.class=y,
                         stop.limit=z, event.class='Default')
  
  temp.DF.woe <- woe.binning.deploy(temp.DF, binning, add.woe.or.dum.var = 'woe')
  Model.DF[,ncol(Model.DF) + 1] <<- temp.DF.woe[,4]
  colnames(Model.DF)[ncol(Model.DF)] <<- xcol
  
  woe_table = woe.binning.table(binning) %>% as.data.frame()
  colnames(woe_table) = c("Final Bin", "Total Count", "Total Distr", "Accepted", "Rejected", "Accepted Distr", "Rejected Distr", "Rejected Rate", "WOE", "IV")
  
  woe_table$`Final Bin` = substr(woe_table$`Final Bin`,0,10)
  
  return(woe_table)
  
}


woe_deploy = function(xcol, x,y,z){
  
  temp.DF = Main.DF[,c(xcol,"defaulted")]
  
  binning <- woe.binning(temp.DF, 'defaulted', xcol,
                         min.perc.total=x, min.perc.class=y,
                         stop.limit=z, event.class='Default')
  
  temp.DF.woe <- woe.binning.deploy(temp.DF, binning, add.woe.or.dum.var = 'woe')
  Model.DF[,ncol(Model.DF) + 1] <<- temp.DF.woe[,4]
  colnames(Model.DF)[ncol(Model.DF)] <<- xcol
  
  woe_table = woe.binning.table(binning) %>% as.data.frame()
  colnames(woe_table) = c("Final Bin", "Total Count", "Total Distr", "Accepted", "Rejected", "Accepted Distr", "Rejected Distr", "Rejected Rate", "WOE", "IV")
  
  woe_table$`Final Bin` = substr(woe_table$`Final Bin`,0,10)
  
  return(woe_table)
  
}

woe_plot = function(woe_table, xcol ){
  
  temp_table = woe_table[1:nrow(woe_table)-1,]
  
  ggplot(temp_table, aes(x = `Final Bin`, y = as.numeric(WOE))) + 
    geom_col(fill = '#99d6ff', color = "black") + theme_minimal(base_size = 10) +
    scale_x_discrete(limits =  temp_table[,1]) +
    labs(y = "WOE", title = paste(xcol, "WOE")) + 
    theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, hjust = 1, size = 12)) -> p1
  
  ggplot(temp_table, aes(x = `Final Bin`,y = `Total Distr`)) + 
    geom_bar(stat = "identity", fill = '#99d6ff', color = "black") +
    theme_minimal(base_size = 10) +
    labs(title = paste(xcol, " Percentage of Cases")) + 
    scale_x_discrete(limits =  temp_table[,1]) +
    theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, hjust = 1, size = 12)) -> p2
  
  ggplot(temp_table, aes(x = `Final Bin`,y = `Rejected Rate`)) + 
    geom_bar(stat = "identity", fill = '#99d6ff', color = "black") +
    theme_minimal(base_size = 10) +
    labs(title = paste(xcol, " Bad Rate %")) + 
    scale_x_discrete(limits =  temp_table[,1]) +
    theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, hjust = 1, size = 12))  -> p3
  
  plot = list(p1,p2,p3)
  
  
}

## LIBRARIES
library(parallel)
library(parallelMap)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(scales)
library(moments)
library(corrplot)
library(parallelMap)
library(woeBinning)
library(caret)
library(LogisticDx) 
library(pROC)
library(gtools) 
library(lmtest)
library(ROCR)
library(tidyr)
library(OptimalCutpoints)
library(forcats)
#https://onlinecourses.science.psu.edu/stat857/node/216
#http://www.rpubs.com/kuhnrl30/CreditScreen
#http://rpubs.com/arifulmondal/216381
#http://srisai85.github.io/GermanCredit/German.html

parallelStartSocket(cpus = detectCores())
set.seed(1)

Main.DF = read.csv('german.data.txt', sep = " ", header = FALSE)

#colnames are missing

colnames(Main.DF) <- c("account_status","months","credit_history","purpose",
                       "credit_amount","savings","employment","installment_rate",
                       "personal_status","guarantors","residence","property","age",
                       "other_installments","housing","credit_cards","job","dependents",
                       "phone","foreign_worker","defaulted")

glimpse(Main.DF)

# numeryczne kolumny w tabeli
colnames(Main.DF)[sapply(Main.DF, is.numeric)]

sapply(sapply(Main.DF, unique), length)

# months ma 33 poziomy, ok
# credit_amount 921, ok
# installment_rate ma 4 poziomy, warto zamienic na factor
# age 53, ok
# credit_cards 4 poziomy -> factor,
# dependents i defaulted takze factor

Main.DF$installment_rate = as.factor(Main.DF$installment_rate)
Main.DF$credit_cards = as.factor(Main.DF$credit_cards)
Main.DF$residence = as.factor(Main.DF$residence)
Main.DF$dependents = as.factor(Main.DF$dependents)
Main.DF$defaulted = factor(Main.DF$defaulted, levels = c(1,2), 
                           labels = c("Nondefault", "Default"))


# ile NA?
colSums(is.na(Main.DF))
# 0, super

###################################### zmiana zawartosci factorow #################################################################

#troche pozmienialem opis niektorych poziomow, bo byl za dlugi, przy zachowaniu sensu

Main.DF$account_status = recode_factor(Main.DF$account_status, 
                                       'A11'='<0', 'A12'='0-200','A13'='>=200', 
                                       'A14'='no checking account')

Main.DF$credit_history = recode_factor(Main.DF$credit_history, 
                                       'A30'='no credits/all paid',
                                       'A31'='all credits paid',
                                       'A32'='existing credits/paid till now',
                                       'A33' = 'delay in the past',
                                       'A34'='critical account')

Main.DF$purpose = recode_factor(Main.DF$purpose, 
                                'A40'='car (new)' , 'A41'='car (used)', 
                                'A42' ='equipment', 'A43' = 'media',
                                'A44'='domestic appliances', 'A45'='repairs', 
                                'A46' ='education', 'A47'='	(vacation - does not exist?)', 
                                'A48'='retraining','A49' = 'business', 
                                'A410' ='others')
#lewostronnie domkniete

Main.DF$savings = recode_factor(Main.DF$savings, 
                                "A61"= "<100", "A62" = "100-500", "A63"="500-1000", 
                                "A64"= ">1000", "A65" ="unknown/no savings account")

Main.DF$employment = recode_factor(Main.DF$employment, 
                                   "A71" = 'unemployed', "A72" = "<1 year", 
                                   "A73" = "1-4 years", "A74" = "4-7 years", 
                                   "A75"=">= 7 years")

# procent dochodu rozporzadzalnego
Main.DF$installment_rate = recode_factor(Main.DF$installment_rate, 
                                         '1' = '10%', '2' = '20%', 
                                         '3' = '30%', '4'= '40%')


# mozna z tego zrobic dwie zmienne, personal status i sex osobno
Main.DF$personal_status = recode_factor(Main.DF$personal_status,
                                        'A91' = 'male - divorced/separated', 
                                        'A92' ='female - divorced/separated/married', 
                                        'A93' = 'male - single', 
                                        'A94' = 'male - married/widowed',
                                        'A95' = 'female - single')

Main.DF$guarantors = recode_factor(Main.DF$guarantors, 
                                   'A101' = 'none', 'A102'='co-applicant', 'A103'='guarantor' )

Main.DF$property = recode_factor(Main.DF$property, 
                                 'A121' = 'real estate', 'A122' = 'life insurance',
                                 'A123' = 'car or other','A124' = 'unknown/no property')

Main.DF$other_installments = recode_factor(Main.DF$other_installments, 
                                           'A141' = 'bank', 'A142' = 'stores', 'A143' ='none')

Main.DF$housing = recode_factor(Main.DF$housing, 
                                'A151' = 'rent' , 'A152' = 'own', 'A153' ='for free')

Main.DF$job = recode_factor(Main.DF$job,
                            'A171' ='unemployed/unskilled - non-resident', 
                            'A172'='unskilled - resident', 'A173'='skilled employee / official',
                            'A174' = 'management/self-employed')

Main.DF$phone = recode_factor(Main.DF$phone, 
                              'A191' ='none', 
                              'A192' ='yes, registered')

Main.DF$foreign_worker = recode_factor(Main.DF$foreign_worker, 
                                       'A201' ='yes', 
                                       'A202' = 'no')

###################################################################################################################################


###################################### EDA #####################################################################################


#1. G/B flag - defaulted

temp.DF = as.data.frame(table(Main.DF$defaulted))
colnames(temp.DF) = c("Loan Status", "Frequency")                     

wykrest_do_MD<-ggplot(temp.DF, aes(x = `Loan Status`, y = Frequency)) + geom_bar(stat = "identity",fill = "steelblue", color = "black") + theme_minimal(base_size = 18) +
  geom_text(aes(label=Frequency), vjust = 1.5, size=14)
plot(wykrest_do_MD)
# ok probka jest troche niezbalansowana, 30% zdarzen to sa defaulty
# mozna sie zastanowic nad robieniem balansowania, ja lubie SMOTE 

#2. factory

plot_factor(Main.DF, "account_status")

lapply(colnames(Main.DF)[sapply(Main.DF, is.factor)], function(i) plot_factor(Main.DF, xcol = i))

#sprawdzam zaleznosc factorow od zmiennej default, za pomoca chi kwadrat

Factor_summary.DF = data.frame(Variable = character(), Test_statistic = numeric(), p_value = numeric(), cramers_v = numeric(), Dependency = character())

factor_cols = (Main.DF[, sapply(Main.DF, is.factor)] %>% names())[-18]


for(i in factor_cols){
  temp.DF = Main.DF[,c(i,"defaulted")] %>% na.omit()
  test_value = chisq.test(temp.DF[,1], temp.DF[,2], correct = F)
  
  
  k = temp.DF[,1] %>% unique() %>% length()
  r = temp.DF[,2] %>% unique %>% length()
  
  
  cramers_v = sqrt(test_value$statistic/(nrow(temp.DF)*min(k-1, r-1)))
  
  cbind("Variable" = i, "Test_statistic" = test_value$statistic %>% round(4), p_value = test_value$p.value %>% round(4), "cramers_v" = cramers_v %>% round(4),
        Dependency = ifelse(test_value$p.value >= 0.05, "Independent", "Dependent")) %>% as.data.frame() -> result.DF
  
  Factor_summary.DF = rbind(Factor_summary.DF, result.DF)
}

rownames(Factor_summary.DF) <- NULL

print(Factor_summary.DF)

#wnioski?
# installment_rate, residence, credit_cards, job, dependents, phone wychodza niezalezne od defaultu

#3. zmienne ciagle

#Months

#plot_cont(Main.DF, "months")

#desc_statby(temp.DF, "months", c("defaulted"))[,1:9]

#pozyczki zdefaultowane znacznie wieksza srednia i mediana

#credit_amount

#plot_cont(Main.DF, "credit_amount")

#desc_statby(temp.DF, "credit_amount", c("defaulted"))[,1:9]

# defaultowane pozyczki zaciagane na srednio znacznie wieksza kwote
# rozklad nondefaultow zdecydowanie leptokurtyczny

#age

#plot_cont(Main.DF, "age")

#ggarrange(p1,p2)

#desc_statby(temp.DF, "age", c("defaulted"))[,1:9]

#rozklady raczej podobne, statystycznie mniejszy wiek wsrod nondefaultow

#4. nowe zmienne  ciagle

# Installment = credit_amount / months

Main.DF = Main.DF %>% mutate(Installment = credit_amount/months)
xcol="Installment"
ggplot(Main.DF, aes( x = Installment)) + geom_density(fill = '#99d6ff', alpha=0.4) +
  facet_grid(defaulted ~ .) +
  scale_x_continuous(limits = c(0,800),breaks=pretty_breaks(n = 10)) +
  scale_y_continuous( breaks=pretty_breaks(n = 5)) + 
  theme_minimal(base_size = 18) +
  labs(x = xcol) 

desc_statby(Main.DF, "Installment", c("defaulted"))[,1:9]

skewness(Main.DF %>% filter(defaulted == "Default") %>% select(Installment))
skewness(Main.DF %>% filter(defaulted == "Nondefault") %>% select(Installment))

#srednia wieksza w defaultach, ale mediana w nondefault
#wieksza skosnosc defaultow, tez maksymalny installment wiekszy 2x w defaultach niz nondefaultach

# skoro mamy wielkosc installmentu, a installment_rate pokazuje stosunek installmentu do rozporzadzalnego dochodu,
# to mozna wyliczyc rozporzadzalny dochod

Main.DF = Main.DF %>% mutate(Income = Installment/((gsub("%", "", Main.DF$installment_rate) %>% as.numeric())/100))

plot_cont(Main.DF, "Income", xlim = 2000)
desc_statby(Main.DF, "Income", c("defaulted"))[,1:9]

#dochod wsrod defaultow cechuje sie 2x wiekszym odchyleniem standardowym niz wsrod nondefaultow

#AmtTI = credit_amount/Income

# +1 dlatego,ze liczba osob na utrzymaniu + pozyczkobiorca

Main.DF = Main.DF %>% mutate(AmtTI = credit_amount/Income)
plot_cont(Main.DF, "AmtTI")
desc_statby(Main.DF, "AmtTI", c("defaulted"))[,1:9]

#wsrod defaultow wieksza srednia i mediana, sklonnosc do bardziej obciazajacych pozyczek?

#sex
Main.DF = Main.DF %>% mutate(sex = ifelse(grepl("female", Main.DF$personal_status) == TRUE, "female","male"))

#personal_status juz bez plci
Main.DF$personal_status = gsub("male - |female - ","",Main.DF$personal_status)


#macierz wspolczynnika korelacji kendalla dla zmiennych ciaglych

M = Main.DF[colnames(Main.DF)[sapply(Main.DF, is.numeric)]]
M_cor = cor(M, method = "kendall")

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(M_cor, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         diag=FALSE 
)

#silna korelacja Installment i Income oraz Month i AmtTI
#jezeli po dyskretyzacji nadal bedzie silna, trzeba zastanowic sie, ktora zmienna wyrzucic


###################################################################################################################################

###################################### WOE #####################################################################################


Model.DF = Main.DF %>% select(defaulted)

Variable = names(Main.DF)

IV = NA

IV_Summary.DF = data.frame(Variable, IV )

IV_Summary.DF = IV_Summary.DF[-21,] #bez defaulted

# Account_Status

make_bin_grid("account_status")

woe_table = woe_deploy("account_status", 0.05,0,0)
print(woe_table)
IV_Summary.DF[1,2] = max(woe_table$IV)

woe_plot1 = woe_plot(woe_table, "account_status")

woe_plot1[[1]]
woe_plot1[[2]]
woe_plot1[[3]]

#months 
xcol = "months"

woe_table = woe_deploy(paste(xcol), 0.05,0.11,0)
print(woe_table)
IV_Summary.DF[2,2] = max(woe_table$IV)

woe_plot1 = woe_plot(woe_table, paste(xcol))

woe_plot1[[1]]
woe_plot1[[2]]
woe_plot1[[3]]

#credit_history

xcol = "credit_history"

woe_table = woe_deploy(paste(xcol), 0.05,0,0)
print(woe_table)
IV_Summary.DF[3,2] = max(woe_table$IV)

woe_plot1 = woe_plot(woe_table, paste(xcol))

woe_plot1[[1]]
woe_plot1[[2]]
woe_plot1[[3]]

#purpose

xcol = "purpose"

woe_table = woe_deploy(paste(xcol), 0.05,0.2,0)
print(woe_table)
IV_Summary.DF[4,2] = max(woe_table$IV)

woe_plot1 = woe_plot(woe_table, paste(xcol))

woe_plot1[[1]]
woe_plot1[[2]]
woe_plot1[[3]]

#credit_amount

xcol = "credit_amount"

woe_table = woe_deploy(paste(xcol), 0.12,0.13,0)
print(woe_table)
IV_Summary.DF[5,2] = max(woe_table$IV)

woe_plot1 = woe_plot(woe_table, paste(xcol))

woe_plot1[[1]]
woe_plot1[[2]]
woe_plot1[[3]]

#savings

xcol = "savings"

woe_table = woe_deploy(paste(xcol), 0.05,0.11,0)
print(woe_table)
IV_Summary.DF[6,2] = max(woe_table$IV)

woe_plot1 = woe_plot(woe_table, paste(xcol))

woe_plot1[[1]]
woe_plot1[[2]]
woe_plot1[[3]]

#employment

xcol = "employment"

woe_table = woe_deploy(paste(xcol), 0.05,0,0)
print(woe_table)
IV_Summary.DF[7,2] = max(woe_table$IV)

woe_plot1 = woe_plot(woe_table, paste(xcol))

woe_plot1[[1]]
woe_plot1[[2]]
woe_plot1[[3]]

#installment_rate

xcol = "installment_rate"

woe_table = woe_deploy(paste(xcol), 0.05,0,0)
print(woe_table)
IV_Summary.DF[8,2] = max(woe_table$IV)

woe_plot1 = woe_plot(woe_table, paste(xcol))

woe_plot1[[1]]
woe_plot1[[2]]
woe_plot1[[3]]

#personal_status

xcol = "personal_status"

woe_table = woe_deploy("personal_status", 0.05,0,0)
print(woe_table)
IV_Summary.DF[9,2] = max(woe_table$IV)

woe_plot1 = woe_plot(woe_table, paste(xcol))

woe_plot1[[1]]
woe_plot1[[2]]
woe_plot1[[3]]

#guarantors

xcol = "guarantors"

woe_table = woe_deploy(paste(xcol), 0.05,0,0)
print(woe_table)
IV_Summary.DF[10,2] = max(woe_table$IV)

woe_plot1 = woe_plot(woe_table, paste(xcol))

woe_plot1[[1]]
woe_plot1[[2]]
woe_plot1[[3]]

#residence

xcol = "residence"

woe_table = woe_deploy(paste(xcol), 0.15,0,0)
print(woe_table)
IV_Summary.DF[11,2] = max(woe_table$IV)

woe_plot1 = woe_plot(woe_table, paste(xcol))

woe_plot1[[1]]
woe_plot1[[2]]
woe_plot1[[3]]

#property

xcol = "property"

woe_table = woe_deploy(paste(xcol), 0.05,0,0)
print(woe_table)
IV_Summary.DF[12,2] = max(woe_table$IV)

woe_plot1 = woe_plot(woe_table, paste(xcol))

woe_plot1[[1]]
woe_plot1[[2]]
woe_plot1[[3]]

#age

xcol = "age"

woe_table = woe_deploy(paste(xcol), 0.15,0.15,0)
print(woe_table)
IV_Summary.DF[13,2] = max(woe_table$IV)

woe_plot1 = woe_plot(woe_table, paste(xcol))

woe_plot1[[1]]
woe_plot1[[2]]
woe_plot1[[3]]

#other_installments

xcol = "other_installments"

woe_table = woe_deploy(paste(xcol), 0.05,0.15,0)
print(woe_table)
IV_Summary.DF[14,2] = max(woe_table$IV)

woe_plot1 = woe_plot(woe_table, paste(xcol))

woe_plot1[[1]]
woe_plot1[[2]]
woe_plot1[[3]]

#housing

xcol = "housing"

woe_table = woe_deploy(paste(xcol), 0.05,0.2,0)
print(woe_table)
IV_Summary.DF[15,2] = max(woe_table$IV)

woe_plot1 = woe_plot(woe_table, paste(xcol))

woe_plot1[[1]]
woe_plot1[[2]]
woe_plot1[[3]]

#credit_cards

xcol = "credit_cards"

woe_table = woe_deploy(paste(xcol), 0.05,0,0)
print(woe_table)
IV_Summary.DF[16,2] = max(woe_table$IV)

woe_plot1 = woe_plot(woe_table, paste(xcol))

woe_plot1[[1]]
woe_plot1[[2]]
woe_plot1[[3]]

#job

xcol = "job"

woe_table = woe_deploy(paste(xcol), 0.15,0,0)
print(woe_table)
IV_Summary.DF[17,2] = max(woe_table$IV)

woe_plot1 = woe_plot(woe_table, paste(xcol))

woe_plot1[[1]]
woe_plot1[[2]]
woe_plot1[[3]]

#dependents

xcol = "dependents"

woe_table = woe_deploy(paste(xcol), 0.05,0,0)
print(woe_table)
IV_Summary.DF[18,2] = max(woe_table$IV)

woe_plot1 = woe_plot(woe_table, paste(xcol))

woe_plot1[[1]]
woe_plot1[[2]]
woe_plot1[[3]]

#phone

xcol = "phone"

woe_table = woe_deploy(paste(xcol), 0.05,0,0)
print(woe_table)
IV_Summary.DF[19,2] = max(woe_table$IV)

woe_plot1 = woe_plot(woe_table, paste(xcol))

woe_plot1[[1]]
woe_plot1[[2]]
woe_plot1[[3]]

#foreign_worker

xcol = "foreign_worker"

woe_table = woe_deploy(paste(xcol), 0.05,0,0)
print(woe_table)
IV_Summary.DF[20,2] = max(woe_table$IV)

woe_plot1 = woe_plot(woe_table, paste(xcol))

woe_plot1[[1]]
woe_plot1[[2]]
woe_plot1[[3]]

#installment

xcol = "Installment"

woe_table = woe_deploy(paste(xcol), 0.05,0.2,0.1)
print(woe_table)
IV_Summary.DF[21,2] = max(woe_table$IV)

woe_plot1 = woe_plot(woe_table, paste(xcol))

woe_plot1[[1]]
woe_plot1[[2]]
woe_plot1[[3]]

#Income

xcol = "Income"

woe_table = woe_deploy(paste(xcol), 0.09,0.2,0.1)
print(woe_table)
IV_Summary.DF[22,2] = max(woe_table$IV)

woe_plot1 = woe_plot(woe_table, paste(xcol))

woe_plot1[[1]]
woe_plot1[[2]]
woe_plot1[[3]]

#AmtTI

xcol = "AmtTI"

woe_table = woe_deploy(paste(xcol), 0.09,0,0.1)
print(woe_table)
IV_Summary.DF[23,2] = max(woe_table$IV)

woe_plot1 = woe_plot(woe_table, paste(xcol))

woe_plot1[[1]]
woe_plot1[[2]]
woe_plot1[[3]]

#sex
Main.DF$sex = factor(Main.DF$sex)

xcol = "sex"

woe_table = woe_deploy(paste(xcol), 0.05,0,0)
print(woe_table)
IV_Summary.DF[24,2] = max(woe_table$IV)

woe_plot1 = woe_plot(woe_table, paste(xcol))

woe_plot1[[1]]
woe_plot1[[2]]
woe_plot1[[3]]


print(IV_Summary.DF %>% arrange(desc(IV)))

# wylaczamy zmienne o IV < 0.025

IV_Summary.DF = IV_Summary.DF %>% filter(IV >= 0.025)
print(IV_Summary.DF %>% arrange(desc(IV)))

#Model.DF = Model.DF[c("defaulted",IV_Summary.DF$Variable %>% as.character())]

Model.DF$defaulted = as.numeric(Model.DF$defaulted)-1
#0 Nondefault
#1 Default


################################################################################################################################
#Fine - classing | Binaryzacja  
#Coarse - classing | Grupowanie
#Fine - tuning | Wygładzanie przedziałów

##################################################################################################################################

#Na podstawie obserwacji zależności pomiędzy zminnymi objaśniającymy wybieramy w jaki sposób
#bedzie traktować zmienną wyrażoną za pomocą WOE
# zmienne<-colnames(Model.DF)
# 
# zmienne_woe_ciagle <- c(" employment",
#                          "guarantors")
# 
# zmienne_woe_kat    <- c( "months", 
#                          "credit_history",
#                          "purpose",
#                          "credit_amount",
#                          "savings",
#                          "installment_rate",
#                          "account_status",
#                          "personal_status",
#                          "property",
#                          "other_installments",
#                          "housing",
#                          "Income",
#                          "foreign_worker",
#                          "job",
#                          "dependents",
#                          "AmtTI",
#                          "sex",
#                          "age")

# colnames(Model.DF)
# Model.DF <- Model.DF %>% mutate(
#     months             = as.factor(months),
#     #credit_history     = as.factor(credit_history),
#     purpose            = as.factor(purpose),
#     credit_amount      = as.factor(credit_amount),
#     #savings            = as.factor(savings),
#     Installment        = as.factor(Installment),
#     installment_rate   = as.factor(installment_rate),
#     #account_status     = as.factor(account_status),
#     #personal_status    = as.factor(personal_status),
#     property           = as.factor(property),
#     #other_installments = as.factor(other_installments),
#     #housing            = as.factor(housing),
#     #AmtTI              = as.factor(AmtTI),
#     sex                = as.factor(sex),
#     age                = as.factor(age),
#     Income             = as.factor(Income),
#     #foreign_worker     = as.factor(foreign_worker)
# )

#################################################In Sample/ Out of Sample######################################################################


#Losujemy próbę trenującą i próbę testową ze strayfikacją względem zmiennej objaśnianej, 


inTrain <- createDataPartition(y = Model.DF$defaulted,
                                     p = .7,
                                     list = FALSE)

Model.DF.train <- Model.DF[ inTrain,]
Model.DF.test  <- Model.DF[-inTrain,]
nrow(Model.DF.train)
describe(Model.DF.train)
describe(Model.DF.test)

prop.table(table(Model.DF.train$defaulted))
prop.table(table(Model.DF.test$defaulted))

prop.table(table(Model.DF.train$account_status))
prop.table(table(Model.DF.test$account_status))

prop.table(table(Model.DF.train$credit_history))
prop.table(table(Model.DF.test$credit_history))

prop.table(table(Model.DF.train$months))
prop.table(table(Model.DF.test$months))

prop.table(table(Model.DF.train$age))
prop.table(table(Model.DF.test$age))


#################################################Estymacja modelu######################################################################

#FUNKCJE POTRZEBNE DO PRZEPROWADZENIA OCENY JAKOŚCI KARTY (CHLEBUS)

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

# estymacja modelu tylko ze stałą - do porównań
baza<-glm(defaulted ~ 1,data=Model.DF.train, family=binomial("logit"))
summary(baza)


# (exp(baza$coefficients[1]))
# (mean(Model.DF.train$defaulted)/(1-(mean(Model.DF.train$defaulted))))
#=> pstwo defaultu w probie testowej wynosi > 29%
a<-as.data.frame(colnames(Model.DF.train))

max<-glm(defaulted ~ .
        ,data=Model.DF.train, 
        family=binomial("logit"))
max$coefficients
summary(max)



########### BACKWARD
tim <- proc.time ()[1]	## applying cor (standard R implementation)
model_stepwise_b<-step(max,  direction = "backward", trace=F,steps=2000,k=2)
cat ("cor runtime [s]:", proc.time ()[1] - tim, "(n =", ncol (baza)-3, ")\n")
save(model_stepwise_b,file="model_stepwise_bck.rdata")

summary(model_stepwise_b)

restricted_1 <-glm(defaulted ~ account_status +	
                             credit_history  + 
                             purpose +	
                             savings  + 
                             employment +
                             guarantors + 
                             residence +
                             property +
                             age +
                             other_installments +
                             housing + 
                          # credit_cards +
                             foreign_worker +
                             Income + 
                             AmtTI +
                             personal_status
                      ,data=Model.DF.train, 
                      family=binomial("logit"))

summary(restricted_1)
lrtest(model_stepwise_b,restricted_1) #=> usuwamy credit cards


restricted_2 <-glm(defaulted ~ account_status +	
                     credit_history  + 
                     purpose +	
                     savings  + 
                     employment +
                     guarantors + 
                     # residence +
                     property +
                     age +
                     other_installments +
                     housing + 
                     # credit_cards +
                     foreign_worker +
                     Income + 
                     AmtTI +
                     personal_status
                   ,data=Model.DF.train, 
                   family=binomial("logit"))

summary(restricted_2)
lrtest(model_stepwise_b,restricted_2) # => usuwamy credit cards i residence

restricted_3 <-glm(defaulted ~ account_status +	
                     credit_history  + 
                     purpose +	
                     savings  + 
                     employment +
                     guarantors + 
                     # residence +
                     #property +
                     age +
                     other_installments +
                     housing + 
                     # credit_cards +
                     foreign_worker +
                     Income + 
                     AmtTI +
                     personal_status
                   ,data=Model.DF.train, 
                   family=binomial("logit"))

summary(restricted_3)
lrtest(model_stepwise_b,restricted_3) # => sa podstawy do odrzucenia h_0 ze property=0, credit cards i residence

#final model
logistic_regression<-restricted_2

#ANALIZA DOBROCI MODELU

#test na jakość dopasowania modelu - zupelnie podstawowy
# załozenie porównujemy uzyskany model z modele "idealnym" i sprawdzamy, czy uzyskana wartosc max wirogodności jest statystycznie bliska 0
# H0: model jest dobrze dopasowany do danych
gf<-pchisq(logistic_regression$deviance, logistic_regression$df.residual,lower.tail = F)
gf

# test LR na istotoność zmiennych
# sprawdzamy czy maxW dla modelu jest istotnie większe niż dla modelu tylko ze stała - test na laczna istotnosc modelu
# H0  zmienne są łacznie nieistotne
ist<-pchisq(logistic_regression$null.deviance-logistic_regression$deviance, logistic_regression$df.null-logistic_regression$df.residual,lower.tail = F)
ist
# wniosek?

# test Hosmera - Lemeshowa - podstawowy test na jakosc dopasowania w modelach dla binarnej zmiennej zaleznej
# H0: model jest dobrze dopasowany do danych
# ma wiele wad - przede wszystkim jest bardzo wrażliwy na liczbe przedziałów

hr<-hosmerlem(y=Model.DF.train$defaulted, yhat=fitted(logistic_regression),g=10)
hosmerlem(y=Model.DF.train$defaulted, yhat=fitted(logistic_regression),g=9)
hosmerlem(y=Model.DF.train$defaulted, yhat=fitted(logistic_regression),g=8)
hosmerlem(y=Model.DF.train$defaulted, yhat=fitted(logistic_regression),g=7)
hosmerlem(y=Model.DF.train$defaulted, yhat=fitted(logistic_regression),g=6)
#Inne testy dopasowania
# generalnie powinny być interpretowane łaczeni, każdy z nich analizuje nieco inną specyfikę dopasowania
# jeżeli jeden to OR

gof<-gof(logistic_regression, g=10)

# https://cran.r-project.org/web/packages/gof/gof.pdf
# HL <- Hosmer-Lemeshow test
# mHL <- modified Hosmer-Lemeshow test
# OsRo <- Osius - Rojek of the link function test
# 
# S Stukel's tests:
#   SstPgeq0.5	 score test for addition of vector z1
#   SstPl0.5	   score test for addition of vector z2
#   SstBoth	     score test for addition of vector z1 and z2
#   SllPgeq0.5	 log-likelihood test for addition of vector z1
#   SllPl0.5	   log-likelihood test for addition of vector z2
#   SllBoth	     log-likelihood test for addition of vectors z1 and z2


#przypisamie PD do zbiorów 
# fitted.values - PD
# linear.predictors - ln(p/(1-p))


#WYBRANA SKALA
# przeskalowanie wartosci dopasowana na wybrana skale
# 660 punktów oznacza ODDS = 72 a ODDS się dubuluje co 40 punktów


# Przypisanie PD i SCORE do zbioru testowego i treningowego całego
Model.DF.test$model<-predict(logistic_regression, newdata=Model.DF.test, type="response") 
Model.DF.test$score<-(660-40/log(1/2)*log(1/72))+40/log(1/2)*predict(logistic_regression, newdata=Model.DF.test, type="link") 
Model.DF.train$model<-predict(logistic_regression, newdata=Model.DF.train, type="response") 
Model.DF.train$score<-(660-40/log(1/2)*log(1/72))+40/log(1/2)*predict(logistic_regression, newdata=Model.DF.train, type="link") 

#test roc - sprawdza czy krzywa ROC jest istotnie lepsza dla dwóch modeli
#H0 krzywe ROC są równie dobre

Model.DF.train$baza <- predict(baza, newdata=Model.DF.train, type="response")
Model.DF.train$max  <- predict(max,  newdata=Model.DF.train, type="response")

Model.DF.test$baza <- predict(baza, newdata=Model.DF.test, type="response")
Model.DF.test$max  <- predict(max,  newdata=Model.DF.test, type="response")

roc_test_baza<-roc.test(Model.DF.train$defaulted, 
                        Model.DF.train$model, 
                        Model.DF.train$baza,method="d")$p.value

roc_test_og  <-roc.test(Model.DF.train$defaulted, 
                        Model.DF.train$max , 
                        Model.DF.train$model,
                        method="d")$p.value
# wniosek? Odrzucamy

#Accuracy rate
# Model.DF.test.acc $defaulted
# Model.DF.test.acc $model_t_f
Model.DF.test.acc <- Model.DF.test  %>% select(model,defaulted) %>% 
  mutate(model_t_f = 1*(model > .5) + 0)
                                    
Model.DF.test.acc <- Model.DF.test.acc %>% mutate(accurate = 1*(model_t_f == defaulted))

sum(Model.DF.test.acc$accurate)/NROW(Model.DF.test.acc)


###STATYSTYKI GINIEGO###

# współczynnik gini 
# im wyższy tym rozkłady scorów u złych i dobrych klientów się od siebie różnią
gini_t<-2*auc(Model.DF.train$defaulted,Model.DF.train$model,direction="<")-1
gini_w<-2*auc(Model.DF.test$defaulted,Model.DF.test$model,direction="<")-1
# a model max?
2*auc(Model.DF.test$defaulted,Model.DF.test$max,direction="<")-1



# policzenie przedziałów ufności dla gini_t
# method "delong" - analiztyczna postac; "bootstrap" szacowanie za pomcą bootstrap
ci_delong_t<-2*ci.auc(Model.DF.train$defaulted, Model.DF.train$model,method="d",direction="<")-1
# 0.5205047 0.5366548 0.5528049
ci_delong_w<-2*ci.auc(Model.DF.test$defaulted, Model.DF.test$model,method="d",direction="<")-1

#bardzo szerokie przedziały ufności dla ginieg -to wynika z małej próby zaledwie 700 obs.

# bootstrap in sample
tim <- proc.time ()[1]	## applying cor (standard R implementation)
ci_bootstrap_t<-2*ci.auc(Model.DF.train$defaulted, Model.DF.train$model,method="b",boot.n=500)-1
cat ("cor runtime [s]:", proc.time ()[1] - tim)

# bootstrap out of sample
tim <- proc.time ()[1]	## applying cor (standard R implementation)
ci_bootstrap_w<-2*ci.auc(Model.DF.test$defaulted, Model.DF.test$model,method="b",boot.n=500)-1
cat ("cor runtime [s]:", proc.time ()[1] - tim)


#statystyka K-S
# statystyka testu Kolmogorova - Smirnova na podobieństwo dwóch rozkładów
# porównywany są rozkłądy scorów u dobrych i u złych klientów,
# im bardziej się od siebie różnią tym lepiej
ks_score_t<-ks.test(Model.DF.train[Model.DF.train$defaulted==0,"score"],Model.DF.train[Model.DF.train$defaulted==1,"score"])$statistic
ks_score_w<-ks.test(Model.DF.test[Model.DF.test$defaulted==0,"score"]  ,Model.DF.test[Model.DF.test$defaulted==1,"score"])$statistic

########################################################################################
# stabilnosc modelu

#PSI - sprawdza na ile dwa rozkłady różnią się od siebie - to jest IV tylko tym razem chcemy by było małe
psi<-cal_psi(data1=Model.DF.train, data2=Model.DF.test, bench="score",target="score",bin=20)

# test Kolmogorowa Sminrnova na podobienstwo dwóch rozkladow 
# porównywane są rozkłady scora w próbie trenującej i testującej
# H0 dwa analizowane rozkłady są statystycznie nierozróżnialne

ks<-ks.test(Model.DF.train$score,Model.DF.test$score)$p.value

#najczestsze scory - jeżeli powtarza się często jeden score, to małe zmiany generują ryzyko zmiany oceny jakości modelu
t<-as.data.frame(sort(table(Model.DF.train$score)/length(Model.DF.train$score),decreasing=T))[1:3,1:2]
w<-as.data.frame(sort(table(Model.DF.test$score)/length(Model.DF.test$score),decreasing=T))[1:3,1:2]

######################################################################################




mdl<-"model_og"
zmienne<-names(logistic_regression$coefficients)[2:length(logistic_regression$coefficients)]

ocena_zmienne<-NULL
ocena_modeli<-NULL
zmienne_tab<-NULL


for (i in 1:length(zmienne)) {
  tab<-NULL 
  tab$model<-mdl
  tab$v<-zmienne[i]
  tab$gini_t<-2*ci.auc(Model.DF.train[!is.na(Model.DF.train[,zmienne[i]]),c("defaulted")],
                       Model.DF.train[!is.na(Model.DF.train[,zmienne[i]]),zmienne[i]],direction=">",method="d")[2]-1
  
  tab$gini_w<-2*ci.auc(Model.DF.test[!is.na(Model.DF.test[,zmienne[i]]),c("defaulted")], 
                       Model.DF.test[!is.na(Model.DF.test[,zmienne[i]]),zmienne[i]],direction=">",method="d")[2]-1
  
  tab$psi<-cal_psi_zm(data1=Model.DF.train[!is.na(Model.DF.train[,zmienne[i]]),zmienne], 
                      data2=Model.DF.test[!is.na(Model.DF.test[,zmienne[i]]),zmienne], 
                      bench=zmienne[i],target=zmienne[i])
  
  tab<-as.data.frame(tab)
  zmienne_tab<-rbind(zmienne_tab, tab)
}




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
                              #"gini_t_g"=gini_t_g,
                              #"gini_w_g"=gini_w_g,
                              #"ks_score_t_g"=ks_score_t_g,
                              #"ks_score_w_g"=ks_score_w_g,
                              #"psi_g"=psi_g,
                              #"ks_test_g"=ks_g,
                              
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
save(Model.DF.train,file="trainm")
save(Model.DF.test,file="testm")

############################## Przygotowanie karty scoringowej i mapowanie na klasy ratingowe ##############################
# Przygotowanie karty scoringowej i mapowanie na klasy ratingowe

coeff<-as.data.frame(logistic_regression$coefficients)
coeff$Zmienna<-rownames(coeff)
names(coeff)[1]<-"Parametr"
names(coeff)[2]<-"Zmienna"

# ustawienie parametrow do skalowania scora
score_points<-660
log_odds<-1/72
ptd<-40

#przypisanie PD o scora modelowego - ostatecznie wybrany model
Model.DF.train$fpd<-logistic_regression$fitted.values
Model.DF.train$score_mod<-(score_points-ptd/log(1/2)*log(log_odds))+ptd/log(1/2)*logistic_regression$linear.predictors

Model.DF.test$fpd<-predict(logistic_regression, newdata=Model.DF.test, type="response") 
Model.DF.test$score_mod<-(score_points-ptd/log(1/2)*log(log_odds))+ptd/log(1/2)*predict(logistic_regression, newdata=Model.DF.test, type="link") 

#histogramy rozk?adu dobrych i zlych klient?w
hist(Model.DF.train[Model.DF.train$def==0,c("score_mod")],col="green")
hist(Model.DF.train[Model.DF.train$def==1,c("score_mod")],col="red",add=T)

# Przygotownaie karty scoringowej
zmienne<-names(logistic_regression$coefficients)[-grep("Intercept",names(logistic_regression$coefficients))]
zmienne_k<-c(1:(length(names(logistic_regression$coefficients))-1))
names(zmienne_k)<-gsub("_woe","_coarse",zmienne)
zm_k<-colnames(Model.DF.train)[which(colnames(Model.DF.train) %in% (names(zmienne_k)))]
zmienne_k<-names(zmienne_k)
zmienne_k<-zmienne_k[order(zmienne_k)]
zmienne<-zmienne[order(zmienne)]
zmienne_c<-cbind(zmienne_k,zmienne)

podzial<-data.frame(Zmienna=character(),Zmienna_c=character(),Groups=character(),WoE=character(),NR=numeric())


for (i in 1:(length(zmienne))) {
  
  # liczebnosc bin?w
  tempa<-cbind(table(Model.DF.train[,zmienne_c[i]],useNA="always"))
  # zastapienie missingow na pozotale
  rownames(tempa)<-fct_explicit_na(rownames(tempa),na_level="OTHER_P")
  tempa<-as.data.frame(tempa)
  tempa$Groups<-rownames(tempa)
  colnames(tempa)[1]<-"NR"
  
  #doklejenie WoE
  tempb<-as.data.frame(cbind(table(Model.DF.train[,zmienne[i]])))
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
write.csv(karta,file="karta_scoringowa.csv")


# Kalibracja
model_calib<-glm(defaulted	~ score_mod,data=Model.DF.train, family=binomial("logit"))

Model.DF.train$int_calib<-model_calib$coefficients[1]
Model.DF.train$slope_calib<-model_calib$coefficients[2]
Model.DF.train$ln_odds_calib<-Model.DF.train$int_calib+Model.DF.train$slope_calib*Model.DF.train$score_mod
Model.DF.train$score_calib<-(score_points-ptd/log(1/2)*log(log_odds))+ptd/log(1/2)*Model.DF.train$ln_odds_calib
Model.DF.train$prob_calib<-exp(Model.DF.train$ln_odds_calib)/(exp(Model.DF.train$ln_odds_calib)+1)
hist(Model.DF.train$prob_calib,breaks=20)
library(sqldf)
train<-Model.DF.train
train<-sqldf("select 
                    a.*,
                    case when	prob_calib	IS		NULL	then		NULL		
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
                   from train a")

Model.DF.train<-sqldf("select 
                              a.*,	
                              case when	KL_RAT	IS		NULL	then		NULL		
                                 when	KL_RAT	in (1,2,3,4)			then		'A'		
                                 when	KL_RAT	in (5,6,7,8)			then		'B'		
                                 when	KL_RAT	in (9,10,11)			then		'c'	
                                 when	KL_RAT	in (12)			then		'D'		
                                 when	KL_RAT	in (13)			then		'E'		
                                 when	KL_RAT	in (14,15)			then	'F'		
                                 else NULL	end as	GR_RYZ							
                        from train a") 


#############################################Wybór Cut-Offu (dla danego score-a)#############################################


options(scipen=999)

# cost ratio = (FP-TN)/(FN-TP)
cr<-(-10-0)/(-50-10)

table(Model.DF.train$defaulted)

cutpoint1 <- optimal.cutpoints(X = score~defaulted, 
                               tag.healthy = 0, 
                               methods = c("Youden", "MaxSpSe","SpEqualSe","ROC01","MaxEfficiency","CB","MinPvalue"), 
                               data = Model.DF.train,
                               direction=">", 
                               control = control.cutpoints(costs.ratio=cr), 
                               ci.fit = TRUE, 
                               trace=T)

summary(cutpoint1)

mean(Model.DF.train[,"defaulted"])

# CRITERION: Youden
#default rate
mean(Model.DF.train[Model.DF.train$score>=466.2755540,"defaulted"])
#wpuszczamy
length(Model.DF.train$score[Model.DF.train$score>=466.2755540])/length(Model.DF.train$score)

# CRITERION: CB
#default rate:)
mean(Model.DF.train[Model.DF.train$score>=596.9828603,"defaulted"])

#wpuszczamy
length(Model.DF.train$score[Model.DF.train$score>=596.9828603])/length(Model.DF.train$score)


#nieszczególnie w skurpulatny sposób zdefiniowanie kryterium cost benefit wskazuje na ustawienie cut off na 596.98
#co jest ultra konsrewatywnym podejsciem nie dopuszczjacym żadnego defaultu do próby
#Bardziej realistyczne do wykorzystania jest krterium Youden wpuszczające do portela potencjalnie 39 defaultow


