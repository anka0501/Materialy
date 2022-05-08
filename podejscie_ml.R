
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
install.packages("moments")
install.packages("corrplot")
install.packages("parallelMap")
install.packages("woeBinning")
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
#https://onlinecourses.science.psu.edu/stat857/node/216
#http://www.rpubs.com/kuhnrl30/CreditScreen
#http://rpubs.com/arifulmondal/216381
#http://srisai85.github.io/GermanCredit/German.html
maxNR()
library(zoo)
library(maxLik)
parallelStartSocket(cpus = detectCores())
set.seed(1)

Main.DF = read.csv('german.data.txt', sep = " ", header = FALSE)

#colnames are missing

colnames(Main.DF) <- c("account_status","months","credit_history","purpose","credit_amount","savings","employment","installment_rate",
                       "personal_status","guarantors","residence","property","age","other_installments","housing","credit_cards","job","dependents",
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
Main.DF$defaulted = factor(Main.DF$defaulted, levels = c(1,2), labels = c("Nondefault", "Default"))


# ile NA?
colSums(is.na(Main.DF))
# 0, super

###################################### zmiana zawartosci factorow #################################################################

#troche pozmienialem opis niektorych poziomow, bo byl za dlugi, przy zachowaniu sensu

Main.DF$account_status = recode_factor(Main.DF$account_status, 'A11'='<0', 'A12'='0-200','A13'='>=200', 'A14'='no checking account')

Main.DF$credit_history = recode_factor(Main.DF$credit_history, 'A30'='no credits/all paid',
                                       'A31'='all credits paid',
                                       'A32'='existing credits/paid till now',
                                       'A33' = 'delay in the past',
                                       'A34'='critical account')

Main.DF$purpose = recode_factor(Main.DF$purpose, 'A40'='car (new)' , 'A41'='car (used)', 'A42' ='equipment', 'A43' = 'media',
                                'A44'='domestic appliances', 'A45'='repairs', 'A46' ='education', 'A47'='	(vacation - does not exist?)', 'A48'='retraining',
                                'A49' = 'business', 'A410' ='others')
#lewostronnie domkniete

Main.DF$savings = recode_factor(Main.DF$savings, "A61"= "<100", "A62" = "100-500", "A63"="500-1000", "A64"= ">1000", "A65" ="unknown/no savings account")

Main.DF$employment = recode_factor(Main.DF$employment, "A71" = 'unemployed', "A72" = "<1 year", "A73" = "1-4 years", "A74" = "4-7 years", "A75"=">= 7 years")

# procent dochodu rozporzadzalnego
Main.DF$installment_rate = recode_factor(Main.DF$installment_rate, '1' = '10%', '2' = '20%', '3' = '30%', '4'= '40%')


# mozna z tego zrobic dwie zmienne, personal status i sex osobno
Main.DF$personal_status = recode_factor(Main.DF$personal_status, 'A91' = 'male - divorced/separated', 'A92' ='female - divorced/separated/married', 
                                        'A93' = 'male - single', 'A94' = 'male - married/widowed', 'A95' = 'female - single')

Main.DF$guarantors = recode_factor(Main.DF$guarantors, 'A101' = 'none', 'A102'='co-applicant', 'A103'='guarantor' )

Main.DF$property = recode_factor(Main.DF$property, 'A121' = 'real estate', 'A122' = 'life insurance',
                                 'A123' = 'car or other',
                                 'A124' = 'unknown/no property')

Main.DF$other_installments = recode_factor(Main.DF$other_installments, 'A141' = 'bank', 'A142' = 'stores', 'A143' ='none')

Main.DF$housing = recode_factor(Main.DF$housing, 'A151' = 'rent' , 'A152' = 'own', 'A153' ='for free')

Main.DF$job = recode_factor(Main.DF$job, 'A171' ='unemployed/unskilled - non-resident', 'A172'='unskilled - resident', 'A173'='skilled employee / official',
                            'A174' = 'management/self-employed')

Main.DF$phone = recode_factor(Main.DF$phone, 'A191' ='none', 'A192' ='yes, registered')

Main.DF$foreign_worker = recode_factor(Main.DF$foreign_worker, 'A201' ='yes', 'A202' = 'no')

###################################################################################################################################


###################################### EDA #####################################################################################


#1. G/B flag - defaulted

temp.DF = as.data.frame(table(Main.DF$defaulted))
colnames(temp.DF) = c("Loan Status", "Frequency")                     

ggplot(temp.DF, aes(x = `Loan Status`, y = Frequency)) + geom_bar(stat = "identity",fill = "steelblue", color = "black") + theme_minimal(base_size = 18) +
  geom_text(aes(label=Frequency), vjust = 1.5, size=14)

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

plot_cont(Main.DF, "months")

desc_statby(temp.DF, "months", c("defaulted"))[,1:9]

#pozyczki zdefaultowane znacznie wieksza srednia i mediana

#credit_amount

plot_cont(Main.DF, "credit_amount")

desc_statby(temp.DF, "credit_amount", c("defaulted"))[,1:9]

# defaultowane pozyczki zaciagane na srednio znacznie wieksza kwote
# rozklad nondefaultow zdecydowanie leptokurtyczny

#age

plot_cont(Main.DF, "age")

ggarrange(p1,p2)

desc_statby(temp.DF, "age", c("defaulted"))[,1:9]

#rozklady raczej podobne, statystycznie mniejszy wiek wsrod nondefaultow

#4. nowe zmienne  ciagle

# Installment = credit_amount / months

Main.DF = Main.DF %>% mutate(Installment = credit_amount/months)

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

woe_table = woe_deploy(paste(xcol), 0.05,0,0)
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

woe_table = woe_deploy(paste(xcol), 0.05,0,0)
print(woe_table)
IV_Summary.DF[4,2] = max(woe_table$IV)

woe_plot1 = woe_plot(woe_table, paste(xcol))

woe_plot1[[1]]
woe_plot1[[2]]
woe_plot1[[3]]

#credit_amount

xcol = "credit_amount"

woe_table = woe_deploy(paste(xcol), 0.12,0.05,0)
print(woe_table)
IV_Summary.DF[5,2] = max(woe_table$IV)

woe_plot1 = woe_plot(woe_table, paste(xcol))

woe_plot1[[1]]
woe_plot1[[2]]
woe_plot1[[3]]

#savings

xcol = "savings"

woe_table = woe_deploy(paste(xcol), 0.05,0,0)
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
Main.DF$personal_status = factor(Main.DF$personal_status)
xcol = "personal_status"

woe_table = woe_deploy(paste(xcol), 0.05,0,0)
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

woe_table = woe_deploy(paste(xcol), 0.15,0,0)
print(woe_table)
IV_Summary.DF[13,2] = max(woe_table$IV)

woe_plot1 = woe_plot(woe_table, paste(xcol))

woe_plot1[[1]]
woe_plot1[[2]]
woe_plot1[[3]]

#other_installments

xcol = "other_installments"

woe_table = woe_deploy(paste(xcol), 0.05,0,0)
print(woe_table)
IV_Summary.DF[14,2] = max(woe_table$IV)

woe_plot1 = woe_plot(woe_table, paste(xcol))

woe_plot1[[1]]
woe_plot1[[2]]
woe_plot1[[3]]

#housing

xcol = "housing"

woe_table = woe_deploy(paste(xcol), 0.05,0,0)
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

woe_table = woe_deploy(paste(xcol), 0.05,0,0.1)
print(woe_table)
IV_Summary.DF[21,2] = max(woe_table$IV)

woe_plot1 = woe_plot(woe_table, paste(xcol))

woe_plot1[[1]]
woe_plot1[[2]]
woe_plot1[[3]]

#Income

xcol = "Income"

woe_table = woe_deploy(paste(xcol), 0.09,0.05,0.1)
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

Model.DF = Model.DF[c("defaulted",IV_Summary.DF$Variable %>% as.character())]
Model.DF$defaulted = as.numeric(Model.DF$defaulted) - 1

#0 Nondefault
#1 Default


################################################################################################################################

########################POdejscie Machine Learinigowe########################


myControl <- trainControl(
  method = "repeatedcv",
  number=10,
  repeats=5,
  classProbs = TRUE,
  allowParallel = TRUE,
  savePredictions="final"
)

##### XGBoosting

## hyperparamter tuning za pomoca optymalizacji bayesowskiej
# install.packages("rBayesianOptimization")
# install.packages("xgboost")
require(rBayesianOptimization)
require(xgboost)

#nie odpalac jak nie trzeba, dlugo szuka

train.label = Model.DF.train$defaulted
new_tr <- model.matrix(~.+0,data = Model.DF.train[,-which(colnames(Model.DF.train) == "defaulted")])

dtrain <- xgb.DMatrix(new_tr,
                      label = train.label)


cv_folds <- KFold(train.label, nfolds = 10,
                  stratified = TRUE, seed = 2017)

# xgb_cv_bayes <- function(max.depth, min_child_weight, subsample, eta, gamma, colsample_bytree, nrounds) {
#   cv <- xgb.cv(params = list(booster = "gbtree",
#                              eta = eta,
#                              max_depth = max.depth,
#                              min_child_weight = min_child_weight,
#                              subsample = subsample, colsample_bytree = colsample_bytree,
#                              gamma = gamma,
#                              lambda = 1, alpha = 0,
#                              objective = "binary:logistic",
#                              eval_metric = "auc"),
#                data = dtrain, nround = 100,
#                folds = cv_folds, prediction = TRUE, showsd = TRUE,
#                early_stopping_rounds = 25, maximize = TRUE, verbose = 0)
#   list(Score = cv$evaluation_log[, max(test_auc_mean)],
#        Pred = cv$pred)
# }
# 
# OPT_Res <- BayesianOptimization(xgb_cv_bayes,bounds = list(max.depth = c(3:12),
#                                                            min_child_weight = c(0.5, 1, 2,5),
#                                                            subsample = c(0.5, 0.8),
#                                                            eta = c(0.01,0.1,0.3),
#                                                            gamma = c(0.1,1, 2,5),
#                                                            colsample_bytree = c(0.4, 0.7, 1.0)
# ),
# init_grid_dt = NULL, init_points = 10, n_iter = 40,
# acq = "ucb", kappa = 2.576, eps = 0.0,
# verbose = TRUE)


# wartosci z optymalizacji bayesowskiej

# nrounds = 100
# max.depth = 3
# min_child_weight = 0.7542
# subsample = 0.8
# eta = 0.1
# gamma = 0.1
# colsample_bytree = 0.4475


# grid search dla najblizszego sasiedztwa
# nie odpalac jak nie trzeba


# xgb.grid <- expand.grid(eta = c(0.08, 0.1,0.12),
#                         max_depth = c(2,3,4),
#                         gamma = c(0.05,1,0.15), 
#                         colsample_bytree = c(0.3, 0.4475, 0.6), 
#                         min_child_weight = c(0.65,0.7542,0.9),
#                         subsample = c(0.5,0.8,1)
# )
# 
# xgb.grid$acc = mapply(function(eta, max_depth, gamma, colsample_bytree,
#                                min_child_weight, subsample){
#   set.seed(2017)
#   Accuracy.Vector = c()
#   
#   #Perform 10 fold cross validation
#   for(i in 1:10){
#     
#     
#     
#     #Segement your data by fold using the which() function 
#     testIndexes <- cv_folds[[i]]
#     
#     testData <- Model.DF.train[testIndexes, ]
#     trainData <- Model.DF.train[-testIndexes, ]
#     
#     labels <- trainData$defaulted 
#     ts_label <- testData$defaulted 
#     
#     new_tr <- model.matrix(~.+0,data = trainData[,-which(colnames(trainData) == "defaulted")])
#     new_ts <- model.matrix(~.+0,data = testData[,-which(colnames(testData) == "defaulted")])
#     
#     dtrain <- xgb.DMatrix(data = new_tr,label = labels)
#     dtest <- xgb.DMatrix(data = new_ts,label=ts_label)
#     
#     params <- list(
#       booster = "gbtree",
#       objective = "binary:logistic",
#       eta = eta,
#       gamma = gamma,
#       max_depth = max_depth,
#       min_child_weight = min_child_weight,
#       colsample_bytree = colsample_bytree,
#       subsample = subsample
#     )
#     
#     xgb.fold.model = xgb.train(data = dtrain, 
#                                nrounds = 100,
#                                params = params,
#                                #early_stopping_rounds = 50,
#                                watchlist = list(val=dtest,train=dtrain),
#                                verbose = FALSE)
#     
#     x = cbind(data = ifelse(predict(xgb.fold.model, dtest) >= 0.5, 1,0), ref = ts_label) %>% as.data.frame()
#     x = x %>% mutate(accurate = ifelse(data == ref,1,0))
#     xgb.fold.acc = sum(x$accurate/nrow(x))
#     
#     Accuracy.Vector = c(Accuracy.Vector,xgb.fold.acc )
#     
#   }
#   paste0("Accuracy ", Accuracy.Vector %>% mean() %>% round(4)," ", 
#          paste(100, eta, max_depth, gamma, colsample_bytree,
#                min_child_weight, subsample)) %>% print()
#   return(Accuracy.Vector %>% mean())
#   
# },
# xgb.grid[,1],xgb.grid[,2],xgb.grid[,3],xgb.grid[,4],
# xgb.grid[,5], xgb.grid[,6])


# trenowanie wlasciwego algorytmu
Model.DF.train$defaulted = recode_factor(Model.DF.train$defaulted, '1' = 'Defaulted', '0' = 'Nondefaulted')


xgb.grid <- expand.grid(nrounds = 100,
                        eta = 0.08,
                        max_depth = 3,
                        gamma = 1,
                        colsample_bytree = 0.6,
                        min_child_weight =0.65,
                        subsample = 0.8
)

#100 0.08 3 1 0.6 0.65 0.8"

model_xgb = caret::train(
  form = defaulted ~ .,
  data = Model.DF.train,
  metric = "Accuracy",
  method = "xgbTree",
  tuneGrid  = xgb.grid,
  nthread = 4,
  trControl = myControl
)

Temp.DF = Model.DF.train$defaulted %>% as.data.frame()
Temp.DF$model = predict(model_xgb, newdata = Model.DF.train, type = "prob")

Pred.DF = Model.DF.test$defaulted %>% as.data.frame()
Pred.DF$model_pred = predict(model_xgb, newdata = Model.DF.test, type = "prob")



acc.perf = ROCR::performance(prediction(Pred.DF$model_pred$Defaulted,Pred.DF$.),measure = "acc")
plot(acc.perf, col = "red", main = "Accuracy as function of cutoff")
ind = which.max( slot(acc.perf, "y.values")[[1]] )
acc = slot(acc.perf, "y.values")[[1]][ind]
cutoff = slot(acc.perf, "x.values")[[1]][ind]
print("Model XGBoost")
print(c(accuracy= acc, cutoff = cutoff))

Pred.DF$raw_predictions = ifelse(Pred.DF$model_pred$Defaulted >= cutoff, 1, 0) %>% as.factor()

confusionMatrix(Pred.DF$., Pred.DF$raw_predictions)



## krzywa ROC
model_xgb_roc = ROCR::performance(prediction(Pred.DF$model_pred$Defaulted,Pred.DF$.),"tpr","fpr")

plot(model_xgb_roc, lwd = 5, col = "blue",  main="XGBoost ROC")
abline(0, 1, col= "red", add = TRUE)
legend("topleft", legend = c("XGBoost") , pch = 15, bty = 'n', col = c("blue"))

KS <- function(pred,depvar){
  p   <- ROCR::prediction(as.numeric(pred),depvar)
  perf <- ROCR::performance(p, "tpr", "fpr")
  ks <- max(attr(perf, "y.values")[[1]] - (attr(perf, "x.values")[[1]])) %>% round(4)
  return(ks)
}

AUC <- function(pred,depvar){
  p   <- ROCR::prediction(as.numeric(pred),depvar)
  auc <- ROCR::performance(p,"auc")
  auc <- unlist(slot(auc, "y.values")) %>% round(4)
  return(auc)
}

paste("KS for train set:", KS(Temp.DF$model$Nondefaulted, Temp.DF$.),
      "KS for test set:",  KS(Pred.DF$model_pred$Defaulted, Pred.DF$.),
      "Gini for train set:", (2*AUC(Temp.DF$model$Nondefaulted, Temp.DF$.) - 1),
      "Gini for test set:", (2*AUC(Pred.DF$model_pred$Defaulted, Pred.DF$.) - 1))


### KSPLOT


Pred.DF$model_pred %>% gather(key = "Type", value = "Probability") -> KSD
minMax <- seq(min(Pred.DF$model_pred$Nondefaulted, Pred.DF$model_pred$Defaulted), 
              max(Pred.DF$model_pred$Nondefaulted, Pred.DF$model_pred$Defaulted), 
              length.out=length(Pred.DF$model_pred$Nondefaulted))

cdf1 <- ecdf(Pred.DF$model_pred$Nondefaulted)
cdf2 <- ecdf(Pred.DF$model_pred$Defaulted)

x0 <- minMax[which( abs(cdf1(minMax) - cdf2(minMax)) == max(abs(cdf1(minMax) - cdf2(minMax))) )] 

y0 <- cdf1(x0) 
y1 <- cdf2(x0) 

ggplot(KSD, aes(x = Probability, group = Type, color = Type)) + 
  stat_ecdf(size=1) + 
  theme_minimal() +  
  theme(legend.position ="top") + 
  xlab("Sample") +
  ylab("ECDF") +
  geom_segment(aes(x = x0[1], y = y0[1], xend = x0[1], yend = y1[1]),
               linetype = "dashed", color = "red", lwd = 2) +
  geom_point(aes(x = x0[1] , y= y0[1]), color="red", size=5) +
  geom_point(aes(x = x0[1] , y= y1[1]), color="red", size=5) +
  ggtitle("K-S Plot: XGBoost")

