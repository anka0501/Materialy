############################################################
#                       Text mining
############################################################

#install.packages("quanteda")
library(quanteda)

#install.packages("tm")  # for text mining
# install.packages("SnowballC") # for text stemming
# install.packages("wordcloud") # word-cloud generator 
# install.packages("RColorBrewer") # color palettes
# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

#text <- read.csv("C:/Users/Anna/Desktop/Mgr/2017Q3/Text.txt")
zb
text <- readLines(file("C:/Users/Anna/Desktop/Mgr/2017Q3/Text.txt"))

docs <- Corpus(VectorSource(text))
inspect(docs)


toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")


# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
#docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)

# Build a term document matrix

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 30)

# Wykres dla najczęstszych zawodów

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 12,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

# Znajdowanie słów, które występują minimum 4 razy
findFreqTerms(dtm, lowfreq = 4)

findFreqTerms(dtm, lowfreq = 2)

# Znajdowanie powiązania między terminami

findAssocs(dtm, terms = "analyst", corlimit = 0.1)

findAssocs(dtm, terms = "director", corlimit = 0.05)

# Wykres częstości

barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")

barplot(d[1:50,]$freq, las = 2, names.arg = d[1:50,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")


write.csv(d,"freq.csv")

# Źródło

#http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know

# fuzzy matching
# https://www.r-bloggers.com/fuzzy-string-matching-a-survival-skill-to-tackle-unstructured-information/



###############################################################################################
#                                          Fuzzy matching   
###############################################################################################

#install.packages("fuzzywuzzyR")
library(fuzzywuzzyR)
#### Nie działa
library(reticulate)
reticulate::py_available()
reticulate::py_config() 
reticulate::py_module_available("fuzzywuzzy")
reticulate::import("fuzzywuzzy")

use_python("C:\\Users\\Anna\\Anaconda3\\python.exe")
# install.packages("kerasR")
# library(kerasR)
reticulate::py_module_available("fuzzywuzzy")
reticulate::py_module_available("difflib")
######


d1<-d[1] # wyswietlaja sie rozne poziomy dla stanowiska

d2<-d$word
s1 = as.character(d2)

init = FuzzUtils$new()

init$Full_process(string = s1, force_ascii = TRUE)


vec = c('Frodo Baggins', 'Tom Sawyer', 'Bilbo Baggin')

str1 = 'Fra Bagg'

GetCloseMatches(string = str1, sequence_strings = vec, n = 2L, cutoff = 0.6)


##############################
install.packages("stringdist")
library(stringdist)

fuzzy_select<-function(name_vec,name){
  Filter(function(x) ain(x, name, maxDist=.7, method="jw"), 
         strsplit(name_vec, "/")[[1]])
}

fuzzy_select("ABN-AMRO-NV/SUNTRUST-BK/WACHOVIA", "SunTrust Banks")

name_vec<-zbior[,7]
name_vec<-tolower(name_vec)
name<-d$word

fuzzy_select(name_vec,name)

fuzzy_select("ABN-AMRO-NV/SUNTRUST-BK/WACHOVIA", "SunTrust Banks")


########################################################################
#              Wyliczenie liczby klastrów na podtawie odległości
#######################################################################

load(file="zbior.rdata")
library(tm)
#install.packages("arules")
library(arules) # or other package with (dis)similarity measures... 
#docs <- zbior[,7,drop=F]
docs <- zbior[,9]
dtm <- as.matrix(DocumentTermMatrix(Corpus(VectorSource(docs))))
# comparse & choose measure, e.g. Jaccard vs Dice Distance
#plot(hc <- hclust(dist(dtm, method="binary")), main="Jaccard Dist")
#plot(hc <- hclust(dissimilarity(dtm, method="Dice")), main="Dice Dist")

#hc <- hclust(dist(dtm, method="binary"))
hc <- hclust(dissimilarity(dtm, method="Dice"))
# determine cutting distance (e.g. 0.6)_
#clusters <- cutree(hc, h=.6)
clusters <- cutree(hc, h=.8) # 456 najlepszy wynik

# result
emp<-cbind.data.frame(docs, clusters)
max(emp[,2])
min(emp[,2])


library(sqldf)
zap_emp<-sqldf("select docs, clusters from emp order by 2")

save(emp,file="emp.rdata")
write.csv(zap_emp, "zap_emp.csv")
# Muszę użyć 456 klastrów, ale 32 klaster to braki danych

############################################################################################
#                         Clustering k-means
############################################################################################

#install.packages("strindist")
library(stringdist)
# Prepare Data
# Wstawienie drop=F bo chcemy, żeby był wymiarowy zbiór, a nie jako zmienna
mydata<-zbior[,7,drop=F]
mydata <- na.omit(mydata) # listwise deletion of missing
#mydata <- scale(mydata) # standardize variables 

#emp_length<-c("account","accountant","accounting","account specialist","Data Scientist","Data Science Expert")


# Wyznaczenie centers za pomocą wykresu osypiska
# wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
# for (i in 1:5527) wss[i] <- sum(kmeans(mydata,
#                                        centers=i)$withinss)
# plot(1:15, wss, type="b", xlab="Number of Clusters",
#      ylab="Within groups sum of squares")



cluster<-kmeans(stringdistmatrix(emp_title,emp_title,method="jw"),centers=912)
#cluster<-kmeans(stringdistmatrix(emp_length,emp_length,method="jw"))

cluster_n<-cluster$cluster

cbind(emp_title,cluster_n)
# emp_length            cluster_n
# [1,] "account"             "2"      
# [2,] "accountant"          "2"      
# [3,] "accounting"          "2"      
# [4,] "account specialist"  "2"      
# [5,] "Data Scientist"      "1"      
# [6,] "Data Science Expert" "1" 


########## Klastrowanie dla pozstałych zmiennych #################

docs <- zbior$zip_code
dtm <- as.matrix(DocumentTermMatrix(Corpus(VectorSource(docs))))
# comparse & choose measure, e.g. Jaccard vs Dice Distance
plot(hc <- hclust(dist(dtm, method="binary")), main="Jaccard Dist")
plot(hc <- hclust(dissimilarity(dtm, method="Dice")), main="Dice Dist")

#hc <- hclust(dist(dtm, method="binary"))
hc <- hclust(dissimilarity(dtm, method="Dice"))
# determine cutting distance (e.g. 0.6)_
# 714 dla 0.6
clusters <- cutree(hc, h=.6)
# result
zip<-cbind.data.frame(docs, clusters)

max(zip[,2])
min(wynik[,2])

save(zip,file="zip.rdata")
write.csv(zip, "zip.csv")



