## Prepare Model
options(expressions = 5e5)
options(java.parameters = "- Xmx4g")
library(tm)
library(SnowballC)
library(RWeka)
library(tm.plugin.webmining)
library(stringi)
library(stringr)
library("dplyr")
time_from <-Sys.time()
txtFileDir <- paste0(getwd(),"/data/final/en_US")
(ovid <- Corpus(DirSource(txtFileDir),readerControl = list(
    reader=readPlain,language="en_US",load=TRUE)))
## PreProcess
rmNonEng <- function(x4) removeNonASCII(x4, fields = c("Content", "Heading", "Description"),from="UTF-8",to="ASCII//TRANSLIT")
ovid <- tm_map(ovid,FUN=rmNonEng)
ovid <-tm_map(ovid,FUN=removeNumbers)
ovid <-tm_map(ovid,FUN=stripWhitespace)
ovid <-tm_map(ovid,content_transformer(tolower))
#ovid <-tm_map(ovid,removeWords, stopwords("english"))
#ovid <-tm_map(ovid,removeWords, c("can", "may", "upon", "shall", "will","must", ""))
ovid <-tm_map(ovid,content_transformer(function(x) gsub(x, pattern = "\\.", replacement = " STOP ")))
ovid <-tm_map(ovid,FUN=stripWhitespace)
ovid <-tm_map(ovid,FUN=removePunctuation)
blogCnt <- length(ovid[[1]]$content)
newsCnt <- length(ovid[[2]]$content)
twitterCnt <- length(ovid[[3]]$content)
set.seed(2020-02-02)
blogSmp <- sample(1:blogCnt,blogCnt*.02,replace=FALSE)
newsSmp <- sample(1:newsCnt,newsCnt*.02,replace=FALSE)
twitterSmp <- sample(1:twitterCnt,twitterCnt*.02,replace=FALSE)
ovidDev <- ovid
ovidDev[[1]]$content <- ovidDev[[1]]$content[blogSmp]
ovidDev[[2]]$content <- ovidDev[[2]]$content[newsSmp]
ovidDev[[3]]$content <- ovidDev[[3]]$content[newsSmp] # Well I know this is mistake
                                                      # But, it does not matter
# ovidDev[[3]]$content <- ovidDev[[3]]$content[twitterSmp]
aStops <- removePunctuation(stopwords("english"))

ovid[[1]]$content <- ovid[[1]]$content[-blogSmp]
ovid[[2]]$content <- ovid[[2]]$content[-newsSmp]
ovid[[3]]$content <- ovid[[3]]$content[-newsSmp]      # Well I know this is mistake
                                                      # But, it does not matter
# ovid[[3]]$content <- ovid[[3]]$content[-twitterSmp]
ovid2 <- ovid
ovid2 <-tm_map(ovid2,removeWords, aStops)
ovidDev <-tm_map(ovidDev,removeWords, aStops)
for(iIter in 1:5) {
    ovid2 <-tm_map(ovid2,content_transformer(function(x) gsub(x, pattern = " [[:alnum:]]{1,2} ", replacement = " ")))
    ovid2 <-tm_map(ovid2,content_transformer(function(x) gsub(x, pattern = "^[[:alnum:]]{1,2} ", replacement = "")))
    ovid2 <-tm_map(ovid2,content_transformer(function(x) gsub(x, pattern = " [[:alnum:]]{1,2}$", replacement = "")))
    ovidDev <-tm_map(ovidDev,content_transformer(function(x) gsub(x, pattern = " [[:alnum:]]{1,2} ", replacement = " ")))
    ovidDev <-tm_map(ovidDev,content_transformer(function(x) gsub(x, pattern = "^[[:alnum:]]{1,2} ", replacement = "")))
    ovidDev <-tm_map(ovidDev,content_transformer(function(x) gsub(x, pattern = " [[:alnum:]]{1,2}$", replacement = "")))
    
}



a <- ovid2[[2]]$content
b <- grep("\07",a)
if(length(b) > 0) a <- a[-b]
ovid2[[2]]$content <- a

time_to <-Sys.time()
time_to - time_from

ovid <- ovid2[1:1]
ovid <- tm_map(ovid,content_transformer(rmUnk2),w1)
wLines <- ovid[[1]]$content
con1 <- file("data/trainSamp/en_US/blog.txt",open="w")
writeLines(wLines,con1)
close(con1)
ovid <- ovid2[2:2]
ovid <- tm_map(ovid,content_transformer(rmUnk2),w1)
wLines <- ovid[[1]]$content
con1 <- file("data/trainSamp/en_US/news.txt",open="w")
writeLines(wLines,con1)
close(con1)
ovid <- ovid2[3:3]
ovid <- tm_map(ovid,content_transformer(rmUnk2),w1)
wLines <- ovid[[1]]$content
con1 <- file("data/trainSamp/en_US/twitter.txt",open="w")
writeLines(wLines,con1)
close(con1)

twoWordsDT <- NULL
threeWordsDT <- NULL
oneWordDT <- NULL

txtFileDir <- paste0(getwd(),"/data/trainSamp/en_US")
(ovid <- Corpus(DirSource(txtFileDir),readerControl = list(
    reader=readPlain,language="en_US",load=TRUE)))




# make tokens for super set.
Tokens1 <-TermDocumentMatrix(ovid,control=list(tolower=FALSE,tokenize=function(x){
    NGramTokenizer(x=x,control = Weka_control(min = 1, max = 1))
}))
Tokens2 <-TermDocumentMatrix(ovid,control=list(tolower=FALSE,tokenize=function(x){
    NGramTokenizer(x=x,control = Weka_control(min = 2, max = 2))
}))
freq1 <- rowSums(as.matrix(Tokens1))
freq2 <- rowSums(as.matrix(Tokens2))
freq2 <- freq2[freq2>1]
save(list=c("freq1","freq2"),file="freq12.Rdt")
Tokens1 <- Tokens2 <-Tokens3 <- freq1 <- freq2 <- NULL

ovid <- ovid2[1]
freq3 <- Tokens3 <- freq4 <- Tokens4 <- NULL
gc()
Tokens4 <-TermDocumentMatrix(ovid,control=list(tolower=FALSE,tokenize=function(x){
    NGramTokenizer(x=x,control = Weka_control(min = 4, max = 4))
}))
freq4 <- rowSums(as.matrix(Tokens4))
freq4 <- freq4[freq4>1]
save(list=c("freq4"),file="freq41.Rdt")
freq3 <- Tokens3 <- freq4 <- Tokens4 <- NULL
gc()
Tokens3 <-TermDocumentMatrix(ovid,control=list(tolower=FALSE,tokenize=function(x){
    NGramTokenizer(x=x,control = Weka_control(min = 3, max = 3))
}))
freq3 <- rowSums(as.matrix(Tokens3))
freq3 <- freq3[freq3>1]
save(list=c("freq3"),file="freq31.Rdt")

ovid <- ovid2[2]
freq3 <- Tokens3 <- freq4 <- Tokens4 <- NULL
gc()
Tokens4 <-TermDocumentMatrix(ovid,control=list(tolower=FALSE,tokenize=function(x){
    NGramTokenizer(x=x,control = Weka_control(min = 4, max = 4))
}))
freq4 <- rowSums(as.matrix(Tokens4))
freq4 <- freq4[freq4>1]
save(list=c("freq4"),file="freq42.Rdt")
freq3 <- Tokens3 <- freq4 <- Tokens4 <- NULL
gc()
Tokens3 <-TermDocumentMatrix(ovid,control=list(tolower=FALSE,tokenize=function(x){
    NGramTokenizer(x=x,control = Weka_control(min = 3, max = 3))
}))
freq3 <- rowSums(as.matrix(Tokens3))
freq3 <- freq3[freq3>1]
save(list=c("freq3"),file="freq32.Rdt")

ovid <- ovid2[3]
freq3 <- Tokens3 <- freq4 <- Tokens4 <- NULL
gc()
Tokens4 <-TermDocumentMatrix(ovid,control=list(tolower=FALSE,tokenize=function(x){
    NGramTokenizer(x=x,control = Weka_control(min = 4, max = 4))
}))
freq4 <- rowSums(as.matrix(Tokens4))
freq4 <- freq4[freq4>1]
save(list=c("freq4"),file="freq43.Rdt")
freq3 <- Tokens3 <- freq4 <- Tokens4 <- NULL
gc()
Tokens3 <-TermDocumentMatrix(ovid,control=list(tolower=FALSE,tokenize=function(x){
    NGramTokenizer(x=x,control = Weka_control(min = 3, max = 3))
}))
freq3 <- rowSums(as.matrix(Tokens3))
freq3 <- freq3[freq3>1]
save(list=c("freq3"),file="freq33.Rdt")
freq3 <- Tokens3 <- freq4 <- Tokens4 <- NULL
gc()


# New prepare 3


#freq11 <- subset(freq1,!grepl("STOP",names(freq1)))
#freq21 <- subset(fre21,!grepl("STOP",names(freq2)))
#freq31 <- subset(freq3,!grepl("STOP", names(freq3)) )
library(data.table)
library("dplyr")
load("freq12.Rdt")
twoWordsDT <- as.data.table(freq2)
oneWordDT <- as.data.table(freq1)
twoWordsDT$name <- names(freq2)
oneWordDT$name <- names(freq1)
twoWordsDT <- subset(twoWordsDT,!grepl("STOP",twoWordsDT$name))
oneWordDT <- subset(oneWordDT,!grepl("STOP",oneWordDT$name))


load("freq41.Rdt")
load("freq31.Rdt")
fourWordsDT1 <- as.data.table(freq4)
threeWordsDT1 <- as.data.table(freq3)
fourWordsDT1$name <- names(freq4)
threeWordsDT1$name <- names(freq3)
threeWordsDT1 <- subset(threeWordsDT1,!grepl("STOP",threeWordsDT1$name))

load("freq32.Rdt")
load("freq42.Rdt")
fourWordsDT2 <- as.data.table(freq4)
threeWordsDT2 <- as.data.table(freq3)
fourWordsDT2$name <- names(freq4)
threeWordsDT2$name <- names(freq3)
threeWordsDT2 <- subset(threeWordsDT2,!grepl("STOP",threeWordsDT2$name))

load("freq33.Rdt")
load("freq43.Rdt")
fourWordsDT3 <- as.data.table(freq4)
threeWordsDT3 <- as.data.table(freq3)
fourWordsDT3$name <- names(freq4)
threeWordsDT3$name <- names(freq3)
threeWordsDT3 <- subset(threeWordsDT3,!grepl("STOP",threeWordsDT3$name))

library("dplyr")

threeWordsDT <- threeWordsDT1 %>% funion(threeWordsDT2,all = TRUE) %>%
    funion(threeWordsDT3,all = TRUE) %>%
    group_by(name) %>% summarize(freq3 = sum(freq3)) %>%
    as.data.table()
fourWordsDT <- fourWordsDT1 %>% funion(fourWordsDT2,all = TRUE) %>%
    funion(fourWordsDT3,all = TRUE) %>%
    group_by(name) %>% summarize(freq4 = sum(freq4)) %>%
    as.data.table()

twoWordsDT$first <- sapply(strsplit(twoWordsDT$name," "),'[[',1)
twoWordsDT$second <- sapply(strsplit(twoWordsDT$name," "),'[[',2)
threeWordsDT$first <- sapply(strsplit(threeWordsDT$name," "),'[[',1)
threeWordsDT$second <- sapply(strsplit(threeWordsDT$name," "),'[[',2)
threeWordsDT$third <- sapply(strsplit(threeWordsDT$name," "),'[[',3)
fourWordsDT$first <- sapply(strsplit(fourWordsDT$name," "),'[[',1)
fourWordsDT$second <- sapply(strsplit(fourWordsDT$name," "),'[[',2)
fourWordsDT$third <- sapply(strsplit(fourWordsDT$name," "),'[[',3)
fourWordsDT$fourth <- sapply(strsplit(fourWordsDT$name," "),'[[',4)
fourWordsDT3 <-fourWordsDT2 <-fourWordsDT1 <- NULL
threeWordsDT3 <-threeWordsDT2 <-threeWordsDT1 <- NULL
freq4 <-freq3 <- freq2 <- freq1 <- NULL


twoPairDT <- twoWordsDT[,c("first","second","freq2")]
twoPairDT1 <- fourWordsDT[fourWordsDT$second != fourWordsDT$third
                          & fourWordsDT$second != "STOP"
                          ,c("first","third","freq4")]
colnames(twoPairDT1)<-colnames(twoPairDT)
twoPairDT <- funion( twoPairDT, twoPairDT1, all = TRUE)
twoPairDT1 <- fourWordsDT[fourWordsDT$second != fourWordsDT$fourth
                          & fourWordsDT$second != "STOP"
                          & fourWordsDT$third != "STOP" 
                          & fourWordsDT$third != fourWordsDT$fourth
                          ,c("first","fourth","freq4")]
colnames(twoPairDT1)<-colnames(twoPairDT)
twoPairDT <- funion( twoPairDT, twoPairDT1, all = TRUE)
twoPairDT1 <- fourWordsDT[fourWordsDT$second != fourWordsDT$first
                          & fourWordsDT$third != "STOP"  
                          & fourWordsDT$third != fourWordsDT$fourth
                          ,c("second","fourth","freq4")]
colnames(twoPairDT1)<-colnames(twoPairDT)
twoPairDT <- funion( twoPairDT, twoPairDT1, all = TRUE)
twoPairDT <- twoPairDT[twoPairDT$first != "STOP",]
twoPairDT <- twoPairDT[twoPairDT$second != "STOP",]


threePairDT <- as.data.table(threeWordsDT[,c("first","second","third","freq3")])
threePairDT1 <- fourWordsDT[fourWordsDT$third != fourWordsDT$fourth
                            & fourWordsDT$third != "STOP"
                            ,c("first","second","fourth","freq4")]
colnames(threePairDT1)<-colnames(threePairDT)
threePairDT <- funion( threePairDT, threePairDT1, all = TRUE)
threePairDT1 <- fourWordsDT[fourWordsDT$second != fourWordsDT$third
                            & fourWordsDT$second != "STOP"
                            ,c("first","third","fourth","freq4")]
colnames(threePairDT1)<-colnames(threePairDT)
threePairDT <- funion( threePairDT, threePairDT1, all = TRUE)
threePairDT <- threePairDT[threePairDT$first != "STOP",]
threePairDT <- threePairDT[threePairDT$second != "STOP",]
threePairDT <- threePairDT[threePairDT$third != "STOP",]

threePairDT <- threePairDT %>% group_by(first,second,third) %>%
    summarize(freq3 = sum(freq3)) %>% as.data.table()
twoPairDT <- twoPairDT %>% group_by(first,second) %>%
    summarize(freq2 = sum(freq2)) %>% as.data.table()



twoPairDT1 <- threePairDT1 <- NULL






