## Train 2
options(java.parameters = "- Xmx1024m")
options(expressions = 5e5)
library(tm)
library(SnowballC)
library(RWeka)
library(tm.plugin.webmining)
library(stringi)
library("dplyr")
library(data.table)
library(hash)
require(stringr)
require(data.table)
require(tm)
require(stringr)
require(stringi)
require(dplyr)
##load("fourWords.Rdt")
#load("ovidDev.Rdt")
#load("wordsDT.Rdt")
#load("pairDT.Rdt")

twoPairDT       <- twoPairDT %>% group_by(first,second) %>% summarize(freq2 = sum(freq2)) %>%
    as.data.frame()
#twoPairDT[,c("first","second","freq2")]
threePairDT     <- threePairDT %>% group_by(first,second,third) %>% summarize(freq3 = sum(freq3)) %>%
    as.data.frame()
#[,c("first","second","third","freq3")]
oneWordDT       <- oneWordDT[oneWordDT$freq1>1,c("name","freq1")]
twoWordsDT      <- twoWordsDT[,c("first","second","freq2")]
threeWordsDT    <- threeWordsDT[,c("first","second","third","freq3")]

twoPairDT <-    twoPairDT %>% group_by(first) %>%
    mutate(prob2=(freq2-1.5)/sum(freq2)) %>% ungroup %>% as.data.table()
threePairDT <-  threePairDT %>% group_by(first,second) %>%
    mutate(prob3=(freq3-1.5)/sum(freq3)) %>% ungroup %>% as.data.table()
oneWordDT$prob1 <- (oneWordDT$freq1-1.5)/ sum(oneWordDT$freq1)
twoWordsDT <-   twoWordsDT %>% group_by(first) %>%
    mutate(prob2=(freq2-1.5)/sum(freq2)) %>% ungroup %>% as.data.table()
threeWordsDT <- threeWordsDT %>% group_by(first,second) %>%
    mutate(prob3=(freq3-1.5)/sum(freq3)) %>% ungroup %>% as.data.table()


twoPairDT       <- twoPairDT[twoPairDT$freq2>4,]
threePairDT     <- threePairDT[threePairDT$freq3>4,]
oneWordDT       <- oneWordDT[oneWordDT$freq1>4,]
twoWordsDT      <- twoWordsDT[twoWordsDT$freq2>4,]
threeWordsDT    <- threeWordsDT[threeWordsDT$freq3>4,]

threePairDT     <- merge(threePairDT,twoPairDT,by.x=c("second","third"), 
                         by.y=c("first","second"), all.x = TRUE, all.y = FALSE)
threePairDT     <- threePairDT[,c("first","second","third","freq3","prob3","prob2")]
twoPairDT       <- merge(twoPairDT,oneWordDT,by.x="second",by.y="name")
twoPairDT       <- twoPairDT[,c("first","second","freq2","prob2","prob1")]

threeWordsDT    <- merge(threeWordsDT,twoWordsDT,by.x=c("second","third"), 
                         by.y=c("first","second"), all.x = TRUE, all.y = FALSE)
threeWordsDT    <- threeWordsDT[,c("first","second","third","freq3","prob3","prob2")]
twoWordsDT      <- merge(twoWordsDT,oneWordDT,by.x="second",by.y="name")
twoWordsDT      <- twoWordsDT[,c("first","second","freq2","prob2","prob1")]

threePairDT1 <- NULL



#save(list=c("threePairDT","twoPairDT", "oneWordDT","threeWordDevDF"),file="pairDT.Rdt")
#load("pairDT.Rdt")
threePairDefDT  <- threePairDT %>% group_by(first,second) %>% 
    summarize(totProb3=1-sum(prob3),totProb2=1-sum(prob2)) %>% as.data.table()
twoPairDefDT    <- twoPairDT %>% group_by(first) %>% 
    summarize(totProb2=1-sum(prob2),totProb1=1-sum(prob1)) %>% as.data.table()
oneWordDef      <- 1- sum(oneWordDT$prob1)
threeWordsDefDT <- threeWordsDT %>% group_by(first,second) %>% 
    summarize(totProb3=1-sum(prob3),totProb2=1-sum(prob2)) %>% as.data.table()
twoWordsDefDT   <- twoWordsDT %>% group_by(first) %>% 
    summarize(totProb2=1-sum(prob2),totProb1=1-sum(prob1)) %>% as.data.table()

#threeWordDevDF1 <- sample(1:length(threeWordDevDF$freq),1002,replace=FALSE)
#threeWordDevDF1 <- threeWordDevDF[threeWordDevDF1,]

oneWordDT1 <- oneWordDT
setkey(oneWordDT1,name)

twoPairDT1 <- twoPairDT
twoPairDT1$name <- paste(twoPairDT1$first,twoPairDT1$second)
twoPairDT1 <- twoPairDT1[,c("name","prob2")]
setkey(twoPairDT1, name)

threePairDT1<- threePairDT
threePairDT1$name <- paste(threePairDT1$first,threePairDT1$second,threePairDT1$third)
threePairDT1 <- threePairDT1[,c("name","prob3")]
setkey(threePairDT1,name)

threePairDefDT1 <- threePairDefDT[]
threePairDefDT1$name <- paste(threePairDefDT1$first,threePairDefDT1$second)
threePairDefDT1 <- threePairDefDT1[,c("name","totProb3","totProb2")]
setkey(threePairDefDT1,name)

twoPairDefDT1 <- twoPairDefDT
twoPairDefDT1$name <- twoPairDefDT1$first
twoPairDefDT1 <- twoPairDefDT1[,c("name","totProb2","totProb1")]
setkey(twoPairDefDT1,name)

twoWordsDT1 <- twoWordsDT
twoWordsDT1$name <- paste(twoWordsDT1$first,twoWordsDT1$second)
twoWordsDT1 <- twoWordsDT1[,c("name","prob2")]
setkey(twoWordsDT1, name)

threeWordsDT1 <- threeWordsDT
threeWordsDT1<- threeWordsDT
threeWordsDT1$name <- paste(threeWordsDT1$first,threeWordsDT1$second,threeWordsDT1$third)
threeWordsDT1 <- threeWordsDT1[,c("name","prob3")]
setkey(threeWordsDT1,name)

threeWordsDefDT1 <- threeWordsDefDT[]
threeWordsDefDT1$name <- paste(threeWordsDefDT1$first,threeWordsDefDT1$second)
threeWordsDefDT1 <- threeWordsDefDT1[,c("name","totProb3","totProb2")]
setkey(threeWordsDefDT1,name)

twoWordsDefDT1 <- twoWordsDefDT
twoWordsDefDT1$name <- twoWordsDefDT1$first
twoWordsDefDT1 <- twoWordsDefDT1[,c("name","totProb2","totProb1")]
setkey(twoWordsDefDT1,name)

#twoPairDT1 <- twoPairDT1[,c("name","prob2")]
#twoWordsDT1 <- twoWordsDT1[,c("name","prob2")]
#threeWordsDefDT1 <- threeWordsDefDT1[,c("name","totProb3","totProb2")]
twoWordDT2 <- twoPairDT1 %>% 
    mutate(prob21=0,prob23=prob2,totProb31=0,totProb21=0,totProb33=0,totProb23=0) %>% 
    select(name,prob21,prob23,totProb21,totProb31,totProb23,totProb33) %>% 
    as.data.table() %>% 
    funion(twoWordsDT1 %>% 
               mutate(prob21=prob2,prob23=0,totProb31=0,totProb21=0,totProb33=0,totProb23=0) %>% 
               select(name,prob21,prob23,totProb21,totProb31,totProb23,totProb33) %>%
               as.data.table() ,all=TRUE) %>% 
    funion(threeWordsDefDT1 %>% 
               mutate(prob21=0,prob23=0,totProb21= totProb2, totProb31 = totProb3,totProb23=0,totProb33=0) %>% 
               select(name,prob21,prob23,totProb21,totProb31,totProb23,totProb33) %>%
               as.data.table(), all= TRUE) %>% 
    funion(threePairDefDT1 %>% 
               mutate(prob21=0,prob23=0,totProb21= 0, totProb31 = 0,totProb23=totProb2,totProb33=totProb3) %>% 
               select(name,prob21,prob23,totProb21,totProb31,totProb23,totProb33) %>%
               as.data.table(), all= TRUE) %>% 
    group_by(name) %>% 
    summarize(prob21=sum(prob21),prob23=sum(prob23),totProb31=sum(totProb31),totProb21=sum(totProb21),
              totProb33=sum(totProb33),totProb23=sum(totProb23)) %>% 
    as.data.table()
threeWordDT2 <- threePairDT1 %>% 
    mutate(prob31=0,prob33=prob3) %>% 
    select(name,prob31,prob33) %>% 
    as.data.table() %>% 
    funion(threeWordsDT1 %>% 
               mutate(prob31=prob3,prob33=0) %>% 
               select(name,prob31,prob33) %>% 
               as.data.table() ,all=TRUE) %>% 
    group_by(name) %>% 
    summarize(prob31=sum(prob31),prob33=sum(prob33)) %>% 
    as.data.table()
oneWordDT2 <- oneWordDT1 %>% 
    mutate(totProb21=0,totProb11=0,totProb23=0,totProb13=0) %>% 
    select(name,prob1,totProb21,totProb11,totProb23,totProb13) %>%  
    as.data.table() %>% 
    funion(twoWordsDefDT1 %>% 
               mutate(prob1=0,totProb21=totProb2,totProb11=totProb1,totProb23=0,totProb13=0) %>% 
               select(name,prob1,totProb21,totProb11,totProb23,totProb13) %>% 
               as.data.table(), all= TRUE) %>% 
    funion(twoPairDefDT1 %>% 
               mutate(prob1=0,totProb21=0,totProb11=0,totProb23=totProb2,totProb13=totProb1) %>% 
               select(name,prob1,totProb21,totProb11,totProb23,totProb13) %>% 
               as.data.table(), all= TRUE) %>% 
    group_by(name) %>% 
    summarize(prob1=sum(prob1),totProb21=sum(totProb21),totProb11=sum(totProb11),
              totProb23=sum(totProb23),totProb13=sum(totProb13)) %>% 
    as.data.table()

setkey(twoWordDT2,name)
setkey(threeWordDT2,name)
setkey(oneWordDT2,name)

twoWordsDTs <- twoWordsDT %>% group_by(first) %>% top_n(5,prob2) %>% 
    mutate(prefix=first,word=second,prob = prob2)  %>%
    select(prefix,word,prob) %>% as.data.table()
threeWordsDTs <- threeWordsDT %>% group_by(first,second) %>% top_n(5,prob3) %>% 
    mutate(prefix = paste(first,second),word=third,prob = prob3) %>%
    select(prefix,word,prob) %>% as.data.table()
oneWordDTs <- oneWordDT %>% top_n(5,prob1) %>%
    mutate(word=name,prob = prob1) %>%
    select(word,name,prob) %>% as.data.table()
setkey(twoWordsDTs,prefix)
setkey(threeWordsDTs,prefix)
setkey(oneWordDTs,name)
# rmUnk3 <- function(b1) {
#     
#     b2 <- unlist(strsplit(b1," "))
#     b2 <- b2[b2 != ""]
#     paste(b2[unlist(lapply(b2,exists,dEnv))],collapse=" ")
# }
getNextSimple <- function (s){
    context <- s
    #context = "come bring home"
    cntRecord <- 5
    context <- removeNumbers(context)
    context <- stripWhitespace(context)
    context <- tolower(context)
    context <- removePunctuation(context)
    for(iIter in 1:5)
        context <-gsub(context, pattern = " [[:alnum:]]{1,2} ", replacement = " ")
    context <-gsub(context, pattern = " [[:alnum:]]{1,2}$", replacement = " ")
    context <- removeWords(context,aStops)
    context <- stripWhitespace(context)
    # context <- rmUnk3(context)
    context <- trimws(context, "r")
    
    sList <- unlist(strsplit(context," "))
    context <- paste("STOP STOP STOP ", context)
    u <- sList[length(sList)-1]
    v <- sList[length(sList)]
    uv <- paste(u,v)
    tmp <- threeWordsDTs[uv,c("word","prob")]
    if(is.na(tmp[1,1])) tmp <- twoWordsDTs[v,c("word","prob")]
    if(is.na(tmp[1,1])) tmp <- oneWordDTs[,c("word","prob")]
    tmp
    
}

# load("dEnv.Rdt")

gc()
txtFileDir <- paste0(getwd(),"/data/DevSamp/en_US")
(ovidDev <- Corpus(DirSource(txtFileDir),readerControl = list(
    reader=readPlain,language="en_US",load=TRUE)))
Tokens4Dev <-TermDocumentMatrix(ovidDev,control=list(tolower=FALSE,tokenize=function(x){
    NGramTokenizer(x=x,control = Weka_control(min = 4, max = 4))
}))


freq4Dev <- rowSums(as.matrix(Tokens4Dev))
#freq4Dev<-freq4Dev[freq4Dev>1]
freq4Dev <-subset(freq4Dev,!grepl("STOP",names(freq4Dev)))
fourWordDevDF <- as.data.table(freq4Dev)
fourWordDevDF$name <- names(freq4Dev)
fourWordDevDF$first <- sapply(strsplit(fourWordDevDF$name," "),'[[',1)
fourWordDevDF$second <- sapply(strsplit(fourWordDevDF$name," "),'[[',2)
fourWordDevDF$third <- sapply(strsplit(fourWordDevDF$name," "),'[[',3)
fourWordDevDF$fourth <- sapply(strsplit(fourWordDevDF$name," "),'[[',4)
fourWordDevDF$list <- sapply(apply(fourWordDevDF,1,kPrep),"[",1)


### Functions

# load("dEnv.Rdt")




mt1 <- matrix(rep(0,150),nrow=25,ncol=6)
replNA <- function(x){if(is.na(x)) 0 else x}
ifNAxy <- function(x,y){if(is.na(x)) y else x}
ifNAxy0 <- function(x,y){if(is.na(x)) y else if (x==0) y else x}

calcPerp6 <- function(x) {
    t <- x["first"]
    u <- x["second"]
    v <- x["third"]
    w <- x["fourth"]
    freq3 <- as.numeric(x["freq4Dev"])
    uv <- paste(u,v)
    vw <- paste(v,w)
    uvw <- paste(u,v,w)
    
    tu <- paste(t,u)
    tv <- paste(t,v)
    tw <- paste(t,w)
    uw <- paste(u,w)
    tvw <- paste(t,v,w)
    tuw <- paste(t,u,w)
    
    if(replNA(twoWordDT2[uv]$prob21)>0) {
        k = 1; Lambda1 <- Lambda11 ; Lambda2 <- Lambda21; Lambda3 <-Lambda31; Lambda4 <- Lambda41;Lambda5 <- Lambda51
    }else if(replNA(twoWordDT2[uv]$prob23)+replNA(twoWordDT2[tu]$prob23)+replNA(twoWordDT2[tv]$prob23)>0) {
        k = 4; Lambda1 <- Lambda11 ; Lambda2 <- Lambda21; Lambda3 <-Lambda31; Lambda4 <- Lambda41A;Lambda5 <- Lambda51A
    }else if(replNA(oneWordDT2[v]$prob1)>0) {
        k = 2; Lambda1 <- Lambda12 ; Lambda2 <- Lambda22; Lambda3 <-Lambda32; Lambda4 <- Lambda42;Lambda5 <- Lambda52
    }else if(replNA(oneWordDT2[v]$prob1)+replNA(oneWordDT2[u]$prob1)+replNA(oneWordDT2[t]$prob1)>0) {
        k = 5; Lambda1 <- Lambda12 ; Lambda2 <- Lambda22; Lambda3 <-Lambda32; Lambda4 <- Lambda42A;Lambda5 <- Lambda52A
    } else {
        k =3; Lambda1 <- Lambda13 ; Lambda2 <- Lambda23; Lambda3 <-Lambda33; Lambda4 <- Lambda43;Lambda5 <- Lambda53
    }
    sum1 <- freq3 * log10(
        Lambda1* Lambda4 / 3.0 * 
            (ifNAxy0( threeWordDT2[ uvw]$prob33, 
                      replNA( twoWordDT2[uv]$totProb33 *  twoWordDT2[vw]$prob23 / twoWordDT2[uv]$totProb23) )
            ) + Lambda1* Lambda4 / 3.0 * 
            (ifNAxy0( threeWordDT2[ tvw]$prob33, 
                      replNA( twoWordDT2[tv]$totProb33 *  twoWordDT2[vw]$prob23 / twoWordDT2[tv]$totProb23) )
            ) +Lambda1* Lambda4 / 3.0 * 
            (ifNAxy0( threeWordDT2[ tuw]$prob33, 
                      replNA( twoWordDT2[tu]$totProb33 *  twoWordDT2[uw]$prob23 / twoWordDT2[tu]$totProb23) )
            ) +
            Lambda1 * (1 - Lambda4 ) * 
            (ifNAxy0( threeWordDT2[ uvw]$prob31, 
                      replNA( twoWordDT2[uv]$totProb31 *  twoWordDT2[vw]$prob21 / twoWordDT2[uv]$totProb21) )
            ) +    
            Lambda2 * Lambda5 / 3.0 * 
            (ifNAxy0(twoWordDT2[vw]$prob23, 
                     replNA(  oneWordDT2[v]$totProb23 * oneWordDT2[w]$prob1  / oneWordDT2[v]$totProb13)) 
            ) +  Lambda2 * Lambda5 / 3.0 * 
            (ifNAxy0(twoWordDT2[tw]$prob23, 
                     replNA(  oneWordDT2[t]$totProb23 * oneWordDT2[w]$prob1  / oneWordDT2[t]$totProb13)) 
            ) +  Lambda2 * Lambda5 / 3.0 * 
            (ifNAxy0(twoWordDT2[uw]$prob23, 
                     replNA(  oneWordDT2[u]$totProb23 * oneWordDT2[w]$prob1  / oneWordDT2[u]$totProb13)) 
            ) +   
            Lambda2 * (1 - Lambda5 ) * 
            (ifNAxy0(twoWordDT2[vw]$prob21, 
                     replNA(  oneWordDT2[v]$totProb21 * oneWordDT2[w]$prob1  / oneWordDT2[v]$totProb11)) 
            ) + 
            Lambda3 * (ifNAxy0(max(oneWordDT1[w]$prob1,oneWordDef/length(oneWordDT2$name)), replNA( oneWordDef/length(oneWordDT2$name)))  )       
    )        
    sum1
    
}

kPrep <- function(x) {
    t <- x["first"]
    u <- x["second"]
    v <- x["third"]
    w <- x["fourth"]
    freq3 <- as.numeric(x["freq4Dev"])
    uv <- paste(u,v)
    vw <- paste(v,w)
    uvw <- paste(u,v,w)
    
    tu <- paste(t,u)
    tv <- paste(t,v)
    tw <- paste(t,w)
    uw <- paste(u,w)
    tvw <- paste(t,v,w)
    tuw <- paste(t,u,w)
    
    if(replNA(twoWordDT2[uv]$prob31)>0) {
        k = 1
    }else if(replNA(twoWordDT2[uv]$prob23)+replNA(twoWordDT2[tu]$prob23)+replNA(twoWordDT2[tv]$prob23)>0) {
        k = 2
    }else if(replNA(oneWordDT2[v]$prob1)>0) {
        k = 3
    }else if(replNA(oneWordDT2[v]$prob1)+replNA(oneWordDT2[u]$prob1)+replNA(oneWordDT2[t]$prob1)>0) {
        k = 4
    } else {
        k =5
    }
    prob33s <- (ifNAxy0( threeWordDT2[ uvw]$prob33, 
                         replNA( twoWordDT2[uv]$totProb33 *  twoWordDT2[vw]$prob23 / twoWordDT2[uv]$totProb23) )
    ) + 
        (ifNAxy0( threeWordDT2[ tvw]$prob33, 
                  replNA( twoWordDT2[tv]$totProb33 *  twoWordDT2[vw]$prob23 / twoWordDT2[tv]$totProb23) )
        ) + 
        (ifNAxy0( threeWordDT2[ tuw]$prob33, 
                  replNA( twoWordDT2[tu]$totProb33 *  twoWordDT2[uw]$prob23 / twoWordDT2[tu]$totProb23) )
        ) 
    prob31s <- (ifNAxy0( threeWordDT2[ uvw]$prob31, 
                         replNA( twoWordDT2[uv]$totProb31 *  twoWordDT2[vw]$prob21 / twoWordDT2[uv]$totProb21) )
    )  
    prob23s <- (ifNAxy0(twoWordDT2[vw]$prob23, 
                        replNA(  oneWordDT2[v]$totProb23 * oneWordDT2[w]$prob1  / oneWordDT2[v]$totProb13)) 
    ) +  (ifNAxy0(twoWordDT2[tw]$prob23, 
                  replNA(  oneWordDT2[t]$totProb23 * oneWordDT2[w]$prob1  / oneWordDT2[t]$totProb13)) 
    ) +  (ifNAxy0(twoWordDT2[uw]$prob23, 
                  replNA(  oneWordDT2[u]$totProb23 * oneWordDT2[w]$prob1  / oneWordDT2[u]$totProb13)) 
    ) 
    prob21s <- (ifNAxy0(twoWordDT2[vw]$prob21, 
                        replNA(  oneWordDT2[v]$totProb21 * oneWordDT2[w]$prob1  / oneWordDT2[v]$totProb11)) 
    ) 
    prob1s <- (ifNAxy0(max(oneWordDT1[w]$prob1,oneWordDef/length(oneWordDT2$name)), replNA( oneWordDef/length(oneWordDT2$name)))  )       
    paste(k,prob33s,prob31s,prob23s,prob21s,prob1s,sep=";")
    
    
}
set.seed(2020-02-14)
devDFsmp <- sample(1:length(fourWordDevDF$freq4Dev),800,replace=FALSE)
devDF <- fourWordDevDF[devDFsmp,]
devDF$list <- sapply(apply(devDF,1,kPrep),"[",1)
devDF$k         <- as.numeric(sapply(strsplit(devDF$list,";"),'[[',1))
devDF$prob33s   <- as.numeric(sapply(strsplit(devDF$list,";"),'[[',2))
devDF$prob31s   <- as.numeric(sapply(strsplit(devDF$list,";"),'[[',3))
devDF$prob23s   <- as.numeric(sapply(strsplit(devDF$list,";"),'[[',4))
devDF$prob21s   <- as.numeric(sapply(strsplit(devDF$list,";"),'[[',5))
devDF$prob1s    <- as.numeric(sapply(strsplit(devDF$list,";"),'[[',6))

aStops <- removePunctuation(stopwords("english"))
predictNextWord2 <- function(context,wList) {
    context <- removeNumbers(context)
    context <- stripWhitespace(context)
    context <- tolower(context)
    context <- removePunctuation(context)
    for(iIter in 1:5) {
        context <-gsub(context, pattern = " [[:alnum:]]{1,2} ", replacement = " ")
        context <-gsub(context, pattern = " [[:alnum:]]{1,2}$", replacement = " ")
    }
    context <- removeWords(context,aStops)
    #context <- rmUnk3(context)
    context <- stripWhitespace(context)
    context <- trimws(context, "r")
    context <- paste ("STOP STOP STOP", context)
    t <- word(context,-3)
    u <- word(context,-2)
    v <- word(context,-1)
    wList <- stripWhitespace(wList)
    testDF <- strsplit(wList,split = " ")[[1]]
    testDF <- as.data.table(testDF)
    colnames(testDF)<- "fourth"
    testDF$first <- word(context,-3)
    testDF$second <- word(context,-2)
    testDF$third <- word(context,-1)
    testDF$freq4Dev <- 1
    testDF$perplexity <- sapply(apply(testDF,1,calcPerp6),"[",1)
    testDF <- testDF[order(-testDF$perplexity),]
    
    testDF
}

#### starting point
Lambda11 <- 0.84
Lambda21 <- 0.08
Lambda31 <- (1 - Lambda11 - Lambda21)

Lambda12 <- 0.04
Lambda22 <- 0.646666
Lambda32 <- (1 - Lambda12 - Lambda22)

Lambda13 <- 0.01
Lambda23 <- 0.0495
Lambda33 <- (1 - Lambda13 - Lambda23)

Lambda41 <- 0.3
Lambda51 <- 0.92

Lambda41A <- 0.9
Lambda51A <- 0.1

Lambda42 <- 0.9
Lambda52 <- 0.9

Lambda42A <- 0.9
Lambda52A <- 0.9

Lambda43 <- 0.2
Lambda53 <- 0.92
#### (first set)
Sys.time()
for (iRow in 1:5) {
    for (jRow in 1:5) {     
        Lambda41A <- 0.1 + 0.2 * (iRow-1)
        Lambda51A <- 0.1 + 0.2 * (jRow-1)  
        mt1[(iRow-1)*5+jRow,3] <- Lambda41A
        mt1[(iRow-1)*5+jRow,4] <- Lambda51A
        
        # Lambda11  <- 0.69 + .05 * (iRow-1)
        # Lambda21 <- (1 - Lambda11) * jRow /6
        # Lambda31 <- (1 - Lambda11 - Lambda21)
        #Lambda12  <- 0.0 + (iRow - 1) * .04
        #Lambda22 <- (0.51 - Lambda12) * (  jRow / 12) + .49
        #Lambda32 <- (1 - Lambda12 - Lambda22)
        mt1[(iRow-1)*5+jRow,1]<- Lambda12;mt1[(iRow-1)*5+jRow,2]<- Lambda22;
        time_from <- Sys.time() 
        #threeWordDevDF1$perplexity<-sapply(apply(threeWordDevDF1,1,calcPerp),"[",1)
        fourWordDevDF$perplexity <- sapply(apply(fourWordDevDF,1,calcPerp6),"[",1)
        time_to <- Sys.time()
        sum1 <- sum(fourWordDevDF$perplexity)
        mt1[(iRow-1)*5+jRow,5]<-sum1
        time_to <-Sys.time()
        mt1[(iRow-1)*5+jRow,6]<-time_to - time_from
    }
}
Sys.time()
mt1[mt1[,5] ==max(mt1[,5]),]


time_from <- Sys.time() ;threeWordDevDF1$perplexity<-sapply(apply(threeWordDevDF1,1,calcPerp),"[",1);time_to <- Sys.time();time_to - time_from
threeWordDevDF1
### Iterations
# for (jRow in 1:5) {
#Lambda13  <- 0.01 + .02 * (iRow-1)
#     Lambda23 <- (1 - Lambda13) * jRow /20
#     Lambda33 <- (1 - Lambda13 - Lambda23)
#     mt1[(iRow-1)*5+jRow,1]<- Lambda13;mt1[(iRow-1)*5+jRow,2]<- Lambda23;mt1[(iRow-1)*5+jRow,3]<- Lambda33;

# for (jRow in 1:5) {
# Lambda12  <- 0.01 + .04 * (iRow-1)
#    Lambda22 <- (1 - Lambda12) * jRow /10 + .2
#    Lambda32 <- (1 - Lambda12 - Lambda22)
#    mt1[(iRow-1)*5+jRow,1]<- Lambda12;mt1[(iRow-1)*5+jRow,2]<- Lambda22;mt1[(iRow-1)*5+jRow,3]<- Lambda32;

# for (jRow in 1:5) {
# Lambda11  <- 0.80 + .01 * (iRow-1)
#     Lambda21 <- (1 - Lambda11) * jRow /6
#     Lambda31 <- (1 - Lambda11 - Lambda21)    
#     mt1[(iRow-1)*5+jRow,1]<- Lambda11;mt1[(iRow-1)*5+jRow,2]<- Lambda21;mt1[(iRow-1)*5+jRow,3]<- Lambda31;

#Lambda41 <- 0.02 + 0.01 * (iRow-1)
#Lambda51 <- 0.08 + 0.01 * (jRow-1)


mt1 <- matrix(rep(0,3750),nrow=625,ncol=6)
cPerp6 <- function(x) {
    for (i in 1:5)
        for (j in 1:5)
            for (k in 1:5)
                for (l in 1:5){
                    lambda1 <- .1 + (i-1)*.2
                    lambda2 <- (1- lambda1) * (.1 + (j-1)*.2)
                    lambda3 <- (1- lambda1 - lambda2 ) * (.1 + (k-1)*.2)
                    lambda4 <- (1- lambda1 - lambda2 - lambda3 ) * (.1 + (l-1)*.2)
                    lambda5 <- (1- lambda1 - lambda2 - lambda3 - lambda4)
                    x$Perp <- x$prob31s * lambda1 + 
                        x$prob33s * lambda2 +
                        x$prob21s * lambda3 +
                        x$prob23s * lambda4 +
                        x$prob1s * lambda5 
                    mt1[((((i-1)*5+(j-1))*5+k-1)*5+l),1]<- lambda1
                    mt1[((((i-1)*5+(j-1))*5+k-1)*5+l),2]<- lambda2
                    mt1[((((i-1)*5+(j-1))*5+k-1)*5+l),3]<- lambda3
                    mt1[((((i-1)*5+(j-1))*5+k-1)*5+l),4]<- lambda4
                    mt1[((((i-1)*5+(j-1))*5+k-1)*5+l),5]<- 
                        sum( x$freq4Dev * log10((x$Perp )))
                }
}