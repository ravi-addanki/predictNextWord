#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

require(shiny)
require(data.table)
require(tm)
require(stringr)
require(stringi)
require(dplyr)
load("data/finalWordDT.Rdt")

twoWordDT2$prefix <- sub(" [[:alnum:]]*$","",twoWordDT2$name)
twoWordDT2$word <- sub("^[[:alnum:]]* ","",twoWordDT2$name)
threeWordDT2$prefix <- sub(" [[:alnum:]]*$","",threeWordDT2$name)
threeWordDT2$word <- sub("^[[:alnum:]]* [[:alnum:]]* ","",threeWordDT2$name)
oneWordDT2$word <- oneWordDT2$name
setkey(twoWordDT2,prefix)
setkey(threeWordDT2,prefix)
setkey(oneWordDT2,word)
twoWordsDTs <- twoWordDT2 %>% group_by(prefix) %>% top_n(5,prob21) %>% 
  mutate(prob = prob21)  %>%
  select(prefix,word,prob) %>% as.data.table()
threeWordsDTs <- threeWordDT2 %>% group_by(prefix) %>% top_n(5,prob31) %>% 
  mutate(prob = prob31) %>%
  select(prefix,word,prob) %>% as.data.table()
oneWordDTs <- oneWordDT2 %>% top_n(5,prob1) %>%
  mutate(prob = prob1) %>%
  select(word,name,prob) %>% as.data.table()
setkey(twoWordsDTs,prefix)
setkey(threeWordsDTs,prefix)
replNA <- function(x){if(is.na(x)) 0 else x}
ifNAxy <- function(x,y){if(is.na(x)) y else x}
ifNAxy0 <- function(x,y){if(is.na(x)) y else if (x==0) y else x}
aStops <- removePunctuation(stopwords("english"))
rmUnk3 <- function(b1) {

    b2 <- unlist(strsplit(b1," "))
    b2 <- b2[b2 != ""]
    paste(b2[unlist(lapply(b2,exists,dEnv))],collapse=" ")
}
#### starting point
Lambda11 <- 0.84;Lambda21 <- 0.08;Lambda31 <- (1 - Lambda11 - Lambda21)
Lambda12 <- 0.04;Lambda22 <- 0.646666;Lambda32 <- (1 - Lambda12 - Lambda22)
Lambda13 <- 0.01;Lambda23 <- 0.0495;Lambda33 <- (1 - Lambda13 - Lambda23)
#Lambda41 <- 0.3;Lambda51 <- 0.92;Lambda41A <- 0.9;Lambda51A <- 0.1
#Lambda42 <- 0.9;Lambda52 <- 0.9;Lambda42A <- 0.9;Lambda52A <- 0.9
#Lambda43 <- 0.2;Lambda53 <- 0.92
getDef3 <- function(w,u,v,uv){
    ifNAxy0(sum(threeWordDT2[prefix==uv&word==w ,prob31]), 
        replNA( sum(twoWordDT2[prefix==u&word==v,totProb31]) * 
                sum(twoWordDT2[prefix==v&word==w,prob21]) /
                sum(twoWordDT2[prefix==u&word==v,totProb21])
            ) 
        )
    
}
getDef2 <- function(w,u,v){
    ifNAxy0(sum(twoWordDT2[prefix==u& word==w ,prob21]), 
        replNA( sum(oneWordDT2[word==v,totProb21]) *
                sum(oneWordDT2[word==w,prob1]) /
                sum(oneWordDT2[word==v,totProb11])
                ) 
        )
    
}
predNextBkt <- function(context,cntRecord,wDiscount,wSimple){
  v <- word(c(context),-1)
  
  w <- substr(v,str_length(v),str_length(v))
  if (w=='*') {
    w <- tolower(substr(v,1,str_length(v)-1))
    context <- substr(context,1,str_length(context)-str_length(v))
  } else w <- ""
    context <- removeNumbers(context)
    context <- stripWhitespace(context)
    context <- tolower(context)
    context <- removePunctuation(context)
    for(iIter in 1:5) 
        context <-gsub(context, pattern = " [[:alnum:]]{1,2} ", replacement = " ")
    context <-gsub(context, pattern = " [[:alnum:]]{1,2}$", replacement = " ")
    context <- removeWords(context,aStops)
    #if (wSimple) context <- rmUnk3(context)
    context <- stripWhitespace(context)
    context <- trimws(context, "r")
    t <- word(c(context),-3)
    u <- word(c(context),-2)
    v <- word(c(context),-1)
    uv <- paste(u,v)
    if (wSimple) {
      abc <- threeWordsDTs[uv,c("word","prob")]
      if(is.na(abc[1,1])) abc <- twoWordsDTs[v,c("word","prob")]
      if(is.na(abc[1,1])) abc <- oneWordDTs[,c("word","prob")]
      abc <- abc[order(-abc$prob),]
    } else {
    #vw <- paste(v,w)
    #uvw <- paste(u,v,w)
    tu <- paste(t,u)
    tv <- paste(t,v)
    #tw <- paste(t,w)
    #uw <- paste(u,w)
    #tvw <- paste(t,v,w)
    #tuw <- paste(t,u,w)
    
    if(replNA(sum(twoWordDT2[prefix==u & word==v,]$prob21))>0) {
        k = 1; Lambda1 <- Lambda11 ; Lambda2 <- Lambda21; Lambda3 <-Lambda31; #Lambda4 <- Lambda41;Lambda5 <- Lambda51
    # }else if(replNA(twoWordDT2[prefix==u & word==v,]$prob23)+replNA(twoWordDT2[tu]$prob23)+replNA(twoWordDT2[tv]$prob23)>0) {
    #     k = 4; Lambda1 <- Lambda11 ; Lambda2 <- Lambda21; Lambda3 <-Lambda31; Lambda4 <- Lambda41A;Lambda5 <- Lambda51A
    }else if(replNA(sum(oneWordDT2[word==v]$prob1))>0) {
        k = 2; Lambda1 <- Lambda12 ; Lambda2 <- Lambda22; Lambda3 <-Lambda32; #Lambda4 <- Lambda42;Lambda5 <- Lambda52
    # }else if(replNA(oneWordDT2[v]$prob1)+replNA(oneWordDT2[u]$prob1)+replNA(oneWordDT2[t]$prob1)>0) {
    #     k = 5; Lambda1 <- Lambda12 ; Lambda2 <- Lambda22; Lambda3 <-Lambda32; Lambda4 <- Lambda42A;Lambda5 <- Lambda52A
    } else {
        k =3; Lambda1 <- Lambda13 ; Lambda2 <- Lambda23; Lambda3 <-Lambda33; #Lambda4 <- Lambda43;Lambda5 <- Lambda53
    }
    # set11 <-  threeWordDT2[ prefix==uv | prefix==tv | prefix == tu,c("word","prob33") ] %>%
    #     mutate(prob=Lambda1* Lambda4 / 3.0 * prob33 ) %>% select(word,prob) %>% as.data.table() %>%
    #     funion(
    #         threeWordDT2[ uv,c("word","prob31") ] %>% mutate(prob=Lambda1* (1 - Lambda4) * prob31 )  %>% 
    #             select(word,prob) %>% as.data.table() ,
    #         all = TRUE
    #     ) %>% group_by(word) %>% summarize(prob=sum(prob)) %>% top_n(cntRecord,prob) %>% as.data.table()
    # 
    # set12 <- twoWordDT2[prefix==v |prefix==t |prefix==u, c("word","prob23") ] %>%
    #     mutate(prob=Lambda2 * Lambda5 / 3.0 * prob23 ) %>% select(word,prob) %>% as.data.table() %>%
    #     funion(
    #         twoWordDT2[ v,c("word","prob21") ] %>%
    #             mutate(prob=Lambda2* (1 - Lambda5) * prob21 )%>% select(word,prob) %>% as.data.table(),
    #         all = TRUE
    #     ) %>% group_by(word) %>% summarize(prob=sum(prob)) %>% top_n(cntRecord,prob) %>% as.data.table()
    # # %>% arrange(desc(prob))
    # set13 <- oneWordDT2[,c("word","prob1")]  %>% 
    #     mutate(prob=Lambda3* prob1 ) %>% select(word,prob) %>% top_n(cntRecord,prob) %>% as.data.table()

    set11 <-  
            threeWordDT2[ uv,c("word","prob31") ] %>% mutate(prob=Lambda1*  prob31 )  %>% 
                select(word,prob) %>% as.data.table()  %>%
          filter(substr(word,1,str_length(w)) == w ) %>%
          filter( word != t &word != u & word != v) %>%
         group_by(word) %>% summarize(prob=sum(prob)) %>% top_n(cntRecord,prob) %>% as.data.table()
    
    set12 <- 
            twoWordDT2[ v,c("word","prob21") ] %>%
                mutate(prob=Lambda2 * prob21 )%>% select(word,prob) %>% as.data.table()  %>% 
      filter(substr(word,1,str_length(w)) == w ) %>%
      filter( word != t &word != u & word != v) %>%
      
      group_by(word) %>% summarize(prob=sum(prob)) %>% top_n(cntRecord,prob) %>% as.data.table()
    # %>% arrange(desc(prob))
    set13 <- oneWordDT2[,c("word","prob1")]  %>% 
        mutate(prob=Lambda3* prob1 ) %>% select(word,prob) %>% 
      filter(substr(word,1,str_length(w)) == w ) %>%
      filter( word != t &word != u & word != v) %>%
      top_n(cntRecord,prob) %>% as.data.table()
    
    if (wDiscount == TRUE) {
      set14 <- as.data.table(setdiff(set12$word,set11$word))
      colnames(set14)<-"word"
      set14$prob <- unlist(lapply(set14$word,getDef3,u,v,uv)) * Lambda1
      set15 <- as.data.table(setdiff(set13$word,set12$word))
      colnames(set15)<-"word"
      set15$prob <- unlist(lapply(set15$word,getDef2,u,v)) * Lambda2
      abc <- set11 %>% funion(set12) %>% funion(set13) %>% funion(set14) %>% funion(set15) %>% 
          group_by(word) %>% summarize(prob=sum(prob)) %>% 
          top_n(cntRecord,prob) %>% arrange(desc(prob)) %>% as.data.table()
    }else {
      abc <- set11 %>% funion(set12) %>% funion(set13) %>% 
      group_by(word) %>% summarize(prob=sum(prob)) %>% 
      top_n(cntRecord,prob) %>% arrange(desc(prob)) %>% as.data.table()
    }
}
    abc
  
    
} 

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$predictedWords <- renderTable({
        
        context <- paste("STOP STOP STOP ", input$Phrase)
        #context = "come bring home"
        cntRecord <- input$TopN
        wDiscount <- input$Discounting
        wSimple <- input$Simple
       abc <- predNextBkt(context = context, cntRecord = cntRecord,
                          wDiscount = wDiscount, wSimple = wSimple)
       abc
    },digits=5)

})
# miss11 <- setdiff(set13$word,set11$word)
# set14 <- twoWordDT2[(prefix==v |prefix==t |prefix==u) & word %in% miss11, c("word","totProb33","totProb23") ] %>%
#     mutate(prob3=Lambda1 * Lambda4 / 3.0 * totProb33,prob2=Lambda1 * Lambda4 / 3.0 * totProb23 )  %>% 
#     select(word,prob3,prob2) %>% as.data.table() %>%
#     funion(
#         twoWordDT2[ prefix == v & word %in% miss11,c("word","totProb31","totProb21") ] %>%
#             mutate(prob3=Lambda1 * (1 - Lambda4)  * totProb31,prob2=Lambda1 * (1 - Lambda4)  * totProb21 ) %>% 
#             select(word,prob3,prob2) %>% as.data.table(),
#         all = TRUE
#     ) %>% group_by(word) %>% summarize(prob3=sum(prob3),prob2=sum(prob2)) %>% as.data.table()
# 
# 
#     Lambda1* Lambda4 / 3.0 * threeWordDT2[ tv]
# $prob33
#     (ifNAxy( , 
#     ) +Lambda1* Lambda4 / 3.0 * 
#     (ifNAxy( threeWordDT2[ tu]$prob33, 
#     ) +
#     Lambda1 * (1 - Lambda4 ) * 
#     (ifNAxy( threeWordDT2[ uvw]$prob31, 
#     ) +    
#     Lambda2 * Lambda5 / 3.0 * 
#     (ifNAxy(twoWordDT2[vw]$prob23, 
#     ) +  Lambda2 * Lambda5 / 3.0 * 
#     (ifNAxy(twoWordDT2[tw]$prob23, 
#     ) +  Lambda2 * Lambda5 / 3.0 * 
#     (ifNAxy(twoWordDT2[uw]$prob23, 
#     ) +   
#     Lambda2 * (1 - Lambda5 ) * 
#     (ifNAxy(twoWordDT2[vw]$prob21, 
#             replNA(  oneWordDT2[v]$totProb21 * oneWordDT2[w]$prob1  / oneWordDT2[v]$totProb11)) 
#     ) + 
#     Lambda3 * (ifNAxy(max(oneWordDT1[w]$prob1,oneWordDef/length(oneWordDT2$name)), 
# 
#     
#     replNA( twoWordDT2[uv]$totProb33 *  twoWordDT2[vw]$prob23 / twoWordDT2[uv]$totProb23) )
#     replNA( twoWordDT2[tv]$totProb33 *  twoWordDT2[vw]$prob23 / twoWordDT2[tv]$totProb23) )
#     replNA( twoWordDT2[tu]$totProb33 *  twoWordDT2[uw]$prob23 / twoWordDT2[tu]$totProb23) )
#     replNA( twoWordDT2[uv]$totProb31 *  twoWordDT2[vw]$prob21 / twoWordDT2[uv]$totProb21) )
#     replNA(  oneWordDT2[v]$totProb23 * oneWordDT2[w]$prob1  / oneWordDT2[v]$totProb13)) 
#     replNA(  oneWordDT2[t]$totProb23 * oneWordDT2[w]$prob1  / oneWordDT2[t]$totProb13)) 
#     replNA(  oneWordDT2[u]$totProb23 * oneWordDT2[w]$prob1  / oneWordDT2[u]$totProb13))
#     replNA( oneWordDef/length(oneWordDT2$name)))  )       
