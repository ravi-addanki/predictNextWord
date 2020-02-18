con1 <- file("data/final/en_US/en_US.blogs.txt",open="rb")
blog_lines <- readLines(con1)
close(con1)
con1 <- file("data/final/en_US/en_US.news.txt",open="rb")
news_lines <- readLines(con1)
close(con1)
con1 <- file("data/final/en_US/en_US.twitter.txt",open="rb")
twitter_lines <- readLines(con1)
close(con1)
blogCnt <- length(blog_lines)
newsCnt <- length(news_lines)
twitterCnt <- length(twitter_lines
)
set.seed(2020-02-02)
blogSmp <- sample(1:blogCnt,blogCnt*.02,replace=FALSE)
newsSmp <- sample(1:newsCnt,newsCnt*.02,replace=FALSE)
twitterSmp <- sample(1:twitterCnt,twitterCnt*.02,replace=FALSE)
blog_lines <- blog_lines[-blogSmp]
news_lines <-news_lines[-newsSmp]
#twitter_lines <-twitter_lines[newsSmp]
twitter_lines <-twitter_lines[-twitterSmp]

 rmUnk3 <- function(b1) {
     
     b2 <- unlist(strsplit(b1," "))
     b2 <- b2[b2 != ""]
     paste(b2[unlist(lapply(b2,exists,dEnv))],collapse=" ")
 }
prPr <- function(blog_lines){
    blog_lines <- removeNumbers(blog_lines)
    blog_lines <- stripWhitespace(blog_lines)
    blog_lines <- tolower(blog_lines)
    blog_lines <- gsub(blog_lines, pattern = "\\.", replacement = " STOP ")
    blog_lines <- removePunctuation(blog_lines)
    blog_lines <- stripWhitespace(blog_lines)
    
    for(iIter in 1:5) {
        blog_lines <-gsub(blog_lines, pattern = " [[:alnum:]]{1,2} ", replacement = " ")
        blog_lines <-gsub(blog_lines, pattern = "^[[:alnum:]]{1,2} ", replacement = "")
        blog_lines <-gsub(blog_lines, pattern = " [[:alnum:]]{1,2}$", replacement = "")
    } 
     blogNew <-as.vector(sapply(blog_lines,rmUnk3))
    blogNew <- paste("STOP",blogNew)
    blogNew
}
# load("dEnv.Rdt")
# load("wordsDT.Rdt")
# dict1 <-oneWordDT[oneWordDT$freq1>200,]$name
# dict1 <- union(dict1,"STOP")
# names(dict1)<-dict1
# save(list=c("dict1"),file="dict1.Rdt")
# dEnv <-list2env(as.list(dict1))
con1 <- file("data/trainSamp/en_US/blog.txt",open="w")
#con1 <- file("data/DevSamp/en_US/blog.txt",open="w")
blog_lines <- prPr(blog_lines)
writeLines(blog_lines,con1)
close(con1)

con1 <- file("data/trainSamp/en_US/news.txt",open="w")
#con1 <- file("data/DevSamp/en_US/news.txt",open="w")
news_lines <- prPr(news_lines)
writeLines(news_lines,con1)
close(con1)
con1 <- file("data/trainSamp/en_US/twitter.txt",open="w")
#con1 <- file("data/DevSamp/en_US/twitter.txt",open="w")
twitter_lines <- prPr(twitter_lines)
writeLines(twitter_lines,con1)
close(con1)


