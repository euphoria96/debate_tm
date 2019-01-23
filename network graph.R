# rm(list=ls()); gc()

library(stringr)
library(tm)
library(topicmodels)
# Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre-10.0.1')
library(KoNLP)
library(NIADic)
useNIADic()
#library(rJava)
#install.packages("NIADic", dependencies=TRUE)
# setwd("C:/Users/korea/Desktop/Ayeon/TM")

# data <- read.csv("dt_cha.csv", head=TRUE)
data = dd

data$Body <- as.character(data$Body)
data$Body <- removeNumbers(data$Body)

dic_f <- function(dic_1, dic_2, data) {
  result<-data;
  for(i in 1:length(dic_1)) {
    result <- gsub(dic_1[i], dic_2[i], result);
  }
  return(result);
} 
dic_1 <- c("문 후보","문후보","문 의원","문의원",
           "홍 후보","홍후보","홍 의원","홍의원",
           "심 후보","심후보","심 의원","심의원",
           "유 후보","유후보","유 의원","유의원",
           "안 후보","안후보","안 의원","안의원",
           "전 대통령","전대통령",
           "dj",
           "mb",
           "518",
           "r&d", "rnd", "알엔디")

dic_2 <- c(rep("문재인",4), rep("홍준표",4), rep("심상정",4),
           rep("유승민",4), rep("안철수",4), rep("박근혜",2),
           "김대중", "이명박", "오일팔", "알앤디")

data$Body <- dic_f(dic_1, dic_2, data$Body)

data$Body <- gsub('\\w*(그것|그거|이것|이거|저것|저거|때문|하지|말씀|우리|하게|번째|대통령|생각|
                  부분|이야기|하시|그걸|이번|들이|이걸|사실|얘기|입장|해서|
                  질문|다음|확인|후보|대한|민국|그때|거기|당시)\\w*', '', data$Body)

data$Body <- removePunctuation(data$Body)
####################
a <- extractNoun(data$Body)
Cps <- Corpus(VectorSource(a))
Cps <- tm_map(Cps, removeWords, c('character'))
Tdm <- TermDocumentMatrix(Cps)
Tdm = Tdm[which(!grepl('않습니|있습니',Tdm$dimnames$Terms)),]
inspect(bb)

### wordcloud
termFq = rowSums(as.matrix(Tdm))
termFq = sort(termFq, decreasing = T)
termFq = subset(termFq, termFq>15)
df <- data.frame(term = names(termFq), freq = termFq)
library(wordcloud2)
library(RColorBrewer)
wordcloud2(df, minRotation=0, maxRotation=0, shape='circle', ellipticity=0.50,
           color='random-light', fontFamily='나눔고딕', shuffle=F)
########################
aa=table.prop(dd[,3],dd[,6])
library(igraph)
peer = graph.data.frame(aa, directed = T)
V(peer)$name
set.seed(1234)
plot(peer,
     layout=layout.fruchterman.reingold, 
     vertex.size = 35,                 
     vertex.shape = "circle", 
     vertex.frame.color="skyblue2", 
     vertex.color = "skyblue", 
     
     vertex.label.dist = 0,          
     vertex.label = V(peer)$name,
     vertex.label.cex = 0.7,           
     vertex.label.color="black",  
     
     edge.color="red",  
     edge.lty ="solid",
     
     edge.arrow.width=1.0, 
     edge.arrow.size=0.3
)
table(ft)
ft = subset(dd,select=c('From','To'))
mat = ft %>% as.vector %>% unique
vec = c(96,85,79,91,102,81,106,141,109,153,148,78,81,41,162,89,91,21,11,16,18,19,11,37,19,11,19,11,21,12,1,1,1,1,1)
mat = as.matrix(cbind(mat, vec))
a = graph.edgelist(mat[,1:2])
E(a)$weight = as.numeric(mat[,3])
a.diag = rep(0,36) + 5
a.name = union(dd$From, dd$To)
set.seed(123456)
plot(a,
     vertex.size = 30,
     vertex.shape = "circle", 
     vertex.frame.color="skyblue2", 
     vertex.color = "skyblue", 
     
     vertex.label.dist = 0,          
     vertex.label = a.name,
     vertex.label.cex = 1,           
     vertex.label.color="black",  
     
     edge.color=ifelse(E(a)$weight/50>=2,'red','darkblue'),  
     edge.lty ="solid",
     edge.width = E(a)$weight/50,
     
     edge.arrow.width=E(a)$weight/50, 
     edge.arrow.size=0.3,
     
     edge.label = vec,
     edge.label.cex = 0.6,
     edge.label.color = 'black'
     
)


ft = subset(dd,dd$index==1, select=c('From','To'))
mat = ft %>% as.vector %>% unique
vec = c(20,12,8,11,19,8,31,20,29,21,19,17,16,14,19,13,15,8,4,10,10,7,4,13,8,4,8,4,9,4)
mat = as.matrix(cbind(mat, vec))
a = graph.edgelist(mat[,1:2])
E(a)$weight = as.numeric(mat[,3])
a.name = union(dd$From, dd$To)
set.seed(123456)
plot(a,
     vertex.size = 30,
     vertex.shape = "circle", 
     vertex.frame.color="skyblue2", 
     vertex.color = "skyblue", 
     
     vertex.label.dist = 0,          
     vertex.label = a.name,
     vertex.label.cex = 1,           
     vertex.label.color="black",  
     
     edge.color=ifelse(E(a)$weight/50>=1,'red','darkblue'),  
     edge.lty ="solid",
     edge.width = E(a)$weight/50,
     
     edge.arrow.width=E(a)$weight/50, 
     edge.arrow.size=0.3,
     
     edge.label = vec,
     edge.label.cex = 0.6,
     edge.label.color = 'black'
)

ft = subset(dd,dd$index==2, select=c('From','To'))
mat = ft %>% as.vector %>% unique
table(ft)
vec = c(4,2,4,59,38,34,17,18,54,51,6,4,2,22,20,35,36,25,22,4,2,4,2,7,7,3,2,16,16,4,1)
mat = as.matrix(cbind(mat, vec))
a = graph.edgelist(mat[,1:2])
E(a)$weight = as.numeric(mat[,3])
a.name = union(dd$From, dd$To)
set.seed(123456)
plot(a,
     vertex.size = 30,
     vertex.shape = "circle", 
     vertex.frame.color="skyblue2", 
     vertex.color = "skyblue", 
     
     vertex.label.dist = 0,          
     vertex.label = a.name,
     vertex.label.cex = 1,           
     vertex.label.color="black",  
     
     edge.color=ifelse(E(a)$weight/50>=1,'red','darkblue'),  
     edge.lty ="solid",
     edge.width = E(a)$weight/50,
     
     edge.arrow.width=E(a)$weight/50, 
     edge.arrow.size=0.5,
     
     edge.label = vec,
     edge.label.cex = 0.6,
     edge.label.color = 'black'
)

ft = subset(dd,dd$index==3, select=c('From','To'))
mat = ft %>% as.vector %>% unique
table(ft)
vec = c(4,2,4,59,38,34,17,18,54,51,6,4,2,22,20,35,36,25,22,4,2,4,2,7,7,3,2,16,16,4,1)
mat = as.matrix(cbind(mat, vec))
a = graph.edgelist(mat[,1:2])
E(a)$weight = as.numeric(mat[,3])
a.name = union(dd$From, dd$To)
set.seed(123456)
plot(a,
     vertex.size = 30,
     vertex.shape = "circle", 
     vertex.frame.color="skyblue2", 
     vertex.color = "skyblue", 
     
     vertex.label.dist = 0,          
     vertex.label = a.name,
     vertex.label.cex = 1,           
     vertex.label.color="black",  
     
     edge.color=ifelse(E(a)$weight/50>=1,'red','darkblue'),  
     edge.lty ="solid",
     edge.width = E(a)$weight/50,
     
     edge.arrow.width=E(a)$weight/50, 
     edge.arrow.size=0.5,
     
     edge.label = vec,
     edge.label.cex = 0.6,
     edge.label.color = 'black'
)

ft = subset(dd,dd$index==4, select=c('From','To'))
mat = ft %>% as.vector %>% unique
table(ft)
vec = c(4,2,4,59,38,34,17,18,54,51,6,4,2,22,20,35,36,25,22,4,2,4,2,7,7,3,2,16,16,4,1)
mat = as.matrix(cbind(mat, vec))
a = graph.edgelist(mat[,1:2])
E(a)$weight = as.numeric(mat[,3])
a.name = union(dd$From, dd$To)
set.seed(123456)
plot(a,
     vertex.size = 30,
     vertex.shape = "circle", 
     vertex.frame.color="skyblue2", 
     vertex.color = "skyblue", 
     
     vertex.label.dist = 0,          
     vertex.label = a.name,
     vertex.label.cex = 1,           
     vertex.label.color="black",  
     
     edge.color=ifelse(E(a)$weight/50>=1,'red','darkblue'),  
     edge.lty ="solid",
     edge.width = E(a)$weight/50,
     
     edge.arrow.width=E(a)$weight/50, 
     edge.arrow.size=0.5,
     
     edge.label = vec,
     edge.label.cex = 0.6,
     edge.label.color = 'black'
)