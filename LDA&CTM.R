## library
library(stringr)
library(KoNLP)
library(tm)
library(rJava)
## data all -------
cps = Corpus(VectorSource(dd$Body))
inspect(cps[1:5])
Noun_cps = tm_map(cps, removeNumbers)
Noun_cps = tm_map(Noun_cps, extractNoun)
# dic = c('ê·¸ê²ƒ','?•Œë¬?','ê·¸ê±°','?•˜ì§€','ë§ì?€','?š°ë¦?','?•˜ê²?','ë²ˆì§¸','?‚¬?‹¤','?–˜ê¸?','?…?¥','ì§ˆë¬¸','?´ê²?','?‹¤?Œ','?™•?¸')
# dic2= c('?›„ë³?','??€?•œ','ë¯¼êµ­')
Noun_cps = tm_map(Noun_cps, removeWords, dic)
Noun_cps = tm_map(Noun_cps, removeWords, dic2)
cpstdm = TermDocumentMatrix(Noun_cps)
inspect(cpstdm)
inspect(removeSparseTerms(cpstdm, sparse = 0.98)) # sparsity(?¬ë°•ì„±)
findFreqTerms(cpstdm, lowfreq = 50)
findAssocs(cpstdm, "?•ˆë³?", 0.3)
### LDA & CTM
# all
cpsdtm = DocumentTermMatrix(Noun_cps)
dim(cpsdtm)
library(topicmodels)
cpsdtm = cpsdtm[apply(cpsdtm,1,sum)!=0, ]
lda.out = LDA(cpsdtm, control = list(seed = 123), k = 5)
dim(lda.out@gamma)
dim(lda.out@beta)
terms(lda.out, 12)
ctm.m = CTM(cpsdtm, control = list(seed = 123), k = 3)
terms(ctm.m, 12)
# 1
cps1 = Corpus(VectorSource(data1$Body))
Noun_cps1 = tm_map(cps1, removeNumbers)
Noun_cps1 = tm_map(Noun_cps1, extractNoun)
cpsdtm1 = DocumentTermMatrix(Noun_cps1)
dim(cpsdtm1)
cpsdtm1 = cpsdtm1[apply(cpsdtm1,1,sum)!=0, ]

lda.out = LDA(cpsdtm1, control = list(seed = 123), k = 5)
terms(lda.out, 12)
ctm.m1 = CTM(cpsdtm1, control = list(seed = 123), k = 5)
terms(ctm.m1, 12)
# 2
cps2 = Corpus(VectorSource(data2$Body))
Noun_cps2 = tm_map(cps2, removeNumbers)
Noun_cps2 = tm_map(Noun_cps2, extractNoun)
cpsdtm2 = DocumentTermMatrix(Noun_cps2)
cpsdtm2 = cpsdtm2[apply(cpsdtm2,1,sum)!=0, ]
ctm.m2 = CTM(cpsdtm2, control = list(seed = 123), k = 5)
terms(ctm.m2, 12)
lda.out2 = LDA(cpsdtm2, control = list(seed = 123), k = 5)
terms(lda.out2, 12)
####
useNIADic()
### data all
a = removeNumbers(dd$Body)
a = gsub('\\w*(ê·¸ê²ƒ|?•Œë¬?|ê·¸ê±°|?•˜×Ù€|×ºì?€|?š°ë¦?|?•˜ê²?|ë²ˆì§¸|??€?†µ? ¹|?ƒê°?|ë¶€ë¶?|?´?•¼ê¸?|?•˜?‹œ|ê·¸ê±¸|?´ë²?|?“¤?´|??€ê²?|?´ê±?|?‚¬?‹¤|?–˜ê¸?|?…?¥|×Ùˆë¬¸|?´ê²?|?‹¤?Œ|?™•?¸|?›„ë³?|??€?•œ|ë¯¼êµ­|ê·¸ë•Œ|ê±°ê¸°|?‹¹?‹œ)\\w*', '', a);
aa = extractNoun(a)
cps = Corpus(VectorSource(aa))
Noun_cps = tm_map(cps, removeWords, c('character'))
cpsdtm = DocumentTermMatrix(Noun_cps)
cpsdtm = cpsdtm[apply(cpsdtm,1,sum)!=0, ]

lda.out = LDA(cpsdtm, control = list(seed = 123), k = 5)
terms(lda.out, 12)
#####
gg = function(x){
  return(gsub('\\w*(ê·¸ê²ƒ|?•Œë¬?|ê·¸ê±°|?•˜×Ù€|×ºì?€|?š°ë¦?|?•˜ê²?|ë²ˆì§¸|??€?†µ? ¹|?ƒê°?|ë¶€ë¶?|?´?•¼ê¸?|?•˜?‹œ|ê·¸ê±¸|?´ë²?|?“¤?´|??€ê²?|?´ê±?|?‚¬?‹¤|?–˜ê¸?|?…?¥|×Ùˆë¬¸|?´ê²?|?‹¤?Œ|?™•?¸|?›„ë³?|??€?•œ|ë¯¼êµ­|ê·¸ë•Œ|ê±°ê¸°|?‹¹?‹œ)\\w*', '', x))
}
lda.sy = function(x){
  y=removeNumbers(x)
  y=gg(y)
  y=extractNoun(y)
  cps = Corpus(VectorSource(y))
  Noun_cps = tm_map(cps, removeWords, c('character'))
  cpsdtm = DocumentTermMatrix(Noun_cps)
  cpsdtm = cpsdtm[apply(cpsdtm,1,sum)!=0, ]
  lda = LDA(cpsdtm, control = list(seed = 100), k = 3)
  return(terms(lda, 12))
}
ctm.sy = function(x){
  y=removeNumbers(x)
  y=gg(y)
  y=extractNoun(y)
  cps = Corpus(VectorSource(y))
  Noun_cps = tm_map(cps, removeWords, c('character'))
  cpsdtm = DocumentTermMatrix(Noun_cps)
  cpsdtm = cpsdtm[apply(cpsdtm,1,sum)!=0, ]
  ctm = CTM(cpsdtm, control = list(seed = 123), k = 3)
  return(terms(ctm, 12))
}
#####
a = removeNumbers(dt$Body)
a1 = removeNumbers(data1$Body)
a2 = removeNumbers(data2$Body)
a3 = removeNumbers(data3$Body)
a4 = removeNumbers(data4$Body)
### 
lda.sy(a4)
### 5.
neg = subset(dt$Body, dt$Section=='negative'|dt$Status=='negative')
lda.sy(neg)
ctm.sy(neg)
### 6.
moon2 = subset(dt$Body, dt$To=='ë¬¸ì¬?¸')
lda.sy(moon2)
ctm.sy(moon2)
