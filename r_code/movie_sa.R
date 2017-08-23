#install.packages("KoNLP")
#install.packages("rJava")
library(KoNLP) 
#useSejongDic()
useNIADic()

#---------------------#file input#---------------------#
#setwd("/Users/hodong/Desktop/review_senti_analysis/r_code/")
mp <- data.frame(read.csv("../raw_data/movie_prepared.csv",header=TRUE))
hm <- data.frame(read.csv("../raw_data/half_m.csv",header=TRUE))
#---------------------#file input#---------------------#


#---------------------#functions#---------------------#
#spliting data into training set & testing set
split_vec <- function(x, ratio){
  return(sample(seq(1, nrow(x),1), round(nrow(x)*ratio,0)))
}

#refining reviews to text&sentiment
sen_divide <- function(x){
  if(x<=6){ return("B") }
  else{ return("G") }
}

#function for extracting words from bad/average/good reviews
ext_noun <- function(x){
  x2 <- (sapply(as.character(x$content), function(x){return(gsub("[^가-힣' ']","",x))}, USE.NAMES=TRUE))
  x3 <- unlist(sapply(x2,extractNoun, USE.NAMES=TRUE))
  x4 <- Filter(function(x){nchar(x)>=2},x3)
  x5 <- head(sort(table(x4),decreasing=T),30)
  return(x5)
}

#function for getting word probability table
get_prob_tab <- function(x,y){ #x : eg. ob_wd, oa_wd, og_wd // y : eg. b_rvs, a_rvs, g_rvs
  bw_prob <- data.frame(matrix(nrow=2,ncol=length(x)))  
  names(bw_prob) <- x
  ttemp <- sapply(as.character(y$content), extractNoun)
  
  for(i in 1:length(x)){
    temp <- sapply(ttemp, function(z){ return(ifelse(length(intersect(z, x[i]))!=0,1,0))  })
    bw_prob[1,i] <- (sum(temp)+1)/(nrow(y)+length(x)) #length(x) is for laplace smoothing
    bw_prob[2,i] <- (nrow(y)-sum(temp)+1)/(nrow(y)+length(x))
  }
  
  return(bw_prob)
}

#get probability of specific class
predict_tab <- function(x,y1,y2,z1,z2){ #x:tdte, y1:gw_prob/gw_prob2, y2:bw_prob/bw_prob2, z1:g_prob, z2:b_porb
  prd_df <- data.frame(matrix(nrow=nrow(tdte), ncol=4))
  names(prd_df) <- c("G","B","predicted","actual")
  for(i in 1:nrow(x)){
    #for(i in 1:10000){
    temp <- extractNoun(x[i,]$content)
    prd_df[i,]$actual <- x[i,]$senti
    prd_df[i,]$G <- prod(y1[t(ifelse(temp %in% names(y1), 1, 2))])*z1
    prd_df[i,]$B <- prod(y2[t(ifelse(temp %in% names(y2), 1, 2))])*z2
    prd_df[i,]$predicted <- names(prd_df)[which(prd_df[i,]==max(prd_df[i,]$G, prd_df[i,]$B, na.rm=TRUE))]
  }
  return(prd_df)
}
#---------------------#functions#---------------------#

#---------------------#data preparing1 - dividing label#---------------------#
hm$senti <- sapply(hm$score, sen_divide)
hm_b <- hm[which(hm$senti=="B"),]
hm_g <- hm[sample(which(hm$senti=="G"), nrow(hm_b)),]

temp <- split_vec(hm_b, 0.8)
hm_b_tr <- hm_b[temp,]
hm_b_te <- hm_b[-temp,]

temp <- split_vec(hm_g, 0.8)
hm_g_tr <- hm_g[temp,]
hm_g_te <- hm_g[-temp,]

tdtr <- data.frame(rbind(hm_b_tr, hm_g_tr))
tdte <- data.frame(rbind(hm_b_te, hm_g_te))

tdte <- tdte[,6:7]
tdtr <- tdtr[,6:7]

b_rvs <- tdtr[which(tdtr$senti=="B"),]
g_rvs <- tdtr[which(tdtr$senti=="G"),]

b_len <- nrow(b_rvs)
g_len <- nrow(g_rvs)
total_len <- nrow(tdtr)

b_prob <- b_len/total_len
g_prob <- g_len/total_len
#---------------------#data preparing1 - dividing label#---------------------#


#---------------------#data preparing2 - extracting words#---------------------#
b_wd <- head(ext_noun(b_rvs),n=100)
g_wd <- head(ext_noun(g_rvs),n=100)
common_wd <- intersect(names(b_wd),names(g_wd))

ob_wd <- setdiff(names(b_wd),common_wd)
og_wd <- setdiff(names(g_wd),common_wd)

bw_prob <- get_prob_tab(names(b_wd), b_rvs)
gw_prob <- get_prob_tab(names(g_wd), g_rvs)

bw_prob2 <- get_prob_tab(ob_wd, b_rvs)
gw_prob2 <- get_prob_tab(og_wd, g_rvs)
#---------------------#data preparing2 - extracting words#---------------------#


#---------------------#Implementing Naive bayesian classifier#---------------------#
prd_df <- predict_tab(tdte, gw_prob, bw_prob, g_prob, b_prob)
prd_df2 <- predict_tab(tdte, gw_prob2, bw_prob2, g_prob, b_prob)

accuracy <- sum(ifelse(as.character(prd_df$actual)==as.character(prd_df$predicted),1,0),na.rm=TRUE)/nrow(tdte)
accuracy2 <- sum(ifelse(as.character(prd_df2$actual)==as.character(prd_df2$predicted),1,0),na.rm=TRUE)/nrow(tdte)

accuracy #SejongDic:?? #NIAdic:0.4988466
accuracy2 #SejongDic:?? #NIAdic:0.4988466

write.csv(prd_df, file="/Users/hodong/Desktop/prd_df.csv",row.names=FALSE)
write.csv(prd_df2, file="/Users/hodong/Desktop/prd_df2.csv",row.names=FALSE)
#---------------------#Implementing Naive bayesian classifier#---------------------#