rm(list=ls())
gdat<-read.csv("german_credit.csv")

head(gdat)
library(bnlearn)
library(igraph)
install.packages("Rgraphviz")
install.packages("Rhugin")
## try http:// if https:// URLs are not supported
source("https://bioconductor.org/biocLite.R")
biocLite("RBGL")
biocLite("Rgraphviz")
require("RBGL")
require("Rgraphviz")
library(Rgraphviz)
library(gRbase)
library(gRain)
library(gRain)
library(RHugin)
library(Rgraphviz)
library(gRbase)
library(ggm)
library(gRim)
library(bnlearn)
library(igraph)


gdat$Duration.of.Credit..month.<-cut(gdat$Duration.of.Credit..month.,breaks=c(0,6,12,18,24,30,36,42,48,54,100),labels=c(10,9,8,7,6,5,4,3,2,1))
gdat$Credit.Amount<-cut(gdat$Credit.Amount,breaks=c(0,500,1000,1500,2500,5000,7500,10000,15000,20000,100000),labels=c(10,9,8,7,6,5,4,3,2,1))
gdat$Age..years.<-cut(gdat$Age..years.,breaks=c(0,25,39,59,64,100),labels=c(1,2,3,5,4))

gdat$Purpose<-as.factor(gdat$Purpose)
gdat[,1:21]<-replace
gdat$Payment.Status.of.Previous.Credit<-

res <- tabu(gdat)  #hill climbing
res_gs<-gs(gdat)#constraint based, dont use
net<-as(amat(res),"graphNEL")
x11()
#plot(net,cex.names=2)
graphviz.plot(res,layout="dot",shape="ellipse")

######
#BLock 1: individual parameters
#BLock 2: individual financial status
#BLock 3: bank criteria
#Block 4: creditability
#####
n<- names(res$nodes)
block <- c(3,2,2,2,1,1,2,1,2,1,2,1,2,1,2,1,2,1,1,1,2)
matrix1<- matrix(0,nrow=21,ncol=21)
rownames(matrix1)<-n
colnames(matrix1)<-n
for (i in 2:4){
  matrix1[block==i,block < i] <- 1
}
blackl<-data.frame(get.edgelist(as(matrix1,"igraph")))
names(blackl)<-c("from","to")
res2<- hc(gdat,blacklist = blackl)
net2<-as(amat(res2),"graphNEL")
x11()
#plot(net,cex.names=2)
graphviz.plot(res2,layout="dot",shape="ellipse")


E(net)
net
x11()
plot(res)
write.csv(gdat,file="germanData.csv")
modelstring(res)
p