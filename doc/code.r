data<-data %>%
  mutate(quality=ifelse(quality<6,-1,1))

set.seed(0)
test.idx<-sample(nrow(data),size =round(nrow(data)*0.3) ,replace = FALSE)
test<-data[test.idx,]
train<-data[-test.idx,]

n<-nrow(train)
weight<-rep(1/n,n)
library("rpart")
library("rpart.plot")



models<-list()
Alpham<-c()
B<-100
for (i in 1:B) {
  stump<-rpart(quality~.,data = train,weights = weight,method = "class",control = list(maxdepth=1),parms = list(split="gini"))
  models[[i]]<-stump
  pre<-c()
  pre<-predict(stump,newdata=train,type = "class")
  pre<-as.character(pre)
  pre<-as.numeric(pre)
  #cla<-rep(-1,n)
  #index1<-which(pre==1)
  #cla[index1]<-1
  #err<-rep(0,n)
  #index2<-which(cla!=train$y)
  #err[index2]<-1
  tf<-ifelse(pre!=train$quality,1,0)
  errm<-as.numeric(sum(weight*tf)/sum(weight))
  alpham<-log((1-errm)/errm)
  weight<-weight*exp(alpham*tf)
  Alpham[i]<-alpham
}






n.test<-nrow(test)
aa<-matrix(0,nrow = n.test,ncol = B)
for (i in 1:B) {
  preee<-predict(models[[i]],newdata=test,type = "class")
  preee<-as.character(preee)
  preee<-as.numeric(preee)
  #claaa<-rep(-1,n.test)
  #index111<-which(preee==2)
  #claaa[index111]<-1
  aa[,i]<-preee
}



testc<-c()

for (j in 1:B) {
  bb<-matrix(0,nrow = n.test, ncol = j)
  

  for (i in 1:j) {
    bb[,i]<-Alpham[i]*aa[,i]
  }
  
  cc<-rowSums(bb)
  check<-ifelse(cc>0,1,-1)
  
  testc[j]<-sum(check!=test$quality)/n.test
}






library(ggplot2)

q<-ggplot(data = data.frame(b=1:B,e=testc),mapping = aes(x=b,y=e))+
  geom_point(size=1)+
  geom_smooth(se=FALSE)
q


b<-1:B
plot(testc~b)

