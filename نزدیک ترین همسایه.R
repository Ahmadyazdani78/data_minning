library(FNN)
s<-sample(seq_len(nrow(valid.df)), size = floor(0.5*nrow(valid.df)))
valid.df.knn <- valid.df[s,]
test.df.knn<-valid.df[-s,]
#dim(valid.df.knn)
#dim(test.df.knn)
#dim(valid.df)
train.norm.df<-train.df
norm.values<-preProcess(train.df[,36:45],method = c("center","scale"))
train.norm.df[,36:45]<-predict(norm.values,train.df[,36:45])
valid.norm.df<-valid.df.knn
test.norm.df<-test.df.knn
valid.norm.df[,36:45]<-predict(norm.values,valid.df.knn[,36:45])
test.norm.df[,36:45]<-predict(norm.values,test.norm.df[,36:45])
View(train.norm.df)
dim(train.norm.df)
for (i in 1:20){
  knn_train<-knn(train = train.norm.df[,36:45] , test = valid.norm.df[,36:45] , cl=as.factor(train.norm.df$X.1) , k=i)
 print( confusionMatrix(as.factor( knn_train),as.factor(valid.norm.df$X.1)))
  print(i)}
k_n_n<-knn(train = train.norm.df[,36:45] , test = test.norm.df[,36:45] , cl=as.factor(train.norm.df$X.1) , k=7)
print(confusionMatrix(as.factor(k_n_n),as.factor(test.norm.df$X.1)))
