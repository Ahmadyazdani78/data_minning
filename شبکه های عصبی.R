library(neuralnet)
train.df.nn<-train.df
valid.df.nn<-valid.df

norm.values<-preProcess(train.df[,36:45],method = c("range"))
train.df.nn[,36:45]<-predict(norm.values,train.df[,36:45])
valid.df.nn[,36:45]<-predict(norm.values,valid.df[,36:45])
n <- names(train.df[,-1])
f <- as.formula(paste("train.df$X.1 ~", paste(n[!n %in% "X.1"], collapse = " + ")))
nn<-neuralnet(f , data = train.df.nn[,-1],linear.output = F,hidden = 5)
prednn<- as.factor(ifelse(predict(nn , valid.df.nn[,-1], type="response")>0.43,1,0))
confusionMatrix(prednn,as.factor(valid.df.nn$X.1))
pred_train <- as.factor(ifelse(predict(nn , train.df.nn[,-1], type="response")>0.43,1,0))
confusionMatrix(pred_train  ,as.factor(train.df.nn$X.1) )
plot(nn)
