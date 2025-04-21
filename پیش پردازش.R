set.seed(1)
s1<-sample(rownames(dataa.df[dataa.df$X.1==1,]), dim(dataa.df[dataa.df$X.1==1,])[1]*0.5)

s2=sample(rownames(dataa.df[dataa.df$X.1==0,]),  dim(dataa.df[dataa.df$X.1==0,])[1]*0.5)
c<-as.numeric( c(s1,s2))
#print(c)

train.rows<-c(s1,s1,s1,s1,s1,s1,s1,s1,s1,s2)

train.df <- dataa.df[train.rows, ]                                                 
valid.df <- dataa.df[-c, ]
dim(valid.df)
View(train.df)
dim(train.df)
dim(valid.df)
summary(train.df)
summary(valid.df)
