#data
library(readxl)
library(rpart, rpart.plot)
library(ggplot2)
library(caret)
library(randomForest)
library(klaR)
library(forecast)
data.df <- read.csv("C:/Users/ahmad/Desktop/dahak.csv", header = TRUE)
dim(data.df)
data.df<-data.df[,-1]
dim(data.df)
Quantile=quantile(data.df$Target,probs=seq(0,1,1/10))
#print(Quantile[10])
dahakakhar = which(data.df$Target>Quantile[10])
X.1 = rep(0,length(data$Target))
X.1 = replace(X.1,dahakakhar,1)
data.df<-cbind(data.df,X.1=X.1)
table(X.1)
View(data.df)
summary(data.df)

dataa.df=data.df[,-c(2,3,39,68)]
dim(dataa.df)    
View(dataa.df)

ggplot(dataa.df) + 
  geom_bar( aes(x = borodatmarkazi  , fill = as.factor(X.1) ), position = "dodge2" )+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 5))

ggplot(dataa.df) + 
  geom_boxplot( aes( x = SarparstSen, y = as.factor(X.1) )   ) + xlab("SarparstSen")
 
ggplot(dataa.df) + 
  geom_bar( aes(x = NoeKhanevar  , fill = as.factor(X.1) ), position = "dodge2" )+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 5))

ggplot(dataa.df) + 
  geom_bar( aes(x = Ostan  , fill = as.factor(X.1) ), position = "fill" )+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 5))
ggplot(dataa.df) + 
  geom_bar( aes(x = lolekeshiab   , fill = as.factor(X.1) ), position = "fill" )+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 5))
ggplot(dataa.df) + 
  geom_bar( aes(x = coolerabisabet   , fill = as.factor(X.1) ), position = "fill" )+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 5))
ggplot(dataa.df) + 
  geom_bar( aes(x = hamam   , fill = as.factor(X.1) ), position = "fill" )+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 5))
ggplot(dataa.df) + 
  geom_bar( aes(x = TedadAza   , fill = as.factor(X.1) ), position = "fill" )+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 5))
ggplot(dataa.df) + 
  geom_bar( aes(x = SarparstJensiat   , fill = as.factor(X.1) ), position = "fill" )+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 5))
ggplot(dataa.df) + 
  geom_bar( aes(x = SarparstSen   , fill = as.factor(X.1) ), position = "fill" )+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 5))
ggplot(dataa.df) + 
  geom_bar( aes(x = SarparstSavad   , fill = as.factor(X.1) ), position = "fill" )+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 5))
ggplot(dataa.df) + 
  geom_bar( aes(x = SarparstTahsil   , fill = as.factor(X.1) ), position = "fill" )+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 5))
ggplot(dataa.df) + 
  geom_bar( aes(x = SarparstMadrak   , fill = as.factor(X.1) ), position = "fill" )+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 5))

ggplot(dataa.df) + 
  geom_bar( aes(x = SarparstFaaliat   , fill = as.factor(X.1) ), position = "fill" )+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 5))
ggplot(dataa.df) + 
  geom_bar( aes(x = SarparstZanashoyi   , fill = as.factor(X.1) ), position = "fill" )+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 5))
ggplot(dataa.df) + 
  geom_bar( aes(x = NahveTasarof   , fill = as.factor(X.1) ), position = "fill" )+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 5))
ggplot(dataa.df) + 
  geom_bar( aes(x = TedadOtagh   , fill = as.factor(X.1) ), position = "fill" )+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 5))
ggplot(dataa.df) + 
  geom_bar( aes(x = SatheZirbana   , fill = as.factor(X.1) ), position = "fill" )+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 5))
ggplot(dataa.df) + 
  geom_bar( aes(x = NoeEskelet   , fill = as.factor(X.1) ), position = "fill" )+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 5))
ggplot(dataa.df) + 
  geom_bar( aes(x = mashin   , fill = as.factor(X.1) ), position = "fill" )+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 5))
ggplot(dataa.df) + 
  geom_bar( aes(x = motor   , fill = as.factor(X.1) ), position = "fill" )+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 5))
ggplot(dataa.df) + 
  geom_bar( aes(x = docharkhe   , fill = as.factor(X.1) ), position = "fill" )+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 5))
ggplot(dataa.df) + 
  geom_bar( aes(x = radio   , fill = as.factor(X.1) ), position = "fill" )+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 5))
ggplot(dataa.df) + 
  geom_bar( aes(x = radiozabt   , fill = as.factor(X.1) ), position = "dodge2" )+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 5))
ggplot(dataa.df) + 
  geom_bar( aes(x = tv   , fill = as.factor(X.1) ), position = "fill" )+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 5))
ggplot(dataa.df) + 
  geom_bar( aes(x = tvrangi   , fill = as.factor(X.1) ), position = "fill" )+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 5))
ggplot(dataa.df) + 
  geom_bar( aes(x = video   , fill = as.factor(X.1) ), position = "fill" )+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 5))
ggplot(dataa.df) + 
  geom_bar( aes(x = computer   , fill = as.factor(X.1) ), position = "fill" )+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 5))
ggplot(dataa.df) + 
  geom_bar( aes(x = mobile   , fill = as.factor(X.1) ), position = "fill" )+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 5))
ggplot(dataa.df) + 
  geom_bar( aes(x = freezer   , fill = as.factor(X.1) ), position = "fill" )+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 5))
ggplot(dataa.df) + 
  geom_bar( aes(x = yakhchal   , fill = as.factor(X.1) ), position = "fill" )+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 5))
ggplot(dataa.df) + 
  geom_bar( aes(x = yakhchalfreezer   , fill = as.factor(X.1) ), position = "fill" )+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 5))
ggplot(dataa.df) + 
  geom_bar( aes(x = ojaghgaz   , fill = as.factor(X.1) ), position = "dodge2" )+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 5))
ggplot(dataa.df) + 
  geom_bar( aes(x = jarobarghi   , fill = as.factor(X.1) ), position = "fill" )+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 5))
ggplot(dataa.df) + 
  geom_bar( aes(x = lebasshoyi   , fill = as.factor(X.1) ), position = "fill" )+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 5))
ggplot(dataa.df) + 
  geom_bar( aes(x = khayati   , fill = as.factor(X.1) ), position = "fill" )+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 5))
ggplot(dataa.df) + 
  geom_bar( aes(x = panke   , fill = as.factor(X.1) ), position = "fill" )+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 5))
ggplot(dataa.df) + 
  geom_bar( aes(x = coolerabimoteharek   , fill = as.factor(X.1) ), position = "fill" )+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 5))
ggplot(dataa.df) + 
  geom_bar( aes(x = coolergazimoteharek   , fill = as.factor(X.1) ), position = "fill" )+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 5))
ggplot(dataa.df) + 
  geom_bar( aes(x = zarfshoyi   , fill = as.factor(X.1) ), position = "fill" )+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 5))
ggplot(dataa.df) + 
  geom_bar( aes(x = microfer   , fill = as.factor(X.1) ), position = "fill" )+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 5))
ggplot(dataa.df) + 
  geom_bar( aes(x = lolekeshiab   , fill = as.factor(X.1) ), position = "fill" )+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 5))
ggplot(dataa.df) + 
  geom_bar( aes(x = bargh   , fill = as.factor(X.1) ), position = "fill" )+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 5))
ggplot(dataa.df) + 
  geom_bar( aes(x = gazlolekeshi   , fill = as.factor(X.1) ), position = "fill" )+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 5))

ggplot(dataa.df) + 
  geom_bar( aes(x = telephone   , fill = as.factor(X.1) ), position = "fill" )+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 5))
ggplot(dataa.df) + 
  geom_bar( aes(x = internet   , fill = as.factor(X.1) ), position = "fill" )+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 5))

ggplot(dataa.df) + 
  geom_bar( aes(x = ashpazkhane   , fill = as.factor(X.1) ), position = "fill" )+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 5))

ggplot(dataa.df) + 
  geom_bar( aes(x = coolerabisabet   , fill = as.factor(X.1) ), position = "fill" )+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 5))

ggplot(dataa.df) + 
  geom_bar( aes(x = borodatmarkazi   , fill = as.factor(X.1) ), position = "fill" )+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 5))

ggplot(dataa.df) + 
  geom_bar( aes(x = hararatmarkazi   , fill = as.factor(X.1) ), position = "fill" )+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 5))

ggplot(dataa.df) + 
  geom_bar( aes(x = package   , fill = as.factor(X.1) ), position = "dodge2" )+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 5))

ggplot(dataa.df) + 
  geom_bar( aes(x = coolergazisabet   , fill = as.factor(X.1) ), position = "fill" )+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 5))

ggplot(dataa.df) + 
  geom_bar( aes(x = fazelabshahri   , fill = as.factor(X.1) ), position = "fill" )+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 5))

ggplot(dataa.df) + 
  geom_bar( aes(x = nsokhtpokhtpaz   , fill = as.factor(X.1) ), position = "fill" )+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 5))

ggplot(dataa.df) + 
  geom_bar( aes(x = nsokhtgarma   , fill = as.factor(X.1) ), position = "fill" )+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 5))

ggplot(dataa.df) + 
  geom_bar( aes(x = nsokhtabgarm   , fill = as.factor(X.1) ), position = "fill" )+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 5))

#ggplot(dataa.df) + 
#  geom_bar( aes(x = HazineKhoraki   , fill = as.factor(X.1) ), position = "fill" )+
#  theme(axis.text.x = element_text(angle = 90))+
#  theme(axis.text.x = element_text(size = 5))
dim(dataa.df)
dataa.df=dataa.df[,-c(46,38)]
library(reshape) 
cormat <- round(cor(dataa.df[,51:62]),2)
library(reshape2)
melted_cormat <- melt(cormat)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+
  theme(axis.text.x = element_text(angle = 90))
ggplot(dataa.df) + 
  geom_boxplot( aes( x = nsokhtabgarm, y = as.factor(X.1) )   ) + xlab("nsokhtabgarm")

#dataa.df$Ostan<-relevel(dataa.df$Ostan , ref = "fars")

dataa.df<-na.omit(dataa.df)
View(dataa.df)
dim(dataa.df)



fit.glm <- glm(as.numeric(X.1)~. , data = dataa.df, family = "binomial")
summary(fit.glm)
#rf <- rpart(as.factor(X.1) ~ ., data = dataa.df)
#summary(rf)
dataa.df=dataa.df[,-c(2,17,19,20,28,33,37,41,45)]
fit.glm <- glm(as.numeric(X.1)~. , data = dataa.df, family = "binomial")
summary(fit.glm)
dataa.df=dataa.df[,-c(2,14,24,35,36,39,43,52)]
fit.glm <- glm(as.numeric(X.1)~. , data = dataa.df, family = "binomial")
summary(fit.glm)
rf <- rpart(as.factor(X.1) ~ ., data = dataa.df)
summary(rf)
table(dataa.df$borodatmarkazi)
dim(dataa.df)
rowSums(is.na(dataa.df))
summary(dataa.df)
View(dataa.df)
head(dataa.df)