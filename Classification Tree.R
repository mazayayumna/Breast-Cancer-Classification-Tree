library(rpart) #classification and regression trees
library(partykit) #treeplots
library(MASS) #breast and pima indiandata
install.packages("randomForest")
library(randomForest) #random forests
install.packages("gbm")
library(gbm) #gradient boosting
install.packages("caret")
library(caret) #tune hyper-parameters

bcancer=read.table("D:\\KULIAHAN\\sem 7\\DSS\\Project 1\\breastcancer.txt",header=T)
head(bcancer)
str(bcancer)
summary(bcancer)
set.seed(123)
bcanc = sample(2, nrow(bcancer), replace=TRUE, prob=c(0.7,0.3))
trainData = bcancer[bcanc==1,]
testData = bcancer[bcanc==2,]
str(trainData[,1])
set.seed(123)
tree.biop= rpart(diagnosis~., data=trainData)
print(tree.biop$cptable)
cp= min(tree.biop$cptable[3,])
prune.tree.biop= prune(tree.biop, cp= cp)
plot(as.party(tree.biop))
plot(as.party(prune.tree.biop))
rparty.test= predict(prune.tree.biop, newdata=testData, type="class")
table(rparty.test, testData$diagnosis)
(85+67)/164
(67/(85+67))	#TPR
(3/(9+3))	#FPR
library(pROC)
roc_df <- data.frame(
	TPR=(67/(85+67)), #TPR
	FPR=(3/(9+63))) #FPR
rectangle <- function(x, y, width, height, density=12, angle=-45, ...) 
  polygon(c(x,x,x+width,x+width), c(y,y+height,y+height,y), 
          density=density, angle=angle, ...)
roc_df <- transform(roc_df, 
  dFPR = c(diff(FPR), 0),
  dTPR = c(diff(TPR), 0))
plot(0:10/10, 0:10/10, type='n', xlab="FPR", ylab="TPR")
abline(h=0:10/10, col="lightblue")
abline(v=0:10/10, col="lightblue")
with(roc_df, {
  mapply(rectangle, x=FPR, y=0,   
         width=dFPR, height=TPR, col="green", lwd=2)
  mapply(rectangle, x=FPR, y=TPR, 
         width=dFPR, height=dTPR, col="blue", lwd=2)
  lines(FPR, TPR, type='b', lwd=3, col="red")
})