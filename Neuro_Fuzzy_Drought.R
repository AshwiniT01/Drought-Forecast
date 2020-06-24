#Agricultural Drought Prediction Using Neuro-Fuzzy Inference System
#Reading dataset
data<-read.csv("bellary_indices_RF.csv")
data[is.na(data)] <- 0


colnames(data)

dm<-subset(data,select=c(SPEI,SPI_1,SPI_2,SPI_3,SPI_6,MAI,OBSERVED))
dmsub <- subset(dm, select = c(SPEI,SPI_1,SPI_2,SPI_3,SPI_6,MAI))
dm
scaleddata<-dmsub
scaleddata <- as.data.frame(scaleddata)
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
maxmindf <- as.data.frame(lapply(scaleddata, normalize))
maxmindf$OBSERVED <- dm$OBSERVED
colnames(maxmindf)

#Splitting data into test and trainset
trainset <- maxmindf[1:42, ]
testset <- maxmindf[43:90, ]
trainset$OBSERVED=as.numeric(trainset$OBSERVED)
testset$OBSERVED=as.numeric(testset$OBSERVED)
library(neuralnet)
nn <- neuralnet(OBSERVED ~ ., data=trainset, hidden=c(3,2),linear.output=FALSE, threshold=0.01)
nn$result.matrix
plot(nn)
temp_test <- subset(testset, select = c("SPEI","SPI_1","SPI_2","SPI_3","SPI_6","MAI"))
head(temp_test)
nn.results <- compute(nn, temp_test)
results <- data.frame(actual = testset$OBSERVED, prediction = nn.results$net.result)
results
class(results) 
r_15 <-results[1:12,]
r_16 <-results[13:24,]
r_17 <-results[25:36,]
r_18 <-results[37:48,]

library(sets)

U1 <- seq(from = 0, to = 1, by = 0.0001)
U2 <- seq(from = 0, to = 100, by = 0.5)
#DIMENSIONS

variables <-
  
  set(
    
    mai =
      fuzzy_partition(varnames= 
                        c( humid = 87.5,agrifavour = 62.5,moderate=37.5,
                           severe = 12.5),
                      FUN= fuzzy_cone, radius = 12.5, universe=U2),
    
    
    predictvalue =
      fuzzy_partition(varnames=
                        c(mild = 0.573, good = 0.133, severe = 0.873),
                      FUN = fuzzy_cone, radius = 0.173, universe=U1),
    
    
    
    drought =
      fuzzy_partition(varnames=
                        c(nodrought= 0, droughtoccurs =1),
                      FUN = fuzzy_cone, radius = 0.5, universe=U1) 
    
    
  )


rules <-
  set(
    
    fuzzy_rule(mai %is% moderate && predictvalue %is% mild, drought %is% nodrought),
    fuzzy_rule( mai %is% moderate && predictvalue %is% severe, drought %is%  droughtoccurs),
    fuzzy_rule(  mai %is% agri.favour && predictvalue %is% good, drought %is% nodrought),
    fuzzy_rule( mai %is% agri.favour && predictvalue %is% severe, drought %is%  droughtoccurs),
    fuzzy_rule( mai %is% humid && predictvalue %is% mild, drought %is% nodrought),
    fuzzy_rule( mai %is% severe && predictvalue %is% good, drought %is%  droughtoccurs)
  )


context <- fuzzy_system(variables, rules)

print(context)
plot(context)


print('2015')
print(r_15$prediction)

# Define 2 vectors
actual <- c(r_15$actual)
predicted <- c(r_15$prediction)

# Graph  using a y axis that ranges from 0 to 1.0
plot(actual, type="o", col="blue", ylim=c(0.0,1.0))

# Graph  with red dashed line and square points
lines(predicted, type="o", pch=22, lty=2, col="red")

# Create a title with a red, bold/italic font
title(main="Drought prediction - 2015", col.main="black", font.main=4)

legend(1, c("actual","predicted"), cex=0.8, 
       col=c("blue","red"), pch=21:22, lty=1:2)

print('2016')
print(r_16$prediction)

# Define 2 vectors
actual <- c(r_16$actual)
predicted <- c(r_16$prediction)

# Graph  using a y axis that ranges from 0 to 1.0
plot(actual, type="o", col="blue", ylim=c(0.0,1.0))

# Graph  with red dashed line and square points
lines(predicted, type="o", pch=22, lty=2, col="red")

# Create a title with a red, bold/italic font
title(main="Drought prediction - 2016", col.main="black", font.main=4)

legend(1, c("actual","predicted"), cex=0.8, 
       col=c("blue","red"), pch=21:22, lty=1:2)

print('2017')
print(r_17$prediction)

# Define 2 vectors
actual <- c(r_17$actual)
predicted <- c(r_17$prediction)

# Graph  using a y axis that ranges from 0 to 1.0
plot(actual, type="o", col="blue", ylim=c(0.0,1.0))

# Graph  with red dashed line and square points
lines(predicted, type="o", pch=22, lty=2, col="red")

# Create a title with a red, bold/italic font
title(main="Drought prediction - 2017", col.main="black", font.main=4)

legend(1, c("actual","predicted"), cex=0.8, 
       col=c("blue","red"), pch=21:22, lty=1:2)


print('2018')
print(r_18$prediction)

# Define 2 vectors
actual <- c(r_18$actual)
predicted <- c(r_18$prediction)

# Graph  using a y axis that ranges from 0 to 1.0
plot(actual, type="o", col="blue", ylim=c(0.0,1.0))

# Graph  with red dashed line and square points
lines(predicted, type="o", pch=22, lty=2, col="red")

# Create a title with a red, bold/italic font
title(main="Drought prediction - 2018", col.main="black", font.main=4)

legend(1, c("actual","predicted"), cex=0.8, 
       col=c("blue","red"), pch=21:22, lty=1:2)

y_15<-mean(r_15$prediction)
y_16<-mean(r_16$prediction)
y_17<-mean(r_17$prediction)
y_18<-mean(r_18$prediction)
finalans <- c(y_15,y_16,y_17,y_18)
finalans
barplot(finalans,
        main = "Prediction of drought",
        xlab = "year",
        ylab = "drought level",
        names.arg = c("2015", "2016", "2017", "2018"),
        col = "darkred")

for(i in 1:length(finalans))
{
  if(finalans[i] <0.4)
    finalans[i]<-0
  else if(finalans[i]<0.7)
    finalans[i]<-1
  else
    finalans[i]<-2
}
roundoff <-function(x)
{
  if(x>0.5)
    return (1)
  else
    return (0)
} 

pred <-sapply(results$prediction, roundoff)
pred
results$prediction<-pred
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(roundedresultsdf$actual,roundedresultsdf$prediction)
library(caret)
library(e1071)
str(roundedresultsdf)
confusion_mat <-confusionMatrix(factor(roundedresultsdf$actual),factor(roundedresultsdf$prediction))
print(confusion_mat)
print(confusion_mat[["table"]])
fi <- fuzzy_inference(context, list(mai=75, predictvalue=0.4))
gset_defuzzify(fi, "centroid")
U1 <- NULL
u2 <- NULL

