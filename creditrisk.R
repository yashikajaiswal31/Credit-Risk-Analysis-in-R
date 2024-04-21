#Installing packages which are required
install.packages("ggplot2") 
install.packages("caret")
install.packages("cowplot")
install.packages("ROCR")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("rattle")
install.packages("randomForest")

#Loading packages
library(ggplot2)
library(cowplot)
library(caret)
library(ROCR)
library(rpart)
library(rpart.plot)
library(rattle)
library(randomForest)

#Importing data from dataset
credit <- read.csv('credit.csv', header = TRUE)
#To display the structure or contents of the dataset
str(credit)

#Data processing
credit$Duration.of.Credit..month. <- cut(credit$Duration.of.Credit..month., c(0,12,18,24,Inf), labels = c(1:4))
credit$Credit.Amount <- cut(credit$Credit.Amount, c(0,1000,5000,10000,Inf), labels = c(1:4))
credit$Age..years. <- cut(credit$Age..years., c(18,25,40,60,Inf), labels = c(1:4))
head(credit[,c(3,6,14)],5)

#Converting each column to factor variable
for(i in 1:21) credit[, i] <- as.factor(credit[, i])

# Checking for null values
null_values <- sapply(credit, function(x) sum(is.na(x)))
print(null_values)

# Checking for outlier
boxplot(credit$Credit.Amount, main="Boxplot of Credit.Amount")

# Identifying potential outlier
outliers <- boxplot.stats(credit$Credit.Amount)$out
print(outliers)

#---------Main part-----------
           #Exploratory analysis

#Plot 1
g <- ggplot(credit, aes(Creditability)) +
  geom_bar(fill = "#ff0000") +
  theme(axis.title.x=element_blank()) + 
  theme(axis.title.y=element_blank()) +
  scale_y_continuous(breaks=seq(0,700,100)) +
  scale_x_discrete(labels = c("Bad","Good")) +
  ggtitle("Count of Good and Bad Credit Risks")
g

#Plot 2
g <- ggplot(credit, aes(Savings.account.bonds, fill = Creditability), stat="identity") +
  geom_bar() +
  scale_fill_manual(values = c("#D3D6D4", "#ff0000"), labels=c("Bad","Good")) +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank()) +
  scale_y_continuous(breaks=seq(0,700,100)) +
  scale_x_discrete(labels = c("< 100 DM", "100-500 DM", "500-1000 DM", "> 1000 DM", "Unknown")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) + 
  theme(axis.text.y = element_text(size = 10)) +
  theme(legend.text=element_text(size=10)) +
  theme(legend.title=element_text(size=12)) +
  ggtitle("Good and Bad Credit Risks by Credit History")
g

#Plot 3
g <- ggplot(credit, aes(Occupation, fill = Creditability), stat="identity") +
  geom_bar() +
  scale_fill_manual(values = c("#D3D6D4", "#ff0000"), labels=c("Bad","Good")) +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank()) +
  scale_y_continuous(breaks=seq(0,700,100)) +
  scale_x_discrete(labels = c("Unemployed", "Unskilled", "Skilled", "Management")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) + 
  theme(axis.text.y = element_text(size = 10)) +
  theme(legend.text=element_text(size=10)) +
  theme(legend.title=element_text(size=12)) +
  ggtitle("Good and Bad Credit Risks by Occupation")
g

#Plot 4
g <- ggplot(credit, aes(Age..years., fill = Creditability), stat="identity") +
  geom_bar() +
  scale_fill_manual(values = c("#D3D6D4", "#ff0000"), labels=c("Bad","Good")) +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank()) +
  scale_y_continuous(breaks=seq(0,700,100)) +
  scale_x_discrete(labels = c("18-25", "26-40", "41-60", "60+")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) + 
  theme(axis.text.y = element_text(size = 10)) +
  theme(legend.text=element_text(size=10)) +
  theme(legend.title=element_text(size=12)) +
  ggtitle("Good and Bad Credit Risks by Age")
g

               #STATISTICAL MODELLING

#LOGISTIC REGRESSION
set.seed(2828)
inTraining <- createDataPartition(credit$Creditability, p=0.7, list=FALSE)
train <- credit[inTraining,]
test <- credit[-inTraining,]

#Model Building and Training using the glm() function.
set.seed(2828)
lmModel <- glm(Creditability ~ ., family = binomial, data = train)

#Model prediction to test set
lmFit <- predict(lmModel, type = "response", test)

#Compare predictions to test set
lmPred <- prediction(lmFit, test$Creditability)

#Model evaluation and performance metrics
plot(performance(lmPred, 'tpr', 'fpr'))
performance(lmPred, measure = 'auc')@y.values[[1]]

#DECISION TREE
set.seed(28)
dtModel <- rpart(Creditability ~ ., data=train)
fancyRpartPlot(dtModel)

#Model evaluation and performance matrix
dtFit <- predict(dtModel, test, type = 'prob')[, 2]
dtPred <- prediction(dtFit, test$Creditability)
plot(performance(dtPred, 'tpr', 'fpr'))
performance(dtPred, measure = 'auc')@y.values[[1]]

#RANDOM FOREST
set.seed(2828)
rfModel <- randomForest(Creditability ~ ., data=train)
rfFit <- predict(rfModel, test, type = 'prob')[,2]
rfPred <- prediction(rfFit, test$Creditability)
plot(performance(rfPred, 'tpr', 'fpr'))
performance(rfPred, measure = 'auc')@y.values[[1]]

#Feature importance visualization
par(mfrow=c(1,1))
varImpPlot(rfModel, pch=1, main="Random Forest Model Variables Importance")

#Model evaluation and confusion matrix
rfCM <- confusionMatrix(test$Creditability,
                        predict(rfModel, test, type="class"))
rfCM

#End
