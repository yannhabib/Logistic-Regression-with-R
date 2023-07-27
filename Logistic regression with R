# Logistic-Regression-with-R

# load packages
library(dplyr)
library(psych)
library(ggpubr)
library(cowplot)
library(corrplot)
library(MASS)
library(caret)

df = read.csv("survey lung cancer.csv")

# Converting categorical variables to numerical variables
df$LUNG_CANCER <- as.factor(df$LUNG_CANCER) 
df$GENDER <- as.factor(df$GENDER)

df$LUNG_CANCER <- ifelse(df$LUNG_CANCER=="YES",1,0)
df$GENDER <- ifelse(df$GENDER=="M",1,0)

# Exploring data
head(df)
summary(df)

# checking for duplicates 
duplicated(df)
sum(duplicated(df))

# taking down the duplicates
df <- df %>% distinct()

# Studying data
df1 <- df[ , -c(1,16)] # creating data frame taking out variables that are not useful for the prediction
cordf1 <- cor(df1)

boxplot(df1$AGE) # checking the age range 

heatmap(cordf1)
corrplot(cordf1)

cordf <- cor(df)
corrplot(cordf)

# Splitting Splitting data into training and test sets
n <- dim(df)[1]
n.train <- round(n * 0.7)  # size of the training set, 70% of all observations
set.seed(2)   # set seed to reproduce the results
ind.train <- sample(n, n.train) # creating index for selecting rows
train <- df[ind.train,]  # creating training set
test <- df[-ind.train,]


# Performing LDA 
lda.fit <- lda(LUNG_CANCER ~ YELLOW_FINGERS + FATIGUE + ALLERGY + 
                 SHORTNESS.OF.BREATH + ALCOHOL.CONSUMING + COUGHING, data = train)
lda.pred <- predict(lda.fit, test)
table(lda.pred$class, test$LUNG_CANCER)

mean(lda.pred$class == test$LUNG_CANCER)

# Performing QDA
qda.fit <- qda(LUNG_CANCER ~ YELLOW_FINGERS + FATIGUE + ALLERGY + 
                 SHORTNESS.OF.BREATH + ALCOHOL.CONSUMING + COUGHING , data = train)
qda.pred <- predict(qda.mod2, test)
table(qda.pred$class, test$LUNG_CANCER)

mean(qda.pred$class == test$LUNG_CANCER)



log.fit <- glm(LUNG_CANCER ~ YELLOW_FINGERS + FATIGUE + ALLERGY + 
                 SHORTNESS.OF.BREATH + ALCOHOL.CONSUMING + COUGHING, data = train, family = binomial)
log.prob <- predict(log.fit, test, type = "response")
log.pred <- rep(0, length(log.prob))
log.pred[log.prob > 0.5] = 1
table(log.pred, test$LUNG_CANCER)

mean(log.pred == test$LUNG_CANCER)

?dim
?sample


qda.mod2<-qda(LUNG_CANCER~YELLOW_FINGERS*ANXIETY+., data=df)
confusionMatrix(predict(qda.mod2)$class, df$LUNG_CANCER)


pred<-predict(qda.mod2)$class
pred<-as.numeric(pred)
real<-as.numeric(slc$LUNG_CANCER)
pred<-ifelse(pred==2,1,0)
real<-ifelse(real==2,1,0)
