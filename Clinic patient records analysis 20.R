remove(list=ls())
setwd(choose.dir())
clinic<-read.csv("Clinic patient records 20.csv")
View(clinic)
str(clinic)

clinic1 <- clinic[,3:11]
str(clinic1)
dim(clinic1)
View(clinic1)

colSums(is.na(clinic1))
df_clinic <- na.omit(clinic1)
colSums(is.na(df_clinic))

str(df_clinic)
df_clinic[df_clinic == "No"] = 0
df_clinic[df_clinic == "Yes"] = 1
head(df_clinic)
df_clinic[df_clinic == ">10 km"] = 3
df_clinic[df_clinic == "6-10 km"] = 2
df_clinic[df_clinic == "0-5 km"] = 1
head (df_clinic)
str(df_clinic)
df_clinic$Swelling<-as.integer(df_clinic$Swelling)
str(df_clinic)

df_clinic$Active.Discharge<-as.integer(df_clinic$Active.Discharge)

df_clinic$Systemic.Conditions<-as.integer(df_clinic$Systemic.Conditions)
df_clinic$Immuno.Compromised<-as.integer(df_clinic$Immuno.Compromised)
df_clinic$Location<-as.integer(df_clinic$Location)

df_clinic$Gender<-as.factor(df_clinic$Gender)
df_clinic$Swelling<-as.factor(df_clinic$Swelling)
df_clinic$Active.Discharge<-as.factor(df_clinic$Active.Discharge)
df_clinic$Systemic.Conditions<-as.factor(df_clinic$Systemic.Conditions)
df_clinic$Immuno.Compromised<-as.factor(df_clinic$Immuno.Compromised)
df_clinic$Location<-as.factor(df_clinic$Location)

str(df_clinic)


library(dplyr)
bar_class_age = df_clinic %>% group_by(Age) %>% summarise(emergency = sum(emergency.admittance))
bar_class_gender = df_clinic %>% group_by(Gender) %>% summarise(emergency = sum(emergency.admittance))
bar_class_ad = df_clinic %>% group_by(Active.Discharge) %>% summarise(emergency = sum(emergency.admittance))
bar_class_sc = df_clinic %>% group_by(Systemic.Conditions) %>% summarise(emergency = sum(emergency.admittance))
bar_class_ic = df_clinic %>% group_by(Immuno.Compromised) %>% summarise(emergency = sum(emergency.admittance))

barplot(emergency ~ Age, data = bar_class_age)
barplot(emergency ~ Gender, data = bar_class_gender)
barplot(emergency ~ Active.Discharge, data = bar_class_ad)
barplot(emergency ~ Systemic.Conditions, data = bar_class_sc)
barplot(emergency ~ Immuno.Compromised, data = bar_class_ic)

df_clinic$emergency.admittance<-as.factor(df_clinic$emergency.admittance)
#splitting into 80% and 20% values to run train and test samples
train_index <- sample(1:nrow(df_clinic), 0.8 * nrow(df_clinic))
test_index <- setdiff(1:nrow(df_clinic), train_index)

X_train <- df_clinic[train_index, -9]
y_train <- df_clinic[train_index, "emergency.admittance"]
train <- df_clinic[train_index,]
str(train)

X_test <- df_clinic[test_index, -9]
y_test <- df_clinic[test_index, "emergency.admittance"]
test <- df_clinic[test_index,]
str(test)

install.packages("e1071")
library(e1071)

svmfit = svm(emergency.admittance ~ ., data = train, kernel = "linear")
print(svmfit)

logregfit <- glm(emergency.admittance ~ ., data = train, family = "binomial")
print(logregfit)


install.packages("party")
library(party)
treefit <- ctree(emergency.admittance ~ ., data = train)
print(treefit)
plot(treefit)


install.packages("caret")
library(caret)

pred_svm <- predict(svmfit, X_test)
print(pred_svm)
pred_log <- predict(logregfit, X_test)
print(pred_log)
pred_log <- ifelse(pred_log > 0.5, 1, 0)
pred_log <- as.factor(pred_log)
print(pred_log)
pred_dt <- predict(treefit, X_test)
print(pred_dt)

cm_svm<-confusionMatrix(pred_svm,y_test)
print(cm_svm)
cm_log<-confusionMatrix(pred_log,y_test)
print(cm_log)
cm_dt<-confusionMatrix(pred_dt,y_test)
print(cm_dt)



