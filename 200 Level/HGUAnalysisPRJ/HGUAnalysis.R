
#############################
## title: "HGU Analysis"
## author: "Connor Voglewede"
## date: "1/27/2019"
## output: Rscript
#############################

# Load Required Packages


library(tidyverse)
library(dummies)
library(mice)
library(caTools)
library(caret)
library(stringr)
library(ROCR)
library(corrplot)
library(popbio)
library(GGally)

# install.packages("carat")
# install.packages("BiocManager")
# install.packages("caret",dependencies = T)
# BiocManager::install("Biobase", version = "3.8")
# library(sjmisc)
# library(VIM)


# Read in Data


HGUdata_raw <- read.csv("HDdata.csv",header = TRUE)
head(HGUdata_raw)


# Exploration 

pairs(HGUdata_raw)
ggpairs(HGUdata_raw)


HGUdata_raw %>% 
  summarise(patients=n(),cases=sum(num),rate=cases/patients)


ggplot(HGUdata_raw,aes(x=num, y=age,fill=as.factor(num))) + geom_boxplot()+ labs(x="Diagnosis",y="age")+ guides(fill=guide_legend(title=NULL))

HGUdata_raw %>% 
  group_by(age) %>% 
  summarise(patients=n(),cases=sum(num),rate=cases/patients) %>% 
  ggplot(aes(x=age,y=rate))+geom_point() + labs(x="Age",y="Heart Disease Rate")

HGUdata_raw %>% 
  mutate(age_bin = ntile(age, 20)) %>% 
  group_by(age_bin) %>% 
  summarise(patients=n(),cases=sum(num),rate=cases/patients) %>% 
  ggplot(aes(x=age_bin,y=rate))+geom_point()+geom_smooth(method='lm',formula=y~x)+ labs(x="Age Ventile",y="Heart Disease Rate")



HGUdata_raw %>% 
  group_by(sex) %>% 
  summarise(patients=n(),cases=sum(num),rate=cases/patients) %>% 
  ggplot(aes(x=sex,y=rate,fill=patients))+geom_bar(stat="identity")+ labs(x="Sex",y="Heart Disease Rate")


HGUdata_raw %>% 
  group_by(cp) %>% 
  summarise(patients=n(),cases=sum(num),rate=cases/patients) %>% 
  ggplot(aes(x=cp,y=rate,fill=patients))+geom_bar(stat="identity")+ labs(x="Chest Pain Type",y="Heart Disease Rate")

HGUdata_raw %>% 
  group_by(fbs) %>% 
  summarise(patients=n(),cases=sum(num),rate=cases/patients) %>% 
  ggplot(aes(x=fbs,y=rate,fill=patients))+geom_bar(stat="identity")+ labs(x="Fasting Blood Sugar >120 ",y="Heart Disease Rate")

HGUdata_raw %>% 
  group_by(restecg) %>% 
  summarise(patients=n(),cases=sum(num),rate=cases/patients) %>% 
  ggplot(aes(x=restecg,y=rate,fill=patients))+geom_bar(stat="identity")+ labs(x="EC Result Category",y="Heart Disease Rate")

HGUdata_raw %>% 
  group_by(exang) %>% 
  summarise(patients=n(),cases=sum(num),rate=cases/patients) %>% 
  ggplot(aes(x=exang,y=rate,fill=patients))+geom_bar(stat="identity")+ labs(x="Exercise Induced Angina",y="Heart Disease Rate")



ggplot(HGUdata_raw,aes(x=num, y=trestbps,fill=as.factor(num))) +  geom_boxplot()+ labs(x="Diagnosis",y="Resting Blood Pressure")+ guides(fill=guide_legend(title=NULL))
HGUdata_raw %>% 
  mutate(trestbps_bin = ntile(trestbps, 20)) %>% 
  group_by(trestbps_bin) %>% 
  summarise(patients=n(),cases=sum(num),rate=cases/patients) %>% 
  ggplot(aes(x=trestbps_bin,y=rate))+geom_point() + labs(x="Resting Blood Pressure",y="Heart Disease Rate")



ggplot(HGUdata_raw,aes(x=num, y=chol,fill=as.factor(num))) +  geom_boxplot()+ labs(x="Diagnosis",y="Serum Cholestorol level")+ guides(fill=guide_legend(title=NULL))
HGUdata_raw %>% 
  mutate(chol_bin = ntile(chol, 20)) %>% 
  group_by(chol_bin) %>% 
  summarise(patients=n(),cases=sum(num),rate=cases/patients) %>% 
  ggplot(aes(x=chol_bin,y=rate))+geom_point()+ labs(x="Serum Cholestorol level",y="Heart Disease Rate")



ggplot(HGUdata_raw,aes(x=num, y=thalach,fill=as.factor(num))) +  geom_boxplot()+ labs(x="Diagnosis",y="Maximum Heart Rate")+ guides(fill=guide_legend(title=NULL))
HGUdata_raw %>% 
  mutate(thalach_bin = ntile(thalach, 20)) %>% 
  group_by(thalach_bin) %>% 
  summarise(patients=n(),cases=sum(num),rate=cases/patients) %>% 
  ggplot(aes(x=thalach_bin,y=rate))+geom_point()+geom_smooth(method='lm',formula=y~x) + labs(x="Maximum Heart Rate",y="Heart Disease Rate")




ggplot(HGUdata_raw,aes(x=num, y=oldpeak,fill=as.factor(num))) +  geom_boxplot()+ labs(x="Diagnosis",y="ST Depression")+ guides(fill=guide_legend(title=NULL))
HGUdata_raw %>% 
  mutate(oldpeak_bin = ntile(oldpeak, 20)) %>% 
  group_by(oldpeak_bin) %>% 
  summarise(patients=n(),cases=sum(num),rate=cases/patients) %>% 
  ggplot(aes(x=oldpeak_bin,y=rate))+geom_point()+geom_smooth(method='lm',formula=y~x)+ labs(x="ST Depression",y="Heart Disease Rate")


(max(HGUdata_raw$chol)-mean(HGUdata_raw$chol))/sd(HGUdata_raw$chol)
HGUdata_raw <- HGUdata_raw[-246,]


# Convert to Factors

outcome <- factor(as.factor(HGUdata_raw$num),levels=c("0","1"))

sex_key <- c("1" = "male", "0" = "female")
cp_key <- c("1" = "typical angina", "2" = "atypical angina","3"="non_anginal pain", "4"="asymptomatic")
fbs_key <- c("1"="high fasting blood sugar","0"="low fasting blood sugar")
restecg_key <- c("0"="normal","1"="ST_T wave abnormality","2"="left ventricular hypertrophy")
exang_key <- c("1"="yes","0"="no")


HGUdata_pre <- HGUdata_raw %>% 
  mutate(sex=as.factor(recode(sex,!!!sex_key))) %>% 
  mutate(cp=as.factor(str_replace_all(recode(cp,!!!cp_key),"\\s+","_"))) %>% 
  mutate(fbs=as.factor(str_replace_all(recode(fbs,!!!fbs_key),"\\s+","_"))) %>% 
  mutate(restecg=as.factor(str_replace_all(recode(restecg,!!!restecg_key),"\\s+","_"))) %>% 
  mutate(exang=as.factor(str_replace_all(recode(exang,!!!exang_key),"\\s+","_"))) 



# Missing data
set.seed(828)
imputed_Data <- mice(HGUdata_pre[,names(HGUdata_pre)!="num"], m=5, maxit =100, method = 'pmm', seed = 500)
HGUdata_complete <- complete(imputed_Data)


# Prepare data for Modeling

dummy_sex <- dummy(HGUdata_complete$sex,sep="_")
dummy_cp <- dummy(HGUdata_complete$cp,sep="_")
dummy_fbs <- dummy(HGUdata_complete$fbs,sep="_")
dummy_restecg <- dummy(HGUdata_complete$restecg,sep="_")
dummy_exang <- dummy(HGUdata_complete$exang,sep="_")

dummies <- cbind(dummy_sex,dummy_cp,dummy_fbs,dummy_restecg,dummy_exang)


HGUdata_ready <- cbind(HGUdata_complete,dummies,outcome)
 
HGUdata_ready <- HGUdata_ready[ ,c(1,4,5,8,10,12,14,15,16,17,19,21,23,24)]
# For markdown... 
names(HGUdata_ready)
cols <- c("age","trestbps","chol","thalach","oldpeak","sex_male","cp_atypical_angina","cp_non_anginal_pain","cp_typical_angina","fbs_high_fasting_blood_sugar","restecg_left_ventricular_hypertrophy","restecg_ST_T_wave_abnormality","exang_yes","outcome")
names(HGUdata_ready) <- cols
head(HGUdata_ready)

# Model Prep

set.seed(9595)
train_ind <- sample.split(HGUdata_ready,SplitRatio = .8)
train <- subset(HGUdata_ready,train_ind ==TRUE)
test <- subset(HGUdata_ready,train_ind ==FALSE)

corrplot::corrplot(cor(train[,1:13]), 
                   order = "hclust", 
                   tl.cex = .8)




# Logistic Regression
set.seed(1192)
model_logistic <- glm(outcome~.,data=train,family = binomial)
summary(model_logistic)


train$prob_logistic <- predict(model_logistic, type="response")
classification_point <- .5
train$pred_class <- as.factor(ifelse(train$prob_logistic>classification_point,1,0))

confusion_matrix_logistic <- confusionMatrix(data = train$pred_class, 
                                             reference = train$outcome, positive = "1")

confusion_matrix_logistic

pr_logistic <- prediction(train$prob_logistic, train$outcome)
roc_logistic <- performance(pr_logistic, measure = "tpr", x.measure = "fpr")
auc_logistic <- performance(pr_logistic, measure = "auc")
auc_value_logistic <- auc_logistic@y.values[[1]]
auc_value_logistic
plot(roc_logistic)+title("Logistic Regression ROC Curve - Train")
abline(0,1)
text(.4,.6,paste(round(auc_value_logistic,3)))


ggplot(train,aes(prob_logistic,fill=as.factor(outcome)))+geom_density(alpha=.7)+ labs(x="Predicted Probabilities")+ guides(fill=guide_legend(title="Outcome"))


# Test Results

test$prob <- predict(model_logistic, newdata = test, type="response")
test$pred_class <- as.factor(ifelse(test$prob>classification_point,1,0))
confusion_matrix_test <- confusionMatrix(data = test$pred_class, reference = test$outcome,positive="1")
confusion_matrix_test

pr_test <- prediction(test$prob, test$outcome)
roc_test <- performance(pr_test, measure = "tpr", x.measure = "fpr")
auc_test <- performance(pr_test, measure = "auc")
auc_value_test <- auc_test@y.values[[1]]
auc_value_test
plot(roc_test)+title("Logistic Regression ROC Curve - Test")
abline(0,1)
text(.4,.6,paste(round(auc_value_test,3)))
plot(roc_logistic,add=TRUE,col="red")



# Manually calcualte Accuracy, PPV, NPV, Sensitivity, and Specificity.



event <- test$outcome == 1
predict <- test$prob > classification_point
TP <- sum(predict & event)
TN <- sum(!predict & !event)
FP <- sum(predict & !event)
FN <- sum(!predict & event)
n <- length(predict)
sensitivity_manual <-  TP/(TP+FN)
specificity_manual <- (TN)/(FP+TN)
prevalence_manual <- (TP+FN)/n
accuracy_manual <- (TP+TN)/n
PPV_manual <- (TP)/(TP+FP)
NPV_manual <- (specificity_manual*(1-prevalence_manual))/((prevalence_manual*(1-sensitivity_manual)+(1-prevalence_manual)*specificity_manual))

metrics <- list(
  prevalence_manual = prevalence_manual,
  accuracy_manual = accuracy_manual, 
  sensitivity_manual = sensitivity_manual, 
  specificity_manual = specificity_manual, 
  PPV_manual = PPV_manual,
  NPV_manual = NPV_manual
)
metrics



# logi.hist.plot(train$thalach,train$outcome==1,boxp=FALSE,type="hist",col="gray")

