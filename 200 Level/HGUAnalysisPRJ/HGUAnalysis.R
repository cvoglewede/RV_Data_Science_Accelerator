
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

# install.packages("carat")
# install.packages("BiocManager")
install.packages("caret",dependencies = T)
BiocManager::install("Biobase", version = "3.8")
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



HGUdata_raw %>% 
  group_by(age) %>% 
  summarise(patients=n(),cases=sum(num),rate=cases/patients) %>% 
  ggplot(aes(x=age,y=rate))+geom_point()

ggplot(HGUdata_raw,aes(x=num, y=age,fill=as.factor(num))) +  geom_boxplot()


HGUdata_raw %>% 
  group_by(sex) %>% 
  summarise(patients=n(),cases=sum(num),rate=cases/patients) %>% 
  ggplot(aes(x=sex,y=rate,fill=patients))+geom_bar(stat="identity")


HGUdata_raw %>% 
  group_by(cp) %>% 
  summarise(patients=n(),cases=sum(num),rate=cases/patients) %>% 
  ggplot(aes(x=cp,y=rate,fill=patients))+geom_bar(stat="identity")

HGUdata_raw %>% 
  group_by(fbs) %>% 
  summarise(patients=n(),cases=sum(num),rate=cases/patients) %>% 
  ggplot(aes(x=fbs,y=rate,fill=patients))+geom_bar(stat="identity")

HGUdata_raw %>% 
  group_by(restecg) %>% 
  summarise(patients=n(),cases=sum(num),rate=cases/patients) %>% 
  ggplot(aes(x=restecg,y=rate,fill=patients))+geom_bar(stat="identity")

HGUdata_raw %>% 
  group_by(exang) %>% 
  summarise(patients=n(),cases=sum(num),rate=cases/patients) %>% 
  ggplot(aes(x=exang,y=rate,fill=patients))+geom_bar(stat="identity")



ggplot(HGUdata_raw,aes(x=num, y=trestbps,fill=as.factor(num))) +  geom_boxplot()
ggplot(HGUdata_raw,aes(x=num, y=chol,fill=as.factor(num))) +  geom_boxplot()
ggplot(HGUdata_raw,aes(x=num, y=thalach,fill=as.factor(num))) +  geom_boxplot()
ggplot(HGUdata_raw,aes(x=num, y=oldpeak,fill=as.factor(num))) +  geom_boxplot()


(max(HGUdata_raw$chol)-mean(HGUdata_raw$chol))/sd(HGUdata_raw$chol)
(min(HGUdata_raw$thalach)-mean(HGUdata_raw$thalach))/sd(HGUdata_raw$thalach)

HGUdata_raw[HGUdata_raw$chol==max(HGUdata_raw$chol)|HGUdata_raw$thalach==min(HGUdata_raw$thalach),]


# Convert to Factors

outcome <- as.factor(HGUdata_raw$num)

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



HGUdata_ready <- HGUdata_complete %>% 
  cbind(dummy_sex,dummy_cp,dummy_fbs,dummy_exang,outcome) %>% 
  select(-c(sex,cp,fbs,restecg,exang,sex_female,cp_asymptomatic,fbs_low_fasting_blood_sugar,exang_no))


# Model Prep

set.seed(9595)
train_ind <- sample.split(HGUdata_ready,SplitRatio = .8)
train <- subset(HGUdata_ready,train_ind ==TRUE)
test <- subset(HGUdata_ready,train_ind ==FALSE)


# Logistic Regression
set.seed(1192)
model_logistic <- glm(outcome~.,data=train,family = binomial)
summary(model_logistic)
anova(model_logistic, test="Chisq")


train$prob_logistic <- predict(model_logistic, type="response")
classification_point <- .5
train$pred_class <- as.factor(ifelse(train$prob_logistic>classification_point,1,0))

confusionMatrix(data = train$pred_class, 
                reference = train$outcome)


pr <- prediction(train$prob_logistic, train$outcome)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

ggplot(train,aes(prob_logistic,fill=as.factor(outcome)))+geom_density(alpha=.7)




