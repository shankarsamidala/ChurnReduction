rm(list=ls())

getwd()

setwd("E:/project_1")

# loading packages

x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees',"readr","class")


lapply(x, require,character.only=T)

rm(x)

data_tr=read_csv("Train_data.csv")

data_tst=read_csv("Test_data.csv")

# Basic stat and data preparation

str(data_tr)

str(data_tst)

class(data_tr)

class(data_tst)

data_tr=data.frame(data_tr)

data_tst=data.frame(data_tst)

data_tr[1:5,1:6]

attach(data_tr)

ggplot(data_tr, aes(x=Churn,fill=Churn)) +
  geom_bar(position="dodge")+theme_bw()+ggtitle("Churn distribution")

ggplot(data_tr, aes(x=Churn,fill=international.plan)) +
  geom_bar(position="dodge")+theme_bw()+ggtitle("Churn vs international.plan")

ggplot(data_tr, aes(x=Churn,fill=voice.mail.plan)) +
  geom_bar(position="dodge")+theme_bw()+ggtitle("Churn vs voice.mail.plan")

ggplot(data_tr, aes(x=state,fill=Churn)) +
  geom_bar(position="dodge")+theme_bw()+ggtitle("Churn vs state")

ggplot(data_tr, aes(account.length,fill=Churn)) +
  geom_histogram(binwidth = 1,position="identity")+ggtitle("account.length vs churn distribution")


ggplot(data_tr, aes(number.vmail.messages,fill=Churn)) +
  geom_histogram(binwidth = 5,position="dodge")+ggtitle("number.vmai.messages vs Churn distribution")

ggplot(data_tr, aes(number.customer.service.calls,fill=Churn)) +
  geom_histogram(binwidth = 5,position="dodge")+ggtitle("number.customer.service.calls vs Churn distribution")

ggplot(data_tr, aes(total.day.charge,fill=Churn)) +
  geom_histogram(binwidth = 5,position="dodge")+ggtitle("total.day.charge vs Churn")

ggplot(data_tr, aes(total.eve.charge,fill=Churn)) +
  geom_histogram(binwidth = 5,position="dodge")+ggtitle("total.eve.charge vs Churn")

ggplot(data_tr, aes(total.intl.charge,fill=Churn)) +
  geom_histogram(binwidth = 5,position="dodge")+ggtitle("total.intl.charge vs Churn")

ggplot(data_tr, aes(total.night.charge,fill=Churn)) +
  geom_histogram(binwidth = 5,position="dodge")+ggtitle("total.night.charge vs Churn")



#### scatter plot

ggplot(data_tr, aes(x = total.intl.minutes, y = total.intl.charge, col= Churn)) +
  geom_point(shape = 2, size = 2)+ggtitle("Total.intl.minutes vs Total.intl.charge vs Churn")

ggplot(data_tr, aes(x = total.intl.minutes, y = total.intl.charge, col= voice.mail.plan)) +
  geom_point(shape = 2, size = 2)+ggtitle("Total.intl.minutes vs Total.intl.charge vs Voice.mail.plan ")

ggplot(data_tr, aes(x = total.intl.minutes, y = total.intl.charge, col= international.plan)) +
  geom_point(shape = 2, size = 2)+ggtitle("Total.intl.minutes vs Total.intl.charge vs International.plan ")

ggplot(data_tr, aes(x = total.night.minutes, y = total.night.charge, col= international.plan)) +
  geom_point(shape = 2, size = 2)+ggtitle("Total.night.minutes vs Total.night.charge vs International.plan ")

ggplot(data_tr, aes(x = total.night.minutes, y = total.night.charge, col= voice.mail.plan)) +
  geom_point(shape = 2, size = 2)+ggtitle("Total.night.minutes vs Total.night.charge vs Voice.mail.plan ")

ggplot(data_tr, aes(x = total.night.minutes, y = total.night.charge, col= Churn)) +
  geom_point(shape = 2, size = 2)+ggtitle("Total.night.minutes vs Total.night.charge vs Churn ")

ggplot(data_tr, aes(x = total.eve.minutes, y = total.eve.charge, col= international.plan)) +
  geom_point(shape = 2, size = 2)+ggtitle("Total.eve.minutes vs Total.eve.charge vs International.plan ")

ggplot(data_tr, aes(x = total.eve.minutes, y = total.eve.charge, col= voice.mail.plan)) +
  geom_point(shape = 2, size = 2)+ggtitle("Total.eve.minutes vs Total.eve.charge vs Voice.mail.plan ")

ggplot(data_tr, aes(x = total.eve.minutes, y = total.eve.charge, col= Churn)) +
  geom_point(shape = 2, size = 2)+ggtitle("Total.eve.minutes vs Total.eve.charge vs Churn ")

##########

ggplot(data_tr, aes(x = total.day.minutes, y= total.day.charge, col= international.plan)) +
  geom_point(shape = 2, size = 2)+ggtitle("Total.day.minutes vs Total.day.charge vs International.plan ")

ggplot(data_tr, aes(x = total.day.minutes, y = total.day.charge, col= voice.mail.plan)) +
  geom_point(shape = 2, size = 2)+ggtitle("Total.day.minutes vs Total.day.charge vs Voice.mail.plan ")

ggplot(data_tr, aes(x = total.day.minutes, y = total.day.charge, col= Churn)) +
  geom_point(shape = 2, size = 2)+ggtitle("Total.day.minutes vs Total.day.charge vs Churn ")


############## boxplot

ggplot(data_tr,aes(x=Churn,y=total.day.calls,fill=Churn))+
  geom_boxplot(outlier.color ="red",outlier.size = 3)+ggtitle("outlier analysis(churn vs total.day.calls)")

ggplot(data_tr,aes(x=Churn,y=total.day.charge,fill=Churn))+
  geom_boxplot(outlier.color ="red",outlier.size = 3)+ggtitle("outlier analysis(churn vs total.day.charge)")

ggplot(data_tr,aes(x=Churn,y=total.day.minutes,fill=Churn))+
  geom_boxplot(outlier.color ="red",outlier.size = 3)+ggtitle("outlier analysis(churn vs total.day.minutes)")

ggplot(data_tr,aes(x=Churn,y=total.intl.calls,fill=Churn))+
  geom_boxplot(outlier.color ="red",outlier.size = 3)+ggtitle("outlier analysis(churn vs total.intl.calls)")

ggplot(data_tr,aes(x=Churn,y=total.intl.charge,fill=Churn))+
  geom_boxplot(outlier.color ="red",outlier.size = 3)+ggtitle("outlier analysis(churn vs total.intl.charge)")

ggplot(data_tr,aes(x=Churn,y=total.intl.minutes,fill=Churn))+
  geom_boxplot(outlier.color ="red",outlier.size = 3)+ggtitle("outlier analysis(churn vs total.intl.minutes)")

ggplot(data_tr,aes(x=Churn,y=total.eve.calls,fill=Churn))+
  geom_boxplot(outlier.color ="red",outlier.size = 3)+ggtitle("outlier analysis(churn vs total.eve.calls)")

ggplot(data_tr,aes(x=Churn,y=total.eve.charge,fill=Churn))+
  geom_boxplot(outlier.color ="red",outlier.size = 3)+ggtitle("outlier analysis(churn vs total.eve.charge)")

ggplot(data_tr,aes(x=Churn,y=total.eve.minutes,fill=Churn))+
  geom_boxplot(outlier.color ="red",outlier.size = 3)+ggtitle("outlier analysis(churn vs total.eve.minutes)")

ggplot(data_tr,aes(x=Churn,y=total.night.charge,fill=Churn))+
  geom_boxplot(outlier.color ="red",outlier.size = 3)+ggtitle("outlier analysis(churn vs total.night.charge)")

ggplot(data_tr,aes(x=Churn,y=total.night.calls,fill=Churn))+
  geom_boxplot(outlier.color ="red",outlier.size = 3)+ggtitle("outlier analysis(churn vs total.night.calls)")

ggplot(data_tr,aes(x=Churn,y=total.night.minutes,fill=Churn))+
  geom_boxplot(outlier.color ="red",outlier.size = 3)+ggtitle("outlier analysis(churn vs total.night.minutes)")

ggplot(data_tr,aes(x=Churn,y=account.length,fill=area))+
  geom_boxplot(outlier.color ="red",outlier.size = 3)+ggtitle("outlier analysis(churn vs account.length)")


# converting variables into their respective types in train data

data_tr$phone.number=as.factor(data_tr$phone.number)

data_tr$international.plan=as.factor(as.character(data_tr$international.plan))

data_tr$voice.mail.plan=as.factor(as.character(data_tr$voice.mail.plan))

data_tr$Churn=as.factor(as.character(data_tr$Churn))

data_tr$area.code=as.factor(data_tr$area.code)

data_tr$state=as.factor(data_tr$state)

# converting variables into their respective types in test data

data_tst$phone.number=as.factor(data_tst$phone.number)

data_tst$international.plan=as.factor(as.character(data_tst$international.plan))

data_tst$voice.mail.plan=as.factor(as.character(data_tst$voice.mail.plan))

data_tst$Churn=as.factor(as.character(data_tst$Churn))

data_tst$area.code=as.factor(data_tst$area.code)

data_tst$state=as.factor(data_tst$state)

# missing value analysis

sum(is.na(data_tr))

sum(is.na(data_tst))

data_tr_missing = data.frame(apply(data_tr,2,function(x){sum(is.na(x))}))

data_tst_missing = data.frame(apply(data_tst,2,function(x){sum(is.na(x))}))

data_tr_missing$colums=row.names(data_tr_missing)

row.names(data_tr_missing)=NULL

names(data_tr_missing)[1]="PERCENT"

data_tr_missing=data_tr_missing[,c(2,1)]

data_tst_missing$colums=row.names(data_tst_missing)

row.names(data_tst_missing)=NULL

names(data_tst_missing)[1]="PERCENT"

data_tst_missing=data_tst_missing[,c(2,1)]

# we dont have any missing values in the two data sets

#assigning levels to categorical variables

for(i in 1:ncol(data_tr)){
  if(class(data_tr[,i])== 'factor'){
    data_tr[,i] = factor(data_tr[,i], labels = (1:length(levels(factor(data_tr[,i])))))
  }
}

for(i in 1:ncol(data_tst)){
  if(class(data_tst[,i])== 'factor'){
    data_tst[,i] = factor(data_tst[,i], labels = (1:length(levels(factor(data_tst[,i])))))
  }
}

# outlier analysis

num_tr_ind=sapply(data_tr,is.numeric)

num_tr_data=data_tr[,num_tr_ind]

cnames=colnames(num_tr_data)

for(i in cnames){
  print(i)
  v=data_tr[,i][data_tr[,i]%in%boxplot.stats(data_tr[,i])$out]
  print(length(v))
  print(v)
}


library("ggplot2")



for (i in 1:length(cnames)) {
  assign(paste0("gn",i), ggplot(aes_string( y = (cnames[i]), x= "Churn") , data = subset(data_tr)) + 
           stat_boxplot(geom = "errorbar" , width = 0.5) +
           geom_boxplot(outlier.color = "red", fill = "grey", outlier.shape = 20, outlier.size = 1, notch = FALSE)+
           theme(legend.position = "bottom")+
           labs(y = cnames[i], x= "Churn")+
           ggtitle(paste("Boxplot" , cnames[i])))
  #print(i)
}


#Now plotting the plots
gridExtra::grid.arrange(gn1, gn2,gn3, ncol=3)
gridExtra::grid.arrange(gn4,gn5,gn6, ncol=3)
gridExtra::grid.arrange(gn7,gn8,gn9, ncol =3)
gridExtra::grid.arrange(gn10,gn11, ncol =3 )


## WE ARE NOT APPLYING OUTLIER ANALYSIS ON TEST DATA BECAUSE WE HAVE LESS NUMBER OF OUTLIERS

# saving num variables

dt_tr=data_tr

dt_tst=data_tst

dt_tr_ind=sapply(dt_tr,is.numeric)

## Correlation Plot 
corrgram(dt_tr[,dt_tr_ind], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

 ## Chi-squared Test of Independence
dt_tr_fac = sapply(dt_tr,is.factor)
fa_dt = dt_tr[,dt_tr_fac]

for (i in 1:5){
  print(names(fa_dt)[i])
  print(chisq.test(table(fa_dt$Churn,fa_dt[,i]))) 
}


# droping variables which are not carring much information

dt_tr=subset(dt_tr,select=-c(area.code,total.day.minutes, total.eve.minutes, total.night.minutes, total.intl.minutes, phone.number))

dt_tst=subset(dt_tst,select=-c(area.code,total.day.minutes, total.eve.minutes, total.night.minutes, total.intl.minutes, phone.number))

# feature scaling

# checking normality

hist(dt_tr$total.day.calls)

hist(dt_tr$number.customer.service.calls)

hist(dt_tr$number.vmail.messages)

#Normalisation

cnames_num = c("account.length","number.vmail.messages","total.day.calls","total.day.charge",
               "total.eve.calls","total.eve.charge","total.night.calls","total.night.charge","total.intl.calls", "total.intl.charge", 
               "number.customer.service.calls")

for(i in cnames_num){
  dt_tr[,i]=(dt_tr[,i]-min(dt_tr[,i]))/(max(dt_tr[,i]-min(dt_tr[,i])))
}

# for test data

for(i in cnames_num){
  dt_tst[,i]=(dt_tst[,i]-min(dt_tst[,i]))/(max(dt_tst[,i]-min(dt_tst[,i])))
}

# building model

rmExcept(c("dt_tr","dt_tst"))

train=dt_tr

test=dt_tst


#### scince the data is unbalanced we are applying balencing methods

# combining data

data=rbind(train,test)

d_ind=createDataPartition(data$Churn,p=0.666,list=F)

train=data[d_ind,]

test=data[-d_ind,]

# Decision tree on unbalanced data 


c50_mod=C5.0(Churn~.,train,trails=50,rules=T)

summary(c50_mod)

write(capture.output(summary(c50_mod)), "c50Rules.txt")

#predicting the test cases

prd_c50=predict(c50_mod,test[,-15],type = "class")

# evaluating the performance

cf_c50=table(test$Churn,prd_c50)


confusionMatrix(cf_c50)

cf_c50

#accuracy= 89.2%
#FNR=FN/TP+FN=34.3 %

rc_dt=roc.curve(test$Churn,prd_c50)

rc_dt

# the value of auc = 79.3% it is quite low

# applying ROSE method

rose_data=ROSE(Churn~.,data=train,seed=111)$data


c50_mod=C5.0(Churn~.,rose_data,trails=50,rules=T)

summary(c50_mod)

write(capture.output(summary(c50_mod)), "c50Rules.txt")

#predicting the test cases

prd_c50=predict(c50_mod,test[,-15],type = "class")

# evaluating the performance

cf_c50=table(test$Churn,prd_c50)


confusionMatrix(cf_c50)

cf_c50


rc_rose=roc.curve(test$Churn,prd_c50)

rc_rose

########### SMOTE method



smote_data=SMOTE(Churn~.,data=train,perc.over = 200,perc.under = 200)

table(smote_data$Churn)

c50_mod=C5.0(Churn~.,smote_data,trails=50,rules=T)

summary(c50_mod)

write(capture.output(summary(c50_mod)), "c50Rules.txt")

#predicting the test cases

prd_c50=predict(c50_mod,test[,-15],type = "class")

# evaluating the performance

cf_c50=table(test$Churn,prd_c50)


confusionMatrix(cf_c50)

cf_c50


rc_smote=roc.curve(test$Churn,prd_c50)

rc_smote

# accuracy=91.7%
# FNR=25.7%%

# Area under the curve (AUC): 0.869


# we can observe that smote method is working fine

# Random forest

rf_mod=randomForest(Churn~.,smote_data,ntree=500,importance=T)

#extracting rules from random forest

tree_list=RF2List(rf_mod)

#extract rules

exct=extractRules(tree_list,smote_data[,-15])

#visualize some rules

exct[1:2,]

#making the rules readable

readable_rules=presentRules(exct,colnames(smote_data))

readable_rules[1:2,]

# rule  metric

rule_metric=getRuleMetric(exct,smote_data[,-15],smote_data$Churn)

rule_metric[1:2,]

#predicting the model

rf_prd=predict(rf_mod,test[,-15])

# confusion matrix

cf_rf=table(test$Churn,rf_prd)

confusionMatrix(cf_rf)

rc_rf=roc.curve(test$Churn,rf_prd)

rc_rf
#if ntrees=100
# accuracy=88.02%
# FNR=21.5%%


# if ntrees=500
#acc=88.9%
#FNR=22.0%



# logistic regression

logit_mod=glm(Churn~.,smote_data,family = "binomial")

summary(logit_mod)

#predicting the model

logit_prd=predict(logit_mod,test,type="response")

logit_prd=ifelse(logit_prd>0.5,1,0)

#evaluating the performance

logit_cm=table(test$Churn,logit_prd)

#ACCURACY

sum(diag(logit_cm)/nrow(test))

#FNR

#FN/FN+TP

#accuracy=78.9%
#FNR=44.6%

# KNN model implemenation

require(class)

#predict test data

knn_prd=knn(smote_data[,1:14],test[,1:14],smote_data$Churn,k=7)

#confusion matrix

cm_knn=table(knn_prd,test$Churn)

cm_knn

confusionMatrix(cm_knn)
#accuracy

sum(diag(cm_knn)/nrow(test))

#FNR
#FNR=FN/FN+TP

# acc =81.18%
# FNR=66.02%


# naive bayes implementation

require(e1071)

# devloping a model

nb_mod=naiveBayes(Churn~.,smote_data)

# prediction on test cases

nb_prd=predict(nb_mod,test[,1:14],type="class")

#confusion matrix

cm_nb=table(observed=test[,15],predicted =nb_prd)

confusionMatrix(cm_nb)

# accuracy=82.09%
# FNR=46.2%

