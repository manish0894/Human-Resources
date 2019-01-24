setwd('C:\\Users\\ADMIN\\Downloads\\R\\Datasets')

library(dplyr)
library(car)
hr_train=read.csv("hr_train.csv")
head(hr_train)
hr_test=read.csv("hr_test.csv")

hr_test$left= NA

hr_train$data = 'train'
hr_test$data = 'test'

all= rbind(hr_train,hr_test)

apply(all,2,function(x) length(unique(x)))
glimpse(all)

apply(all,2,function(x) sum(is.na(x)))

CreateDummies=function(data,var,freq_cutoff=100){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    name=gsub("/","_",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
} 

all$promotion_last_5years = as.factor(all$promotion_last_5years)
all$Work_accident = as.factor(all$Work_accident)

# Impute the NA values with Mean in the dataset

for(col in names(all)){
  if(sum(is.na(all[,col]))>0 & !(col %in% c("data","left"))){
    all[is.na(all[,col]),col]=mean(all[all$data=='train',col],na.rm=T)
  }
}


##--- separating the dataset

hr_train = all %>% filter(data == 'train') %>% select(-data) 
hr_test= all %>% filter(data == 'test') %>% select(-left, -data)

for_vif = lm(left ~., data = hr_train) 
vif(for_vif)
sort(vif(for_vif),decreasing = T)[1:3]

library(randomForest)

fit_hr= randomForest(as.factor(left)~.,data=hr_train)
fit_hr

response=predict(fit_hr, newdata = hr_test,type = "prob")[,2]
