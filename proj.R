#Importing data
dataset=read.csv("agaricus-lepiota.data",header = F,
                 col.names =c("poiede","cap_shape","cap_surface","cap_color","bruises",
                              "odor","gill_attachment","gill_spacing","gill_size",
                              "gill_color","stalk_shape","stalk_root",
                              "stalk_surface_above_ring","stalk_surface_below_ring",
                              "stalk_color_above_ring","stalk_color_below_ring",
                              "veil_type","veil_color","ring_number","ring_type",
                              "spore_print_color","population","habitat") )
View(dataset)
str(dataset)
#cleaning the data
library(dplyr)
summary(dataset)     #shows missing values
#removes missing values
clean_dataset=dataset %>%filter(dataset$stalk_root!='?') %>% droplevels()
View(clean_dataset)
summary(clean_dataset)
#changing factors to numeric
levels(dataset$poiede)
for(i in 1:23){
  clean_dataset[,i]=as.numeric(factor(clean_dataset[,i],levels=levels(clean_dataset[,i]),labels=length(levels(clean_dataset[,i]))))
}

View(clean_dataset)
pairs(clean_dataset)
#To find the correlation between poiede with other variables 
library(ggcorrplot)
round(cor(clean_dataset),1)
ggcorrplot(corr,outline.color = "white",lab = T,lab_size =3)

ggplot(data=clean_dataset,aes(x=odor,y=poiede,fill=as.factor(poiede)))+geom_col()+
  ggtitle("Distribution of ediblity over odor")

ggplot(data=clean_dataset,aes(x=gill_color,y=poiede,fill=as.factor(poiede)))+geom_col()+
  ggtitle("Distribution of ediblity over gill_color")


ggplot(data = clean_dataset,aes(fill=as.factor(poiede),x=gill_color))+geom_density(alpha=0.4)

ggplot(data = clean_dataset,aes(fill=as.factor(poiede),x=odor))+geom_density(alpha=0.4)

#Though gill_color have high correlation it have less significance coefficient with model

#Splitting dataset
set.seed(123)
library(caTools)
split=sample.split(clean_dataset$poiede,SplitRatio = 0.75)
training_set<-subset(clean_dataset,split==T)
testing_set<-subset(clean_dataset,split==F)
dim(training_set)
dim(testing_set)
View(training_set)
View(testing_set )

#Encoding as factor
class(training_set$poiede)
training_set$poiede=as.factor(training_set$poiede)

#Buliding linear model
classifiers=glm(formula=poiede~odor+bruises+stalk_color_above_ring+
                  stalk_surface_above_ring+stalk_surface_below_ring+
                  stalk_shape+stalk_root+gill_spacing+ring_type,
                family = binomial(),data = training_set)
summary(classifiers)
#prediction
prob_predict=predict.glm(classifiers,newdata=testing_set[-1],type ="response")
y_predict=ifelse(prob_predict>0.5,2,1)
y_predict

#visualize results
cm<-table(testing_set[,1],y_predict)
cm
accuracy=(cm[1,1]+cm[2,2])/NROW(testing_set)
accuracy
misclassification=1-accuracy
misclassification
#plotting edible and poisonous mushrooms in testing_set
edible=cm[1,1]
poisonous=cm[2,2]
library(plotrix)
pie3D(x=c(poisonous,edible),explode = 0.25,labels = c("poisonous","edible"),main=" Ediblity in testing_set")

#Model building using Decision Tree
library(rpart)
library(rpart.plot)
mod_fit=rpart(formula =poiede~odor+bruises+stalk_color_above_ring+
                stalk_surface_above_ring+stalk_surface_below_ring+
                stalk_shape+stalk_root+gill_spacing+ring_type,
              data = training_set,method = "class")
#prediction
pred_mod=predict(object = mod_fit,newdata =testing_set,type ="class")

tab=table(testing_set$poiede,pred_mod)
tab
accuracy=(tab[1,1]+tab[2,2])/NROW(testing_set)
accuracy
misclassification=1-accuracy
misclassification
#visualizing result
rpart.plot(mod_fit)

#for edibility variables please refer link
#"https://m.wikihow.com/Identify-Edible-Mushrooms"