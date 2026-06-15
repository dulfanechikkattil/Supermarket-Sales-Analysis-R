library(caret)
set.seed(123)
df
trainlndex<-createDataPartition(df$`Tax 5%`,p=0.8,list=FALSE)
train_data<-df[trainlndex,]
test_data<-df[-trainlndex,]

train_control<-trainControl(
  method="cv",
  number = 5,
  verboseIter = TRUE
  
)

set.seed(123)
lm_model<-train(
  `Tax 5%`~.,
  data=train_data,
  method= "glm",
  trControl=train_control
)
print(lm_model)
pred_lm<-predict(lm_model,newdata=test_data)
postResample(pred=pred_lm,obs=test_data$`Tax 5%`)


new_data <- df[2, -which(names(df) == "Tax 5%")]
new_data
predict(lm_model, newdata = new_data)
head(df)

