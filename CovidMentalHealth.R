###COVID research

#Anxiety
########Age
rm(list = ls()) # clear global environment
graphics.off() # close all graphics
gc()
setwd("")

library("readxl")
library(caret)


my_data <- read_excel("Anxiety.xlsx", sheet = "Age")[,c(4,5,7)]
str(my_data)

data.variables<-my_data[,1]
data.variables[,] <- lapply(my_data[,1], factor) 
dummy<- data.frame(predict(caret::dummyVars(" ~ .", data = data.variables, fullRank=F),
                           newdata = data.variables))
my_data<-cbind(dummy[,-7],my_data[,c(2,3)])



test<- my_data[which(my_data$Week<=12),]
train<-my_data[which(my_data$Week<=12),]
  

ctrl1 <- trainControl(method = "cv", number = 5, returnResamp = "all")
model <- caret::train(Value ~ ., data = train, "lm",trControl= ctrl1)
pred <- predict(model, test)
results<- postResample(pred =  pred, obs = test$Value)

summary(model)
results

sink("A.Age.1.txt")
print(summary(model))
sink()

sink("A.Age.2.txt")
print(results)
sink()

########### Education
rm(list = ls()) # clear global environment
graphics.off() # close all graphics
gc()
setwd("")

library("readxl")

my_data <- read_excel("Anxiety.xlsx", sheet = "Education")[,c(4,5,7)]
str(my_data)

data.variables<-my_data[,1]
data.variables[,] <- lapply(my_data[,1], factor) 
dummy<- data.frame(predict(caret::dummyVars(" ~ .", data = data.variables, fullRank=F),
                           newdata = data.variables))
my_data<-cbind(dummy[,-1],my_data[,c(2,3)])



test<- my_data[which(my_data$Week<=12),]
train<-my_data[which(my_data$Week<=12),]

library(caret)
ctrl1 <- trainControl(method = "cv", number = 5, returnResamp = "all")
model <- caret::train(Value ~ ., data = train, "lm",trControl= ctrl1)
pred <- predict(model, test)
results<- postResample(pred =  pred, obs = test$Value)

summary(model)
results

sink("A.Education.1.txt")
print(summary(model))
sink()

sink("A.Education.2.txt")
print(results)
sink()


########### Sex
rm(list = ls()) # clear global environment
graphics.off() # close all graphics
gc()
setwd("")

library("readxl")

my_data <- read_excel("Anxiety.xlsx", sheet = "Sex")[,c(4,5,7)]
str(my_data)

data.variables<-my_data[,1]
data.variables[,] <- lapply(my_data[,1], factor) 
dummy<- data.frame(predict(caret::dummyVars(" ~ .", data = data.variables, fullRank=F),
                           newdata = data.variables))
my_data<-cbind(dummy$Subgroup.Female,my_data[,c(2,3)])



test<- my_data[which(my_data$Week<=12),]
train<-my_data[which(my_data$Week<=12),]


library(caret)
ctrl1 <- trainControl(method = "cv", number = 5, returnResamp = "all")
model <- caret::train(Value ~ ., data = train, "lm",trControl= ctrl1)
pred <- predict(model, test)
results<- postResample(pred =  pred, obs = test$Value)

summary(model)
results

sink("A.Sex.1.txt")
print(summary(model))
sink()

sink("A.Sex.2.txt")
print(results)
sink()

########### Race
rm(list = ls()) # clear global environment
graphics.off() # close all graphics
gc()
setwd("")

library("readxl")

my_data <- read_excel("Anxiety.xlsx", sheet = "Race")[,c(4,5,7)]
str(my_data)

data.variables<-my_data[,1]
data.variables[,] <- lapply(my_data[,1], factor) 
dummy<- data.frame(predict(caret::dummyVars(" ~ .", data = data.variables, fullRank=F),
                           newdata = data.variables))
my_data<-cbind(dummy[,-2],my_data[,c(2,3)])



test<- my_data[which(my_data$Week<=12),]
train<-my_data[which(my_data$Week<=12),]

library(caret)
ctrl1 <- trainControl(method = "cv", number = 5, returnResamp = "all")
model <- caret::train(Value ~ ., data = train, "lm",trControl= ctrl1)
pred <- predict(model, test)
results<- postResample(pred =  pred, obs = test$Value)

summary(model)
results

sink("A.Race.1.txt")
print(summary(model))
sink()

sink("A.Race.2.txt")
print(results)
sink()

########### State
rm(list = ls()) # clear global environment
graphics.off() # close all graphics
gc()
setwd("")

library("readxl")

my_data <- read_excel("Anxiety.xlsx", sheet = "State")[,c(4,5,7)]
str(my_data)

data.variables<-my_data[,1]
data.variables[,] <- lapply(my_data[,1], factor) 
dummy<- data.frame(predict(caret::dummyVars(" ~ .", data = data.variables, fullRank=F),
                           newdata = data.variables))
my_data<-cbind(dummy[,-28],my_data[,c(2,3)])
#nebraska as reference


test<- my_data[which(my_data$Week<=12),]
train<-my_data[which(my_data$Week<=12),]

library(caret)
ctrl1 <- trainControl(method = "cv", number = 5, returnResamp = "all")
model <- caret::train(Value ~ ., data = train, "lm",trControl= ctrl1)
pred <- predict(model, test)
results<- postResample(pred =  pred, obs = test$Value)

summary(model)
results

sink("A.State.1.txt")
print(summary(model))
sink()

sink("A.State.2.txt")
print(results)
sink()


########### USA
rm(list = ls()) # clear global environment
graphics.off() # close all graphics
gc()
setwd("")

library("readxl")

my_data <- read_excel("Anxiety.xlsx", sheet = "USA")[,c(5,7)]
str(my_data)



test<- my_data[which(my_data$Week<=12),]
train<-my_data[which(my_data$Week<=12),]

library(caret)
ctrl1 <- trainControl(method = "cv", number = 5, returnResamp = "all")
model <- caret::train(Value ~ ., data = train, "lm",trControl= ctrl1)
pred <- predict(model, test)
results<- postResample(pred =  pred, obs = test$Value)

summary(model)
results

sink("A.US.1.txt")
print(summary(model))
sink()

sink("A.US.2.txt")
print(results)
sink()







###################################################################################################
##################################################################################################
#Depression

########Age
rm(list = ls()) # clear global environment
graphics.off() # close all graphics
gc()
setwd("")

library("readxl")
my_data <- read_excel("Depression.xlsx", sheet = "Age")[,c(4,5,7)]
str(my_data)

data.variables<-my_data[,1]
data.variables[,] <- lapply(my_data[,1], factor) 
dummy<- data.frame(predict(caret::dummyVars(" ~ .", data = data.variables, fullRank=F),
                           newdata = data.variables))
my_data<-cbind(dummy[,-7],my_data[,c(2,3)])



test<- my_data[which(my_data$Week<=12),]
train<-my_data[which(my_data$Week<=12),]

library(caret)
ctrl1 <- trainControl(method = "cv", number = 5, returnResamp = "all")
model <- caret::train(Value ~ ., data = train, "lm",trControl= ctrl1)
pred <- predict(model, test)
results<- postResample(pred =  pred, obs = test$Value)

summary(model)
results

sink("D.Age.1.txt")
print(summary(model))
sink()

sink("D.Age.2.txt")
print(results)
sink()

########### Education
rm(list = ls()) # clear global environment
graphics.off() # close all graphics
gc()
setwd("")

library("readxl")

my_data <- read_excel("Depression.xlsx", sheet = "Education")[,c(4,5,7)]
str(my_data)

data.variables<-my_data[,1]
data.variables[,] <- lapply(my_data[,1], factor) 
dummy<- data.frame(predict(caret::dummyVars(" ~ .", data = data.variables, fullRank=F),
                           newdata = data.variables))
my_data<-cbind(dummy[,-1],my_data[,c(2,3)])


test<- my_data[which(my_data$Week<=12),]
train<-my_data[which(my_data$Week<=12),]

library(caret)
ctrl1 <- trainControl(method = "cv", number = 5, returnResamp = "all")
model <- caret::train(Value ~ ., data = train, "lm",trControl= ctrl1)
pred <- predict(model, test)
results<- postResample(pred =  pred, obs = test$Value)

summary(model)
results

sink("D.Education.1.txt")
print(summary(model))
sink()

sink("D.Education.2.txt")
print(results)
sink()

########### Sex
rm(list = ls()) # clear global environment
graphics.off() # close all graphics
gc()
setwd("")

library("readxl")

my_data <- read_excel("Depression.xlsx", sheet = "Sex")[,c(4,5,7)]
str(my_data)

data.variables<-my_data[,1]
data.variables[,] <- lapply(my_data[,1], factor) 
dummy<- data.frame(predict(caret::dummyVars(" ~ .", data = data.variables, fullRank=F),
                           newdata = data.variables))
my_data<-cbind(dummy$Subgroup.Female,my_data[,c(2,3)])


test<- my_data[which(my_data$Week<=12),]
train<-my_data[which(my_data$Week<=12),]

library(caret)
ctrl1 <- trainControl(method = "cv", number = 5, returnResamp = "all")
model <- caret::train(Value ~ ., data = train, "lm",trControl= ctrl1)
pred <- predict(model, test)
results<- postResample(pred =  pred, obs = test$Value)

summary(model)
results

sink("D.Sex.1.txt")
print(summary(model))
sink()

sink("D.Sex.2.txt")
print(results)
sink()


########### Race
rm(list = ls()) # clear global environment
graphics.off() # close all graphics
gc()
setwd("")

library("readxl")

my_data <- read_excel("Depression.xlsx", sheet = "Race")[,c(4,5,7)]
str(my_data)

data.variables<-my_data[,1]
data.variables[,] <- lapply(my_data[,1], factor) 
dummy<- data.frame(predict(caret::dummyVars(" ~ .", data = data.variables, fullRank=F),
                           newdata = data.variables))
my_data<-cbind(dummy[,-2],my_data[,c(2,3)])


test<- my_data[which(my_data$Week<=12),]
train<-my_data[which(my_data$Week<=12),]

library(caret)
ctrl1 <- trainControl(method = "cv", number = 5, returnResamp = "all")
model <- caret::train(Value ~ ., data = train, "lm",trControl= ctrl1)
pred <- predict(model, test)
results<- postResample(pred =  pred, obs = test$Value)

summary(model)
results

sink("D.Race.1.txt")
print(summary(model))
sink()

sink("D.Race.2.txt")
print(results)
sink()

########### State
rm(list = ls()) # clear global environment
graphics.off() # close all graphics
gc()
setwd("")

library("readxl")

my_data <- read_excel("Depression.xlsx", sheet = "State")[,c(4,5,7)]
str(my_data)

data.variables<-my_data[,1]
data.variables[,] <- lapply(my_data[,1], factor) 
dummy<- data.frame(predict(caret::dummyVars(" ~ .", data = data.variables, fullRank=F),
                           newdata = data.variables))
my_data<-cbind(dummy[,-24],my_data[,c(2,3)])
#minnesota as reference


test<- my_data[which(my_data$Week<=12),]
train<-my_data[which(my_data$Week<=12),]

library(caret)
ctrl1 <- trainControl(method = "cv", number = 5, returnResamp = "all")
model <- caret::train(Value ~ ., data = train, "lm",trControl= ctrl1)
pred <- predict(model, test)
results<- postResample(pred =  pred, obs = test$Value)

summary(model)
results

sink("D.State.1.txt")
print(summary(model))
sink()

sink("D.State.2.txt")
print(results)
sink()



########### USA
rm(list = ls()) # clear global environment
graphics.off() # close all graphics
gc()
setwd("")

library("readxl")

my_data <- read_excel("Depression.xlsx", sheet = "USA")[,c(5,7)]
str(my_data)


test<- my_data[which(my_data$Week<=12),]
train<-my_data[which(my_data$Week<=12),]

library(caret)
ctrl1 <- trainControl(method = "cv", number = 5, returnResamp = "all")
model <- caret::train(Value ~ ., data = train, "lm",trControl= ctrl1)
pred <- predict(model, test)
results<- postResample(pred =  pred, obs = test$Value)

summary(model)
results

sink("D.US.1.txt")
print(summary(model))
sink()

sink("D.US.2.txt")
print(results)
sink()


