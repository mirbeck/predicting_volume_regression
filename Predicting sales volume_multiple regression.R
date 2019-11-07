#Importing the Data

library(readr)
existingproducts <- read_csv("~/Desktop/ubiqum/Data Science projects/DS2/Task#3/existingproductattributes2017.csv")
View(existingproducts)

newproductattributes2017 <- read_csv("~/Desktop/ubiqum/Data Science projects/DS2/Task#3/newproductattributes2017.csv")
View(newproductattributes2017)

#Pre-proccessing the Data

existingproducts <- existingproducts[-c(34,35,36,37,39,40,41), ] #Deleting similar rows "Extended Warranty" 
View(existingproducts)


#Using dummyVars
library(caret)
newDataFrame <- dummyVars(" ~ .", data = existingproducts)
readyData <- data.frame(predict(newDataFrame, newdata = existingproducts))

new_products <- dummyVars(" ~ .", data = newproductattributes2017)
readyData_1 <- data.frame(predict(new_products, newdata = newproductattributes2017))

is.na(readyData_1) #Checking dataframe for missing values

#Using mice package for missing values
library(mice)
p<- function(x) {sum(is.na(x))/length(x)*100}
apply(readyData, 2, p)
md.pattern(readyData)

impute<-mice(readyData[ ,],m=3, seed = 123)
print(impute)

clean_df <- complete(impute, 1)
View(clean_df)
str(clean_df)

# Creating correlation matrix for clean_df and visualization
#install.packages("corrplot")
library(corrplot)
corrData <- cor(clean_df)
corrplot(corrData)

#After reviewing correlation matrix "4StarReview", "3StarReview" and "PositiveService Review" 
# was chosen as a main features for linear model

# Removing features aren't needed and creating new dataset
library(dplyr)
df <-select(clean_df, -c(13,14,15,18,19,21,22,23,24,25,26,27,28)) #Cleaning rows with bad correlation
View(df)

df1 <-select(readyData_1, -c(13,14,15,18,19,21,22,23,24,25,26,27,28)) #Cleaning rows with bad correlation
View(df1)
#Creating testing and training set

pacman :: p_load(caret,reshape, ggplot2, dplyr)


library(e1071)
set.seed(100)
in_training <- createDataPartition(df$Volume, p = 0.7, list = F)
train <- df[in_training,]
test <- df[-in_training,]

a <- c("Volume ~ x4StarReviews + PositiveServiceReview", "Volume ~ x3StarReviews + PositiveServiceReview")
b <- c("knn", "svmLinear", "rf")
compare_var_mod <- c()
names_var <- c()


for ( i in a) {
  for (j in b) {
    
    model <- train(formula(i), data = train, method = j)
    
    pred <- predict(model, newdata = test)
    
    pred_metric <- postResample(test$Volume, pred)
    
    compare_var_mod <- cbind(compare_var_mod , pred_metric)
    
    names_var <- append(names_var,paste(i,j))
    
  }
  
}
colnames(compare_var_mod) <- names_var

compare_var_mod

compare_var_mod_melt <- melt(compare_var_mod, varnames = c("metric", "model"))
compare_var_mod_melt <- as.data.frame(compare_var_mod_melt)
compare_var_mod_melt

ggplot(compare_var_mod_melt, aes(x=model, y=value))+
  geom_col()+
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  facet_grid(metric~., scales="free")+
  theme(legend.position='top')

#After comparing parameters of 3 models I choose as a model "random forest" and as a predictors: 4StarReviews + PositiveServiceReview
# In "model" stored "rf", so I can just use "model" for new data.

prediction <- predict(model, newdata = df1)
prediction

#Adding prediction to new data set

output <- newproductattributes2017 
output$predictions <- prediction
View(output$predictions)

#Creating new csv file 
write.csv(output, file="final_output.csv", row.names = TRUE)

