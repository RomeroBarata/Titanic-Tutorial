organizeData <- function(titanic){
  train <- filter(titanic, !is.na(Survived))
  train_median_age <- summarise(train, median(Age, na.rm = TRUE))[[1]]
  train_median_fare <- summarise(train, median(Fare, na.rm = TRUE))[[1]]
  train_mc_embark <- summarise(train, names(which.max(table(Embarked))))[[1]]
  
  titanic %>%
    mutate(Survived = as.factor(Survived), 
           Pclass = as.factor(Pclass), 
           Sex = as.factor(Sex), 
           Age = if_else(is.na(Age), train_median_age, Age), 
           Fare = if_else(is.na(Fare), train_median_fare, Fare), 
           Embarked = if_else(is.na(Embarked), train_mc_embark, Embarked), 
           Embarked = as.factor(Embarked))
}