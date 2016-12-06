organizeData <- function(titanic){
  train <- filter(titanic, !is.na(Survived))
  train_median_age <- summarise(train, median(Age, na.rm = TRUE))[[1]]
  train_median_fare <- summarise(train, median(Fare, na.rm = TRUE))[[1]]
  train_mc_embark <- summarise(train, names(which.max(table(Embarked))))[[1]]
  
  titanic %>%
    replace_na(list(Age = train_median_age, 
                    Fare = train_median_fare, 
                    Embarked = train_mc_embark)) %>% 
    mutate(Survived = as.factor(Survived), 
           Pclass = as.factor(Pclass), 
           Sex = as.factor(Sex), 
           Embarked = as.factor(Embarked))
}