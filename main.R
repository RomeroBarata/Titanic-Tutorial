## Importando os pacotes necessários -------------------------------------------
library(readr)         # Importar/exportar dados
library(tidyr)         # Organizar/Limpar dados
library(dplyr)         # Manipular os dados de forma eficiente
library(stringr)       # Manipular strings e regex
library(rpart)         # Árvores de decisão
library(randomForest)  # Florestas aleatórias
library(caret)         # Criação de modelos preditivos

## Definindo constantes --------------------------------------------------------
DATA_PATH <- "data"
R_PATH <- "R"

## Source scripts auxiliares ---------------------------------------------------
source(file.path(R_PATH, "data-functions.R"))

## Importar os dados -----------------------------------------------------------
tit_train <- read_csv(file.path(DATA_PATH, "train.csv"))
tit_test  <- read_csv(file.path(DATA_PATH, "test.csv"))
tit_test  <- mutate(tit_test, Survived = NA)

# Organizar os dados
titanic   <- organizeData(rbind(tit_train, tit_test))

## Feature engineering ---------------------------------------------------------
# Criança ou não?
titanic <- titanic %>% mutate(isChild = if_else(Age < 18, 1, 0), 
                              isChild = as.factor(isChild))

# Quantas pessoas na família?
titanic <- titanic %>% mutate(familySize = SibSp + Parch + 1)

# Titulação?
names_regex <- "(.*), (.*?)\\. (.*)"
names_title <- str_match(titanic$Name, names_regex)[, 3]
names_title[names_title %in% c("Mme", "Mlle")] <- "Mlle"
names_title[names_title %in% c("Dona", "Lady", "the Countess")] <- "Lady"
names_title[names_title %in% 
              c("Capt", "Don", "Jonkheer", "Major", "Sir")] <- "Sir"
titanic <- titanic %>% mutate(Title = as.factor(names_title))

# Discretizar o preço do ticket (variável Fare)
titanic$DFare <- '>30'
titanic$DFare[titanic$Fare < 30 & titanic$Fare >= 20] <- '20-30'
titanic$DFare[titanic$Fare < 20 & titanic$Fare >= 10] <- '10-20'
titanic$DFare[titanic$Fare < 10] <- '<10'
titanic <- titanic %>% mutate(DFare = as.factor(DFare))

# Dividir novamente em treino e teste
tit_train <- filter(titanic, !is.na(Survived))
tit_test  <- filter(titanic, is.na(Survived))

## Análise Exploratória --------------------------------------------------------
# Resumo sobre todas as variáveis
summary(tit_train)

# Informação acerca dos 15 primeiros/últimos passageiros
head(tit_train, 15)
tail(tit_train, 15)

# Quantidade de valores únicos de cada variável
vapply(tit_train, function(x) length(unique(x)), numeric(1))

# Quantas pessoas de cada sexo sobreviveram?
(survival_sex <- table(tit_train$Sex, tit_train$Survived))
prop.table(survival_sex)
prop.table(survival_sex, 1)
prop.table(survival_sex, 2)

## Árvores de decisão ----------------------------------------------------------
tree1 <- rpart(Survived ~ Sex + Age + Title + isChild, 
               data = tit_train, 
               method = "class")

## Florestas aleatórias --------------------------------------------------------
rf1 <- randomForest(Survived ~ Title + Age + DFare + Pclass, 
                    data = tit_train, 
                    ntree = 100, 
                    importance = TRUE)

## caret -----------------------------------------------------------------------
fit_control <- trainControl(method = "repeatedcv",  # k-fold cv
                            number = 10,            # 10 folds
                            repeats = 5)            # 5 repetições

set.seed(10345)
rf_fit <- train(Survived ~ Sex + Age + Pclass + Fare + Embarked + Title, 
                data = tit_train, 
                method = "rf", 
                ntree = 100, 
                metric = "Accuracy", 
                trControl = fit_control, 
                tuneLength = 3)

rf_preds <- predict(rf_fit, newdata = tit_test)

## Exportar as predições para submissão ----------------------------------------
kaggle_sub <- data_frame(PassengerId = tit_test$PassengerId, 
                         Survived = rf_preds)
write_csv(kaggle_sub, path = "submissions/my-sub-01.csv")
