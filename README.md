# Predicting-Employee-Attrition

---
title: "Predicting Employee Attrition"
author: "Aaron T. Camacho"
date: "July 5, 2018"
output: html_document
---
  
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)



library(data.table)
library(ggplot2)
library(highcharter)
library(caret)
library(dplyr)
library(caTools)




```


```{r warnings = FALSE, echo = FALSE, cache = FALSE, message = FALSE}
dataset_attrition <- read.csv("~/Desktop/r_intro/employee_attrition.csv")
```

```{r warnings = FALSE, echo = FALSE, cache = FALSE, message = FALSE}
dim(dataset_attrition)
```
## Structure

```{r warnings = FALSE, echo = FALSE, cache = FALSE, message = FALSE}
str(dataset_attrition)
```

## Summary

```{r warnings = FALSE, echo = FALSE, cache = FALSE, message = FALSE}
summary(dataset_attrition)
```

## Data Cleaning

###Rename Column"ï..Age" to "Age"
```{r warnings = FALSE, echo = FALSE, cache = FALSE, message = FALSE}
colnames(dataset_attrition)[colnames(dataset_attrition)=="ï..Age"] <- "Age"
```

### Find missing values if any
```{r warnings = FALSE, echo = FALSE, cache = FALSE, message = FALSE}
sapply(dataset_attrition, function(x) sum(is.na(x))) 
```

### Discover Correlation between Numneric Variables
```{r warnings = FALSE, echo = FALSE, cache = FALSE, message = FALSE}
library(corrplot)
numeric_variables <- sapply(dataset_attrition, is.numeric)
matrix <- cor(dataset_attrition[,numeric_variables])
corrplot(matrix, main="\n\nCorrelation for Numerical Variables", method="number")
```

### Remove Variables due to strong correlation between variables
```{r warnings = FALSE, echo = TRUE, cache = FALSE, message = FALSE}
# attrition$JobLevel <- NULL
dataset_attrition$PerformanceRating <- NULL
dataset_attrition$YearsInCurrentRole <- NULL
dataset_attrition$TotalWorkingYears <- NULL
dataset_attrition$StandardHours <- NULL
dataset_attrition$YearsWithCurrManager <- NULL
dataset_attrition$YearsSinceLastPromotion <- NULL
```


# Remove columns not needed for analysis
```{r warnings = FALSE, echo = TRUE, cache = FALSE, message = FALSE}
dataset_attrition$EmployeeCount <- NULL
dataset_attrition$EmployeeNumber <- NULL
dataset_attrition$Over18 <- NULL
dataset_attrition$DailyRate <- NULL
dataset_attrition$HoulyRate <- NULL
dataset_attrition$YearsSinceLastPromotion <- NULL
dataset_attrition$Education <- NULL
```

### Re-run Coorplot to Discover Correlation between Numneric Variables after removal of variables
```{r warnings = FALSE, echo = FALSE, cache = FALSE, message = FALSE}
library(corrplot)
numeric_variables <- sapply(dataset_attrition, is.numeric)
matrix <- cor(dataset_attrition[,numeric_variables])
corrplot(matrix, main="\n\nCorrelation for Numerical Variables", method="number")
```

## Some visualisations {.tabset}


### Distribution of Age

```{r warnings = FALSE, echo = FALSE, cache = FALSE, message = FALSE}
ggplot(dataset_attrition, aes(x =  Age)) +
  geom_density(fill = "red") +
  ggtitle("Age density Distribution")
```

### Distribution of Age/Overtime 
```{r warnings = FALSE, echo = FALSE, cache = FALSE, message = FALSE}
ggplot(dataset_attrition, aes(x =  Age, fill = OverTime, 
                    colour = OverTime, alpha = .3)) +
  geom_density()
```

### Explanatory Variable - Attrition 

```{r warnings = FALSE, echo = FALSE, cache = FALSE, message = FALSE}
ggplot(dataset_attrition, aes(x =  Age, fill = Attrition, 
                    colour = Attrition, alpha = .3)) +
  geom_density()

```

### Attrition - Monthly Income 

```{r warnings = FALSE, echo = FALSE, cache = FALSE, message = FALSE}
ggplot(dataset_attrition, aes(x  = MonthlyIncome, fill = Attrition, 
                    colour = Attrition, alpha = .3)) +
  geom_density()
```


### Attrition - log(Monthly Income)

```{r warnings = FALSE, echo = FALSE, cache = FALSE, message = FALSE}

ggplot(dataset_attrition, aes(x =  log(MonthlyIncome), fill = Attrition, 
                    colour = Attrition, alpha = .3)) +
  geom_density() + ggtitle("")


```

### YearsAtCompany - Attrition 


```{r warnings = FALSE, echo = FALSE, cache = FALSE, message = FALSE}
ggplot(dataset_attrition, aes(x = YearsAtCompany, fill = Attrition, 
                    colour = Attrition, alpha = .3)) +
  geom_density()

```

### Count - EducationField


```{r warnings = FALSE, echo = FALSE, cache = FALSE, message = FALSE}
attrition_edufield <- dataset_attrition %>%
  select(Attrition, EducationField) %>%
  group_by(Attrition, EducationField) %>%
  summarize(count = n())


library(DT)
datatable(attrition_edufield)
```





```{r warnings = FALSE, echo = FALSE, cache = FALSE, message = FALSE}
ggplot(attrition_edufield, aes(x = EducationField, 
                               y = count,
                               fill = EducationField, colour = EducationField))  + 
  geom_bar(stat = "identity") + facet_wrap(~Attrition)

```

```{r warnings = FALSE, echo = FALSE, cache = FALSE, message = FALSE}

ggplot(dataset_attrition, aes(x = Age, 
                    fill = EducationField, 
                    colour = EducationField, alpha = .3)) + 
  geom_density()

```

```{r warnings = FALSE, echo = FALSE, cache = FALSE, message = FALSE}
ggplot(dataset_attrition, aes(x =  Age, 
                    fill = BusinessTravel, 
                    colour = BusinessTravel, alpha = .3)) + 
  geom_density()

```




###

```{r warnings = FALSE, echo = FALSE, cache = FALSE, message = FALSE}
dataset <- fread("~/Desktop/r_intro/employee_attrition.csv")
```




```{r warnings = FALSE, echo = FALSE, cache = FALSE, message = FALSE}
prop.table(ftable(dataset[, OverTime]))
ftable(dataset[, OverTime])
unique(dataset[, OverTime])

```
# Split into training/test dataset



```{r}

dataset[, Over18 := NULL]



ind_num <-  sapply(train, is.character)
ind_num

names_char <- names(train)[which(ind_num == 1)]
names_char

for (f in names_char) {
  if (class(dataset[[f]])=="character") {
    levels <- unique(c(dataset[[f]]))
    dataset[[f]] <- as.numeric(factor(dataset[[f]], levels=levels))
  }
}



```


```{r}
index <- sample.split(1:nrow(dataset), SplitRatio = 0.7)

train <- subset(dataset, index == TRUE)
test  <- subset(dataset, index == FALSE)
unique(train[, Attrition])
train[, c("EmployeeCount", "StandardHours") := NULL]

```


# Control parameters part
```{r}

control <- trainControl(method = "repeatedcv", 
                        number = 5, 
                        summaryFunction = twoClassSummary, 
                        classProbs = TRUE)
```






# eXtreme Gradient Boosting



```{r}

xgb.grid2 <- expand.grid(nrounds = c(50), 
                        max_depth = c(10, 20), 
                        eta = c(0.01, 0.1), 
                        gamma = c(0.1, 1), 
                        colsample_bytree = c(0.75), 
                        min_child_weight = c(0.1), 
                        subsample = c(0.7))



train[, c("EmployeeCount", "StandardHours") := NULL]

xgb.start <- proc.time()

xgb.train <- train(Attrition ~., 
                   data = train, 
                   method = "xgbTree", 
                   metric = "auc", 
                   trControl = control, 
                   tuneGrid = xgb.grid2, 
                   preProcess =  c("scale", "center"))


xgb.stop <- proc.time() - xgb.start
xgb.train
xgb.stop



```
ttttt
