# Predicting-Employee-Attrition

# Packages installed
install.packages("ggpubr")
install.packages("rmarkdown")
install.packages("corrplot")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("ggthemes")
install.packages("caret")
install.packages("randomForest")
install.packages("party")
install.packages("stringi")
install.packages("Hmisc")
install.packages("pastecs")
install.packages("dplyr")
install.packages("olsrr")
install.packages("devtools")
# install.packages("lmtest")

# Library List
library(plyr)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(caret)
library(MASS)
library(randomForest)
library(party)
library(Hmisc)
library(pastecs)
library(psych)
library(dplyr)
library(grid)


# Load the Telco Churn Data
attrition <- read.csv("~/Desktop/r_intro/employee_attrition.csv")


############################### EDA #######################################################

# Variables
colnames(attrition)

"
 ï..Age                   Attrition                BusinessTravel          
 DailyRate                Department               DistanceFromHome        
 Education                EducationField           EmployeeCount           
 EmployeeNumber           EnvironmentSatisfaction  Gender                  
 HourlyRate               JobInvolvement           JobLevel                
 JobRole                  JobSatisfaction          MaritalStatus           
 MonthlyIncome            MonthlyRate              NumCompaniesWorked      
 Over18                   OverTime                 PercentSalaryHike       
 PerformanceRating        RelationshipSatisfaction StandardHours           
 StockOptionLevel         TotalWorkingYears        TrainingTimesLastYear   
 WorkLifeBalance          YearsAtCompany           YearsInCurrentRole      
 YearsSinceLastPromotion  YearsWithCurrManager 
"

## Display basic distribution of variables and view data 
str(attrition)
summary(attrition)
class(attrition)
head(attrition)
View(attrition)

#Rename Column"ï..Age" to "Age"
colnames(attrition)[colnames(attrition)=="ï..Age"] <- "Age"
# colnames(attrition)[1] <- "Age" # Renaming the column
table(attrition$Age)

# Attrition
ggplot(attrition,aes(Attrition,fill=Attrition))+geom_bar()
prop.table(table(attrition$Attrition))
summary(attrition$Attrition)

# Bar Plots 1: Age, BusinessTravel, DailyRate, Department
p1 <- ggplot(attrition,aes(Age,fill=Attrition, alpha = 0.03))+geom_density()
p2 <- ggplot(attrition,aes(BusinessTravel,fill=Attrition))+geom_bar()
p3 <- ggplot(attrition,aes(DailyRate,Attrition))+geom_point(size=5,alpha = 0.03, col="blue")
p4 <- ggplot(attrition,aes(Department,fill = Attrition))+geom_bar()
grid.arrange(p1,p2,p3,p4,ncol=2,top = "Figure: Bar Plots 1")

# Age: the majority of employees who leave approx. around 31 Years of age.
# Business Travel: Employees who travel, are more likely to leave.
# Daily Rate: There is no significant indications that can be found. 
# Department: R&D and Sales is where the most attrition occurred. However, it is important to note that the HR Department is proportionally smaller compared to the other departments. 

# Bar Plots 2: DistanceFromHome, Education, EducationField, EmployeeCount
p5 <- ggplot(attrition,aes(DistanceFromHome,fill=Attrition))+geom_bar()
p6 <- ggplot(attrition,aes(Education,fill=Attrition))+geom_bar()
p7 <- ggplot(attrition,aes(EducationField,fill=Attrition))+geom_bar()
p8 <- ggplot(attrition,aes(EmployeeCount,Attrition))+geom_point(size=5,alpha = 0.03, col="blue")
grid.arrange(p5,p6,p7,p8,ncol=2,top = "Figure: Bar Plots 2")

# Distance From Home: An unexpected result where employees who lived closer where more apt to leave. 
# Education:1 = "Below College", 2 = "College", 3 = "Bachelor", 4 = "Master", 5 = "Doctor" . Those with a bachelors degree have the highest attrition.  Important to note that there are very few employees with a doctorate degree.  May have an impact on the amount that left in the Doctorate category.
# Education Field: AS we saw in the Departments graph, those in an HR Field are less likely to leave. Again, this may be due to the low number of individuals in this group. 
# Employee Count: No significant findings. All numbers in variable are 1.  

# Bar Plots 3: EmployeeNumber, EnvironmentSatisfaction, Gender, HourlyRate, JobInvolvement, JobLevel
p9 <- ggplot(attrition,aes(EmployeeNumber,Attrition))+geom_point(size=5,alpha = 0.03, col="blue")
p10 <- ggplot(attrition,aes(EnvironmentSatisfaction,fill=Attrition))+geom_bar()
p11 <- ggplot(attrition,aes(Gender,fill=Attrition))+geom_bar()
p12 <- ggplot(attrition,aes(HourlyRate,fill=Attrition))+geom_bar()
p13 <- ggplot(attrition,aes(JobInvolvement,fill=Attrition))+geom_bar()
p14 <- ggplot(attrition,aes(JobLevel,fill=Attrition))+geom_bar()
grid.arrange(p9,p10,p11,p12,p13,p14,ncol=2,top = "Figure: Bar Plots 3")

# Employee Number: No significant findings.
# Environment Satisfaction:  1 = "Low",  2 = "Medium",  3 = "High", 4 = "Very High". All levels are nearly the same. No significnat findings from graph.
  prop.table(table(attrition$EnvironmentSatisfaction, attrition$Attrition) ) # Again no significant findings.
# Gender: Males are more likey to leave. However, there is 60% males and 40% female distribution which may be impacting the results. 
  prop.table(table(attrition$Gender, attrition$Attrition) )
  table(attrition$Gender)/length(attrition$Gender)
# HourlyRate : No Significant findings. Also, there seems to be no direct relation to DailyRate.
# Job Involvement: 1 = "Low",  2 = "Medium",  3 = "High", 4 = "Very High". It seems that the majority of employees who don't leave are either Very Highly involved or Low Involved in their Jobs. This may be coorelated with the amount of pay they recieve for the output of work performed.
# JobLevel: An infered meaning of ratings could be: 1 = "Entry level", 2 = "Junior Level", 3 = "Junior Manager", " 4 = "Senior level", 5 = "Senior Manger Level" but it is not sure. But, by looking at the graph it is clear that the high the job level the more unlikely an employee is to leave.

# Bar Plots 4:JobRole
p15 <- ggplot(attrition,aes(JobRole,fill=Attrition))+geom_bar()
grid.arrange(p15,ncol=1,top = "Figure: Bar Plots 4")
prop.table(table(attrition$JobRole, attrition$Attrition))

# Job Role: Proportions could be influenced to group size differences. However, the graph indicates that if an employee has one of the  following job roles he/she is more likely to leave; Lab Tech, Research Scientist, Sales Executive, Sales Rep.
  prop.table(table(attrition$JobRole, attrition$Attrition)) #Cooberates above statement. 

# Bar Plots 5: JobSatisfaction, MaritalStatus, MonthlyIncome
p16 <- ggplot(attrition,aes(JobSatisfaction,fill=Attrition))+geom_bar()+facet_grid(~Attrition)
p17 <- ggplot(attrition,aes(MaritalStatus,fill=Attrition))+geom_bar()
p18 <- ggplot(attrition,aes(MonthlyIncome,fill=Attrition))+geom_density()+facet_grid(~Attrition)
grid.arrange(p16,p17,p18,ncol=2,top = "Figure: Bar Plots 5")

# Job Satisfaction: 1 = "Low",  2 = "Medium",  3 = "High", 4 = "Very High". Though attrition levels stay mostly the same, the amount of employees who did not leave increases with Job Satisfaction.
  prop.table(table(attrition$JobSatisfaction, attrition$Attrition)) #Cooberates above statement. 
# Marital Status:Employees who are single are more likely to leave whereas, employees who are divorced are more likely to not leave.
# Monthly Income: There are higher levels of attrition among the lower wage earners. 
  mi1 <- ggplot(attrition,aes(MonthlyIncome, Attrition))+geom_point()
  mi2 <- ggplot(attrition,aes(MonthlyIncome))+geom_density()
  grid.arrange(mi1,mi2,ncol=2,top = "Figure: Monthly Income")
  
# Bar Plots 6: MonthlyRate, NumCompaniesWorked, Over18, OverTime
p19 <- ggplot(attrition,aes(MonthlyRate,fill=Attrition, alpha = 0.03))+geom_density()
p20 <- ggplot(attrition,aes(NumCompaniesWorked,fill=Attrition))+geom_bar()
p21 <- ggplot(attrition,aes(Over18,Attrition))+geom_point(size=5,alpha = 0.03, col="blue")
p22 <- ggplot(attrition,aes(OverTime,fill=Attrition))+geom_bar()
grid.arrange(p19,p20,p21,p22,ncol=2,top = "Figure: Bar Plots 6")

# Monthly Rate: No Significant findings. Also, there seems to be little to no correlation to the Monthly Income variable.
# Number of Companies Worked: It is clear the if an employee has worked for only 1 company he/she is more likely to leave.
# Over18: Not a significant variable.  All employees are over 18 years old.
# Over Time: Though attrition first appears to be nearly equal, a larger Proportion of employees working overtime are leaving.
  prop.table(table(attrition$OverTime, attrition$Attrition)) #Cooberates above statement. 

# Bar Plots 7:PercentSalaryHike, PerformanceRating, RelationshipSatisfaction, StandardHours
p23 <- ggplot(attrition,aes(PercentSalaryHike,fill=Attrition))+geom_bar()+facet_grid(~Attrition)
p24 <- ggplot(attrition,aes(PerformanceRating,fill = Attrition))+geom_bar()
p25 <- ggplot(attrition,aes(RelationshipSatisfaction,fill = Attrition))+geom_bar()
p26 <- ggplot(attrition,aes(StandardHours,Attrition))+geom_point(size=5,alpha = 0.03, col="blue")
grid.arrange(p23,p24,p25,p26,ncol=2,top = "Figure: Bar Plots 7")

# Percent Salary Hike: Lower the percent salary hike equals more likely to leave. 
# Performance Rating: 1 = "Low", 2 = "Good", 3 = "Excellent", 4 = "Outstanding". As expected, lower the performance rating more likely an employee is to leave.
# Relationship Satisfaction: 1 = "Low",  2 = "Medium",  3 = "High", 4 = "Very High". Higher the relationship satisfaction the more employees don't leave. 
# Standard Hours: Not a significant variable. All employees have standard hours of 80.

# Bar Plots 8:StockOptionLevel, TotalWorkingYears, TrainingTimesLastYear, WorkLifeBalance
p27 <- ggplot(attrition,aes(StockOptionLevel,fill = Attrition))+geom_bar()
p28 <- ggplot(attrition,aes(TotalWorkingYears,fill = Attrition))+geom_bar()
p29 <- ggplot(attrition,aes(TrainingTimesLastYear,fill = Attrition))+geom_bar()
p30 <- ggplot(attrition,aes(WorkLifeBalance,fill = Attrition))+geom_bar()
grid.arrange(p27,p28,p29,p30,ncol=2,top = "Figure: Bar Plots 8")

# Stock Option Level: Larger the stock option level less likely an employee is to leave.  It is expected that there would be more 0 and 1 levels because most employees would have very little to no stock options. 
# Total Working Years: The more years of working the less likely you are to leave.  1 year highly likely to leave. It appears years 0 to 12 have a high chance of attrition.
# Training Times Last Year: 2 to 3 trainings seem to indicate a higher chance of attrition. Though the majority of employees seem to have 2 or 3 trainings. 
# Work Life Balance: 1 = "Bad", 2 = "Good", 3 = "Better", 4 = "Best". Those that have a higher work life balance are more likely to not leave.
  prop.table(table(attrition$WorkLifeBalance, attrition$Attrition)) #Cooberates above statement. 

# Bar Plots 9: YearsAtCompany, YearsInCurrentRole, YearsSinceLastPromotion, YearsWithCurrentManager
p31 <- ggplot(attrition,aes(YearsAtCompany,fill = Attrition))+geom_bar()
p32 <- ggplot(attrition,aes(YearsInCurrentRole,fill = Attrition))+geom_bar()
p33 <- ggplot(attrition,aes(YearsSinceLastPromotion,fill = Attrition))+geom_bar()
p34 <- ggplot(attrition,aes(YearsWithCurrManager,fill = Attrition))+geom_bar()
grid.arrange(p31,p32,p33,p34,ncol=2,top = "Figure: Bar Plots 9")

# Years at Company: Employees with less tenure are leaving more.  However, that is also where the majority of employee tenure is, 0 to 10 years. 
# Years In Current Role: Employees with less years in role are leaving.  However, we do not know if they just left for another position within the same company. 
# Years Since Last Promotion: It appears that those that have recently got a new promotion, 0 to 3 years, are more likely to leave. 
# Years With Current Manager: Mangers play a large role in retention. Increased years with manager decreases chances of attrition.



######New Variables
# Unique Variable Creation
attrition$AverageTenurePerJob <- ifelse(attrition$NumCompaniesWorked!=0, attrition$TotalWorkingYears/attrition$NumCompaniesWorked,0)
attrition$YearsWithoutPromotion_InCurrentRole <- attrition$YearsInCurrentRole - attrition$YearsSinceLastPromotion
attrition$YearsWithoutPromotion_WithCurrentManager <- attrition$YearsWithCurrManager - attrition$YearsSinceLastPromotion

averagetenurePerJob_Plot <- ggplot(attrition,aes(AverageTenurePerJob, fill=Attrition, alpha = 0.3))+geom_density()
ywopcurrole_Plot <- ggplot(attrition,aes(YearsWithoutPromotion_InCurrentRole, fill=Attrition))+geom_bar()
ywopcurmanager_Plot <- ggplot(attrition,aes(YearsWithoutPromotion_WithCurrentManager, fill=Attrition))+geom_bar()
grid.arrange(averagetenurePerJob_Plot, ywopcurrole_Plot, ywopcurmanager_Plot, ncol=2,top = "Figure 8 - Average Tenure & Years w/o Promotion")



################### Binning ##################

attrition$AgeGroup <- with(attrition,
                           ifelse(Age>55,8,
                                  ifelse(Age>50,7,
                                         ifelse(Age>45,6,
                                                ifelse(Age>40,5,
                                                       ifelse(Age>35,4,
                                                              ifelse(Age>30,3,
                                                                     ifelse(Age>25,2,1)))))))) 

attrition$DistanceGroup <- with(attrition,
                                ifelse(DistanceFromHome>25,6,
                                       ifelse(DistanceFromHome>20,5,
                                              ifelse(DistanceFromHome>15,4,
                                                     ifelse(DistanceFromHome>10,3,
                                                            ifelse(DistanceFromHome>5,2,1)))))) 

attrition$YearsWithManagerGroup <- with(attrition,
                                        ifelse(YearsWithCurrManager>15,5,
                                               ifelse(YearsWithCurrManager>10,4,
                                                      ifelse(YearsWithCurrManager>5,3,
                                                             ifelse(YearsWithCurrManager>2,2,1))))) 

attrition$AverageTenurePerJob_Group <- with(attrition,
                                            ifelse(AverageTenurePerJob>35,9,
                                                   ifelse(AverageTenurePerJob>30,8,
                                                          ifelse(AverageTenurePerJob>25,7,
                                                                 ifelse(AverageTenurePerJob>20,6,
                                                                        ifelse(AverageTenurePerJob>15,5,
                                                                               ifelse(AverageTenurePerJob>10,4,
                                                                                      ifelse(AverageTenurePerJob>5,3,
                                                                                             ifelse(AverageTenurePerJob>2,2,1))))))))) 

attrition$YearsWithoutPromotion_InCurrentRole_group <- with(attrition,
                                                            ifelse(YearsWithoutPromotion_InCurrentRole>10,5,
                                                                  ifelse(YearsWithoutPromotion_InCurrentRole>5,4,
                                                                        ifelse(YearsWithoutPromotion_InCurrentRole>0,3,
                                                                              ifelse(YearsWithoutPromotion_InCurrentRole>-5,2,1))))) 


attrition$YearsWithoutPromotion_WithCurrentManager_group <- with(attrition,
                                                                 ifelse(YearsWithoutPromotion_WithCurrentManager>10,6,
                                                                        ifelse(YearsWithoutPromotion_WithCurrentManager>5,5,
                                                                               ifelse(YearsWithoutPromotion_WithCurrentManager>0,4,
                                                                                      ifelse(YearsWithoutPromotion_WithCurrentManager>-5,3,
                                                                                             ifelse(YearsWithoutPromotion_WithCurrentManager>-10,2,1)))))) 

attrition$TotalWorkingYears_Group <- with(attrition,ifelse(TotalWorkingYears>35,9,
                                                           ifelse(TotalWorkingYears>30,8,
                                                                  ifelse(TotalWorkingYears>25,7,
                                                                         ifelse(TotalWorkingYears>20,6,
                                                                                ifelse(TotalWorkingYears>15,5,
                                                                                       ifelse(TotalWorkingYears>10,4,
                                                                                              ifelse(TotalWorkingYears>5,3,
                                                                                                     ifelse(TotalWorkingYears>2,2,1)))))))))

attrition$NumComppaniesWorked_Group <- with(attrition,
                                            ifelse(NumCompaniesWorked>4,3,
                                                   ifelse(NumCompaniesWorked>2,2,1))) 


attrition$YearsAtCompany_Group <- with(attrition,ifelse(YearsAtCompany>35,9,
                                                           ifelse(YearsAtCompany>30,8,
                                                                  ifelse(YearsAtCompany>25,7,
                                                                         ifelse(YearsAtCompany>20,6,
                                                                                ifelse(YearsAtCompany>15,5,
                                                                                       ifelse(YearsAtCompany>10,4,
                                                                                              ifelse(YearsAtCompany>5,3,
                                                                                                     ifelse(YearsAtCompany>2,2,1)))))))))



######------Grouped/Binned Variables
# Bar Plots 10: AgeGroup, DistanceGroup, YearsWithManagerGroup, AverageTenurePerJob_Group 
p35 <- ggplot(attrition,aes(AgeGroup, fill = Attrition, alpha = 0.3))+geom_bar()
p36 <- ggplot(attrition,aes(DistanceGroup, fill = Attrition, alpha = 0.3))+geom_bar()
p37 <- ggplot(attrition,aes(YearsWithManagerGroup, fill = Attrition, alpha = 0.3))+geom_bar()
p38 <- ggplot(attrition,aes(AverageTenurePerJob_Group, fill = Attrition, alpha = 0.3))+geom_bar()
grid.arrange(p35,p36,p37,p38,ncol=2,top = "Figure: Bar Plots 10")

# Bar Plots 11: YearsWithoutPromotion_InCurrentRole_group, YearsWithoutPromotion_WithCurrentManager_group, TotalWorkingYears_Group, NumComppaniesWorked_Group
p39 <- ggplot(attrition,aes(YearsWithoutPromotion_InCurrentRole_group, fill = Attrition, alpha = 0.3))+geom_bar()
p40 <- ggplot(attrition,aes(YearsWithoutPromotion_WithCurrentManager_group, fill = Attrition, alpha = 0.3))+geom_bar()
p41 <- ggplot(attrition,aes(TotalWorkingYears_Group, fill = Attrition, alpha = 0.3))+geom_bar()
p42 <- ggplot(attrition,aes(NumComppaniesWorked_Group, fill = Attrition, alpha = 0.3))+geom_bar()
grid.arrange(p39,p40,p41,p42,ncol=2,top = "Figure: Bar Plots 11")

# Bar Plots 12: YearsWithCompany_Group
p43 <- ggplot(attrition,aes(YearsAtCompany_Group, fill = Attrition, alpha = 0.3))+geom_bar()
grid.arrange(p43,ncol=1,top = "Figure: Bar Plots 12")



################################## Data Cleaning #########################################
## Find missing values
sapply(attrition, function(x) sum(is.na(x))) # No missing values


#-------------NUmeric Variables---------------
# -----Correlation-----
# Discover Correlation between Numneric Variables
numeric_variables <- sapply(attrition, is.numeric)
matrix <- cor(attrition[,numeric_variables])
corrplot(matrix, main="\n\nCorrelation for Numerical Variables", method="number")

#--------------------------------OUTLIERS----------------------------
boxplot(attrition$YearsAtCompany, horizontal = TRUE, 
        main = "Boxplot of YearsAtCompany", xlab = "YearsAtCompany")
boxplot(attrition$MonthlyIncome, horizontal = TRUE,
        main = "Boxplot of MonthlyIncome", xlab = "MonthlyIncome")

# Variables to Keep
"
Attrition, BusinessTravel, Department, Education, EducationField, 
EnvironmentSatisfaction, Gender, JobInvolvement, JobLevel, JobRole, 
JobSatisfaction, MaritalStatus, NumCompaniesWorked, OverTime, 
PercentSalaryHike, RelationshipSatisfaction, TrainingTimesLastYear, 
WorkLifeBalance, DistanceGroup, YearsWithManagerGroup, 
AverageTenurePerJob_Group, YearsWithoutPromotion_InCurrentRole_group, 
YearsWithoutPromotion_WithCurrentManager_group, NumCompaniesWorked_Group
YearsAtCompany_Group
"
colnames(attrition)
attrition <- attrition[,c(2,3,5,7,8,11,12,14,15,16,17,18,21,23,24,26,30,31,40, 41,42,44)]


############################### Analysis & Statistics ####################################

##GROUPED VARIABLES
# AgeGroup 
str(attrition$AgeGroup)
summary(attrition$AgeGroup)
var(attrition$AgeGroup)
sd(attrition$AgeGroup)
hist(attrition$AgeGroup, main = "Histogram of AgeGroup", xlab = "YearsAtCompany", col = "blue")
boxplot(attrition$AgeGroup, horizontal = TRUE, 
        main = "Boxplot of AgeGroup", xlab = "AgeGroup")
table(attrition$Attrition, attrition$AgeGroup)
prop.table(table(attrition$AgeGroup, attrition$Attrition))

# DistanceGroup 
str(attrition$DistanceGroup)
summary(attrition$DistanceGroup)
var(attrition$DistanceGroup)
sd(attrition$DistanceGroup)
hist(attrition$DistanceGroup, main = "Histogram of DistanceGroup", xlab = "YearsAtCompany", col = "blue")
boxplot(attrition$DistanceGroup, horizontal = TRUE, 
        main = "Boxplot of DistanceGroup", xlab = "DistanceGroup")
table(attrition$Attrition, attrition$DistanceGroup)
prop.table(table(attrition$DistanceGroup, attrition$Attrition))

# YearsWithManagerGroup 
str(attrition$YearsWithManagerGroup)
summary(attrition$YearsWithManagerGroup)
var(attrition$YearsWithManagerGroup)
sd(attrition$YearsWithManagerGroup)
hist(attrition$YearsWithManagerGroup, main = "Histogram of YearsWithManagerGroup", xlab = "YearsAtCompany", col = "blue")
boxplot(attrition$YearsWithManagerGroup, horizontal = TRUE, 
        main = "Boxplot of YearsWithManagerGroup", xlab = "YearsWithManagerGroup")
table(attrition$Attrition, attrition$YearsWithManagerGroup)
prop.table(table(attrition$YearsWithManagerGroup, attrition$Attrition))

# AverageTenurePerJob_Group 
str(attrition$AverageTenurePerJob_Group)
summary(attrition$AverageTenurePerJob_Group)
var(attrition$AverageTenurePerJob_Group)
sd(attrition$AverageTenurePerJob_Group)
hist(attrition$AverageTenurePerJob_Group, main = "Histogram of AverageTenurePerJob_Group", xlab = "YearsAtCompany", col = "blue")
boxplot(attrition$AverageTenurePerJob_Group, horizontal = TRUE, 
        main = "Boxplot of AverageTenurePerJob_Group", xlab = "AverageTenurePerJob_Group")
table(attrition$Attrition, attrition$AverageTenurePerJob_Group)
prop.table(table(attrition$AverageTenurePerJob_Group, attrition$Attrition))

# YearsWithoutPromotion_InCurrentRole_group 
str(attrition$YearsWithoutPromotion_InCurrentRole_group)
summary(attrition$YearsWithoutPromotion_InCurrentRole_group)
var(attrition$YearsWithoutPromotion_InCurrentRole_group)
sd(attrition$YearsWithoutPromotion_InCurrentRole_group)
hist(attrition$YearsWithoutPromotion_InCurrentRole_group, main = "Histogram of YearsWithoutPromotion_InCurrentRole_group", xlab = "YearsAtCompany", col = "blue")
boxplot(attrition$YearsWithoutPromotion_InCurrentRole_group, horizontal = TRUE, 
        main = "Boxplot of YearsWithoutPromotion_InCurrentRole_group", xlab = "YearsWithoutPromotion_InCurrentRole_group")
table(attrition$Attrition, attrition$YearsWithoutPromotion_InCurrentRole_group)
prop.table(table(attrition$YearsWithoutPromotion_InCurrentRole_group, attrition$Attrition))

# YearsWithoutPromotion_WithCurrentManager_group 
str(attrition$YearsWithoutPromotion_WithCurrentManager_group)
summary(attrition$YearsWithoutPromotion_WithCurrentManager_group)
var(attrition$YearsWithoutPromotion_WithCurrentManager_group)
sd(attrition$YearsWithoutPromotion_WithCurrentManager_group)
hist(attrition$YearsWithoutPromotion_WithCurrentManager_group, main = "Histogram of YearsWithoutPromotion_WithCurrentManager_group", xlab = "YearsAtCompany", col = "blue")
boxplot(attrition$YearsWithoutPromotion_WithCurrentManager_group, horizontal = TRUE, 
        main = "Boxplot of YearsWithoutPromotion_WithCurrentManager_group", xlab = "YearsWithoutPromotion_WithCurrentManager_group")
table(attrition$Attrition, attrition$YearsWithoutPromotion_WithCurrentManager_group)
prop.table(table(attrition$YearsWithoutPromotion_WithCurrentManager_group, attrition$Attrition))


# TotalWorkingYears_Group 
summary(attrition$TotalWorkingYears_Group)
var(attrition$TotalWorkingYears_Group)
sd(attrition$TotalWorkingYears_Group)
hist(attrition$TotalWorkingYears_Group, main = "Histogram of TotalWorkingYears_Group", xlab = "YearsAtCompany", col = "blue")
boxplot(attrition$TotalWorkingYears_Group, horizontal = TRUE, 
        main = "Boxplot of TotalWorkingYears_Group", xlab = "TotalWorkingYears_Group")
table(attrition$Attrition, attrition$TotalWorkingYears_Group)
prop.table(table(attrition$TotalWorkingYears_Group, attrition$Attrition))

# NumComppaniesWorked_Group
summary(attrition$NumComppaniesWorked_Group)
var(attrition$NumComppaniesWorked_Group)
sd(attrition$NumComppaniesWorked_Group)
hist(attrition$NumComppaniesWorked_Group, main = "Histogram of NumComppaniesWorked_Group", xlab = "YearsAtCompany", col = "blue")
boxplot(attrition$NumComppaniesWorked_Group, horizontal = TRUE, 
        main = "Boxplot of NumComppaniesWorked_Group", xlab = "NumComppaniesWorked_Group")
table(attrition$Attrition, attrition$NumComppaniesWorked_Group)
prop.table(table(attrition$NumComppaniesWorked_Group, attrition$Attrition))


##CATEGORICAL Variables
# Gender
summary(attrition$Gender)
prop.table(table(attrition$Gender))
table(attrition$Attrition, attrition$Gender)
prop.table(table(attrition$Gender, attrition$Attrition))
ggplot(attrition,aes(Gender,fill=Attrition))+geom_bar()

# Attrition
summary(attrition$Attrition)
prop.table(table(attrition$Attrition))
ggplot(attrition,aes(Attrition,fill=Attrition))+geom_bar()
          
# Business Travel
summary(attrition$BusinessTravel)
prop.table(table(attrition$BusinessTravel))
table(attrition$Attrition, attrition$BusinessTravel)
prop.table(table(attrition$BusinessTravel, attrition$Attrition))
ggplot(attrition,aes(BusinessTravel,fill=Attrition))+geom_bar()

# Department
summary(attrition$Department)
prop.table(table(attrition$Department))
table(attrition$Attrition, attrition$Department)
prop.table(table(attrition$Department, attrition$Attrition))
ggplot(attrition,aes(Department,fill=Attrition))+geom_bar()

# Education Field 
summary(attrition$EducationField)
prop.table(table(attrition$EducationField))
table(attrition$Attrition, attrition$EducationField)
prop.table(table(attrition$EducationField, attrition$Attrition))
ggplot(attrition,aes(EducationField,fill=Attrition))+geom_bar()

# Job Role
summary(attrition$JobRole)
prop.table(table(attrition$JobRole))
table(attrition$Attrition, attrition$JobRole)
prop.table(table(attrition$JobRole, attrition$Attrition))
ggplot(attrition,aes(JobRole,fill=Attrition))+geom_bar()

# Marital Status
summary(attrition$MaritalStatus)
prop.table(table(attrition$MaritalStatus))
table(attrition$Attrition, attrition$MaritalStatus)
prop.table(table(attrition$MaritalStatus, attrition$Attrition))
ggplot(attrition,aes(MaritalStatus,fill=Attrition))+geom_bar()

# Over Time
summary(attrition$OverTime)
prop.table(table(attrition$OverTime))
table(attrition$Attrition, attrition$OverTime)
prop.table(table(attrition$OverTime, attrition$Attrition))
ggplot(attrition,aes(OverTime,fill=Attrition))+geom_bar()



##NUMERIC VARIABLES
# YearsWithoutPromotion_WithCurrentManager
summary(attrition$YearsWithoutPromotion_WithCurrentManager)
var(attrition$YearsWithoutPromotion_WithCurrentManager)
sd(attrition$YearsWithoutPromotion_WithCurrentManager)
hist(attrition$YearsWithoutPromotion_WithCurrentManager, main = "Histogram of YearsWithoutPromotion_WithCurrentManager", xlab = "YearsWithCurrManager", col = "blue")
boxplot(attrition$YearsWithoutPromotion_WithCurrentManager, horizontal = TRUE, 
        main = "Boxplot of YearsWithoutPromotion_WithCurrentManager", xlab = "YearsWithoutPromotion_WithCurrentManager")
ggplot(attrition,aes(YearsWithoutPromotion_WithCurrentManager,fill=Attrition, alpha = 0.03))+geom_density()

# YearsWithCurrManager
summary(attrition$YearsWithCurrManager)
var(attrition$YearsWithCurrManager)
sd(attrition$YearsWithCurrManager)
hist(attrition$YearsWithCurrManager, main = "Histogram of YearsWithCurrManager", xlab = "YearsWithCurrManager", col = "blue")
boxplot(attrition$YearsWithCurrManager, horizontal = TRUE, 
        main = "Boxplot of YearsWithCurrManager", xlab = "YearsWithCurrManager")
ggplot(attrition,aes(YearsWithCurrManager,fill=Attrition, alpha = 0.03))+geom_density()

# HourlyRate
summary(attrition$HourlyRate)
var(attrition$HourlyRate)
sd(attrition$HourlyRate)
hist(attrition$HourlyRate, main = "Histogram of HourlyRate", xlab = "HourlyRate", col = "blue")
boxplot(attrition$HourlyRate, horizontal = TRUE, 
        main = "Boxplot of HourlyRate", xlab = "HourlyRate")
ggplot(attrition,aes(HourlyRate,fill=Attrition, alpha = 0.03))+geom_density()

# Age
summary(attrition$Age)
var(attrition$Age)
sd(attrition$Age)
hist(attrition$Age, main = "Histogram of Age", xlab = "Age", col = "blue")
boxplot(attrition$Age, horizontal = TRUE, 
        main = "Boxplot of Age", xlab = "Age")
ggplot(attrition,aes(Age,fill=Attrition, alpha = 0.03))+geom_density()

# Distance from Home
summary(attrition$DistanceFromHome)
var(attrition$DistanceFromHome)
sd(attrition$DistanceFromHome)
hist(attrition$DistanceFromHome, main = "Histogram of DistanceFromHome", xlab = "DistanceFromHome", col = "blue")
boxplot(attrition$DistanceFromHome, horizontal = TRUE, 
        main = "Boxplot of DistanceFromHome", xlab = "DistanceFromHome")
ggplot(attrition,aes(DistanceFromHome,fill=Attrition, alpha = 0.03))+geom_density()

# Education
summary(attrition$Education)
var(attrition$Education)
sd(attrition$Education)
hist(attrition$Education, main = "Histogram of Education", xlab = "Education", col = "blue")
boxplot(attrition$Education, horizontal = TRUE, 
        main = "Boxplot of Education", xlab = "Education")
ggplot(attrition,aes(Education,fill=Attrition, alpha = 0.03))+geom_bar()


# Enviroment Satisfaction
summary(attrition$EnvironmentSatisfaction)
var(attrition$EnvironmentSatisfaction)
sd(attrition$EnvironmentSatisfaction)
hist(attrition$EnvironmentSatisfaction, main = "Histogram of EnvironmentSatisfaction", xlab = "EnvironmentSatisfaction", col = "blue")
boxplot(attrition$EnvironmentSatisfaction, horizontal = TRUE, 
        main = "Boxplot of EnvironmentSatisfaction", xlab = "EnvironmentSatisfaction")
ggplot(attrition,aes(EnvironmentSatisfaction,fill=Attrition, alpha = 0.03))+geom_bar()


# JobInvolvement
summary(attrition$JobInvolvement)
var(attrition$JobInvolvement)
sd(attrition$JobInvolvement)
hist(attrition$JobInvolvement, main = "Histogram of JobInvolvement", xlab = "JobInvolvement", col = "blue")
boxplot(attrition$JobInvolvement, horizontal = TRUE, 
        main = "Boxplot of JobInvolvement", xlab = "JobInvolvement")
ggplot(attrition,aes(JobInvolvement,fill=Attrition, alpha = 0.03))+geom_bar()


# JobSatisfaction
summary(attrition$JobSatisfaction)
var(attrition$JobSatisfaction)
sd(attrition$JobSatisfaction)
hist(attrition$JobSatisfaction, main = "Histogram of JobSatisfaction", xlab = "JobSatisfaction", col = "blue")
boxplot(attrition$JobSatisfaction, horizontal = TRUE, 
        main = "Boxplot of JobSatisfaction", xlab = "JobSatisfaction")
ggplot(attrition,aes(JobSatisfaction,fill=Attrition, alpha = 0.03))+geom_bar()


# Monthly Income
summary(attrition$MonthlyIncome)
var(attrition$MonthlyIncome)
sd(attrition$MonthlyIncome)
hist(attrition$MonthlyIncome, main = "Histogram of Monthy Income", xlab = "MonthlyIncome", col = "blue")
boxplot(attrition$MonthlyIncome, horizontal = TRUE, 
        main = "Boxplot of Monthly Income", xlab = "Monthly Income")
ggplot(attrition,aes(MonthlyIncome,fill=Attrition, alpha = 0.03))+geom_density()
  # Outliers Univariate - Monthly Income
    outlier_values <- boxplot.stats(attrition$MonthlyIncome)$out # outlier values
    boxplot(attrition$MonthlyIncome, main="Monthly Income", boxwex=0.1, horizontal = TRUE, 
            xlab = "Monthly Income")
    mtext(paste("Outliers: ", paste(outlier_values, collapse = ", ")), cex = 0.6)
  # Outliers Bivariate - Monthly Income
    # For categorical variable
      boxplot(MonthlyIncome ~ JobRole, data=attrition, main="Monthly Income Accross Job Role", 
              horizontal = FALSE)  # clear pattern is noticeable.
      boxplot(MonthlyIncome ~ Department, data=attrition, main="Monthly Income Accross Department", 
              horizontal = FALSE)  # this may not be significant, as day of week variable is a subset of the month var.
  # Plot of data with Outliers
    par(mfrow=c(1,1))
    plot(attrition$YearsAtCompany, attrition$MonthlyIncome, xlim=c(0, 40), ylim=c(1000, 20000), 
         main="With Outliers", xlab="YearsAtCompany", ylab="MonthlyIncome", pch="*", col="red", cex=2)
    abline(lm(MonthlyIncome ~ YearsAtCompany, data = attrition), col="blue", lwd = 3, lty=2)


# NumCompaniesWorked
summary(attrition$NumCompaniesWorked)
var(attrition$NumCompaniesWorked)
sd(attrition$NumCompaniesWorked)
hist(attrition$NumCompaniesWorked, main = "Histogram of NumCompaniesWorked", xlab = "NumCompaniesWorked", col = "blue")
boxplot(attrition$NumCompaniesWorked, horizontal = TRUE, 
        main = "Boxplot of NumCompaniesWorked", xlab = "NumCompaniesWorked")
ggplot(attrition,aes(NumCompaniesWorked,fill=Attrition, alpha = 0.03))+geom_density()
table(attrition$NumCompaniesWorked)
prop.table(table(attrition$NumCompaniesWorked))
prop.table(table(attrition$NumCompaniesWorked, attrition$Attrition))


# PercentSalaryHike
summary(attrition$PercentSalaryHike)
var(attrition$PercentSalaryHike)
sd(attrition$PercentSalaryHike)
hist(attrition$PercentSalaryHike, main = "Histogram of PercentSalaryHike", xlab = "PercentSalaryHike", col = "blue")
boxplot(attrition$PercentSalaryHike, horizontal = TRUE, 
        main = "Boxplot of PercentSalaryHike", xlab = "PercentSalaryHike")
ggplot(attrition,aes(PercentSalaryHike,fill=Attrition, alpha = 0.03))+geom_density()


# RelationshipSatisfaction
summary(attrition$RelationshipSatisfaction)
var(attrition$RelationshipSatisfaction)
sd(attrition$RelationshipSatisfaction)
hist(attrition$RelationshipSatisfaction, main = "Histogram of RelationshipSatisfaction", xlab = "RelationshipSatisfaction", col = "blue")
boxplot(attrition$RelationshipSatisfaction, horizontal = TRUE, 
        main = "Boxplot of RelationshipSatisfaction", xlab = "RelationshipSatisfaction")
ggplot(attrition,aes(RelationshipSatisfaction,fill=Attrition, alpha = 0.03))+geom_bar()


# StockOptionLevel
summary(attrition$StockOptionLevel)
var(attrition$StockOptionLevel)
sd(attrition$StockOptionLevel)
hist(attrition$StockOptionLevel, main = "Histogram of StockOptionLevel", xlab = "StockOptionLevel", col = "blue")
boxplot(attrition$StockOptionLevel, horizontal = TRUE, 
        main = "Boxplot of StockOptionLevel", xlab = "StockOptionLevel")
ggplot(attrition,aes(StockOptionLevel,fill=Attrition, alpha = 0.03))+geom_bar()

# TrainingTimesLastYear
summary(attrition$TrainingTimesLastYear)
var(attrition$TrainingTimesLastYear)
sd(attrition$TrainingTimesLastYear)
hist(attrition$TrainingTimesLastYear, main = "Histogram of TrainingTimesLastYear", xlab = "TrainingTimesLastYear", col = "blue")
boxplot(attrition$TrainingTimesLastYear, horizontal = TRUE, 
        main = "Boxplot of TrainingTimesLastYear", xlab = "TrainingTimesLastYear")
ggplot(attrition,aes(TrainingTimesLastYear,fill=Attrition, alpha = 0.03))+geom_bar()

# WorkLifeBalance
summary(attrition$WorkLifeBalance)
var(attrition$WorkLifeBalance)
sd(attrition$WorkLifeBalance)
hist(attrition$WorkLifeBalance, main = "Histogram of WorkLifeBalance", xlab = "WorkLifeBalance", col = "blue")
boxplot(attrition$WorkLifeBalance, horizontal = TRUE, 
        main = "Boxplot of WorkLifeBalance", xlab = "WorkLifeBalance")
ggplot(attrition,aes(WorkLifeBalance,fill=Attrition, alpha = 0.03))+geom_bar()

# YearsAtCompany
summary(attrition$YearsAtCompany)
var(attrition$YearsAtCompany)
sd(attrition$YearsAtCompany)
hist(attrition$YearsAtCompany, main = "Histogram of YearsAtCompany", xlab = "YearsAtCompany", col = "blue")
boxplot(attrition$YearsAtCompany, horizontal = TRUE, 
        main = "Boxplot of YearsAtCompany", xlab = "YearsAtCompany")
  # Outliers Univariate - Monthly Income
  outlier_values <- boxplot.stats(attrition$YearsAtCompany)$out # outlier values
  boxplot(attrition$YearsAtCompany, main="Monthly Income", boxwex=0.1, horizontal = TRUE, 
        xlab = "YearsAtCompany")
  mtext(paste("Outliers: ", paste(outlier_values, collapse = ", ")), cex = 0.6)
  # Outliers Bivariate - YearsAtCompany
  # For categorical variable
  boxplot(YearsAtCompany ~ JobRole, data=attrition, main="YearsAtCompany Accross Job 
        Role", ylab = "YearsAtComapny", xlab = "JobRole")  # clear pattern is noticeable.
  boxplot(YearsAtCompany ~ Department, data=attrition, main="YearsAtCompany Accross 
        Department", ylab = "YearsAtComapny", xlab = "Department")  # this may not be significant, as day of week variable is a subset of the month var.
  # Plot of data with Outliers
  par(mfrow=c(1,1))
  plot(attrition$YearsAtCompany, attrition$MonthlyIncome, xlim=c(0, 40), ylim=c(1000, 20000), 
     main="With Outliers", xlab="MonthlyIncome", ylab="YearsAtCompany", pch="*", col="red", cex=2)
  abline(lm(MonthlyIncome ~ YearsAtCompany, data = attrition), col="blue", lwd = 3, lty=2)
ggplot(attrition,aes(YearsAtCompany,fill=Attrition, alpha = 0.03))+geom_bar()
  
# CompaOverallGroup 
str(attrition$CompaOverallGroup)
summary(attrition$CompaOverallGroup)
var(attrition$CompaOverallGroup)
sd(attrition$CompaOverallGroup)
hist(attrition$CompaOverallGroup, main = "Histogram of CompaOverallGroup", xlab = "CompaOverallGroup", col = "blue")
boxplot(attrition$CompaOverallGroup, horizontal = TRUE, 
        main = "Boxplot of CompaOverallGroup", xlab = "CompaOverallGroup")
table(attrition$Attrition, attrition$CompaOverallGroup)
prop.table(table(attrition$CompaOverallGroup, attrition$Attrition))
ggplot(attrition,aes(CompaOverallGroup, fill = Attrition, alpha = 0.3))+geom_bar()

########################################## K-Modes Algorithm ####################################################
library(klaR)
data.to.cluster <- attrition
cluster.results <- kmodes(data.to.cluster, 3, iter.max = 10, weighted = FALSE)
cluster.results
summary(cluster.results)

# ---------------------Comparative "TEST" Testing--------------------------------

# install.packages("klaR")
# install.packages("caret")

# load libraries
library(mlbench)
library(caret)
library(klaR)

# rename dataset to keep code below generic
dataset_test <- attrition

control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7

metric <- "Accuracy"
preProcess=c("center", "scale")


# Linear Discriminant Analysis
set.seed(seed)
fit.lda <- train(Attrition~., data=dataset_test, method="lda", metric=metric, preProc=c("center", "scale"), trControl=control)
# Logistic Regression
set.seed(seed)
fit.glm <- train(Attrition~., data=dataset_test, method="glm", metric=metric, trControl=control)
# GLMNET
set.seed(seed)
fit.glmnet <- train(Attrition~., data=dataset_test, method="glmnet", metric=metric, preProc=c("center", "scale"), trControl=control)
# SVM Radial
set.seed(seed)
fit.svmRadial <- train(Attrition~., data=dataset_test, method="svmRadial", metric=metric, preProc=c("center", "scale"), trControl=control, fit=FALSE)
# kNN
set.seed(seed)
fit.knn <- train(Attrition~., data=dataset_test, method="knn", metric=metric, preProc=c("center", "scale"), trControl=control)
# Naive Bayes
# set.seed(seed)
# fit.nb <- train(Attrition~., data=dataset_test, method="nb", metric=metric, trControl=control)
# CART
set.seed(seed)
fit.cart <- train(Attrition~., data=dataset_test, method="rpart", metric=metric, trControl=control)
# C5.0
# set.seed(seed)
# fit.c50 <- train(Attrition~., data=dataset_test, method="C5.0", metric=metric, trControl=control)
# Bagged CART
set.seed(seed)
fit.treebag <- train(Attrition~., data=dataset_test, method="treebag", metric=metric, trControl=control)
# Random Forest
set.seed(seed)
fit.rf <- train(Attrition~., data=dataset_test, method="rf", metric=metric, trControl=control)
# Stochastic Gradient Boosting (Generalized Boosted Modeling)
set.seed(seed)
fit.gbm <- train(Attrition~., data=dataset_test, method="gbm", metric=metric, trControl=control, verbose=FALSE)
# Decision Tree 
set.seed(seed)
fit.dt <- train(Attrition~., data=dataset_test, method="rpart", metric=metric, trControl=control)

results <- resamples(list("Logistic Regression"=fit.glm,"SVM Radial"=fit.svmRadial, kNN=fit.knn, CART=fit.cart,
                          "Bagged CART"=fit.treebag, "Random Forest"=fit.rf, "Stochastic Gradient Boosting"=fit.gbm, 
                          "Decision Tree" =fit.dt ))
# Table comparison
summary(results)
results


# boxplot comparison
bwplot(results)
# Dot-plot comparison
dotplot(results)


# -------------------------------LOGISITIC REGRESSION----------------------------------------------
nrow(attrition)
# 1st Split data into training and testing sets:

train <- createDataPartition(attrition$Attrition,p=0.7,list=FALSE)
set.seed(2017)
training <- attrition[train,]
testing <- attrition[-train,]
# Check Spliting Results
dim(training); dim(testing)

# Fitting the LOg Regresssion Model
mod_fit <- glm(Attrition ~ .,family=binomial(link="logit"),data=training)
mod_fit
summary(mod_fit)

qchisq(0.95, 992)

# Predictive Model for Attrition - Most Significant Variables
"Age, BusinessTravel, DistanceFromHome, EnviromentSatisfaction, Gender, 
JobInvolvement, JobRole, JobSatisfaction, NumCompaniesWorked, OverTime, 
RelationshipSatisfaction" 

# ANOVA of model_log
anova(mod_fit, test="Chisq")

# Logistic Regression Accuracy or the predictive ability of the mod_fit
testing$Attrition <- as.character(testing$Attrition)
testing$Attrition[testing$Attrition=="No"] <- "0"
testing$Attrition[testing$Attrition=="Yes"] <- "1"
fitted.results <- predict(mod_fit,newdata=testing,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != testing$Attrition)
print(paste('Logistic Regression Accuracy',1-misClasificError))

# Log Reg Confusion Matrix
print("Confusion Matrix for Logistic Regression"); table(testing$Attrition, fitted.results > 0.5)


mod_fit_selective <- glm(Attrition ~ Age + BusinessTravel + DistanceFromHome + EnvironmentSatisfaction + Gender + 
                         JobInvolvement + JobRole + JobSatisfaction + NumCompaniesWorked + OverTime + 
                         RelationshipSatisfaction
                         , family=binomial(link="logit"),data=training)
summary(mod_fit_selective)

# ANOVA of model_log
anova(mod_fit_selective, test="Chisq")

# Logistic Regression Accuracy or the predictive ability of the mod_fit_selective
testing$Attrition <- as.character(testing$Attrition)
testing$Attrition[testing$Attrition=="No"] <- "0"
testing$Attrition[testing$Attrition=="Yes"] <- "1"
fitted.results <- predict(mod_fit_selective,newdata=testing,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != testing$Attrition)
print(paste('Logistic Regression Accuracy',1-misClasificError))

# Log Reg Confusion Matrix
print("Confusion Matrix for Logistic Regression"); table(testing$Attrition, fitted.results > 0.5)



# ---------------------Odds Ratio--------------------
library(MASS)
exp(cbind(OR=coef(mod_fit), confint(mod_fit)))




# ====================================Decision Trees====================================

# ---------------------Decision Tree------------------
tree <- ctree(Attrition~., training)
plot(tree)

# Decision Tree Confusion Matrix
pred_tree <- predict(tree, testing)
print("Confusion Matrix for Decision Tree"); table(Predicted = pred_tree, Actual = testing$Attrition)

# Decision Tree Accuracy 
p1 <- predict(tree, training)
tab1 <- table(Predicted = p1, Actual = training$Attrition)
tab2 <- table(Predicted = pred_tree, Actual = testing$Attrition)
print(paste('Decision Tree Accuracy',sum(diag(tab2))/sum(tab2)))
"Accuracy is not imporved over log regression"


# ----------------Decision Tree Selective---------------
tree_selective <- ctree(Attrition ~ ., training)
plot(tree_selective)

# Decision Tree Confusion Matrix
pred_tree_selective <- predict(tree_selective, testing)
print("Confusion Matrix for Decision Tree"); table(Predicted = pred_tree_selective, Actual = testing$Attrition)

# Decision Tree Accuracy 
p1_selective <- predict(tree_selective, training)
tab1_selective <- table(Predicted = p1_selective, Actual = training$Attrition)
tab2_selective <- table(Predicted = pred_tree_selective, Actual = testing$Attrition)
print(paste('Decision Tree Accuracy',sum(diag(tab2_selective))/sum(tab2_selective)))
"Accuracy is not imporved over log regression"

summary(attrition$YearsWithoutPromotion_WithCurrentManager_group)

# ----------------Decision Tree Selective 2---------------
tree_selective_2 <- ctree(Attrition ~ BusinessTravel + EnvironmentSatisfaction + JobInvolvement 
                          + MaritalStatus + NumCompaniesWorked + OverTime 
                          + YearsWithoutPromotion_WithCurrentManager_group, training)
plot(tree_selective_2)

# Decision Tree Confusion Matrix
pred_tree_selective_2 <- predict(tree_selective_2, testing)
print("Confusion Matrix for Decision Tree"); table(Predicted = pred_tree_selective_2, Actual = testing$Attrition)

# Decision Tree Accuracy 
p1_selective_2 <- predict(tree_selective_2, training)
tab1_selective_2 <- table(Predicted = p1_selective_2, Actual = training$Attrition)
tab2_selective_2 <- table(Predicted = pred_tree_selective_2, Actual = testing$Attrition)
print(paste('Decision Tree Accuracy',sum(diag(tab2_selective_2))/sum(tab2_selective_2)))
"Accuracy is not imporved over log regression"


#### Discriminate Analysis #################

library(lmtest)
lrtest(mod_fit_one, mod_fit_two)

# Discriminate Analysis
mod_fit_one <- glm(Attrition ~ ., data=training, family="binomial")
mod_fit_two <- glm(Attrition ~ BusinessTravel + EnvironmentSatisfaction + JobInvolvement 
                   + MaritalStatus + NumCompaniesWorked + OverTime 
                   + YearsWithoutPromotion_WithCurrentManager_group, data=training, family="binomial")

library(lmtest)
lrtest(mod_fit_one, mod_fit_two)

#### RocK Curve ####
# install.packages("ROCR")
library(ROCR)
# Compute AUC for predicting Class with the model
prob <- predict(mod_fit_one, newdata=testing, type="response")
pred <- prediction(prob, testing$Attrition)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf)

auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc

# VarImp
sig_var <- train(Attrition ~ ., data=attrition, method ="glm", family="binomial")
varImp(sig_var)

#Significant Variables: BusinessTravel, EnvironmentalSatisfaction, JobInvolvement, MaritalStatus,  NumCompaniesWorked, OverTime, YearsWithoutPromotion_WithCurrentManager_group
sv1 <- ggplot(attrition,aes(BusinessTravel,fill = Attrition))+geom_bar()
sv2 <- ggplot(attrition,aes(EnvironmentSatisfaction,fill = Attrition))+geom_bar()
sv3 <- ggplot(attrition,aes(JobInvolvement,fill = Attrition))+geom_bar()
sv4 <- ggplot(attrition,aes(MaritalStatus,fill = Attrition))+geom_bar()
sv5 <- ggplot(attrition,aes(NumCompaniesWorked,fill = Attrition))+geom_bar()
sv6 <- ggplot(attrition,aes(OverTime,fill = Attrition))+geom_bar()
sv7 <- ggplot(attrition,aes(YearsWithoutPromotion_WithCurrentManager_group,fill = Attrition))+geom_bar()
grid.arrange(sv1,sv2,sv3,sv4,sv5,sv6,sv7,ncol=2,top = "Figure: Significant Variables")



# Cleaned Data Output attrition
write.csv(attrition, file = "cleaned_attrition.csv", row.names = FALSE)

