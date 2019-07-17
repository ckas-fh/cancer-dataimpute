#######################
#Caroline Cancer Data Cleaning/Imputation
# Feb 26, 2019
########################

#1. loading relevant packages
library(dplyr)
library(mice)
library(naniar)

#2. loading dataframe and setting working directory
setwd("Desktop")
cancer_data <- read.csv("Caroline-Dataset.csv")

#3. general data cleaning 
#changing BonyLesions to be more readable
cancer_data <- cancer_data %>%
  mutate(BonyLesions=recode(BonyLesions, ">3"="3"))

#changing ClinStages data and column name to be more readable
cancer_data <- cancer_data %>%
  mutate(ClinStageS=recode(ClinStageS, "I"="1", "II"="2","III"="3"))%>%
  rename(ClinStages=ClinStageS)

#4. observing how many missing values in overall data frame
cancer_data[cancer_data == ""] <- NA
print(cancer_data)
sum(is.na(cancer_data))
md.pattern(cancer_data)

#5. observing how many missing values in each column 
colSums(is.na(cancer_data))
#tells us that YearofDx,SurvivalDays,AgeAtDx have missing values

#6. imputing missing values with MICE
imputed_Data <- mice(cancer_data, m=5, maxit = 20, method = 'pmm', seed = NA)
summary(imputed_Data)

#7. observing imputed values
imputed_Data$imp$YearOfDx
imputed_Data$imp$AgeAtDx
imputed_Data$imp$SurvivalDays

#8. completing dataset with imputed values
complete_Data <- complete(imputed_Data,2)

#9. checking that dataset has imputed values
sum(is.na(complete_Data))
