data <- read.csv("C:\\Users\\jakel\\OneDrive\\Documents\\.Uni Stuff\\.Winter 2023\\STAT 362\\ObesityDataSet_raw_and_data_sinthetic.csv") 

#Create BMI variable
data$bmi <- data$Weight / (data$Height * data$Height)
#Ordered the variable for obesity category
data$NObeyesdad <- factor(data$NObeyesdad,
                          levels = c("Insufficient_Weight","Normal_Weight",
                                    "Overweight_Level_I","Overweight_Level_II",
                                    "Obesity_Type_I","Obesity_Type_II","Obesity_Type_III"),
                          ordered = TRUE)
#Rounded all synthetic data from floats to integers
data$FAF <- round(data$FAF)

#Data Visualization
library(tidyverse)
#Bar chart of counts for obesity categories
data %>% 
  ggplot(mapping=aes(x=NObeyesdad))+
  geom_bar()+
  ylab("Number of Records")+
  xlab("Obesity Level Category")+
  ggtitle("Distribution of Obesity Class")+
  theme_minimal()


#Data Analysis
#T-tests
#T-test for bmi and 