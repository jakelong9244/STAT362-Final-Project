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
  theme_bw()


#Histogram of BMI
ggplot(data,aes(x=bmi,fill=Gender))+
  geom_histogram()+
  labs(title="Distribution of BMI by Gender",x="Body Mass Index",y="Frequency")
  theme_bw()

# PHYSICAL ACTIVITY
# 1. factor FAF (physical activity)
data$FAF %>% 
  round() %>% 
  factor(levels = c(0, 1, 2, 3), ordered = TRUE)
         
# 2. data visualization
data %>% 
  ggplot(mapping = aes(x = FAF, fill = factor(FAF))) +
  geom_bar() +
  labs(
    title = "Distribution of Patient Physical Activity Levels",
    x = "Physical Activity Level",
    y = "Count",
    fill = "Levels"
  ) + 
  theme_bw()

# HIGH CALORIC FOOD
# 1. factor FAVC
data$FAVC <- factor(data$FAVC, levels = c("no", "yes"))

# 2. change name for levels
data$FAVC <- ifelse(data$FAVC == "no",
                         "No", "Yes")

# 3. data visualization
data %>% 
  ggplot(mapping = aes(x = FAVC, fill = factor(FAVC))) +
  geom_bar() +
  labs(
    title = "Patient Responses to Frequent High-Calorie Food Intake",
    x = "Frequent High Calorie Food Intake?",
    y = "Count",
    fill = "Response"
  ) + 
  theme_bw()




#Data Analysis
#T-tests

#T-test for bmi and smoking
t.test(data$bmi[data$SMOKE=="no"],data$bmi[data$SMOKE=="yes"])


#Prop tests

