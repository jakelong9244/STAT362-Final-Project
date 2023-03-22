data <- read.csv("C:\\Users\\jakel\\OneDrive\\Documents\\.Uni Stuff\\.Winter 2023\\STAT 362\\ObesityDataSet_raw_and_data_sinthetic.csv") 

#Create BMI variable
data$BMI <- data$Weight / (data$Height * data$Height)
#Ordered the variable for obesity category
data$NObeyesdad <- factor(data$NObeyesdad,
                          levels = c("Insufficient_Weight","Normal_Weight",
                                    "Overweight_Level_I","Overweight_Level_II",
                                    "Obesity_Type_I","Obesity_Type_II","Obesity_Type_III"),
                          ordered = TRUE)
#Rounded all synthetic data from floats to integers
data$FAF <- round(data$FAF)
data$TUE <- round(data$TUE)

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
ggplot(data,aes(x=BMI,fill=Gender))+
  geom_histogram()+
  labs(title="Distribution of BMI by Gender",x="Body Mass Index",y="Frequency")
  theme_bw()

#Pie chart of the "number of main meals" category
roundata1 <- round(data$NCP)
data <- data.frame("category" = c("Between 1 & 2", "Three", "More than three"),
                   "amount" = c(round(length(which(roundata1 < 3))/length(roundata1)*100),
                                round(length(which(roundata1 == 3))/length(roundata1)*100),
                                round(length(which(roundata1 > 3))/length(roundata1)*100)))

ggplot(data, aes(x="", y=amount, fill=category)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme_void()+
  geom_text(aes(label = paste0(amount, "%")), position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL, fill = NULL)+
  ggtitle("Pie chart of the number of main meals")

# PHYSICAL ACTIVITY
# 1. factor FAF (physical activity)
data$FAF <- data$FAF %>% 
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

#T-test for bmi and smoking (no-smoking vs smoking)
t.test(data$BMI[data$SMOKE=="no"],data$BMI[data$SMOKE=="yes"])
#T-test for bmi and physical activity (no PA vs 1 or more days of PA)
t.test(data$BMI[data$FAF==0],data$BMI[data$FAF>=1])
#T-test for bmi and tech usage (less than 5 hours tech usage vs 5 or more hours tech usage)
t.test(data$BMI[data$TUE<2],data$BMI[data$TUE==2])
#T-test for bmi and high caloric food eating (no vs yes)
t.test(data$BMI[data$FAVC=="no"],data$BMI[data$FAVC=="yes"])
#T-test for bmi and family history
t.test(data$BMI[data$family_history_with_overweight=="no"],data$BMI[data$family_history_with_overweight=="yes"])


#KS Test
# smoking
smokers <- data$bmi[data$SMOKE == "no"]
nonsmokers <- data$bmi[data$SMOKE == "yes"]
ks.test(smokers, nonsmokers)

# physical activity
sedentary <- data$bmi[data$FAF == 0]
active <- data$bmi[data$FAF >= 1]
ks.test(sedentary, active)

# tech usage
less <- data$bmi[data$TUE < 2]
more <- data$bmi[data$TUE == 2]
ks.test(less, more)

# high calorie food intake
nonfrequent <- data$bmi[data$FAVC == "no"]
frequent <- data$bmi[data$FAVC == "yes"]
ks.test(nonfrequent, frequent)

# family history of obesity
none <- data$bmi[data$family_history_with_overweight == "no"]
history <- data$bmi[data$family_history_with_overweight == "yes"]
ks.test(none, history)

#Knn Classification




#Linear Regression? Step-wise Regression?
data_rem <- select(data, c(-Height,-Weight,-NObeyesdad))


full_model <- lm (BMI ~ ., data = data_rem)
step (full_model, direction = "backward")

model <- lm(formula = BMI ~ Gender + Age + family_history_with_overweight + 
               FAVC + FCVC + NCP + CAEC + CH2O + SCC + FAF + TUE + CALC + 
               MTRANS, data = data_rem)
summary(model)
bestglm()


#K-means clustering
#Clustering of Age and BMI (young ppl with high variance whereas old not[they dead])
data_kmeans <- kmeans(x=scale(data[,c(2,18)]), centers = 3, nstart = 25)
data_kmeans$centers

library(factoextra)
fviz_cluster(data_kmeans,  data = data[, c(2,18)], geom = "point")

#K-modes clustering?
WSS <- rep(0, 10)
for (k in 1:10) {
  # extract the total within-group sum of squared errors
  WSS[k] = kproto(x=data[,c(13,18)], k = 6, nstart = 25)$tot.withinss
}

ggplot(mapping = aes(x = 1:10, y = WSS)) +
  geom_line() + 
  geom_point() +
  geom_vline(xintercept = 4) +
  scale_x_discrete(name = "k", limits = factor(1:10)) +
  labs(title = "Elbow Method")
#4 clusters is ideal
library(clustMixType)

data_kproto <- kproto(x=data[,c(13,18)], k = 4, nstart = 25)
data_kproto$centers
data_kproto$cluster
clprofiles(data_kproto,  data[, c(13,18)], vars=NULL, col=NULL)
#no exercise = high variance, exercise = low bmi generally

