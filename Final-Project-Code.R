# Import external libraries
library(class)
library(tidyverse)
library(bestglm)
library(clustMixType)
library(ggplot2)
library(dplyr)
library(randomForest)
library(caret)
library(pROC)
library(factoextra)

#---------------------------------------------------------------------------------------------------------------------------------------------
# VARIABLE INITIALIZATON

#Read raw csv file
data <- read.csv("C:\\Users\\jakel\\OneDrive\\Documents\\.Uni Stuff\\.Winter 2023\\STAT 362\\Final_Project\\ObesityDataSet_raw_and_data_sinthetic.csv") 

#Create BMI variable
data$BMI <- data$Weight / (data$Height * data$Height)
#Create variable for obesity or no obesity
data$Obesity <- factor(ifelse(data$NObeyesdad=="Obesity_Type_I"|data$NObeyesdad=="Obesity_Type_II"|data$NObeyesdad=="Obesity_Type_III","yes","no"))

#Cleaning of Variables in Dataset

#Ordered the variable for obesity category
data$NObeyesdad <- factor(data$NObeyesdad,
                          levels = c("Insufficient_Weight","Normal_Weight",
                                    "Overweight_Level_I","Overweight_Level_II",
                                    "Obesity_Type_I","Obesity_Type_II","Obesity_Type_III"),
                          ordered = TRUE)

#Rounded all synthetic data from floats to integers
data$FAF <- data$FAF %>% 
  round() %>% 
  factor(levels = c(0, 1, 2, 3), ordered = TRUE)
data$TUE <- round(data$TUE)
data$FCVC <- round(data$FCVC)
data$NCP <- round(data$NCP)
data$TUE <- round(data$TUE)
data$CH2O <- round(data$CH2O)
data$FCVC <- factor(data$FCVC, levels = c(1,2,3))
data$Gender <- factor(data$Gender)
data$FAVC <- factor(data$FAVC, levels = c("no", "yes"))


#---------------------------------------------------------------------------------------------------------------------------------------------
#DATA VISUALIZATION

#Data Information
table(data$Gender)
table(data$family_history_with_overweight)

#Bar chart of counts for obesity categories
data %>% 
  ggplot(mapping=aes(x=NObeyesdad, fill = Gender))+
  geom_bar()+
  ylab("Number of Records")+
  xlab("Obesity Level Category")+
  ggtitle("Distribution of Obesity Class") +
  theme_bw()

#Histogram of BMI
ggplot(data,aes(x=BMI,fill=Gender))+
  geom_histogram()+
  labs(title="Distribution of BMI by Gender",x="Body Mass Index (BMI)",y="Frequency") +
  theme_bw()

#Pie chart of the "number of main meals" category
data_NCP <- data.frame("category" = c("Between 1 & 2", "Three", "More than three"),
                   "amount" = c(round(length(which(data$NCP < 3))/length(data$NCP)*100),
                                round(length(which(data$NCP == 3))/length(data$NCP)*100),
                                round(length(which(data$NCP > 3))/length(data$NCP)*100)))
ggplot(data_NCP, aes(x="", y=amount, fill=category)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme_void()+
  geom_text(aes(label = paste0(amount, "%")), position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL, fill = NULL)+
  ggtitle("Number of Main Meals")

#Pie chart of "do you eat high caloric meals frequently"
datayes <- sum(data$FAVC == 'yes')
datano <- sum(data$FAVC == 'no')
data_FAVC <- data.frame("category" = c("Yes","No"),
                   "amount" = c(round(datayes/(length(data$FAVC))*100),
                              round(datano/(length(data$FAVC))*100)))
ggplot(data_FAVC, aes(x="", y=amount, fill=category)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme_void()+
  geom_text(aes(label = paste0(amount, "%")),
  position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL, fill = NULL)+
  ggtitle("Frequent High Caloric Meal Intake")
  
#Pie chart of "Frequency of physical activity per week"
data_FAF <- data.frame("category" = c("None","1-2 days","2-4 days","4-5 days"),
                   "amount" = c(round(length(which(data$FAF == 0))/length(data$FAF)*100),
                                round(length(which(data$FAF == 1))/length(data$FAF)*100),
                                round(length(which(data$FAF == 2))/length(data$FAF)*100),
                                round(length(which(data$FAF == 3))/length(data$FAF)*100)))
ggplot(data_FAF, aes(x="", y=amount, fill=category)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme_void()+
  geom_text(aes(label = paste0(amount, "%")),
            position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL, fill = NULL)+
  ggtitle("Frequency of Physical Activity per Week")

#Pie chart of "Mode of transportation"
data_MTRANS <- data.frame("category" = c("Automobile", "Other", "Public Transportation", "Walking"),
                          "amount" = c(ifelse(nrow(filter(data, MTRANS == "Automobile"))/length(data$MTRANS)*100 < 1,nrow(filter(data, MTRANS == "Automobile"))/length(data$MTRANS)*100,round(nrow(filter(data, MTRANS == "Automobile"))/length(data$MTRANS)*100)),
                                       ifelse(nrow(filter(data, MTRANS %in% c("Motorbike","Bike")))/length(data$MTRANS)*100 < 1,nrow(filter(data, MTRANS %in% c("Motorbike","Bike")))/length(data$MTRANS)*100, round(nrow(filter(data, MTRANS %in% c("Motorbike","Bike")))/length(data$MTRANS)*100)),
                                       ifelse(nrow(filter(data, MTRANS == "Public_Transportation"))/length(data$MTRANS)*100 < 1, nrow(filter(data, MTRANS == "Public_Transportation"))/length(data$MTRANS)*100, round(nrow(filter(data, MTRANS == "Public_Transportation"))/length(data$MTRANS)*100)),
                                       ifelse(nrow(filter(data, MTRANS == "Walking"))/length(data$MTRANS)*100 < 1,nrow(filter(data, MTRANS == "Walking"))/length(data$MTRANS)*100, round(nrow(filter(data, MTRANS == "Walking"))/length(data$MTRANS)*100))))

ggplot(data_MTRANS, aes(x="", y=amount, fill=category)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme_void() +
  geom_text(aes(label = paste0(ifelse(amount < 1,"<1",amount), "%")), position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL, fill = NULL) +
  ggtitle("Mode of Transport")

#Pie chart of the "Obesity" category
data_NObeyesdad <- data.frame("category" = c("Insufficient Weight", "Normal Weight", "Overweight Level I", "Overweight Level II", "Obesity Type I", "Obesity Type II", "Obesity Type III"),
                              "amount" = c(round(nrow(filter(data, data$NObeyesdad == "Insufficient_Weight"))/length(data$NObeyesdad)*100),
                                           round(nrow(filter(data, data$NObeyesdad == "Normal_Weight"))/length(data$NObeyesdad)*100),
                                           round(nrow(filter(data, data$NObeyesdad == "Overweight_Level_I"))/length(data$NObeyesdad)*100),
                                           round(nrow(filter(data, data$NObeyesdad == "Overweight_Level_II"))/length(data$NObeyesdad)*100),
                                           round(nrow(filter(data, data$NObeyesdad == "Obesity_Type_I"))/length(data$NObeyesdad)*100),
                                           round(nrow(filter(data, data$NObeyesdad == "Obesity_Type_II"))/length(data$NObeyesdad)*100),
                                           round(nrow(filter(data, data$NObeyesdad == "Obesity_Type_III"))/length(data$NObeyesdad)*100)))
ggplot(data_NObeyesdad, aes(x="", y=amount, fill=category)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme_void()+
  geom_text(aes(label = paste0(amount, "%")), position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL, fill = NULL)+
  ggtitle("Distribution of Obesity Levels")

#Pie chart of the "frequency of vegetables in meals" category
data_FCVC <- data.frame("category" = c("Never", "Sometimes", "Always"),
                   "amount" = c(round(length(which(data$FCVC == 1))/length(data$FCVC)*100),
                                round(length(which(data$FCVC == 2))/length(data$FCVC)*100),
                                round(length(which(data$FCVC == 3))/length(data$FCVC)*100)))
ggplot(data_FCVC, aes(x="", y=amount, fill=category)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme_void()+
  geom_text(aes(label = paste0(amount, "%")), position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL, fill = NULL)+
  ggtitle("Frequency of Vegetables Consumption")

# Bar graph of counts of Physical Activity
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

# Bar graph of counts of High Caloric Food Intake
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

#Summary Statistics
summary(data$BMI)
summary(data$Age)
summary(data$Height)
summary(data$Weight)

sd(data$BMI)
sd(data$Age)
sd(data$Height)
sd(data$Weight)

#Boxplots
boxplot(data$BMI,main='Boxplot of BMI',ylab="BMI")
boxplot(data$Age,main='Boxplot of Age',ylab="Age")
boxplot(data$Height,main='Boxplot of Height',ylab="Height (m)")
boxplot(data$Weight,main='Boxplot of Weight',ylab="Weight (kg)")


#---------------------------------------------------------------------------------------------------------------------------------------------
#DATA ANALYSIS

#Stepwise regression
#Dataset with height, weight, and overweight class removed
data_removed <- data[,-c(3,4,17,19)]
full_model <- lm (BMI ~ ., data = data_removed)
step(full_model, direction = "backward")

#New model from regression
model <- lm(formula = BMI ~ Gender + Age + family_history_with_overweight + 
    FAVC + FCVC + NCP + CAEC + CH2O + SCC + FAF + CALC + MTRANS,
    data = data_removed)
summary(model)
  
#Random Forest
forest_train <-
  randomForest(BMI ~ ., data = data_removed, importance = TRUE)
varImpPlot(forest_train)

#T-test for bmi and smoking (no-smoking vs smoking)
t.test(data$BMI[data$SMOKE=="no"],data$BMI[data$SMOKE=="yes"])

#T-test for bmi and physical activity (no PA vs 1 or more days of PA)
t.test(data$BMI[data$FAF==0],data$BMI[data$FAF>=1])

#T-test for bmi and tech usage (less than 5 hours tech usage vs 5 or more hours tech usage)
t.test(data$BMI[data$TUE<2],data$BMI[data$TUE==2])

#T-test for bmi and high caloric food eating (no vs yes)
t.test(data$BMI[data$FAVC=="no"],data$BMI[data$FAVC=="yes"])

#T-test for bmi and FCVC(veggie consumption) (never vs always)
t.test(data$BMI[data$FCVC==1],data$BMI[data$FCVC==3])

#T-test for bmi and family history
t.test(data$BMI[data$family_history_with_overweight=="no"],data$BMI[data$family_history_with_overweight=="yes"])

#Prop test for Family History
prop_none <- filter(data,family_history_with_overweight == "no")
prop_history <- filter(data,family_history_with_overweight == "yes")
prop.test(c(nrow(filter(prop_none,Obesity == "yes")), nrow(filter(prop_history,Obesity == "yes"))),c(nrow(prop_none),nrow(prop_history)))

#Prop test for frequency of vegetable consumption
prop_FCVC_1 <- filter(data,FCVC == 1)
prop_FCVC_2 <- filter(data,FCVC == 2)
prop_FCVC_3 <- filter(data,FCVC == 3)
prop.test(c(nrow(filter(prop_FCVC_1,Obesity == "yes")), nrow(filter(prop_FCVC_2,Obesity == "yes")),nrow(filter(prop_FCVC_3,Obesity == "yes"))),c(nrow(prop_FCVC_1),nrow(prop_FCVC_2),nrow(prop_FCVC_3)))

#Prop test for physical activity
prop_FAF_no <- filter(data, FAF == 0)
prop_FAF_1 <- filter(data, FAF == 1)
prop_FAF_2 <- filter(data, FAF == 2)
prop_FAF_3 <- filter(data, FAF == 3)
prop.test(c(nrow(filter(prop_FAF_no,Obesity == "yes")), nrow(filter(prop_FAF_1,Obesity == "yes")),nrow(filter(prop_FAF_2,Obesity == "yes")),nrow(filter(prop_FAF_3,Obesity == "yes"))),c(nrow(prop_FAF_no),nrow(prop_FAF_1),nrow(prop_FAF_2),nrow(prop_FAF_3)))

#Prop test for monitoring calorie consumption
prop_no_monitor <- filter(data,SCC=="no")
prop_monitor <- filter(data,SCC=="yes")
prop.test(c(nrow(filter(prop_no_monitor,Obesity == "yes")), nrow(filter(prop_monitor,Obesity == "yes"))),c(nrow(prop_no_monitor),nrow(prop_monitor)))

#KS Test for smoking
ks_smokers <- data$BMI[data$SMOKE == "no"]
ks_nonsmokers <- data$BMI[data$SMOKE == "yes"]
ks.test(ks_smokers, ks_nonsmokers)

#KS test for physical activity
ks_sedentary <- data$BMI[data$FAF == 0]
ks_active <- data$BMI[data$FAF >= 1]
ks.test(ks_sedentary, ks_active)

#KS test for tech usage
ks_less <- data$BMI[data$TUE < 2]
ks_more <- data$BMI[data$TUE == 2]
ks.test(ks_less, ks_more)

#KS test for high calorie food intake
ks_nonfrequent <- data$BMI[data$FAVC == "no"]
ks_frequent <- data$BMI[data$FAVC == "yes"]
ks.test(ks_nonfrequent, ks_frequent)

#KS test for family history of obesity
ks_none <- data$BMI[data$family_history_with_overweight == "no"]
ks_history <- data$BMI[data$family_history_with_overweight == "yes"]
ks.test(ks_none, ks_history)

#Knn Classification
set.seed(1)
random_index <- sample(nrow(data),469)

train <- data[random_index, -c(3:4,18,19)]
test <- data[-random_index, -c(3:4,18,19)]

train_labels <- data[random_index, 19]
test_labels <- data[-random_index, 19]

train_n <- train
test_n <- test

train_min <- apply(train, 2, min)
train_max <- apply(train, 2, max)
for (i in 1:ncol(train)) {
  train_n[, i] <- (train[, i] - train_min[i]) / (train_max[i] - train_min[i]) 
  # use the min and max from training data to normalize the testing data
  test_n[, i] <- (test[, i] - train_min[i]) / (train_max[i] - train_min[i]) 
}
knn_predicted <- knn(train = train_n, test = test_n, 
                     cl = train_labels, k = 21)

#Elbow Method
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

#K-means clustering
#Clustering of Age and BMI (young ppl with high variance whereas old has little variance)
data_kmeans <- kmeans(x=scale(data[,c(2,18)]), centers = 4, nstart = 25)
data_kmeans$centers
fviz_cluster(data_kmeans,  data = data[, c(2,18)], geom = "point")

#Clustering of exercise and bmi
data_kproto <- kproto(x=data[,c(13,18)], k = 4, nstart = 25)
data_kproto$centers
data_kproto$cluster
clprofiles(data_kproto,  data[, c(13,18)], vars=NULL, col=NULL)
#no exercise = high variance, exercise = low bmi generally

#Cross-Validation
k <- 10
# the result is a list
folds <- createFolds(data$Obesity, k = k)
accuracy <- rep(0, k)
AUC <- rep(0, k)

#Cross-validation using randomForest
for (i in 1:k) {
  train_data  <- data[folds[[i]], ]
  test_data <- data[-folds[[i]], ]
  forest_train <- randomForest(Obesity ~ Age + FCVC + family_history_with_overweight, data = train_data, importance = TRUE)
  
  # Prediction
  prob <- predict(forest_train, test_data, type = "prob")
  predicted_class <- ifelse(prob[,2] > 0.5, "yes", "no")
  
  # Compute accuracy and AUC
  accuracy[i] <- mean(predicted_class == test_data$Obesity)
  AUC[i] <- auc(roc(predictor = as.numeric(prob[,2]), response = test_data$Obesity))
}

accuracy
mean(accuracy)
AUC
mean(AUC)

#Cross-validation using glm
for (i in 1:k) {
  train_data  <- data[folds[[i]], ]
  test_data <- data[-folds[[i]], ]
  fit_simple <- glm(Obesity ~ Age + FCVC + family_history_with_overweight, data = train_data, family = binomial)
  
  # Prediction
  prob <- predict(fit_simple, test_data, type = "response")
  predicted_class <- ifelse(prob > 0.5, "yes", "no")
  
  # Compute accuracy and AUC
  accuracy[i] <- mean(predicted_class == test_data$Obesity)
  AUC[i] <- auc(roc(predictor = as.numeric(prob), response = test_data$Obesity))
}

accuracy
mean(accuracy)
AUC
mean(AUC)