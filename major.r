library(readxl)
data<-read_excel("C:/Users/DELL/Downloads/data science lab/Mini major/Chronic_Kidney_Dsease_data.xlsx")
colnames(data)

#reg and cor
library(ggplot2)
library(corrplot)
library(plotly)
data<-read_excel("C:/Users/DELL/Downloads/data science lab/Mini major/Chronic_Kidney_Dsease_data.xlsx")
model <- lm(GFR ~ Age + BMI + SystolicBP + DiastolicBP + HbA1c + SerumCreatinine + ProteinInUrine, data = data)
summary(model)
data$residuals <- model$residuals
data$fitted_values <- model$fitted.values
p1 <- ggplot(data, aes(x = Age, y = GFR)) +
  geom_point(color = "orange") + 
  geom_smooth(method = "lm", color = "red") +
  labs(title = "GFR vs. Age", x = "Age", y = "GFR (Glomerular Filtration Rate)")
p2 <- ggplot(data, aes(x = fitted_values, y = residuals)) +
  geom_point(color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs. Fitted Values", x = "Fitted GFR Values", y = "Residuals")
p3 <- ggplot(data, aes(x = fitted_values, y = GFR)) +
  geom_point(color = "purple") +
  geom_abline(slope = 1, intercept = 0, color = "green", linetype = "dashed") +
  labs(title = "Predicted vs. Observed GFR", x = "Predicted GFR", y = "Observed GFR")
interactive_plot <- ggplotly(p1)
print(p1)
print(p2)
print(p3)
print(interactive_plot)
corr_matrix <- cor(data[, c("Age", "BMI", "SystolicBP", "DiastolicBP", "HbA1c", "SerumCreatinine", "ProteinInUrine", "GFR")], use = "complete.obs")
print(corr_matrix)
corr_matrix <- cor(data[, c("Age", "BMI", "SystolicBP", "DiastolicBP", "HbA1c", "SerumCreatinine", "ProteinInUrine", "GFR")], use = "complete.obs")
print(corr_matrix)
corrplot(corr_matrix, method = "number")

#anova
library(ggplot2)
library(dplyr)
data <- data %>%
  mutate(BP_Category = case_when(
    SystolicBP < 120 ~ "Low",
    SystolicBP >= 120 & SystolicBP < 140 ~ "Normal",
    SystolicBP >= 140 ~ "High"
  ))
data$BP_Category <- factor(data$BP_Category, levels = c("Low", "Normal", "High"))
anova_model <- aov(SerumCreatinine ~ BP_Category, data = data)
summary(anova_model)
p <- ggplot(data, aes(x = BP_Category, y = SerumCreatinine, fill = BP_Category)) +
  geom_boxplot() +
  labs(title = "Serum Creatinine across Blood Pressure Categories", x = "Blood Pressure Category", y = "Serum Creatinine") +
  theme_minimal() +
  scale_fill_manual(values = c("lightblue", "lightgreen", "salmon"))
print(p)

#clustering - k-means
library(ggplot2)
library(dplyr)
data_clustering <- data %>%
  select(BMI, DiastolicBP, HbA1c, GFR, ProteinInUrine) %>%
  na.omit() 
data_scaled <- scale(data_clustering)
set.seed(123)  
k <- 3
kmeans_result <- kmeans(data_scaled, centers = k, nstart = 25)
data_clustering$Cluster <- as.factor(kmeans_result$cluster)
p <- ggplot(data_clustering, aes(x = GFR, y = HbA1c, color = Cluster)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "K-means Clustering of Patients by Kidney Health Indicators",
       x = "GFR (Glomerular Filtration Rate)",
       y = "HbA1c (%)",
       color = "Cluster") +
  theme_minimal()
print(p)
print("Cluster Centers:\n")
print(kmeans_result$centers)
print("Cluster Sizes:\n")
print(table(data_clustering$Cluster))

#association-apriori
library(dplyr)
library(arules)
library(arulesViz)
data_association <- data %>%
  select(Smoking, AlcoholConsumption, PhysicalActivity, FamilyHistoryKidneyDisease,
         FamilyHistoryHypertension, FamilyHistoryDiabetes, ProteinInUrine, Edema, NauseaVomiting) %>%
  mutate(across(everything(), as.factor)) 
transactions <- as(data_association, "transactions")
rules <- apriori(transactions, parameter = list(support = 0.1, confidence = 0.6, minlen = 2))
summary(rules)
inspect(head(sort(rules, by = "lift"), 10))  
plot(rules, method = "graph", control = list(type = "items"))
plot(rules, measure = c("support", "confidence"), shading = "lift")

#classification-KNN
library(class)
library(ggplot2)
library(caret)
data <- read_excel("C:/Users/DELL/Downloads/data science lab/Mini major/Chronic_Kidney_Dsease_data.xlsx")
data$Diagnosis <- factor(data$Diagnosis, levels = c(0, 1), labels = c("No Chronic Kidney Disease", "Chronic Kidney Disease"))
train_data <- data[, -which(names(data) == "Diagnosis")]
new_patient <- data.frame(
  Age = 37, 
  BMI = 27.5, 
  SystolicBP = 82, 
  DiastolicBP = 80, 
  FastingBloodSugar = 140, 
  HbA1c = 6.2, 
  SerumCreatinine = 1.1,
  BUNLevels = 15, 
  GFR = 75, 
  ProteinInUrine = 0.1, 
  ACR = 10,
  SerumElectrolytesSodium = 140, 
  SerumElectrolytesPotassium = 4.1, 
  SerumElectrolytesCalcium = 9.0, 
  SerumElectrolytesPhosphorus = 4.5,
  HemoglobinLevels = 12.5, 
  CholesterolTotal = 200, 
  CholesterolLDL = 120,
  CholesterolHDL = 40, 
  CholesterolTriglycerides = 150
)

print(names(train_data))  
print(names(new_patient)) 
train_labels <- data$Diagnosis
print(paste("Prediction for new patient: ", knn_prediction))
ggplot(data, aes(x = Age, y = FastingBloodSugar, color = Diagnosis)) +
  geom_point(size = 4) + 
  geom_point(data = new_patient, aes(x = Age, y = FastingBloodSugar), 
             color = "red", size = 5) +
  labs(title = "KNN Classification: Chronic Kidney Disease vs No Disease",
       x = "Age", y = "Fasting Blood Sugar Level") +
  theme_minimal()
