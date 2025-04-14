#What is the distribution of patient age?
library(ggplot2)
ggplot(data, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "#0073C2", color = "black", alpha = 0.7) +
  labs(
    title = "Age Distribution of Patients",
    x = "Age (Years)",
    y = "Number of Patients"
  ) +
  theme_minimal() +
  theme(
    title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

#What is the gender distribution of the patients?

ggplot(data, aes(x = as.factor(Gender))) +
  geom_bar(fill = "lightgreen", alpha = 0.7) +
  labs(title = "Gender Distribution", x = "Gender", y = "Count") +
  scale_x_discrete(labels = c("Male", "Female"))

#What is the ethnicity breakdown of the patients?

ggplot(data, aes(x = as.factor(Ethnicity))) +
  geom_bar(fill = "purple", alpha = 0.7) +
  labs(title = "Ethnicity Breakdown", x = "Ethnicity", y = "Count") +
  scale_x_discrete(labels = c("Caucasian", "African American", "Asian", "Other"))

#How is BMI distributed among patients?

ggplot(data, aes(x = BMI)) +
  geom_histogram(binwidth = 2, fill = "orange", alpha = 0.7) +
  labs(title = "BMI Distribution", x = "BMI", y = "Count")

ggplot(data, aes(y = BMI)) +
  geom_boxplot(fill = "lightgreen", color = "darkgreen", alpha = 0.7, outlier.color = "red", outlier.size = 2) +
  theme_minimal() +
  labs(title = "BMI Boxplot", y = "BMI") +
  theme(panel.grid.major = element_line(color = "grey85"))

#How does alcohol consumption vary by gender?

ggplot(data, aes(x = as.factor(Gender), y = AlcoholConsumption)) +
  geom_boxplot(fill = "yellow", alpha = 0.7) +
  labs(title = "Alcohol Consumption by Gender", x = "Gender", y = "Alcohol Consumption (units)") +
  scale_x_discrete(labels = c("Male", "Female"))

#What is the relationship between physical activity and socioeconomic status?

ggplot(data, aes(x = as.factor(SocioeconomicStatus), y = PhysicalActivity)) +
  geom_boxplot(fill = "lightblue", alpha = 0.7) +
  labs(title = "Physical Activity by Socioeconomic Status", x = "Socioeconomic Status", y = "Physical Activity (hours)") +
  scale_x_discrete(labels = c("Low", "Middle", "High"))

#What is the distribution of systolic and diastolic blood pressure?
library(ggplot2)

bp_data <- data.frame(
  BloodPressure = c(data$SystolicBP, data$DiastolicBP),
  Type = rep(c("Systolic", "Diastolic"), each = nrow(data))
)
ggplot(bp_data, aes(x = Type, y = BloodPressure, fill = Type)) +
  geom_boxplot(alpha = 0.6) +
  labs(title = "Box Plot of Systolic and Diastolic Blood Pressure",
       x = "Blood Pressure Type",
       y = "Blood Pressure (mmHg)") +
  theme_minimal() +
  scale_fill_manual(values = c("blue", "red"))

#How does sleep quality vary with education level?

ggplot(data, aes(x = as.factor(EducationLevel), y = SleepQuality)) +
  geom_boxplot(fill = "cyan", alpha = 0.7) +
  labs(title = "Sleep Quality by Education Level", x = "Education Level", y = "Sleep Quality") +
  scale_x_discrete(labels = c("None", "High School", "Bachelor's", "Higher"))

#What is the relationship between BMI and diet quality?

ggplot(data, aes(x = DietQuality, y = BMI)) +
  geom_point(color = "purple", alpha = 0.7) +
  labs(title = "BMI vs. Diet Quality", x = "Diet Quality", y = "BMI")


#How often do patients with a family history of diabetes experience kidney disease?

ggplot(data, aes(x = as.factor(FamilyHistoryDiabetes), fill = as.factor(Diagnosis))) +
  geom_bar(position = "fill", alpha = 0.7, color = "black") +
  labs(title = "Proportion of Kidney Disease Diagnosis Based on Family History of Diabetes", 
       x = "Family History of Diabetes", 
       y = "Proportion of Diagnosis") +
  scale_x_discrete(labels = c("No", "Yes")) +
  scale_fill_manual(name = "Diagnosis", 
                    values = c("#1f78b4", "#33a02c"), 
                    labels = c("No CKD", "CKD")) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.position = "right"
  )


#How does fasting blood sugar vary with smoking status?
ggplot(data, aes(x = as.factor(Smoking), y = FastingBloodSugar)) +
  geom_boxplot(fill = "orange", alpha = 0.7) +
  labs(title = "Fasting Blood Sugar by Smoking Status", x = "Smoking", y = "Fasting Blood Sugar (mg/dL)") +
  scale_x_discrete(labels = c("Non-Smoker", "Smoker"))

#What is the relationship between hemoglobin levels and fatigue?
library(ggplot2)
library(dplyr)
data_binned <- data %>%
  mutate(HemoglobinBins = cut(HemoglobinLevels, breaks = seq(min(HemoglobinLevels), max(HemoglobinLevels), by = 1))) %>%
  group_by(HemoglobinBins) %>%
  summarize(MeanFatigueLevels = mean(FatigueLevels, na.rm = TRUE))

ggplot(data_binned, aes(x = HemoglobinBins, y = MeanFatigueLevels)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Average Fatigue Levels by Hemoglobin Level Ranges", x = "Hemoglobin Level Range (g/dL)", y = "Average Fatigue Levels") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

#How does protein in urine vary with glomerular filtration rate (GFR)?
library(dplyr)
data_binned <- data %>%
  mutate(GFRBins = cut(GFR, breaks = seq(min(GFR), max(GFR), length.out = 10))) %>%
  group_by(GFRBins) %>%
  summarize(MeanProteinInUrine = mean(ProteinInUrine, na.rm = TRUE))

ggplot(data_binned, aes(x = GFRBins, y = MeanProteinInUrine)) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(title = "Average Protein in Urine by GFR Ranges", x = "GFR Range (mL/min/1.73 m²)", y = "Average Protein in Urine (g/day)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#What is the distribution of cholesterol levels (Total, LDL, HDL)
library(ggplot2)
library(dplyr)
total_cholesterol_summary <- data %>%
  mutate(CholesterolTotalBin = cut(CholesterolTotal, breaks = seq(0, 300, by = 20))) %>%
  count(CholesterolTotalBin)
ggplot(total_cholesterol_summary, aes(x = "", y = n, fill = CholesterolTotalBin)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  labs(title = "Total Cholesterol Distribution") +
  theme_void()
ldl_cholesterol_summary <- data %>%
  mutate(CholesterolLDLBin = cut(CholesterolLDL, breaks = seq(0, 200, by = 20))) %>%
  count(CholesterolLDLBin)
ggplot(ldl_cholesterol_summary, aes(x = "", y = n, fill = CholesterolLDLBin)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  labs(title = "LDL Cholesterol Distribution") +
  theme_void()
hdl_cholesterol_summary <- data %>%
  mutate(CholesterolHDLBin = cut(CholesterolHDL, breaks = seq(0, 100, by = 10))) %>%
  count(CholesterolHDLBin)
ggplot(hdl_cholesterol_summary, aes(x = "", y = n, fill = CholesterolHDLBin)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  labs(title = "HDL Cholesterol Distribution") +
  theme_void()

#How does medication adherence vary with quality of life scores?
ggplot(data, aes(x = MedicationAdherence, y = QualityOfLifeScore)) +
  geom_point(color = "brown", alpha = 0.7) +
  labs(title = "Medication Adherence vs. Quality of Life Score", x = "Medication Adherence", y = "Quality of Life Score")

# Scenario-:What is the probability that out of 10 randomly selected patients, 
# at least 6 are diagnosed with Chronic Kidney Disease (CKD)?
#Binomial
p_ckd <- mean(data$Diagnosis == 1)
n_patients <- 10
q_binom <- qbinom(0.7, size = n_patients, prob = p_ckd)
r_binom <- rbinom(10, size = n_patients, prob = p_ckd)
d_binom <- dbinom(6, size = n_patients, prob = p_ckd)
p_binom <- pbinom(6, size = n_patients, prob = p_ckd)
print(q_binom)
print(r_binom)
print(d_binom)
print(p_binom)

#Scenario -: What is the probability that a randomly selected patient has a serum creatinine level above 2.5 mg/dL, 
#assume the serum creatine levels follow a normal distribution with a mean of 1.2 mg/dL and a standard deviation of 0.8 mg/dL?
#normal
mean_creatinine <- 1.2
sd_creatinine <- 0.8
q_norm <- qnorm(0.95, mean = mean_creatinine, sd = sd_creatinine)
r_norm <- rnorm(10, mean = mean_creatinine, sd = sd_creatinine)
d_norm <- dnorm(2.0, mean = mean_creatinine, sd = sd_creatinine)
p_norm <- pnorm(2.5, mean = mean_creatinine, sd = sd_creatinine)
print(q_norm)
print(r_norm)
print(d_norm)
print(p_norm)

#Scenario -: What is the probability that a patient has more than 3 urinary tract 
#infections (UTIs) in a year, given that the average number of UTIs per year is 1?
#poisson
lambda_uti <- 1
q_pois_uti <- qpois(0.9, lambda = lambda_uti)
r_pois_uti <- rpois(10, lambda = lambda_uti)
d_pois_uti <- dpois(3, lambda = lambda_uti)
p_pois_uti <- ppois(3, lambda = lambda_uti)
print(q_pois_uti)
print(r_pois_uti)
print(d_pois_uti)
print(p_pois_uti)

summary_stats <- summary(data)

print(summary_stats)
library(dplyr)

descriptive_stats <- data %>%
  summarize(
    Age_mean = mean(Age, na.rm = TRUE),
    Age_median = median(Age, na.rm = TRUE),
    Age_sd = sd(Age, na.rm = TRUE),
    Age_min = min(Age, na.rm = TRUE),
    Age_max = max(Age, na.rm = TRUE),
    BMI_mean = mean(BMI, na.rm = TRUE),
    BMI_median = median(BMI, na.rm = TRUE),
    BMI_sd = sd(BMI, na.rm = TRUE),
    BMI_min = min(BMI, na.rm = TRUE),
    BMI_max = max(BMI, na.rm = TRUE),
    
    Smoking_percentage = mean(Smoking == 1, na.rm = TRUE) * 100,
    Alcohol_mean = mean(AlcoholConsumption, na.rm = TRUE),
    PhysicalActivity_mean = mean(PhysicalActivity, na.rm = TRUE),
    DietQuality_mean = mean(DietQuality, na.rm = TRUE),
    SleepQuality_mean = mean(SleepQuality, na.rm = TRUE),
    
    FamilyHistoryKidneyDisease_percentage = mean(FamilyHistoryKidneyDisease == 1, na.rm = TRUE) * 100,
    FamilyHistoryHypertension_percentage = mean(FamilyHistoryHypertension == 1, na.rm = TRUE) * 100,
    FamilyHistoryDiabetes_percentage = mean(FamilyHistoryDiabetes == 1, na.rm = TRUE) * 100,
    
    SystolicBP_mean = mean(SystolicBP, na.rm = TRUE),
    SystolicBP_median = median(SystolicBP, na.rm = TRUE),
    SystolicBP_sd = sd(SystolicBP, na.rm = TRUE),
    
    DiastolicBP_mean = mean(DiastolicBP, na.rm = TRUE),
    FastingBloodSugar_mean = mean(FastingBloodSugar, na.rm = TRUE),
    HbA1c_mean = mean(HbA1c, na.rm = TRUE),
    SerumCreatinine_mean = mean(SerumCreatinine, na.rm = TRUE),
    GFR_mean = mean(GFR, na.rm = TRUE),
    ProteinInUrine_mean = mean(ProteinInUrine, na.rm = TRUE),
    ACR_mean = mean(ACR, na.rm = TRUE),

    SerumSodium_mean = mean(SerumElectrolytesSodium, na.rm = TRUE),
    SerumPotassium_mean = mean(SerumElectrolytesPotassium, na.rm = TRUE),
    SerumCalcium_mean = mean(SerumElectrolytesCalcium, na.rm = TRUE),
    SerumPhosphorus_mean = mean(SerumElectrolytesPhosphorus, na.rm = TRUE),
    
    HemoglobinLevels_mean = mean(HemoglobinLevels, na.rm = TRUE),
    CholesterolTotal_mean = mean(CholesterolTotal, na.rm = TRUE),
    CholesterolLDL_mean = mean(CholesterolLDL, na.rm = TRUE),
    CholesterolHDL_mean = mean(CholesterolHDL, na.rm = TRUE),
    CholesterolTriglycerides_mean = mean(CholesterolTriglycerides, na.rm = TRUE),
    
    FatigueLevels_mean = mean(FatigueLevels, na.rm = TRUE),
    NauseaVomiting_mean = mean(NauseaVomiting, na.rm = TRUE),
    MuscleCramps_mean = mean(MuscleCramps, na.rm = TRUE),
    Itching_mean = mean(Itching, na.rm = TRUE),
    QualityOfLifeScore_mean = mean(QualityOfLifeScore, na.rm = TRUE)
  )

cat("Age - Mean:", descriptive_stats$Age_mean, "\n",
    "Age - Median:", descriptive_stats$Age_median, "\n",
    "Age - Standard Deviation:", descriptive_stats$Age_sd, "\n",
    "Age - Minimum:", descriptive_stats$Age_min, "\n",
    "Age - Maximum:", descriptive_stats$Age_max, "\n\n",
    
    "BMI - Mean:", descriptive_stats$BMI_mean, "\n",
    "BMI - Median:", descriptive_stats$BMI_median, "\n",
    "BMI - Standard Deviation:", descriptive_stats$BMI_sd, "\n",
    "BMI - Minimum:", descriptive_stats$BMI_min, "\n",
    "BMI - Maximum:", descriptive_stats$BMI_max, "\n\n",
    
    "Smoking Percentage:", descriptive_stats$Smoking_percentage, "%\n",
    "Alcohol Consumption (Mean):", descriptive_stats$Alcohol_mean, "\n",
    "Physical Activity (Mean):", descriptive_stats$PhysicalActivity_mean, "\n",
    "Diet Quality (Mean):", descriptive_stats$DietQuality_mean, "\n",
    "Sleep Quality (Mean):", descriptive_stats$SleepQuality_mean, "\n\n",
    
    "Family History of Kidney Disease Percentage:", descriptive_stats$FamilyHistoryKidneyDisease_percentage, "%\n",
    "Family History of Hypertension Percentage:", descriptive_stats$FamilyHistoryHypertension_percentage, "%\n",
    "Family History of Diabetes Percentage:", descriptive_stats$FamilyHistoryDiabetes_percentage, "%\n\n",
    
    "Systolic Blood Pressure - Mean:", descriptive_stats$SystolicBP_mean, "\n",
    "Systolic Blood Pressure - Median:", descriptive_stats$SystolicBP_median, "\n",
    "Systolic Blood Pressure - Standard Deviation:", descriptive_stats$SystolicBP_sd, "\n",
    "Diastolic Blood Pressure - Mean:", descriptive_stats$DiastolicBP_mean, "\n\n",
    
    "Fasting Blood Sugar - Mean:", descriptive_stats$FastingBloodSugar_mean, "\n",
    "HbA1c - Mean:", descriptive_stats$HbA1c_mean, "\n",
    "Serum Creatinine - Mean:", descriptive_stats$SerumCreatinine_mean, "\n",
    "GFR - Mean:", descriptive_stats$GFR_mean, "\n",
    "Protein in Urine - Mean:", descriptive_stats$ProteinInUrine_mean, "\n",
    "ACR - Mean:", descriptive_stats$ACR_mean, "\n\n",
    
    "Serum Sodium - Mean:", descriptive_stats$SerumSodium_mean, "\n",
    "Serum Potassium - Mean:", descriptive_stats$SerumPotassium_mean, "\n",
    "Serum Calcium - Mean:", descriptive_stats$SerumCalcium_mean, "\n",
    "Serum Phosphorus - Mean:", descriptive_stats$SerumPhosphorus_mean, "\n\n",
    
    "Hemoglobin Levels - Mean:", descriptive_stats$HemoglobinLevels_mean, "\n",
    "Total Cholesterol - Mean:", descriptive_stats$CholesterolTotal_mean, "\n",
    "LDL Cholesterol - Mean:", descriptive_stats$CholesterolLDL_mean, "\n",
    "HDL Cholesterol - Mean:", descriptive_stats$CholesterolHDL_mean, "\n",
    "Triglycerides - Mean:", descriptive_stats$CholesterolTriglycerides_mean, "\n\n",
    
    "Fatigue Levels - Mean:", descriptive_stats$FatigueLevels_mean, "\n",
    "Nausea/Vomiting - Mean:", descriptive_stats$NauseaVomiting_mean, "\n",
    "Muscle Cramps - Mean:", descriptive_stats$MuscleCramps_mean, "\n",
    "Itching - Mean:", descriptive_stats$Itching_mean, "\n",
    "Quality of Life Score - Mean:", descriptive_stats$QualityOfLifeScore_mean, "\n")
