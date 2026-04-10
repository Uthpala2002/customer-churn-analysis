###############################################
# 📊 SPEEDYCALL CUSTOMER CHURN ANALYSIS PLOTS #
# Using ggplot2 (R)                           #
###############################################

# Load required libraries
library(ggplot2)
library(dplyr)
library(scales)
library(broom)
install.packages("broom")

# Make sure your dataset is loaded as: data
# Example: data <- read.csv("SpeedyCall_Customer_Churn.csv")
data <- read.csv("C:\\Users\\Uthpala\\Downloads\\SpeedyCall_Customer_Churn_Data.csv")

# See the column names
names(data)

# See the first few rows
head(data)

# Check the structure of the dataset
str(data)

# Check if 'Churn' variable looks correct (Yes/No or 1/0)
table(data$Churn)


###################################################
# 🎯 OBJECTIVE 1:
# To identify key demographic, behavioral, and service-related
# factors influencing customer churn among SpeedyCall customers.
###################################################

# Example 1: Contract Type vs Churn (percentage bar plot)
data %>%
  group_by(Contract, Churn) %>%
  summarise(Count = n()) %>%
  mutate(Percent = Count / sum(Count) * 100) %>%
  ggplot(aes(x = Contract, y = Percent, fill = Churn)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(Percent, 1), "%")),
            position = position_dodge(0.9), vjust = -0.3, size = 3) +
  scale_fill_manual(values = c("No" = "gray30", "Yes" = "lightblue")) +
  labs(title = "Customer Churn by Contract Type",
       x = "Contract Type", y = "Percentage of Customers") +
  theme_minimal()

# Example 2: InternetService vs Churn (percentage bar plot)
data %>%
  group_by(InternetService, Churn) %>%
  summarise(Count = n()) %>%
  mutate(Percent = Count / sum(Count) * 100) %>%
  ggplot(aes(x = InternetService, y = Percent, fill = Churn)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(Percent, 1), "%")),
            position = position_dodge(0.9), vjust = -0.3, size = 3) +
  scale_fill_manual(values = c("No" = "gray30", "Yes" = "lightblue")) +
  labs(title = "Customer Churn by InternetService",
       x = "InternetService Type", y = "Percentage of Customers") +
  theme_minimal()

# Example 3: PhoneService vs Churn (percentage bar plot)
data %>%
  group_by(PhoneService, Churn) %>%
  summarise(Count = n()) %>%
  mutate(Percent = Count / sum(Count) * 100) %>%
  ggplot(aes(x = PhoneService, y = Percent, fill = Churn)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(Percent, 1), "%")),
            position = position_dodge(0.9), vjust = -0.3, size = 3) +
  scale_fill_manual(values = c("No" = "gray30", "Yes" = "lightblue")) +
  labs(title = "Customer Churn by PhoneService",
       x = "PhoneService Type", y = "Percentage of Customers") +
  theme_minimal()

# Example 4: SeniorCitizen vs Churn (percentage bar plot)
data %>%
  group_by(SeniorCitizen, Churn) %>%
  summarise(Count = n()) %>%
  mutate(Percent = Count / sum(Count) * 100) %>%
  ggplot(aes(x = SeniorCitizen, y = Percent, fill = Churn)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(Percent, 1), "%")),
            position = position_dodge(0.9), vjust = -0.3, size = 3) +
  scale_fill_manual(values = c("No" = "gray30", "Yes" = "lightblue")) +
  labs(title = "Customer Churn by SeniorCitizen",
       x = "SeniorCitizen Type", y = "Percentage of Customers") +
  theme_minimal()

# Example 5: Dependents vs Churn (percentage bar plot)
data %>%
  group_by(Dependents, Churn) %>%
  summarise(Count = n()) %>%
  mutate(Percent = Count / sum(Count) * 100) %>%
  ggplot(aes(x = Dependents, y = Percent, fill = Churn)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(Percent, 1), "%")),
            position = position_dodge(0.9), vjust = -0.3, size = 3) +
  scale_fill_manual(values = c("No" = "gray30", "Yes" = "lightblue")) +
  labs(title = "Customer Churn by Dependents",
       x = "Dependents Type", y = "Percentage of Customers") +
  theme_minimal()

# Example 6: Partner vs Churn (percentage bar plot)
data %>%
  group_by(Partner, Churn) %>%
  summarise(Count = n()) %>%
  mutate(Percent = Count / sum(Count) * 100) %>%
  ggplot(aes(x = Partner, y = Percent, fill = Churn)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(Percent, 1), "%")),
            position = position_dodge(0.9), vjust = -0.3, size = 3) +
  scale_fill_manual(values = c("No" = "gray30", "Yes" = "lightblue")) +
  labs(title = "Customer Churn by Partner",
       x = "Partner", y = "Percentage of Customers") +
  theme_minimal()

# Example 7: gender vs Churn (percentage bar plot)
data %>%
  group_by(gender, Churn) %>%
  summarise(Count = n()) %>%
  mutate(Percent = Count / sum(Count) * 100) %>%
  ggplot(aes(x = gender, y = Percent, fill = Churn)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(Percent, 1), "%")),
            position = position_dodge(0.9), vjust = -0.3, size = 3) +
  labs(title = "Customer Churn by Gender",
       x = "Gender", y = "Percentage of Customers") +
  theme_minimal()

# You can replace 'Contract' with 'InternetService', 'PhoneService', or 'SeniorCitizen'
# to explore other categorical variables.

###################################################
# 🎯 OBJECTIVE 2:
# To develop a statistical model that predicts the likelihood of churn.
###################################################

# Fit a logistic regression model
model <- glm(Churn ~ Contract + tenure + MonthlyCharges,data = data, family = binomial)

# Add predicted probabilities
data$PredictedProb <- predict(model, type = "response")

# Plot predicted probabilities by Contract Type
ggplot(data, aes(x = Contract, y = PredictedProb, fill = Contract)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "text",
               aes(label = paste0(round(..y.. * 100, 1), "%")),
               vjust = -0.5, color = "black", size = 3.5) +
  scale_fill_manual(values = c("Month-to-month" = "gray30", "One year" = "lightblue", "Two year"="green")) +
  labs(title = "Predicted Churn Probability by Contract Type",
       x = "Contract Type", y = "Predicted Probability of Churn") +
  theme_minimal()

###################################################
# 🎯 OBJECTIVE 3:
# To compare churned vs non-churned customer profiles.
###################################################

# A. Boxplot comparison for numeric variable (Monthly Charges)
ggplot(data, aes(x = Churn, y = MonthlyCharges, fill = Churn)) +
  geom_boxplot() + scale_fill_manual(values = c("No" = "gray30", "Yes" = "lightblue")) +
  labs(title = "Comparison of Monthly Charges by Churn Status",
       x = "Churn Status", y = "Monthly Charges (USD)") +
  theme_minimal()

#Boxplot comparison for numeric variable (tenure)
ggplot(data, aes(x = Churn, y = tenure, fill = Churn)) +
  geom_boxplot() + scale_fill_manual(values = c("No" = "gray30", "Yes" = "lightblue")) +
  labs(title = "Comparison of Tenure by Churn Status",
       x = "Churn Status", y = "Tenure") +
  theme_minimal()

#Boxplot comparison for numeric variable (TotalCharges)
ggplot(data, aes(x = Churn, y = TotalCharges, fill = Churn)) +
  geom_boxplot() + scale_fill_manual(values = c("No" = "gray30", "Yes" = "lightblue")) +
  labs(title = "Comparison of Total Charges by Churn Status",
       x = "Churn Status", y = "Total Charges") +
  theme_minimal()

# B. Bar plot comparison for categorical variable (Gender)
data %>%
  group_by(gender, Churn) %>%
  summarise(Count = n()) %>%
  mutate(Percent = Count / sum(Count) * 100) %>%
  ggplot(aes(x = gender, y = Percent, fill = Churn)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(Percent, 1), "%")),
            position = position_dodge(0.9), vjust = -0.3, size = 3) +
  scale_fill_manual(values = c("No" = "gray30", "Yes" = "lightblue")) +
  labs(title = "Churn Percentage by Gender",
       x = "Gender", y = "Percentage of Customers") +
  theme_minimal()


##############################################
# OBJECTIVE 4: Key Factors Influencing Churn
# Plotting Odds Ratios from Logistic Model
##############################################

library(ggplot2)
library(dplyr)
library(broom)

# Recode churn
data$Churn_num <- ifelse(data$Churn == "Yes", 1, 0)

# Fit logistic model
model <- glm(Churn_num ~ Contract + tenure + MonthlyCharges,
             data = data,
             family = binomial)

# Check model
summary(model)

# Create model summary table
model_summary <- tidy(model) %>%
  mutate(OddsRatio = exp(estimate)) %>%
  filter(term != "(Intercept)")

# Print to confirm
model_summary

# Plot odds ratios
ggplot(model_summary, aes(x = reorder(term, OddsRatio), y = OddsRatio)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = round(OddsRatio, 2)),
            vjust = -0.4, size = 3.5) +
  coord_flip() +
  labs(title = "Key Factors Influencing Customer Churn (Odds Ratios)",
       x = "Predictor Variable",
       y = "Odds Ratio (>1 = Higher Risk of Churn)") +
  theme_minimal()


