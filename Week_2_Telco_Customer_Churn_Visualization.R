# WEEK 2 - TELCO CUSTOMER CHURN
# Data Visualization and Insight Communication

# Overall churn
table(WA_Fn_UseC_Telco_Customer_Churn$Churn, useNA = "ifany")
prop.table(table(WA_Fn_UseC_Telco_Customer_Churn$Churn)) * 100

# Contract vs Churn
contract_churn <- prop.table(
  table(
    WA_Fn_UseC_Telco_Customer_Churn$Contract,
    WA_Fn_UseC_Telco_Customer_Churn$Churn
  ),
  margin = 1
) * 100

barplot(
  contract_churn[, "Yes"],
  main = "Churn Rate by Contract Type",
  xlab = "Contract Type",
  ylab = "Churn Rate (%)",
  ylim = c(0, 50)
)

# Tenure vs Churn
tenure_churn <- prop.table(
  table(
    cut(
      WA_Fn_UseC_Telco_Customer_Churn$tenure,
      breaks = c(0, 12, 24, 48, 72),
      include.lowest = TRUE
    ),
    WA_Fn_UseC_Telco_Customer_Churn$Churn
  ),
  margin = 1
) * 100

barplot(
  tenure_churn[, "Yes"],
  main = "Churn Rate by Customer Tenure",
  xlab = "Tenure (Months)",
  ylab = "Churn Rate (%)",
  ylim = c(0, 50)
)

# Payment Method vs Churn
payment_churn <- prop.table(
  table(
    WA_Fn_UseC_Telco_Customer_Churn$PaymentMethod,
    WA_Fn_UseC_Telco_Customer_Churn$Churn
  ),
  margin = 1
) * 100

barplot(
  payment_churn[, "Yes"],
  main = "Churn Rate by Payment Method",
  xlab = "Payment Method",
  ylab = "Churn Rate (%)",
  ylim = c(0, 50),
  las = 2
)

# Internet Service vs Churn
internet_churn <- prop.table(
  table(
    WA_Fn_UseC_Telco_Customer_Churn$InternetService,
    WA_Fn_UseC_Telco_Customer_Churn$Churn
  ),
  margin = 1
) * 100

barplot(
  internet_churn[, "Yes"],
  main = "Churn Rate by Internet Service",
  xlab = "Internet Service",
  ylab = "Churn Rate (%)",
  ylim = c(0, 50)
)

# Senior Citizen vs Churn
senior_churn <- prop.table(
  table(
    WA_Fn_UseC_Telco_Customer_Churn$SeniorCitizen,
    WA_Fn_UseC_Telco_Customer_Churn$Churn
  ),
  margin = 1
) * 100

barplot(
  senior_churn[, "Yes"],
  names.arg = c("Non-Senior", "Senior Citizen"),
  main = "Churn Rate by Senior Citizen Status",
  xlab = "Customer Type",
  ylab = "Churn Rate (%)",
  ylim = c(0, 50)
)

# Gender vs Churn
gender_churn <- prop.table(
  table(
    WA_Fn_UseC_Telco_Customer_Churn$gender,
    WA_Fn_UseC_Telco_Customer_Churn$Churn
  ),
  margin = 1
) * 100

barplot(
  gender_churn[, "Yes"],
  names.arg = c("Female", "Male"),
  main = "Churn Rate by Gender",
  xlab = "Gender",
  ylab = "Churn Rate (%)",
  ylim = c(0, 50)
)

# Monthly Charges Histogram
hist(
  WA_Fn_UseC_Telco_Customer_Churn$MonthlyCharges,
  main = "Distribution of Monthly Charges",
  xlab = "Monthly Charges",
  ylab = "Number of Customers",
  breaks = 20
)

# Monthly Charges vs Total Charges
plot(
  WA_Fn_UseC_Telco_Customer_Churn$MonthlyCharges,
  WA_Fn_UseC_Telco_Customer_Churn$TotalCharges,
  main = "Monthly Charges vs Total Charges",
  xlab = "Monthly Charges",
  ylab = "Total Charges",
  pch = 19
)
