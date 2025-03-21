# Load the dataset
load("C:/Users/LENOVO/Desktop/R/Final_Work/df.rdata")


#### REGRESSION ----
#for the regression I subtracted from every PAS 1 so 0 represent the reference level
df$PASkey.keys <- df$PASkey.keys - 1

# Logistic Regression: predicting success by Perceptual Awareness Scale (PAS)
accuracy_by_PAS <- glm(WMresp_acc ~ PASkey.keys, 
                       data = df, 
                       family = binomial)

# Print summary of the logistic regression model
summary(accuracy_by_PAS)
#MODEL: y = 0.12 + 0.19 
#Both coefficient were significant


# Compute Odds Ratio (OR) and 95% Confidence Intervals
conf_int <- exp(confint.default(accuracy_by_PAS))  # Convert CI to OR scale
odds_ratios <- exp(coef(accuracy_by_PAS))  # Convert log-odds to OR


# Print results
print(conf_int)  
#               2.5 %   97.5 %
#(Intercept) 1.084883 1.182332
#PASkey.keys 1.167753 1.247595

print(odds_ratios)  
#(Intercept) PASkey.keys 
# 1.132560    1.207014

#Correlation
df$WMresp_acc <- as.numeric(df$WMresp_acc)# changing to numeric for Pearson correlation
cor.test(df$PASkey.keys, df$WMresp_acc, method = "pearson") #correlation
#cor = 0.09, t = 11.25, p-value < 2.2e-16

#Logistic regression plot
#moving the blue dots to the left
pas_summary$PASkey.num <- pas_summary$PASkey.keys - 1

ggplot(df, aes(x = PASkey.keys, y = WMresp_acc)) +  
  geom_smooth(method = "glm", method.args = list(family = "binomial"), 
              color = "red", fill = "pink", alpha = 0.3, se = TRUE) +  # Logistic regression curve
  geom_point(data = pas_summary, aes(x = PASkey.num, y = WM_Mean), color = "blue", size = 4) +  # שינוי רק בנקודות
  scale_x_continuous(breaks = c(0, 1, 2, 3)) +  
  scale_y_continuous(labels = percent_format()) +  
  labs(
    title = "Predicted Success Probability by PAS Score",
    x = "PAS Score (0-3)",
    y = "Probability of Success in WM Task",
    caption = "Logistic regression model with confidence intervals"
  ) +
  theme_minimal() +
  theme(text = element_text(size = 14))

#ROC
roc_curve <- roc(df$WMresp_acc, fitted(accuracy_by_PAS))
auc_value <- auc(roc_curve)

# Print AUC
print(auc_value)
# Area under the curve: 0.5598

# Plot ROC curve
ggplot(data.frame(TPR = rev(roc_curve$sensitivities), 
                  FPR = rev(1 - roc_curve$specificities)), 
       aes(x = FPR, y = TPR)) +
  geom_line(color = "blue", size = 1.2) +
  geom_abline(linetype = "dashed", color = "red") +
  labs(title = "ROC Curve",
       x = "False Positive Rate",
       y = "True Positive Rate") +
  theme_minimal()

# Calculate and print the AIC of the logistic regression model
aic_value_logistic <- AIC(accuracy_by_PAS)
print(aic_value_logistic)
# AIC = 19551.79

#Multiple Linear Regression: predicting PAS by contrast level
PAS_by_contrast_and_succession <- lm(PASkey.keys ~ contrast * WMresp_acc, data = df)
summary(PAS_by_contrast_and_succession)

#MODEL: y = 0.8 - 0.92 + 0.16 + 0.57

df$WMresp_acc <- as.factor(df$WMresp_acc)
ggplot(df, aes(x = contrast, y = PASkey.keys, color = factor(WMresp_acc))) +  
  geom_smooth(method = "lm", se = TRUE) +  
  scale_color_manual(values = c("red", "blue"), labels = c("Failure (0)", "Success (1)")) +  
  labs(
    title = "Regression of PAS on Contrast by Success/Failure", 
    x = "Contrast",
    y = "PAS",
    color = "Outcome"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(size = 13, hjust = 0.5, margin = margin(b = 5))
  )

aic_value_linear <- AIC(PAS_by_contrast_and_succession)
print(aic_value_linear)
#AIC = 41391.94


