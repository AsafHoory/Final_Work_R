# Load the data set
load("C:/Users/LENOVO/Desktop/R/Final_Work/df.rdata")

# Compute descriptive statistics (Mean & SE)
summary_stats <- df |> 
  summarise(
    PAS_Mean = mean(PASkey.keys), PAS_SE = sd(PASkey.keys) / sqrt(n()),
    WM_Mean = mean(WMresp_acc), WM_SE = sd(WMresp_acc) / sqrt(n()),
    Contrast_Mean = mean(contrast), Contrast_SE = sd(contrast) / sqrt(n())
  )

# Create a summary table with rounded values
summary_table <- data.frame(
  Variable = c("PAS Score", "WM Accuracy", "Contrast"),
  Mean = round(c(summary_stats$PAS_Mean, summary_stats$WM_Mean, summary_stats$Contrast_Mean), 3),
  SE = round(c(summary_stats$PAS_SE, summary_stats$WM_SE, summary_stats$Contrast_SE), 3)
)

# Display the table using kable
kable(summary_table, caption = "Summary Statistics: Mean and Standard Error")

# Compute mean and standard error of WM Accuracy for each PAS score
pas_summary <- df |> 
  group_by(PASkey.keys) |> 
  summarise(
    WM_Mean = round(mean(WMresp_acc), 3),
    WM_SE = round(sd(WMresp_acc) / sqrt(n()), 3)
  )

# Display summary table using kable
kable(pas_summary, caption = "WM Accuracy: Mean and Standard Error by PAS Score")

#Density plot for PAS
Mean <- mean(df$PASkey.keys)
Median <- median(df$PASkey.keys)

ggplot(df, aes(x = PASkey.keys)) +
  geom_density(aes(y = ..scaled..), 
               fill = "skyblue", alpha = 0.5, 
               color = "blue", size = 1.2) +
  geom_jitter(data = df[sample(nrow(df), 1000), ], 
              aes(y = -0.05), color = "black", 
              alpha = 0.3, height = 0.02) +  
  geom_vline(xintercept = Mean, color = "blue", linetype = "dotted", size = 1) +  
  geom_vline(xintercept = Median, color = "red", linetype = "dashed", size = 1) +  
  annotate("text", x = Mean, y = 0.9, label = "Mean", color = "blue", angle = 90, vjust = -0.5) +  
  annotate("text", x = Median, y = 0.9, label = "Median", color = "red",angle = 90, vjust = -0.5) +  
  scale_x_continuous(breaks = seq(floor(min(df$PASkey.keys)), ceiling(max(df$PASkey.keys)), by = 1)) +
  labs(
    title = "Distribution of PAS Scores",
    subtitle = "Density Plot with Mean, Median, and Sampled Observations",
    x = "PAS Score",
    y = "Density (Scaled)"
  ) +
  theme_minimal()

# Histogram for WM Accuracy (Success vs. Failure) 
ggplot(df, aes(x = factor(WMresp_acc, levels = c(0, 1), labels = c("Failure", "Success")))) +  
  geom_bar(fill = "lightgreen", alpha = 0.6) +  
  geom_text(stat = "count", aes(label = ..count..), 
            vjust = -0.3, size = 5, fontface = "bold") +  
  ylim(0, max(table(df$WMresp_acc)) * 1.1) + 
  labs(title = "Histogram of Success Rate", x = "Outcome", y = "Frequency") +
  theme_minimal()

# Density plot for Contrast 
ggplot(df, aes(x = contrast)) +
  geom_density(fill = "lightblue", alpha = 0.5, color = "blue", size = 1.2) +  
  geom_vline(xintercept = summary_stats$Contrast_Mean, color = "red", linetype = "dashed", size = 1) +  
  geom_vline(xintercept = median(df$contrast), color = "purple", linetype = "dotted", size = 1) +  
  annotate("text", x = summary_stats$Contrast_Mean + 0.025, 
           y = 0.6 * max(density(df$contrast)$y), label = "Mean", color = "red", angle = 90, hjust = -1, size = 5) +  
  annotate("text", x = median(df$contrast) + 0.05, 
           y = 0.5 * max(density(df$contrast)$y), label = "Median", color = "purple", angle = 90, hjust = 0, size = 5) +  
  labs(title = "Density Plot of Contrast", x = "Contrast", y = "Density") +
  theme_minimal()

# Bar plot for accuracy in each PAS 
ggplot(pas_summary, aes(x = PASkey.keys, y = WM_Mean)) +
  geom_point(size = 4, color = "blue") +  # Points for each PAS score
  geom_line(group = 1, color = "blue", size = 1) +  # Line connecting points
  geom_errorbar(aes(ymin = WM_Mean - WM_SE, ymax = WM_Mean + WM_SE), 
                width = 0.2, color = "black", size = 1) +  # Error bars
  labs(
    title = "WM Accuracy by PAS Score",
    x = "PAS Score",
    y = "Mean WM Accuracy"
  ) +
  theme_minimal()

##Plot of PAS in each level of contrast
# Step 1: Define the number of bins (4 groups)
bin_size <- max(df$contrast) / 4  

# Step 2: Create a new factor variable by binning contrast
df <- df |> 
  mutate(contrast_factor = cut(contrast, 
                               breaks = seq(0, max(contrast), by = bin_size), 
                               include.lowest = TRUE, 
                               labels = c("Low", "Medium-Low", "Medium-High", "High")))

# Step 3: Calculate mean PAS and standard error (SE) for each contrast bin
PAS_by_contrast <- df |>
  group_by(contrast_factor) |>  
  summarise(
    mean_PAS = round(mean(PASkey.keys), 3),
    se_PAS_by_factor = round(sd(PASkey.keys) / sqrt(n()), 3)
  )

# Step 4: Create plot
ggplot(PAS_by_contrast, aes(x = contrast_factor, y = mean_PAS, group = 1)) +
  geom_point(size = 4, color = "blue") +  # Consistent with other plots
  geom_line(color = "blue", linewidth = 1) +  
  geom_errorbar(aes(ymin = mean_PAS - se_PAS_by_factor, ymax = mean_PAS + se_PAS_by_factor),
                width = 0.2, color = "black", size = 1) +  
  labs(
    title = "Mean PAS Score by Contrast Level (Binned)",
    x = "Contrast Level",
    y = "Mean PAS Score"
  ) +
  theme_minimal()

save(df, file = "C:/Users/LENOVO/Desktop/R/Final_Work/df.rdata")

