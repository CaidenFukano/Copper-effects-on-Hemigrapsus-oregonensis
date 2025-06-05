# Load necessary libraries
library(ggplot2)
library(dplyr)

# Create the data frame
data <- data.frame(
  Group = c(rep("Low Concentration", 4), rep("Low Concentration", 3),
            rep("High Concentration", 4), "High Concentration",
            rep("Control", 4), rep("Control", 4)),
  RightingTime = c(3.48, 1.33, 3.96, 0.74, 2.2, 1.11, 2.0,
                   16.33, 11.08, 0.63, 4.38, 12.3,
                   3.36, 1.01, 0.71, 0.71, 3.36, 1.01, 0.71, 0.71),
  Week = as.factor(c(rep(1, 4), rep(2, 3),
                     rep(1, 4), 2,
                     rep(1, 4), rep(2, 4)))
)

# Summary stats: mean ± SE per group & week
summary_stats <- data %>%
  group_by(Group, Week) %>%
  summarise(
    mean_rt = mean(RightingTime),
    se = sd(RightingTime) / sqrt(n()),
    .groups = "drop"
  )

# Boxplot with jitter and error bars
ggplot(data, aes(x = interaction(Group, Week), y = RightingTime, fill = Group)) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA) +
  geom_jitter(width = 0.15, size = 2, alpha = 0.8) +
  geom_point(data = summary_stats, aes(y = mean_rt), 
             shape = 18, size = 3, color = "black") +
  geom_errorbar(data = summary_stats,
                aes(y = mean_rt, ymin = mean_rt - se, ymax = mean_rt + se),
                width = 0.2, color = "black") +
  labs(
    title = "Crab Righting Time by Group and Week",
    x = "Group and Week", y = "Righting Time (seconds)"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = "none")

