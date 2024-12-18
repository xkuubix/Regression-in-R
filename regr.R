library(ggplot2)
library(caret)
library(cowplot)

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
data <- read.csv("data.csv")
set.seed(42)
print(table(data$group))


train_index <- caret::createDataPartition(data$group, p = 0.7, list = FALSE)
train_data <- data[train_index, ]
val_data <- data[-train_index, ]

print(table(train_data$group))
print(table(val_data$group))

model <- lm(dependent ~ time + var1 + var2 + var3 + var4, data = train_data)

print(summary(model))

train_data$predicted <- predict(model, newdata = train_data)
val_data$predicted <- predict(model, newdata = val_data)
train_rmse <- sqrt(mean((train_data$dependent - train_data$predicted)^2))
val_rmse <- sqrt(mean((val_data$dependent - val_data$predicted)^2))
cat("Train RMSE:", train_rmse, "\nVal RMSE:", val_rmse, "\n")

train_r_squared <- summary(model)$r.squared
val_r_squared <- 1 - sum(
                         (val_data$dependent - val_data$predicted)^2) /
  sum((val_data$dependent - mean(val_data$dependent))^2)
cat("Train R-squared:",
    train_r_squared, "\nVal R-squared:", val_r_squared, "\n")

RSQUARE = function(y_actual,y_predict){
  cor(y_actual,y_predict)^2
}
train_r_squared_g1 <- RSQUARE(train_group1$dependent, train_group1$predicted)
train_r_squared_g2 <- RSQUARE(train_group2$dependent, train_group2$predicted)
cat("Train Group 1 R-squared:", train_r_squared_g1, "\nTrain Group 2 R-squared:", train_r_squared_g2, "\n")

val_r_squared_g1 <- RSQUARE(val_group1$dependent, val_group1$predicted)
val_r_squared_g2 <- RSQUARE(val_group2$dependent, val_group2$predicted)
cat("Validation Group 1 R-squared:", val_r_squared_g1, "\nValidation Group 2 R-squared:", val_r_squared_g2, "\n")




# Normalize train and validation data
train_data$dependent <- normalize(train_data$dependent)
train_data$predicted <- normalize(train_data$predicted)
val_data$dependent <- normalize(val_data$dependent)
val_data$predicted <- normalize(val_data$predicted)

# 1. scatter train g1
train_group1 <- train_data[train_data$group == "group1", ]
p_train_g1 <- ggplot(train_group1, aes(x = dependent, y = predicted)) +
  geom_point(size = 1, color = "black", alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Train: Group 1", x = "Actual", y = "Predicted") +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
  scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  theme_minimal() + theme(panel.grid.minor = element_blank())

# 2. scatter train g2
train_group2 <- train_data[train_data$group == "group2", ]
p_train_g2 <- ggplot(train_group2, aes(x = dependent, y = predicted)) +
  geom_point(size = 1, color = "black", alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Train: Group 2", x = "Actual", y = "Predicted") +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
  scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  theme_minimal() + theme(panel.grid.minor = element_blank())

# 3. scatter val g1
val_group1 <- val_data[val_data$group == "group1", ]
p_val_g1 <- ggplot(val_group1, aes(x = dependent, y = predicted)) +
  geom_point(size = 1, color = "black", alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Validation: Group 1", x = "Actual", y = "Predicted") +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
  scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  theme_minimal() + theme(panel.grid.minor = element_blank())

# 4. scatter val g2
val_group2 <- val_data[val_data$group == "group2", ]
p_val_g2 <- ggplot(val_group2, aes(x = dependent, y = predicted)) +
  geom_point(size = 1, color = "black", alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Validation: Group 2", x = "Actual", y = "Predicted") +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
  scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  theme_minimal() + theme(panel.grid.minor = element_blank())

title_main <- "Scatter Plots for Train and Validation Data (normalized)"
combined_plot <- plot_grid(p_train_g1, p_train_g2, p_val_g1, p_val_g2,
                           nrow = 2, ncol = 2, labels = c("A", "B", "C", "D"),
                           title = title_main)
f_path <- "combined_train_val_scatterplots.jpeg"
ggsave(f_path, plot = combined_plot, width = 10, height = 8)

print(combined_plot)