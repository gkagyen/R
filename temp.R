

my_iris <- iris
my_iris |> group_by(Species) |>
  summarise(mean_SL = mean(Sepal.Length),
            stdev_SL = sd(Sepal.Length),
            mean_SW = mean(Sepal.Width),
            stdev_SW = sd(Sepal.Width),
            mean_PL = mean(Petal.Length),
            stdev_PL = sd(Petal.Length),
            mean_PW = mean(Petal.Width),
            stdev_PW = sd(Petal.Width))

dummy_model <- dummyVars(~ Species, data = my_iris)

# Transform the data using the dummyVars model
encoded <- predict(dummy_model, newdata = my_iris)

#Combine the other variables
iris_encoded <- bind_cols(my_iris[ ,1:4], encoded)


lm(response ~ predictor1 + predictor_2, data = my_data)


B_housing <- as_tibble(Boston)
head(B_housing)

set.seed(111)
index <- createDataPartition(B_housing$medv, p = 0.75, list = FALSE)
trainData <- B_housing[index, ]
testData <- B_housing[-index, ]

lm_model <- train(
  medv ~ dis + rm + lstat + tax,
  data = trainData,
  method = "lm",
  trControl = trainControl(method = 'cv', number = 5)
)
print(lm_model)

summary((lm_model$finalModel))

# Make predictions on test data
predictions <- predict(lm_model, newdata = testData)

# Calculate RMSE and R-squared
postResample(pred = predictions, obs = testData$medv)

testData |> mutate(predictions = predictions) |>
  ggplot(aes(x = predictions, y = medv)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1,
              colour = "red", linetype = "dashed") +
  labs(x = "Predicted Median House Value",
       y = "Actual Median House value",
       title = "Linear Regression: Predicted vs. Actual Medv") +
  theme_minimal()

# Visualize the relationship between medv and predictors
B_housing |> select(medv, dis, rm, lstat, tax) |>
  pivot_longer(cols = 2:5, values_to = 'values', names_to = 'predictors') |>
ggplot(aes(x=values, y=medv)) +
  geom_point() + facet_wrap(~predictors, scales = 'free') +
  theme_minimal()


glm(formula, data = my_data, family = 'binomial')


my_cars <- as_tibble(mtcars, rownames = NA)

# convert am to a factor (0=automatic, 1=manual)
my_cars <- my_cars |> mutate(
  am = case_when(
    am == 0 ~ 'Automatic',
    am == 1 ~ 'Manual'
  ),
  am = as.factor(am)
)
my_cars$am

# Split the data
set.seed(222)
index <- createDataPartition(my_cars$am, p = 0.7, list = FALSE)
trainData <- my_cars[index, ]
testData <- my_cars[-index, ]

# Create a logistic regression model using caret
logit_model <- train(
  am ~ mpg + hp + wt,
  data = trainData,
  method = "glm",
  family = "binomial",
  trControl = trainControl(method = "cv", number = 5)
)
print(logit_model)
summary(logit_model$finalModel)

# Make predictions on test data
predictions <- predict(logit_model,
                       newdata = testData, type = "raw")

# Calculate accuracy and create confusion matrix
confMat <- confusionMatrix(predictions, testData$am,
                           positive = 'Manual',
                           mode = 'everything')
print(confMat)

# Get class probabilities for plotting
prob_predictions <- predict(logit_model,
                            newdata = testData,
                            type = "prob")


# Use predicted  probabilities for plotting
results_df <- tibble(
  actual = testData$am,
  prob_man = prob_predictions$Manual,
  prob_auto = prob_predictions$Automatic
)

# Plot probabilities by actual class
results_df |> pivot_longer(cols = -actual,
                           values_to = 'probabilities',
                           names_to = 'transmission' ) |>
ggplot(aes(x = actual, y = probabilities, colour = actual)) +
  geom_jitter(width = 0.2, height = 0,
              size = 3, alpha = 0.7) +
  labs(x = "Actual Transmission Type",
       y = "Predicted Probability",
       title = "Logistic Regression: Predicted Probabilities by Class") +
  theme_bw() +
  scale_color_manual(values = c("Automatic" = "green4", "Manual" = "red")) +
  facet_wrap(~transmission, nrow = 1)


#Create prediction object
prediction_obj <- prediction(
  prob_predictions$Manual,
  testData$am)

#Generate performance measures (TPR and FPR for the ROC curve)
performance_obj <- performance(prediction_obj,
                               measure = "tpr", x.measure = "fpr")

#Extract TPR and FPR from the performance object
roc_data <- tibble(
  FPR = performance_obj@x.values[[1]],
  TPR = performance_obj@y.values[[1]]
)

#Calculate the AUC
auc <- performance(prediction_obj, measure = "auc")
auc_value <- auc@y.values[[1]]
#Plot ROC Curve using ggplot2
ggplot(roc_data, aes(x = FPR, y = TPR))+
  geom_line(colour='green', linewidth=1.5) +
  geom_abline(linetype = 2, color='grey15') + #draws diagonal line
  labs(title="Receiver Operating Characteristic Curve",
       subtitle = paste("Area Under Curve is", round(auc_value,3)),
       x = "False Positive Rate",
       y = "True Positive Rate") +
  theme_bw() +
  annotate("segment", y=0.29, yend=0.485, x=0.5, xend=0.5, colour = 'black', arrow=arrow(angle = 20, length = unit(0.3, "cm")))+
  annotate("text", x=0.5, y=0.25, label="random classifier",colour='black', size=4) +
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5))


