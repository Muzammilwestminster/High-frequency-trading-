## Import packages and data

{r}
# package to perform data manipulation  and visualization
library(tidyverse)
# package to compute cross - validation methods
library(caret)
# package to calculate AUC-ROC
library(pROC)
# package to perform high-level data visualization
library(lattice)
# package to support forecasting data
library ( fpp2 )

# Set random seed
set.seed(12345)

# Import data
HBAT_data <- read.csv ("HBAT.csv", header = TRUE )

#Remove id column
HBAT_data <- HBAT_data %>% select(-id)

# Rename columns
names(HBAT_data) <- c("Customer.Type","Industry.Type", "Firm.Size", "Region", 
                      "Distribution.System", "Product.Quality",
                      "Website.Activities", "Technical.Support",
                      "Complaint.Resolution", "Advertising", "Product.Line",
                      "Salesforce.Image", "Competitive.Pricing",
                      "Warranty.Claims", "New.Products", "Order.Bill",
                      "Price.Flexibility", "Delivery.Speed", "Satisfaction",
                      "Recommendation", "Future.Purchase", "Current.Purchase",
                      "Partnership")

# Convert some columns to factors
cols <- c("Customer.Type","Industry.Type", "Firm.Size", "Region", 
          "Distribution.System", "Partnership")
HBAT_data[cols] <- lapply(HBAT_data[cols], factor)

## Part 1. Analyse HBAT's customer profile using descriptive statistics and exploratory data analysis

### 1.1. Descriptive statistics

**Customers' profile**

Style 1. Simple bar chart
```{r}
# Calculate the category count
category_counts <- HBAT_data %>% group_by(Distribution.System) %>% 
                  summarise ( Count = length(Distribution.System))

# Create a bar chart
barplot(
  category_counts$Count,
  names.arg = c("Magazine", "Newsprint"), # Add category names as labels
  col = c("blue", "red"), # Set bar color
  main = "Chart About Customers' Distribution Systems", # Add a title
  xlab = "Distribution Systems", # Label for x-axis
  ylab = "Counts" # Label for y-axis
)
```

Style 2. Stacked bar chart
```{r}
# Create the barplot
barplot_data <- barplot(
  table(HBAT_data$Customer.Type, HBAT_data$Region),
  beside = TRUE,
  names.arg = c("UK", "Outside UK"),
  col = c("blue", "green", "red"), # Adding colors for clarity
  main = "Customer Type by Region", # Optional: Add a title
  xlab = "Region", # Optional: Add x-axis label
  ylab = "Number of Customers", # Optional: Add y-axis label
  ylim = c(0, max(table(HBAT_data$Customer.Type, HBAT_data$Region)) + 10) 
  # Add space for legend
)

# Add extra space to right of plot area; change clipping to figure
par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)

# Add legend outside the plot area
legend(
  "topright",
  inset=c(-0.05,-0.1), # Move legend slightly below the plotting area
  legend = c("<1 year", ">=1 & <5 years", ">5 years"),
  fill = c("blue", "green", "red"), # Match the colors used in the barplot
  horiz = FALSE # Arrange the legend horizontally
)
```

Style 3. Pie chart
```{r}
# Calculate the percentage of each category
category_counts <- HBAT_data %>% group_by(Firm.Size) %>% 
                  summarise ( Count = length(Firm.Size))
category_percentages <- 
  round(100 * category_counts$Count / sum(category_counts$Count), 1)

# Combine category names with percentages for labeling
category_labels <- paste(c("Small firm", "Large firm"), 
                         "(", category_percentages, "%)", sep = "")

# Plot the pie chart
pie(
  category_counts$Count, 
  labels = category_labels, 
  main = "Pie Chart of Firm Size",
  col = rainbow(length(category_labels)) # Optional: Adds colors
)
```
**Hypothesis testing**

Notice: We need to test __3 different hypotheses__ here. Below I only provide a demo code, so you can write on your own code of other hypotheses. 

__Sample__: Testing differences in satisfaction levels across groups of different firm size

-A graph -
```{r}
satisfaction_by_firm_size <- HBAT_data %>% group_by(Firm.Size) %>% 
                  summarise (Mean = mean(Satisfaction))

small_firm <- HBAT_data[HBAT_data$Firm.Size == 0,]$Satisfaction
large_firm <- HBAT_data[HBAT_data$Firm.Size == 1,]$Satisfaction

# Create a bar chart
barplot(
  satisfaction_by_firm_size$Mean,
  names.arg = c("Small firm", "Big firm"), # Add category names as labels
  col = c("blue", "red"), # Set bar color
  main = "Chart About Satisfaction by Firm Size", # Add a title
  xlab = "Types of Firm Size", # Label for x-axis
  ylab = "Avg. Satisfaction" # Label for y-axis
)
```

-A test -
```{r}
t.test (small_firm , large_firm , conf.level = 0.95)
```
## Part 2. Customer satisfaction and other purchase outcomes between the two channels in the distribution system.

Notice: We need to test __1 hypothesis__ here. Below I only write a demo code for satisfaction. You can apply for other purchase outcomes.

- A graph -
```{r}
satisfaction_by_industry_type <- HBAT_data %>% group_by(Industry.Type) %>% 
                  summarise (Mean = mean(Satisfaction))

Magazine <- HBAT_data[HBAT_data$Industry.Type == 0,]$Satisfaction
Newspaper <- HBAT_data[HBAT_data$Industry.Type == 1,]$Satisfaction

# Create a bar chart
barplot(
  satisfaction_by_industry_type$Mean,
  names.arg = c("Magazine", "Newspaper"), # Add category names as labels
  col = c("blue", "red"), # Set bar color
  main = "Chart About Satisfaction by Industry", # Add a title
  xlab = "Types of Industry", # Label for x-axis
  ylab = "Avg. Satisfaction" # Label for y-axis
)
```
- A test -
```{r}
t.test (Magazine , Newspaper , conf.level = 0.95)
```
## Part 3. Relationships with its customers over time.

Notice: The variables related to future outcomes are X20 and X21. Additionally, variable X1 provides insights into how new and returning customers feel. These variables offer valuable information over time, provided they are utilized correctly. We need to test __2 hypothesis__ here.

### Testing for satisfaction between new and old customers
You can create your own hypotheses by change satisfaction by different purchase outcome
```{r}
anova <- aov(Satisfaction ~ Customer.Type, data = HBAT_data)
summary(anova)
TukeyHSD (anova)
```
### Testing for whether current satisfaction affects future purchases
You can create your own hypotheses by change satisfaction by different purchase outcome
```{r}
model <- lm(Future.Purchase ~ Satisfaction, data = HBAT_data)
summary(model)
```
## Part 4. Systematically group and analyse perception variables to clearly interpret key factors that help HBAT understand customer views

__Notice__: Try to modify the code below to find out the suitable number of factors using scree plot. Then, critically discuss your factors.

#Principal Component Analysis
data.pca <- prcomp(HBAT_data[,6:18], center = TRUE, scale = TRUE)
summary(data.pca)

# Extract the eigenvalues from the PCA object
eigenvalues <- data.pca$sdev^2

# Create a scree plot
plot(eigenvalues, type = "b",
     xlab = "Principal Component",
     ylab = "Eigenvalue",
     main = "Scree plot for PCA")

# Add a line at y = 1 to indicate the elbow
abline(v = 4, col = "red")

#Transformed data
key.factors <- data.pca$x[,1:4]
#Merge to the data
HBAT_data <- merge(HBAT_data,key.factors)

The code below is assuming that we have 4 factors.

# Loadings
data.pca$rotation[,1:4]

load    <- data.pca$rotation
sorted.loadings <- load[order(load[, 1]), 1]
myTitle <- "Loadings Plot for PC1" 
myXlab  <- "Variable Loadings"
dotplot(sorted.loadings, main=myTitle, xlab=myXlab, cex=1.5, col="red")

sorted.loadings <- load[order(load[, 2]), 2]
myTitle <- "Loadings Plot for PC2" 
myXlab  <- "Variable Loadings"
dotplot(sorted.loadings, main=myTitle, xlab=myXlab, cex=1.5, col="red")

sorted.loadings <- load[order(load[, 3]), 3]
myTitle <- "Loadings Plot for PC3" 
myXlab  <- "Variable Loadings"
dotplot(sorted.loadings, main=myTitle, xlab=myXlab, cex=1.5, col="red")

sorted.loadings <- load[order(load[, 4]), 4]
myTitle <- "Loadings Plot for PC4" 
myXlab  <- "Variable Loadings"
dotplot(sorted.loadings, main=myTitle, xlab=myXlab, cex=1.5, col="red")

Part 5
# Function to compute total within-cluster sum of squares
wss <- function(k) {
  kmeans(HBAT_data[, 6:18], k, nstart = 13)$tot.withinss
}

# Compute and plot WSS for k = 1 to k = 13
k.values <- 1:13
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of Clusters K",
     ylab="Total Within-Clusters Sum of Squares")

# Run K-means with 3 clusters
set.seed(1234)  # Ensure reproducibility
k_clusters <- kmeans(HBAT_data[,6:18], centers = 3, nstart = 25)

# Assign cluster labels
HBAT_data$Cluster <- factor(k_clusters$cluster)

# Check differences in Recommendation among clusters
anova_result <- aov(Recommendation ~ Cluster, data = HBAT_data)
summary(anova_result)
TukeyHSD(anova_result)

Running K with 4 Clusters


# Function to compute total within-cluster sum of squares
wss <- function(k) {
  kmeans(HBAT_data[, 6:18], k, nstart = 13)$tot.withinss
}

# Compute and plot WSS for k = 1 to k = 13
k.values <- 1:13
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of Clusters K",
     ylab="Total Within-Clusters Sum of Squares")

# Run K-means with 4 clusters (Updated)
set.seed(1234)  # Ensure reproducibility
k_clusters <- kmeans(HBAT_data[,6:18], centers = 8, nstart = 25)  # Updated centers = 8

# Assign cluster labels
HBAT_data$Cluster <- factor(k_clusters$cluster)

# Check differences in Recommendation among clusters
anova_result <- aov(Recommendation ~ Cluster, data = HBAT_data)
summary(anova_result)
TukeyHSD(anova_result)


### Part 6: Identify the perceptions of HBAT that best distinguish customers based on geographic regions, firm sizes, and partnerships

# Boxplot for Competitive Pricing by Geographic Region
boxplot(Competitive.Pricing ~ Region, 
        data = HBAT_data,
        main = "Boxplot of Competitive Price by Region",
        xlab = "Region",
        ylab = "Price")

# Boxplot for Competitive Pricing by Firm Size
boxplot(Competitive.Pricing ~ Firm.Size, 
        data = HBAT_data,
        main = "Boxplot of Competitive Price by Firm Size",
        xlab = "Firm Size",
        ylab = "Price")

# Boxplot for Competitive Pricing by Partnership
boxplot(Competitive.Pricing ~ Partnership,
        data = HBAT_data,
        main = "Boxplot of Competitive Price by Partnership",
        xlab = "Partnership Type",
        ylab = "Price",
        names = c("No partnership", "Partnership"))

# Create a list of 80% of the rows in the original dataset we can use for training
train_validation.Index <- createDataPartition(HBAT_data$Partnership, p = 0.80, list = FALSE)

# Select 20% of the data for test set
testset <- HBAT_data[-train_validation.Index,]

# Use the remaining 80% of data for training and testing the models
train_validation.set <- HBAT_data[train_validation.Index,]

# Set up cross-validation
set_up_train_cv <- trainControl(method = "cv", 
             number = 5,   
             classProbs = TRUE,                # Needed for AUC calculation
             summaryFunction = twoClassSummary # Sets up to compute AUC
)

# Logistic regression model incorporating geographic region and firm size
model_distinction <- train(
  form = make.names(Partnership) ~ Product.Quality + 
                      Website.Activities + Technical.Support + 
                      Complaint.Resolution + Advertising + Product.Line + 
                      Salesforce.Image + Competitive.Pricing + 
                      Warranty.Claims + New.Products + 
                      Order.Bill + Price.Flexibility + 
                      Delivery.Speed + Region + 
                      Firm.Size,
  data = train_validation.set,
  trControl = set_up_train_cv,
  method = "glm",
  family = "binomial",
  metric = "ROC"
)

# Print out AUC-ROC, Sensitivity, and Specificity of model (Cross-validation)
model_distinction

# Print out Confusion Matrix
predictions <- make.names(predict(model_distinction, testset))
confusionMatrix(factor(predictions), factor(make.names(testset$Partnership)))

# Summary of the Logistic model
summary(model_distinction)

### Part 7. Predict customer satisfaction based on their perceptions of HBAT’s performance

Train and select model using cross-validation
```{r}
# Create a list of 80% of the rows in the original dataset we can use for training
train_validation.Index <- createDataPartition(HBAT_data$Satisfaction, p=0.80, list=FALSE)
# Select 20% of the data for test set
testset <- HBAT_data[-train_validation.Index,]
# Use the remaining 80% of data to training and testing the models
train_validation.set <- HBAT_data[train_validation.Index,]

# Set up cross-validation
set_up_train_cv <- trainControl(method = "cv", 
             number = 5,   
)

# Train linear model 1 with the results using cross-validation:
model_1_cv <- train(
  form = Satisfaction ~ Product.Quality + 
                      Website.Activities + Technical.Support + 
                      Complaint.Resolution + Advertising + Product.Line + 
                      Salesforce.Image + Competitive.Pricing + Warranty.Claims + 
                      New.Products + Order.Bill + Price.Flexibility + 
                      Delivery.Speed,
  data = train_validation.set,
  trControl = set_up_train_cv,
  method = "lm"
)
model_1_cv

# Train linear model 2 with the results using cross-validation:
model_2_cv <- train(
  form = Satisfaction ~ PC1 + PC2 + PC3 + PC4,
  data = train_validation.set,
  trControl = set_up_train_cv,
  method = "lm"
)
model_2_cv
```

Re-evaluate the accuracy of models using testset
```{R}
# Evaluate model 1 on test set
model_1_test <- predict(model_1_cv, testset)
accuracy(model_1_test, testset$Satisfaction)

# Evaluate model 2 on test set
model_2_test <- predict(model_2_cv, testset)
accuracy(model_2_test, testset$Satisfaction)

# Create a dataframe for model metrics
model_metrics <- data.frame(
  Model = c("Model 1", "Model 2"),
  RMSE = c(RMSE(model_1_test, testset$Satisfaction), RMSE(model_2_test, testset$Satisfaction)),
  R_Squared = c(R2(model_1_test, testset$Satisfaction), R2(model_2_test, testset$Satisfaction))
)

# Bar Plot for Model Comparison
ggplot(model_metrics, aes(x = Model)) +
  geom_bar(aes(y = RMSE, fill = "RMSE"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = R_Squared * 10, fill = "R²"), stat = "identity", position = "dodge") +
  scale_y_continuous(sec.axis = sec_axis(~./10, name = "R²")) +
  labs(title = "Model Performance Metrics", fill = "Metric") +
  theme_minimal()

# Scatter Plot for Predicted vs Actual (Model 1)
ggplot(data.frame(Actual = testset$Satisfaction, Predicted = model_1_test), aes(x = Actual, y = Predicted)) +
  geom_point() + geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Model 1: Predicted vs Actual Satisfaction", x = "Actual", y = "Predicted") +
  theme_minimal()

# Scatter Plot for Predicted vs Actual (Model 2)
ggplot(data.frame(Actual = testset$Satisfaction, Predicted = model_2_test), aes(x = Actual, y = Predicted)) +
  geom_point() + geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Model 2: Predicted vs Actual Satisfaction", x = "Actual", y = "Predicted") +
  theme_minimal()