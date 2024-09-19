library(dplyr)
library(stringr)
library("tidyverse")
library(corrplot)
library(datarium)

#------------------------------------------------PREPROCESSING---------------------------------------------

cpu_data = read.csv("Intel_CPUs.csv") 

launch_date_replacements <- c('Q1\'00', 'Q2\'00', 'Q3\'00', 'Q4\'00', 'Q1\'01', 'Q2\'01', 'Q3\'01', 'Q4\'01', 'Q1\'02', 'Q2\'02', 'Q3\'02', 'Q4\'02', 'Q1\'03', 'Q2\'03', 'Q3\'03', 'Q4\'03', 'Q1\'04', 'Q2\'04', 'Q3\'04', 'Q4\'04', 'Q1\'05', 'Q2\'05', 'Q3\'05', 'Q4\'05', 'Q1\'06', 'Q2\'06', 'Q3\'06', 'Q4\'06', 'Q1\'07', 'Q2\'07', 'Q3\'07', 'Q4\'07', 'Q1\'08', 'Q2\'08', 'Q3\'08', 'Q4\'08', 'Q1\'09', 'Q2\'09', 'Q3\'09', 'Q4\'09', 'Q1\'10', 'Q2\'10', 'Q3\'10', 'Q4\'10', 'Q1\'11', 'Q2\'11', 'Q3\'11', 'Q4\'11', 'Q1\'12', 'Q2\'12', 'Q3\'12', 'Q4\'12', 'Q1\'13', 'Q2\'13', 'Q3\'13', 'Q4\'13', 'Q1\'14', 'Q2\'14', 'Q3\'14', 'Q4\'14', 'Q1\'15', 'Q2\'15', 'Q3\'15', 'Q4\'15', 'Q1\'16', 'Q2\'16', 'Q3\'16', 'Q4\'16', 'Q1\'17', 'Q2\'17', 'Q3\'17', 'Q4\'17', 'Q1\'18', 'Q2\'18', 'Q3\'18', 'Q4\'18', 'Q1 \'15', '04\'16', 'Q1\'99', 'Q2\'99')
replacement_values <- c(seq(1, 19, 0.25), 15.75, 17.0, 0.0, 0.25)

# Perform replacement
cpu_data$Launch_Date <- ifelse(cpu_data$Launch_Date %in% launch_date_replacements, 
                               replacement_values[match(cpu_data$Launch_Date, launch_date_replacements)], 
                               cpu_data$Launch_Date)
data_feature <- c('Product_Collection',
                  'Launch_Date', 
                  'Recommended_Customer_Price', 
                  'nb_of_Cores', 'nb_of_Threads', 'Processor_Base_Frequency', 
                  'Cache', 'Bus_Speed', 
                  'Max_Memory_Size', 'Max_Memory_Bandwidth')

cpu_df <- cpu_data[, data_feature, drop = FALSE]

cpu_df <- cpu_df %>%
  mutate(Product_Collection = gsub('.*Core.*', 'Core', Product_Collection),
         Product_Collection = gsub('.*X-series.*', 'X-series', Product_Collection),
         Product_Collection = gsub('.*Celeron.*', 'Celeron', Product_Collection),
         Product_Collection = gsub('.*Pentium.*', 'Pentium', Product_Collection),
         Product_Collection = gsub('.*Quark.*', 'Quark', Product_Collection),
         Product_Collection = gsub('.*Core. [mM].*', 'm', Product_Collection),
         Product_Collection = gsub('.*Atom.*', 'Atom', Product_Collection),
         Product_Collection = gsub('.*Itanium.*', 'Itanium', Product_Collection),
         Product_Collection = gsub('.*Xeon.*', 'Xeon', Product_Collection))



PriceProcessor <- function(x) {
  x <- gsub(",", "", x)
  matches <- regmatches(x, gregexpr("\\$([0-9]+(\\.[0-9]+)?)", x))
  if (any(grepl("-", x))) {
    values <- as.numeric(gsub("\\$", "", unlist(matches)))
    if (length(values) >= 2) {
      ans <- mean(values, na.rm = TRUE)
    } else {
      ans <- NA
    }
  } else if (length(matches[[1]]) > 0) {
    ans <- as.numeric(sub("\\$", "", matches[[1]][1]))
  } else {
    ans <- NA
  }
  return(ans)
}

cpu_df$Recommended_Customer_Price <- sapply(cpu_df$Recommended_Customer_Price, PriceProcessor)
cpu_df$Recommended_Customer_Price <- as.numeric(cpu_df$Recommended_Customer_Price)


ProcessorMapper = function(x) {
  value <- as.double(as.numeric(substr(x, 1, nchar(x) - 4)))
  if (substr(x, nchar(x) - 2, nchar(x) - 2) == "G") {
    value <- value*1000000000
  } else if (substr(x, nchar(x)-2, nchar(x)-2) == "M"){
    value <- value*1000000
  } else if (substr(x, nchar(x)-2, nchar(x)-2) == "K"){
    value <- value*1000
  } else if (substr(x, nchar(x)-2, nchar(x)-2) == "T") {
    value <- value*1000000000000
  }
  
  return(value)
}

Cleanify = function(tag, func, df) {
  clean_df <- df[!is.na(df[[tag]]), , drop = FALSE]
  clean_df <- data.frame(clean_df)
  rownames(clean_df) <- NULL
  clean_df[[tag]] <- lapply(clean_df[[tag]], func)
  return(clean_df)
}


cpu_df <- Cleanify("Processor_Base_Frequency", ProcessorMapper, df=cpu_df)
cpu_df$Processor_Base_Frequency <- as.numeric(cpu_df$Processor_Base_Frequency)

get_numbers <- function(word) {
  if (is.character(word)) {
    return(as.numeric(str_extract(word, "[\\d]*[.]?[\\d]+")))
  } else {
    return(word)
  }
}

CacheMapper = function(x) {
  if (is.numeric(x)){
    return(x)
  } else if (grepl("K", x)){
    fac <- 1000
  } else if (grepl("M", x)){
    fac <- 1000000
  } else if (grepl("G", x)){
    fac <- 1000000000
  } else if (grepl("T", x)){
    fac <- 1000000000000
  } else {
    fac <- 1 
  }
  return(fac*get_numbers(x))
}

cpu_df$Cache <- sapply(cpu_df$Cache, CacheMapper)
cpu_df$Bus_Speed <- sapply(cpu_df$Bus_Speed, CacheMapper)
cpu_df$Launch_Date <- as.numeric(cpu_df$Launch_Date)
cpu_df$Max_Memory_Size <- sapply(cpu_df$Max_Memory_Size, CacheMapper)
cpu_df$Max_Memory_Bandwidth <- gsub("[^0-9.]", "", cpu_df$Max_Memory_Bandwidth)
cpu_df$Max_Memory_Bandwidth <- as.numeric(cpu_df$Max_Memory_Bandwidth)
cpu_df$Product_Collection <- as.factor(cpu_df$Product_Collection)

for (col in 2: ncol(cpu_df)) {
  if (class(cpu_df[[col]]) != "character") {
    cpu_df[[col]] <- ifelse(is.na(cpu_df[[col]]), mean(cpu_df[[col]], na.rm = TRUE), cpu_df[[col]])
  }
}

#integer variables
cpu_df$nb_of_Cores <- round(cpu_df$nb_of_Cores, digits = 0)
cpu_df$nb_of_Threads <- round(cpu_df$nb_of_Threads, digits = 0)

#-------------------------------------------------DESCRIPTIVE STATISTIC-----------------------------------------
#summary
summary(cpu_df)
cor(subset(cpu_df, select = -c(Product_Collection)))
corrplot(cor(subset(cpu_df, select = -c(Product_Collection))) ,
         number.cex = 1, tl.cex = 0.8,
         method = "color",
         addCoef.col = "orange",
         type = "full")

par(mfrow = c(1, 2))
#Diagram of Launch date
hist(cpu_df$Launch_Date, breaks = 50, xlim = c(0,20), ylim = c(0,600), xlab = "", ylab = "Frequency", main = "Histogram of Launch_Date")
boxplot(cpu_df$Launch_Date, main = "Boxplot of Launch_Date")

#Diagram of Recommended customer price
hist(cpu_df$Recommended_Customer_Price, breaks = 50, xlab = "Dollar", ylab = "Frequency", xlim = c(0, 15000), ylim = c(0, 1100), main = "Histogram of \n Recommended_Customer_Price")
boxplot(cpu_df$Recommended_Customer_Price, main = "Boxplot of \n Recommended_Customer_Price")

#Diagram of nb of cores
hist(cpu_df$nb_of_Cores, breaks = 50, xlab = "Core", ylab = "Frequency", xlim = c(0, 80), ylim = c(0, 1400), main = "Histogram of nb_of_Cores")
boxplot(cpu_df$nb_of_Cores, main = "Boxplot of nb_of_Cores")

#Diagram of nb of threads
hist(cpu_df$nb_of_Threads, breaks = 50, xlab = "Thread", ylab = "Frequency", xlim = c(0, 60), ylim = c(0, 1000), main = "Histogram of nb_of_Threads")
boxplot(cpu_df$nb_of_Threads, main = "Boxplot of nb_of_Threads")

#Diagram of Processor base frequency
hist(cpu_df$Processor_Base_Frequency, breaks = 50, xlim = c(0, 5000000000), ylim = c(0, 150), xlab = "Hz", ylab = "Frequency", main = "Histogram of \n Processor_Base_Frequency")
boxplot(cpu_df$Processor_Base_Frequency, main = "Boxplot of \n Processor_Base_Frequency")

#Diagram of Cache
hist(cpu_df$Cache, breaks = 50, xlab = "Byte", ylab = "Frequency", ylim = c(0, 600), main = "Histogram of Cache")
boxplot(cpu_df$Cache, main = "Boxplot of Cache")

#Diagram of Bus speed
hist(cpu_df$Bus_Speed, breaks = 50, xlim = c(0, 10000000000), ylim = c(0, 1000), xlab = "Hz/s", ylab = "Frequency", main = "Histogram of Bus_Speed")
boxplot(cpu_df$Bus_Speed, main = "Boxplot of Bus_Speed")

#Diagram of Max memory size
hist(cpu_df$Max_Memory_Size, breaks = 50, ylim = c (0, 1000), xlab = "Byte", ylab = "Frequency", main = "Histogram of \n Max_Memory_Size")
boxplot(cpu_df$Max_Memory_Size, main = "Boxplot of \n Max_Memory_Size")

#Diagram of Max memory bandwidth
hist(cpu_df$Max_Memory_Bandwidth, breaks = 50, ylim = c (0, 1400), xlab = "GB", ylab = "Frequency", main = "Histogram of \n Max_Memory_Bandwidth")
boxplot(cpu_df$Max_Memory_Bandwidth, main = "Boxplot of \n Max_Memory_Bandwidth")

##Inferential statistic
##Assumptions
fitted(formula)
resid(formula)
vephandu<-par(mfrow=c(2,2))
plot(formula)
#predict: 
set.seed(50)
# use 80% of dataset as training and 20% as testing
sample <- sample(c(TRUE, FALSE), nrow(new_cpu_df), replace = TRUE, prob = c(0.8, 0.2))
train <- new_cpu_df[sample, ]
test <- new_cpu_df[!sample, ]

formula1 <- as.formula("Cache ~ Launch_Date + Recommended_Customer_Price + nb_of_Cores + nb_of_Threads + Processor_Base_Frequency + Bus_Speed + Max_Memory_Size + Max_Memory_Bandwidth")

formula_model1 <- lm(formula = formula1, data = train)

predicted_values <- predict(formula_model1, newdata = train)

comparison_df <- data.frame(Predicted = predicted_values, Original = train)

stepwise_model1 <- step(formula_model1, direction = "both")

summary(stepwise_model1)

formula2 <- as.formula("Cache ~ Launch_Date + Recommended_Customer_Price + nb_of_Cores + nb_of_Threads + Processor_Base_Frequency + Bus_Speed + Max_Memory_Size")

formula_model2 <- lm(formula = formula2, data = train)

predicted_values2 <- predict(formula_model2, newdata = train)

comparison_df2 <- data.frame(Predicted = predicted_values2, Original = train)

stepwise_model <- step(formula_model2, direction = "both")

summary(comparison_df)

summary(formula)

coef(formula)
test_variables <- test[, c("Launch_Date", "Recommended_Customer_Price", "nb_of_Cores", "nb_of_Threads", "Processor_Base_Frequency", "Bus_Speed", "Max_Memory_Size", "Max_Memory_Bandwidth")]
pred <- predict(formula, newdata = test_variables)

compare <- cbind(test$Cache,pred)
colnames(compare) <- c("test_set","prediction")
head(compare,10)

SSE <- sum((test$Cache - pred)^2)
SST <- sum((test$Cache - mean(test$Cache))^2)
cat("The accuracy of the model on test set: " , round((1 - SSE / SST )* 100 ,2) , "%" )

#--------------------------------------------------POLYNOMINALMODEL----------------------------------------------
predictor_variables <- c("Launch_Date", "Recommended_Customer_Price", "nb_of_Cores", 
                         "nb_of_Threads", "Processor_Base_Frequency", "Bus_Speed", 
                         "Max_Memory_Size", "Max_Memory_Bandwidth")

# Loop through polynomial degrees
for (degree in 1:10) {
  # Create a list of polynomial terms for each predictor variable
  poly_terms <- lapply(predictor_variables, function(var) {
    paste("poly(", var, ",", degree, ")", collapse = "")
  })
  
  # Combine the polynomial terms for all predictor variables
  formula <- as.formula(paste("Cache ~", paste(poly_terms, collapse = "+")))
  
  # Create the polynomial regression model
  poly_model <- lm(formula, data = cpu_df)
  
  # Print the summary
  cat("\nSummary for Polynomial Degree:", degree, "\n")
  print(summary(poly_model))
}

sample <- sample(c(T, F), nrow(cpu_df), replace=T, prob=c(0.8,0.2))
train <- cpu_df[sample, ]
test <- cpu_df[!sample, ]

degree <- 1
linear_formula <- as.formula(paste("Cache ~ poly(Launch_Date, ", degree, ") + poly(Recommended_Customer_Price, ", degree, ") + poly(nb_of_Cores, ", degree, ") + poly(nb_of_Threads, ", degree, ") + poly(Processor_Base_Frequency, ", degree, ") + poly(Bus_Speed, ", degree, ") + poly(Max_Memory_Size, ", degree, ") + poly(Max_Memory_Bandwidth, ", degree, ")", sep = ""))
linear <- lm(formula = linear_formula, data = train)

degree <- 10
poly_formula <- as.formula(paste("Cache ~ poly(Launch_Date, ", degree, ") + poly(Recommended_Customer_Price, ", degree, ") + poly(nb_of_Cores, ", degree, ") + poly(nb_of_Threads, ", degree, ") + poly(Processor_Base_Frequency, ", degree, ") + poly(Bus_Speed, ", degree, ") + poly(Max_Memory_Size, ", degree, ") + poly(Max_Memory_Bandwidth, ", degree, ")", sep = ""))
poly10 <- lm(formula = poly_formula, data = train)


# Apply the model to the test data
test_predictors <- subset(test, select = -c(Product_Collection))
predictions_poly10 <- predict(poly10, newdata = test_predictors)
predictions_linear <- predict(linear, newdata = test_predictors)
error_poly10 <- abs(predictions_poly10 - test$Cache)
error_linear <- abs(predictions_linear - test$Cache)
model_type = ifelse(error_poly10 < error_linear, "Poly10", "Linear")

# Create a data frame with the requested information
result_df <- data.frame(testing_Cache = test$Cache, predictedbyPoly10 = predictions_poly10, predictedbyLinear = predictions_linear,
                        DifferencePoly10 = error_poly10, DifferenceLinear = error_linear, model_type)
print(result_df)

# Create a table of counts for each 'model_type'
model_type_table <- table(model_type)

# Calculate the percentages
percentage_poly10 <- (model_type_table["Poly10"] / sum(model_type_table)) * 100
percentage_linear <- (model_type_table["Linear"] / sum(model_type_table)) * 100

# Print the results
cat("\nPercentage of observations classified as Poly10:", percentage_poly10, "%",
    "\nPercentage of observations classified as Linear:", percentage_linear, "%\n")
