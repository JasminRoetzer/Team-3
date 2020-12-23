
###################################################
### Preparation of the Environment ####

# Clear environment
remove(list = ls())

# Create list with needed libraries
pkgs <- c("readr", "fastDummies")

# Load each listed library and check if it is installed and install if necessary
for (pkg in pkgs) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}


###################################################
### Function Definition ####

#' Title Fast creation of normalized variables
#' Quickly create normalized columns from numeric type columns in the input data. This function is useful for statistical analysis when you want normalized columns rather than the actual columns.
#'
#' @param .data An object with the data set you want to make normalized columns from.
#' @param norm_values Dataframe of column names, means, and standard deviations that is used to create corresponding normalized variables from.
#'
#' @return A data.frame (or tibble or data.table, depending on input data type) with same number of rows an dcolumns as the inputted data, only with normalized columns for the variables indicated in the norm_values argument.
#' @export
#'
#' @examples
norm_cols <- function (.data, norm_values = NULL) {
  for (i in 1:nrow(norm_values)  ) {
    .data[[norm_values$name[i]]] <- (.data[[norm_values$name[i]]] - norm_values$mean[i]) / norm_values$sd[i]
  }
  return (.data)
}


#' Title Creation of a Dataframe including the Information to Standardize Variables
#' This function is meant to be used in combination with the function norm_cols
#'
#' @param .data A data set including the variables you want to get the means and standard deviations from.
#' @param select_columns A vector with a list of variable names for which you want to get the means and standard deviations from.
#'
#' @return A data.frame (or tibble or data.table, depending on input data type) including the names, means, and standard deviations of the variables included in the select_columns argument.
#' @export
#'
#' @examples
get.norm_values <- function (.data, select_columns = NULL) {
  result <- NULL
  for (col_name in select_columns) {
    mean <- mean(.data[[col_name]], na.rm = TRUE)
    sd <- sd(.data[[col_name]], na.rm = TRUE)
    result <- rbind (result, c(mean, sd))
  }
  result <- as.data.frame(result, stringsAsFactors = FALSE)
  result <- data.frame (select_columns, result, stringsAsFactors = FALSE)
  names(result) <- c("name", "mean", "sd")
  return (result)
}



###################################################
### Data Import ####

# Reading the data file
house_pricing <- read_csv("https://raw.githubusercontent.com/opencampus-sh/sose20-datascience/master/house_pricing_test.csv")


###################################################
### Data Preparation ####

# Recoding of the variables into one-hot encoded (dummy) variables
dummy_list <- c("view", "condition")
house_pricing_dummy = dummy_cols(house_pricing, dummy_list)

# Definition of lists for each one-hot encoded variable (just to make the handling easier)
condition_dummies = c('condition_1', 'condition_2', 'condition_3', 'condition_4', 'condition_5')
view_dummies = c('view_0', 'view_1', 'view_2', 'view_3','view_4')


# Standardization of all variables (features and label)
norm_list <- c("price", "sqft_lot", "bathrooms", "grade", "waterfront", view_dummies, condition_dummies) # list of all relevant variables
norm_values_list <- get.norm_values(house_pricing_dummy, norm_list)    # Calculation of the means and standard deviations
house_pricing_norm <- norm_cols(house_pricing_dummy, norm_values_list) # Standardization of the variables


###################################################
### Selection of the Feature Variables and the Label Variable ####

# Selection of the features (the independent variables used to predict the dependent)
features <- c('sqft_lot', 'waterfront', 'grade', 'bathrooms', view_dummies, condition_dummies)
# Selection of the label (the dependent variable)
label <- 'price'


###################################################
### Selection of Training and Validation data ####

# Setting the random counter to a fixed value, so the random initialization stays the same (the random split is always the same)
set.seed(1)
# Generating the random indices for the training data set
train_ind <- sample(seq_len(nrow(house_pricing_norm)), size = floor(0.66 * nrow(house_pricing_norm)))

# Splitting the data into training and validation data and selecting the feature variables as a separate data frame
train_dataset = house_pricing_norm[train_ind, features]
test_dataset = house_pricing_norm[-train_ind, features]

# Splitting the data into training and validation data and selecting the label variable as a separate vector
train_labels = house_pricing_norm[train_ind, label]
test_labels = house_pricing_norm[-train_ind, label]


