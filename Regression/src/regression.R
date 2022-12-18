# Regression Methods -------------------------------------------------------------------------------
#
# @author Tyler B. Garner tbgarner5023@gmail.com
# @author Jennifer Valcin jpv5319@psu.edu

library(tidyverse)

adm_df <- read_csv("data/raw/adm_data.csv")

# Re-code variables as factors
adm_df$Research <- factor(adm_df$Research)
adm_df$Discipline <- factor(adm_df$Discipline)
adm_df$Admit <- factor(adm_df$Admit)

head(adm_df)


# Linear Regression ------
#'
#' Fit a linear regression model predicting GRE scores from cumulative GPA.
#' 
#' * `lm()` fits a linear model and needs two arguments, a model `formula` (1st) and `data` (2nd).
#' * `summary()` prints summary statistics from a fitted model.

adm_fit <- lm(GRE ~ CGPA, adm_df)
summary(adm_fit)


# * Two Continuous Variables -----
#'
#' Fit a linear regression model predicting GRE scores from cumulative GPA and TOEFL scores.

adm_fit2 <- lm(GRE ~ CGPA + TOEFL, data = adm_df)
summary(adm_fit2)


# * Adding a Factor -----
#'
#' Fit a linear regression model predicting GRE scores from cumulative GPA, TOEFL scores, and
#' field of discipline.

adm_fit3 <- lm(GRE ~ CGPA + TOEFL + Discipline, data = adm_df)
summary(adm_fit3)


# Create dummy variables
#
# Fist make a column with the value of '1' for every observation.

adm_df$Dummies <- 1


# Use the pivot_wider() function to create the dummy variables for Discipline using the Dummies
# column we made above.

adm_df <- adm_df %>%
  pivot_wider(names_from = Discipline,
              values_from = Dummies,
              values_fill = 0) %>%
  select(!c(`Humanities`))


adm_fit4 <- lm(GRE ~ CGPA + TOEFL + `Natural Science` + `Social Science` +
                 `Formal Science` + `Applied Science`, data = adm_df)

summary(adm_fit4)


# Logistic Regression -----
#'
#' Recode Admit to 0 for Rejected and 1 for Accepted
#' 
#' * `mutate()` adds a new column named `Admit_bin`.
#' * `recode()` will assign a value of 1 for Accepted applicants and 0 for rejected applicants,
#'   which will be convered to the numeric type by `as.numeric()`.

adm_df <- adm_df %>%
  mutate(Admit_bin = as.numeric(recode(Admit,
                                       'Accepted' = 1,
                                       'Rejected' = 0)))


#' Fit a logistic regression model predicting admittance from all other variables
#' 
#' * `glm()` fits a generalized linear model (GLM) and requires two arguments, a model `formula`
#'   (1st) and `data` (2nd). The `family` argument can be used to change the type of GLM model, in
#'   this case to a binomial (binary) logistic model.
#' * `summary()` can also be used to print summary results from a GLM model.

adm_fit <- glm(Admit_bin ~ . - Admit - UniqueID, data = adm_df, family = 'binomial')
summary(adm_fit)


