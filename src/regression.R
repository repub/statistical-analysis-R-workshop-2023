#' ---
#' title: "Regression"
#' author:
#'   - Tyler B. Garner, tbg5023@psu.edu
#'   - Jennifer Valcin, jpv5319@psu.edu
#' output:
#'   html_document:
#'     toc: true
#'     toc_float: true
#'     collapsed: false
#'     theme: united
#'     highlight: tango
#' ---

#' ## Introduction
#' 
#' Regression analysis is one of the most fundamental and widely used statistical methods for modeling the relationship between variables. It involves identifying patterns and strengths of the relationship between one or more independent variables and a dependent variable. Regression models are extensively used in various fields, including finance, economics, social sciences, and engineering, to name a few. In this tutorial, we will start by introducing the basic concepts of regression analysis and gradually move on to more advanced techniques, such as multiple regression and logistic regression. By the end of this tutorial, you will have a solid understanding of the various regression methods in `R` and how to apply them to real-world problems.


#+ load-data
adm_df <- readRDS("data/interim/adm_df.RDS")

head(adm_df)

# Regression vs ANOVA vs ANCOVA ---------

#' ## Linear regression
#' 
#' Linear regression is a statistical method used to model the relationship between a dependent variable and one or more independent variables with the goal of identifying a linear relationship that best describes the data. Linear regression makes several assumptions, including linearity, independence, normality, and equal variance of residuals. Its advantages include simplicity, interpretability, and its ability to model the relationship between variables. However, linear regression has some disadvantages, such as its inability to model nonlinear relationships and the risk of overfitting if the model is too complex or has too many predictors.
#' 
#' ### Simple linear regression
#' 
#' Simple linear regression is a type of linear regression where there is only one independent variable that is used to predict the value of the dependent variable. It is a commonly used technique in statistical analysis and can be used to model and predict the relationship between variables in a simple and interpretable way.
#' 
#' Here, we will fit a simple linear regression model predicting GRE scores from cumulative GPA. We will supply the `lm()` function with two arguments, first a model formula and second our dataset.

#+ simple
adm_fit <- lm(GRE ~ CGPA, adm_df)

summary(adm_fit)

#' #### Model diagnostics

#+ simple-diag
par(mfrow = c(2, 2))
plot(adm_fit)
par(mfrow = c(1, 1))

#' ### Multiple linear regression
#'
#' Multiple linear regression is a type of linear regression where there are two or more independent variables that are used to predict the value of the dependent variable. As multiple linear regression can be used to model the relationship between multiple predictors and the dependent variable, enabling more accurate predictions and more comprehensive analysis of the data, including potential interactions between variables.
#'
#' Here, we will fit a linear regression model predicting GRE scores from cumulative GPA and TOEFL scores using the same functions and similar format as simple linear regression above.

#+ multiple
adm_fit2 <- lm(GRE ~ CGPA + TOEFL, data = adm_df)

summary(adm_fit2)

#' #### Model diagnostics

#+ multiple-diag
par(mfrow = c(2, 2))
plot(adm_fit2)
par(mfrow = c(1, 1))


#' #### Multiple regression with categorical predictors
#' 
#' Linear regression with all categorical variables is equivalent to ANOVA models, while regression with a mix of categorical and continuous variables is equivalent to ANCOVA models.

adm_fit3 <- lm(GRE ~ CGPA + TOEFL + Discipline, data = adm_df)
summary(adm_fit3)

# Diagnosing the model --------------------
plot(adm_fit3)

# ANCOVA
anova(adm_fit3)



# Create dummy variables
#
# Fist make a column with the value of '1' for every observation.

adm_df$Dummies <- 1


# Contrasts? ------

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


