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


#' ## Linear regression
#' 
#' Linear regression is a statistical method used to model the relationship between a dependent variable and one or more independent variables with the goal of identifying a linear relationship that best describes the data. Linear regression makes several assumptions, including linearity, independence, normality, and equal variance of residuals. Its advantages include simplicity, interpretability, and its ability to model the relationship between variables. However, linear regression has some disadvantages, such as its inability to model nonlinear relationships and the risk of overfitting if the model is too complex or has too many predictors.
#' 
#' Linear regression is closely related to ANOVA (analysis of variance) and ANCOVA (analysis of covariance) in that they all use similar statistical techniques to model the relationship between variables. ANOVA is a type of linear regression that is used when the dependent variable is categorical, while ANCOVA is a type of linear regression that adjusts for the effects of one or more continuous variables, known as covariates. In both ANOVA and ANCOVA, the aim is to identify the sources of variability in the data and to determine the significance of each predictor variable in explaining the variation in the dependent variable.
#' 
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
#' 
#' Diagnostic plots are a visual tool used to assess the validity of a linear regression model and identify any potential problems or violations of assumptions. There are several types of diagnostic plots, including:
#'  
#' - **Residuals vs. fitted** - used to check for linearity and homoscedasticity, as well as the presence of outliers. A pattern in the plot may indicate that the relationship between the dependent and independent variables is not linear or that the variance of the errors is not constant across the range of the data.
#' - **Q-Q** - used to check for normality of the residuals. A straight line in the plot indicates that the residuals are normally distributed, while deviations from the line indicate non-normality.
#' - **Scale-location** - used to check for homoscedasticity. A pattern in the plot may indicate that the variance of the residuals is not constant across the range of the data.
#' - **Residuals vs. leverage** - used to identify influential observations, which can have a large impact on the regression line. Observations with high leverage and high residuals are known as outliers and may need to be removed or further investigated.
#' 
#' When making inferences from linear models it is important to check the fit of the model, as violations of the model assumptions can lead to misleading results. The following code chunk creates a 2 x 2 graphical space to plot all four plots for our fitted model.

#+ multiple-diag
par(mfrow = c(2, 2))
plot(adm_fit2)
par(mfrow = c(1, 1))

#' The residuals vs. fitted shows random variation along the horizontal 0 line, the points of the Q-Q plot closely follow the theoretical line, the points of the scale-location plot also are randomly distributed, and there are no significant points in the residuals vs leverage plot, although one observation (53) may be a slight outlier. Together, the diagnostic plots suggest a good model fit.


#' #### Multiple regression with categorical predictors
#' 
#' Linear regression with all categorical variables is equivalent to ANOVA models, while regression with a mix of categorical and continuous variables is equivalent to ANCOVA models.

adm_fit3 <- lm(GRE ~ CGPA + TOEFL + Discipline, data = adm_df)
summary(adm_fit3)

plot(adm_fit3)

# ANCOVA
anova(adm_fit3)


#' ## Generalized linear models
#' 
#' Generalized linear models (GLMs) are a flexible class of models that extend the linear regression framework to a wide range of response variables, including categorical, count, and continuous data. They allow for the modeling of complex relationships between variables by using a link function to transform the response variable into a linear combination of the predictors. Generalized linear models are commonly used in various fields, including biology, economics, and psychology, to analyze data that does not meet the assumptions of traditional linear regression.
#' 
#' 
#' ## Logistic regression
#' 
#' Logistic regression is a type of GLM used to model the relationship between a categorical dependent variable and one or more independent variables. Generally, there are three types of logistic regression models:
#' 
#' - **binary** - models the probability of a binary outcome, such as yes/no or success/failure.
#' - **multinomial** - models the relationship between an unordered (nominal) categorical dependent variable.
#' - **ordinal** - models the relationship between an ordered categorical dependent variable.
#' 
#' Here, we will fit a binary logistic GLM that predicts *Admit* from all available variables. To fit a binary logistic regression model we can use the `glm()` function, which can fit a series of GLMs. Like other modeling functions in `R`, we first define a model formula and provide our dataset to the `data` argument. For the `glm()` function in particular, we also provide a string to the `family` argument to indicate the type of GLM we wish to fit. You can find the list of options for the `family` argument by typing `?family` in the `R` console.
#' 
#' While the `glm()` function will accept a binary factor variable as the response, it is good habit to manually define the response variable. What we mean by that is to select which of the outcomes will be coded as `1` and `0`. This is important when assessing coefficients from logistic models, where the direction of the effect is as important or more important than the magnitude of that coefficient. Here, we will assign *Accepted* to `1` and *Rejected* to `0`.

#+ logit
adm_df$Admit_logit <- ifelse(adm_df$Admit == "Accepted", 1, 0)

adm_logit <- glm(Admit_logit ~ GRE + TOEFL + SOP + LOR + CGPA + Research + Discipline, family = 'binomial', data = adm_df)

summary(adm_logit)

#+ logit-diag
logit_null <- glm(Admit ~ 1, family = "binomial", data = adm_df)

logLik(adm_logit) - logLik(logit_null)


#' ## Mixed-effect linear models
#' 
#' Mixed effect models, also known as multilevel models or hierarchical models, are a type of regression model used to analyze data that has a nested structure or repeated measures. They account for both fixed and random effects in the data and allow for the estimation of parameters at both the individual and group levels. Mixed effect models are commonly used when observations are not independent or where individual variability needs to be taken into account.

library(lme4)
library(lmerTest)

adm_lmer <- lmer(GRE ~ TOEFL + (1 | Discipline), data = adm_df)

summary(adm_lmer)








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

