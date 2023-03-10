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

#+ setup, include = FALSE
knitr::opts_knit$set(root.dir = '../')

#' ## Introduction
#' 
#' Regression analysis is one of the most fundamental and widely used statistical methods for modeling the relationship between variables. It involves identifying patterns and strengths of the relationship between one or more independent variables and a dependent variable. Regression models are extensively used in various fields, including finance, economics, social sciences, and engineering, to name a few. In this tutorial, we will start by introducing the basic concepts of regression analysis and gradually move on to more advanced techniques, such as multiple regression and logistic regression. By the end of this tutorial, you will have a solid understanding of the various regression methods in `R` and how to apply them to real-world problems.
#' 
#' 
#' ### Data and libraries
#'
#' First, we are going to call an `R` script which contains a function, `get_libs()`, that we will use to install and load all of the libraries for this guide. In this section we will be using:
#' 
#' - `car` - includes additional statistical functions.
#' - `lmtest` - tools to assess model fit.
#' - `lme4` and `lmerTest` - provide functions for fitting and analyzing mixed effect models.

#+ get-libs
source("src/scripts/get_libs.R")

libs <- c('car', 'lmtest', 'lme4', 'lmerTest')

get_libs(libs)

#' Next, we will need to import our data set into `R`. The data is stored in a .csv file, which we can read in using the `read.csv()` function by passing a string that gives the relative file path. When loading in data we should assign it to an object so that we can call functions on it. We should also get a glance at our data to get a sense for what it looks like, which we can do using the `head()` function to see the first few rows.

#+ load-data
adm_df <- readRDS("data/interim/adm_df.RDS")

head(adm_df)
str(adm_df)


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

#+ simple-diag
par(mfrow = c(2, 2))
plot(adm_fit)
par(mfrow = c(1, 1))

#' From the plots we can determine that:
#' 
#' - **Residuals vs Fitted** - the residuals are randomly scattered across the horizontal line to suggest constant variance and no patterns. 
#' - **Normal Q-Q** - the points fit tightly along the theoretical line, except for some tailing on the right side.
#' - **Scale_Location** - The root of the residuals are also randomly scattered horizontally.
#' - **Residuals vs Leverage** - Although one point clearly has higher leverage than the others (59), there are no points with significant leverage (which would be seen as existing outside of a significance line).
#' 
#' Overall, the diagnostics suggest that the model has a good fit, so we can continue by using the `summary()` function to print out a set of model statistics.

#+ simple-summ
summary(adm_fit)

#' The p-value for the model is statistically significant and the model has a high R^2^ value of 0.694, both suggesting a good fit. We can then interpret the coefficient for *CGPA*, which with a significant p-value can conclude that *CGPA* is significantly related to *GRE* scores, and that with a 1 unit increase in *CGPA* the expected *GRE* score increases by 40.
#' 
#' 
#' ### Multiple linear regression
#'
#' Multiple linear regression is a type of linear regression where there are two or more independent variables that are used to predict the value of the dependent variable. As multiple linear regression can be used to model the relationship between multiple predictors and the dependent variable, enabling more accurate predictions and more comprehensive analysis of the data, including potential interactions between variables.
#'
#' Here, we will fit a linear regression model predicting GRE scores from cumulative GPA and TOEFL scores using the same functions and similar format as simple linear regression above.

#+ multiple
adm_fit2 <- lm(GRE ~ CGPA + TOEFL, data = adm_df)


#' #### Model diagnostics
#' 
#' Just like with simple linear regression, we should check the fit of the model prior to making inferences from it. We can similarly plot diagnotics of the residuals using the `plot()` function on the model.

#+ multiple-diag
par(mfrow = c(2, 2))
plot(adm_fit2)
par(mfrow = c(1, 1))

#' Similar to the simple linear model, the diagnostic plots here suggest a good fit as there are no extreme deviations from normality, equal variance, or outliers or leverage points.
#' 
#' In multiple linear regression we may also want to assess multicollinearity, which occurs when two or more independent variables in a linear regression model are highly correlated with each other. This can lead to problems in the estimation of the model's coefficients, as the presence of multicollinearity can cause the coefficients to be unstable or even have the opposite sign from what would be expected, making it difficult to interpret the relationship between each independent variable and the dependent variable, and can reduce the model's predictive power.
#' 
#' We can compute variance inflation factors (VIFs), which are a measure of the degree of multicollinearity between independent variables in a linear regression model, to assess multicollinearity. VIFs represent how much the variance of the estimated coefficient is inflated due to multicollinearity. A high VIF value (>10) is often considered indicative of significant multicollinearity and suggests that the variable may need to be dropped from the model or further investigated.
#' 
#' We can use the `vif()` function from the `car` package to compute VIFs from our model.

#+ multiple-vif
library(car)

vif(adm_fit2)

#' The VIFs for *CGPA* and *TOEFL* are both relatively low, and therefore we can assume that multicollinearity is not an issue in this model.
#' 
#' 
#' #### Multiple regression with categorical predictors
#' 
#' Linear regression with all categorical variables is equivalent to ANOVA models, while regression with a mix of categorical and continuous variables is equivalent to ANCOVA models.

#+ multiple2-fit
adm_fit3 <- lm(GRE ~ CGPA + TOEFL + Discipline, data = adm_df)
summary(adm_fit3)

plot(adm_fit3)

#'  ANCOVA

#+ ancova
anova(adm_fit3)

#' Contrasts


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

adm_logit <- glm(Admit_logit ~ GRE + TOEFL + SOP + LOR + CGPA + Research + Discipline,
                 family = 'binomial',
                 data = adm_df)

#' ### Model diagnostics
#' 
#' While we can use the `plot()` function to assess the residuals, they are much more difficult to interpret for logistic regression models compared to linear models. Instead, we can use a likelihood ratio test to determine if the model has a better fit over its null model, which is the model where the only independent variable is the intercept. Rather than re-writing everything above, we can use the `update()` function to "update" our previous model, using the formula `. ~ 1` to indicate that we will keep the dependent variable (with `.`) while keeping only the intercept (with `1`). Then, we will perform a likelihood ratio test on the null and full models to determine if the full model has a better fit on the data compared to random chance using the `lrtest()` function from the `lmtest` library.

#+ logit-diag
logit_null <- update(adm_logit, . ~ 1)

lrtest(logit_null, adm_logit)

#' The results of the likelihood ratio test indicate that the model does have a significantly better fit to the data than random chance.
#' 
#' We can also use a deviance test to determine if the model has a lack of it. To do so, we calculate the deviance and residual degrees-of-freedom of the model using `deviance()` and `df.residual()`, respectively. We then use the `pchisq()` function to calculate a probability value from a &chi;^2^ distribution using the deviance and residual degrees of freedom. Subtracting this value from 1 will give us a p-value for this deviance test.

#+ logit-deviance
g2 = deviance(adm_logit)
df = df.residual(adm_logit)
1 - pchisq(g2, df)

#' With a p-value of approximately 1, we can conclude that the logistic regression model does not have a lack of fit. Next, we will use the `summary()` function to make inferences from the model.
#' 
#' As with linear models we should assess whether there are any issues with multicollinearity. We will again use the `vif()` function from the `car` package to do so.

#+ logit-vif
vif(adm_logit)

#' All of the VIFs (labeled GVIF here as they are an approximation for a generalized linear model) are low, and we can assume that there are no issues of multicollinearity. Next, we will use the `summary()` functoin to print summary statistics from the model to make inferences.

#+ logit-summ
summary(adm_logit)

#' The results of the logistic regression model suggest that *GRE*, *LOR*, and *CGPA* are all significant predictors of applicant admittance. The coefficients for each of these variables are positive to indicate that the log-odds of being accepted into the school increase when any of those variables increases.
#' 
#' 
#' ## Mixed-effect linear models
#' 
#' Mixed effect models, also known as multilevel models or hierarchical models, are a type of regression model used to analyze data that has a nested structure or repeated measures. They account for both fixed and random effects in the data and allow for the estimation of parameters at both the individual and group levels. Mixed effect models are commonly used when observations are not independent or where individual variability needs to be taken into account.
#' 
#' There are a few packages that can fit mixed effect models, with `lme4` being one of the more commonly used. An additional package, `lmerTest` expands the toolset and improves some of the output of the `lme4` package.
#' 
#' Mixed effect models have the model formula format of "response ~ fixed_variables + (random_effects)". The random effect term will depend on the data, but will typically follow the formula "(intercept | grouping_factor)".
#' 
#' For example, we may want to identify what factors predict an accepted student's GPA in their first year of their program, but have reason to believe that the year the student applied does not have a fixed effect. Therefore, we will use the `lmer()` function to fit a fixed model with *GRE*, *TOEFL*, *SOP*, *LOR*, *CGPA*, *Research* and *Discipline* as fixed effects and *Year* as a random effect. While `lmer()` will automatically ignore rows with missing values, it is good practice to pre-filter them which we will do by creating a new data frame with only "Accepted" applicants.

#+ lmer
library(lme4)
library(lmerTest)

adm_lmer_df <- adm_df[adm_df$Admit == "Accepted", ]

adm_lmer <- lmer(GPA1 ~ GRE + TOEFL + SOP + LOR + CGPA + Research + Discipline + (1 | Year),
                 data = adm_lmer_df)

#' Note that we receive a message: `boundary (singular) fit: see help('isSingular')`. While this is not necessarily an issue, it can be indicative that there is some underlying problem, such as overfitting or not coding the random effect properly. Here, it is not an issue, however if you run into this with your own dataset you may want to consult the documentation to learn more by typing `?isSingular` in your `R` console.
#' 
#' 
#' ### Model diagnostics
#' 
#' While the `plot()` function provides 4 different diagnostic plots on models fit using `lm()`, we only get the Residuals vs. Fitted plot with `lmer()` models. Therefore, we will also use two other functions, `qqnorm()` and `qqlint()` to generate a QQ-plot of the residuals, which we will get with `resid()`.

#+ lmer-diagnostic
plot(adm_lmer)
qqnorm(resid(adm_lmer))
qqline(resid(adm_lmer))

#' Both plots suggest that the model is a good fit as there is no extreme deviation from normality, constant variance, or high leverage points or outliers. However, knowing that there is some correlation among these variables we will also want to compute VIFs using the `vif()` function from the `car` package.

#+ lmer-vif
vif(adm_lmer)

#' All of the VIFs are low enough to not indicate problematic multicollinearity in the model, so we will conclude that the model is a good fit and print summary statistics.

#+
summary(adm_lmer)

#' From the results we can conclude that of the variables in the model only *CGPA* is predictive of a student's first-year GPA in their program.
#' 
#' 
#' ## Concluding remarks
#' 
#' In conclusion, regression methods are powerful tools for analyzing relationships between variables and making predictions based on available data. Linear regression is a widely used technique for modeling linear relationships, and can be extended through the use multiple regression to capture more complex relationships. Generalized Linear Models (GLMs) are a class of regression models that extend the linear regression framework to handle non-normally distributed response variables, such as binary or count data. Mixed effect models can account for both fixed and random effects in the data, allowing for the modeling of individual differences and repeated measures over time. However, it is important to remember that regression analysis cannot prove causation and should be used in conjunction with other analytical tools and domain knowledge to support decision making.