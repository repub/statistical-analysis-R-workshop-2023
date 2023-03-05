#' ---
#' title: "Exploratory Data Analysis"
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

# data set modified from https://www.kaggle.com/datasets/akshaydattatraykhare/data-for-admission-in-the-university

#' ## Introduction
#' 
#' Exploratory Data Analysis (EDA) is an approach to analyzing and summarizing data sets to gain a better understanding of the underlying structure, patterns, and relationships. It is often used as a preliminary step before more formal statistical modeling or hypothesis testing. EDA typically involves visualizing data using plots and graphs, calculating summary statistics, and identifying outliers and other unusual observations. The goal is to identify patterns, trends, and insights that can inform further analysis and decision-making.
#' 
#' There are several steps and techniques that are commonly used in performing Exploratory Data Analysis (EDA):
#'
#' 1. **Importing and cleaning the data** - This involves loading the data into a software environment and checking for missing values, duplicated values, and other inconsistencies.
#'
#' 2. **Identifying patterns and outliers** - This involves identifying patterns, trends, and  outliers in the data by examining the visualizations and summary statistics from the previous steps.
#'
#' 3. **Univariate analysis** - This involves analyzing each variable independently by calculating summary statistics (such as mean, median, and standard deviation) and creating visualizations (such as histograms and box plots) to understand the distribution of each variable.
#' 
#' 4. **Bivariate analysis** - This involves analyzing the relationship between two variables by  creating visualizations (such as scatter plots and correlation matrix) and calculating  correlation coefficients (such as Pearson’s  *r*) to understand the strength and direction of the relationship.
#' 
#' 5. **Multivariate analysis** - This involves analyzing the relationship between more than two variables by creating visualizations (such as scatter plot matrix and parallel coordinates plot) and using techniques such as principal component analysis (PCA) to understand the structure of  the data.
#' 
#' Note that EDA is an iterative process and not all techniques will be applied depending on the data set and question(s).  Additionally, you may need to go back and repeat steps or apply different techniques as you gain a deeper understanding of the data.
#' 
#' 
#' ## Data import and cleaning
#'
#' First we will need to import our data set into `R`. The data is stored in a .csv file, which we can read in using the `read.csv()` function by passing a string that gives the relative file path. When loading in data we should assign it to an object so that we can call functions on it. We should also get a glance at our data to get a sense for what it looks like, which we can do using the `head()` function to see the first few rows.

#+ load-data
adm_df <- read.csv("data/raw/adm_data.csv")

head(adm_df)

#' ### Data structure
#' 
#' Before diving into analyses it is good to know how the dataset is structured. With the `str()` function we can print the dimensions of the dataset in rows (observations) by columns (variables), in addition to listing the variables, their types, and their first few values. 

#+ str
str(adm_df)

#' We can see that there are 400 total observations across 11 variables. Four of the variables were read in as `int`, meaning *integer*, which is specific numeric data type that does not allow decimal points. Other variables are coded as either `num` for *numeric*, which can include decimal points, and `chr` for *character* which is the same as *string*.
#' 
#' For our analysis we will want to re-code these variables as factors, which we can easily do with the `factor()` function. After assigning the variables to factors we can use the `summary()` function again to see how those variables are distributed.

#+ code-factors
adm_df$Research <- factor(adm_df$Research)
adm_df$Discipline <- factor(adm_df$Discipline)
adm_df$Admit <- factor(adm_df$Admit)

summary(adm_df)


#' ### Check for missing values
#' 
#' To check for missing values or `NA` values we can use the `is.na()` function. This function returns a logical vector or matrix indicating which elements are missing.  When wrapped in the `sum()` function, the total number of `NA` values will be returned.

#+ any-na
sum(is.na(adm_df))

#' We can further use an apply function to get the total number of missing values in each variable (column).

#+ sapply-na
sapply(adm_df, function(x) sum(is.na(x)))

#' The output above shows us that all of the missing values are within the *GPA1* variable. Using some filtering and the `head()` function we can print a few rows that have missing values.

#+ head-na
head(adm_df[is.na(adm_df$GPA1), ])

#' It appears that when *Admit* is "Rejected" then there is a missing value for *GPA1*. This would make sense as students who were not admitted to the school would not have a GPA for their first year in the program. We can check this further by filtering for missing values in *GPA1* then getting the summary statistics for *Admit* and *GPA1* with `summary()`.

#+ summary-na
summary(adm_df[is.na(adm_df$GPA1), c("Admit", "GPA1")])

#' We can confirm that all of the missing values in *GPA1* are when an applicant was "Rejected", and it would make sense to leave the missing values as they are. However, if we did need to resolve missing values we have a few options available to handle them:
#'
#' 1. **Remove observations** - This involves removing any observations that have missing values. This can be a viable option if the missing values are relatively few and scattered throughout the dataset. However, if a large proportion of the dataset is missing, this approach may result in losing important information.
#' 2. **Use a flag variable** - This involves creating a new variable that indicates whether a value is missing or not. This allows the model to take into account the missing data, but it requires the model to be able to handle missing data.
#' 3. **Ignore the variable** - If the variable with missing values is not important for the analysis, it can be ignored.
#' 4. **Imputation** - This involves replacing the missing values with estimates. Simple imputation methods replace missing values using the mean, median, or mode of the non-missing values. Some methods include using a predictive model, such as regression or k-nearest neighbors to estimate the missing values based on the other variables in the data set. There are also advanced imputation methods like hot-deck imputation or multiple imputation.
#' 
#' It's important to consider the specific characteristics of your data and the goals of your analysis when deciding which strategy to use. It is also important to note that imputation methods can introduce bias and variability in the data, so it's essential to validate the results and check the assumptions of the model if imputation method is chosen.
#' 
#' 
#' ### Check for duplicated values
#' 
#' Whether to keep or remove duplicate rows depends on the specific characteristics of your data and the goals of your analysis. If the duplicates represent meaningful observations, such as multiple measurements taken at different times, then it may be appropriate to keep the duplicate rows. However, if the duplicates are the result of errors or inaccuracies in the data collection process, it may be appropriate to remove them.
#'
#' In `R`, the `duplicated()` function is used to identify duplicate rows in a data frame or matrix. This function returns a logical vector indicating which rows are duplicates.  Like with `is.na()`, when wrapped in the `sum()` function, the total number of duplicated rows (`TRUE` values) will be returned. 

#+ any-duplicated
sum(duplicated(adm_df))

#' This dataset also lacks any duplicated observations. If you were to have duplicates that would be problematic for your analysis, you can use the `unique()` function, which returns a data frame or matrix with only the unique rows. If you use the `unique()` function on a dataset that does not have any duplicates, it will just return the original dataset.

#+ unique
adm_df <- unique(adm_df)

str(adm_df)

#' Alternatively, if you decide to keep the duplicate rows, it is important to be aware that they may introduce bias, noise, and variability in the data, so it is essential to validate the results and check the assumptions of the model. If the duplicates represent a small proportion of the data and not having them would affect the analysis, it is a good practice to keep them and note their presence in the analysis.
#' 
#' 
#' ### Check for patterns
#' 
#' Checking for patterns in exploratory data analysis is important because it allows us to identify relationships, trends, and outliers in the data, which can help inform our modeling choices and ensure that we are not making erroneous conclusions.
#' 
#' One type of pattern that can quickly be problematic is whether the variables have any trends with their location in the dataset. This could be the result of non-random sampling and/or non-independent observations, which would violate the assumptions of many analysis methods, or could be the result of prior sorting, which would not be a problem. Alternatively, if we are dealing with time-series data we would likely expect to see some kind of trend that we would want to model.
#' 
#' To check whether there are any trends among the variables by their location, we can plot each variable against their row number. First, we will create a vector of numbers ranging from 1 to the total number of rows. That vector will then be used for the x-axis in the `plot()` function, while we plot our variables on the y-axis.

#+ pattern
ints <- 1:nrow(adm_df)

plot(ints, adm_df$GRE, type = 'l')

#' Plotting each variable will be quite tedious, so instead we can use a loop function to quickly generate each plot. We will give these plots titles so that when we view them we are sure which variable we are observing.
#' 
#' Because there are 10 variables we will be making 10 plots. So, it would make sense to plot some of them together in the same graphic. We will use `par(mfrow = c(2, 2))` to tell `R` to plot on a 2 x 2 grid, so that we can fit 4 plots in a single graphical space. After the loop we will reset to the default of a 1 x 1 space with `par(mfrow = c(1, 1))`.
#' 
#' In the `for` loop, we will loop through a sequence of numbers from 1 to the total number of columns, 10. At each iteration, the loop will plot a line plot of the observed values against the number of the observation, which we defined in `ints` above. We will also add a title to the plot by indexes the column name at each iteration so that it is clear which plots belong to which variables.

#+ pattern-loop
par(mfrow = c(2, 2))
for (ii in 1:ncol(adm_df)) {
  plot(ints, adm_df[, ii], type = 'l')
  title(colnames(adm_df)[ii])
}
par(mfrow = c(1, 1))

#' Except for *UniqueID*, where a trend would be expected, we do not notice any obvious trends in the variables.
#' 
#' 
#' ## Univariate analysis
#' 
#' Univariate analysis is a statistical technique that is used to analyze and summarize a single variable. The goal of univariate analysis is to understand the distribution and properties of a single variable, without considering any relationships with other variables.
#' 
#' Univariate analysis typically involves calculating summary statistics such as the mean, median, and standard deviation, and creating visualizations such as histograms, box plots, and frequency tables to understand the distribution of the variable.
#' 
#' 
#' ### Summary statistics
#' 
#' To get some basic descriptive statistics on the variables we can call the `summary()` function on our data set.

#+ summary-stats
summary(adm_df)

#' In the output we can see the values on the centrality and distribution of the numeric variables, while for the *Research*, *Discipline*, and *Admit* variables we are told that they are of the character type.
#' ___________FIX__________
#' 
#' The output now gives us the total number of instances for category in the factor variables. Notably, the *Discipline* variable has a nearly balanced split between the three disciplines while in the *Admit* variable most of the students are *Accepted*.
#' 
#' 
#' #### Printing specific summary statistics
#' 
#' The `summarise()` function in `dplyr` is a powerful tool that, when combined with other functions, provide details about the data set.  For example, we can print the means and standard deviations of GRE scores separately for accepted and rejected applicants.
#' 
#' * `group_by()` groups each unique level in the Admit column (Accepted or Rejected).
#' * `summarize()` creates two new columns that will will name mean_GRE and sd_GRE with the means   and standard deviations of GRE scores as given by the `mean()` and `sd()` functions.

#+ dplyr-summ
library(dplyr)

adm_df %>%
  group_by(Admit, Year) %>%
  summarize(mean_GRE = mean(GRE),
            sd_GRE = sd(GRE))


#' ### Distributions
#' 
#' Understanding the distribution of variables is crucial for building accurate and effective models. The distribution of a variable refers to the way in which the values of that variable are spread across the data set.
#' 
#' Different types of variables can have different distributions. For example, continuous variables (such as height or weight) often follow a normal distribution, which is bell-shaped and symmetric. Categorical variables (such as gender or nationality) may follow a discrete distribution, where the values are restricted to a finite number of categories.
#' 
#' Understanding the distribution of variables is important because it affects how we model them. For example, if a variable follows a normal distribution, we can use a linear model to predict its values. However, if the variable is highly skewed or has outliers, we may need to use a different type of model, such as a non-linear model or a generalized linear model.
#' 
#' In addition, the distribution of variables can affect the assumptions of the modeling process. For example, linear models assume that the residuals (the difference between the predicted values and the actual values) follow a normal distribution. If this assumption is violated, the model may not be accurate or reliable.
#' 
#' In the context of EDA, univariate distributions can be visualized using histograms, box plots, density plots, and other statistical plots. These plots can provide insights into the shape of the distribution, such as whether it is symmetrical or skewed, as well as information about the central tendency and variability of the data.
#' 
#' 
#' #### Histograms
#' 
#' Histograms are graphs used to visualize the distribution of a continuous variable. They display the frequency or count of observations within each interval or bin of the variable's range, allowing for a quick understanding of the shape, central tendency, and variability of the data.
#' 
#' We can plot a basic histogram by supplying a continuous variable (as a vector) to the `hist()` function.

#+ hist
hist(adm_df$GRE)

#' With `ggplot()`, part of the `ggplot2` package, we can easily make more advanced plots. For example, we can plot multiple histograms for a continuous variable, each representing some factor.
#' 
#' Here, we will plot the histograms of *GRE* for each level of *Research*. The first argument in `ggplot()` takes our dataset, then we will out the `aes()` (aesthetics) argument, where we define which variables to plot on the axis, to use for coloring, etc. Next, we add `geom_histogram()` following a `+` to tell `ggplot()` to use a histogram geometry for plotting. Finally, we add `facet_wrap()` to define how we want to wrap a set of panels, which we do with `~ Research` to indicate wrapping by the values of the *Research* variable.

#+ gg-hist
library(ggplot2)

ggplot(adm_df,
       aes(x = GRE,
           fill = Research)) +
  geom_histogram() +
  facet_wrap(~ Research)

#' Using piping and some data manipulation tools from the `dplyr` and `tidyr` packages, we can also simultaneously plot histograms of multiple variables.
#' 
#' Here, we will simultaneously plot histograms for *CGPA*, *GRE*, and *TOEFL* with each separated by *Research*. We will first pipe the dataset into the `pivot_longer()` function, which creates two new variables based on a list of other variables, one with the *name* of the variables and the second with their *value*. We will then pipe the modified dataset into `ggplot()`.

#+ gg-hist-facet
library(dplyr)
library(tidyr)

adm_df %>%
  pivot_longer(c(CGPA, GRE, TOEFL)) %>%
  ggplot(aes(x = value,
             fill = Research)) +
  geom_histogram(position = 'identity') +
  facet_wrap(~ name * Research,
             # Let each panel have independently scaled x- and y-axis
             scales = 'free')

#' From the histograms, it appears students who have undergraduate research experience tend to have higher cumulative GPAs and score higher on the GRE and TOEFL exams.
#' 
#' 
#' #### Density plots
#' 
#' Density plots are a type of graph used to display the probability distribution of a continuous variable. They estimate the underlying probability density function of the data and display it as a smooth curve. The area under the curve represents the probability of observing a value within a particular range of the variable. Density plots are useful for understanding the shape, central tendency, and variability of the distribution of a variable, and they can be particularly helpful when comparing the distributions of multiple variables or groups.
#' 
#' 

#+ gg-density
ggplot(adm_df,
       aes(x = GRE,
           fill = Research)) +
  geom_density(alpha = 0.5)

#' While density plots can be better than histograms to compare the distributions of multiple groups, like if there are too few bins in a histogram if there is too much smoothing in a density plot it can hide important features of the distribution.
#' 
#' 
#' #### Boxplots
#' 
#' Boxplots are a type of graph used to visualize the distribution of a variable. They display the five-number summary (minimum, maximum, median, and first and third quartiles) of the variable, as well as any outliers. Boxplots are useful for comparing the distribution of one or more variables across different groups or categories.
#' 
#' Here, we will plot the distributions of *SOP* for the three disciplines. While we can do so with the `boxplot()` function in base `R`, `ggplot()` makes it much easier for us.

#+ gg-boxpl
ggplot(adm_df,
       aes(x = Discipline,
           y = SOP)) +
  geom_boxplot()

#' From the plots, it appears that students in *Science & Eng* have higher scores on *SOP* while those in *Humanities and Soc Science* score the lowest overall.
#' 
#' Because boxplots summarize the data into quantiles it is very useful for non-normal data, in addition to normalized data. Just note that if using a statistical test based on the assumption of normality, boxplots may not be the most appropriate type of plot to visualize your results. Instead, pointranges, lineranges, and errorbars around the means may be more appropriate.
#' 
#' 
#' #### Scatterplots
#' 
#' Scatterplots are a type of graph used to display the relationship between two continuous variables. They plot the values of one variable on the x-axis and the values of the other variable on the y-axis. Each point on the plot represents a single observation, and the pattern of the points can provide insights into the strength and direction of the relationship between the variables. Scatterplots are commonly used in exploratory data analysis and can be helpful in identifying patterns, trends, and outliers in the data.
#' 
#' 

#+ gg-point
ggplot(adm_df,
       aes(x = GRE,
           y = CGPA)) +
  geom_point()


#' ### Statistical methods to assess distributions
#' 
#' _----------------------------
#' 
#' #### Shapiro-wilk's normality test

#+ shapiro
shapiro.test(adm_df$CGPA)
shapiro.test(adm_df$GRE)



#' #### Correlations

#' First mutate all of the variables into the numeric type, then use the cor() function to get the Spearman-rank correlations between each variable.

#+ cor
adm_cor <- adm_df %>%
  mutate(across(everything(), as.numeric)) %>%
  cor(method = 'spearman')

library(corrplot)

corrplot(adm_cor, method = 'square')


#' Why do so many variables have negative relationships with admittance? Because when we changed
#' the Admit variable to numeric it was recoded so that Accepted is 1 and Rejected is 2.


#+ pairs
pairs(adm_df)

#+ gg-pairs
library(GGally)

adm_df %>%
  select(c(CGPA, GRE, TOEFL)) %>%
  ggpairs()




#' Before wrapping up, we should save our dataset that we modified so that we do not have to go through the same process again every time we come back to it. By saving the dataset as a `.RDS` file using the `saveRDS()` function. Then, when we load the `.RDS` file later the modified dataset and its metadata (things like variable types) will be restored.

#+ save
saveRDS(adm_df, 'data/interim/adm_df.RDS')
