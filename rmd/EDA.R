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
#' ### Data
#' 
#' Before working with the dataset you should get an idea of how it was collected and what it (should) contain. The dataset we will be using was collected by a university to determine what factors influence acceptance and the success of foreign students in their graduate programs. The data was collected for 400 applicants over a 4 year period, some of who were accepted and the rest rejected into the school. The variables include:
#' 
#' - *UniqueID* - a unique identifier for each applicant.
#' - *GRE* - the Graduate Record Examination score.
#' - *TOEFL* - the Test of English as a Foreign Language score.
#' - *SOP* - a scoring of their Statement of Purpose.
#' - *LOR* - a scoring for their Letters of Recommendation.
#' - *CGPA* - their standardized, Cumulative Grade Point Average.
#' - *Research* - whether the applicant has undergraduate research experience.
#' - *Discipline* - the general discipline that the applicant is applying to.
#' - *Admit* - if the applicant was accepted.
#' - *GPA1* - the first year Grade Point Average for accepted students.
#' - *Year* - the year that the applicant applied to the school.
#' 
#' 
#' ## Data import and cleaning
#'
#' First, we are going to call an `R` script which contains a function, `get_libs()`, that we will use to install and load all of the libraries for this guide. These packages are:
#' 
#' - `dplyr` and `tidyr` - tools for data manipulation.
#' - `ggplot2` - a graphical package for creating elegant data visualizations.
#' - `corrplot` - allows easy visualizations of correlation matrices.
#' - `GGally` - an extension of `ggplot2` for drawing pairs plots.

#+ get-libs
source("src/scripts/get_libs.R")

libs <- c('dplyr', 'tidyr', 'ggplot2', 'corrplot', 'GGally')

get_libs(libs)

#' Next, we will need to import our data set into `R`. The data is stored in a .csv file, which we can read in using the `read.csv()` function by passing a string that gives the relative file path. When loading in data we should assign it to an object so that we can call functions on it. We should also get a glance at our data to get a sense for what it looks like, which we can do using the `head()` function to see the first few rows.

#+ load-data
adm_df <- read.csv("data/raw/adm_data.csv")

head(adm_df)


#' ### Data structure
#' 
#' Before diving into analyses it is good to know how the dataset is structured. With the `str()` function we can print the dimensions of the dataset in rows (observations) by columns (variables), in addition to listing the variables, their types, and their first few values. 

#+ str
str(adm_df)

#' We can see that there are 402 total observations across 11 variables. Four of the variables were read in as `int`, meaning *integer*, which is specific numeric data type that does not allow decimal points. Other variables are coded as either `num` for *numeric*, which can include decimal points, and `chr` for *character* which is the same as *string*.
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

#' This dataset has 2 duplicated observations, which makes sense as we expected there to be 400 observations in total but saw that there are actually 402. Since we also know that each observation has a unique identifier, these duplicates are likely an error and can be removed. To do so, we can wrap the data frame in the `unique()` function to return only unique rows.

#+ unique
adm_df <- unique(adm_df)

str(adm_df)

#' Alternatively, if you were to decide to keep the duplicate rows, it is important to be aware that they may introduce bias, noise, and variability in the data, so it is essential to validate the results and check the assumptions of the model. If the duplicates represent a small proportion of the data and not having them would affect the analysis, it is a good practice to keep them and note their presence in the analysis.
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
#' Because there are 11 variables we will be making 11 plots. So, it would make sense to plot some of them together in the same graphic. We will use `par(mfrow = c(2, 2))` to tell `R` to plot on a 2 x 2 grid, so that we can fit 4 plots in a single graphical space. After the loop we will reset to the default of a 1 x 1 space with `par(mfrow = c(1, 1))`.
#' 
#' In the `for` loop, we will loop through a sequence of numbers from 1 to the total number of columns, 10. At each iteration, the loop will plot a line plot of the observed values against the number of the observation, which we defined in `ints` above. We will also add a title to the plot by indexes the column name at each iteration so that it is clear which plots belong to which variables.

#+ pattern-loop
par(mfrow = c(2, 2))
for (ii in 1:ncol(adm_df)) {
  plot(ints, adm_df[, ii], type = 'l')
  title(colnames(adm_df)[ii])
}
par(mfrow = c(1, 1))

#' Except for *UniqueID* and *Year*, where a trend would be expected sa they are sorted, we do not notice any obvious trends in the variables.
#' 
#' 
#' ## Univariate analysis
#' 
#' Univariate analysis is a statistical technique that is used to analyze and summarize a single variable. The goal of univariate analysis is to understand the distribution and properties of a single variable, without considering any relationships with other variables.
#' 
#' Univariate analysis typically involves calculating summary statistics such as the mean, median, and standard deviation, and creating visualizations such as histograms, box plots, and frequency tables to understand the distribution of the variable.
#' 
#' 
#' ### Printing specific summary statistics
#' 
#' As we saw above we can use the `summary()` function to get general summary statistics on our dataset, however what if we want to know some more specific statistics? For example, maybe we want to know the group means and errors between the *GRE* scores of applicants who did or did not do undergraduate research for each year in our dataset. The `summarise()` function in `dplyr` is a powerful tool that, when combined with other functions, provide details about the data set.  For example, we can print the means and standard deviations of GRE scores separately for accepted and rejected applicants.
#' 
#' First, we will pipe (`%>%`) the data frame into the `group_by()` function to group the observations on the *Research* and *Year* variables. Then, we will pipe the grouped data to `summarize()` and calculate the mean, `mean()`, and standard deviation, `sd()`, of the GRE scores within each group.

#+ dplyr-summ
adm_df %>%
  group_by(Research, Year) %>%
  summarize(mean_GRE = mean(GRE),
            sd_GRE = sd(GRE))

#' From the summary statistics, we might hypothesize that GRE scores are higher for students who have undergraduate research experience, and that the scores generally may be decreasing over the four years for those applicants who did not do research during their undergraduate program.
#' 
#' 
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
ggplot(adm_df,
       aes(x = GRE,
           fill = Research)) +
  geom_histogram() +
  facet_wrap(~ Research)

#' Using piping and some data manipulation tools from the `dplyr` and `tidyr` packages, we can also simultaneously plot histograms of multiple variables.
#' 
#' Here, we will simultaneously plot histograms for *CGPA*, *GRE*, and *TOEFL* with each separated by *Research*. We will first pipe the dataset into the `pivot_longer()` function, which creates two new variables based on a list of other variables, one with the *name* of the variables and the second with their *value*. We will then pipe the modified dataset into `ggplot()`.

#+ gg-hist-facet
adm_df %>%
  pivot_longer(c(CGPA, GRE, TOEFL)) %>%
  ggplot(aes(x = value,
             fill = Research)) +
  geom_histogram(position = 'identity') +
  facet_wrap(~ Research * name,
             # Let each panel have independently scaled x- and y-axis
             scales = 'free')

#' From the histograms, it appears students who have undergraduate research experience tend to have higher cumulative GPAs in addition to scoring higher on the GRE and TOEFL exams on average.
#' 
#' 
#' #### Density plots
#' 
#' Density plots are a type of graph used to display the probability distribution of a continuous variable. They estimate the underlying probability density function of the data and display it as a smooth curve. The area under the curve represents the probability of observing a value within a particular range of the variable. Density plots are useful for understanding the shape, central tendency, and variability of the distribution of a variable, and they can be particularly helpful when comparing the distributions of multiple variables or groups.
#' 
#' To create a density plot with `ggplot()` we can simply replace `geom_histogram()` in the code above with `geom_density()`. We will also set `alpha = 0.5` to add some transparency to best see both groups in the *Research* variable.

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
#' Here, we will plot the distributions of *SOP* for the three disciplines. We will do this with both the `boxplot()` function in base `R` and using `ggplot()`.

#+ gg-boxpl
boxplot(SOP ~ Discipline,
        adm_df)

ggplot(adm_df,
       aes(x = Discipline,
           y = SOP)) +
  geom_boxplot()

#' From the boxplot, it appears that students in *Science & Eng* have higher scores on *SOP* while those in *Humanities and Soc Science* score the lowest overall.
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
#' EDA often involves assessing the distribution of data using statistical methods. This can include measures such as mean and standard deviation, tests or normality or independence, and correlations. These techniques can provide insight into the shape, spread, and relationships within the data, allowing for a better understanding of its characteristics and potential relationships.
#' 
#' 
#' #### Shapiro-Wilk normality test
#' 
#' The Shapiro-Wilk test is a statistical test used to assess whether a given dataset follows a normal distribution. It is based on the comparison between the observed distribution of the data and the expected distribution under a normal distribution assumption. The test produces a test statistic and a p-value, with a p-value below a specified threshold indicating evidence against the null hypothesis of normality. The Shapiro-Wilk test is commonly used in many fields, including psychology, biology, and economics, to determine whether data should be analyzed using parametric statistical methods that assume normality.
#' 
#' For example, we can use the `shapiro.test()` function on the *CGPA* variable to test whether it has an approximately normal distribution.

#+ shapiro
shapiro.test(adm_df$CGPA)

#' From the results of the Shapiro-Wilk test we can conclude that the *CGPA* variable is approximately normally distributed.
#' 
#' 
#' #### &chi;^2^ test
#' 
#' The &chi;^2^ test is a statistical test used to determine whether there is a significant association between two categorical variables. The test compares the observed frequencies of each category to the expected frequencies, assuming that there is no association between the variables. The test produces a test statistic and a p-value, with a p-value below a specified threshold indicating evidence against the null hypothesis of no association.
#' 
#' For example, we may be interested in whether applicants vary in undergraduate research experience by their discipline. We can then use the `chisq.test()` function to test whether *Discipline* and *Research* are independent (null hypothesis) or dependent. We will also use the `table()` function to print a contingency table of the two variables.

#+ chisq
table(adm_df$Discipline,
      adm_df$Research)

chisq.test(adm_df$Discipline,
           adm_df$Research)

#' The results of the &chi;^2^ test suggests that the two variables are not independent, or that applicants do vary in having undergraduate experience by their chosen discipline.
#' 
#' 
#' #### Correlations
#' 
#' Correlation is a statistical technique used to measure the strength and direction of the relationship between two variables. Correlation coefficients range from -1 to 1, with 0 indicating no correlation, -1 indicating a perfect negative correlation (i.e., as one variable increases, the other decreases), and 1 indicating a perfect positive correlation (i.e., as one variable increases, the other increases). Correlations can be used to identify patterns in data and to make predictions about future behavior. However, it is important to remember that correlation does not imply causation and that other factors may be influencing the relationship between the variables.
#' 
#' First, we will mutate all of the variables into the numeric type so that correlations can be calculated on them. Then, we will pipe the transformed data into the `cor()` function to calculate the Spearman-rank correlations between each variable. We will use the `corrplot()` function from the `corrplot` library to visualize the resulting Spearman-rank correlations.

#+ cor
adm_cor <- adm_df %>%
  mutate(across(everything(), as.numeric)) %>%
  cor(method = 'spearman')

corrplot(adm_cor, method = 'square')

#' Notably there are some `?` values on the plot. These originate from having missing values in the *GPA1* variable, which the `cor()` function is not able to handle. To look at relationships with the *GPA1* variable, we would need to subset our data to only include observations without missing values.
#' 
#' As we observe the other variables in the dataset we do see a lot of correlations between the variables. Positive (blue) correlations between *GRE*, *TOEFL*, *SOP*, *LOR*, and *CGPA* indicate that they follow similar increasing trends. For *Discipline*, while we see a moderately positive correlation on the plot, because the data is unordered we should not necessarily make any inferences just yet. Alternatively, the *Admit* variable is strongly (and negatively) correlated with a lot of the scoring variables as well as *Research*. The reason for this negative relationship is that when we changed the *Admit* variable to the numeric type it was recoded so that *Accepted* is 1 and *Rejected* is 2. When recoding, it is always important to understand how the variables are being recoded.
#' 
#' 
#' ## Multivariate comparisons
#' 
#' Multivariate comparisons involve analyzing relationships between multiple variables simultaneously. These comparisons can be useful for identifying complex patterns of association and for understanding the joint effects of different variables on an outcome of interest. There are several methods for conducting multivariate comparisons, including multiple regression analysis, principal component analysis, factor analysis, and cluster analysis. Each of these methods has its own strengths and weaknesses and should be chosen based on the research question and the type of data being analyzed. Overall, multivariate comparisons can provide a more comprehensive understanding of complex relationships between variables and can help to improve the accuracy and precision of statistical analyses.
#' 
#' 
#' ### Pairs plots
#' 
#' We can also plot the data points against each other into one plot to visualize relationships. One simple method for doing this is using the `pairs()` function, which will plot all of the variables against one another after automatically converting them to the numeric type.
#' 
#' As a warning: as the number of variables and observations increase the time it takes to plot all of the data points also increases. Make sure you have a good understanding of your data before trying to plot a large number of data points.

#+ pairs
pairs(adm_df)

#' The `GGally` package extends the utility of this type of plotting with the `ggpairs()` function, by printing a `ggplot`-style pairs plot which can identify different types of variables and select the appropriate style of plot rather than only producing scatterplots. `ggpairs()` also has the utility of plotting density distributions of each variable and giving some basic statistical information such as correlation coefficients and their significance.
#' 
#' This also increases the complexity of plotting the data compared to the `pairs()` plot above, so we will only plot a select group of variables against one another as an example.

#+ gg-pairs
adm_df %>%
  select(c(CGPA, GRE, SOP, LOR, Research, Discipline)) %>%
  ggpairs()


#' ### Principal component analysis
#' 
#' Principal component analysis (PCA) is a statistical technique used to reduce the dimensionality of a dataset by transforming the original variables into a smaller number of uncorrelated variables, called principal components. The principal components are ordered by the amount of variance they explain in the original data, with the first component explaining the most variance. PCA can be useful for visualizing complex relationships between variables, identifying underlying patterns in the data, and reducing noise and redundancy in the data.
#' 
#' To perform PCA on our dataset we can use the `prcomp()` function to compute the principal components. However, we will need to first select the variables we are interested in and convert them all to the numeric type. We will use the `select()` function to get all of the variables except for *UniqueID* and *GPA1*, then combine `mutate()`, `across()`, and `everything()` to change all the remaining variables to numeric. We then use the `prcomp()` function on the modified dataset and set both `center` and `scale.` to `TRUE`. Finally, we will use the `biplot()` function to plot the first two principal components and the vectors of the variables, setting `xlabs` to `rep('.', 400)` so that each individual observation is plotted as a point.

#+ pca
adm_pca_df <- adm_df %>%
  select(!c(UniqueID, GPA1)) %>%
  mutate(across(everything(),
                as.numeric))

adm_pca <- prcomp(adm_pca_df,
                  center = TRUE,
                  scale. = TRUE)

biplot(adm_pca,
       xlabs = rep('.', 400))

#' Each of *CGPA*, *Research*, *GRE*, *TOEFL*, *SOP*, and *LOR* point generally in the same direction with similar magnitudes, suggesting that there is some correlation between these variables (which we also saw in the correlogram above). Alternatively, *Year* is orthogonal to those variables, suggesting that they may not be changing over the 4 year period studied.
#' 
#' 
#' ## Save modified dataset
#' 
#' Before wrapping up, we should save our dataset that we modified so that we do not have to go through the same process again every time we come back to it. By saving the dataset as a `.RDS` file using the `saveRDS()` function. Then, when we load the `.RDS` file later the modified dataset and its metadata (things like variable types) will be restored.

#+ save
saveRDS(adm_df, 'data/interim/adm_df.RDS')


#' ## Concluding remarks
#' 
#' EDA is an essential step in the statistical analysis of any dataset. By examining the distribution, variability, and relationships between variables, researchers can gain a deeper understanding of the data and make informed decisions about appropriate statistical methods. In this tutorial, we have discussed several techniques for EDA, including histograms, box plots, correlation analysis, and PCA. By using these techniques, researchers can identify patterns, outliers, and potential sources of bias or confounding, ultimately leading to more accurate and reliable statistical analyses.
#' 
#' In the next section we will use basic statistical tools to make inferences on the data.




