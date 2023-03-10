#' ---
#' title: "Basic Inferential Statistics"
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

# PLAN:
# - Revisit shapiro test
# - levene test
# - lead-in to t-tests
# - regular t-tests
# - failed assumption of homoskedasicity
# - failed assumption of normality
# - wilcox test
# - mann-whitney test


#' ## Introduction
#' 
#' When analyzing data, it is important to ensure that the data meets certain assumptions before applying statistical tests. Failure to do so can lead to incorrect conclusions and misleading results. In this section, we will provide an overview of tests for assumptions, such as normality and homogeneity of variance, and how to assess whether these assumptions have been met. We will then cover the use of t-tests for comparing means, as well as non-parametric tests for situations where the assumptions of t-tests are not met. By the end of this section, you will have a solid understanding of these important concepts and be able to apply them to your own data analysis tasks.
#' 
#' 
#' ### Data and libraries
#' 
#' First, we are going to call an `R` script which contains a function, `get_libs()`, that we will use to install and load all of the libraries for this guide. For this section, most of the tools we will be using are already included in base `R` so will only be loading the `car` package, which provides additional statistical functions.

#+ get-libs
source("src/scripts/get_libs.R")

get_libs('car')

#' Next, we will need to import our data set into `R`. The data is stored in a .csv file, which we can read in using the `read.csv()` function by passing a string that gives the relative file path. When loading in data we should assign it to an object so that we can call functions on it. We should also get a glance at our data to get a sense for what it looks like, which we can do using the `head()` function to see the first few rows.

#+ load-data
adm_df <- readRDS("data/interim/adm_df.RDS")

head(adm_df)
str(adm_df)

#' Notice that since we saved our dataset as a .RDS file during EDA all of the features remain, importantly how we coded some variables as the factor type. So, we do not need to re-perform any manipulation to the data while still having the raw dataset as a .csv file available in case we need to revisit it.
#' 
#' 
#' ## Tests on assumptions
#' 
#' Statistical tests on assumptions are used to assess whether the data meets certain assumptions required for statistical tests, such as normality and homogeneity of variance. These tests are important to ensure that the results of the statistical tests are valid and reliable. Some common tests for assumptions include the Shapiro-Wilk test for normality, the Levene's test for homogeneity of variance, and the Kolmogorov-Smirnov test for distributional differences.
#' 
#' 
#' ### Shapiro-Wilk test
#' 
#' The Shapiro-Wilk test is a statistical test used to determine whether a sample of data comes from a normal distribution. The test calculates a test statistic, W, and compares it to a critical value from a table of values. If the calculated test statistic is less than the critical value, the null hypothesis (that the sample comes from a normal distribution) is rejected. The test is generally considered to be one of the most powerful tests for normality, although it is not always the most appropriate test to use in every situation, such as when the sample size is very small (typically less than 50) or when the sample is not normally distributed but is instead skewed or has heavy tails. Additionally, it is not well suited for samples that have large amounts of outliers. 
#' 
#' The Shapiro-Wilk test can be performed in `R` with the `shapiro.test()` function, which takes a single variable as an argument.

#+ shapiro
shapiro.test(adm_df$GRE)
shapiro.test(adm_df$TOEFL)
shapiro.test(adm_df$CGPA)

#' The p-values for the *GRE* and *TOEFL* variables suggest that both variables are not approximately normally distributed. However, a non-significant result for *CGPA*  suggests that it is approximately normally distributed.
#' 
#' When we are performing analyses with continuous and categorical variables, such as with t-tests and ANOVA analyses, we would also want to see if the continuous variables are approximately normal within each subgroup.  For example, if we were to compare if there are differences in *GRE* scores between *Accepted* and *Rejected* applicants we should see if the distribution of those scores is approximately normal in both cases.
#' 
#' To do so we can employ an apply function, `tapply()`, to gather each level in the *Admit* variable and perform a Shipero-Wilk test using the `shapiro.test()` function on the subsets of *GRE* scores.

#+ tapply
tapply(adm_df$GRE, adm_df$Admit, shapiro.test)

#' From the results we could conclude that the *GRE* scores for both *Accepted* and *Rejected* applicants are not normally distributed.
#' 
#' 
#' ### Levene's test
#' 
#' Levene's test can be used to determine if there is a significant difference in variances between two or more groups. It is a robust test, meaning it is not affected by outliers or non-normality of the data. The null hypothesis of the test is that the variances are equal across all groups, and the alternative hypothesis is that at least one group has a different variance. A significant p-value from the test would indicate that the variances are not equal and you should not use a t-test that assumes equal variances.
#' 
#' To perform a Levene's test in `R`, you can use the `leveneTest()` function in the `car` package. The function takes a formula and a data frame as its arguments. The formula should specify the variable of interest and the group variable, separated by a `~` symbol.

#+ levene
library(car)

leveneTest(GRE ~ Admit, adm_df)

#' We see from the results that the Levene test was not significant for GRE scores (p = 0.154), and we can therefore conclude that the variances are equal.
#' 
#' 
#' ## Comparing two categorical variables
#' 
#' ### &chi;^2^ Test
#' 
#' The &chi;^2^ (chi-squared) test is a statistical significance test used to determine the association between two categorical variables in a contingency table. It compares the observed frequencies of each combination of the two variables with the frequencies that would be expected under the assumption of independence between the variables. If the difference between the observed and expected frequencies is large enough, we reject the null hypothesis of independence and conclude that there is an association between the two variables.
#' 
#' For example, we may want to test whether undergraduate research experience influences grad school admittance. We could alternatively ask whether undergraduate research experience and graduate school admittance are independent.
#' 
#' In the following code block will will use the `table()` function to create a contingency table with the total number of observations within each group crossed by *Research* and *Admit*. Then, we will use the `chisq.test()` function to perform the &chi;^2^ test.

#+ chisq
table(adm_df$Discipline,
      adm_df$Research)

chisq.test(adm_df$Discipline,
           adm_df$Research)

#' The results of the &chi;^2^ test suggests that the two variables are not independent, or that applicants do vary in having undergraduate experience by their chosen discipline.
#' 
#' 
#' ### Fisher's exact test
#' 
#' Fisher's exact test is a statistical significance test used to determine the association between two categorical variables in a contingency table. It is often used when sample sizes are small or when the assumptions of the &chi;^2^ test, another commonly used test for categorical data, are not met. Fisher's exact test calculates the probability of observing the data or more extreme data if the null hypothesis (that there is no association between the variables) is true. If this probability is small enough, we reject the null hypothesis in favor of the alternative hypothesis (that there is an association between the variables).

#+ fisher
fisher.test(adm_df$Research, adm_df$Admit)


#' ## Comparing distributions of two groups
#' 
#' ### Parametric tests
#' 
#' T-tests are a family of statistical significance tests used to compare the means of one or two groups of data. There are two main types of t-tests: the independent samples t-test, which compares the means of one or two independent groups of data, and the paired samples t-test, which compares the means of two related groups of data. The t-test calculates a t-value and corresponding p-value, which can be used to determine whether the difference between the means is statistically significant.
#' 
#' 
#' #### One-sample t-test
#' 
#' A one-sample t-test is a statistical significance test used to determine whether the mean of a single sample is significantly different from a known or hypothesized value. It is commonly used to test whether a sample is drawn from a population with a specific mean.
#' 
#' For example, we may want to know wither the *CGPA* of the *Applied Science* students is less than 3.5. We can answer this question using a one-sample t-test by filtering the *Discipline* variable for only *Applied Science* students, and passing the vector of their *CGPA* scores to the `t.test()` function. We also set the `mu` argument to `3.5` and `alternative` to `'less'` to test whether the mean *CGPA* scores are less than 3.5.

#+ ttest1
t.test(adm_df[adm_df$Discipline == "Applied Science", "CGPA"],
       mu = 3.5,
       alternative = 'less')

#' From the results of the t-test, we can conclude with statistical evidence that the mean *CGPA* is less than 3.5.
#' 
#' 
#' #### Unpaired, two-sample t-test
#' 
#' An unpaired two-sample t-test is a statistical significance test used to compare the means of two independent groups of data. It is commonly used to test whether there is a significant difference between the means of two groups, such as a treatment group and a control group. This test assumes that the two groups have equal variances and are normally distributed.

#+ ttest2
t.test(CGPA ~ Research, data = adm_df)

#' The `t.test()` function is set by default to assume unequal variances.



#' #### Paired t-test
#' 
#' A paired t-test is a statistical significance test used to compare the means of two related groups of data. It is commonly used to test whether there is a significant difference between the means of two measurements taken on the same individuals or objects, such as before and after a treatment. This test assumes that the differences between the pairs are normally distributed.
#' 
#' For example, we may be interested if an accepted applicant's GPA in their first year differs from their undergraduate GPA. We could then use a paired t-test using the `t.test()` function with `paired = TRUE` on the two variables. Note that while *GPA1* has missing values, the `t.test()` function automatically omits them, so will only include applicants who have a first-year GPA (accepted applicants) in the test.

t.test(x = adm_df$CGPA,
       y = adm_df$GPA1,
       paired = TRUE)

#' In the results of the test we see that there is a very small p-value and therefore there is a significant difference in the student's undergraduate GPA and first-year GPA in the graduate program. Additionally, we see that from the `mean difference` the average first-year GPA is lower than the average undergraduate GPA by 0.313.
#' 
#' 
#' ### Non-parametric tests
#'
#'Non-parametric tests are statistical significance tests that do not make assumptions about the distribution of the data being tested. They are often used when the data is not normally distributed or when the assumptions of parametric tests, such as t-tests, are not met. Non-parametric tests for one or two groups of data include the Wilcoxon test and the Mann-Whitney U test. These tests are based on ranks or other non-parametric measures of the data, and are often less powerful than their parametric counterparts, but they are more robust and can provide reliable results in a wider range of data scenarios.
#'
#'
#' #### Wilcoxon tests
#' 
#' The Wilcoxon test is a non-parametric statistical significance test used to compare two independent samples of data. It is commonly used when the assumptions of the t-test cannot be met, such as when the data is not normally distributed or the sample size is small. The test compares the median values of the two groups and calculates a p-value, which can be used to determine whether the difference between the medians is statistically significant.

wilcox.test(adm_df[adm_df$Discipline == "Science & Eng", "LOR"],
            alternative = "greater",
            mu = 4)


wilcox.test(SOP ~ Admit,
            data = adm_df)


#' #### Mann-Whitney U test
#' 
#' The Mann-Whitney U test is a non-parametric statistical significance test used to compare the medians of two independent groups of data. It is commonly used when the assumptions of the t-test cannot be met, such as when the data is not normally distributed or the sample size is small. The test ranks the values in the two groups and calculates a U-value, which can be used to determine whether the difference between the medians is statistically significant. The Mann-Whitney U test is often used as an alternative to the t-test when the data does not meet the assumptions of parametric tests.


#' ## Comparing two continuous or ordinal variables
#' 
#' Simple linear regression and correlations are statistical methods used to describe the relationship between two variables. Simple linear regression is used when one variable is used to predict another, and is characterized by a linear equation that represents the relationship between the variables. Correlation, on the other hand, measures the strength and direction of the relationship between two variables without making any assumptions about causality. Pearson's correlation is used when both variables are continuous and normally distributed, while Kendall and Spearman correlations are non-parametric measures used when the variables are ordinal or not normally distributed. Each measure provides a correlation coefficient, which ranges from -1 to 1 and indicates the strength and direction of the relationship between the variables.
#' 
#' 
#' ### Simple linear regression
#' 
#' Simple linear regression is a type of linear regression where there is only one independent variable that is used to predict the value of the dependent variable. It is a commonly used technique in statistical analysis and can be used to model and predict the relationship between variables in a simple and interpretable way.
#' 
#' Here, we will fit a simple linear regression model predicting GRE scores from cumulative GPA. We will supply the `lm()` function with two arguments, first a model formula and second our dataset.

#+ slr
adm_fit <- lm(GRE ~ CGPA, adm_df)

summary(adm_fit)

#' From the results we can make a few inferences:
#' 
#' 1. The very small p-value allows us to conclude that the model has a statistically significant fit.
#' 2. A `R^2^` value of `0.694` indicates that 69.4% of the variance in *GRE* is explained by *CGPA*.
#' 3. The coefficient for *CGPA* is statistically significant, and as *CGPA* increases by 1 unit the expected value of *GRE* increases by 40.
#' 
#' 
#' ### Pearson's correlation
#' 
#' Pearson correlation is a measure of the strength and direction of the linear relationship between two continuous variables. It calculates the correlation coefficient, which ranges from -1 to 1, and indicates the strength and direction of the relationship between the two variables. Pearson correlation assumes that the data is normally distributed and that there is a linear relationship between the two variables.
#' 
#' While we were introduced to correlations to observe general relationships between variables during EDA, to make inferences on these correlations we would also want to perform a statistical test. Here, we will use the `cor.test()` function to calculate the Pearson correlation coefficient and perform a Pearson's product-moment correlation test on *GRE* and *CGPA*.

#+ cor
cor.test(adm_df$GRE, adm_df$CGPA)

#' The results of the test give a very small p-value to indicate that *GRE* and *CGPA* are significantly correlated, and the Pearson coefficient of that correlation, *r*, is 0.833, further suggesting a strong, positive relationship. Note that this coefficient is the square of the coefficient given in the linear regression model above!
#' 
#' 
#' ### Spearman and Kendall correlations
#' 
#' Kendall and Spearman correlations are non-parametric measures of correlation used to describe the relationship between two variables. Unlike Pearson correlation, which assumes that the variables are continuous and normally distributed, Kendall and Spearman correlations do not make any assumptions about the distribution of the data. Kendall correlation measures the strength and direction of the relationship between two variables that are ordinal or ranked, while Spearman correlation measures the strength and direction of the relationship between two variables that are measured on an interval or ratio scale. 
#' 
#' Here we will again use the `cor.test()` function to compute the Kendall correlation and perform a Kendall's rank correlation *&tau;* test on *SOP* and *LOR*, which are on an ordinal scale. The `cor.test()` function defaults to using Pearson statistics, so we will set `method = "kendall"` to specify using Kendall statistics.

#+ kendall
cor.test(adm_df$SOP,
         adm_df$LOR,
         method = "kendall")

#' From the results of the Kendall rank test we can conclude that there is a significant relationship between *SOP* and *LOR*, where the Kendall correlation coefficient, *&tau;*, is 0.601 to suggest a moderately strong, positive relationship.
#'
#'
#' ## Concluding remarks
#' 
#' Understanding and testing assumptions is a critical step in any data analysis task. By checking for normality, homogeneity of variance, and other assumptions, you can ensure that your statistical tests are valid and reliable. Additionally, having a strong grasp of t-tests and non-parametric tests allows you to choose the appropriate statistical tests for your data and draw accurate conclusions. By mastering these fundamental concepts, you will be better equipped to analyze and interpret data, and make informed decisions based on evidence.
#' 
#' The next sections will build upon these tools, where instead of one and two groups we will learn to use tools to to assess multiple groups in a single test.