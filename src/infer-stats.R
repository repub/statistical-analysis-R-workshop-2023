#' ---
#' title: "Inferential Statistics"
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
#' 

#+ load-data
adm_df <- readRDS("data/interim/adm_df.RDS")

head(adm_df)

#' ## Tests on assumptions
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

#' ### Levene's test
#' 
#' Levene's test can be used to determine if there is a significant difference in variances between two or more groups. It is a robust test, meaning it is not affected by outliers or non-normality of the data. The null hypothesis of the test is that the variances are equal across all groups, and the alternative hypothesis is that at least one group has a different variance. A significant p-value from the test would indicate that the variances are not equal and you should not use a t-test that assumes equal variances.
#' 
#' To perform a Levene's test in `R`, you can use the `leveneTest()` function in the `car` package. The function takes a formula and a data frame as its arguments. The formula should specify the variable of interest and the group variable, separated by a `~` symbol.

#+ levene
library(car)

leveneTest(GRE ~ Admit, adm_df)

#' We see from the results that the Levene test was not significant for GRE scores (p = 0.15), and we can therefore conclude that the variances are equal.

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
table(adm_df$Research, adm_df$Admit)

chisq.test(adm_df$Research, adm_df$Admit)

#' ### Fisher's exact test
#' 
#' Fisher's exact test is a statistical significance test used to determine the association between two categorical variables in a contingency table. It is often used when sample sizes are small or when the assumptions of the &chi;^2^ test, another commonly used test for categorical data, are not met. Fisher's exact test calculates the probability of observing the data or more extreme data if the null hypothesis (that there is no association between the variables) is true. If this probability is small enough, we reject the null hypothesis in favor of the alternative hypothesis (that there is an association between the variables).

#+ fisher
fisher.test(adm_df$Research, adm_df$Admit)

#' ## Comparing distributions
#' 
#' ## T-tests
#' 
#' T-tests are a family of statistical significance tests used to compare the means of one or two groups of data. There are two main types of t-tests: the independent samples t-test, which compares the means of one or two independent groups of data, and the paired samples t-test, which compares the means of two related groups of data. The t-test calculates a t-value and corresponding p-value, which can be used to determine whether the difference between the means is statistically significant.
#' 
#' ### One-sample t-test
#' 
#' A one-sample t-test is a statistical significance test used to determine whether the mean of a single sample is significantly different from a known or hypothesized value. It is commonly used to test whether a sample is drawn from a population with a specific mean. The test calculates a t-value and corresponding p-value, which can be used to determine whether the difference between the sample mean and the hypothesized mean is statistically significant.
#' 
#' For example, we may want to know wither the *CGPA* of the *Applied Science* students is less than 3.5. We can answer this question using a one-sample t-test by filtering the *Discipline* variable for only *Applied Science* students, and passing the vector of their *CGPA* scores to the `t.test()` function. We also set the `mu` argument to `3.5` and `alternative` to `'less'` to test whether the mean *CGPA* scores are less than 3.5.

# ttest1
t.test(adm_df[adm_df$Discipline == "Applied Science", "CGPA"],
       mu = 3.5,
       alternative = 'less')

#' From the results of the t-test, we can conclude with statistical evidence that the mean *CGPA* is less than 3.5.


# * Unpaired two-sample t-test

# Assumption for t-test = equal variances

# Is the mean CGPA different between students who did and did not do undergraduate research?

t.test(CGPA ~ Research, data = adm_df)

# * Paired



# Non-parametric test

# * Wilcox-test


# * Mann-Whitney U-test


# ?