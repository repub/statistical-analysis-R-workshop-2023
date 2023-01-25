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

#+ load-dplyr
library(dplyr)

adm_df <- read.csv("data/raw/adm_data.csv")

# Re-code variables as factors
adm_df$Research <- factor(adm_df$Research)
adm_df$Discipline <- factor(adm_df$Discipline)
adm_df$Admit <- factor(adm_df$Admit)

head(adm_df)

#' ## Tests of distributions

#' ### Shapiro-Wilk test
#' 
#' The Shapiro-Wilk test is a statistical test used to determine whether a sample of data comes from
#' a normal distribution. The test calculates a test statistic, W, and compares it to a critical
#' value from a table of values. If the calculated test statistic is less than the critical value,
#' the null hypothesis (that the sample comes from a normal distribution) is rejected. The test is
#' generally considered to be one of the most powerful tests for normality, although it is not
#' always the most appropriate test to use in every situation, such as when the sample size is very
#' small (typically less than 50) or when the sample is not normally distributed but is instead
#' skewed or has heavy tails. Additionally, it is not well suited for samples that have large
#' amounts of outliers. 
#' 
#' The Shapiro-Wilk test can be performed in `R` with the `shapiro.test()` function, which takes a
#' single variable as an argument.

shapiro.test(adm_df$GRE)
shapiro.test(adm_df$TOEFL)

#' The p-values for the *GRE* and *TOEFL* variables suggest that both variables are not normally
#' distributed.

#' ### Levene's test
#' 
#' A statistical test for equal variances is called a Levene's test. It is used to determine if
#' there is a significant difference in variances between two or more groups. It is a robust test,
#' meaning it is not affected by outliers or non-normality of the data. The null hypothesis of the
#' test is that the variances are equal across all groups, and the alternative hypothesis is that at
#' least one group has a different variance. A significant p-value from the test would indicate that
#' the variances are not equal and you should not use a t-test that assumes equal variances.
#' 
#' To perform a Levene's test in `R`, you can use the `leveneTest()` function in the `car` package. The
#' function takes a formula and a data frame as its arguments. The formula should specify the
#' variable of interest and the group variable, separated by a `~` symbol.

library(car)

leveneTest(GRE ~ Admit, adm_df)

#' We see from the results that the Levene test was not significant for GRE scores (p = 0.15), and
#' we can therefore conclude that the variances are equal.

#' ## Fisher's exact test

#' ## Chi-square Test

# Does undergraduate research experience influence grad school admittance?
# Or, are undergraduate research experience and graduate school admittance independent?

chisq.test(adm_df$Research, adm_df$Admit)


# T-test

# * One-sample

# * Unpaired

# Assumtion for t-test = equal variances

# * Paired

# Is the mean CGPA different between students who did and did not do undergraduate research?

t.test(CGPA ~ Research, data = adm_df)

# Non-parametric test

# * Wilcox-test


# * Mann-Whitney U-test


# ?