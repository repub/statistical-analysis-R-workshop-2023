# Multiple Group Comparison Tests ------------------------------------------------------------------
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


#' ## Parametric tests
#' 
#' 
#' 
#' ### ANOVA
#' 
#' 
#' 
#' #### one-way ANOVA
#'
#' Fit an one-way ANOVA model to assess differences in CGPA scores by discipline.
#' 
#' * `aov()` fits an ANOVA model and needs two arguments, a model `formula` (1st) and `data` (2nd).
#' * `summary()` prints summary statistics from a fitted model.

adm_aov <- aov(CGPA ~ Discipline, data = adm_df)
summary(adm_aov)

#' From the results, we know there is a statistically significant difference in the means of at
#' least one of the disciplines.  However, an ANOVA does not provide information on which of those
#' means are different, which must be determined using a post-hoc pairwise comparisons test.  A 
#' commonly used post-hoc test for ANOVA models is the Tukey Honest Significant Difference test.
#' 
#' #### Tukey post-hoc test
#'
#' We can wrap our ANOVA model in the `TukeyHSD()` function to run the Tukey pairwise comparisons.

TukeyHSD(adm_aov)


#' #### two-way ANOVA
adm_aov2 <- aov(CGPA ~ Research * Discipline, data = adm_df)
summary(adm_aov2)

#' #### Tukey post-hoc test
TukeyHSD(adm_aov2)

#' ### MANOVA
#' 
#' MANOVA (Multivariate Analysis of Variance) is a statistical technique used to analyze the relationships between multiple dependent variables and one or more independent variables. It tests whether the mean values of the dependent variables are significantly different across groups defined by the independent variables. When a significant overall effect is found in MANOVA, it indicates that at least one of the dependent variables has a different mean value across groups.

adm_man <- manova(cbind(GRE, TOEFL, CGPA) ~ Discipline, data = adm_df)

anova(adm_man)

#' #### ANOVA as a post-hoc test
#' 
#' To determine which dependent variables are contributing to the overall significant effect, multiple ANOVAs can be conducted on each dependent variable separately. This approach is useful when there are many dependent variables and it is not clear which variables are driving the overall effect. By analyzing each variable separately, it is possible to identify the specific variables that are contributing to the overall effect.
#' 
#' However, there are some limitations to this approach. Unlike with Tukey's post-hoc test above that corrects for multiple testing, conducting multiple ANOVAs can increase the overall Type I error rate, so it is important to adjust the significance level accordingly (e.g., using Bonferroni correction). In addition, this approach does not account for the correlations between the dependent variables, so it may not provide a complete understanding of the relationships between the variables.

anova(lm(GRE ~ Discipline, data = adm_df))
anova(lm(TOEFL ~ Discipline, data = adm_df))
anova(lm(CGPA ~ Discipline, data = adm_df))

#' Bonferroni's adjustment for multiplicity is fairly straight-forward.  We simply divide our chosen &alpha; value (0.05) by the number of comparisons to be made (3), then use that number as our threshold for statistical significance.

0.05 / 3

#' From our calculation, we would now only accept a statistical result as significant if its p-value falls below 0.01667.
#' 
#' The Bonferroni adjustment is very simple to use and significantly reduces the number of false positives (Type I errors) by adjusting the significance level for each comparison as more comparisons are added. However, the adjustment is considered to be very conservative by reducing statistical power and thus increasing the likelihood of false negatives (Type II errors). Therefore, other methods such as Benjamini-Hochberg or Holm-Bonferroni may be more appropriate if increasing the Type II error rate is an issue.


#' ## Non-parameteric multiple-group tests
#' 
#' Non-parametric tests are statistical tests that do not require assumptions about the distribution of the data being analyzed. They are used when the data do not meet the assumptions of traditional parametric tests, such as normality or equal variances. Non-parametric tests are typically less powerful than parametric tests but are more robust to violations of assumptions and can be used with a wider range of data types.
#'
#'
#' ### Kruskal-Wallis H-test
#' 
#' The Kruskal-Wallis H test is a non-parametric statistical test used to determine if there are significant differences between three or more groups on a continuous variable. It is based on ranks and is used when assumptions of normality and equal variances are not met. The test calculates a test statistic that follows a chi-square distribution, and a significant result indicates that at least one group differs significantly from the others.
#' 
#' One question we may ask is if the GRE scores for admitted students different among the 5 disciplines? From the histogram and density plots we generated during EDA, we might conclude that the underlying distribution is not normal, and therefore we can try the Kruskal-Wallis test. 
#' 
#' This can be done in `R` with the `kruskal.test()` function. The first argument takes the model formula, which we will define as `GRE ~ Discipline` as *GRE* will be the response and *Discipline* the predictor variable. We also will filter our data to only include observations where *Admit* is *Accepted* and pass that to the `data` argument.

kruskal.test(GRE ~ Discipline, data = adm_df[adm_df$Admit == 'Accepted', ])

#' With an extremely small p-value, we can conclude that the distribution of GRE scores is different in at least one of the three discplines. However, like with the ANOVA test we are not provided information on which of those groups are different from the others. Therefore, we will need to employ a post-hoc test.
#' 
#' 
#' #### Dunn's post-hoc test
#' 
#' Dunn's test is a post-hoc test used to compare pairs of groups in a Kruskal-Wallis test. It involves calculating all possible pairwise comparisons of groups and adjusting the p-values using the Bonferroni correction. Dunn's test is a non-parametric alternative to Tukey's HSD and pairwise t-tests and is particularly useful when the data do not meet the assumptions of parametric tests.
#' 
#' To perform a Dunn's test in `R` we can employ the `dunnTest()` function from the `FSA` library and pass the same model formula and data arguments as we did in the `kruskal.test()` function above.

library(FSA)

dunnTest(GRE ~ Discipline, data = adm_df[adm_df$Admit == 'Accepted', ])

#' From the tests we can conclude that each of the three *Discipline* groups have significantly different *GRE* distributions.
#' 
#' 
#' ### Non-parametric tests for 2 or more variables.
#' 
#' If we have 2 or more variables that we want to employ in one non-parametric test there are a few options available, however we are likely going to want to use some form of logistic regression. 
