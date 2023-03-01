#' ---
#' title: "Multiple Group Comparison Tests"
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
#' Multiple group comparison tests are statistical tests used to analyze the differences between three or more groups on a continuous or categorical dependent variable. These tests are essential in experimental and observational studies, as they can provide insights into the relationships between multiple factors and an outcome variable. There are several tests available for multiple group comparison, including parametric tests such as ANOVA and MANOVA, non-parametric tests such as Kruskal-Wallis, and post-hoc tests that allow for pairwise comparisons between groups. Understanding the assumptions, advantages, and limitations of each test is crucial for choosing the appropriate test for a specific analysis.
#' 
#' This section will provide a comprehensive overview of multiple group comparison tests, including their types, assumptions, and procedures, and will guide you in choosing the best test for your data analysis.

#+ load-data
adm_df <- readRDS("data/interim/adm_df.RDS")

head(adm_df)


#' ## Parametric tests
#' 
#' Parametric tests are statistical tests that assume that the data being analyzed follow a particular distribution, typically a normal distribution. They are based on the assumption of equal variances and are sensitive to outliers. Parametric tests are commonly used in scientific research and can provide powerful and precise results when the data meet the assumptions of the test.
#' 
#' 
#' ### ANOVA
#' 
#' ANOVA (Analysis of Variance) is a statistical technique used to analyze the differences between three or more independent groups on a continuous dependent variable. It tests whether the mean values of the dependent variable are significantly different across groups defined by the independent variable. ANOVA is based on the assumption of equal variances and normality, and is sensitive to outliers.
#' 
#' 
#' #### one-way ANOVA
#'
#' The simplest form of the ANOVA is the one-way ANOVA (ANOVA^1^), where we have one predictor variable. Here, we will fit an ANOVA^1^ model to assess differences in CGPA scores by discipline. The `aov()` fits an ANOVA model and needs two arguments, a model `formula` (1st) and `data` (2nd). We then will use `summary()` to print summary statistics from the fitted model.

#+ anova1
adm_aov <- aov(CGPA ~ Discipline, data = adm_df)

summary(adm_aov)

#' From the results, we know there is a statistically significant difference in the means of at least one of the disciplines.  However, an ANOVA does not provide information on which of those means are different, which must be determined using a post-hoc pairwise comparisons test. Commonly used post-hoc tests for ANOVA models are Tukey Honest Significant Difference (HSD) test and pairwise t-tests.
#' 
#' #### Tukey HSD post-hoc test
#' 
#' Tukey's HSD test is a post-hoc test used to determine which specific pairs of groups differ significantly from each other after a significant effect has been found using ANOVA. It is a conservative test that controls the family-wise error rate and compares the difference between the means of each pair of groups to a critical value based on the number of groups and the total number of observations. Tukey's HSD is widely used in scientific research and is considered a reliable method for post-hoc testing.
#'
#' We can wrap our ANOVA^1^ model in the `TukeyHSD()` function to run the Tukey pairwise comparisons.

#+ tukey1
TukeyHSD(adm_aov)

#' From the results of the Tukey HSD pairwise comparisons, we can conclude that the mean **CGPA** of each of the *Discipline* groups is statistically different from one another, specifically with **Science & Eng** having the highest mean **CGPA** and **Humanities and Soc Science** having the lowest mean **CGPA**.


#' #### two-way ANOVA
#' 
#' A two-way ANOVA (ANOVA^2^) is an extension of the one-way ANOVA used to analyze the effects of two independent variables on a continuous dependent variable. The ANOVA^2^ is commonly used in experimental and observational studies to examine the effects of two factors on an outcome variable, and it can provide valuable insights into the relationships between these factors and the outcome.
#' 
#' With two-way and higher order ANOVA models we can include interaction terms which represent the combined effect of two or more independent variables on the dependent variable that cannot be explained by the individual effects of each variable alone. In other words, the effect of one variable on the dependent variable depends on the level of the other variable(s).

#+ anova2
adm_aov2 <- aov(CGPA ~ Research * Discipline, data = adm_df)

summary(adm_aov2)

#' Sums of squares
#' 
#' Type I, Type II, and Type III sums of squares are methods for partitioning the total sum of squares in a linear regression model. The main difference between the three types of sums of squares is the order in which the variables are entered into the model and the way in which they are tested for significance.
#' 
#' Type I sums of squares is the most commonly used method and is based on sequential addition of predictor variables to the model. Type II sums of squares is similar to Type I, but tests each predictor variable's unique contribution to the model while controlling for the other variables in the model. Type III sums of squares is a more complex method that accounts for the correlation between the predictor variables and can be used in models with categorical variables or interactions.
#' 
#' Typically, when we are testing interaction terms we first use Type III sum of squares to determine whether those interaction terms are statistically significant. However, base `R` returns the Type II sums of squares when using interaction terms, so we can empoy the `Anova()` function from the `car` package to calculate the Type III sum of squares table.

#+ type3
library(car)

Anova(adm_aov2, type = "3")

#' Notably, the interaction term between *Research* and *Discipline* is statistically significant, and therefore we should not assess their independent terms.
#' 
#' Interpreting interaction effects can be quite confusing however. First, there will be a number of pairwise comparisons equal to:
#' #' $$(k_1 \* k_2)((k_1 \* k_2) - 1) / 2$$
#' 
#' Which in this case will be (2 \* 3)((2 \* 3) - 1) / 2 = 15 comparisons. 


#' #### Tukey post-hoc test
#' 
#' By default, the `TukeyHSD()` function performs pairwise comparisons for every term in the ANOVA model. Since we had a significant interaction term, we will only print out the pairwise comparisons of that interaction.

#+ tukey2
adm_aov2_Tukey <- TukeyHSD(adm_aov2)

adm_aov2_Tukey$`Research:Discipline`


#' ### ANCOVA
#' 
#' ANCOVA (Analysis of Covariance) is a statistical technique used to analyze the relationship between a continuous dependent variable and one or more independent variables, while controlling for the effects of one or more covariates (variables that may be related to both the dependent variable and the independent variable(s)). ANCOVA tests whether the groups defined by the independent variable(s) differ significantly on the dependent variable, while controlling for the effects of the covariate(s). If a significant effect is found in ANCOVA, post-hoc tests can be used to determine which specific groups differ from each other, while controlling for the effects of the covariate(s).

#+ ancova
adm_anc <- lm(CGPA ~ Research + GRE, data = adm_df)

anova(adm_anc)

#' ### MANOVA
#' 
#' MANOVA (Multivariate Analysis of Variance) is a statistical technique used to analyze the relationships between multiple dependent variables and one or more independent variables. It tests whether the mean values of the dependent variables are significantly different across groups defined by the independent variables. When a significant overall effect is found in MANOVA, it indicates that at least one of the dependent variables has a different mean value across groups.

#+ manova
adm_man <- manova(cbind(GRE, TOEFL, CGPA) ~ Discipline, data = adm_df)

anova(adm_man)

#' #### ANOVA as a post-hoc test
#' 
#' To determine which dependent variables are contributing to the overall significant effect, multiple ANOVAs can be conducted on each dependent variable separately. This approach is useful when there are many dependent variables and it is not clear which variables are driving the overall effect. By analyzing each variable separately, it is possible to identify the specific variables that are contributing to the overall effect.
#' 
#' However, there are some limitations to this approach. Unlike with Tukey's post-hoc test above that corrects for multiple testing, conducting multiple ANOVAs can increase the overall Type I error rate, so it is important to adjust the significance level accordingly (e.g., using Bonferroni correction). In addition, this approach does not account for the correlations between the dependent variables, so it may not provide a complete understanding of the relationships between the variables.

#+ anova-post
anova(lm(GRE ~ Discipline, data = adm_df))
anova(lm(TOEFL ~ Discipline, data = adm_df))
anova(lm(CGPA ~ Discipline, data = adm_df))

#' Bonferroni's adjustment for multiplicity is fairly straight-forward.  We simply divide our chosen &alpha; value (0.05) by the number of comparisons to be made (3), then use that number as our threshold for statistical significance.

#+ bonferroni
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

#+ kruskal
kruskal.test(GRE ~ Discipline, data = adm_df[adm_df$Admit == 'Accepted', ])

#' With an extremely small p-value, we can conclude that the distribution of GRE scores is different in at least one of the three discplines. However, like with the ANOVA test we are not provided information on which of those groups are different from the others. Therefore, we will need to employ a post-hoc test.
#' 
#' 
#' #### Dunn's post-hoc test
#' 
#' Dunn's test is a post-hoc test used to compare pairs of groups in a Kruskal-Wallis test. It involves calculating all possible pairwise comparisons of groups and adjusting the p-values using the Bonferroni correction. Dunn's test is a non-parametric alternative to Tukey's HSD and pairwise t-tests and is particularly useful when the data do not meet the assumptions of parametric tests.
#' 
#' To perform a Dunn's test in `R` we can employ the `dunnTest()` function from the `FSA` library and pass the same model formula and data arguments as we did in the `kruskal.test()` function above.

#+ dunn
library(FSA)

dunnTest(GRE ~ Discipline, data = adm_df[adm_df$Admit == 'Accepted', ])

#' From the tests we can conclude that each of the three *Discipline* groups have significantly different *GRE* distributions.
#' 
#' 
#' ### Non-parametric tests for 2 or more variables.
#' 
#' If we have 2 or more variables that we want to employ in one non-parametric test there are a few options available, however we are likely going to want to use some form of logistic regression. 
#' 
#' 
#' ## Concluding remarks
#' 
#' Multiple group tests are important in analyzing differences between three or more independent groups. In this section, we have covered parametric tests including ANOVA, ANCOVA, and MANOVA, the non-parametric Kruskal-Wallis test, and post-hoc tests. Understanding the assumptions, advantages, and limitations of each test is crucial for choosing the appropriate test for the specific analysis. By using these tests and post-hoc methods effectively, you can gain valuable insights into the differences between multiple groups in your data.