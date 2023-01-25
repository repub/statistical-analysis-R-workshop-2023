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


# Parametric Tests -----
# * One-way ANOVA ------
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

# * * Tukey Post-hoc Test ------
#'
#' We can wrap our ANOVA model in the `TukeyHSD()` function to run the Tukey pairwise comparisons.

TukeyHSD(adm_aov)


# * Two-way ANOVA -----
adm_aov2 <- aov(CGPA ~ Research * Discipline, data = adm_df)
summary(adm_aov2)

# * * Tukey Post-hoc Test ------
TukeyHSD(adm_aov2)


# Non-parametric tests ------
# * Kruskal-Wallis H-test -----

# Are the GRE scores for admitted students different among the 5 disciplines?

kruskal.test(GRE ~ Discipline, data = adm_df[adm_df$Admit == 'Accepted', ])


# Alternatively with tidyverse and rstatix:
library(rstatix)

adm_df %>%
  filter(Admit == 'Accepted') %>%
  kruskal_test(GRE ~ Discipline)

# For Higher-order models use logistic regression.

# * * Dunn's post-hoc -------------

# MANOVA -----

# one-way

# two-way as a exercise