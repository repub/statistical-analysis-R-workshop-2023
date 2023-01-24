# Exploratory Data Analysis ------------------------------------------------------------------------
#
# @author Tyler B. Garner tbgarner5023@gmail.com
# @author Jennifer Valcin jpv5319@psu.edu

library(dplyr)

### modified from https://www.kaggle.com/datasets/akshaydattatraykhare/data-for-admission-in-the-university

adm_df <- read_csv("data/raw/adm_data.csv")

# Check for NA and duplicates -----

# * Descriptive Statistics -----

head(adm_df)
summary(adm_df)


# Re-code variables as factors

adm_df$Research <- factor(adm_df$Research)
adm_df$Discipline <- factor(adm_df$Discipline)
adm_df$Admit <- factor(adm_df$Admit)


#' Printing specific summary statistics
#' 
#' The `summarise()` function in Tidyverse is a powerful tool that, when combined with other
#' functions, provide details about the data set.  For example, we can print the means and standard
#' deviations of GRE scores separately for accepted and rejected applicants.
#' 
#' * `group_by()` groups each unique level in the Admit column (Accepted or Rejected).
#' * `summarize()` creates two new columns that will will name mean_GRE and sd_GRE with the means
#'   and standard deviations of GRE scores as given by the `mean()` and `sd()` functions.

adm_df %>%
  group_by(Admit) %>%
  summarize(mean_GRE = mean(GRE),
            sd_GRE = sd(GRE))


# * Assessing Distributions -----
# * * Histograms -----
# Observe the distributions of GPA and exam scores

# Create a basic histogram of GRE scores

hist(adm_df$GRE)


# Use ggplot() to create histogram

ggplot(adm_df,
       aes(x = GRE)) +
  geom_histogram()


# Group GRE scores by applicants who did or did not perform undergraduate research
# First create a ggplot object

p <- ggplot(adm_df,
            aes(x = GRE,
                fill = Research))


# Plot histogram (the columns will be stacked by default)
p + geom_histogram()


# Un-stack the histogram and add transparency

p + geom_histogram(position = 'identity',
                   alpha = 0.5)


# Graph multiple histograms in one plot

adm_df %>%
  pivot_longer(c(CGPA, GRE, TOEFL)) %>%
  ggplot(aes(x = value,
             fill = Research)) +
    geom_histogram(position = 'identity',
                   alpha = 0.5) +
    facet_wrap(~ name,
               scales = 'free')


# From the histograms, it appears students who have undergraduate research experience tend to have
# higher cumulative GPAs and score higher on the GRE and TOEFL exams.

# * * Boxplots -----
# Does the Statement of Purpose have a relationship with admittance?

ggplot(adm_df,
       aes(x = Admit,
           y = SOP)) +
  geom_boxplot()


# * Assessing Relationships -----
# * * Scatterplots -----
# Are there relationships between cumulative GPA and test scores?

library(GGally)

adm_df %>%
  select(c(CGPA, GRE, TOEFL)) %>%
  ggpairs()


# * * Correlations -----
# Are there correlations among the variables?

#' First mutate all of the variables into the numeric type, then use the cor() function to get the
#' Spearman-rank correlations between each variable.
#' 
adm_cor <- adm_df %>%
  mutate(across(everything(), as.numeric)) %>%
  cor(method = 'spearman')

library(corrplot)

corrplot(adm_cor)


#' Why do so many variables have negative relationships with admittance? Because when we changed
#' the Admit variable to numeric it was recoded so that Accepted is 1 and Rejected is 2.



# * Normality Tests -----

shapiro.test(adm_df$GRE)


# Multiple tests with rstatix

library(rstatix)

adm_df %>%
  shapiro_test(GRE, TOEFL, SOP, LOR, CGPA) %>%
  adjust_pvalue() %>%
  add_significance()


# Multivariate normality test

adm_df %>%
  select(GRE, TOEFL, SOP, LOR, CGPA) %>%
  mshapiro_test()


# Feature engineering/transformation ? ------


# Box-Cox transformations ? ----------
# Pros vs Cons - potentially a better model fit and prediction accuracy - loss of interpretability

saveRDS(adm_df, 'EDA-and-visualizations/data/interim/adm_df')
