# Inferential Statistics ---------------------------------------------------------------------------
#
# @author Tyler B. Garner tbgarner5023@gmail.com
# @author Jennifer Valcin jpv5319@psu.edu

library(dplyr)

adm_df <- read.csv("data/raw/adm_data.csv")

# Re-code variables as factors
adm_df$Research <- factor(adm_df$Research)
adm_df$Discipline <- factor(adm_df$Discipline)
adm_df$Admit <- factor(adm_df$Admit)

head(adm_df)

# Tests of distributions -----------

# * Shapiro-wilks test ---------

# * Homoskedacity test -------

# * Box-M test --------------


# Fisher's exact test -------

# Chi-square Test -----

# Does undergraduate research experience influence grad school admittance?
# Or, are undergraduate research experience and graduate school admittance independent?

chisq.test(adm_df$Research, adm_df$Admit)


# T-test ------

# * One-sample --------

# * Unpaired --------

# Assumtion for t-test = equal variances

# * Paired ---------

# Is the mean CGPA different between students who did and did not do undergraduate research?

t.test(CGPA ~ Research, data = adm_df)

# Non-parametric test --------

# * Wilcox-test ----------


# * Mann-Whitney U-test ----------


# ?