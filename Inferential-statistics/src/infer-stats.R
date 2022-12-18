# Inferential Statistics ---------------------------------------------------------------------------
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


# Chi-square Test -----

# Does undergraduate research experience influence grad school admittance?
# Or, are undergraduate research experience and graduate school admittance independent?

chisq.test(adm_df$Research, adm_df$Admit)


# T-test ------

# Is the mean CGPA different between students who did and did not do undergraduate research?

t.test(CGPA ~ Research, data = adm_df)



