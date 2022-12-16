library(tidyverse)

# Exploratory Data Analysis ------------------------------------------------------------------------

### https://www.kaggle.com/datasets/akshaydattatraykhare/data-for-admission-in-the-university
adm_df <- read_csv("data/raw/adm_data.csv")

head(adm_df)
summary(adm_df)

## Create a basic histogram of GRE scores
hist(adm_df$GRE)

## Use ggplot to create histogram
ggplot(adm_df, aes(x = GRE)) +
  geom_histogram()

