# 1) Calculate the volume of a cube with length = 4.2.
# Hint: expected answer is 74.088


# answer ------
4.2 ** 3


# 2) Given the two vectors below, what elements are equal?
# Hint: expected answer is TRUE TRUE FALSE

v1 <- c(1, 2, 3)
v2 <- c('1', '2', 'three')


# answer ------
v1 == v2


# 3) With the following vector, v3, use a loop structure to print "TRUE" if the number is even and "FALSE" if it is odd.
# Hint: expected answer is FALSE TRUE FALSE TRUE FALSE

v3 <- c(1, 2, 3, 4, 5)


# answer --------
for (i in v3) {
  if (i %% 2 == 0) {
    print(TRUE)
  } else {
    print(FALSE)
  }
}


# 4) Write a function that takes the following two arguments, x and y, and calculates their inverse sum.
# Hint: expected answer is 0.2909091.

x <- 5
y <- 11


# answer ------
my_fun <- function(x, y) {
  z <- 1 / x + 1 / y
  return(z)
}

my_fun(x, y)


# 5a) Load in the "cars.csv" file from the "data/raw" directory.


#answer ------
df <- read.csv("data/raw/cars.csv")


# 5b) Rename the first column to "Car"


#answer ------
colnames(df)[1] <- "Car"


# 5c) Filter for cars with at least 100 horsepower (hp) and over 20 miles per gallon (mpg)


# answer ------
df[df$hp >= 100 & df$mpg > 20, ]

# or
library(dplyr)

df %>%
  filter(hp >= 100 & mpg > 20)
