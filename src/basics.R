# Mathematical operations

1 + 1
3 - 1
1 * 2
4 / 2

5 %/% 2
5 %% 2

2^3
2**3


# Strings 

'Hello'
"World"

paste('Hello', "World")


# Logic operations

2 == 1

2 == 1 | 2 == 2


# Variables
x <- 5

x

y <- x + 3

y

# Data structures

c(1, 2, 3)
list(1, 2, '3')
list(1, 2, list('3', '4'))


## Matrices

m <- matrix(c(1, 2, 3, 4, 5, 6),
            nrow = 2,
            ncol = 3)

m


## Arrays

a <- array(c(1, 2, 3, 4, 5, 6),
           dim = c(2, 3, 2))

a

## Data frames

df <- data.frame(name = c("Alice", "Bob", "Charlie"),
                 age = c(25, 30, 35),
                 weight = c(60, 70, 80))

df

df$age

df[df$age > 30, ]


# Functions

v <- c(1, 2, 3, 4, 5)

mean(v)

sum(v)


squared_sums <- function(x) {
  ss <- sum(x ** 2)
  return(ss)
}

squared_sums(v)


# Control structures
## `if` statements

x <- 12

if (x > 10) {
  print("x is greater than 10")
}


## `if`-`else` statements

x <- 12

if (x > 10) {
  print("x is greater than 10")
} else {
  print("x is not greater than 10")
}


## `for` loops

for (i in 1:5) {
  print(i)
}


## `while` loops

i <- 1

while (i <= 5) {
  print(i)
  
  i <- i + 1
}

## Apply functions

l <- list(c(1, 2, 3), c(4, 5, 6))

l

lapply(l, mean)

# Packages

install.packages("dplyr")
library(dplyr)


filter(df, age > 30)


# Piping

df %>%
  filter(age > 30)


# Scripts

source("src/scripts/get_libs.R")

libs <- c("dplyr", "tidyr", "ggplot2")

get_libs(libs)


# File management
## Loading files

data <- read.csv("data/raw/adm_data.csv")

head(data)


## Saving files

write.csv(df, "data/processed/basics-df.csv")

