# Basics of R and RStudio --------------------------------------------------------------------------
#
# @author Tyler B. Garner tbgarner5023@gmail.com
# @author Jennifer Valcin jpv5319@psu.edu

# Basics -----

# * Math Functions -----
#'
#' Use `+` and `-` for addition and subtraction, `*` and `/` for multiplication and division.

1 + 1
3 - 1
1 * 2
4 / 2


#' Use `%/%` integer division (result without the remainder) and `%%` to return the remainder from
#' the division of two numbers.

5 %/% 2
5 %% 2


#' Exponentials can be calculated with `^` or `**`.

2^3
2**3


# * Strings -----
#'
#' Strings can be written in either single or double quotes.

'Hello'
"Hello"


# * Objects -----
#' We can assign values to "objects" simply by using the `<-` operator.  While `=` can also be used,
#' in R it is best practice to use `<-` for assigning objects.  We can then use functions on those
#' objects.  For example, we can assign the strings "Hello" and "World" to `h` and `w`, then use
#' the `paste()` function to paste the strings together, with a space between them as default.

h <- "Hello"
w <- "World"

paste(h, w)


# * Vectors and lists
#'
#' We can combine multiple values of the same type into a vector using `c()`.  We could also
#' combine values of any time into a list with `list()`.  Lists are recursive, meaning you can have
#' lists within lists.

c(1, 2, 3)
list(1, 2, '3')
list(1, 2, list('3', '4'))


# * Logic Operators -----
#'
#' In R, logical statements can be defined as either `TRUE` or `FALSE`.
#'
#' The `==` operator will return a logical result on whether two items are identical. For example,
#' we can check whether each item in two vectors are the same.

v1 = c(1, 2, 3)
v2 = c(0, 2, 3)

v1 == v2


#' If we wanted to know if the two vectors are identical we can add the `all()` function.

all(v1 == v2)


#' Multiple conditions can be assessed logically using the and (`&`) and or (`|`) operators.  

TRUE & FALSE

TRUE | FALSE

TRUE & TRUE | FALSE


# * Install and load packages -----
#'
#' Libraries can be installed and loaded to add extra functionality in R.
#' 
#' * `install.packages()` installs a package from CRAN given the package name as a string. Note
#'   that you only need to install a package once.
#' * `library()` loads an already-installed library into the current R environment. Note that you
#'   will need to load the library any time your re-load or start a new R environment.

install.packages('dplyr')

library(dplyr)


# * Load a file ------
#'
#'

df <- read.csv("data/raw/adm_data.csv")

# * Data Frames ------
#' The fundamental data structure in most of R is the data frame, which is a two-dimensional
#' structure where each column contains values of one variable and each row contains one set of
#' values from each column.  Data frames are referenced first by their row, then their column.
#' Typically, we will load in data frames from existing data, but if we wanted to write our own
#' data frame we can use the `data.frame()` function.
#' 
#' For example, the `df` object we loaded in above is already a data frame by default. With the
#' `dim()` and `str()` functions we can return the dimensions and structure of the data frame.

dim(df)
str(df)


# * Piping -----
#'
#' Tidyverse uses a pipe function, `%>%`, that "pipes" an object into the first argument of a
#' function (the `data` argument is standard in tidy format).  This can be applied multiple times
#' to perform multiple tasks.
#' 
#' For example, we will use the `head()` function with and without piping to get the first few
#' lines of the data set.

head(df)

df %>% head()


# * Select columns and rows -----
#'
#' In base R, the `$` operator can be used to select a column.  Alternatively, you can call a column
#' or columns by their index or name in brackets `[]`.

df$Admit

df %>% select(Admit)


#' To select rows, you can index the rows you want to select before a comma within brackets.  We can
#' use a colon to select everything within the range of two numbers.  For example, to select the 5th
#' to 10th rows:

df[5:10, ]


#' Rows can also be selected on conditions.  In base R, this can be done within brackets.  With the
#' tidyverse, this can be done with the `filter()` function.

df[df$Admit == "Accepted", ]

df %>% filter(Admit == "Accepted")


# * Long vs Wide format -----
#'
#' In short, wide format is more human-friendly, in that it is easier for humans to input and
#' understand data in this format.  Long format, however, is more computer-friendly and is
#' therefore needed for some functions.
#' 
#' Currently, the data set is in wide format, where each row represents a single observation.  In
#' long format, each row represents some subset of an observation.  For example, we can lengthen the
#' Research, Admit, and Discipline columns using `pivot_longer()` from Tidyverse.

df %>%
  pivot_longer(c(Research, Admit, Discipline))

#' Now we see that there are two new columns with the variable names and their values.