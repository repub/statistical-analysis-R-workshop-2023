# Basics of R and RStudio --------------------------------------------------------------------------
# @author Tyler B. Garner
# @author Jennifer Valcin

# Basics -----

# * Math Functions -----
#'
#' Use `+` and `-` for addition and subtraction, `*` and `/` for multiplication and division.

1 + 1
3 - 1
1 * 2
4 / 2


#' Use `%/%` integer division (without the remainder) and `%%` to return the remainder from the
#' division of two numbers.

5 %/% 2
5 %% 2


#' Exponentials can be calculated with `^` or `**`.

2^3
2**3


# * Strings -----
#'
#' Strings can be writtin in either single or double quotes.

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


# * Vectors and lists -----

c(1, 2, 3)
list(1, 2, 3)


# * Logic Operators -----
#'
#' In R, logical statements can be defined as either `TRUE` or `FALSe`.
#'
#' The `==` operator will return a logical result on whether two items are identical.

v1 = c(1, 2, 3)
v2 = c(0, 2, 3)

v1 == v2


#' The `&` and `|` operators represent "and" and "or".

TRUE & FALSE

TRUE | FALSE


# Install and load packages -----
#'
#' Libraries can be installed and loaded to add extra functionality in R.
#' 
#' * `install.packages()` installs a package from CRAN given the package name as a string. Note
#'   that you only need to install a package once.
#' * `library()` loads an already-installed library into the current R environment. Note that you
#'   will need to load the library any time your re-load or start a new R environment.

install.packages('tidyverse')

library(tidyverse)


# Load a file ------
#'
#'

df <- read_csv("data/raw/adm_data.csv")

# Piping -----
#'
#' Tidyverse uses a pipe function, `%>%`, that "pipes" an object into the first argument of a
#' function (the `data` argument is standard in tidy format).  This can be applied multiple times
#' to perform multiple tasks.
#' 
#' For example, we will use the `head()` function with and without piping to get the first few
#' lines of the data set.

head(df)

df %>% head()


# Select columns and rows -----
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


# Long vs Wide format -----
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