#' ---
#' title: "`R` Basics"
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

#+ setup, include = FALSE
knitr::opts_knit$set(root.dir = '../')


#' ## Introduction
#' 
#' `R` is a popular programming language and environment for statistical computing and data analysis. It provides a powerful set of tools for working with data, including data manipulation, visualization, and statistical modeling. `R` is widely used in academia, industry, and government, and is an essential skill for many data science and analytics roles. This section will provide an introduction to the basics of `R`, including basic operations, data types, variables, functions, control structures, and packages. Whether you're new to `R` or looking to refresh your skills, this tutorial will help you get started with the fundamentals of this powerful language.
#' 
#'
#' ## Mathematical operations 
#'
#' `R` provides many built-in functions for performing basic mathematical operations. For example, we can use `+` and `-` for addition and subtraction, `*` and `/` for multiplication and division.

#+ math
1 + 1
3 - 1
1 * 2
4 / 2

#' The `%/%` operator can be used for integer division (result without the remainder) while `%%` to returns the remainder from the division of two numbers.

#+ divide
5 %/% 2
5 %% 2

#' Exponentials can be calculated with either `^` or `**`.

#+ expo
2^3
2**3


#' ## Strings 
#'
#' In `R`, a string is a sequence of characters enclosed in single or double quotes. Strings are commonly used to represent text data in `R`, such as names, addresses, or other text-based information.

#+ string
'Hello'
"World"

#' Strings can also be joined together using the `paste()` function, which by default adds a space between the strings.

#+ paste
paste('Hello', "World")


#' ## Logic operations
#' 
#' Logical operators in R are used to evaluate logical expressions, which are expressions that return either `TRUE` or `FALSE`.  Common logic operators are:
#' 
#' - `==` - tests whether two values are equal.
#' - `!=` - tests whether two values are not equal.
#' - `<` and `>` - tests whether one value is less than or greater than the other.
#' - `<=` and `>=` - the same as above but includes equal values.
#' 
#' For example, we can test if `2` is equal to `1` with the `==` operator:

#+ equal
2 == 1

#' In addition to these main operators, there are also compound logical operators that allow you to combine logical expressions. The main compound logical operators in `R` are:
#' 
#' - `&` - tests whether two logical expressions are both true.
#' - `|` - tests whether at least one of two logical expressions is true.
#' - `!` - negates a logical expression.
#' 
#' These logical operators are commonly used in control structures and data manipulation in R, allowing you to make decisions based on logical conditions or to filter data based on logical expressions.
#' 
#' For example, we can test if `2` is equal to `1` or if `2` is equal to `2` with the `==` and `|` operators:

#+ or
2 == 1 | 2 == 2


#' ## Variables
#' 
#' Variables in `R` are used to store values that you can use later in your code. To create a variable, you can use the `<-` or `=` operators, which assigns a value to a name. For example, `x <- 5` assigns the value 5 to the variable x.

#+ var
x <- 5

x

#' Once you create a variable, you can use it in your code by referring to its name. For example, if you created a variable x with the value 5, you could use it in a calculation like `y <- x + 3`, which would assign the value 8 to the variable y.

#+ var2
y <- x + 3

y

#' Variables in `R` can store different types of data, such as numbers, characters, logical values, and more. Some common data types in `R` include:
#' 
#' - **Numeric** - numbers with or without decimal places, such as 1, 2.5, or -3.14.
#' - **Character** - sequences of characters, such as "Hello, world!" or "R is fun".
#' - **Factor** - categorical data that can be ordered or unordered.
#' - **Logical**- values that represent truth or falsehood, such as TRUE or FALSE.
#' - **Integer**- whole numbers, such as 1, 2, or -3.
#' - **Complex**- numbers with real and imaginary components, such as 1 + 2i.
#' 
#' In `R`, variable names can contain letters, numbers, periods, and underscores, but cannot begin with a number. Variable names are case sensitive, which means that `x`, `X`, and `x1` are all different variables.
#' 
#' 
#' ## Data structures
#' 
#' Data structures in `R` are essential for organizing and manipulating data. `R` provides several data structures, including vectors, lists, matrices, arrays, and data frames, each with its own set of functions and properties. Understanding these data structures and how to use them is critical for effective data analysis and programming in `R`.
#' 
#' 
#' ### Vectors and lists
#'
#' We can combine multiple values of the same type into a vector using `c()`.  We could also combine values of any time into a list with `list()`.  Lists are recursive, meaning you can have lists within lists.

#+ vec-list
c(1, 2, 3)
list(1, 2, '3')
list(1, 2, list('3', '4'))


#' ### Matrices
#' 
#' A matrix in `R` is a two-dimensional array that contains elements of the same data type. You can create a matrix using the `matrix()` function, which takes a vector of values as input and arranges them into a specified number of rows and columns. For example, `m <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, ncol = 3)` creates a 2-by-3 matrix with the values 1, 2, 3, 4, 5, and 6.

#+ mat
m <- matrix(c(1, 2, 3, 4, 5, 6),
            nrow = 2,
            ncol = 3)

m


#' ### Arrays
#' 
#' An array in `R` is a multi-dimensional extension of a matrix that can have any number of dimensions. You can create an array using the `array()` function, which takes a vector of values and a vector of dimension sizes as input. For example, `a <- array(c(1, 2, 3, 4, 5, 6), dim = c(2, 3, 2))` creates a 2-by-3-by-2 array with the values 1, 2, 3, 4, 5, and 6.

#+ array
a <- array(c(1, 2, 3, 4, 5, 6),
           dim = c(2, 3, 2))

a

#' ### Data frames
#' 
#' A data frame in `R` is a tabular data structure that is similar to a spreadsheet. It is a list of equal-length vectors, where each vector represents a column in the data frame. You can create a data frame using the `data.frame()` function, which takes vectors or variables as input. For example, `df <- data.frame(name=c("Alice", "Bob", "Charlie"), age=c(25, 30, 35), weight=c(60, 70, 80))` creates a data frame with three columns: "name", "age", and "weight".

#+ df
df <- data.frame(name = c("Alice", "Bob", "Charlie"),
           age = c(25, 30, 35),
           weight = c(60, 70, 80))

df

#' Data frames are commonly used for data analysis and manipulation in `R`, as they allow you to perform operations on entire columns of data. You can access specific columns using the `$` operator, for example `df$age` will return the age column of the data frame df. You can also subset a data frame using logical operators or indexing. For example, `df[df$age > 30, ]` will return all rows of `df` where the age column is greater than 30.

#+ index
df$age

df[df$age > 30, ]


#' ## Functions
#' 
#' A function in `R` is a block of code that performs a specific task. Functions take input values, called arguments, and return output values.  `R` provides a large number of built-in functions for performing operations on data. For example, the `mean()` function computes the mean of a vector, and likewise the `sum()` function the sum. To use a function, you simply call it by name and pass in the required arguments.

#+ mean-sum
v <- c(1, 2, 3, 4, 5)

mean(v)

sum(v)

#' You can also create your own functions using the `function()` keyword. In the example below, we write a function that takes an argument `x` and returns the sum of the squared values in `x`.

#+ function
squared_sums <- function(x) {
  ss <- sum(x ** 2)
  return(ss)
}

squared_sums(v)


#' ## Plotting
#' 
#' One of the major advantages of `R` over other coding languages is its inherent plotting functions. For instance, we can use the `plot()` function with two vectors to plot a scatterplot. In the next example, we will use another one of `R`'s advantages, functions for random number generation, to create two vectors and plot them.

#+ plot
v1 <- rnorm(100)
v2 <- rnorm(100)

plot(v1, v2)


#' ## Control structures
#' 
#' Control structures are statements that allow you to control the flow of your code. They allow you to perform different actions depending on whether a condition is true or false, or to repeat a block of code multiple times. The main control structures in R are:
#' 
#' ### `if` statements
#' 
#' If statements allow you to execute a block of code if a certain condition is true. For example, the following code checks if a variable x is greater than 10, and prints a message if it is.

#+ if
x <- 12

if (x > 10) {
  print("x is greater than 10")
}


#' ### `if`-`else` statements
#' 
#' If-else statements extend the `if` statement by allowing you to execute one block of code if a condition is true, and a different block of code if it is false. For example, the following code checks if a variable x is greater than 10, and prints a message if it is, or a different message if it is not.

#+ if-else
x <- 12

if (x > 10) {
  print("x is greater than 10")
} else {
  print("x is not greater than 10")
}


#' ### `for` loops
#' 
#' For loops allow you to repeat a block of code a specific number of times.  For example, the following code uses a for loop to sequentially loop through the numbers 1, 2, 3, 4, and 5 and prints the current number at each iteration.

#+ for
for (i in 1:5) {
  print(i)
}


#' ### `while` loops
#' 
#' While loops allow you to repeat a block of code as long as a certain condition is true. For example, the following code uses a while loop to print the numbers 1 to 5. The loop starts at a number specified in `i`, prints that value, then adds 1 to `i`, then goes to the next iteration, stopping when the value of `i` is greater than 5.

#+ while
i <- 1

while (i <= 5) {
  print(i)
  
  i <- i + 1
}

#' ### Apply functions
#' 
#' `R` also has a set of apply functions, which use loops to perform a specific function on data and return a set of values. For example, the `lapply()` function will iterate through a given list and apply some function to it.

#+ lapply
l <- list(c(1, 2, 3), c(4, 5, 6))

l

lapply(l, mean)

#' Other common apply functions are `apply()`, `sapply()`, `tapply()`, and `mapply()`.
#' 
#' 
#' ## Packages
#' 
#' A package in `R` is a collection of functions, data sets, and documentation that extend the capabilities of base `R`. Packages are typically created by other users and developers in the `R` community, and can be installed and loaded into your `R` session to provide additional functionality for specific tasks. 
#' 
#' `R` packages can be installed from a variety of sources, including the Comprehensive `R` Archive Network (CRAN), Bioconductor, GitHub, and others. Most commonly you will be installing packages from CRAN, where you can easily install a package using the `install.packages()` function, which takes the name of the package (as a string) as an argument.  Once a package is installed, you can load it into your `R` session using the `library()` function.
#' 
#' For example, `dplyr` is a package for data manipulation and cleaning, which we will install and load in the following code.

#+ dplyr
install.packages("dplyr")
library(dplyr)

#' With the `dplyr` package loaded we can use its functions.  For example, the `filter()` function is used to select a subset of rows from a data frame based on one or more logical conditions, similarly to how we used the `$` operator previously.  We can use the `filter()` function to return all rows of the `df` object we created above where the age column is greater than 30 by:

#+ dplyr-filter
filter(df, age > 30)


#' ## Piping
#' 
#' Piping is a concept in `R` (and many other programming languages) that allows you to chain together a series of functions, passing the output of one function as the input to the next. This can help make your code more concise and easier to read, especially when working with complex data manipulation tasks.
#' 
#' In `R`, piping is typically done using the `%>%` operator, originally provided in the `magrittr` package that is also within `dplyr`. We can use piping with the example above to "pipe" the `df` dataset into the `filter()` function, then pass the expression to filter on as follows:

#+ pipe-filter
df %>%
  filter(age > 30)

#' By using piping, we are able to perform data manipulation tasks in a more concise and readable way, without having to nest multiple function calls or create intermediate variables. Piping can be a powerful tool for working with complex data manipulation tasks in `R`, and is commonly used in conjunction with other packages like `tidyr` and `ggplot2`.
#' 
#' For example, we can use a series of pipes to first filter for people who are at least 30 years old and select only the column with their names.

df %>%
  filter(age >= 30) %>%
  select(name)

 
#' ## Scripts
#' 
#' In `R`, a script is a file containing a sequence of `R` commands that can be executed together. Scripts are used to automate repetitive tasks, document procedures, and allow others to reproduce the analysis. For example, we can write a script which defines functions that we can then use across different projects without having to recode those functions for each project.
#' 
#' As an example, we have provided an `R` script called "get_libs.R" that has a function of the same name which takes a vector of strings with the names of libraries. The function first checks if the packages are already installed, and if they are not they will install the missing packages. Then, the function will load all of the listed packages into the environment.
#' 
#' In the next code block, we will use the `source()` function to read the script. Then, we will define a vector of libraries and give them to the `get_libs()` function to install and load them. You can open the `R` script located in the "src/scripts/get_libs.R" folder to see how it works.

#+ script
source("src/scripts/get_libs.R")

libs <- c("dplyr", "tidyr", "ggplot2")

get_libs(libs)


#' ## File management
#' 
#' File management is an essential skill for any data analyst or programmer working with large datasets. This includes organizing files in a logical and consistent manner, documenting your files and file structures, and maintaining backups to ensure data integrity and availability. Effective file management can help you work more efficiently and reduce the risk of errors or data loss.
#' 
#' 
#' ### Loading files
#'
#' You can load various types of files into your `R` session, including data files, image files, and code files. To load a data file, such as a .csv file, into `R` you can use the `read.csv()` function. For example, we will use the `read.csv()` function below to read the dataset for this workshop which is in the .csv file format. We will then use the `head()` function to print the first few rows of that dataset.

#+ read-csv
data <- read.csv("data/raw/adm_data.csv")

head(data)


#' ### Saving files
#' 
#' Just as we can load data into `R` we can save it externally into different file formats. To save a data frame as a CSV file in `R`, we can use the `write.csv()` function. This function takes two main arguments: the data frame you want to save and the name of the output file. For example, the following code will save the `df` object we created above into the "data/processed" directory.

#+ save-csv
write.csv(df, "data/processed/basics-df.csv")

#' Keep in mind that it's important to include the file extension in the name of the file when saving it. Additionally, it's generally considered a best practice to avoid overwriting the original raw data when saving your output files, so be sure to choose a new and descriptive name for your output file.
#' 
#' 
#' ## Closing remarks
#' 
#' We hope this guide has been helpful in introducing you to the basics of `R`! `R` is a powerful language with a steep learning curve, but mastering its fundamentals is an essential first step toward becoming proficient in data analysis and statistical modeling. With practice and further study, you can gain a deeper understanding of `R`'s capabilities and begin to apply them to real-world problems. Whether you're a student, researcher, or data analyst, `R` is a valuable tool to have in your toolkit, and I encourage you to continue exploring its many features and possibilities.
#' 
#' In the next section we will employ and expand upon these tools to explore a dataset.