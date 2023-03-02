# 1) Calculate the volume of a cube with length = 4.2.

# answer ------
4.2 ** 3

# 2) Given the two vectors below, what elements are equal?

v1 <- c(1, 2, 3)
v2 <- c('1', '2', 'three')

# answer ------
v1 == v2

# 3) With the following vector, v3, use a loop structure to print "TRUE" if the number is even and "FALSE" if it is odd.

v3 <- c(1, 2, 3, 4, 5)

# answer --------
for (i in v3) {
  if (i %% 2 == 0) {
    print(TRUE)
  } else {
    print(FALSE)
  }
}


# 4) Write a function that takes two arguments, x and y, and calculates their inverse sum.
# Hint: expected answer is 0.2909091.

x <- 5
y <- 11

___

# answer ------
my_fun <- function(x, y) {
  z <- 1 / x + 1 / y
  return(z)
}

my_fun(x, y)
