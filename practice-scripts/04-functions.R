### -----------------------------
## Writing functions
### -----------------------------


## preparations -------------------

library(tidyverse)


## introduction ------------------------

# R is a functional programming language, i.e. it provides many tools for the creation and manipulation of functions
# you can do virtually anything with functions: assign them to variables, store them in lists, pass them as arguments to other functions, and much more
# very helpful in obeying the "don't repeat yourself" a.k.a. DRY principle
# on functions in R:
  # objects of their own
  # you can work with them as with any other type of object
  # functions have three parts:
      # the body(), the code inside the function
      # the formals(), the list of arguments which controls how you can call a function
      # the environment(), the "map" of the location of the function's variables
  # when you print a function in R, all these components are shown

f <- function(x) x^2
f
formals(f)
body(f)
environment(f)

# not so easy for pre-built S3 generics...

str
getAnywhere(str)
getAnywhere(str)[1]
methods(str)
getAnywhere(str.default)

# another exception: primitive functions, such as sum(), call C code directly with .Primitive() and contain no R code



## refresher: how to create functions ------------

# function that returns the mean of a vector
my_mean <- function(my_vector) {
  mean <- sum(my_vector)/length(my_vector) 
  mean
}
my_mean(c(1, 2, 3))
my_mean


# another function that finds the remainder after division ("modulo operation")
remainder <- function(num = 10, divisor = 4) {
  remain <- num %% divisor
  remain
}
remainder()
args(remainder)


# pass functions as arguments
evaluate <- function(func, dat){
  res <- func(dat)
  return(res)
}

evaluate(sum, c(2, 4, 6))
evaluate(function(x){x+1}, 6) # works with "anonymous functions", too (i.e. functions that are defined on-the fly without being given a name of their own)


# implement conditions
has_name <- function(x) {
  nms <- names(x)
  if (is.null(nms)) {
    rep(FALSE, length(x))
  } else {
    !is.na(nms) & nms != ""
  }
}
has_name(c(1, 2, 3))
has_name(mtcars)


# stopifnot() to ensure truth of R expressions
wt_mean <- function(x, w, na.rm = FALSE) {
  stopifnot(is.logical(na.rm), length(na.rm) == 1)
  stopifnot(length(x) == length(w))
  
  if (na.rm) {
    miss <- is.na(x) | is.na(w)
    x <- x[!miss]
    w <- w[!miss]
  }
  sum(w * x) / sum(x)
}
wt_mean(1:6, 5:1, na.rm = FALSE)

# stop with better error messages
wt_mean <- function(x, w, na.rm = FALSE) {
  stopifnot(is.logical(na.rm), length(na.rm) == 1)
  if(length(x) != length(w)) {
    stop("The length of x, the numeric input vector, is not equal to the length of w, the weights vector.")
  }
  if (na.rm) {
    miss <- is.na(x) | is.na(w)
    x <- x[!miss]
    w <- w[!miss]
  }
  sum(w * x) / sum(x)
}
wt_mean(1:6, 5:1, na.rm = FALSE)




## when to use functions: example ------------

# Generate a sample dataset 
set.seed(1014) 
df <- data.frame(replicate(6, sample(c(1:5, -99), 6, rep = TRUE))) 
names(df) <- letters[1:6] 
df

# how to replace -99 with NA?
df$a[df$a == -99] <- NA
df$b[df$b == -99] <- NA
df$c[df$c == -98] <- NA
df$d[df$d == -99] <- NA
df$e[df$e == -99] <- NA
df$f[df$g == -99] <- NA

fix_missing <- function(x) { 
  x[x == -99] <- NA
  x 
}
# lapply is called a "functional" because it takes a function as an argument
df[] <- lapply(df, fix_missing) # the square brackets in df[] are little trick we use to make sure we get back a data frame, not a list

# easy to generalize to a subset of columns
df[1:3] <- lapply(df[1:3], fix_missing)
df

# what if different codes for missing values are used?
fix_missing_99 <- function(x) { 
  x[x == -99] <- NA
  x 
}

fix_missing_999 <- function(x) { 
  x[x == -999] <- NA
  x 
}

# NOOO! Instead:
missing_fixer <- function(x, na.value) { 
  x[x == na.value] <- NA
  x
}

# applying multiple functions
summary_ext <- function(x) { 
  c(mean(x, na.rm = TRUE), 
    median(x, na.rm = TRUE), 
    sd(x, na.rm = TRUE), 
    mad(x, na.rm = TRUE), 
    IQR(x, na.rm = TRUE)) 
}
lapply(df, summary_ext)

# better: store functions in lists
summary_ext <- function(x) { 
  funs <- c(mean, median, sd, mad, IQR)
  lapply(funs, function(f) f(x, na.rm = TRUE)) 
}
sapply(df, summary_ext)

# using anonymous functions
sapply(mtcars, function(x) length(unique(x)))

# final note: dplyr comes with many convenience functions that let you apply functions across columns or rows very easily



######################
### IT'S YOUR SHOT ###
######################

# 1. program a function ultimateAnswer() that always returns the number 42!

# 2. program a function normalize() that produces normalizes a numeric vector x to mean(x) = 0 and sd(x) = 1!

# 3. Use sapply() and an anonymous function to find the coefficient of variation for all variables in the mtcars dataset!

# 4. Use integrate and an anonymous function to find the area under the curve for the following functions:
# a) y = x ^ 2 - x, x in [0, 10]
# b) y = sin(x) + cos(x), x in [-pi, pi]




## THINGS GET A BIT MORE COMPLICATED FROM HERE ON. TREAT THE FOLLOWING AS AN OUTLOOK FOR THE INTERESTED AND AN OPTIONAL REFERENCE.


## principles of scoping ------------------------

# scoping is the set of rules that govern how R looks up the value of a symbol. example:
x <- 10
x
# "how does R proceed to figure out that x represents 10?"
# understanding how scoping works is useful when you compose functions
# R has two types of scoping: lexical and dynamic
# lexical scoping looks up symbol values based on how functions were nested when they were created (not how they are nested when they are called)

# R starts with looking inside a function's definition to look up symbol values
f <- function() {
  x <- 1 
  y <- 2 
  c(x, y) 
}
f()
rm(f)

# if a name isn't defined inside a function, R will look one level up:
x <- 2 
g <- function() { 
  y <- 1
  c(x, y) 
}
g()
rm(x, g)

# same applies if a function is defined inside another function:
x <- 1 
h <- function() { 
  y <- 2
  i <- function() { 
    z <- 3 
    c(x, y, z)
  }
i()
} 
h() 
i()
rm(x, h)

# something magical with closures ("functions created by other functions"):
j <- function(x) {
  y <- 2 
  function() {
    c(x, y) 
  } 
}
k <- j(1)
k()
k(3)
rm(k, j)


# by default, every time a function is called, a new environment is created to host execution. each invocation is completely independent:
j <- function() { 
  if (!exists("a")) {
    a <- 1 
  } else {
    a <- a + 1 
  } 
  print(a) 
} 
j()
a <- 4
j()
rm(a, j)


# dynamic lookup: R looks for values when the function is run, not when it's created
x <- 10
f <- function() x
f()
x <- 15
f()
x <- 20
f()
# undesirable behavior because the function is no longer self-contained
# to detect this problem, call findGlobals() from codetools package:
# lists all external dependencies of a function

f <- function() x + 1
codetools::findGlobals(f)

f <- function(x) sqrt(x)
codetools::findGlobals(f)


## every operation is a function call -----------

# "To understand computations in R, two slogans are helpful:
  # Everything that exists is an object.
  # Everything that happens is a function call.
# -- John Chambers

x <- 10
y <- 5
x + y
`+`(x, y)

for (i in 1:2) print(i)
`for`(i, 1:2, print(i))

add <- function(x, y) x + y
sapply(1:10, add, 3)

sapply(1:5, `+`, 3)

x <- list(1:3, 4:9, 10:12)
sapply(x, "[", 2)



## doing things with arguments -----------

# how to call a function given a list of arguments:
args <- list(1:10, na.rm = TRUE)
do.call(mean, args)

# since arguments in R are evaluated lazily, the default value can be defined in terms of other arguments:
g <- function(a = 1, b = a * 2) {
  c(a, b) 
}
g()
g(10)
g(10, 30)

# you can determine if an argument was supplied or not with the missing() function
i <- function(a, b) {
  c(missing(a), missing(b)) 
} 
i()
i(a = 1)

# R function arguments are lazy---they're only evaluated if they're actually used:
f <- function(x) {
  10 
} 
f(stop("This is an error!"))

# if you want to ensure that an argument is evaluated, you can use force():

f <- function(x) {
  force(x) 
  10 
} 
f(stop("This is an error!"))




## infix functions -----------

# most functions are "prefix" operators, i.e. the name comes before the arguments
# with infix functions, the function name comes in between its arguments, e.g., + or - or $
# user-generated infix functions must start and end with %, such as %% or %in%

# create your own infix function like this (using backticks):
`%+%` <- function(a, b) paste(a, b, sep = "") 
"new" %+% " string"

# useful application: Wickham's implementation of xor ("||") operator:
`%||%` <- function(a, b) if (!is.null(a)) a else b
function_that_might_return_null() %||% 3
NULL %||% 3


## return values --------------

# functions can only return a single object
# the last expression evaluated in a function becomes the return value:
f <- function(x) {
  if (x < 10) {
    0
  } else {
    10
  }
}
f(1)

# you can make the return of a value explicit with return():
f <- function(x) {
  foo <- mean(x)
  return(foo)
}

# can be useful to improve readability of code, as in:
f <- function(x, y) {
  if (!x) return(y)
  
  # complicated processing here
}


## invisible output --------------

# example
show_missings <- function(df) {
  n <- sum(is.na(df))
  cat("Missing values: ", n, "\n", sep = "")
  invisible(df)
}
show_missings(mtcars)
x <- show_missings(mtcars) 
class(x)
dim(x)



######################
### IT'S YOUR SHOT ###
######################

# 1. try to inspect the source code of the summary function when applied to a data.frame object.

# 2. the following code makes a list of all functions in the base package. use it to answer the following questions: 
  # a) which base function has the most arguments?
  # b) how many base functions have no arguments? what's special about them?
objs <- mget(ls("package:base"), inherits = TRUE)
funs <- Filter(is.function, objs)

# 3. what does the following code return? why? what does each of the three c's mean?
c <- 10
c(c = c)

# 4. what does the following function return? make a prediction before running the code yourself.
f <- function(x) { 
  f <- function(x) { 
    f <- function(x) {
      x ^ 2
    } 
    f(x) + 1
  } 
  f(x) * 2
} 
f(10)

# 5. create infix versions of the set functions intersect(), union(), and setdiff().

