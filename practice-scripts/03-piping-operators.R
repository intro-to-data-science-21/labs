### -----------------------------
## Piping
### -----------------------------


## peparations -------------------

library(tidyverse)
library(babynames)


## piping with magrittr ----------

# what is piping?
  # structures sequences of data operations as "pipes, i.e. left-to-right (as opposed to from the inside and out)
  # serves the natural way of reading ("do this, then this, then this, ...")
  # avoids nested function calls
  # improves "cognitive performance" of code writers and readers
  # minimizes the need for local variables and function definitions
  # why name "magrittr"?

browseURL("https://upload.wikimedia.org/wikipedia/en/b/b9/MagrittePipe.jpg")


# basic example  -----------------
# [source: https://goo.gl/sWz2DH]

# traditional way of writing code
dat <- babynames 
dim(dat)
dat_filtered <- filter(dat, name == "Kim")
dat_grouped <- group_by(dat_filtered, year, sex)
dat_sum <- summarize(dat_grouped, total = sum(n))
qplot(year, total, color = sex, data = dat_sum, geom = "line") +
  ggtitle('People named "Kim"')

# traditional, even more awkward way of writing code
dat <- summarize(group_by(filter(babynames, name == "Kim"), year, sex), total = sum(n))
qplot(year, total, color = sex, data = dat, geom = "line") +  ggtitle('People named "Kim"')

# magrittr style of piping code
babynames %>%
  filter(name %>% equals("Kim")) %>%
  group_by(year, sex) %>%
  summarize(total = sum(n)) %>%
  qplot(year, total, color = sex, data = ., geom = "line") %>%
  magrittr::add(ggtitle('People named "Kim"')) %>%
  print


# syntax and vocabulary -------------------------
# [source: https://goo.gl/SKnPn7]

# by default, the left-hand side (LHS) will be piped in as the first argument of the function appearing on the right-hand side (RHS)
# %>% may be used in a nested fashion, e.g. it may appear in expressions within arguments. This is used in the mpg to kpl conversion
# when the LHS is needed at a position other than the first, one can use the dot,'.', as placeholder
# whenever only one argument is needed--the LHS--, the parentheses can be omitted


# additional pipe operators ---------------------
# [source: https://goo.gl/SKnPn7]

## "tee" operator %T>%
  # returns the left-hand side value, and not the result of the right-hand side operation
  # useful when a step in a pipeline is used for its side-effect (printing, plotting, logging, etc.)

rnorm(200) %>%
  matrix(ncol = 2) %T>%
  plot %>% # plot usually does not return anything
  colSums

## "exposition" operator %$%
  # exposes the names within the left-hand side object to the right-hand side expression
  # short-hand for using the 'with' functions (and the same left-hand side objects are accepted)
  # handy when functions do not themselves have a data argument, as is the case with, e.g., lm

babynames %>%
  subset(prop > mean(prop)) %$%
  cor(year, prop)

data.frame(z = rnorm(100)) %$% 
  ts.plot(z)

## compound assignment pipe operator %<>%
  # can be used as the first pipe in a chain
  # the result of the pipeline is assigned to the left-hand side object, rather than returning the result as usual
  # makes it more difficult to read, so maybe not use it?

foo <- foo %>% bar %>% baz # equal to...
foo %<>% bar %>% baz

babynames$prop %<>% sqrt



# functions in right-hand sides ("lambda expressions") --------
babynames %>% 
{
  n <- sample(1:10, size = 1)
  H <- head(., n)
  T <- tail(., n)
  rbind(H, T)
} %>%
  dim


# aliases ---------------------

# there are some operators that do not fit the usual function(.) syntax
# therefore, magrittr provides aliases

# example
rnorm(10)    %>%
  multiply_by(5) %>%
  add(5)         %>%
  { 
    cat("Mean:", mean(.), 
        "Variance:", var(.), "\n")
    head(.)
  }

rnorm(10) %>% `*`(5) %>% `+`(5) %>% 
{
  cat("Mean:", mean(.), "Variance:", var(.),  "\n")
  head(.)
}

# overview of available aliases
?magrittr::extract


# when rather not to use the pipe-----
# [source: https://goo.gl/EZ4La8]

# when your pipes are really long (Hadley Wickham suggests > 10 steps); because then code becomes difficult to read again. creating intermediate objects might help
# when you have multiple inputs or outputs

