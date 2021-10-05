pacman::p_load(tidyverse)

# Exercises Part (I) ------------------------------------------------------


# Prep for exercises ------------------------------------------------------

study <- read_csv("./study.csv")

# Question 1: Write a function to replace the emotions with Smiley --------

### Answer: 

replace_with_emoticons <- function(x) {
  if (x == "happy") {
    ":)"
  } else if (x == "sad") {
    ":("
  } else {
    ":/"
  }
}


# Question 2: Create a new column Smiles in the existing dataset ---------

study <- study %>% 
  rowwise() %>% # otherwise the function will only evaluate the first argument
  mutate(emoticons = replace_with_emoticons(emotions))



# Question 3: Write custom function to calculate mean with CI around ------

mean_ci <- function(x, conf = 0.95) {
  x <- na.omit(x) # watch out for NAs in the data
  se <- sd(x) / sqrt(length(x)) 
  alpha <- 1 - conf
  mean(x) + se * qnorm(c(alpha / 2, 1 - alpha / 2))
}

# Question 4: Replace this chunk of code to get th the mean with CI for age, 
# height and weight without repeating yourself. ----------------------------
mean_ci(study$age)
mean_ci(study$ht_in)
mean_ci(study$wt_lbs)

### Aanswer = Iteration

var_list <- list(study$age, study$ht_in, study$wt_lbs)

map(.x = var_list, .f = mean_ci)
