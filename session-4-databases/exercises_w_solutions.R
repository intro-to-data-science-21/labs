pacman::p_load(tidyverse)

# Prep for exercises ------------------------------------------------------

study <- read_csv("study.csv")
names(study)

# Question 0: Write a mean function and apply it to the bmi variable ---

### Answer:

# clean data
my_mean <- function(x){
  sum(x)/length(x)
}

my_vec <- c(1,2,3,4,5)
my_mean(my_vec)

my_mean(study$bmi)

# wrong 
my_mean2 <- function(x){
  sum(x, na.rm = T)/length(x)
}

mean(study$bmi, na.rm=T) # oops

# one correct way
my_mean2 <- function(x){
  sum(x, na.rm = T) / length(which(!is.na(x)))
}

study%>%
  filter(!is.na(bmi))%>%
  select(bmi)%>%
  my_mean2()

my_mean2(study$bmi)

# other way
my_mean2 <- function(x){
  x <- na.omit(x)
  sum(x)/length(x)
}

my_mean2(study$bmi)


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
  mutate(emoticons2 = replace_with_emoticons(emotions))

study <- study %>% 
  mutate(emoticons = map_chr(emotions, replace_with_emoticons))

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

### Answer = Iteration

var_list <- list(study$age, study$ht_in, study$wt_lbs)

map(var_list, mean_ci)

map(.x = var_list, .f = mean_ci)
