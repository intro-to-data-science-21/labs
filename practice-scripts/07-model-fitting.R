## Model fitting and simulation

library(tidyverse)
library(broom)
library(nycflights13)
library(modelsummary)
library(lme4)


## crafting formulas ------------------------------

# The basic structure of a formula 
y ~ x
  
# Running a model with a formula is straightforward. Note that we don't even have to put the formula in parentheses:
lm(arr_delay ~ distance + origin, data = flights)

# A more explicit way to write formulas
fmla <- as.formula("arr_delay ~ distance + origin")
class(fmla)
lm(fmla, data = flights)

# More formula syntax
y ~ x1 + x2
y ~ x1 - x2 # ignore x2 in analysis
y ~ . # select all other variables in model matrix


# Interactions
y ~ x1 * x2 # That's equivalent to
y ~ x1 + x2 + x1*x2
y ~ x1:x2  # only interaction, not main effects


# As-is variables
y ~ x + I(x^2)

# Extract all variable names from a formula object
all.vars(fmla)

# Incrementally modify/update a formula
?update

## Further reading

# Model formulae in R (Thomas Leeper)
browseURL("https://thomasleeper.com/Rcourse/Tutorials/formulae.html")

# formula() {stats} documentation
browseURL("https://stat.ethz.ch/R-manual/R-devel/library/stats/html/formula.html")


## advanced crafting of formulas: fitting multiple models ------------------------------

# Create a formula for a model with a large number of variables:
xnam <- paste0("x", 1:25)
(fmla <- as.formula(paste("y ~ ", paste(xnam, collapse= "+"))))

## Create a multitude of covariate sets

# Step 1: Define dependent variable + covariate set
depvar <- "arr_delay"
covars <- c("dep_delay", "carrier", "origin", "air_time", "distance", "hour")

# Step 2: Build function to run lm models across set of all possible variable combinations 
combn_models <- function(depvar, covars, data)
{
  combn_list <- list()
  # generate list of covariate combinations
  for (i in seq_along(covars)) {
    combn_list[[i]] <- combn(covars, i, simplify = FALSE)
  }
  combn_list <- unlist(combn_list, recursive = FALSE)
  # function to generate formulas
  gen_formula <- function(covars, depvar) {
    form <- as.formula(paste0(depvar, " ~ ", paste0(covars, collapse = "+")))
    form
  }
  # generate formulas
  formulas_list <- purrr::map(combn_list, gen_formula, depvar = depvar)
  # run models
  models_list <- purrr::map(formulas_list, lm, data = data)
  models_list
}

# Step 3: Run models (careful, this'll generate a quite heavy list)
models_list <- combn_models(depvar = depvar,  covars = covars, data = flights)
length(models_list)


## Further reading

# Multiverse analysis with multiverse
browseURL("https://github.com/MUCollective/multiverse")

# Specification curves with specr
browseURL("https://cran.r-project.org/web/packages/specr/vignettes/specr.html")

# For more background on multiverse analysis, see
browseURL("https://journals.sagepub.com/doi/pdf/10.1177/1745691616658637")

# For more background on specification curve analysis, see
browseURL("https://repository.upenn.edu/cgi/viewcontent.cgi?article=1314&context=marketing_papers")



## working with tidy model outputs ------------------------------

# Inspecting models in R is straightforward with the summary() function
lm(arr_delay ~ distance + origin, data = flights) %>% summary()

# But often you want to post-process estimation results. So let's examine the output of a model function more closely
model_out <- lm(arr_delay ~ distance + origin, data = flights)
class(model_out)
str(model_out)

# Ugh, that's a lot of information in a list structure. Let's use some helper functions to unpack these:
coef(model_out)
fitted.values(model_out)
residuals(model_out)
model.matrix(model_out)

# We can also dig in the summary of the model
str(summary(model_out))
summary(model_out)$coefficients


## The broom philosophy
# "The broom package takes the messy output of built-in functions in R, such as lm, nls, or t.test, and turns them into tidy tibbles."

## Example: linear model output
model_out <- lm(arr_delay ~ distance + origin, data = flights)
model_out
summary(model_out)

# Examine model object
?tidy.lm
broom::tidy(model_out)
broom::tidy(model_out, conf.int = TRUE, conf.level = 0.95)

# Inspect summary statistics
?glance.lm
broom::glance(model_out)

# Add fitted values and residuals to original data
?augment.lm
broom::augment(model_out) %>% slice(1:5)

# Tidy can also be applied to htest objects
tt <- t.test(wt ~ am, mtcars)
tidy(tt)

# The true power of broom unfolds in settings where you want to combine results from multiple analyses (using subgroups of data, different models, bootstrap replicates of the original data frame, permutations, imputations, ...)

# Step 4 (continuing the analysis from above): Extract results from all models
models_broom <- map(models_list, broom::tidy)
models_broom[[1]] # inspect one list entry
# turn into df by rbinding all tidied results
models_broom_df <- map_dfr(models_broom, rbind)

# Step 5: Summarize results
models_broom_df %>% 
  filter(!str_detect(term, "Intercept|carrier")) %>%
  ggplot(aes(estimate)) + 
  geom_histogram(binwidth = .1) + 
  geom_vline(xintercept = 0, linetype="dashed") + 
  facet_grid(cols = vars(term), scales = "free_y") + 
  theme_minimal()

## Further reading

# introductory broom package vignette
browseURL("https://cran.r-project.org/web/packages/broom/vignettes/broom.html")

# overview of available methods
browseURL("https://cran.r-project.org/web/packages/broom/vignettes/available-methods.html")




## Reporting model results: tables ------------------------

# A table of models
model1_out <- lm(arr_delay ~ dep_delay, data = flights)
model2_out <- lm(arr_delay ~ distance, data = flights)
model3_out <- lm(arr_delay ~ dep_delay + distance, data = flights)

models <- list(model1_out, model2_out, model3_out)

modelsummary(models, 
             estimate = "{estimate} [{conf.low}, {conf.high}]",
             statistic = NULL,
             gof_omit = ".+",
             title = "Linear regression of flight delay at arrival (in mins)") 



## Reporting model results: coefficient plots ------------------------

# One model, one plot
cm <- c("distance" = "Distance",
        "originLGA" = "Origin: LGA",
        "originJFK" = "Origin: JFK")
modelplot(model_out, 
          coef_omit = "Interc", 
          coef_map = cm)

# Standardized continuous variable
flights$distance_std <- effectsize::standardize(flights$distance)
model_out_std2 <-lm(arr_delay ~ distance_std + origin, data = flights)
cm <- c("distance_std" = "Distance",
        "originLGA" = "Origin: LGA",
        "originJFK" = "Origin: JFK")

# One model, one plot (more options)
modelplot(model_out_std2, coef_omit = "Interc", coef_map = cm) + 
  xlim(-5, .25) + 
  geom_vline(xintercept = 0, linetype="dashed") + 
  labs(title = "Linear regression of flight delay at arrival (in mins)",
       caption = "Data source: nycflights13 package") + 
  theme_minimal()

# Rescale continuous variable
flights$distance1kmiles <- flights$distance / 1000
model_out_kmiles <-lm(arr_delay ~ distance1kmiles + origin, data = flights)
cm <- c("distance1kmiles" = "Distance (1k miles)",
        "originLGA" = "Origin: LGA",
        "originJFK" = "Origin: JFK")

# One model, one plot (more options)
modelplot(model_out_kmiles, coef_omit = "Interc", coef_map = cm) + 
  xlim(-5, .25) + 
  geom_vline(xintercept = 0, linetype="dashed") + 
  labs(title = "Linear regression of flight delay at arrival (in mins)",
       caption = "Data source: nycflights13 package") + 
  theme_minimal()

# Multiple options, one faceted plot
modelplot(models, coef_omit = "Interc") + 
  facet_grid(~model)

library(effectsize)
?standardize
model_out_std <- standardize_parameters(model_out)
modelplot(model_out_std, coef_omit = "Interc", coef_map = cm) + 
  labs(title = "Linear regression of flight delay at arrival (in mins)",
       caption = "Data source: nycflights13 package") + 
  theme_minimal()


## Reporting model results: true-vs-fitted plots ------------------------

model_out %>% 
  augment() %>%
  slice_sample(n = 10000) %>% 
  ggplot(aes(x = .fitted, y = arr_delay)) +
  geom_point(alpha =  0.25) +
  geom_abline(intercept = 0, slope = 1) + 
  labs(title = "Fitted vs. true values, lm(arr_delay ~ distance + origin)",
       caption = "Data source: nycflights13 package") + 
  xlab("Fitted values") + 
  ylab("Arrival delay (in mins)") + 
  theme_minimal()

model_out_improve <- lm(arr_delay ~ dep_delay + distance + origin, data = flights)
model_out_improve %>% 
  augment() %>%
  slice_sample(n = 10000) %>% 
  ggplot(aes(x = .fitted, y = arr_delay)) +
  geom_point(alpha =  0.25) +
  geom_abline(intercept = 0, slope = 1) + 
  labs(title = "Fitted vs. true values, lm(arr_delay ~ dep_delay + distance + origin)",
       caption = "Data source: nycflights13 package") + 
  xlab("Fitted values") + 
  ylab("Arrival delay (in mins)") + 
  theme_minimal()

## Further reading

# modelsummary creates tables and plots to summarize statistical models and data in R
browseURL("https://github.com/vincentarelbundock/modelsummary")

# alternative packages for model summaries, including stargazer and texreg
browseURL("https://github.com/vincentarelbundock/modelsummary#alternative-packages")

# yet another alternative for data visualization for statistics: sjPlot
browseURL("https://strengejacke.github.io/sjPlot/")
