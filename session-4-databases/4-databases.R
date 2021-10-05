## ----setup, include=FALSE--------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)

pacman::p_load(tidyverse, purrr)


## ---- results=FALSE--------------------------
df <- data.frame(
  a = rnorm(100, 5, 2),
  b = rnorm(100, 100, 15),
  c = rnorm(100, 2, 1),
  d = rnorm(100, 36, 7)
)

df$a <- (df$a - mean(df$a, na.rm = TRUE)) / sd(df$a, na.rm = TRUE)
df$b <- (df$b - mean(df$b, na.rm = TRUE)) / sd(df$a, na.rm = TRUE) # spot the mistake?
df$c <- (df$c - mean(df$c, na.rm = TRUE)) / sd(df$c, na.rm = TRUE)
df$d <- (df$d - mean(df$d, na.rm = TRUE)) / sd(df$d, na.rm = TRUE)


## --------------------------------------------
zscale <- function(x){
  (x - mean(x, na.rm = T) / sd(x, na.rm = T))
}


## --------------------------------------------
zscale <- function(x){
  if (is.numeric(x)) {
  (x - mean(x, na.rm = T) / sd(x, na.rm = T))
  }
}


## ---- results=FALSE--------------------------
df$a <- zscale(df$a)
df$b <- zscale(df$b)
df$c <- zscale(df$c)
df$d <- zscale(df$d)

# you can also use your function with a pipe!
df$d %>% zscale()


## ---- results=FALSE--------------------------
# repetitive code
df$a <- zscale(df$a)
df$b <- zscale(df$b)
df$c <- zscale(df$c)
df$d <- zscale(df$d)

# equivalent iteration
for (i in seq_along(df)) {       # seq_along() similar to length()
  df[[i]] <- zscale(df[[i]])     # [[]] because we are working on single elements
}


## ---- results=FALSE--------------------------
# repetitive code
mean(df$a)
mean(df$b)
mean(df$c)
mean(df$d)

# equivalent map function
map_dbl(df,mean)

# map function in tidyverse style
df %>% map_dbl(mean)



## ---- eval=T---------------------------------
library(tidyverse)
study <- read_csv("./study.csv")

glimpse(study)


## ---- eval=F---------------------------------
## 
## replace_w_emoticons <- # If person is "happy", person is ":)",
##                        # Else if person is emotion "neutral", person is ":/"
##                        # Else person is ":("
## 


## ---- eval=T---------------------------------
mean_ci <- function(x, conf = 0.95) {
  se <- sd(x) / sqrt(length(x)) 
  alpha <- 1 - conf
  mean(x) + se * qnorm(c(alpha / 2, 1 - alpha / 2))
}

mean_ci(study$age)


## ---- eval=F---------------------------------
## mean_ci(study$age)
## mean_ci(study$ht_in)
## mean_ci(study$wt_lbs)
## 


## ---- include=FALSE--------------------------
pacman::p_load(RSQLite, DBI, bigrquery, dbplyr, nycflights13)


## ---- eval=FALSE-----------------------------
## con <- DBI::dbConnect(RMariaDB::MariaDB(),
##                       host = "database.rstudio.com",
##                       user = "tom",
##                       password = rstudioapi::askForPassword("Tom's password")
## )
## 


## --------------------------------------------
# set up connection with DBI and RSQLite
con <- dbConnect(RSQLite::SQLite(), ":memory:")


## --------------------------------------------
summary(con)


## --------------------------------------------

# upload local data frame into remote data source; here: database
copy_to(
  dest = con, 
  df = nycflights13::flights, 
  name = "flights")



## --------------------------------------------

copy_to(
  dest = con, 
  df = nycflights13::flights, 
  name = "flights",
  temporary = FALSE, 
  indexes = list(
    c("year", "month", "day"), 
    "carrier", 
    "tailnum",
    "dest"
  ),
  overwrite = T # throws error as table already exists
)



## --------------------------------------------
DBI::dbListTables(con)


## --------------------------------------------
# generate reference table from the database
flights_db <- tbl(con, "flights")
flights_db 


## --------------------------------------------
# perform various queries
flights_db %>% select(year:day, dep_delay, arr_delay)



## --------------------------------------------

flights_db %>% filter(dep_delay > 240)


## --------------------------------------------
flights_db %>% 
  group_by(dest) %>%
  summarise(delay = mean(dep_time))


## ----echo=TRUE, results='hide', fig.keep='all'----

flights_db %>% 
  filter(distance > 75) %>%
  group_by(origin, hour) %>%
  summarise(delay = mean(dep_delay, na.rm = TRUE)) %>%
  ggplot(aes(hour, delay, color = origin)) + geom_line()



## --------------------------------------------
copy_to(
  dest = con, 
  df = nycflights13::planes, 
  name = "planes",
  temporary = FALSE, 
  indexes = "tailnum"
)

copy_to(
  dest = con, 
  df = nycflights13::airlines, 
  name = "airlines",
  temporary = FALSE, 
  indexes = "carrier"
)

copy_to(
  dest = con, 
  df = nycflights13::airports, 
  name = "airports",
  temporary = FALSE, 
  indexes = "faa"
)

copy_to(
  dest = con, 
  df = nycflights13::weather, 
  name = "weather",
  temporary = FALSE, 
  indexes = list(
    c("year", "month", "day", "hour", "origin")
  )
)


## --------------------------------------------
dbListTables(con)


## --------------------------------------------
planes_db = tbl(con, 'planes')
left_join(
  flights_db,
  planes_db %>% rename(year_built = year),
  by = "tailnum" ## Important: Be specific about the joining column
) %>%
  select(year, month, day, dep_time, arr_time, carrier, flight, tailnum,
         year_built, type, model) 



## ---- warning=TRUE---------------------------

tailnum_delay_db <- flights_db %>% 
  group_by(tailnum) %>%
  summarise(
    delay = mean(arr_delay),
    n = n()
  ) %>% 
  arrange(desc(delay)) %>%
  filter(n > 100)



## --------------------------------------------
nrow(tailnum_delay_db)


## ---- error=TRUE-----------------------------
tail(tailnum_delay_db)


## --------------------------------------------

tailnum_delay_db %>% show_query()


## --------------------------------------------
tailnum_delay <- tailnum_delay_db %>% collect()
tailnum_delay


## --------------------------------------------
sql_query <- "SELECT * FROM flights WHERE dep_delay > 240.0 LIMIT 5"
dbGetQuery(con, sql_query)


## --------------------------------------------

DBI::dbDisconnect(con)



## ---- eval=F---------------------------------
## con <- dbConnect(
##   bigrquery::bigquery(),
##   project = "publicdata",
##   dataset = "samples",
##   billing = google_cloud_project_name # This will tell Google whom to charge
## )

