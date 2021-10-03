### Working with databases

# This script borrows heavily from
browseURL("https://cran.r-project.org/web/packages/dbplyr/vignettes/dbplyr.html")
browseURL("https://db.rstudio.com/getting-started/connect-to-database")
browseURL("https://github.com/uo-ec607/lectures/blob/master/16-databases/16-databases.html")

# load packages
library(DBI)
library(tidyverse)


# Connecting to an SQLite database -----------------------

# SQLite only needs a path to the database. (Here, ":memory:" is a special
# path that creates an in-memory database.) Other database drivers
# will require more details (like user, password, host, port, etc.)

# set up connection with DBI and RSQLite
con <- dbConnect(RSQLite::SQLite(), ":memory:")

# upload local data frame into remote data source; here: database
copy_to(
  dest = con, 
  df = nycflights13::flights, 
  name = "flights")

# Some more general notes on database connections:
# The arguments to DBI::dbConnect() vary from database to database, but the first argument is always the database backend. 
# For instance, it’s RSQLite::SQLite() for RSQLite, RMariaDB::MariaDB() for RMariaDB, RPostgres::Postgres() for RPostgres, odbc::odbc() for odbc, and bigrquery::bigquery() for BigQuery.

# Most existing databases don’t live in a file, but instead live on another server. That means in real-life that your code will look more like this:

con <- DBI::dbConnect(RMariaDB::MariaDB(), 
                      host = "database.rstudio.com",
                      user = "hadley",
                      password = rstudioapi::askForPassword("Database password")
)


# Indexing  ---------------------------------

# We're basically following the approach from above, but are now also passing a list of indexes to the function.  
# Here we set up indexes that will allow us to quickly process the data by day, carrier, plane, and destination. 
# Creating the right indices is key to good database performance. In common applications where we don't set up the database, this will be taken care of by the database maintainer.

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
  )
)



# Example queries ----------------------------

# generate reference table from the database
flights_db <- tbl(con, "flights")
flights_db # note that it's a remote source; the table is not stored in our local environment

# perform various queries
flights_db %>% select(year:day, dep_delay, arr_delay)

flights_db %>% filter(dep_delay > 240)

flights_db %>% 
  group_by(dest) %>%
  summarise(delay = mean(dep_time))

flights_db %>% 
  filter(distance > 75) %>%
  group_by(origin, hour) %>%
  summarise(delay = mean(dep_delay, na.rm = TRUE)) %>%
  ggplot(aes(hour, delay, color = origin)) + geom_line()


# The most important difference between ordinary data frames and remote database queries is that your R code is translated into SQL and executed in the database on the remote server, not in R on your local machine. When working with databases, dplyr tries to be as lazy as possible:
  # - It never pulls data into R unless you explicitly ask for it.
  # - It delays doing any work until the last possible moment: it collects together everything you want to do and then sends it to the database in one step.

# This even applies when you assign the output of a database query to an object:

tailnum_delay_db <- flights_db %>% 
  group_by(tailnum) %>%
  summarise(
    delay = mean(arr_delay),
    n = n()
  ) %>% 
  arrange(desc(delay)) %>%
  filter(n > 100)

# This also has some downsides.
# Exhibit A: Because there’s generally no way to determine how many rows a query will return unless you actually run it, nrow() is always NA.
nrow(tailnum_delay_db)

# Exhibit B: Because you can’t find the last few rows without executing the whole query, you can’t use tail().
tail(tailnum_delay_db)


# If you then want to pull the data into a local data frame, use collect():
tailnum_delay <- tailnum_delay_db %>% collect()
tailnum_delay


# Inspecting queries ----------------------------

# We can always inspect the SQL that dbplyr is generating:

tailnum_delay_db %>% show_query()

# That's probably not how you would have written it, but it works.

#More information about SQL translation can be found here:
vignette("translation-verb")



# Joins ----------------------------

# Databases become more exciting with more tables. So let's add a couple more:

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

## List tables in our "con" database connection
dbListTables(con)


# A left join

planes_db = tbl(con, 'planes')
left_join(
  flights_db,
  planes_db %>% rename(year_built = year),
  by = "tailnum" ## Important: Be specific about the joining column
) %>%
  select(year, month, day, dep_time, arr_time, carrier, flight, tailnum,
         year_built, type, model) 


# Using SQL directly in R ----------------------

# Use DBI::dbGetQuery() to run SQL queries in R scripts
sql_query <- "SELECT * FROM flights WHERE dep_delay > 240.0 LIMIT 5"
dbGetQuery(con, sql_query)

# If you want to learn more about writing SQL with dbplyr, check out
vignette('sql', package = 'dbplyr')



# Disconnect from database ----------------------

DBI::dbDisconnect(con)

