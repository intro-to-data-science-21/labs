### -----------------------------
### Data import and export
### -----------------------------


## load packages -------------------

library(tidyverse)


## importing rectangular spreadsheet data --------------

# our recommendation: go with readr package from the tidyverse (automatically loaded above)
# readr provides a fast and friendly way to read rectangular data (like csv, tsv, and fwf)
# it is designed to flexibly parse many types of data found in the wild, while still cleanly failing when data unexpectedly changes.

# why readr, not base R?
  # readr is much faster (up to 10x)
  # strings remain strings by default
  # automatically parse common date/time formats
  # progress bar if needed

# set up new directory
dir.create("data/import_export")

# import and export comma-delimited files
readr_example("mtcars.csv")
mtcars <- read_csv(readr_example("mtcars.csv"))
head(mtcars)
write_csv(mtcars, "data/import_export/mtcars-comma.csv")

# modify column type if desired
mtcars <- read_csv(readr_example("mtcars.csv"), col_types = 
                     cols(
                       mpg = col_character(),
                       cyl = col_integer(),
                       disp = col_double(),
                       hp = col_integer(),
                       drat = col_double(),
                       vs = col_integer(),
                       wt = col_double(),
                       qsec = col_double(),
                       am = col_integer(),
                       gear = col_integer(),
                       carb = col_integer()
                     )
)


# import and export semi-colon delimited files (Germans!)
write_delim(mtcars, delim = ";", path = "data/import_export/mtcars-semicolon.csv")
mtcars <- read_csv2("data/import_export/mtcars-semicolon.csv")
head(mtcars)

# other functions
# read_fwf()
# read_tsv()




## importing SPSS and Stata files (SAS, too) --------------

# our recommendation: go with haven package from the tidyverse (automatically loaded above)
# provides compatibility with newer data formats
# provides good handling of variable and value labels; consistent with rest of tidyverse

# Stata
write_dta(mtcars, "data/import_export/mtcars.dta")
mtcars_stata <- read_dta("data/import_export/mtcars.dta")

# SPSS
write_sav(mtcars, "data/import_export/mtcars.sav")
mtcars_spss <- read_sav("data/import_export/mtcars.sav")
