### -----------------------------
### Folder and file management
### -----------------------------


## load packages -------------------

library(tidyverse)



## introduction ------------------

# interacting with the file system  can be very useful to keep your research reproducible
# useful procedures are:
  # implement a workflow based on relative, not absolute paths
  # create a rigid folder structure
  # download files to a specific folder
  # check whether a file exists
  # remove temporarily stored files


## basic functions for folder management ---------

# check what's in your current folder
dir()

# get current working directory
getwd()

# set working directory (let's avoid doing that though because others will not be able to follow)
#setwd("yourpath")

# create new folder
dir.create("data")
dir.create("data/r-datasets") # if you want to craete both folder and subfolder at once, use recursive = FALSE argument

# get list of all data sets that currently come with R
dat <- as.data.frame(data(package = "datasets")$results)
dat$Item <- dat$Item %>% str_replace(" \\(.+\\)", "")

# store first 10 data sets in local folder
for (i in 1:10) {
  try(df_out <- dat$Item[i] %>% as.character %>% get)
  save(df_out, file = paste0("data/r-datasets/", dat$Item[i], ".RData"))
}

# inspect folder
dir("data/r-datasets")
filenames <- dir("data/r-datasets", full.names = TRUE) # gives us complete relative path
dir("data/r-datasets", pattern = "US") # look for file names that match given pattern
dir("data/r-datasets", pattern = "US", ignore.case = TRUE)

# check if folder exists
dir.exists("data")


## more functions for file management --------
?files

# get basename (= returns the lowest level in a path)
filenames
basename(filenames)

# this is useful for URL handling, too!
url <- "https://www.hertie-school.org/en/fall-2020-safety-hygiene-rules"
browseURL(url)
basename(url)

# get dirname (returns all but the lower level in a path)
dirname(url)

# get file information
file_inf <- file.info(dir(recursive = F))
?base::file.info
file_inf[difftime(Sys.time(), file_inf[,"mtime"], units = "days") < 7 ,]

# identify file extension
tools::file_ext(filenames)

# check if file exists
file.exists(filenames)
file.exists("voterfile.RData")

# rename file
filenames_lower <- tolower(filenames)
file.rename(filenames, filenames_lower)
dir("data/r-datasets")

# remove file
file.remove(filenames_lower[1])

# copy file
file.copy(filenames_lower[2], to = "copy.rdata")
file.remove("copy.rdata")

# compress and unzip files
?zip
?unzip
?tar
?untar

# create temporary files or directories
tempfile()
tempdir()



######################
### IT'S YOUR SHOT ###
######################

# go to the following webpage.
url <- "https://cses.org/data-download/module-4-2011-2016/"
browseURL(url)

# the following piece of code identifies all links to resources on the webpage and selects the subset of links that refers to the survey questionnaire PDFs.
library(rvest)
page_links <- read_html(url) %>% html_nodes("a") %>% html_attr("href")
survey_pdfs <- str_subset(page_links, "/survey")

# set up folder data/cses-pdfs.

# download a sample of 10 of the survey questionnaire PDFs into that folder using a for loop and the download.file() function.

# check if the number of files in the folder corresponds with the number of downloads and list the names of the files.

# inspect the files. which is the largest one?

# zip all files into one zip file.




