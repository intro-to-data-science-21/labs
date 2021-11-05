## ----setup, include=FALSE--------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)


## ---- fig.align='center', echo=F, out.width = "90%"----
knitr::include_graphics("pics/workflow.png")


## ---- message=F------------------------
library(rvest)
library(stringr)


## --------------------------------------
parsed_url <- read_html("https://en.wikipedia.org/wiki/Cologne")


## --------------------------------------
parsed_nodes <- html_nodes(parsed_url, 
                           xpath = '//*[@id="mw-content-text"]/div[1]/p[75]')
carnival <- html_text(parsed_nodes)
carnival


## ---- fig.align='center', echo=F, out.width = "90%"----
knitr::include_graphics("pics/inspect.png")


## ---- fig.align='center', echo=F, out.width = "90%"----
knitr::include_graphics("pics/selector.png")



## ---- fig.align='center', echo=F, out.width = "90%"----

knitr::include_graphics("pics/selector2.png")


## --------------------------------------
hot100page <- "https://www.billboard.com/charts/hot-100"
hot100 <- read_html(hot100page)


## --------------------------------------
rank <- hot100 %>% 
  rvest::html_nodes('body') %>% 
  xml2::xml_find_all("//span[contains(@class, 'chart-element__rank__number')]") %>% 
  rvest::html_text()

artist <- hot100 %>% 
  rvest::html_nodes('body') %>% 
  xml2::xml_find_all("//span[contains(@class, 'chart-element__information__artist')]") %>% 
  rvest::html_text()

title <- hot100 %>% 
  rvest::html_nodes('body') %>% 
  xml2::xml_find_all("//span[contains(@class, 'chart-element__information__song')]") %>% 
  rvest::html_text()


## --------------------------------------
chart_df <- data.frame(rank, artist, title)

knitr::kable(chart_df  %>% head(10))


## --------------------------------------
url_p <- read_html("https://en.wikipedia.org/wiki/List_of_Nobel_laureates_in_Literature")
tables <- html_nodes(url_p, xpath = '//*[@id="mw-content-text"]/div[1]/table[3]')
html_table(tables)



spaceflights <- as.data.frame(tables[[1]])

knitr::kable(spaceflights[,1:7]  %>% head(10))


## ---- eval=F---------------------------
## browseURL("http://www.jstatsoft.org/issue/archive")


## --------------------------------------
baseurl <- "http://www.jstatsoft.org/article/view/v"
volurl <- paste0("0", seq(1, 99, 1))
volurl[1:9] <- paste0("00", seq(1, 9, 1))
brurl <- paste0("0", seq(1, 9, 1))
urls_list <- paste0(baseurl, volurl)
urls_list <- paste0(rep(urls_list, each = 9), "i", brurl)
urls_list[1:5]


## ---- eval=F---------------------------
## tempwd <- ("data/jstatsoftStats")
## dir.create(tempwd)
## setwd(tempwd)


## ---- eval=F---------------------------
## folder <- "html_articles/"
## dir.create(folder)
## 
## for (i in 1:length(urls_list)) {
##   # only update, don't replace
##     if (!file.exists(paste0(folder, names[i]))) {
##   # skip article when we run into an error
##       tryCatch(
##         download.file(urls_list[i], destfile = paste0(folder, names[i])),
##         error = function(e) e
##       )
##   # don't kill their server --> be polite!
##       Sys.sleep(runif(1, 0, 1))
## 
## } }


## ---- fig.align='center', echo=F, out.width = "70%"----
knitr::include_graphics("pics/html_files.png")


## ---- eval=F---------------------------
## list_files <- list.files(folder, pattern = "0.*")
## list_files_path <- list.files(folder, pattern = "0.*", full.names = TRUE)
## length(list_files)


## ---- eval=F---------------------------
## # define output first
## authors <- character()
## title <- character()
## datePublish <- character()
## 
## # then run the loop
## for (i in 1:length(list_files_path)) {
##   html_out <- read_html(list_files_path[i])
## 
##   authors[i] <- html_text(html_nodes(html_out , xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "authors_long", " " ))]//strong'))
## 
##   title[i] <- html_text(html_nodes(html_out , xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "page-header", " " ))]'))
## 
##   datePublish[i] <- html_text(html_nodes(html_out , xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "article-meta", " " ))]//*[contains(concat( " ", @class, " " ), concat( " ", "row", " " )) and (((count(preceding-sibling::*) + 1) = 2) and parent::*)]//*[contains(concat( " ", @class, " " ), concat( " ", "col-sm-8", " " ))]'))
## }
## 
## # inspect data
## authors[1:3]
## title[1:2]
## datePublish[1:3]
## 
## # create a data frame
## dat <- data.frame(authors = authors, title = title, datePublish = datePublish)
## dim(dat)

