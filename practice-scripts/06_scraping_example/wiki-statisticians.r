### -----------------------------
### simon munzert
### build a network of statisticians
### -----------------------------

## goals

# gather list of phi
# fetch Wikipedia entries
# identify links
# construct connectivity matrix
# visualize network

## load packages  -----------------------
library(tidyverse)
library(rvest)
library(igraph)
library(networkD3)


# set temporary working directory
tempwd <- ("../data/wikipediaStatisticians")
dir.create(tempwd)
setwd(tempwd)


## step 1: inspect page
url <- "https://en.wikipedia.org/wiki/List_of_statisticians"
browseURL(url)


## step 2: retrieve links
html <- read_html(url)
anchors <- html_nodes(html, xpath = "//ul/li/a[1]")
links <- html_attr(anchors, "href")

links <- links[!is.na(links)]
links_iffer <-
  seq_along(links) >=
  seq_along(links)[str_detect(links, "Odd_Aalen")] &
  seq_along(links) <=
  seq_along(links)[str_detect(links, "George_Kingsley_Zipf")] &
  str_detect(links, "/wiki/")
links_index <- seq_along(links)[links_iffer]
links <- links[links_iffer]
length(links)


##  step 3: extract names
names <- links %>% basename %>% sapply(., URLdecode)  %>% str_replace_all("_", " ") %>% str_replace_all(" \\(.*\\)", "") %>% str_trim


## step 4: fetch personal wiki pages
baseurl <- "http://en.wikipedia.org"
HTML <- list()
Fname <- str_c(basename(links), ".html")
URL <- str_c(baseurl, links)
# loop
for ( i in seq_along(links) ){
  # url
  url <- URL[i]
  # fname
  fname <- Fname[i]
  # download
  if ( !file.exists(fname) ) download.file(url, fname)
  # read in files
  HTML[[i]] <- read_html(fname)
}


## step 5: identify links between statisticians
# loop preparation
connections <- data.frame(from=NULL, to=NULL)
# loop
for (i in seq_along(HTML)) {
  pslinks <- html_attr(
    html_nodes(HTML[[i]], xpath="//p//a"), # note: only look for links in p sections; otherwise too many links collected
    "href")
  links_in_pslinks <- seq_along(links)[links %in% pslinks]
  links_in_pslinks <- links_in_pslinks[links_in_pslinks!=i]
  connections <- rbind(
    connections,
    data.frame(
      from=rep(i-1, length(links_in_pslinks)), # -1 for zero-indexing
      to=links_in_pslinks-1 # here too
    )
  )
}

# results
names(connections) <- c("from", "to")
head(connections)

# make symmetrical
connections <- rbind(
  connections,
  data.frame(from=connections$to,
             to=connections$from)
)
connections <- connections[!duplicated(connections),]


## step 6: visualize connections
connections$value <- 1
nodesDF <- data.frame(name = names, group = 1)

network_out <- forceNetwork(Links = connections, Nodes = nodesDF, Source = "from", Target = "to", Value = "value", NodeID = "name", Group = "group", zoom = TRUE, fontSize = 14, opacityNoHover = 3)

saveNetwork(network_out, file = 'connections.html')
browseURL("connections.html")


## step 7: identify top nodes in data frame
nodesDF$id <- as.numeric(rownames(nodesDF)) - 1
connections_df <- merge(connections, nodesDF, by.x = "to", by.y = "id", all = TRUE)
to_count_df <- count(connections_df, name)
arrange(to_count_df, desc(n))  %>% slice(1:20)
