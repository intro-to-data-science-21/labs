##### DEBUGGING & PACKAGING #####

geod_dist <- function(lat1, lon1, lat2, lon2, earth.radius = 6371) {
 # from degrees to radians
 deg2rad <- function(deg) return(deg*pi/180)
 lon1 <- deg2rad(lon1)
 lat1 <- deg2rad(lat1)
 lon2 <- deg2rad(long2)
 lat2 <- deg2rad(lat2)
 # calculation
 delta.long <- (lon2 - lon1)
 delta.lat <- (lat2 - lat1)
 a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sing(delta.long/2)^2
 c <- 2 * asin(min(1,sqrt(a)))
 d = earth_radius * c
 return(d)
}

geod_dist(lat1 = 49.5, lon1 = 8.4, lat2 = 52.5, lon2 = 13.4)

# GLOBAL OBJECTS
# make the objects that are otherwise entered as input parameters to your function global
lat1 = 49.5; lon1 = 8.4; lat2 = 52.5; lon2 = 13.4
# now, execute line by line
deg2rad <- function(deg) return(deg*pi/180)
lon1 <- deg2rad(lon1)
lat1 <- deg2rad(lat1)
lon2 <- deg2rad(lon2)
lat2 <- deg2rad(lat2)
delta.long <- (lon2 - lon1)
delta.lat <- (lat2 - lat1)
a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sing(delta.long/2)^2
c <- 2 * asin(min(1,sqrt(a)))
d = earth_radius * c
return(d)


# TRACEBACK
geod_dist(lat1 = 49.5, lon1 = 8.4, lat2 = 52.5, lon2 = 13.4)
traceback()


# BROWSER
geod_dist <- function(lat1, lon1, lat2, lon2, earth.radius = 6371) {
  # from degrees to radians
  browser()
  deg2rad <- function(deg) return(deg*pi/180)
  lon1 <- deg2rad(lon1)
  lat1 <- deg2rad(lat1)
  lon2 <- deg2rad(long2)
  lat2 <- deg2rad(lat2)
  # calculation
  delta.long <- (lon2 - lon1)
  delta.lat <- (lat2 - lat1)
  a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
  c <- 2 * asin(min(1,sqrt(a)))
  d = earth_radius * c
  return(d)
}
geod_dist(lat1 = 49.5, lon1 = 8.4, lat2 = 52.5, lon2 = 13.4)


########################### CONDITION HANDLING ###################################

f1 <- function(x) {
  log(x)
  10
 }

f1("x")


## -----------------------------------------------------------------------------------------------------------------
f1 <- function(x) { 
  try(log(x))
  10 
} 

f1("x")


## -----------------------------------------------------------------------------------------------------------------
f1 <- function(x) { 
  try(log(x), silent = TRUE)
  10 
} 

f1("x")


## -----------------------------------------------------------------------------------------------------------------
try({ 
  a <- 1 
  b <- "x" 
  a + b 
}, silent = TRUE)


## -----------------------------------------------------------------------------------------------------------------
success <- try(as.character(1 + 2))
failure <- try("a" + "b", silent = TRUE) 
class(success)
class(failure) 


## ----eval = FALSE-------------------------------------------------------------------------------------------------
elements <-list(1,2,3,"f")
results <- map(elements, log)
results <- map(elements, possibly(log, NA))
results

## -----------------------------------------------------------------------------------------------------------------
show_condition <- function(code) { 
  tryCatch(code, 
           error = function(c) "error", 
           warning = function(c) "warning", 
           message = function(c) "message" )
  }

show_condition(stop("!"))
show_condition(warning("?!"))
show_condition(message("?"))


## -----------------------------------------------------------------------------------------------------------------
show_condition(10+5)


read.csv_new <- function(file, ...) {
   tryCatch(read.csv(file, ...), error = function(c) {
     c$message <- paste0(c$message, " (in ", file, ")")
     stop(c)
   })
 }
 
read.csv("code/dummy.csv")
read.csv_new("code/dummy.csv")


 
###################### PACKAGING ######################################################

library(devtools)
library(roxygen2)


create_package("~/Desktop/hertie", open = FALSE)


setwd("~/Desktop/hertie")


theme_hertie <- ggplot2::theme_classic() +
  ggplot2::theme(text =  element_text(family = "Georgia", size = 23, color = "darkred"))
  


use_r("theme_hertie")


document()

?log
?theme_hertie



setwd("~/Desktop")
install("hertie")


setwd("~/Desktop/hertie")

check()


use_mit_license("Lisa Oswald")


#devtools::install_github("lfoswald/heRtie")
#library(heRtie)


# test
df <- data.frame(cbind(x = rnorm(100, 0, 1),
                       y = rnorm(100, 5, 3),
                       z = rbinom(100, 6, 0.5)))

ggplot(df, aes(x,y))+
  geom_point()+
  theme_hertie




