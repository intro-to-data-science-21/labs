# Demonstration of relative and absolute paths
library(readr)

# create some data

my_df <- data.frame(id = letters[1:10], x = 1:10, y = 11:20)


# save data as csv 

# a) in working directory (GOOD)
write.csv(my_df, "my_df.csv")

# b) in data folder of working directory (EVEN BETTER)
write.csv(my_df, "data/my_df.csv", row.names=FALSE)
write.csv(my_df, "./data/my_df.csv", row.names=FALSE) #same

# c) by specifying the absolute path (WRONG)
write.csv(my_df,"/Users/l.oswald/Documents/R/Session-2-test/data/my_df2.csv", row.names=FALSE)
# abbreviate root dirctory - a little bit shorter but still absolute - no one has your folder structure!
write.csv(my_df,"~/Documents/R/Session-2-test/data/my_df2.csv", row.names=FALSE) 


# read csv

# absolute path
my_read_df2 <- read_csv("/Users/l.oswald/Documents/R/Session-2-test/data/my_df2.csv")
# relative path
my_read_df <- read_csv("data/my_df.csv") # better, right?


