# Demonstration of relative and absolute paths

# create some data

x <- c(1:20)
y <- c(letters[1:20])
z <- rnorm(20,1,100)

my_df <- data.frame(x,y,z)

# save data as csv 

# a) in working directory (GOOD)
write.csv(my_df, "my_df.csv")

# b) in data folder of working directory (EVEN BETTER)
write.csv(my_df, "data/my_df.csv")
write.csv(my_df, "./data/my_df.csv") #same

# c) by specifying the absolute path (WRONG)
write.csv(my_df,"/Users/l.oswald/Documents/R/Session-2-test/data/my_df2.csv")
# abbreviate root dirctory - a little bit shorter but still absolute - no one has your folder structure!
write.csv(my_df,"~/Documents/R/Session-2-test/data/my_df2.csv") 


# read csv

# absolute path
my_read_df2 <- read_csv("/Users/l.oswald/Documents/R/Session-2-test/data/my_df2.csv")
# relative path
my_read_df <- read_csv("my_df.csv") # better, right?


