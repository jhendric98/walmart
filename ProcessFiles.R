#
#  Process Walmart Data
#

#set working dir
setwd("~/Documents/kaggle/walmart")


# import raw data
features <- read.csv("./data/features.csv")
sampleSubmission <- read.csv("./data/sampleSubmission.csv")
store <- read.csv("./data/test.csv")
train <- read.csv("./data/train.csv")
test <- read.csv("./data/test.csv")

# fix features data
features[is.na(MarkDown1), 5] <- 0
features[is.na(MarkDown2), 6] <- 0
features[is.na(MarkDown3), 7] <- 0
features[is.na(MarkDown4), 8] <- 0
features[is.na(MarkDown5), 9] <- 0

#fix missing CPIs by replacing with the mean of the Store's CPI
x <- unique(features[is.na(CPI), "Store"])

for (i in x) {
  y <- mean(features[Store==i, "CPI"], na.rm = T)
  features[Store==i & is.na(CPI), "CPI"] <- y
}

# fix missing Unemployment by replacing with mean of the store's unemployment
x <- unique(features[is.na(Unemployment), "Store"])

for (i in x) {
  y <- mean(features[Store==i, "Unemployment"], na.rm = T)
  features[Store==i & is.na(Unemployment), "Unemployment"] <- y
}



# Process the features file
#   - missing data
#   - add correct holiday weeks

#  Create random subspace files

# Run model 

# Cross validate results.

