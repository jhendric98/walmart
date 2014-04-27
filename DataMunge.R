#
# Walmart competition processing 
#

# Common read.csv wrapper to extend functionality
readData <- function(path.name, file.name, column.types, missing.types) {
  read.csv( paste(path.name, file.name, sep=""),
            header=TRUE,
            sep=",",
            colClasses=column.types,
            na.strings=missing.types )
}


# Setup working environment
setwd("~/Documents/kaggle/walmart")

# Setup for file loads
walmart.path <- "./data/"
train.data.file <- "train.csv"
test.data.file <- "test.csv"
stores.data.file <- "store.csv"
features.data.file <- "features.csv"
samplesubmission.data.file <- "sample.Submission.csv"

missing.types <- c("NA", "")

# column types can be: logical, integer, numeric, complex, character, raw
#                      factor, Date, or POSIXct

features.col.types <- c('factor',    # StoreID
                        'Date',      # SalesWeek 
                        'numeric',   # Temperature
                        'numeric',   # Fuel_Price
                        'numeric',   # MarkDown1
                        'numeric',   # MarkDown2
                        'numeric',   # MarkDown3
                        'numeric',   # MarkDown4
                        'numeric',   # MarkDown5
                        'numeric',   # CPI
                        'numeric',   # Unemployment
                        'logical'    # IsHoliday
)


# Read in the features file
features.raw <- readData(walmart.path, features.data.file, 
                      features.col.types, missing.types)
df.features <- features.raw


# fix features data
df.features[!is.na(df.features$MarkDown1), 5] <- 1
df.features[!is.na(df.features$MarkDown2), 6] <- 1
df.features[!is.na(df.features$MarkDown3), 7] <- 1
df.features[!is.na(df.features$MarkDown4), 8] <- 1
df.features[!is.na(df.features$MarkDown5), 9] <- 1

df.features[is.na(df.features$MarkDown1), 5] <- 0
df.features[is.na(df.features$MarkDown2), 6] <- 0
df.features[is.na(df.features$MarkDown3), 7] <- 0
df.features[is.na(df.features$MarkDown4), 8] <- 0
df.features[is.na(df.features$MarkDown5), 9] <- 0

# change markdown to be a logical field
df.features$MarkDown1 <- as.logical(df.features$MarkDown1)
df.features$MarkDown2 <- as.logical(df.features$MarkDown2)
df.features$MarkDown3 <- as.logical(df.features$MarkDown3)
df.features$MarkDown4 <- as.logical(df.features$MarkDown4)
df.features$MarkDown5 <- as.logical(df.features$MarkDown5)

# ## map missing data by provided feature
# require(Amelia)
# missmap(df.features, main="Walmart Feature Data - Missings Map", 
#         col=c("yellow", "black"), legend=FALSE)


#fix missing CPIs by replacing with the mean of the Store's CPI
x <- unique(df.features[is.na(df.features$CPI), "Store"])

for (i in x) {
  y <- mean(df.features[df.features$Store==i, "CPI"], na.rm = T)
  df.features[df.features$Store==i & is.na(df.features$CPI), "CPI"] <- y
}

# fix missing Unemployment by replacing with mean of the store's unemployment
x <- unique(df.features[is.na(df.features$Unemployment), "Store"])

for (i in x) {
  y <- mean(df.features[df.features$Store==i, "Unemployment"], na.rm = T)
  df.features[df.features$Store==i & is.na(df.features$Unemployment), "Unemployment"] <- y
}


# clean up vars
rm(list=c("i","x","y"))


# add holiday columns
# Super Bowl: 12-Feb-10, 11-Feb-11, 10-Feb-12, 8-Feb-13
x <- c("superbowl","2010-02-12")
x <- rbind(x,c("superbowl","2011-02-11"))
x <- rbind(x,c("superbowl","2012-02-10"))
x <- rbind(x,c("superbowl","2013-02-08"))

#  Labor Day: 10-Sep-10, 9-Sep-11, 7-Sep-12, 6-Sep-13
x <- rbind(x,c("laborday","2010-09-10"))
x <- rbind(x,c("laborday","2011-09-09"))
x <- rbind(x,c("laborday","2012-09-07"))
#x <- rbind(x,c("laborday","2013-09-06"))

#  Thanksgiving: 26-Nov-10, 25-Nov-11, 23-Nov-12, 29-Nov-13
x <- rbind(x,c("thanksgiving","2010-11-26"))
x <- rbind(x,c("thanksgiving","2011-11-25"))
x <- rbind(x,c("thanksgiving","2012-11-23"))
#x <- rbind(x,c("thanksgiving","2013-11-29"))

#  Christmas: 31-Dec-10, 30-Dec-11, 28-Dec-12, 27-Dec-13
x <- rbind(x,c("christmas","2010-12-31"))
x <- rbind(x,c("christmas","2011-12-30"))
x <- rbind(x,c("christmas","2012-12-28"))
#x <- rbind(x,c("christmas","2013-12-27"))

colnames(x) <- c("Holiday","Date")
rownames(x) <- NULL
x <- as.data.frame(x)
x$Date <- as.Date(x$Date)


#merge features and x to add holiday column
tmp <- merge(x=df.features, y=x, by="Date", all.x=TRUE)

levels(tmp$Holiday) <- c("christmas","laborday","superbowl","thanksgiving","normal")
tmp[is.na(tmp$Holiday), 13] <- "normal"

# reorder and cleanup names
tmp <- tmp[,c(colnames(df.features),"Holiday")]
tmp <- tmp[order(tmp[,1],tmp[,2]),]
rownames(tmp) <- NULL

df.features <- tmp
rm(tmp)
rm(x)






