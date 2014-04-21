#
#  Process Walmart Data
#

#set working dir
setwd("~/Documents/kaggle/walmart")


# import raw data
features <- read.csv("./data/features.csv")
#orgfeatures <- read.csv("./data/features.csv")
sampleSubmission <- read.csv("./data/sampleSubmission.csv")
store <- read.csv("./data/stores.csv")
train <- read.csv("./data/train.csv")
test <- read.csv("./data/test.csv")

# fix features data
features[is.na(features$MarkDown1), 5] <- 0
features[is.na(features$MarkDown2), 6] <- 0
features[is.na(features$MarkDown3), 7] <- 0
features[is.na(features$MarkDown4), 8] <- 0
features[is.na(features$MarkDown5), 9] <- 0

#fix missing CPIs by replacing with the mean of the Store's CPI
x <- unique(features[is.na(features$CPI), "Store"])

for (i in x) {
  y <- mean(features[features$Store==i, "CPI"], na.rm = T)
  features[features$Store==i & is.na(features$CPI), "CPI"] <- y
}

# fix missing Unemployment by replacing with mean of the store's unemployment
x <- unique(features[is.na(features$Unemployment), "Store"])

for (i in x) {
  y <- mean(features[features$Store==i, "Unemployment"], na.rm = T)
  features[features$Store==i & is.na(features$Unemployment), "Unemployment"] <- y
}

# clean up vars
rm(i)
rm(x)
rm(y)

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
rownames(x) <- c(1:nrow(x))
x <- as.data.frame(x)

#merge features and x to add holiday column
tmp <- merge(features, x, by = "Date", all=TRUE)

levels(tmp$Holiday) <- c("christmas","laborday","superbowl","thanksgiving","normal")
tmp[is.na(tmp$Holiday), 13] <- "normal"

# reorder and cleanup names
tmpordered <- tmp[order(tmp[,1],tmp[,2]),]
cleanfeatures <- tmpordered[,c(colnames(features),"Holiday")]
rownames(cleanfeatures) <- c(1:nrow(cleanfeatures))
rm(tmp)
rm(tmpordered)
rm(x)


# merge store data to cleanfeatures
tmp <- merge(cleanfeatures, store, by="Store", all=TRUE)
tmpordered <- tmp[order(tmp[,1],tmp[,2]),]
cleanfeatures <- tmpordered[,c(colnames(features),"Holiday","Type","Size")]
rownames(cleanfeatures) <- c(1:nrow(cleanfeatures))
rm(tmp)
rm(tmpordered)


features <- cleanfeatures
rm(cleanfeatures)

#save features set for later reference
write.table(features, file="./data/models/features.csv",row.names=F,col.names=T,sep=",")


# merge features and training set
#  *** this merge takes too mauch memory for R session
#tmp <- merge(features,train,by="Store",all=TRUE)

dept <- sort(unique(train[,"Dept"]))
strs <- sort(unique(store[,"Store"]))

#create complete feature files for each store.
#TODO need to join the store and date fields to join on both*
for (i in strs) {
  traini <- train[train$Store==i,]
  featuresi <- features[features$Store==i,]
  
  modeli <- merge(featuresi,traini,by="Store",all=TRUE)
  write.table(modeli, file=paste("./data/models/model",i,".csv", sep=""), row.names=F, col.names=T, sep=",")
  rm(traini)
  rm(featuresi)
  rm(modeli)
}

rm(i)

#load and split each store by dept sales and save to deptsales files.
for (o in strs) {
  modelo <- read.csv(file=paste("./data/models/model",o,".csv", sep=""), header=T, sep=",")
  for (i in dept) {
    #split model by dept
    
    #append dept to dept file
    
    #loop to next dept
  }
  
}

# build models for each dept.



