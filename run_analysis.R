#Merges the training and test sets to create one data set
x1 <- read.table("train/X_train.txt")
x2 <- read.table("test/X_test.txt")
X <- rbind(x1,x2)


s1 <- read.table("train/subject_train.txt") 
s2 <- read.table("test/subject_test.txt") 
S <- rbind(s1, s2) 


y1 <- read.table("train/y_train.txt") 
y2 <- read.table("test/y_test.txt") 
Y <- rbind(y1, y2) 


#Extracts only the measurements on the mean and standard deviation for each measurement
features <- read.table("features.txt")
indices_of_good_features <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
X <- X[, indices_of_good_features]
names(X) <- features[indices_of_good_features, 2]
names(X) <- gsub("\\(|\\)", "", names(X))
names(X) <- tolower(names(X))

#Uses descriptive activity names to name the activities in the data set
activity <- read.table("activity_labels.txt")
activity[, 2] = gsub("_", "", tolower(as.character(activity[, 2])))
Y[,1] = activity[Y[,1], 2]
names(Y) <- "activity"

#Appropriately labels the data set with descriptive variable names.

names(S) <- "subject"
cleaned <- cbind(S, Y, X)
write.table(cleaned, "merged_clean_data.txt")


# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activitie and each subject
uniqueSubjects = unique(S)[,1] 
numSubjects = length(unique(S)[,1]) 
numActivities = length(activity[,1]) 
numCols = dim(cleaned)[2] 
result = cleaned[1:(numSubjects*numActivities), ] 
row = 1 
for (s in 1:numSubjects) { 
for (a in 1:numActivities) { 
result[row, 1] = uniqueSubjects[s] 
result[row, 2] = activity[a, 2] 
tmp <- cleaned[cleaned$subject==s & cleaned$activity==activity[a, 2], ] 
result[row, 3:numCols] <- colMeans(tmp[, 3:numCols]) 
row = row+1 
} 
} 
write.table(result, "data_set_averages.txt") 
