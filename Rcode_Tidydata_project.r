###########################################################################################################
# One of the most exciting areas in all of data science right now is wearable computing. Companies like Fitbit, Nike, and Jawbone Up are racing to develop 
# the most advanced algorithms to attract new users. Data is collected from the accelerometers from the Samsung Galaxy S smartphone.
 
# This R script - run_analysis.R does the following. 

# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

###########################################################################################################

# Read the training dataset - 7352 observations of 561 variables
training <- read.csv("./UCI HAR Dataset/train/X_train.txt", sep="", header=FALSE)
training[,562] <- read.csv("./UCI HAR Dataset/train/Y_train.txt", sep="", header=FALSE)  # Add the activity id
training[,563] <- read.csv("./UCI HAR Dataset/train/subject_train.txt", sep="", header=FALSE) # Add the subject id

# Read the testing dataset - 2947 observations of 561 variables
testing <- read.csv("./UCI HAR Dataset/test/X_test.txt", sep="", header=FALSE)
testing[,562] <- read.csv("./UCI HAR Dataset/test/Y_test.txt", sep="", header=FALSE)   # Add the activity id
testing[,563] <- read.csv("./UCI HAR Dataset/test/subject_test.txt", sep="", header=FALSE)  # Add the subject id

# Read the activity labels
activityLabels <- read.csv("./UCI HAR Dataset/activity_labels.txt", sep="", header=FALSE)
colnames(activityLabels) <- c("activity_id","activity_name")

# Read features
features = read.csv("./UCI HAR Dataset/features.txt", sep="", header=FALSE)

# Merge the training and test dataset
fulldata <- rbind(training,testing)

# Columns we want are the mean and standard deviation columns
mean_cols_required <- grep("-mean",features[,2])
std_cols_required <- grep("-std",features[,2])
features_required <- features[c(mean_cols_required,std_cols_required),"V1"]  # These are the column numbers that are required
feature_col_names <- features[c(mean_cols_required,std_cols_required),"V2"]  # Column names corresponding to the column numbers
features_required <- c(features_required,562,563)

# Remove the unwanted columns
fulldata <- fulldata[,features_required]

# Label the columns appropriately
variable_names <- levels(feature_col_names)[as.numeric(feature_col_names)]  # Change the factor to a character string
colnames(fulldata) <- c(variable_names,"Activity","Subject")

# Write the first output file
# write.table(fulldata, "tidy_1.txt", sep="\t",row.names=F)

# Create the tidy dataset
tidy = aggregate(fulldata, by=list(activity = fulldata$Activity, subject=fulldata$Subject), mean)
tidy = tidy[,c(-82,-83)]  # Remove the last 2 columns which are redundant

# Adding descriptive name for the activity
tidy <- merge(activityLabels,tidy, by.x="activity_id",by.y="activity")
tidy <- tidy[,-1]

# Write the output file
write.table(tidy, "tidy.txt", sep="\t",row.names=F)

