#### Getting & Cleaning Data - Project Assignment
### run_analysis.R
### Created on 10/9/2016 
#downloaded Zip file into data folder. 

# read the files 

# Read Test Tables into R 

xtst <- read.table("y_test.txt")
ytst <- read.table("y_test.txt")
stst <- read.table("subject_test.txt")

#checked the read of the test tables
head(xtst,2)
head(ytst,2)
head(stst,2)

# Read Training Tables into R

xtrn <- read.table("X_train.txt")
ytrn <- read.table("y_train.txt")
strn <- read.table("subject_train.txt")

#checked the read of the train tables

head(strn,2)
head(ytrn,2)
head(xtrn,2)

## Obtaining Column names

colnames(xtrn) <- t(ftr[2])
colnames(xtst) <- t(ftr[2])

# Appending Activities and Participants columns to the Test data
xtst$activities <- ytst[,1]  
xtst$participants <- stst[,1] 

# Appening Activities and Participants columns to the Train data  

xtrn$activities <- ytrn [,1]
xtrn$participants <- strn [,1]

# Checking the columns were added to Training data
colnames(xtrn)
head(xtrn,2)

# Checking the columns were added to Test data
colnames(xtst)
head(xtst,2)
### =================================================================


### Question 1:  Merges the training and the test sets to create one data set.

## Binding the rows from the two datasets 
MD3 <- rbind(xtrn, xtst)
View MD3 

### On checking the columns of Training and Testing data sets found duplicate columns
## Checking duplicate names 

duplicated(colnames(MD3),value = TRUE)

### writing a physical file to capture all column names to see the duplicates -- just an extra check 
write.table(colnames(MD3),"MD3ColNames.csv",row.names = FALSE)


## Getting rid of duplicates
MD4 <- MD3 [, !duplicated(colnames(MD3))]

## Visually checking MD4 for duplicates
View(MD4)


# Ensured No duplicate Columns... 
duplicated(colnames(MD4),value = TRUE)


### writing a physical file to capture all column names to ensure duplicates removed -- just an extra check 
write.table(colnames(MD4),"MD4ColNames.csv",row.names = FALSE) 

### Writing a physical file with the merged data 
write.table (MD4, file="MergedData-MD4.csv", row.names=FALSE)

###### MD4 - is the final Merged Data #####################

#### ==========================================================


### Question 2: Extracts only the measurements on the mean and standard deviation for each measurement. 
### Using MD4 merged data for this section

### Extracting the Means related columns

Mean <- grep("[mM]ean[()]", names(MD4), value = FALSE)

# Displaying Mean for a visual check
Mean 

# Creating a Matrix of data that includes all Means related Columns
MeanMatrix = MD4[,Mean]

### Extracting Standard Deviation related columns.

SD <- grep("std()", names(MD4), value = FALSE)

## Displaying SD for a visual check
SD

## Creating a Matrix of data that includes all SD Related Columns
SDMatrix = MD4[,SD]

### Appending SD Related Columns to Mean Related Columns 
MeanSD <- append(Mean,SD)

## Creating a Matrix of data that includes all Means & SD Related Columns
MeanSDMatrix = MD4[,MeanSD]


### writing a physical file that captures data with all Means and SD related columns -- just an extra check 
write.table (MeanSDMatrix, file="MeanSDMatrix.csv", row.names=FALSE)

###########===========================================================


### Question 3: Uses descriptive activity names to name the activities in the data set
### continued using MD4 Merged data for this section


### Visually checking how activities are listed again, and turning the activities to character for manipulation 
head(MD4,2)
MD4$activities <- as.character(MD4$activities)

### using descriptive words for the numbered activities adn updating the fields in MD4 

MD4$activities[MD4$activities==1] <- "Walking"
MD4$activities[MD4$activities==2] <- "Walking Upstairs"
MD4$activities[MD4$activities==3] <- "Walking Downstairs"
MD4$activities[MD4$activities==4] <- "Sitting"
MD4$activities[MD4$activities==5] <- "Standing"
MD4$activities[MD4$activities==6] <- "Laying"


#### Finally checking that the updates have gone through & Visually checked the display 

head(MD4,2)

#### ============================================

#### Question 4: Appropriately labels the data set with descriptive variable names 
###  Continue using MD4 Merged data for this section

### Identified some of the variables started with t for time, f for freq and used mag for magnitude.
### Changed them to be descriptive

names(MD4)
names(MD4) <- gsub("^t", "time", names(MD4))
names(MD4) <- gsub("^f", "Freq", names(MD4))
names(MD4)
names(MD4) <- gsub("Mag", "Magnitude", names(MD4))

#### Now want to change the Participants currently displayed as numbers 1, 2, 5 etc to something more verbose
#####

for (i in 1:30) {
MD4$participants[MD4$participants == i] <- paste("Participant", as.character(i))
}

### Checking Participants are appropriately formatted visually

head(MD4,2)
tail(MD4,10)

##### ==========================================

#### Question 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for #### each activity and each subject. ---#


TidyMD <- data.table(MD4)

### grouping the data based on participants and Activities

TidyMDgrouped <- TidyMD[,lapply(.SD,mean), by='participants,activities']

### Finally writing a physical file with the data grouped by participant and activities. 

write.table(TidyMDgrouped,file="TidyMDgroupedfinal.csv", row.names=FALSE)

 
################ Completed Assignment ########################################




