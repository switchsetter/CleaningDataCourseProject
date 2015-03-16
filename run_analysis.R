
## run_analysis.R

## Using data from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
## This script will:
## 
## 1. Merge the training and the test sets to create one data set.
## 2. Extract only the columns on the mean and standard deviation for each measurement.
## 3. Output a file "data_out.txt"  with the average of each
##    variable for each activity and each subject.
## 
## Usage:
## Get data from:
## https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
## Extract into your working directory
## load library("dplyr")
## Have this file in your working directory
## run run_analysis()

run_analysis <- function(){
      
      ## set the extracted directory
      extractedDir<-"UCI HAR Dataset"
      
      ## read the subjects    
      subject_test <- read.csv(paste(extractedDir,'/test/subject_test.txt',sep = ""),header = FALSE, sep = "", colClasses = "character")
      subject_train <- read.csv(paste(extractedDir,'/train/subject_train.txt',sep = ""),header = FALSE, sep = "", colClasses = "character")

      ## rename subject column
      colnames(subject_test)[1] <- "Subject"
      colnames(subject_train)[1] <- "Subject"
      
      ## read the X test data  
      X_data_test <- read.csv(paste(extractedDir,'/test/X_test.txt',sep = ""),header = FALSE, sep = "", colClasses = "character")
      
      ## read the X train data
      X_data_train <- read.csv(paste(extractedDir,'/train/X_train.txt',sep = ""),header = FALSE, sep = "", colClasses = "character")
      
      
      
      ## read the y test data   
      y_data_test <- read.csv(paste(extractedDir,'/test/y_test.txt',sep = ""),header = FALSE, sep = "", colClasses = "character")
      
      ## read the y train data
      y_data_train <- read.csv(paste(extractedDir,'/train/y_train.txt',sep = ""),header = FALSE, sep = "", colClasses = "character")
      
      ## read the the  label names and reduce to vector  
      col_labels<-read.csv(paste(extractedDir,'/features.txt',sep = ""),header = FALSE, sep = "", colClasses = "character")
      col_labels<-col_labels[,2]
      
      ## add the label names to X test data and to X train data data frame
      colnames(X_data_test) <- col_labels
      colnames(X_data_train) <- col_labels
      
      
      ## attach subjects
      X_data_test<-cbind(subject_test,X_data_test)
      X_data_train<-cbind(subject_train,X_data_train)
      
      
      
      ## add sequence to data frames for resorting after merge
      X_data_test<-cbind(q=seq(along = X_data_test[,1]), X_data_test)
      X_data_train<-cbind( q=seq(from=(length(X_data_test[,1])+1),to=(length(X_data_train[,1])+length(X_data_test[,1])) ), X_data_train)
      
      ## rename y data first column to AC
      colnames(y_data_test)[1] <- "AC"
      colnames(y_data_train)[1] <- "AC"
      
      
      ## add y data to the x data frames
      data_test<-cbind(y_data_test,X_data_test)
      data_train<-cbind(y_data_train,X_data_train)
      
      ## combine test and train data
      data<-rbind(data_test,data_train)
      
      ## read the acivity labels and rename    
      act_labels <- read.csv(paste(extractedDir,'/activity_labels.txt',sep = ""),header = FALSE, sep = "", colClasses = "character")
      colnames(act_labels)[1] <- "AC"
      colnames(act_labels)[2] <- "Activity"
      
      ## merge activity labels with data
      dataF<-merge(act_labels,data,all=TRUE)
      
      
      
      ## resort to original order
      dataF<-dataF[ order(dataF$q), ]
      
      dataF$q<-NULL
      dataF$AC<-NULL
      dataF$row.names<-NULL
      
      ## get column names to extract
      cols2<-col_labels[grep("mean", col_labels)]
      cols3<-col_labels[grep("std", col_labels)]
      
      ## extracted columns
      dataT<- dataF[,c('Subject','Activity',cols2,cols3)]
      
      
      ## change numeric columns to numeric
      dataT[, 1] <- sapply(dataT[, 1], as.numeric)
      dataT[, 3:81] <- sapply(dataT[, 3:81], as.numeric)
      dataT[, 2] <- sapply(dataT[, 2], as.factor)
      
      
     createDataSet(act_labels,dataT,cols2,cols3) 
         
}


## group by subject, activity and get the mean of each variable, write to data_out.txt
## 
createDataSet <-  function(activities,dataT,cols2,cols3){
      
      data<-data.frame()
      data2<-data.frame()
      temp<-data.frame()

      
      for(subject in 1:30 ){
            
            subjectFrame<-filter(dataT,Subject==subject)
            
            for( i in 1:length(activities[,1])){
                  tempRow<-NULL
                  tempRow2<-NULL
                  activityFrame<-filter(subjectFrame,Activity==activities[i,2])
                  tempRow<- colMeans(activityFrame[,3:81], na.rm = FALSE, dims = 1)      
                  tempRow2<-c(as.character(subject),activities[i,2])
                  temp[1,"Subject"]<-subject
                  temp[1,"Activity"]<-activities[i,2]
                  data <- rbind(data,tempRow)
                  
                  data2<-rbind(data2,temp)
            }
            
            
      }
      cols2new<-addToName(cols2)
      cols3new<-addToName(cols3)
      colnames(data) <- c(cols2new,cols3new)
      
      data2<-cbind(data2,data)
      
      
     
      
      write.table(data2,'data_out.txt',row.name=FALSE)
     
      
}

## add "Average of" to each string in passed vector
addToName <- function(v){
      
      ret<-character()
      for(s in  v){
            t<-paste('Average of ',s)
            ret <- c(ret, t)
            
      }
      return(ret)
      
}


