# Vinay's Flower Similairty and Preference Experiment

# load libraries
shhh <- suppressPackageStartupMessages # stops annoying warnings when loading libraries
library(tidyr)
library(plyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(MASS)
library(Matrix)
library(reshape2)
library(ape) # stats
library(vegan) # stats
library(RColorBrewer)
library(cocor)
library(DescTools)
library(reshape2)
library(grid)
library(ggplotify)
library(stringr)
library(tidyverse)
library(plot.matrix)

# read the csv data files into a dataframe named "pilotdata"
files = list.files(pattern="*.csv")
pilotdata = sapply(files, read.csv, simplify=FALSE)%>% bind_rows(.id = "id")

# obtain a list of all participants
participants <- data.frame(unique(pilotdata$participant))

# create dataframe to store catch accuracy and mean reaction times
resultsagg <- participants
resultsagg <- resultsagg %>% rename(participant = unique.pilotdata.participant.)

#Select variables we need for analysis. Keep seperate dataframes for catches and trials
trial_vars<- c( "participant",  "Flower1", "Flower2", "similarity", "response_time", "trialnumber") 

catch_vars<- c("participant", "response_time", "catchnumberprac", "catchnumber", "catchresponse", "catchtrialorder")

trialdata <- pilotdata[trial_vars]
catchdata <- pilotdata[catch_vars]

# tag what kind of data each row contains for each participant
trialdata$pass <- "practice"
trialdata$pass[8:56] <- "first pass"
trialdata$pass[57] <- "blank row"
trialdata$pass[58:106] <- "second pass"
trialdata$pass[107:108] <- "post expt responses"

# create dataframe to  put the mean responses of participants by comparison index
allcomps <- data.frame(c(1:4371))
allcomps <- allcomps %>% rename(comparisons = c.1.4371.)
stretchedmeans <- data.frame(c(1:4371))
stretchedmeans <- stretchedmeans %>% rename(comparisons = c.1.4371.)

# start FOR loop
for (i in c(1:nrow(participants))){
  
  # create dataframes for participant i's trial and catch data
  partitrials <- trialdata %>% filter(participant == participants[i,])
  catchitrials <- catchdata %>% filter(participant == participants[i,])

  # create a dataframe with 1-49 with the first and second passes in different columns.
  organised <- data.frame(c(1:49))
  organised$firstpasses <- partitrials$similarity[8:56]
  organised$firsttime <- partitrials$response_time[8:56]
  organised$secondpasses <- partitrials$similarity[58:106]
  organised$secondtime <- partitrials$response_time[58:106]
  organised$comparison <- partitrials$realcomparison[8:56]
  organised$meanresponse <- (organised$firstpasses + organised$secondpasses)/2
  organised$meantime <- (organised$firsttime + organised$secondtime)/2
  organised <- organised %>% rename(trialno = c.1.49.)
  organised$difference <- abs(organised$firstpasses - organised$secondpasses)
  
  #extract and clean up the coordinates of the catch trials
  catchcoords <- unique(catchitrials$catchtrialorder)
  catchcoords <- catchcoords[-c(1)]
  catchcoords <- str_sub(catchcoords, 2, -2)
  catchcoords <- str_split(catchcoords, pattern = fixed(","))
  catchcoords <- as.numeric(unlist(catchcoords))
  catchcoords <- sort(catchcoords)
  
  #make a dataframe with just the catch data in it, without all the empty lines
  propercatches <- catchitrials[-c(1:7),]
  propercatches <- propercatches[c(catchcoords),]
  
  propercatches$accuracy <- propercatches$catchnumber == propercatches$catchsimilarity
  correctcatches <- 0
  for (j in c(1:nrow(propercatches))){
    
    if (propercatches$accuracy[j] == TRUE){
      correctcatches <- correctcatches + 1
    }
  }
  
  spearman <- cor.test(x=organised$firstpasses, y=organised$secondpasses, method="spearman")
  spearman <- spearman$estimate[[1]]
  pearson <- cor.test(x=organised$firstpasses, y=organised$secondpasses, method="pearson")
  pearson <- pearson$estimate[[1]]
  #plots the participant responses
  responses <- ggplot(data=organised, aes(x=trialno)) +
    geom_line(aes(y=firstpasses, colour = "first pass")) + 
    geom_point(aes(y=firstpasses, colour = "first pass"))+
    geom_line(aes(y=secondpasses, colour = "second pass")) + 
    geom_point(aes(y=secondpasses, colour= "second pass"))+
    labs(title=paste("participant:", participants[i,], "\nrho =", spearman, "\nr =", pearson, sep=" "), y="similarity rating", x= "trial number")
  
  print(responses)
  
  resultsagg$spearman[i] <- spearman
  resultsagg$pearson[i] <- pearson
  resultsagg$meanrt[i] <- mean(organised$meantime)
  resultsagg$meandiff[i] <- mean(organised$difference)
  resultsagg$meancatchrt[i] <- mean(propercatches$catchRT)
  resultsagg$catchacc[i] <- sum(correctcatches/nrow(propercatches))
  stretchedmeans$newthing <- "NA"
  for (k in (1:nrow(stretchedmeans))){
    if(k %in% organised$comparison ){
      stretchedmeans$newthing[k] <- organised$meanresponse[which(organised$comparison==k)]
      
    }
  }
  
    names(stretchedmeans)[names(stretchedmeans) == "newthing"] <- participants[i,]
  #end of the big for loop  
}

stretchedmeans <- subset(stretchedmeans, select = -c(comparisons))
stretchedmeans <- data.frame(lapply(stretchedmeans,as.numeric))

# Make the similarity column have the average response on each comparison
# across the whole sample
# This will feed into a dissimilarity matrix
allcomps$similarity <- rowMeans(stretchedmeans, na.rm=TRUE)

ggplot(data=resultsagg, aes(x=meancatchrt, y=catchacc)) +
  geom_point() +
  labs(y="catch score accuracy", x="mean response time on catch trials (seconds)", title="catch scores")



  








