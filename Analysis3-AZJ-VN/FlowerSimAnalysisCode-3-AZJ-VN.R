# Vinay's Flower Similarity Experiment

# live dangerously, get rid of pesky warnings
oldw <- getOption("warn")
options(warn = -1)

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

# read the csv data files into a dataframe named "data"
files = list.files(pattern="*.csv")
data = sapply(files, read.csv, simplify=FALSE)%>% bind_rows(.id = "id")

# select variables we need for analysis 
trial_vars<- c("participant",
                "Flower1", "Flower2",
                "similarity", "response_time", "catchnumber", "catchnumberprac", "catchresponse", "catchtrialorder", "trialnumber")

# extract selected data columns
data <- subset(data, select = trial_vars)

# Catch Trial Check - find which trials were catch trials

get.catchtrial.info <- function(df.catchtrialorder){
  info <- (unique(df.catchtrialorder)[2])
  info <- as.character(info) # convert to string
  info <- str_sub(info, 2, -2) # remove the square brackets
  info <- str_split(info, pattern = fixed(',')) # get a vector of the catch trials in string format
  info <- info[[1]]
  #print(info) # testing
  info <- as.numeric(info) # convert to numeric
  return(info)
}

# add a column with catch trial info
add.catchtrial.info <- function(df){
  IDs <- unique(df$participant)
  colnames <- colnames(df)
  output.df <- df[FALSE,]
  for(ID in IDs){
    tempdf <- subset(df, participant == ID)
    catch.trials <- get.catchtrial.info(tempdf$catchtrialorder)
    tempdf$catch.trial <- ifelse(is.element(tempdf$trialnumber,catch.trials),TRUE,FALSE)
    #print(colnames(tempdf)) #testing
    output.df <- rbind(output.df,tempdf)
  }
  return(output.df)
  
}
data$catch.trials <- NA # need to add this here to make stuff work nicely later
test <- add.catchtrial.info(data)

# Check catch scores - find the scores of all catch trials 
catch_trial_checker <- function(datadf){
  
  subjectlist <- sort(unique(test$participant))
  print("Catch scores")
  for (participant in subjectlist){
    subjectdf <- test[which(test$participant == participant),] 
    catch_trials <- subset(subjectdf, catch.trial == TRUE)
    catch_num = nrow(catch_trials)
    catch_correct = nrow(subset(catch_trials, catchnumber == catchresponse))
    
    print(paste("Subject",participant,":",catch_correct,"/",catch_num))
  }
}

# Create data frame for trials - this excludes catch trials 
dftrials <- subset(data, !is.na(Flower2))

# Label participant number from 1 - 15 - adds label to dftrials 
dftrials$ID <- NA
subjectlist <- unique(dftrials$participant)
k= 0
for (participant in subjectlist){
  k = k + 1
  dftrials$ID[dftrials$participant == participant] <- k
}

# Check average Response Time

rt_avg <- function(data){
  return(mean(data$response_time))
}

rt_avg_check <- function(dftrials){
  
  subjectlist <- sort(unique(dftrials$participant))
  print("RT avg")
  for (participant in subjectlist){
    subjectdf <- dftrials[which(dftrials$participant == participant),] 
    rt = rt_avg(subjectdf)
    print(paste("Subject:",participant,"mean rt",rt))
  }
}

# For graphing 
dftrials$Flower1 <- as.character(dftrials$Flower1)
dftrials$Flower1 <- revalue(dftrials$Flower1, 
                            c(  "1" = 'flower1',
                                "2" = 'flower2',
                                "3" = 'flower3',
                                "4" = 'flower4',
                                "5" = 'flower5',
                                "6" = 'flower6',
                                "7" = 'flower7'))
dftrials$Flower2 <- as.character(dftrials$Flower2)
dftrials$Flower2 <- revalue(dftrials$Flower2, 
                            c(  "1" = 'flower1',
                                "2" = 'flower2',
                                "3" = 'flower3',
                                "4" = 'flower4',
                                "5" = 'flower5',
                                "6" = 'flower6',
                                "7" = 'flower7'))



flowers <- c('flower1', 'flower2','flower3','flower4','flower5','flower6','flower7')

# set the maximum and minimum dissimilarity values for later analysis
min_val = 0 # 0 = least similar
max_val = 7 # 7 = most similar

# Similarity judgment histogram
simhistplot <- function(datadf){
  
  plot <- ggplot(dftrials, aes(x = similarity)) + geom_bar(aes(y = ..prop..)) +
    scale_x_discrete(limits=c(0,1,2,3,4,5,6,7), name = 'Dissimilarity') +
    ylab('Frequency') + ylim(0,0.8)
  return(plot)
}
simhistplot(datadf)

simhistplot_summary <- function(datadf){
  
  datadf$ID <- as.character(datadf$ID) # necessary for visualisation
  
  plot <- ggplot(datadf, aes(x = similarity)) + 
    geom_line(stat='count',aes(y = ..prop..,group = ID),color='#CC9933') +
    geom_line(stat='count',aes(y = ..prop..),size=1.5) +
    scale_x_discrete(limits=c(0,1,2,3,4,5,6,7), name = 'Dissimilarity') +
    ylab('Frequency') + ylim(0,0.8)
  return(plot)
  
}
## simhistplot_summary(datadf) - gives ERROR Error in `$<-.data.frame`(`*tmp*`, ID, value = character(0)) : replacement has 0 rows, data has 7 

# reaction time for each similarity

rsplot <- function(datadf){
  
  plot <- ggplot(dftrials, aes(x= similarity, y=response_time)) + 
    stat_summary(fun.y = mean, geom = "bar") + 
    stat_summary(fun.data = mean_se, geom = "errorbar", size =0.5, aes(width=0.5)) +
    scale_x_discrete(limits=c(0,1,2,3,4,5,6,7), name = 'Dissimilarity') + ylab('Reaction Time (s)') +
    theme(legend.position = "none") +
    ylim(0,4) # anyone taking more than 4 seconds has probably mindwandered
  
  return(plot)
}
rsplot(datadf)

rsplot_all <- function(data){
  subjectlist <- sort(unique(dftrials$ID))
  par(mfrow=c(3,5))
  for (ID in subjectlist){
    subjectdf <- dftrials[which(dftrials$ID==ID),]
    plot <- rsplot(subjectdf)
    return(plot)
  }
}
rsplot_all(data)



# factor the dataframes for the plot function
# Do I really need this? Is it for asymmetry?
dissimdata2 <- function(dftrials, flowers){
  
  # refactor the levels so they can be plotted properly later if need be
  dftrials$Flower1 <- with(dftrials, factor(Flower1, levels = flowers))
  dftrials$Flower2 <- with(dftrials, factor(Flower2, levels = flowers))
  
  return(dftrials)
}

dissimdata2(dftrials, flowers)

# aggregates similarity judgments # also used later for correlation between passes
df2mat.full <- function(dftrials){
  
  
  # aggregate over the remaining columns of interest
  datadf <- aggregate(dftrials, by = list(dftrials$Flower1, dftrials$Flower2),FUN=mean)
  datadf$Flower1 <- datadf$Group.1
  datadf$Flower2 <- datadf$Group.2
  
  datadf = subset(datadf, select = c("Flower1","Flower2","similarity"))  # get rid of unnecessary columns
  datadf <- spread(datadf, Flower1, similarity)
  
  # convert the dataframe to a matrix - now datadf no longer exists
  datamatrix <- data.matrix(datadf)
  datamatrix <- datamatrix[,-1] # get rid of the labels in the first column, it messes up the code
  rownames(datamatrix) <- colnames(datamatrix)
  return(datamatrix)
  
}

# Dissimplot for all data - this makes the dissimilarity matrix

dissimplot_temporal <- function(subjectdf,flowers,dependent='color'){
  
  # refine data using function "dissimdata2 "
  datatemp <- dissimdata2(subjectdf, flowers)
  datatemp <- aggregate(datatemp, by = list(datatemp$Flower1, datatemp$Flower2),FUN=mean)
  
  plot <- ggplot(datatemp, aes(x = Group.1, y = Group.2)) +
    theme(axis.text.x = element_text(), axis.text.y = element_text(),
          axis.title.x = element_blank(), axis.title.y = element_blank(),
          plot.title = element_text(hjust = 0.5))
  
  # stuff that's standard across plot types
  plot <- plot + geom_raster(aes(fill = similarity)) +
    labs(title = 'Presented - Response Screen') +
    scale_fill_gradientn(colours = c("white","black")) +
    guides(fill=guide_legend(title="Dissimilarity"))
  return(plot)
}

dissimplot_temporal(dftrials, flowers) # creates the matrix

# Plot a dissmiliarity matrix for all subjects individually
dissimplot_temporal_subject <- function(dftrials, flowers){
  
  IDs <- unique(dftrials$ID)
  plot_list <- list()
  
  for (ID in IDs){
    #Subset data for the subject
    
    subjectdf = dftrials[which(dftrials$ID == ID),] 
    
    # refine data using function "dissimdata2 "
    datatemp <- dissimdata2(subjectdf, flowers)
    datatemp <- aggregate(datatemp, by = list(datatemp$Flower1, datatemp$Flower2),FUN=mean)
    
    
    plot <- ggplot(datatemp, aes(x = Group.1, y = Group.2)) +
      theme(axis.text.x = element_text(), axis.text.y = element_text(),
            axis.title.x = element_blank(), axis.title.y = element_blank(),
            plot.title = element_text(hjust = 0.5))+
      ggtitle(paste("Subject ID:", ID))
    
    
    # stuff that's standard across plot types
    plot <- plot + geom_raster(aes(fill = similarity)) +
      scale_fill_gradientn(colours = c("white","black")) +
      guides(fill=guide_legend(title="Dissimilarity"))
    
    plot_list[[ID]] <- plot
    
  }
  plot_grob <- arrangeGrob(grobs=plot_list)
  return(grid.arrange(plot_grob))
}

dissimplot_temporal_subject(dftrials, flowers) 

# CORRELATION BETWEEN PASSES 


matrixcor_pear <- function(dftrials){
  
  matrix1 <- df2mat.full(dftrials[which(dftrials$trialnumber<=49),])
  matrix2 <- df2mat.full(dftrials[which(dftrials$trialnumber>=50),])
  return(cor(c(matrix1), c(matrix2), method = "pearson"))
}

matrixcor_spear <- function(dftrials){
  
  matrix1 <- df2mat.full(dftrials[which(dftrials$trialnumber<=49),])
  matrix2 <- df2mat.full(dftrials[which(dftrials$trialnumber>=50),])
  return(cor(c(matrix1), c(matrix2), method = "spearman"))
}

matrixcor_spear(subjectdf)

pass_compare_list_plot <- function(dftrials){
  
  subjectlist <- sort(unique(dftrials$ID)) # obtain a list of all the subjects
  
  correlation_list <- vector() # array to store the values in
  
  for (ID in subjectlist){ # go through subject by subject
    subjectdf <-  dftrials[which(dftrials$ID == ID),]  # select the ID for subject of interest
    correlation_list <- c(correlation_list, (matrixcor_pear(subjectdf)))
    
    plot <- plot(correlation_list, main = '1st and 2nd pass Pearson correlation - r',
                 xlab='Participant',ylab='r',xlim=c(1,14),pch = 21, col="black")
    axis <- axis(1,seq(1,14,1))
  }
  return(correlation_list)
  return(plot)
  return(axis)
}

pass_compare_list_plot(dftrials)

#mean reaction time vs. catch trial score
rt_catch <- function (dftrials){
  
  subjectlist <- sort(unique(dftrials$ID)) # obtain a list of all the subjects
  
  for (ID in subjectlist){ # go through subject by subject
    tempdf <- subset(dftrials, participant== ID)
    dftrials$catch_trial_checker[dftrials$participant == ID] = unlist(catch_trial_checker(tempdf),use.names=FALSE)[1]
  }
  dftrials<- aggregate(dftrials, by=list(dftrials$participant), FUN = mean)
  return(dftrials)
}    
rt_catch(dftrials)

rt_catch_plot <- function(dftrials,xtype='linear',label=FALSE){
  
  dftrials <- rt_catch(dftrials)
  dftrials$subject <- as.character(dftrials$subject) # necessary for visualisation
  
  plot <- ggplot (dftrials, aes(x=response_time, y=catch_trial_checker)) +
    geom_point() + xlab ("Mean Reaction Time") + ylab("Catch Score")
  if(xtype == 'log'){
    plot <- plot + scale_x_continuous(trans='log10') + xlim(0,5000)
  } else{
    plot <- plot + xlim(0,5000)
  }
  plot <- plot + geom_smooth(method=lm) + ylim(0,1) # Linear Line of best fit
  #plot <- plot + geom_point(aes(color = subject))
  if (label){
    plot <- plot + geom_text(aes(label = subject),check_overlap=TRUE)
  }
  return (plot)
}
rt_catch_plot(dftrials,xtype='linear',label=FALSE)



