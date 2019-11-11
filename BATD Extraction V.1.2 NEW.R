#Batch analysis of Tactile Data (BATD) -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Extract

#To do ------------------------------------------------------------------------------------------------------------------------------

#readme ----
#This code was developed by Jason He, at Johns Hopkins University, Department of Radiology
#If you are having difficulties with using this code or you are wanting to adapt it for your personal use, feel free to contact me at my personal email: jasonhe93@gmail.com 
#The purpose of this code is to extract the relevant performance variables out of the .txt files that the Cortical Metrics Brain Gauge outputs
#This code is generally meant to be used in combination with 'BATD Analayze and Plot' in the sense that this code ('BATD Extraction') extracts the data, which is then analyzed and plotted using 'BATD Analyze and Plot'


#To run this code, you will have to change the working directory (under #setwd) to the directory in which all your participant's text files are kept
#Also, to save the output, I recommend making a separate folder (referred to as the master directory at the end of the code)


#UPDATES ------------------
#Updated on November 6th (2019) to account for duration discrimination and dual-staircase amplitude discrimination 
#The code now outputs 'duration' of stim1 and stim2, as well as astim1 and astim2

#Install packages, remove '#' in order to run ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#R requires that you have a series of packages installed in order to run the code below.
#In order to run the code, you will need to install each of the packages below if you haven't already done so
#Simply toggle on the install.packages code by removing the '#' in front of the code, and put it back once you've installed it 
#note, the packages installed here aren't necessarily used, they're just the general packages I have installed when I use R
#When I have more time, I will remove the packages that are not in use (in a later version)

#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("ggpubr")
#install.packages("sm")
#install.packages("Hmisc")
#install.packages("plyr")
#install.packages("Rmisc")
#install.packages("retimes")
#install.packages("data.table")
#install.packages("tidyr")
#install.packages("lme4")
#install.packages("multcomp")
#install.packages("pastecs")
#install.packages("effects")
#install.packages("car")
#install.packages("DataCombine")
#install.packages("gridExtra")
#install.packages("leaps")
#install.packages("ppcor")
#install.packages("ggm")

#read in packages ----
library(dplyr)
library(ggplot2)
library(ggpubr)
library(Hmisc)
library(plyr)
library(Rmisc)
library(retimes)
library(data.table)
library(tidyr)
library(lme4)
library(multcomp)
library(pastecs)
library(effects)
library(car)
library(DataCombine)
library(gridExtra)
library(leaps)
library(ppcor)
library(ggm)


#setwd ---- 
setwd("")#set the working directory to the folder containing all the participant's .txt files

#Developer working directories -----------
#setwd("~/Desktop/Toronto Data/ARBA1")#set the working directory to the folder containing all the participant's .txt files
setwd("~/Dropbox/Documents/Data repository/Tactile Data/Raw/New Format/CCH")
site <- "CCH"

participantDirectory <- getwd()
#site <- ""
#site <- "University of Toronto" #insert site name (used to create a column which identifies the site at which the data was collected)
dir.create("output") #set the wd to the folder where you wish to save the output data to
setwd(paste0(participantDirectory,"/output")) #automatically creates a folder in that directory named 'output' - if you already have a folder named output, ignore this code. 
outputDirectory <- getwd()

#run below to extract data ----
setwd(paste0(participantDirectory))
participants <- list.files(pattern="*.txt") #Read in the participant ids for each of the folders
participant_performance <- list() #Create a list external to the loop for the performance of each participant to be put in

for(p in 1:length(participants)){
setwd(paste0(participantDirectory))
output <- read.csv(participants[p], header = FALSE) #read in participant's file
#House cleaning ---- 
output$V1 <- as.character(output$V1) #turn column 1 into a character
output$V1 <- gsub(" ","",output$V1) #replace all blank spaces in column 1
tempo <- t(as.data.frame(strsplit(output$V1,":"))) #split the string by ':' and transpose it to long format
rownames(tempo) <- c() #clear the row numbers
temp <- as.data.frame(tempo) #turn the output into a dataframe

#Break output into separate protocols ----
#Use "X" to mark the beginning of each protocol
temp$V4 <- ifelse(temp$V1=="date", "X",NA) #All of the files start with date, we will use date as marker of the start of each protocol - create a column which marks date with a 'X'
protocols <- (which(temp$V4=="X")) #This code tells us all of the instances in *which* 'X' occurs
protocols <- as.list(protocols) 

list <- list() #Create a list external of the for loop for breaking the output into seperate protocols 
for (i in (1:length(protocols))){
if(i==length(protocols)){break} #Note to self, if it's 14, then we want i+1 to be something else (i.e., the end of the file)
list[[i]] <-  temp[(paste(protocols[i])):(paste(protocols[i+1])),]
} #loop for breaking the output into separate protocols 
list[[i]] <- temp[(paste(protocols[i])):nrow(temp),] #puts the last protocol into the list (for loop above cannot account for last protocol)

#Put each separate protocol into a dataframe for ease of subsequent analyses ----  
ProtocolOutputList <- list() #Create a list external to the for loop to insert each participant's details and raw task performance 
for (i in (1:length(list))){
output <- list[[i]]
rownames(output) <- c() #clear the row numbers

#Protocol details
protocol <- as.character(output$V2[output$V1=="protocol"])
date <- substr(gsub("T","",(output$V2[output$V1=="date"][1])), 1,10)
numberofPracticeTrials <- as.character(output$V2[output$V1=="numTrials"][1])
numberofTestTrials <- as.character(output$V2[output$V1=="numTrials"][2])


#interstimulus or intertrial intervals
#The data collected at JHU had the time between stimuli referred to as 'intervalBetwenAdaptAndTest'
#The data collected from University of Calgary had the time between stimuli referred to as 'ITI'
#The code adapts to either below, however, I have opted to refer to this as interstimulus interval (ISI) rather than ITI 
#The reason for this is that ISI is technically more correct here than ITI, since ITI would be the time between trials, whereas we are interested in the time between stimuli if they were delivered sequentially


if("TRUE" %in% count(output$V1=="intervalBetweenAdaptAndTest")$x){
ISI <- as.character(output$V2[output$V1=="intervalBetweenAdaptAndTest"][1])
}

if("TRUE" %in% count(output$V1=="ITI")$x){
ISI <- as.character(output$V2[output$V1=="ITI"][1])
if(ISI=="["){
ISI <- NA
}
}

stim1amplitude <- as.character(output$V2[output$V1=="amplitude"])[1]
stim2amplitude <- as.character(output$V2[output$V1=="amplitude"])[2]
astim1amplitude <- as.character(output$V2[output$V1=="amplitude"])[3]
astim2amplitude <- as.character(output$V2[output$V1=="amplitude"])[4]

stim1duration <- as.character(output$V2[output$V1=="duration"])[1]
stim2duration <- as.character(output$V2[output$V1=="duration"])[2]
astim1duration <- as.character(output$V2[output$V1=="duration"])[3]
astim2duration <- as.character(output$V2[output$V1=="duration"])[4]


taskDetails <- as.data.frame(cbind(protocol, date, numberofPracticeTrials, numberofTestTrials, ISI, 
stim1amplitude, stim2amplitude, 
astim1amplitude, astim2amplitude,
stim1duration, stim2duration,
astim1duration, astim2duration))

#Participant details
id <- as.character(output$V2[output$V1=="number"][1])
race <- as.character(output$V2[output$V1=="race"])
gender <- as.character(output$V2[output$V1=="gender"])
handedness <- as.character(output$V2[output$V1=="handedness"])
birthYear <- as.character(output$V2[output$V1=="birthYear"])
number <- as.character(output$V2[output$V1=="number"]) #is this participant id?

participantDetails <- as.data.frame(cbind(number, race, gender, handedness, birthYear))

#Performance details
value <- as.character(output$V2[output$V1=="value"])
expected <- as.character(output$V2[output$V1=="expected"])
response <- as.character(output$V2[output$V1=="response"])
correctResponse <- as.character(output$V2[output$V1=="correct"])
responseTime <- as.character(output$V2[output$V1=="responseTime"])

Performance <- as.data.frame(cbind(value, expected, response, correctResponse, responseTime))

All <- cbind(id, taskDetails, participantDetails, Performance)
All$trialNumber <- 1:nrow(All)

ProtocolOutputList[[i]] <- All
} #loop for putting each protocol into a dataframe format for ease of subsequent analyses
participantTactileData <- do.call(rbind.data.frame, ProtocolOutputList) 

#Change correctResponse to a 0 or 1 numeric (currently its in true or false, I just want to standardise this between the old and new format, also string descriptions are not useful here)
participantTactileData$correctResponse <- as.numeric(participantTactileData$correctResponse) #make correctResponse numeric, making true = 2, and false = 1
participantTactileData$correctResponse <- participantTactileData$correctResponse - 1 #subtract 1, so that 1 = correct, 0 = incorrecty

#Label protocol with names ----
#These are the standard protocol names for the protocols described in Puts et al., (2013) 
#Simple and Choice Reaction times
participantTactileData$protocolName[participantTactileData$protocol==801] <- "Simple Reaction Time" 
participantTactileData$protocolName[participantTactileData$protocol==800] <- "Choice Reaction Time" 

#Static Detection Thresholds with and without adaptation (with ISI 30 and 100)
participantTactileData$protocolName[participantTactileData$protocol==900 & participantTactileData$stim1amplitude==0] <- "Static Detection Threshold" 
participantTactileData$protocolName[participantTactileData$protocol==100 & participantTactileData$stim1amplitude==0] <- "Static Detection Threshold" 
participantTactileData$protocolName[participantTactileData$protocol==910 & participantTactileData$ISI==5000] <- "Static Detection Threshold"
participantTactileData$protocolName[participantTactileData$protocol==910 & participantTactileData$ISI==30] <- "Static Detection Threshold with Adaptation ISI 30" 
participantTactileData$protocolName[participantTactileData$protocol==910 & participantTactileData$ISI==100] <- "Static Detection Threshold with Adaptation ISI 100" 

#Duration discrimination
participantTactileData$protocolName[participantTactileData$protocol==350] <- "Duration Discrimination" 

#Dynamic detection threshold
participantTactileData$protocolName[participantTactileData$protocol==713] <- "Dynamic Detection Threshold" 

#Amplitude Discrimination Threshold with and without adaptation (with single site and dual site)
participantTactileData$protocolName[participantTactileData$protocol==900 & participantTactileData$stim2amplitude!=0] <- "Amplitude Discrimination Threshold without Adaptation" 
participantTactileData$protocolName[participantTactileData$protocol==171 & participantTactileData$astim2amplitude==100] <- "Amplitude Discrimination with Single Site Adaptation" 
participantTactileData$protocolName[participantTactileData$protocol==171 & participantTactileData$astim1amplitude==100] <- "Amplitude Discrimination with Dual Site Adaptation" 

participantTactileData$protocolName[participantTactileData$protocol==171 & participantTactileData$astim1amplitude==200] <- "Dual Staircase Amplitude Discrimination (up)" 
participantTactileData$protocolName[participantTactileData$protocol==171 & participantTactileData$astim2amplitude==200] <- "Dual Staircase Amplitude Discrimination (down)" 

#Simultaneous and Sequential requency Discrimination 
participantTactileData$protocolName[participantTactileData$protocol==920] <- "Simultaneous Frequency Discrimination" 
participantTactileData$protocolName[participantTactileData$protocol==925] <- "Sequential Frequency Discrimination" 

#Simultaneous and sequential Amplitude Discrimination 
participantTactileData$protocolName[participantTactileData$protocol==105] <- "Simultaneous Amplitude Discrimination" 
participantTactileData$protocolName[participantTactileData$protocol==100 & participantTactileData$stim1amplitude==100] <- "Sequential Amplitude Discrimination" 

#Temporal Order Judgement with and without carrier 
participantTactileData$protocolName[participantTactileData$protocol==930] <- "Temporal Order Judgement" 
participantTactileData$protocolName[participantTactileData$protocol==931] <- "Temporal Order Judgement with Carrier"

#Duration Discrimination 
participantTactileData$protocolName[participantTactileData$protocol==950] <- "Duration Discrimination"

#University of Calgary patch
if(site=="University of Calgary"){
participantTactileData$protocolName[participantTactileData$protocol==900] <- "Sequential Amplitude Discrimination" 
participantTactileData$protocolName[participantTactileData$protocol==905] <- "Simultaneous Amplitude Discrimination" 
}

#Site ----------------------------------------------
participantTactileData$site <- site

#write the standardised output for each participant to the master depository for all tactile data ----
setwd(paste0(outputDirectory))
#participantid <- substr(participants[p],1,nchar(participants[p])-15) #create a participant id variable
participantid <- gsub(".txt","", participants[p])
participantTactileData$Format <- "NF"
participantTactileData <- participantTactileData[,c(1,2,25:28,3:24)] #Reorder columns so that protocol details are closer to the start of the file (further left)

#Session ----------------
#There are some cases where participants completed the same protocol twice, we need to be able to differentiate whether it was their first or second attempt
#Note that the attempts appear to be in chronological order

Table <- as.data.frame(table(participantTactileData$protocolName))
Table <- Table[Table$Freq>25,] #Identify any of the protocols that have more than 25 trials (these must be protocols where the participant did it twice, since no protocol has more than 25 trials)


if(nrow(Table)>0){
ProtocolsAttemptedMoreThanOnce <- list()
for(i in 1:length(Table$Var1)){ #for all the protocols that were completed more than once 
temp <- participantTactileData[participantTactileData$protocolName %in% Table$Var1,] #narrow dataframe to those completed more than once
temp <- temp[temp$protocolName==Table$Var1[i],] #split off the nth protocol that was completed more than once (denoted by "i)

sessions <- which(temp$trialNumber==1) #identify the start of the protocol (trial number one)

list <- list() #create an external list 


for (s in 1:length(sessions)){ #for the given the number of attempts or 'sessions'
if(is.na(sessions[2])){
  sessions[2] <- 1
}else{
  triallength <- sessions[2] - sessions[1] #identify the trial length
}
tempo <- temp[sessions[s]:(sessions[s]+triallength),] #here it is just, 
tempo <- as.data.frame(tempo)[1:triallength,] #convert to dataframe and remove last row (it's ane extra row, unsure why)
tempo$session <- s #label the session number as 's', which updates every loop 
list[[s]] <- tempo #put the reduced and labelled session into a list
}

temp <- rbindlist(list) #row bind the list - adding all the sessions of the same protocol together
ProtocolsAttemptedMoreThanOnce[[i]] <- temp #put the rowbinded sessions into this list

}

moreThanOnce <- do.call(rbind.data.frame, ProtocolsAttemptedMoreThanOnce) #rowbind all the sessions that were completed more than once and save them as a dataframe
once <- participantTactileData[!participantTactileData$protocolName %in% Table$Var1,] #These are the protocols which have NOT been completed more than once

}

if(nrow(once)>1){
once$session <- 1
}

participantTactileData <- as.data.frame(rbind.fill(moreThanOnce, once)) #combine sessions completed more than once with those completed just once 

write.csv(participantTactileData, file = paste0("BATD_NF_",participantid,".csv")) #save and write
}



