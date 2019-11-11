#Batch analysis of Tactile Data (BATD) -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Analyze and Plot 

#TO DO LIST ------
#I want to eventually rename sessions to blocks and add in a column for 'Session', which is based on the date (currently block is session)

#readme ----
#This code was developed by Jason He, at Johns Hopkins University, Department of Radiology
#If you are having difficulties with using this code or adapting it for your personal use, please contact me at my personal email: jasonhe93@gmail.com

#To run this code, you will have to change the working directory (under #setwd & visualisation) to the directory in which all your participant's text files are kept
#The latest version now creates subfolders to save the combined .csv file with each participant's performance data and plots
#the combined .csv file is saved in the directory of your home direcotry + "/Combined"
#similarly, the plots are saved in the directory of your home directory + "/Plots"

#Also under the section "setwd & visualisation, I've built in the ability to turn off the plotting. 
#This is because the plotting is time consuming and will strain your computer
#Set visualise to 0 if you don't want to plot, set it to 1 if you want to plot

#Updates ---------------------------------------
#V1.2. -----
#code has been updated to accomodate University of Calgary, University of Toronto and other POND based projects
#This update involved changing the code a bit to account for the possibility of multiple sessions (i.e., blocks) for a given participant (since they do SRT twice for the POND projects)
#The code has also been updated to make the plots look nicer - this included the removal of the gray gridded background, and the change of the dot colors to blue and red (easier to discern)
#horizontal lines now indicate thresholds (solid lines) and medianRT (dashed lines)
#The code is also now more specified for each site, so that protocols that were NOT completed at a given site are not plotted or added to the final dataframe (otherwise there are too many columns with NAs, making it messy for the end user)
#Other than that, the layout of the code is a little bit nicer, with more clearly defined sections for each protocol if they need to be edited
#Importantly, the end-user can still just change the input directory and make visualise = 1 or 0 and simply run the code


#Install packages, remove '#' in order to run ----
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
library(stringr)

#setwd & visualisation ---- 
inputDirectory <- ("") #set the working directory to the folder containing all the participant's .txt files (see examples below)
visualise <- 1 #set visualise to 1 if you don't want plots - *warning* plots are GPU intensive and can take a lot of time, especially with large datasets
#inputDirectory <- ("~/Dropbox/Documents/Projects/GABARB Tactile/Toronto Data/Test/output") #Used to test changes to code 
#inputDirectory <- ("~/Dropbox/Documents/Projects/GABARB Tactile/Toronto Data/ARBA1/output") 
#inputDirectory <- ("~/Dropbox/Documents/Projects/GABARB Tactile/Toronto Data/ARBA2/output") 
#inputDirectory <- ("~/Dropbox/Documents/Projects/GABARB Tactile/Toronto Data/ARBA3/output") 
inputDirectory <- ("~/Dropbox/Documents/Data repository/Tactile Data/Raw/New Format/CCH/output") #Cinccinnati Children's Hopsital (Tourette's Project)

#Analyze & Plot Data (caution: avoid editing) -----------------------
#setwd ----
setwd(inputDirectory) #set working directory to user selected directory
baseDirectory <- getwd() #set baseDirectory to user selected directory

#Read in the output file ----
participants <- list.files(pattern="*.csv") #Make sure there are only files in the wd

#Loop the extraction process for participants ----
participant_performance <- list() #Create a list external to the loop for the performance of each participant to be put in
allSessions <- list() #Create a list external to the loop for the performance of each participant for each session to be put in (only relevant for multi-session designs)
SessionPlots <- list() #Create a list external to the loop for the plots of each participant for each session to be put in (only relevant for multi-session designs)

for(p in 1:length(participants)){ #LOOP STARTS HERE, p is participant
temp <- read.csv(participants[p], header = TRUE)
temp <- temp[temp$session==1,]
sessions <- unique(temp$session) #identify the number of sessions completed

#Session control ------
for(s in sessions){ #This for loop is used to control situations where there are more than one session (sessions are analyzed separately)
temp <- read.csv(participants[p], header = TRUE) #read in the participant data into a temporary dataframe called 'temp'
temp <- temp[temp$session==s,] #subset to the current session 's' (a numerical value from 1 to number of sessions)
protocolsCompleted <- as.character(unique(temp$protocolName)) #Make a list of all the protocols completed by a given participant for a given session

#Some projects had coding for the number of practice trials that was different to other projects, this code accounts for it  -----
if(temp$site=="University of Toronto" & temp$numberofPracticeTrials>5){ #if there were more than 5 practice trials (appears to be an error for some projects)
temp$numberofPracticeTrials <- 2 #make the number of practice trials 2 (appears to be the correct value for the number of trials based on observation)
#It is likely that the 'University of Toronto' site specifying code is unecessary. Any case where there are more than 5 practice trials should only have 2 (based on observation)
}

#General commentary regarding extraction of performance variables ----
  #For practically every protocol, we estimate the Median RT and Accuracy in the exact same way
  #Where the protocols differ is how threshold is estimated, the way that threshold is currently estimated is based on Puts et al., (201X) work, 
    #rather than the defaults provided by Cortical Metrics
  
  #If a participant did NOT complete a given protocol (listed in protocolsCompleted), then the performance outcomes are still presented, but are given 'NA' as the value

#Calculate performance for each task IF they had completed that task, else NA ----

#### SIMPLE REACTION TIME #### ----------------------------------------------------------------------------
if("Simple Reaction Time" %in% protocolsCompleted){
#House keeping (cleaning up variables and variable types) ----
Data <- temp[temp$protocolName=="Simple Reaction Time" & !is.na(temp$protocolName),] #Subset to relevant protocol
Data <- Data[1:20,] #TEMPORARY FIX - NEED TO ACCOUNT FOR MULTIPLE SESSIONS
Data$trialNumber <- 1:nrow(Data) #add trialNumber column
Data$responseTime <- as.numeric(as.character(Data$responseTime)) #turn responseTime to numeric
Data$correctResponse <- as.numeric(as.character(Data$correctResponse)) #turn correctResponse to numeric
Data$value <- as.numeric(as.character(Data$value)) #turn string variables into numeric

#Performance analysis ----
#General variables ---- ----
medianRT_SRT <- median(Data$responseTime) 
accuracy_SRT <- (sum(Data$correctResponse)/nrow(Data))*100

#Key variables ----
if(sum(Data$correctResponse) > 6){ #They have to have at least more than 6 correct responses, if not, chances are they've completed the task wrong, where it's better to not have estimated the value at all 
correctResponses <- Data$responseTime[Data$correctResponse==1]
middle <- length(correctResponses)/2
median6 <- correctResponses[(middle-3):(middle+2)]
meanRT_SRT <- mean(median6) #mean of the median 6

} else {
meanRT_SRT <- NA
}

#Column bind variables -----
SRT <- cbind(accuracy_SRT, medianRT_SRT, meanRT_SRT)

#Visualisation ----
if(visualise==1){#Plot ----
plot_SRT1 <- ggplot(Data, aes(x=trialNumber, y=responseTime, color=as.factor(correctResponse)))+
geom_point(size =3) +theme(legend.position="none") +
labs(y = "Response Time (ms)", x = "Trial Number") +
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+
scale_color_manual(values = c("royalblue", "red"))+geom_hline(linetype = "dashed", yintercept=medianRT_SRT) 
#Note that the y-axis is not limited here so that people can see outlier RTs

#Descriptive text
text = paste("\n", "Median RT all across trials:", "\n", round(medianRT_SRT, digits = 2), "\n",
"\n", "ACC across all trials:", "\n",100-round(accuracy_SRT, digits = 2) , "% \n")

text_SRT <- ggplot() +
annotate("text", x = 4, y = 25, label = text) +
theme_bw() +
theme(panel.grid.major=element_blank(),
panel.grid.minor=element_blank(),
panel.border=element_blank(),
axis.text = element_blank(),
axis.ticks = element_blank(),
axis.title = element_blank())

#Plot with text
plots_SRT <- ggarrange(plot_SRT1, text_SRT)
plots_SRT <- annotate_figure(plots_SRT, top = text_grob("Simple Reaction Time", color = "black", face = "bold"))

}} else { #if participant didn't complete task, make all variables NA and plot NA
accuracy_SRT <- NA
medianRT_SRT <- NA
meanRT_SRT <- NA

SRT <- cbind(accuracy_SRT, medianRT_SRT, meanRT_SRT)

text = paste("Participant did not complete Simple Reaction Time protocol")
plots_SRT <- ggplot() +
annotate("text", x = 4, y = 25, label = text) +
theme_bw() +
theme(panel.grid.major=element_blank(),
panel.grid.minor=element_blank(),
panel.border=element_blank(),
axis.text = element_blank(),
axis.ticks = element_blank(),
axis.title = element_blank())}

#### CHOICE REACTION TIME #### ----------------------------------------------------------------------------
if("Choice Reaction Time" %in% protocolsCompleted){
#House keeping ----
Data <- temp[temp$protocolName=="Choice Reaction Time" & !is.na(temp$protocolName),] #Subset to relevant protocol
Data <- Data[1:20,] #TEMPORARY FIX - NEED TO ACCOUNT FOR MULTIPLE SESSIONS
Data$trialNumber <- 1:nrow(Data) #add trialNumber column
Data$responseTime <- as.numeric(as.character(Data$responseTime)) #turn responseTime to numeric
Data$correctResponse <- as.numeric(as.character(Data$correctResponse)) #turn correctResponse to numeric
Data$value <- as.numeric(as.character(Data$value)) #turn string variables into numeric

#Performance analysis ----
#General variables ----
medianRT_CRT <- median(Data$responseTime)
accuracy_CRT <- (sum(Data$correctResponse)/20)*100

#Key variables
if(sum(Data$correctResponse) > 6){
correctResponses <- Data$responseTime[Data$correctResponse==1]
middle <- length(correctResponses)/2
median6 <- correctResponses[(middle-3):(middle+2)]
meanRT_CRT <- mean(median6) #mean of the median 6
} else {
meanRT_CRT <- NA
}

#Column bind variables
CRT <- cbind(medianRT_CRT, accuracy_CRT, meanRT_CRT)

#Visualisation ----
if(visualise==1){#Plot ----
plot_CRT1 <- ggplot(Data, aes(x=trialNumber, y=responseTime, color=as.factor(correctResponse)))+
geom_point(size =3) +theme(legend.position="none") +
labs(y = "Response Time (ms)", x = "Trial Number")+
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+
scale_color_manual(values = c("royalblue", "red"))+geom_hline(linetype = "dashed", yintercept=medianRT_CRT)

#Descriptive text
text = paste("\n", "Median RT all across trials:", "\n", round(medianRT_CRT, digits = 2), "\n",
"\n", "ACC across all trials:", "\n",100-round(accuracy_CRT, digits = 2) , "% \n")

text_CRT <- ggplot() +
annotate("text", x = 4, y = 25, label = text) +
theme_bw() +
theme(panel.grid.major=element_blank(),
panel.grid.minor=element_blank(),
panel.border=element_blank(),
axis.text = element_blank(),
axis.ticks = element_blank(),
axis.title = element_blank())

#Plot with text
plots_CRT <- ggarrange(plot_CRT1, text_CRT)
plots_CRT <- annotate_figure(plots_CRT, top = text_grob("Choice Reaction Time", color = "black", face = "bold"))
}} else { #if particpant didn't complete task, make all variables NA and plot NA
accuracy_CRT <- NA
medianRT_CRT <- NA
meanRT_CRT <- NA

CRT <- cbind(medianRT_CRT, accuracy_CRT, meanRT_CRT)

text = paste("Participant did not complete Choice Reaction Time protocol")
plots_CRT <- ggplot() +
annotate("text", x = 4, y = 25, label = text) +
theme_bw() +
theme(panel.grid.major=element_blank(),
panel.grid.minor=element_blank(),
panel.border=element_blank(),
axis.text = element_blank(),
axis.ticks = element_blank(),
axis.title = element_blank())}

#### STATIC DETECTION #### ----------------------------------------------------------------------------
if("Static Detection Threshold" %in% protocolsCompleted){
#House keeping ----
Data <- temp[temp$protocolName=="Static Detection Threshold" & !is.na(temp$protocolName),] #Subset to relevant protocol
numberofPracticeTrials <- (Data$numberofPracticeTrials)[1]
Data$trialNumber <- 1:nrow(Data) #create a trial number variable
Data$responseTime <- as.numeric(as.character(Data$responseTime)) #turn responseTime to numeric
Data$correctResponse <- as.numeric(as.character(Data$correctResponse)) #turn correctResponse to numeric
Data$value <- as.numeric(as.character(Data$value)) #turn string variables into numeric

#Performance analysis ----
#General variables ----
medianRT_SDT <- median(Data$responseTime, na.rm = TRUE)
accuracy_SDT <- (sum(Data$correctResponse==TRUE, na.rm = TRUE)/(nrow(Data)-1))*100

#Key variables
threshold_SDT <- mean(Data$value[(nrow(Data)-4):(nrow(Data))])
thresholdACC_SDT <- sum(Data$correctResponse[(nrow(Data)-5):(nrow(Data)-1)])
finalAmplitude_SDT <- Data$value[nrow(Data)]

#Reversals
a <- Data$value
b <- lag(Data$value,1)
reversals <- as.data.frame(cbind(b,a))
colnames(reversals) <- c("valueprior", "value")
reversals$valuediff <- reversals$value-reversals$valueprior #value differnece
reversals$valuediffprior <- lead(reversals$valuediff,1)
reversals$sign <- sign(reversals$valuediff)
reversals$signs <- lead(sign(reversals$valuediff),1)
reversals$signs[reversals$signs==0] <- -1
reversals$reversals  <-reversals$sign+reversals$signs
reversals$reversals[reversals$reversals==0] <- "Reversals"
reversals_SDT <- length(which(reversals$reversals=="Reversals"))
Data$reversals <- reversals$reversals

#Column bind variables
SDT <- cbind(accuracy_SDT, medianRT_SDT, threshold_SDT, thresholdACC_SDT, finalAmplitude_SDT, reversals_SDT)

#Visualisation ----
if(visualise==1){#Plot ----
plot_SDT1 <- ggplot(Data, aes(x=trialNumber, y=value, color=as.factor(correctResponse)))+
geom_point(size =3) + theme(legend.position="none") +
labs(y = expression("Amplitude"~(mu*m)), x = "Trial Number") +
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+
scale_color_manual(values = c("royalblue", "red"))+geom_hline(yintercept=threshold_SDT)

plot_SDT2 <- ggplot(Data, aes(x=trialNumber, y=responseTime, color=as.factor(correctResponse)))+
geom_point(size =3) +theme(legend.position="none") +
labs(y = "Response Time (ms)", x = "Trial Number")   +
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+
scale_color_manual(values = c("royalblue", "red"))+geom_hline(linetype = "dashed", yintercept=medianRT_SDT)

plot_SDT1and2 <- ggarrange(plot_SDT1, plot_SDT2, ncol = 1, nrow = 2)

text = paste("\n   Median RT all across trials:", round(medianRT_SDT, digits = 2), "\n",
"       ACC across all trials:", 100-round(accuracy_SDT, digits = 2), "% \n",
"\n",
"       Detection threshold:", round(threshold_SDT ,digits = 2), "\n",
"       Final amplitude:", round(finalAmplitude_SDT ,digits = 2),
"\n", "Number of reversals:", "\n", reversals_SDT, "\n")

text_SDT <- ggplot() +
annotate("text", x = 4, y = 25, label = text) +
theme_bw() +
theme(panel.grid.major=element_blank(),
panel.grid.minor=element_blank(),
panel.border=element_blank(),
axis.text = element_blank(),
axis.ticks = element_blank(),
axis.title = element_blank())

#Plot with text
plots_SDT <- ggarrange(plot_SDT1and2, text_SDT, ncol = 2, nrow = 1)
plots_SDT <- annotate_figure(plots_SDT, top = text_grob("Static detection threshold", color = "black", face = "bold"))
}} else {
accuracy_SDT <- NA
medianRT_SDT <- NA
threshold_SDT <- NA
thresholdACC_SDT <- NA
finalAmplitude_SDT <- NA
reversals_SDT <- NA

SDT <- cbind(accuracy_SDT, medianRT_SDT, threshold_SDT, thresholdACC_SDT, finalAmplitude_SDT, reversals_SDT)

text = paste("Participant did not complete Static Detection Threshold Protocol")
plots_SDT <- ggplot() +
annotate("text", x = 4, y = 25, label = text) +
theme_bw() +
theme(panel.grid.major=element_blank(),
panel.grid.minor=element_blank(),
panel.border=element_blank(),
axis.text = element_blank(),
axis.ticks = element_blank(),
axis.title = element_blank())}

#### STATIC DETECTION WITH PREPULSE STIMULATION (30MS) #### ----------------------------------------------------------------------------

if("Static Detection Threshold with Adaptation ISI 30" %in% protocolsCompleted){
#House keeping ----
Data <- temp[temp$protocolName=="Static Detection Threshold with Adaptation ISI 30" & !is.na(temp$protocolName),] #Subset to relevant protocol

if(numberofPracticeTrials<5){ #This is a current bandaid fix, for some reason new data from Toronto suggests there are 24 practice trials?
#I've put this in place to work for protocols where the number of practice trials are less than 5 (the case for all previous work)
Data <- Data[numberofPracticeTrials:nrow(Data),] #Removes practice trials based on number of practice trials stated in protocol Details
}

Data$trialNumber <- 1:nrow(Data) #create a trial number variable

Data$responseTime <- as.numeric(as.character(Data$responseTime)) #turn responseTime to numeric
Data$correctResponse <- as.numeric(as.character(Data$correctResponse)) #turn correctResponse to numeric
Data$value <- as.numeric(as.character(Data$value)) #turn string variables into numeric

#Performance analysis ----
#General variables ----
medianRT_SDT30 <- median(Data$responseTime, na.rm = TRUE)
accuracy_SDT30 <- (sum(Data$correctResponse==TRUE, na.rm = TRUE)/(nrow(Data)-1))*100

#Key variables
threshold_SDT30 <- mean(Data$value[(nrow(Data)-4):(nrow(Data))])
thresholdACC_SDT30 <- sum(Data$correctResponse[(nrow(Data)-5):(nrow(Data)-1)])
finalAmplitude_SDT30 <- Data$value[nrow(Data)]

#Reversals
a <- Data$value
b <- lag(Data$value,1)
reversals <- as.data.frame(cbind(b,a))
colnames(reversals) <- c("valueprior", "value")
reversals$valuediff <- reversals$value-reversals$valueprior #value differnece
reversals$valuediffprior <- lead(reversals$valuediff,1)
reversals$sign <- sign(reversals$valuediff)
reversals$signs <- lead(sign(reversals$valuediff),1)
reversals$signs[reversals$signs==0] <- -1
reversals$reversals  <-reversals$sign+reversals$signs
reversals$reversals[reversals$reversals==0] <- "Reversals"
reversals_SDT30 <- length(which(reversals$reversals=="Reversals"))
Data$reversals <- reversals$reversals

#Column bind variables
SDT30 <- cbind(accuracy_SDT30, medianRT_SDT30, threshold_SDT30, thresholdACC_SDT30, finalAmplitude_SDT30, reversals_SDT30)

#Visualisation  ----
if(visualise==1){#Plot ----
plot_SDT301 <- ggplot(Data, aes(x=trialNumber, y=value, color=as.factor(correctResponse)))+
geom_point(size =3) +theme(legend.position="none") +
labs(y = expression("Amplitude"~(mu*m)), x = "Trial Number") +
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+
scale_color_manual(values = c("royalblue", "red"))+geom_hline(yintercept=threshold_SDT30)

plot_SDT302 <- ggplot(Data, aes(x=trialNumber, y=responseTime, color=as.factor(correctResponse)))+
geom_point(size =3) +theme(legend.position="none") +
labs(y = "Response Time (ms)", x = "Trial Number") +
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+
scale_color_manual(values = c("royalblue", "red"))+geom_hline(linetype = "dashed", yintercept=medianRT_SDT30)

plot_SDT301and2 <- ggarrange(plot_SDT301, plot_SDT302, ncol = 1, nrow = 2)

text = paste("\n   Median RT all across trials:", round(medianRT_SDT30, digits = 2), "\n",
"       ACC across all trials:", 100-round(accuracy_SDT30, digits = 2), "% \n",
"\n",
"Detection threshold:", round(threshold_SDT30 ,digits = 2), "\n",
"Final amplitude:", round(finalAmplitude_SDT30 ,digits = 2),
"\n", "Number of reversals:", "\n", reversals_SDT30, "\n")

text_SDT30 <- ggplot() +
annotate("text", x = 4, y = 25, label = text) +
theme_bw() +
theme(panel.grid.major=element_blank(),
panel.grid.minor=element_blank(),
panel.border=element_blank(),
axis.text = element_blank(),
axis.ticks = element_blank(),
axis.title = element_blank())

#Plot with text
plots_SDT30 <- ggarrange(plot_SDT301and2, text_SDT30, ncol = 2, nrow = 1)
plots_SDT30 <- annotate_figure(plots_SDT30, top = text_grob("Static detection threshold with Adaptation ISI 30", color = "black", face = "bold"))
}} else {
accuracy_SDT30 <- NA
medianRT_SDT30 <- NA
threshold_SDT30 <- NA
finalAmplitude_SDT30 <- NA
SDT30 <- cbind(accuracy_SDT30, medianRT_SDT30, threshold_SDT30, finalAmplitude_SDT30)

text = paste("Participant did not complete Static Detection Threshold with Adaptation (ISI 30) Protocol")
plots_SDT30 <- ggplot() +
annotate("text", x = 4, y = 25, label = text) +
theme_bw() +
theme(panel.grid.major=element_blank(),
panel.grid.minor=element_blank(),
panel.border=element_blank(),
axis.text = element_blank(),
axis.ticks = element_blank(),
axis.title = element_blank())}

#### STATIC DETECTION WITH PREPULSE STIMULATION (100MS) #### ----------------------------------------------------------------------------
if("Static Detection Threshold with Adaptation ISI 100" %in% protocolsCompleted){
#House keeping ----
Data <- temp[temp$protocolName=="Static Detection Threshold with Adaptation ISI 100" & !is.na(temp$protocolName),] #Subset to relevant protocol
numberofPracticeTrials <- (Data$numberofPracticeTrials)[1]

if(numberofPracticeTrials<5){ #This is a current bandaid fix, for some reason new data from Toronto suggests there are 24 practice trials?
#I've put this in place to work for protocols where the number of practice trials are less than 5 (the case for all previous work)
Data <- Data[numberofPracticeTrials:nrow(Data),] #Removes practice trials based on number of practice trials stated in protocol Details
}

Data$trialNumber <- 1:nrow(Data) #create a trial number variable

Data$responseTime <- as.numeric(as.character(Data$responseTime)) #turn responseTime to numeric
Data$correctResponse <- as.numeric(as.character(Data$correctResponse)) #turn correctResponse to numeric
Data$value <- as.numeric(as.character(Data$value)) #turn string variables into numeric

#Performance analysis ----
#General variables ----
medianRT_SDT100 <- median(Data$responseTime, na.rm = TRUE)
accuracy_SDT100 <- (sum(Data$correctResponse==TRUE, na.rm = TRUE)/(nrow(Data)-1))*100

#Key variables
threshold_SDT100 <- mean(Data$value[(nrow(Data)-4):(nrow(Data))])
thresholdACC_SDT100 <- sum(Data$correctResponse[(nrow(Data)-5):(nrow(Data)-1)])
finalAmplitude_SDT100 <- Data$value[nrow(Data)]

#Reversals
a <- Data$value
b <- lag(Data$value,1)
reversals <- as.data.frame(cbind(b,a))
colnames(reversals) <- c("valueprior", "value")
reversals$valuediff <- reversals$value-reversals$valueprior #value differnece
reversals$valuediffprior <- lead(reversals$valuediff,1)
reversals$sign <- sign(reversals$valuediff)
reversals$signs <- lead(sign(reversals$valuediff),1)
reversals$signs[reversals$signs==0] <- -1
reversals$reversals  <-reversals$sign+reversals$signs
reversals$reversals[reversals$reversals==0] <- "Reversals"
reversals_SDT100 <- length(which(reversals$reversals=="Reversals"))
Data$reversals <- reversals$reversals

#Column bind variables
SDT100 <- cbind(accuracy_SDT100, medianRT_SDT100, threshold_SDT100, thresholdACC_SDT100, finalAmplitude_SDT100)

#Visualisation  ----
if(visualise==1){#Plot ----
plot_SDT1001 <- ggplot(Data, aes(x=trialNumber, y=value, color=as.factor(correctResponse)))+
geom_point(size =3) +theme(legend.position="none") +
labs(y = expression("Amplitude"~(mu*m)), x = "Trial Number") +
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+
scale_color_manual(values = c("royalblue", "red"))+geom_hline(yintercept=threshold_SDT100)

plot_SDT1002 <- ggplot(Data, aes(x=trialNumber, y=responseTime, color=as.factor(correctResponse)))+
geom_point(size =3) +theme(legend.position="none") +
labs(y = "Response Time (ms)", x = "Trial Number") +
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+
scale_color_manual(values = c("royalblue", "red"))+geom_hline(linetype = "dashed", yintercept=medianRT_SDT100)

plot_SDT1001and2 <- ggarrange(plot_SDT1001, plot_SDT1002, ncol = 1, nrow = 2)

text = paste("\n   Median RT all across trials:", round(medianRT_SDT100, digits = 2), "\n",
"       ACC across all trials:", 100-round(accuracy_SDT100, digits = 2), "% \n",
"\n",
"       Detection threshold:", round(threshold_SDT100 ,digits = 2), "\n",
"       Final amplitude:", round(finalAmplitude_SDT100 ,digits = 2),
"\n", "Number of reversals:", "\n", reversals_SDT100, "\n")

text_SDT100 <- ggplot() +
annotate("text", x = 4, y = 25, label = text) +
theme_bw() +
theme(panel.grid.major=element_blank(),
panel.grid.minor=element_blank(),
panel.border=element_blank(),
axis.text = element_blank(),
axis.ticks = element_blank(),
axis.title = element_blank())

#Plot with text
plots_SDT100 <- ggarrange(plot_SDT1001and2, text_SDT100, ncol = 2, nrow = 1)
plots_SDT100 <- annotate_figure(plots_SDT100, top = text_grob("Static detection threshold with Adaptation ISI 100", color = "black", face = "bold"))
}} else{
accuracy_SDT100 <- NA
medianRT_SDT100 <- NA
threshold_SDT100 <- NA
finalAmplitude_SDT100 <- NA
SDT100 <- cbind(accuracy_SDT100, medianRT_SDT100, threshold_SDT100, finalAmplitude_SDT100)

text = paste("Participant did not complete Static Detection Threshold with Adaptation (ISI 100) Protocol")
plots_SDT100 <- ggplot() +
annotate("text", x = 4, y = 25, label = text) +
theme_bw() +
theme(panel.grid.major=element_blank(),
panel.grid.minor=element_blank(),
panel.border=element_blank(),
axis.text = element_blank(),
axis.ticks = element_blank(),
axis.title = element_blank())}

#### DYNAMIC DETECTION #### ----------------------------------------------------------------------------
if("Dynamic Detection Threshold" %in% protocolsCompleted){
#House keeping ----
Data <- temp[temp$protocolName=="Dynamic Detection Threshold" & !is.na(temp$protocolName),] #Subset to relevant protocol
numberofPracticeTrials <- (Data$numberofPracticeTrials)[1]

if(numberofPracticeTrials<5){ #This is a current bandaid fix, for some reason new data from Toronto suggests there are 24 practice trials?
#I've put this in place to work for protocols where the number of practice trials are less than 5 (the case for all previous work)
Data <- Data[numberofPracticeTrials:nrow(Data),] #Removes practice trials based on number of practice trials stated in protocol Details
}

Data$trialNumber <- 1:nrow(Data) #create a trial number variable

Data$responseTime <- as.numeric(as.character(Data$responseTime)) #turn responseTime to numeric
Data$correctResponse <- as.numeric(as.character(Data$correctResponse)) #turn correctResponse to numeric
Data$value <- as.numeric(as.character(Data$value)) #turn string variables into numeric

#Performance analysis ----
#General variables ----
medianRT_DDT <- median(Data$responseTime, na.rm = TRUE)
accuracy_DDT <- (sum(Data$correctResponse==TRUE, na.rm = TRUE)/(nrow(Data)))*100

#Key variables
threshold_DDT <- mean(Data$value[Data$correctResponse==1])
finalAmplitude_DDT <- Data$value[nrow(Data)]

#Reversals
a <- Data$value
b <- lag(Data$value,1)
reversals <- as.data.frame(cbind(b,a))
colnames(reversals) <- c("valueprior", "value")
reversals$valuediff <- reversals$value-reversals$valueprior #value differnece
reversals$valuediffprior <- lead(reversals$valuediff,1)
reversals$sign <- sign(reversals$valuediff)
reversals$signs <- lead(sign(reversals$valuediff),1)
reversals$signs[reversals$signs==0] <- -1
reversals$reversals  <-reversals$sign+reversals$signs
reversals$reversals[reversals$reversals==0] <- "Reversals"
reversals_DDT <- length(which(reversals$reversals=="Reversals"))
Data$reversals <- reversals$reversals

#Column bind variables
DDT <- cbind(accuracy_DDT, medianRT_DDT, threshold_DDT, finalAmplitude_DDT, reversals_DDT)

#Visualisation  ----
if(visualise==1){#Plot ----
plot_DDT1 <- ggplot(Data, aes(x=trialNumber, y=value, color=as.factor(correctResponse)))+
geom_point(size =3) +theme(legend.position="none") +
labs(y = expression("Amplitude"~(mu*m)), x = "Trial Number") +
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+
scale_color_manual(values = c("royalblue", "red"))+geom_hline(yintercept=threshold_DDT)

plot_DDT2 <- ggplot(Data, aes(x=trialNumber, y=responseTime, color=as.factor(correctResponse)))+
geom_point(size =3) +theme(legend.position="none") +
labs(y = "Response Time (ms)", x = "Trial Number") +
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+
scale_color_manual(values = c("royalblue", "red")) +geom_hline(linetype = "dashed", yintercept=medianRT_DDT)

plot_DDT1and2 <- ggarrange(plot_DDT1, plot_DDT2, ncol = 1, nrow = 2)

text = paste("\n   Median RT all across trials:", round(medianRT_DDT, digits = 2), "\n",
"       ACC across all trials:", 100-round(accuracy_DDT, digits = 2), "% \n",
"\n",
"Detection threshold:", round(threshold_DDT ,digits = 2), "\n",
"Final amplitude:", round(finalAmplitude_DDT ,digits = 2))

text_DDT <- ggplot() +
annotate("text", x = 4, y = 25, label = text) +
theme_bw() +
theme(panel.grid.major=element_blank(),
panel.grid.minor=element_blank(),
panel.border=element_blank(),
axis.text = element_blank(),
axis.ticks = element_blank(),
axis.title = element_blank())

#Plot with text
plots_DDT <- ggarrange(plot_DDT1and2, text_DDT, ncol = 2, nrow = 1)
plots_DDT <- annotate_figure(plots_DDT, top = text_grob("Dynamic detection threshold", color = "black", face = "bold"))
}} else {
accuracy_DDT <- NA
medianRT_DDT <- NA
threshold_DDT <- NA
finalAmplitude_DDT <- NA
reversals_DDT <- NA

DDT <- cbind(accuracy_DDT, medianRT_DDT, threshold_DDT, finalAmplitude_DDT, reversals_DDT)

text = paste("Participant did not complete Dynamic Detection Protocol")
plots_DDT <- ggplot() +
annotate("text", x = 4, y = 25, label = text) +
theme_bw() +
theme(panel.grid.major=element_blank(),
panel.grid.minor=element_blank(),
panel.border=element_blank(),
axis.text = element_blank(),
axis.ticks = element_blank(),
axis.title = element_blank())}

#### AMPLITUDE DISCRIMINATION #### ----------------------------------------------------------------------------
if("Amplitude Discrimination Threshold without Adaptation" %in% protocolsCompleted){
#House keeping ----
Data <- temp[temp$protocolName=="Amplitude Discrimination Threshold without Adaptation" & !is.na(temp$protocolName),] #Subset to relevant protocol
numberofPracticeTrials <- (Data$numberofPracticeTrials)[1]

if(numberofPracticeTrials<5){ #This is a current bandaid fix, for some reason new data from Toronto suggests there are 24 practice trials?
#I've put this in place to work for protocols where the number of practice trials are less than 5 (the case for all previous work)
Data <- Data[numberofPracticeTrials:nrow(Data),] #Removes practice trials based on number of practice trials stated in protocol Details
}

Data$trialNumber <- 1:nrow(Data) #create a trial number variable

Data$responseTime <- as.numeric(as.character(Data$responseTime)) #turn responseTime to numeric
Data$correctResponse <- as.numeric(as.character(Data$correctResponse)) #turn correctResponse to numeric
Data$value <- as.numeric(as.character(Data$value)) #turn string variables into numeric

#Performance analysis ----
#General variables ----
medianRT_ADT <- median(Data$responseTime, na.rm = TRUE)
accuracy_ADT <- (sum(Data$correctResponse==TRUE, na.rm = TRUE)/(nrow(Data)))*100

#Key variables
thresholdACC_ADT <- sum(Data$correctResponse[(nrow(Data)-5):(nrow(Data)-1)])
threshold_ADT <- mean(Data$value[(nrow(Data)-4):(nrow(Data))]) - 100 #removing the standard rerference amplitude
finalAmplitude_ADT <- Data$value[nrow(Data)]

#Reversals
a <- Data$value
b <- lag(Data$value,1)
reversals <- as.data.frame(cbind(b,a))
colnames(reversals) <- c("valueprior", "value")
reversals$valuediff <- reversals$value-reversals$valueprior #value differnece
reversals$valuediffprior <- lead(reversals$valuediff,1)
reversals$sign <- sign(reversals$valuediff)
reversals$signs <- lead(sign(reversals$valuediff),1)
reversals$signs[reversals$signs==0] <- -1
reversals$reversals  <-reversals$sign+reversals$signs
reversals$reversals[reversals$reversals==0] <- "Reversals"
reversals_ADT <- length(which(reversals$reversals=="Reversals"))
Data$reversals <- reversals$reversals

#Column bind variables
ADT <- cbind(accuracy_ADT, medianRT_ADT, threshold_ADT, thresholdACC_ADT, finalAmplitude_ADT, reversals_ADT)

#Visualisation  ----
if(visualise==1){#Plot ----
plot_ADT1 <- ggplot(Data, aes(x=trialNumber, y=value, color=as.factor(correctResponse)))+
geom_point(size =3) +theme(legend.position="none") +
labs(y = expression("Amplitude"~(mu*m)), x = "Trial Number") +
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+
scale_color_manual(values = c("royalblue", "red"))+geom_hline(yintercept=threshold_ADT)

plot_ADT2 <- ggplot(Data, aes(x=trialNumber, y=responseTime, color=as.factor(correctResponse)))+
geom_point(size =3) +theme(legend.position="none") +
labs(y = "Response Time (ms)", x = "Trial Number")+
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+
scale_color_manual(values = c("royalblue", "red"))+geom_hline(linetype = "dashed", yintercept=medianRT_ADT)

plot_ADT1and2 <- ggarrange(plot_ADT1, plot_ADT2, ncol = 1, nrow = 2)

text = paste("\n   Median RT all across trials:", round(medianRT_ADT, digits = 2), "\n",
"       ACC across all trials:", 100-round(accuracy_ADT, digits = 2), "% \n",
"\n",
"Discrimination threshold:", round(threshold_ADT ,digits = 2), "\n",
"Final Amplitude:", round(finalAmplitude_ADT ,digits = 2),
"\n", "Number of reversals:", "\n", reversals_ADT, "\n")

text_ADT <- ggplot() +
annotate("text", x = 4, y = 25, label = text) +
theme_bw() +
theme(panel.grid.major=element_blank(),
panel.grid.minor=element_blank(),
panel.border=element_blank(),
axis.text = element_blank(),
axis.ticks = element_blank(),
axis.title = element_blank())

#Plot with text
plots_ADT <- ggarrange(plot_ADT1and2, text_ADT, ncol = 2, nrow = 1)
plots_ADT <- annotate_figure(plots_ADT, top = text_grob("Amplitude Discrimination without Adaptation", color = "black", face = "bold"))
}} else {
accuracy_ADT <- NA
medianRT_ADT <- NA
threshold_ADT <- NA
thresholdACC_ADT <- NA
finalAmplitude_ADT <- NA
reversals_ADT <- NA

ADT <- cbind(accuracy_ADT, medianRT_ADT, threshold_ADT, thresholdACC_ADT, finalAmplitude_ADT, reversals_ADT)

text = paste("Participant did not complete Amplitude Discrimination protocol")
plots_ADT <- ggplot() +
annotate("text", x = 4, y = 25, label = text) +
theme_bw() +
theme(panel.grid.major=element_blank(),
panel.grid.minor=element_blank(),
panel.border=element_blank(),
axis.text = element_blank(),
axis.ticks = element_blank(),
axis.title = element_blank())}

#### SEQUENTIAL AMPLITUDE DISCRIMINATION #### ----------------------------------------------------------------------------
if("Sequential Amplitude Discrimination" %in% protocolsCompleted){
#House keeping ----
Data <- temp[temp$protocolName=="Sequential Amplitude Discrimination" & !is.na(temp$protocolName),] #Subset to relevant protocol

if(numberofPracticeTrials<5){ #This is a current bandaid fix, for some reason new data from Toronto suggests there are 24 practice trials?
#I've put this in place to work for protocols where the number of practice trials are less than 5 (the case for all previous work)
Data <- Data[numberofPracticeTrials:nrow(Data),] #Removes practice trials based on number of practice trials stated in protocol Details
}

Data$trialNumber <- 1:nrow(Data) #create a trial number variable

Data$responseTime <- as.numeric(as.character(Data$responseTime)) #turn responseTime to numeric
Data$correctResponse <- as.numeric(as.character(Data$correctResponse)) #turn correctResponse to numeric
Data$value <- abs(200-as.numeric(as.character(Data$value))) #turn string variables into numeric

#Performance analysis ----
#General variables ----
medianRT_sqADT <- median(Data$responseTime, na.rm = TRUE)
accuracy_sqADT <- (sum(Data$correctResponse==TRUE, na.rm = TRUE)/(nrow(Data)))*100

#Key variables
thresholdACC_sqADT <- sum(Data$correctResponse[(nrow(Data)-5):(nrow(Data)-1)])
threshold_sqADT <- mean(Data$value[(nrow(Data)-4):(nrow(Data))]) #removing the standard rerference amplitude
finalAmplitude_sqADT <- Data$value[nrow(Data)]

#Reversals
a <- Data$value
b <- lag(Data$value,1)
reversals <- as.data.frame(cbind(b,a))
colnames(reversals) <- c("valueprior", "value")
reversals$valuediff <- reversals$value-reversals$valueprior #value differnece
reversals$valuediffprior <- lead(reversals$valuediff,1)
reversals$sign <- sign(reversals$valuediff)
reversals$signs <- lead(sign(reversals$valuediff),1)
reversals$signs[reversals$signs==0] <- -1
reversals$reversals  <-reversals$sign+reversals$signs
reversals$reversals[reversals$reversals==0] <- "Reversals"
reversals_sqADT <- length(which(reversals$reversals=="Reversals"))
Data$reversals <- reversals$reversals

#Column bind variables
sqADT <- cbind(accuracy_sqADT, medianRT_sqADT, threshold_sqADT, thresholdACC_sqADT, finalAmplitude_sqADT, reversals_sqADT)

#Visualisation  ----
if(visualise==1){
plot_sqADT1 <- ggplot(Data, aes(x=trialNumber, y=value, color=as.factor(correctResponse)))+
geom_point(size =3) +theme(legend.position="none") +
labs(y = expression("Amplitude Difference"~(mu*m)), x = "Trial Number")+
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+
scale_color_manual(values = c("royalblue", "red")) +geom_hline(yintercept=threshold_sqADT)

plot_sqADT2 <- ggplot(Data, aes(x=trialNumber, y=responseTime, color=as.factor(correctResponse)))+
geom_point(size =3) +theme(legend.position="none") +
labs(y = "Response Time (ms)", x = "Trial Number")+
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+
scale_color_manual(values = c("royalblue", "red"))+geom_hline(linetype = "dashed", yintercept=medianRT_sqADT)

plot_sqADT1and2 <- ggarrange(plot_sqADT1, plot_sqADT2, ncol = 1, nrow = 2)

text = paste("\n   Median RT all across trials:", round(medianRT_sqADT, digits = 2), "\n",
"       ACC across all trials:", 100-round(accuracy_sqADT, digits = 2), "% \n",
"\n",
"Discrimination threshold:", round(threshold_sqADT ,digits = 2), "\n",
"Final Amplitude:", round(finalAmplitude_sqADT ,digits = 2),
"\n", "Number of reversals:", "\n", reversals_sqADT, "\n")

text_sqADT <- ggplot() +
annotate("text", x = 4, y = 25, label = text) +
theme_bw() +
theme(panel.grid.major=element_blank(),
panel.grid.minor=element_blank(),
panel.border=element_blank(),
axis.text = element_blank(),
axis.ticks = element_blank(),
axis.title = element_blank())

#Plot with text
plots_sqADT <- ggarrange(plot_sqADT1and2, text_sqADT, ncol = 2, nrow = 1)
plots_sqADT <- annotate_figure(plots_sqADT, top = text_grob("Sequential Amplitude Discrimination", color = "black", face = "bold"))}} else {
accuracy_sqADT <- NA
medianRT_sqADT <- NA
threshold_sqADT <- NA
thresholdACC_sqADT <- NA
finalAmplitude_sqADT <- NA
reversals_sqADT <- NA


sqADT <- cbind(accuracy_sqADT, medianRT_sqADT, threshold_sqADT, thresholdACC_sqADT, finalAmplitude_sqADT, reversals_sqADT)

text = paste("Participant did not complete Sequential Amplitude Discrimination protocol")
plots_sqADT <- ggplot() +
annotate("text", x = 4, y = 25, label = text) +
theme_bw() +
theme(panel.grid.major=element_blank(),
panel.grid.minor=element_blank(),
panel.border=element_blank(),
axis.text = element_blank(),
axis.ticks = element_blank(),
axis.title = element_blank())}

#### SIMULTANEOUS AMPLITUDE DISCRIMINATION #### ----------------------------------------------------------------------------
if("Simultaneous Amplitude Discrimination" %in% protocolsCompleted){
#House keeping ----
Data <- temp[temp$protocolName=="Simultaneous Amplitude Discrimination" & !is.na(temp$protocolName),] #Subset to relevant protocol

if(numberofPracticeTrials<5){ #This is a current bandaid fix, for some reason new data from Toronto suggests there are 24 practice trials?
#I've put this in place to work for protocols where the number of practice trials are less than 5 (the case for all previous work)
Data <- Data[numberofPracticeTrials:nrow(Data),] #Removes practice trials based on number of practice trials stated in protocol Details
}

Data$trialNumber <- 1:nrow(Data) #create a trial number variable

Data$responseTime <- as.numeric(as.character(Data$responseTime)) #turn responseTime to numeric
Data$correctResponse <- as.numeric(as.character(Data$correctResponse)) #turn correctResponse to numeric
Data$value <- abs(200-as.numeric(as.character(Data$value))) #turn string variables into numeric

#Performance analysis ----
#General variables ----
medianRT_smADT <- median(Data$responseTime, na.rm = TRUE)
accuracy_smADT <- (sum(Data$correctResponse==TRUE, na.rm = TRUE)/(nrow(Data)))*100

#Key variables
thresholdACC_smADT <- sum(Data$correctResponse[(nrow(Data)-5):(nrow(Data)-1)])
threshold_smADT <- mean(Data$value[(nrow(Data)-4):(nrow(Data))]) #removing the standard rerference amplitude
finalAmplitude_smADT <- Data$value[nrow(Data)]

#Reversals
a <- Data$value
b <- lag(Data$value,1)
reversals <- as.data.frame(cbind(b,a))
colnames(reversals) <- c("valueprior", "value")
reversals$valuediff <- reversals$value-reversals$valueprior #value differnece
reversals$valuediffprior <- lead(reversals$valuediff,1)
reversals$sign <- sign(reversals$valuediff)
reversals$signs <- lead(sign(reversals$valuediff),1)
reversals$signs[reversals$signs==0] <- -1
reversals$reversals  <-reversals$sign+reversals$signs
reversals$reversals[reversals$reversals==0] <- "Reversals"
reversals_smADT <- length(which(reversals$reversals=="Reversals"))
Data$reversals <- reversals$reversals

#Column bind variables
smADT <- cbind(accuracy_smADT, medianRT_smADT, threshold_smADT, thresholdACC_smADT, finalAmplitude_smADT, reversals_smADT)

#Visualisation  ----
if(visualise==1){#Plot ----
plot_smADT1 <- ggplot(Data, aes(x=trialNumber, y=value, color=as.factor(correctResponse)))+
geom_point(size =3) +theme(legend.position="none") +
labs(y = expression("Amplitude Difference"~(mu*m)), x = "Trial Number") +
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+
scale_color_manual(values = c("royalblue", "red"))+geom_hline(yintercept=threshold_smADT)

plot_smADT2 <- ggplot(Data, aes(x=trialNumber, y=responseTime, color=as.factor(correctResponse)))+
geom_point(size =3) +theme(legend.position="none") +
labs(y = "Response Time (ms)", x = "Trial Number") +
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+
scale_color_manual(values = c("royalblue", "red"))+geom_hline(linetype = "dashed", yintercept=medianRT_smADT)

plot_smADT1and2 <- ggarrange(plot_smADT1, plot_smADT2, ncol = 1, nrow = 2)

text = paste("\n   Median RT all across trials:", round(medianRT_smADT, digits = 2), "\n",
"       ACC across all trials:", 100-round(accuracy_smADT, digits = 2), "% \n",
"\n",
"Discrimination threshold:", round(threshold_smADT ,digits = 2), "\n",
"Final Amplitude:", round(finalAmplitude_smADT ,digits = 2),
"\n", "Number of reversals:", "\n", reversals_smADT, "\n")

text_smADT <- ggplot() +
annotate("text", x = 4, y = 25, label = text) +
theme_bw() +
theme(panel.grid.major=element_blank(),
panel.grid.minor=element_blank(),
panel.border=element_blank(),
axis.text = element_blank(),
axis.ticks = element_blank(),
axis.title = element_blank())

#Plot with text
plots_smADT <- ggarrange(plot_smADT1and2, text_smADT, ncol = 2, nrow = 1)
plots_smADT <- annotate_figure(plots_smADT, top = text_grob("Simultaneous Amplitude Discrimination", color = "black", face = "bold"))
}} else {
accuracy_smADT <- NA
medianRT_smADT <- NA
threshold_smADT <- NA
thresholdACC_smADT <- NA
finalAmplitude_smADT <- NA
reversals_smADT <- NA

smADT <- cbind(accuracy_smADT, medianRT_smADT, threshold_smADT, thresholdACC_smADT, finalAmplitude_smADT, reversals_smADT)

text = paste("Participant did not complete Simultaneous Amplitude Discrimination protocol")
plots_smADT <- ggplot() +
annotate("text", x = 4, y = 25, label = text) +
theme_bw() +
theme(panel.grid.major=element_blank(),
panel.grid.minor=element_blank(),
panel.border=element_blank(),
axis.text = element_blank(),
axis.ticks = element_blank(),
axis.title = element_blank())}

####  AMPLITUDE DISCRIMINATION with ssa #### ----------------------------------------------------------------------------
if("Amplitude Discrimination with Single Site Adaptation" %in% protocolsCompleted){
#House keeping ----
Data <- temp[temp$protocolName=="Amplitude Discrimination with Single Site Adaptation" & !is.na(temp$protocolName),] #Subset to relevant protocol
numberofPracticeTrials <- (Data$numberofPracticeTrials)[1]

if(numberofPracticeTrials<5){ #This is a current bandaid fix, for some reason new data from Toronto suggests there are 24 practice trials?
#I've put this in place to work for protocols where the number of practice trials are less than 5 (the case for all previous work)
Data <- Data[numberofPracticeTrials:nrow(Data),] #Removes practice trials based on number of practice trials stated in protocol Details
}

Data$trialNumber <- 1:nrow(Data) #create a trial number variable

Data$responseTime <- as.numeric(as.character(Data$responseTime)) #turn responseTime to numeric
Data$correctResponse <- as.numeric(as.character(Data$correctResponse)) #turn correctResponse to numeric
Data$value <- as.numeric(as.character(Data$value)) #turn string variables into numeric

#Performance analysis ----
#General variables ----
medianRT_ADTssa <- median(Data$responseTime, na.rm = TRUE)
accuracy_ADTssa <- (sum(Data$correctResponse==TRUE, na.rm = TRUE)/(nrow(Data)))*100

#Key variables
thresholdACC_ADTssa <- sum(Data$correctResponse[(nrow(Data)-5):(nrow(Data)-1)])
threshold_ADTssa <- mean(Data$value[(nrow(Data)-4):(nrow(Data))])- 100 #removing the standard rerference amplitude
finalAmplitude_ADTssa <- Data$value[nrow(Data)]

#Reversals
a <- Data$value
b <- lag(Data$value,1)
reversals <- as.data.frame(cbind(b,a))
colnames(reversals) <- c("valueprior", "value")
reversals$valuediff <- reversals$value-reversals$valueprior #value differnece
reversals$valuediffprior <- lead(reversals$valuediff,1)
reversals$sign <- sign(reversals$valuediff)
reversals$signs <- lead(sign(reversals$valuediff),1)
reversals$signs[reversals$signs==0] <- -1
reversals$reversals  <-reversals$sign+reversals$signs
reversals$reversals[reversals$reversals==0] <- "Reversals"
reversals_ADTssa <- length(which(reversals$reversals=="Reversals"))
Data$reversals <- reversals$reversals

#Column bind variables
ADTssa <- cbind(accuracy_ADTssa, medianRT_ADTssa, threshold_ADTssa, thresholdACC_ADTssa, finalAmplitude_ADTssa, reversals_ADTssa)



#Visualisation  ----
if(visualise==1){#Plot ----
plot_ADTssa1 <- ggplot(Data, aes(x=trialNumber, y=value, color=as.factor(correctResponse)))+
geom_point(size =3) +theme(legend.position="none") +
labs(y = expression("Amplitude Difference"~(mu*m)), x = "Trial Number") +
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+
scale_color_manual(values = c("royalblue", "red"))+geom_hline(yintercept=threshold_ADTssa)

plot_ADTssa2 <- ggplot(Data, aes(x=trialNumber, y=responseTime, color=as.factor(correctResponse)))+
geom_point(size =3) +theme(legend.position="none") +
labs(y = "Response Time (ms)", x = "Trial Number")+
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+
scale_color_manual(values = c("royalblue", "red"))+geom_hline(linetype = "dashed", yintercept=medianRT_ADTssa)

plot_ADTssa1and2 <- ggarrange(plot_ADTssa1, plot_ADTssa2, ncol = 1, nrow = 2)

text = paste("\n   Median RT all across trials:", round(medianRT_ADTssa, digits = 2), "\n",
"       ACC across all trials:", 100-round(accuracy_ADTssa, digits = 2), "% \n",
"\n",
"       Discrimination threshold:", round(threshold_ADTssa ,digits = 2), "\n",
"       Final Amplitude:", round(finalAmplitude_ADTssa ,digits = 2),
"\n", "Number of reversals:", "\n", reversals_ADTssa, "\n")

text_ADTssa <- ggplot() +
annotate("text", x = 4, y = 25, label = text) +
theme_bw() +
theme(panel.grid.major=element_blank(),
panel.grid.minor=element_blank(),
panel.border=element_blank(),
axis.text = element_blank(),
axis.ticks = element_blank(),
axis.title = element_blank())

#Plot with text
plots_ADTssa <- ggarrange(plot_ADTssa1and2, text_ADTssa, ncol = 2, nrow = 1)
plots_ADTssa <- annotate_figure(plots_ADTssa, top = text_grob("Amplitude Discrimination with Single Site Adaptation", color = "black", face = "bold"))
}} else {
accuracy_ADTssa <- NA
medianRT_ADTssa <- NA
threshold_ADTssa <- NA
finalAmplitude_ADTssa <- NA
ADTssa <- cbind(accuracy_ADTssa, medianRT_ADTssa, threshold_ADTssa, finalAmplitude_ADTssa)

text = paste("Participant did not complete Amplitude Discrimination with SSA protocol")
plots_ADTssa <- ggplot() +
annotate("text", x = 4, y = 25, label = text) +
theme_bw() +
theme(panel.grid.major=element_blank(),
panel.grid.minor=element_blank(),
panel.border=element_blank(),
axis.text = element_blank(),
axis.ticks = element_blank(),
axis.title = element_blank())}

#### SEQUENTIAL AMPLITUDE DISCRIMINATION with dsa #### ----------------------------------------------------------------------------
if("Amplitude Discrimination with Dual Site Adaptation" %in% protocolsCompleted){
#House keeping ----
Data <- temp[temp$protocolName=="Amplitude Discrimination with Dual Site Adaptation" & !is.na(temp$protocolName),] #Subset to relevant protocol
numberofPracticeTrials <- (Data$numberofPracticeTrials)[1]

if(numberofPracticeTrials<5){ #This is a current bandaid fix, for some reason new data from Toronto suggests there are 24 practice trials?
#I've put this in place to work for protocols where the number of practice trials are less than 5 (the case for all previous work)
Data <- Data[numberofPracticeTrials:nrow(Data),] #Removes practice trials based on number of practice trials stated in protocol Details
}
Data$trialNumber <- 1:nrow(Data) #create a trial number variable

Data$responseTime <- as.numeric(as.character(Data$responseTime)) #turn responseTime to numeric
Data$correctResponse <- as.numeric(as.character(Data$correctResponse)) #turn correctResponse to numeric
Data$value <- as.numeric(as.character(Data$value)) #turn string variables into numeric

#Performance analysis ----
#General variables ----
medianRT_ADTdsa <- median(Data$responseTime, na.rm = TRUE)
accuracy_ADTdsa <- (sum(Data$correctResponse==TRUE, na.rm = TRUE)/(nrow(Data)))*100

#Key variables
thresholdACC_ADTdsa <- sum(Data$correctResponse[(nrow(Data)-5):(nrow(Data)-1)])
threshold_ADTdsa <- mean(Data$value[(nrow(Data)-4):(nrow(Data))])- 100 #removing the standard rerference amplitude
finalAmplitude_ADTdsa <- Data$value[nrow(Data)]

#Reversals
a <- Data$value
b <- lag(Data$value,1)
reversals <- as.data.frame(cbind(b,a))
colnames(reversals) <- c("valueprior", "value")
reversals$valuediff <- reversals$value-reversals$valueprior #value differnece
reversals$valuediffprior <- lead(reversals$valuediff,1)
reversals$sign <- sign(reversals$valuediff)
reversals$signs <- lead(sign(reversals$valuediff),1)
reversals$signs[reversals$signs==0] <- -1
reversals$reversals  <-reversals$sign+reversals$signs
reversals$reversals[reversals$reversals==0] <- "Reversals"
reversals_ADTdsa <- length(which(reversals$reversals=="Reversals"))
Data$reversals <- reversals$reversals


#Column bind variables
ADTdsa <- cbind(accuracy_ADTdsa, medianRT_ADTdsa, threshold_ADTdsa, thresholdACC_ADTdsa, finalAmplitude_ADTdsa, reversals_ADTdsa)

#Visualisation  ----
if(visualise==1){#Plot ----
plot_ADTdsa1 <- ggplot(Data, aes(x=trialNumber, y=value, color=as.factor(correctResponse)))+
geom_point(size =3) +theme(legend.position="none") +
labs(y = expression("Amplitude Difference"~(mu*m)), x = "Trial Number")+
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+
scale_color_manual(values = c("royalblue", "red")) + geom_hline(yintercept=threshold_ADTdsa)

plot_ADTdsa2 <- ggplot(Data, aes(x=trialNumber, y=responseTime, color=as.factor(correctResponse)))+
geom_point(size =3) +theme(legend.position="none") +
labs(y = "Response Time (ms)", x = "Trial Number")  +
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+
scale_color_manual(values = c("royalblue", "red")) + geom_hline(linetype = "dashed", yintercept=medianRT_ADTdsa)

plot_ADTdsa1and2 <- ggarrange(plot_ADTdsa1, plot_ADTdsa2, ncol = 1, nrow = 2)

text = paste("\n   Median RT all across trials:", round(medianRT_ADTdsa, digits = 2), "\n",
"       ACC across all trials:", 100-round(accuracy_ADTdsa, digits = 2), "% \n",
"\n",
"       Discrimination threshold:", round(threshold_ADTdsa ,digits = 2), "\n",
"       Final Amplitude:", round(finalAmplitude_ADTdsa ,digits = 2),
"\n", "Number of reversals:", "\n", reversals_ADTdsa, "\n")

text_ADTdsa <- ggplot() +
annotate("text", x = 4, y = 25, label = text) +
theme_bw() +
theme(panel.grid.major=element_blank(),
panel.grid.minor=element_blank(),
panel.border=element_blank(),
axis.text = element_blank(),
axis.ticks = element_blank(),
axis.title = element_blank())

#Plot with text
plots_ADTdsa <- ggarrange(plot_ADTdsa1and2, text_ADTdsa, ncol = 2, nrow = 1)
plots_ADTdsa <- annotate_figure(plots_ADTdsa, top = text_grob("Amplitude Discrimination with Dual Site Adaptation", color = "black", face = "bold"))
}} else {
accuracy_ADTdsa <- NA
medianRT_ADTdsa <- NA
threshold_ADTdsa <- NA
finalAmplitude_ADTdsa <- NA
ADTdsa <- cbind(accuracy_ADTdsa, medianRT_ADTdsa, threshold_ADTdsa, finalAmplitude_ADTdsa)

text = paste("Participant did not complete Amplitude Discrimination with DSA protocol")
plots_ADTdsa <- ggplot() +
annotate("text", x = 4, y = 25, label = text) +
theme_bw() +
theme(panel.grid.major=element_blank(),
panel.grid.minor=element_blank(),
panel.border=element_blank(),
axis.text = element_blank(),
axis.ticks = element_blank(),
axis.title = element_blank())
}

#### SIMULTANEOUS FREQUENCY DISCRIMINATION #### ----------------------------------------------------------------------------
if("Simultaneous Frequency Discrimination" %in% protocolsCompleted){
#House keeping ----
Data <- temp[temp$protocolName=="Simultaneous Frequency Discrimination" & !is.na(temp$protocolName),] #Subset to relevant protocol

if(numberofPracticeTrials<5){ #This is a current bandaid fix, for some reason new data from Toronto suggests there are 24 practice trials?
#I've put this in place to work for protocols where the number of practice trials are less than 5 (the case for all previous work)
Data <- Data[numberofPracticeTrials:nrow(Data),] #Removes practice trials based on number of practice trials stated in protocol Details
}

Data$trialNumber <- 1:nrow(Data) #create a trial number variable

Data$responseTime <- as.numeric(as.character(Data$responseTime)) #turn responseTime to numeric
Data$correctResponse <- as.numeric(as.character(Data$correctResponse)) #turn correctResponse to numeric
Data$value <- as.numeric(as.character(Data$value)) #turn string variables into numeric

#Performance analysis ----
#General variables ----
medianRT_SFD <- median(Data$responseTime, na.rm = TRUE)
accuracy_SFD <- (sum(Data$correctResponse==TRUE, na.rm = TRUE)/(nrow(Data)))*100

#Key variables
thresholdACC_SFD <- sum(Data$correctResponse[(nrow(Data)-5):(nrow(Data)-1)])
threshold_SFD <- mean(Data$value[(nrow(Data)-4):(nrow(Data))])
finalFrequency_SFD <- Data$value[nrow(Data)]

#Reversals
a <- Data$value
b <- lag(Data$value,1)
reversals <- as.data.frame(cbind(b,a))
colnames(reversals) <- c("valueprior", "value")
reversals$valuediff <- reversals$value-reversals$valueprior #value differnece
reversals$valuediffprior <- lead(reversals$valuediff,1)
reversals$sign <- sign(reversals$valuediff)
reversals$signs <- lead(sign(reversals$valuediff),1)
reversals$signs[reversals$signs==0] <- -1
reversals$reversals  <-reversals$sign+reversals$signs
reversals$reversals[reversals$reversals==0] <- "Reversals"
reversals_SFD <- length(which(reversals$reversals=="Reversals"))
Data$reversals <- reversals$reversals


#Column bind variables
SFD <- cbind(accuracy_SFD, medianRT_SFD, threshold_SFD, thresholdACC_SFD, finalFrequency_SFD, reversals_SFD)

#Visualisation  ----
if(visualise==1){#Plot ----
plot_SFD1 <- ggplot(Data, aes(x=trialNumber, y=value, color=as.factor(correctResponse)))+
geom_point(size =3) +theme(legend.position="none") +
labs(y = "Frequency (Hz)", x = "Trial Number")   +
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+
scale_color_manual(values = c("royalblue", "red")) + geom_hline(yintercept=threshold_SFD)

plot_SFD2 <- ggplot(Data, aes(x=trialNumber, y=responseTime, color=as.factor(correctResponse)))+
geom_point(size =3) +theme(legend.position="none") +
labs(y = "Response Time (ms)", x = "Trial Number")   +
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+
scale_color_manual(values = c("royalblue", "red")) + geom_hline(linetype = "dashed", yintercept=medianRT_SFD)

plot_SFD1and2 <- ggarrange(plot_SFD1, plot_SFD2, ncol = 1, nrow = 2)

text = paste("\n   Median RT all across trials:", round(medianRT_SFD, digits = 2), "\n",
"       ACC across all trials:", 100-round(accuracy_SFD, digits = 2), "% \n",
"\n",
"       Discrimination Threshold:", round(threshold_SFD ,digits = 2), "\n",
"       Final Frequency:", round(finalFrequency_SFD ,digits = 2),
"\n", "Number of reversals:", "\n", reversals_SFD, "\n")

text_SFD <- ggplot() +
annotate("text", x = 4, y = 25, label = text) +
theme_bw() +
theme(panel.grid.major=element_blank(),
panel.grid.minor=element_blank(),
panel.border=element_blank(),
axis.text = element_blank(),
axis.ticks = element_blank(),
axis.title = element_blank())

#Plot with text
plotsSFD <- ggarrange(plot_SFD1and2, text_SFD, ncol = 2, nrow = 1)
plots_SFD <- annotate_figure(plotsSFD, top = text_grob("Simultaneous Frequency Discrimination Threshold", color = "black", face = "bold"))
}} else {
accuracy_SFD <- NA
medianRT_SFD <- NA
threshold_SFD <- NA
finalFrequency_SFD <- NA
SFD <- cbind(accuracy_SFD, medianRT_SFD, threshold_SFD, finalFrequency_SFD)

text = paste("Participant did not complete Simultaneous Frequency Discrimination protocol")
plots_SFD <- ggplot() +
annotate("text", x = 4, y = 25, label = text) +
theme_bw() +
theme(panel.grid.major=element_blank(),
panel.grid.minor=element_blank(),
panel.border=element_blank(),
axis.text = element_blank(),
axis.ticks = element_blank(),
axis.title = element_blank())}

#### SEQUENTIAL FREQUENCY DISCRIMINATION #### ----------------------------------------------------------------------------
if("Sequential Frequency Discrimination" %in% protocolsCompleted){
#House keeping ----
Data <- temp[temp$protocolName=="Sequential Frequency Discrimination" & !is.na(temp$protocolName),] #Subset to relevant protocol
numberofPracticeTrials <- (Data$numberofPracticeTrials)[1]
Data$trialNumber <- 1:nrow(Data) #create a trial number variable
Data$responseTime <- as.numeric(as.character(Data$responseTime)) #turn responseTime to numeric
Data$correctResponse <- as.numeric(as.character(Data$correctResponse)) #turn correctResponse to numeric
Data$value <- as.numeric(as.character(Data$value)) #turn string variables into numeric

#Performance analysis ----
#General variables ----
medianRT_SQFD <- median(Data$responseTime, na.rm = TRUE)
accuracy_SQFD <- (sum(Data$correctResponse==TRUE, na.rm = TRUE)/(nrow(Data)-1))*100

#Key variables
thresholdACC_SQFD <- sum(Data$correctResponse[(nrow(Data)-5):(nrow(Data)-1)])
threshold_SQFD <- mean(Data$value[(nrow(Data)-4):(nrow(Data))])
finalFrequency_SQFD <- Data$value[nrow(Data)]

#Reversals
a <- Data$value
b <- lag(Data$value,1)
reversals <- as.data.frame(cbind(b,a))
colnames(reversals) <- c("valueprior", "value")
reversals$valuediff <- reversals$value-reversals$valueprior #value differnece
reversals$valuediffprior <- lead(reversals$valuediff,1)
reversals$sign <- sign(reversals$valuediff)
reversals$signs <- lead(sign(reversals$valuediff),1)
reversals$signs[reversals$signs==0] <- -1
reversals$reversals  <-reversals$sign+reversals$signs
reversals$reversals[reversals$reversals==0] <- "Reversals"
reversals_SQFD <- length(which(reversals$reversals=="Reversals"))
Data$reversals <- reversals$reversals

#Column bind variables
SQFD <- cbind(accuracy_SQFD, medianRT_SQFD, threshold_SQFD, thresholdACC_SQFD, finalFrequency_SQFD, reversals_SQFD)

#Visualisation  ----
if(visualise==1){#Plot ----
plot_SQFD1 <- ggplot(Data, aes(x=trialNumber, y=value, color=as.factor(correctResponse)))+
geom_point(size =3) +theme(legend.position="none") +
labs(y = "Frequency (Hz)", x = "Trial Number")+
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+
scale_color_manual(values = c("royalblue", "red")) + geom_hline(yintercept=threshold_SQFD)

plot_SQFD2 <- ggplot(Data, aes(x=trialNumber, y=responseTime, color=as.factor(correctResponse)))+
geom_point(size =3) +theme(legend.position="none") +
labs(y = "Response Time (ms)", x = "Trial Number")+
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+
scale_color_manual(values = c("royalblue", "red")) + geom_hline(linetype = "dashed", yintercept=medianRT_SQFD)

plot_SQFD1and2 <- ggarrange(plot_SQFD1, plot_SQFD2, ncol = 1, nrow = 2)

text = paste("\n   Median RT all across trials:", round(medianRT_SQFD, digits = 2), "\n",
"       ACC across all trials:", 100-round(accuracy_SQFD, digits = 2), "% \n",
"\n",
"       Discrimination Threshold:", round(threshold_SQFD ,digits = 2), "\n",
"       Final Frequency:", round(finalFrequency_SQFD ,digits = 2),
"\n", "Number of reversals:", "\n", reversals_SQFD, "\n")

text_SQFD <- ggplot() +
annotate("text", x = 4, y = 25, label = text) +
theme_bw() +
theme(panel.grid.major=element_blank(),
panel.grid.minor=element_blank(),
panel.border=element_blank(),
axis.text = element_blank(),
axis.ticks = element_blank(),
axis.title = element_blank())

#Plot with text
plots_SQFD <- ggarrange(plot_SQFD1and2, text_SQFD, ncol = 2, nrow = 1)
plots_SQFD <- annotate_figure(plots_SQFD, top = text_grob("Simultaneous Frequency Discrimination Threshold", color = "black", face = "bold"))
plot_SQFD1 <- ggplot(Data, aes(x=trialNumber, y=value, color=as.factor(correctResponse)))+
geom_point(size =3) +theme(legend.position="none") +
labs(y = "Frequency (Hz)", x = "Trial Number")

plot_SQFD2 <- ggplot(Data, aes(x=trialNumber, y=responseTime, color=as.factor(correctResponse)))+
geom_point(size =3) +theme(legend.position="none") +
labs(y = "Response Time (ms)", x = "Trial Number")

plot_SQFD1and2 <- ggarrange(plot_SQFD1, plot_SQFD2, ncol = 1, nrow = 2)

text = paste("\n   Median RT all across trials:", round(medianRT_SQFD, digits = 2), "\n",
"       ACC across all trials:", 100-round(accuracy_SQFD, digits = 2), "% \n",
"\n",
"       Discrimination Threshold:", round(threshold_SQFD ,digits = 2), "\n",
"       Final Frequency:", round(finalFrequency_SQFD ,digits = 2),
"\n", "Number of reversals:", "\n", reversals_SQFD, "\n")

text_SQFD <- ggplot() +
annotate("text", x = 4, y = 25, label = text) +
theme_bw() +
theme(panel.grid.major=element_blank(),
panel.grid.minor=element_blank(),
panel.border=element_blank(),
axis.text = element_blank(),
axis.ticks = element_blank(),
axis.title = element_blank())

#Plot with text
plotsSQFD <- ggarrange(plot_SQFD1and2, text_SQFD, ncol = 2, nrow = 1)
plots_SQFD <- annotate_figure(plotsSQFD, top = text_grob("Sequential Frequency Discrimination Threshold", color = "black", face = "bold"))
}} else{
accuracy_SQFD <- NA
medianRT_SQFD <- NA
threshold_SQFD <- NA
finalFrequency_SQFD <- NA
SQFD <- cbind(accuracy_SQFD, medianRT_SQFD, threshold_SQFD, finalFrequency_SQFD)

text = paste("Participant did not complete Sequential Frequency Discrimination protocol")
plots_SQFD <- ggplot() +
annotate("text", x = 4, y = 25, label = text) +
theme_bw() +
theme(panel.grid.major=element_blank(),
panel.grid.minor=element_blank(),
panel.border=element_blank(),
axis.text = element_blank(),
axis.ticks = element_blank(),
axis.title = element_blank())}

#### TEMPORAL ORDER JUDGEMENT #### ----------------------------------------------------------------------------
if("Temporal Order Judgement" %in% protocolsCompleted){
#House keeping ----
Data <- temp[temp$protocolName=="Temporal Order Judgement" & !is.na(temp$protocolName),] #Subset to relevant protocol
numberofPracticeTrials <- (Data$numberofPracticeTrials)[1]

if(numberofPracticeTrials<5){ #This is a current bandaid fix, for some reason new data from Toronto suggests there are 24 practice trials?
#I've put this in place to work for protocols where the number of practice trials are less than 5 (the case for all previous work)
Data <- Data[numberofPracticeTrials:nrow(Data),] #Removes practice trials based on number of practice trials stated in protocol Details
}

Data$trialNumber <- 1:nrow(Data) #create a trial number variable

Data$responseTime <- as.numeric(as.character(Data$responseTime)) #turn responseTime to numeric
Data$correctResponse <- as.numeric(as.character(Data$correctResponse)) #turn correctResponse to numeric
Data$value <- as.numeric(as.character(Data$value)) #turn string variables into numeric

#Performance analysis ----
#General variables ----
medianRT_TOJ <- median(Data$responseTime, na.rm = TRUE)
accuracy_TOJ <- (sum(Data$correctResponse==TRUE, na.rm = TRUE)/(nrow(Data)-1))*100

#Key variables
thresholdACC_TOJ <- sum(Data$correctResponse[(nrow(Data)-5):(nrow(Data)-1)])
threshold_TOJ <- mean(Data$value[(nrow(Data)-4):(nrow(Data))])
finalDuration_TOJ <- Data$value[nrow(Data)]

#Reversals
a <- Data$value
b <- lag(Data$value,1)
reversals <- as.data.frame(cbind(b,a))
colnames(reversals) <- c("valueprior", "value")
reversals$valuediff <- reversals$value-reversals$valueprior #value differnece
reversals$valuediffprior <- lead(reversals$valuediff,1)
reversals$sign <- sign(reversals$valuediff)
reversals$signs <- lead(sign(reversals$valuediff),1)
reversals$signs[reversals$signs==0] <- -1
reversals$reversals  <-reversals$sign+reversals$signs
reversals$reversals[reversals$reversals==0] <- "Reversals"
reversals_TOJ <- length(which(reversals$reversals=="Reversals"))
Data$reversals <- reversals$reversals

#Column bind variables
TOJ <- cbind(accuracy_TOJ, medianRT_TOJ, threshold_TOJ, thresholdACC_TOJ, finalDuration_TOJ, reversals_TOJ)

#Visualisation  ----
if(visualise==1){#Plot ----
# plot_TOJ1 <- ggplot(Data, aes(x=trialNumber, y=value, color=as.factor(correctResponse)))+
# geom_point(size =3) +theme(legend.position="none") +
# labs(y = "Interstimulus interval (ms)", x = "Trial Number")  +
# theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
# panel.background = element_blank(), axis.line = element_line(colour = "black"))+
# scale_color_manual(values = c("royalblue", "red")) + geom_hline(yintercept=threshold_TOJ)
# 
# plot_TOJ2 <- ggplot(Data, aes(x=trialNumber, y=responseTime, color=as.factor(correctResponse)))+
# geom_point(size =3) +theme(legend.position="none") +
# labs(y = "Response Time (ms)", x = "Trial Number")+
# theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
# panel.background = element_blank(), axis.line = element_line(colour = "black"))+
# scale_color_manual(values = c("royalblue", "red")) + geom_hline(linetype = "dashed", yintercept=medianRT_TOJ)
# 
# plot_TOJ1and2 <- ggarrange(plot_TOJ1, plot_TOJ2, ncol = 1, nrow = 2)

text = paste("\n   Median RT all across trials:", round(medianRT_TOJ, digits = 2), "\n",
"       ACC across all trials:", 100-round(accuracy_TOJ, digits = 2), "% \n",
"\n",
"       Discrimination Threshold:", round(threshold_TOJ ,digits = 2), "\n",
"       Final Frequency:", round(finalDuration_TOJ ,digits = 2),
"\n", "Number of reversals:", "\n", reversals_TOJ, "\n")

text_TOJ <- ggplot() +
annotate("text", x = 4, y = 25, label = text) +
theme_bw() +
theme(panel.grid.major=element_blank(),
panel.grid.minor=element_blank(),
panel.border=element_blank(),
axis.text = element_blank(),
axis.ticks = element_blank(),
axis.title = element_blank())

#Plot with text
plots_TOJ <- ggarrange(plot_TOJ1and2, text_TOJ, ncol = 2, nrow = 1)
plots_TOJ <- annotate_figure(plots_TOJ, top = text_grob("Temporal Order Judgement threshold (ms)", color = "black", face = "bold"))
plot_TOJ1 <- ggplot(Data, aes(x=trialNumber, y=value, color=as.factor(correctResponse)))+
geom_point(size =3) +theme(legend.position="none") +
labs(y = "Interstimulus interval (ms)", x = "Trial Number")

plot_TOJ2 <- ggplot(Data, aes(x=trialNumber, y=responseTime, color=as.factor(correctResponse)))+
geom_point(size =3) +theme(legend.position="none") +
labs(y = "Response Time (ms)", x = "Trial Number")

plot_TOJ1and2 <- ggarrange(plot_TOJ1, plot_TOJ2, ncol = 1, nrow = 2)

text = paste("\n   Median RT all across trials:", round(medianRT_TOJ, digits = 2), "\n",
"       ACC across all trials:", 100-round(accuracy_TOJ, digits = 2), "% \n",
"\n",
"       TOJ Threshold:", round(threshold_TOJ ,digits = 2),
"\n", "Number of reversals:", "\n", reversals_TOJ, "\n")

text_TOJ <- ggplot() +
annotate("text", x = 4, y = 25, label = text) +
theme_bw() +
theme(panel.grid.major=element_blank(),
panel.grid.minor=element_blank(),
panel.border=element_blank(),
axis.text = element_blank(),
axis.ticks = element_blank(),
axis.title = element_blank())

#Plot with text
plots_TOJ <- ggarrange(plot_TOJ1and2, text_TOJ, ncol = 2, nrow = 1)
plots_TOJ <- annotate_figure(plots_TOJ, top = text_grob("Temporal Order Judgement", color = "black", face = "bold"))
}} else{
accuracy_TOJ <- NA
medianRT_TOJ <- NA
threshold_TOJ <- NA
finalFrequency_TOJ <- NA
TOJ <- cbind(accuracy_TOJ, medianRT_TOJ, threshold_TOJ, finalFrequency_TOJ)

text = paste("Participant did not complete Temporal Order Judgement protocol")
plots_TOJ <- ggplot() +
annotate("text", x = 4, y = 25, label = text) +
theme_bw() +
theme(panel.grid.major=element_blank(),
panel.grid.minor=element_blank(),
panel.border=element_blank(),
axis.text = element_blank(),
axis.ticks = element_blank(),
axis.title = element_blank())}

#### TEMPORAL ORDER JUDGEMENT with carrier #### ----------------------------------------------------------------------------
if("Temporal Order Judgement with Carrier" %in% protocolsCompleted){
#House keeping ----
Data <- temp[temp$protocolName=="Temporal Order Judgement with Carrier" & !is.na(temp$protocolName),] #Subset to relevant protocol
numberofPracticeTrials <- (Data$numberofPracticeTrials)[1]

if(numberofPracticeTrials<5){ #This is a current bandaid fix, for some reason new data from Toronto suggests there are 24 practice trials?
#I've put this in place to work for protocols where the number of practice trials are less than 5 (the case for all previous work)
Data <- Data[numberofPracticeTrials:nrow(Data),] #Removes practice trials based on number of practice trials stated in protocol Details
}

Data$trialNumber <- 1:nrow(Data) #create a trial number variable

Data$responseTime <- as.numeric(as.character(Data$responseTime)) #turn responseTime to numeric
Data$correctResponse <- as.numeric(as.character(Data$correctResponse)) #turn correctResponse to numeric
Data$value <- as.numeric(as.character(Data$value)) #turn string variables into numeric

#Performance analysis ----
#General variables ----
medianRT_TOJwc <- median(Data$responseTime, na.rm = TRUE)
accuracy_TOJwc <- (sum(Data$correctResponse==TRUE, na.rm = TRUE)/(nrow(Data)-1))*100

#Key variables
thresholdACC_TOJwc <- sum(Data$correctResponse[(nrow(Data)-5):(nrow(Data)-1)])
threshold_TOJwc <- mean(Data$value[(nrow(Data)-4):(nrow(Data))])
finalDuration_TOJwc <- Data$value[nrow(Data)]

#Reversals
a <- Data$value
b <- lag(Data$value,1)
reversals <- as.data.frame(cbind(b,a))
colnames(reversals) <- c("valueprior", "value")
reversals$valuediff <- reversals$value-reversals$valueprior #value differnece
reversals$valuediffprior <- lead(reversals$valuediff,1)
reversals$sign <- sign(reversals$valuediff)
reversals$signs <- lead(sign(reversals$valuediff),1)
reversals$signs[reversals$signs==0] <- -1
reversals$reversals  <-reversals$sign+reversals$signs
reversals$reversals[reversals$reversals==0] <- "Reversals"
reversals_TOJwc <- length(which(reversals$reversals=="Reversals"))
Data$reversals <- reversals$reversals

#Column bind variables
TOJwc <- cbind(accuracy_TOJwc, medianRT_TOJwc, threshold_TOJwc, thresholdACC_TOJwc, finalDuration_TOJwc, reversals_TOJwc)

#Visualisation  ----
if(visualise==1){#Plot ----
plot_TOJwc1 <- ggplot(Data, aes(x=trialNumber, y=value, color=as.factor(correctResponse)))+
geom_point(size =3) +theme(legend.position="none") +
labs(y = "Interstimulus interval (ms)", x = "Trial Number")  +
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+
scale_color_manual(values = c("royalblue", "red")) + geom_hline(yintercept=threshold_TOJwc)

plot_TOJwc2 <- ggplot(Data, aes(x=trialNumber, y=responseTime, color=as.factor(correctResponse)))+
geom_point(size =3) +theme(legend.position="none") +
labs(y = "Response Time (ms)", x = "Trial Number")   +
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+
scale_color_manual(values = c("royalblue", "red"))+ geom_hline(linetype = "dashed", yintercept=medianRT_TOJwc)

plot_TOJwc1and2 <- ggarrange(plot_TOJwc1, plot_TOJwc2, ncol = 1, nrow = 2)

text = paste("\n   Median RT all across trials:", round(medianRT_TOJwc, digits = 2), "\n",
"       ACC across all trials:", 100-round(accuracy_TOJwc, digits = 2), "% \n",
"\n",
"       TOJ Threshold:", round(threshold_TOJwc ,digits = 2), "\n",
"\n", "Number of reversals:", "\n", reversals_TOJwc, "\n")

text_TOJwc <- ggplot() +
annotate("text", x = 4, y = 25, label = text) +
theme_bw() +
theme(panel.grid.major=element_blank(),
panel.grid.minor=element_blank(),
panel.border=element_blank(),
axis.text = element_blank(),
axis.ticks = element_blank(),
axis.title = element_blank())

#Plot with text
plots_TOJwc <- ggarrange(plot_TOJwc1and2, text_TOJwc, ncol = 2, nrow = 1)
plots_TOJwc <- annotate_figure(plots_TOJwc, top = text_grob("Temporal Order Judgement with Carrier", color = "black", face = "bold"))
}} else{
accuracy_TOJwc <- NA
medianRT_TOJwc <- NA
threshold_TOJwc <- NA
finalFrequency_TOJwc <- NA
TOJwc <- cbind(accuracy_TOJwc, medianRT_TOJwc, threshold_TOJwc, finalFrequency_TOJwc)

text = paste("Participant did not complete Temporal Order Judgement with carrier protocol")
plots_TOJwc <- ggplot() +
annotate("text", x = 4, y = 25, label = text) +
theme_bw() +
theme(panel.grid.major=element_blank(),
panel.grid.minor=element_blank(),
panel.border=element_blank(),
axis.text = element_blank(),
axis.ticks = element_blank(),
axis.title = element_blank())}

#### DURATION DISCRIMINATION #### ----------------------------------------------------------------------------
if("Duration Discrimination" %in%  protocolsCompleted){
#House keeping ----
Data <- temp[temp$protocolName=="Duration Discrimination" & !is.na(temp$protocolName),] #Subset to relevant protocol
numberofPracticeTrials <- (Data$numberofPracticeTrials)[1]

if(numberofPracticeTrials<5){ #This is a current bandaid fix, for some reason new data from Toronto suggests there are 24 practice trials?
#I've put this in place to work for protocols where the number of practice trials are less than 5 (the case for all previous work)
Data <- Data[numberofPracticeTrials:nrow(Data),] #Removes practice trials based on number of practice trials stated in protocol Details
}

Data$trialNumber <- 1:nrow(Data) #create a trial number variable

Data$responseTime <- as.numeric(as.character(Data$responseTime)) #turn responseTime to numeric
Data$correctResponse <- as.numeric(as.character(Data$correctResponse)) #turn correctResponse to numeric
Data$value <- abs(500-as.numeric(as.character(Data$value))) #turn string variables into numeric

#Performance analysis ----
#General variables ----
medianRT_DD <- median(Data$responseTime, na.rm = TRUE)
accuracy_DD <- (sum(Data$correctResponse==TRUE, na.rm = TRUE)/(nrow(Data)-1))*100

#Key variables
thresholdACC_DD <- sum(Data$correctResponse[(nrow(Data)-5):(nrow(Data)-1)])
threshold_DD <- mean(Data$value[(nrow(Data)-4):(nrow(Data))])
finalAmplitude_DD <- Data$value[nrow(Data)]

#Reversals
a <- Data$value
b <- lag(Data$value,1)
reversals <- as.data.frame(cbind(b,a))
colnames(reversals) <- c("valueprior", "value")
reversals$valuediff <- reversals$value-reversals$valueprior #value differnece
reversals$valuediffprior <- lead(reversals$valuediff,1)
reversals$sign <- sign(reversals$valuediff)
reversals$signs <- lead(sign(reversals$valuediff),1)
reversals$signs[reversals$signs==0] <- -1
reversals$reversals  <-reversals$sign+reversals$signs
reversals$reversals[reversals$reversals==0] <- "Reversals"
reversals_DD <- length(which(reversals$reversals=="Reversals"))
Data$reversals <- reversals$reversals

#Column bind variables
DD <- cbind(accuracy_DD, medianRT_DD, threshold_DD, thresholdACC_DD, finalAmplitude_DD, reversals_DD)

#Visualisation  ----
if(visualise==1){#Plot ----
plot_DD1 <- ggplot(Data, aes(x=trialNumber, y=value, color=as.factor(correctResponse)))+
geom_point(size =3) +theme(legend.position="none") +
labs(y = "Duration difference (ms)", x = "Trial Number")  +
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+
scale_color_manual(values = c("royalblue", "red")) + geom_hline(yintercept=threshold_DD)

plot_DD2 <- ggplot(Data, aes(x=trialNumber, y=responseTime, color=as.factor(correctResponse)))+
geom_point(size =3) +theme(legend.position="none") +
labs(y = "Response Time (ms)", x = "Trial Number")   +
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+
scale_color_manual(values = c("royalblue", "red"))+ geom_hline(yintercept= medianRT_DD)

plot_DD1and2 <- ggarrange(plot_DD1, plot_DD2, ncol = 1, nrow = 2)

text = paste("\n   Median RT all across trials:", round(medianRT_DD, digits = 2), "\n",
"       ACC across all trials:", 100-round(accuracy_DD, digits = 2), "% \n",
"\n",
"       Discrimination Threshold:", round(threshold_DD ,digits = 2), "\n",
"\n", "Number of reversals:", "\n", reversals_DD, "\n")

text_DD <- ggplot() +
annotate("text", x = 4, y = 25, label = text) +
theme_bw() +
theme(panel.grid.major=element_blank(),
panel.grid.minor=element_blank(),
panel.border=element_blank(),
axis.text = element_blank(),
axis.ticks = element_blank(),
axis.title = element_blank())

#Plot with text
plots_DD <- ggarrange(plot_DD1and2, text_DD, ncol = 2, nrow = 1)
plots_DD <- annotate_figure(plots_DD, top = text_grob("Duration Discrimination", color = "black", face = "bold"))
}} else {
accuracy_DD <- NA
medianRT_DD <- NA
threshold_DD <- NA
thresholdACC_DD <- NA
finalAmplitude_DD <- NA
reversals_DD <- NA

DD <- cbind(accuracy_DD, medianRT_DD, threshold_DD, thresholdACC_DD, finalAmplitude_DD, reversals_DD)

text = paste("Participant did not complete Duration Discrimination protocol")
plots_DD <- ggplot() +
annotate("text", x = 4, y = 25, label = text) +
theme_bw() +
theme(panel.grid.major=element_blank(),
panel.grid.minor=element_blank(),
panel.border=element_blank(),
axis.text = element_blank(),
axis.ticks = element_blank(),
axis.title = element_blank())}

#### DUAL STAIRCASE AMPLITUDE DISCRIMINATION (down) #### ----------------------------------------------------------------------------
if("Dual Staircase Amplitude Discrimination (down)" %in%  protocolsCompleted){
#House keeping ----
Data <- temp[temp$protocolName=="Dual Staircase Amplitude Discrimination (down)" & !is.na(temp$protocolName),] #Subset to relevant protocol
numberofPracticeTrials <- (Data$numberofPracticeTrials)[1]

if(numberofPracticeTrials<5){ #This is a current bandaid fix, for some reason new data from Toronto suggests there are 24 practice trials?
#I've put this in place to work for protocols where the number of practice trials are less than 5 (the case for all previous work)
Data <- Data[numberofPracticeTrials:nrow(Data),] #Removes practice trials based on number of practice trials stated in protocol Details
}

Data$trialNumber <- 1:nrow(Data) #create a trial number variable

Data$responseTime <- as.numeric(as.character(Data$responseTime)) #turn responseTime to numeric
Data$correctResponse <- as.numeric(as.character(Data$correctResponse)) #turn correctResponse to numeric
Data$value <- abs(200-as.numeric(as.character(Data$value))) #turn string variables into numeric

#Performance analysis ----
#General variables ----
medianRTADdown <- median(Data$responseTime, na.rm = TRUE)
accuracyADdown <- (sum(Data$correctResponse==TRUE, na.rm = TRUE)/(nrow(Data)-1))*100

#Key variables
thresholdACCADdown <- sum(Data$correctResponse[(nrow(Data)-5):(nrow(Data)-1)])
thresholdADdown <- mean(Data$value[(nrow(Data)-4):(nrow(Data))])
finalAmplitudeADdown <- Data$value[nrow(Data)]

#Reversals
a <- Data$value
b <- lag(Data$value,1)
reversals <- as.data.frame(cbind(b,a))
colnames(reversals) <- c("valueprior", "value")
reversals$valuediff <- reversals$value-reversals$valueprior #value differnece
reversals$valuediffprior <- lead(reversals$valuediff,1)
reversals$sign <- sign(reversals$valuediff)
reversals$signs <- lead(sign(reversals$valuediff),1)
reversals$signs[reversals$signs==0] <- -1
reversals$reversals  <-reversals$sign+reversals$signs
reversals$reversals[reversals$reversals==0] <- "Reversals"
reversalsADdown <- length(which(reversals$reversals=="Reversals"))
Data$reversals <- reversals$reversals

#Column bind variables
ADdown <- cbind(accuracyADdown, medianRTADdown, thresholdADdown, thresholdACCADdown, finalAmplitudeADdown, reversalsADdown)

#Visualisation  ----
if(visualise==1){#Plot ----
plotADdown1 <- ggplot(Data, aes(x=trialNumber, y=value, color=as.factor(correctResponse)))+
geom_point(size =3) +theme(legend.position="none") +
labs(y = expression("Amplitude Difference"~(mu*m)), x = "Trial Number") +
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+
scale_color_manual(values = c("royalblue", "red")) + geom_hline(yintercept=thresholdADdown)

plotADdown2 <- ggplot(Data, aes(x=trialNumber, y=responseTime, color=as.factor(correctResponse)))+
geom_point(size =3) +theme(legend.position="none") +
labs(y = "Response Time (ms)", x = "Trial Number") +
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+
scale_color_manual(values = c("royalblue", "red")) + geom_hline(linetype = "dashed", yintercept=medianRTADdown)

plotADdown1and2 <- ggarrange(plotADdown1, plotADdown2, ncol = 1, nrow = 2)

text = paste("\n   Median RT all across trials:", round(medianRTADdown, digits = 2), "\n",
"       ACC across all trials:", 100-round(accuracyADdown, digits = 2), "% \n",
"\n",
"       Discrimination Threshold:", round(thresholdADdown ,digits = 2), "\n",
"\n", "Number of reversals:", "\n", reversalsADdown, "\n")

textADdown <- ggplot() +
annotate("text", x = 4, y = 25, label = text) +
theme_bw() +
theme(panel.grid.major=element_blank(),
panel.grid.minor=element_blank(),
panel.border=element_blank(),
axis.text = element_blank(),
axis.ticks = element_blank(),
axis.title = element_blank())

#Plot with text
plotsADdown <- ggarrange(plotADdown1and2, textADdown, ncol = 2, nrow = 1)
plots_ADdown <- annotate_figure(plotsADdown, top = text_grob("Dual Staircase Amplitude Discrimination (down)", color = "black", face = "bold"))
}} else {
accuracyADdown <- NA
medianRTADdown <- NA
thresholdADdown <- NA
thresholdACCADdown <- NA
finalAmplitudeADdown <- NA
reversalsADdown <- NA


ADdown <- cbind(accuracyADdown, medianRTADdown, thresholdADdown, thresholdACCADdown, finalAmplitudeADdown, reversalsADdown)

text = paste("Participant did not complete Dual Staircase Amplitude Discrimination (down)")
plots_ADdown <- ggplot() +
annotate("text", x = 4, y = 25, label = text) +
theme_bw() +
theme(panel.grid.major=element_blank(),
panel.grid.minor=element_blank(),
panel.border=element_blank(),
axis.text = element_blank(),
axis.ticks = element_blank(),
axis.title = element_blank())}


#### DUAL STAIRCASE AMPLITUDE DISCRIMINATION (up) #### ----------------------------------------------------------------------------
if("Dual Staircase Amplitude Discrimination (up)" %in%  protocolsCompleted){
#House keeping ----
Data <- temp[temp$protocolName=="Dual Staircase Amplitude Discrimination (up)" & !is.na(temp$protocolName),] #Subset to relevant protocol
numberofPracticeTrials <- (Data$numberofPracticeTrials)[1]
Data$trialNumber <- 1:nrow(Data) #create a trial number variable
Data$responseTime <- as.numeric(as.character(Data$responseTime)) #turn responseTime to numeric
Data$correctResponse <- as.numeric(as.character(Data$correctResponse)) #turn correctResponse to numeric
Data$value <- abs(200-as.numeric(as.character(Data$value))) #turn string variables into numeric

#Performance analysis ----
#General variables ----
medianRTADup <- median(Data$responseTime, na.rm = TRUE)
accuracyADup <- (sum(Data$correctResponse==TRUE, na.rm = TRUE)/(nrow(Data)-1))*100

#Key variables
thresholdACCADup <- sum(Data$correctResponse[(nrow(Data)-5):(nrow(Data)-1)])
thresholdADup <- mean(Data$value[(nrow(Data)-4):(nrow(Data))])
finalAmplitudeADup <- Data$value[nrow(Data)]

#Reversals
a <- Data$value
b <- lag(Data$value,1)
reversals <- as.data.frame(cbind(b,a))
colnames(reversals) <- c("valueprior", "value")
reversals$valuediff <- reversals$value-reversals$valueprior #value differnece
reversals$valuediffprior <- lead(reversals$valuediff,1)
reversals$sign <- sign(reversals$valuediff)
reversals$signs <- lead(sign(reversals$valuediff),1)
reversals$signs[reversals$signs==0] <- -1
reversals$reversals  <-reversals$sign+reversals$signs
reversals$reversals[reversals$reversals==0] <- "Reversals"
reversalsADup <- length(which(reversals$reversals=="Reversals"))
Data$reversals <- reversals$reversals

#Column bind variables
ADup <- cbind(accuracyADup, medianRTADup, thresholdADup, thresholdACCADup, finalAmplitudeADup, reversalsADup)

#Visualisation  ----
if(visualise==1){#Plot ----
plotADup1 <- ggplot(Data, aes(x=trialNumber, y=value, color=as.factor(correctResponse)))+
geom_point(size =3) +theme(legend.position="none") +
labs(y = expression("Amplitude Difference"~(mu*m)), x = "Trial Number")  +
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+
scale_color_manual(values = c("royalblue", "red")) + geom_hline(yintercept=thresholdADup)

plotADup2 <- ggplot(Data, aes(x=trialNumber, y=responseTime, color=as.factor(correctResponse)))+
geom_point(size =3) +theme(legend.position="none") +
labs(y = "Response Time (ms)", x = "Trial Number") +
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+
scale_color_manual(values = c("royalblue", "red"))  + geom_hline(linetype = "dashed", yintercept=medianRTADup)

plotADup1and2 <- ggarrange(plotADup1, plotADup2, ncol = 1, nrow = 2)

text = paste("\n   Median RT all across trials:", round(medianRTADup, digits = 2), "\n",
"       ACC across all trials:", 100-round(accuracyADup, digits = 2), "% \n",
"\n",
"       Discrimination Threshold:", round(thresholdADup ,digits = 2), "\n",
"\n", "Number of reversals:", "\n", reversalsADup, "\n")

textADup <- ggplot() +
annotate("text", x = 4, y = 25, label = text) +
theme_bw() +
theme(panel.grid.major=element_blank(),
panel.grid.minor=element_blank(),
panel.border=element_blank(),
axis.text = element_blank(),
axis.ticks = element_blank(),
axis.title = element_blank())

#Plot with text
plotsADup <- ggarrange(plotADup1and2, textADup, ncol = 2, nrow = 1)
plots_ADup <- annotate_figure(plotsADup, top = text_grob("Dual Staircase Amplitude Discrimination (up)", color = "black", face = "bold"))
}} else {
accuracyADup <- NA
medianRTADup <- NA
thresholdADup <- NA
thresholdACCADup <- NA
finalAmplitudeADup <- NA
reversalsADup <- NA


ADup <- cbind(accuracyADup, medianRTADup, thresholdADup, thresholdACCADup, finalAmplitudeADup, reversalsADup)

text = paste("Participant did not complete Dual Staircase Amplitude Discrimination (up)")
plots_ADup <- ggplot() +
annotate("text", x = 4, y = 25, label = text) +
theme_bw() +
theme(panel.grid.major=element_blank(),
panel.grid.minor=element_blank(),
panel.border=element_blank(),
axis.text = element_blank(),
axis.ticks = element_blank(),
axis.title = element_blank())}

#### PARTICIPANT DETAILS #### -------------------------------------------------------------------
Data <- temp[temp$protocolName=="Simple Reaction Time" & !is.na(temp$protocolName),] #Use information from SRT, since all participants should have completed this
id <- as.character(Data$id)[1]
race <- as.character(Data$race)[1]
gender <- as.character(Data$gender)[1]
handedness <- as.character(Data$handedness)[1]
birthDate <- as.character(Data$birthYear)[1]
date <- as.character(Data$date)[1] #refers to date tested
session <- as.character(Data$session)[1]

#Extract id 
#This code is a bit confusing, but the idea is that not every participant completes every protocol, and participant's ids are stored in the protocols completed
#What I've done here is to pull out the participant id for all the protocols, and then extracting their id that is present in any of the protocols completed

protocolNames <- c(
"Simple Reaction Time", 
"Choice Reaction Time", 
"Static Detection Threshold",
"Static Detection Threshold with Adaptation ISI 30", 
"Static Detection Threshold with Adaptation ISI 100", 
"Dynamic Detection Threshold", 
"Amplitude Discrimination without Adaptation", 
"Amplitude Discrimination with Single Site Adaptation", 
"Ampltiude Discrimination with Dual Site Adaptation"
,"Simultaneous Frequency Discrimination", 
"Sequential Frequency Discrimination", 
"Temporal Order Judgement", 
"Temporal Order Judgement", 
"Temporal Order Judgement with Carrier", 
"Duration Discrimination",
"Dual Staircase Amplitude Discrimination (down)", 
"Dual Staircase Amplitude Discrimination (up)")

idProtocolList <- list()
for(i in 1:length(protocolNames)){
Data <- temp[temp$protocolName==paste0(protocolNames[i]) & !is.na(temp$protocolName),]
idProtocolList[[i]] <- as.character(Data$id)[1]
}

id <- as.character(count(unlist(idProtocolList))$x[1]) #The id is the value that is not NA here, which will always be at the top 

#Plot participant details  
participantDetails <- cbind(id, date, race, gender, handedness, birthDate, session)
text = paste("Participant number:", paste(id), "\n",
"Participant race:", paste(race), "\n",
"Participant gender:", paste(gender), "\n",
"Handedness:", paste(handedness), "\n",
"Birth Year:", paste(birthDate)[1], "\n",
"Session:", paste(session), "\n",
"Date:", paste(date)
)

plots_participantDetails <- ggplot() + 
annotate("text", x = 4, y = 25, size = 6, label = text, hjust = 1) + 
theme_bw() +
theme(panel.grid.major=element_blank(),
panel.grid.minor=element_blank(),
panel.border=element_blank(),
axis.text = element_blank(),
axis.ticks = element_blank(),
axis.title = element_blank())


format <- as.data.frame(str_sub(participants[p],-6,-5)) #Format 
colnames(format)[1] <- "Format"

if("site" %in% colnames(temp)){#Site- can't really remmeber what this code does, but it is important (do not delete)
Site <- as.data.frame(temp$site[1])
colnames(Site)[1] <- "Site"
} else {
Site <- as.data.frame(NA)
colnames(Site)[1] <- "Site"}




#### COMBINING AND SAVING PLOTS #### ----------------------------------------------------------------------------------------------------------------------------
  #Note that the codes are specified for each site (and hence not done with a standardised code or a for loop)
  #This was done in order to prevent having plots being presented for protocols that were never completed by a given site 

if(visualise==1 & temp$site=="JHU"){
Allplots <- ggarrange(plots_SRT,
plots_CRT,
plots_SDT,
plots_SDT30,
plots_SDT100,
plots_DDT,
plots_ADT,
plots_ADTssa,
plots_ADTdsa,
plots_SFD,
plots_SQFD,
plots_TOJ,
plots_TOJwc,
plots_DD,
plots_participantDetails,
ncol = 3, nrow = 5)

Allplots <- annotate_figure(Allplots, top = text_grob("Batch Analyzed Tactile Data", face = "bold", color = "black", size = 20))

#Save all plots to a separate subfolder within the original directory 
setwd(baseDirectory)
dir.create("Plots", showWarnings = FALSE) #Creates a directory to put the combined .csv file into
setwd(paste0(baseDirectory,"/Plots")) #Switch to a folder to save the plots
ggsave(paste0(id,"session",s,"_performancePlots.pdf"), width = 25, height = 25, limitsize = FALSE) #save plot
setwd(paste0(baseDirectory)) #Return to base directory 
}

if(visualise==1 & temp$site=="KKI"){
Allplots <- ggarrange(plots_SRT,
plots_CRT,
plots_SDT,
plots_SDT30,
plots_SDT100,
plots_DDT,
plots_ADT,
plots_ADTssa,
plots_ADTdsa,
plots_SFD,
plots_SQFD,
plots_TOJ,
plots_TOJwc,
plots_DD,
plots_participantDetails,
ncol = 3, nrow = 5)

Allplots <- annotate_figure(Allplots, top = text_grob("Batch Analyzed Tactile Data", face = "bold", color = "black", size = 20))

#Save all plots to a separate subfolder within the original directory 
setwd(baseDirectory)
dir.create("Plots", showWarnings = FALSE) #Creates a directory to put the combined .csv file into
setwd(paste0(baseDirectory,"/Plots")) #Switch to a folder to save the plots
ggsave(paste0(id,"session",s,"_performancePlots.pdf"), width = 25, height = 25, limitsize = FALSE) #save plot
setwd(paste0(baseDirectory)) #Return to base directory 
}

if(visualise==1 & temp$site=="CCH"){
  Allplots <- ggarrange(plots_SRT,
                        plots_CRT,
                        plots_SDT,
                        plots_DDT,
                        plots_ADT,
                        plots_ADTssa,
                        plots_ADTdsa,
                        plots_SFD,
                        plots_SQFD,
                        plots_TOJ,
                        plots_TOJwc,
                        plots_DD,
                        plots_participantDetails,
                        ncol = 3, nrow = 5)
  
  Allplots <- annotate_figure(Allplots, top = text_grob("Batch Analyzed Tactile Data", face = "bold", color = "black", size = 20))
  SessionPlots[[s]] <- Allplots  
}
if(visualise==1 & temp$site=="University of Calgary"){
Allplots <- ggarrange(plots_SRT,
plots_sqADT,
plots_smADT,
plots_DD,
plots_TOJ,
plots_participantDetails,
ncol = 3, nrow =2)

Allplots <- annotate_figure(Allplots, top = text_grob("Batch Analyzed Tactile Data", face = "bold", color = "black", size = 20))

#Save all plots to a separate subfolder within the original directory 
setwd(baseDirectory)
dir.create("Plots") #Creates a directory to put the combined .csv file into
setwd(paste0(baseDirectory,"/Plots")) #Switch to a folder to save the plots
ggsave(paste0(id,"session",s,"_performancePlots.pdf"), width = 24, height = 12, limitsize = FALSE) #save plot
setwd(paste0(baseDirectory)) #Return to base directory 
}
if(visualise==1 & temp$site=="University of Toronto"){
Allplots <- ggarrange(plots_SRT,
plots_CRT,
plots_SDT,
plots_DDT,
plots_DD,
plots_smADT,
plots_sqADT,
plots_ADdown,
plots_ADup,
plots_participantDetails,
ncol = 3, nrow =4)

Allplots <- annotate_figure(Allplots, top = text_grob("Batch Analyzed Tactile Data", face = "bold", color = "black", size = 20))
SessionPlots[[s]] <- Allplots  

}

#### COMBINING DATA #### -------------------------------------------------------------------------------
#All depedent variables ---

if(temp$site=="University of Toronto"){
  Performance <- as.data.frame(cbind(
    participantDetails, 
    format, 
    Site,
    session,
    SRT,
    CRT, 
    SDT, 
    DDT,
    DD, 
    sqADT, 
    smADT, 
    ADdown, 
    ADup))
} else {
  Performance <- as.data.frame(cbind(
    participantDetails, 
    format, 
    Site, 
    SRT,
    CRT, 
    SDT, 
    SDT30, 
    SDT100, 
    DDT,
    ADT, 
    ADTssa, 
    ADTdsa, 
    SFD, 
    SQFD, 
    TOJ, 
    TOJwc, 
    DD, 
    ADup, 
    ADdown, 
    sqADT, 
    smADT, 
    session))
  
}



row.names(Performance) <- NULL #just removes the rownames, which tend to be chopped up values from the original files (irrelevant)
participant_performance[[s]] <- Performance #record performance of a given participant for a given session in list


#This for loop puts all the plots into a single pdf for each participant (only applicable to projects with multiple sessions)
if(visualise==1 & temp$site=="University of Toronto"){
  dir.create("Plots", showWarnings = FALSE) #Creates a directory to put the combined .csv file into
  setwd(paste0(baseDirectory,"/Plots")) #Switch to a folder to save the plots
  pdf(paste(id,"_plots.pdf"), onefile = TRUE, width = 24, height = 16)
  print(SessionPlots)
  dev.off()
}

if(visualise==1 & temp$site=="CCH"){
  dir.create("Plots", showWarnings = FALSE) #Creates a directory to put the combined .csv file into
  setwd(paste0(baseDirectory,"/Plots")) #Switch to a folder to save the plots
  pdf(paste(id,"_plots.pdf"), onefile = TRUE, width = 24, height = 16)
  print(SessionPlots)
  dev.off()
}

participantPerformance <- as.data.frame(rbindlist(participant_performance, fill = TRUE)) #Combines output from the for loop above
allSessions[[p]] <- participantPerformance
setwd(paste0(baseDirectory)) #Switch back to a folder to save the plots
}

#Print to show progress of loop (participant id and session) -------------------------------------------------------------------
print(paste("now processing participant:", id, "session: ",s))}

### COMBINE ALL PERFORMANCE DATA FOR ALL PARTICIPANTS AND ALL SESSIONS INTO A COMBINED DATAFRAME ### ----------------------
Data <- as.data.frame(rbindlist(allSessions, fill=FALSE, use.names = TRUE))
dir.create("Combined", showWarnings = FALSE) #Creates a directory to put the combined .csv file into
setwd(paste0(baseDirectory,"/Combined")) #Return to base directory
write.csv(Data, file = "allTacCombined.csv") #Change this to whatever you wish to save the data as (make sure to leave .csv in there)

