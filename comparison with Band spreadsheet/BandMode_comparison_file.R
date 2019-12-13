### Set working directory
#setwd("F:\\BAND CRM For R\\BAND CRM For R\\test")

###name results folder
results_folder <- "results" ## set name for results folder

#######################################
###set data input file names
BirdDataFile<-"data\\BirdData.csv"
TurbineDataFile<-"data\\TurbineData.csv"
CountDataFile<-"data\\CountData.csv"  
FlightDataFile<-"data\\FlightHeight.csv" #generic flight heigh data supplied by BTO

######set number of iterations#########
iter<- 2 ##enter number of iterations required

## set collision risk species
CRSpecies = c("Northern_Gannet")# , "Arctic_Skua", "Northern_Fulmar", "Great_Black_backed_Gull", "Common_Guillemot", "Lesser_Black_Backed_Gull", "Little_Auk", "Great_Skua", "Atlantic_Puffin", "Razorbill")

## Target Power - how much power will the wind farm generate (MW)
TPower = 800

## Do you want a large array correction? "yes" or "no"
LargeArrayCorrection = "yes"

## Width of wind farm - distance travelled by birds crossing wind farm
WFWidth = 7.8 #### Used in Large Array Correction 

## Proportion of flights upwind - should be 50% unless direction of travel is biased in a particular direction
Prop_Upwind = 0.5

## Latitude
Latitude = 55.8

## Tidal Offset (m)
TideOff = 2.5

###Enter wind speed characteristics for site###
###These will be used to sample wind speeds from a truncated normal distribution###
windSpeedMean<- 9.9
windSpeedSD<- 0.000001

#' Band parameterisation for option 3
#' Masden code uses a wind-speed-pitch function to determine rotor speed
#' For comparability with the Band spreadsheet, this must be avoided
#' Setting this to "T" here will source a different version of the 
#' function "pcoll". This is sourced in the same place as in the original code
#' within the option2.txt file

bandParameterisation <- T

##################NO MORE INPUTS REQUIRED##########################################
#############DO NOT ALTER CODE BELOW THIS LINE#####################################

###start timers
start.time <- Sys.time()
###load packages
library(msm)

###create results folder
if(results_folder == "") results_folder<-paste(Sys.Date()) ## if no name given for results folder, use today's date
if(results_folder !="") dir.create(paste(results_folder)) ## if name given for results folder use that and create folder

##make input, figures and tables folders
dir.create(paste(results_folder, "figures", sep="\\"))
dir.create(paste(results_folder, "tables", sep="\\"))
dir.create(paste(results_folder, "input", sep="\\"))

###set random number seed
set.seed(100)
S<-iter*20 ##this is number of samples, increased to ensure enough valid values

### Read in Distance corrected count data for each species, bird biometric data, flight height distributions and turbine characteristics

# Bird Data (flight characteristics etc)
source("scripts\\readBirdData.txt")

# Turbine characteristics
source("scripts\\readTurbineData.txt")

# Distance corrected count data
source("scripts\\readCountData.txt")

# Generic flight Height Distribution data for all species
source("scripts\\readFlightData.txt")

###############################################
######Calculate hours daylight per month#######
###############################################
source("scripts\\DayLength.txt") #### produces a data frame with number of hours daylight and night per month

######get sample functions###
######for bird parameters####
source("scripts\\samplewingspan.R")
source("scripts\\samplebirdlength.R")
source("scripts\\sampleCRH.R")
source("scripts\\samplebirdflight.R")
source("scripts\\samplenocturnal.R")
source("scripts\\samplecount.R")
source("scripts\\sampleavoidance.R")
###for turbine parameters###
source("scripts\\samplebladewidth.R")
source("scripts\\samplerotorradius.R")
source("scripts\\samplehubheightadd.R")
source("scripts\\sampleop.R")
source("scripts\\cv.R") # to calculate coefficent of variation

##set progress bar
pb   <- txtProgressBar(1, iter*length(CRSpecies)*nrow(TurbineData), style=3)

###create overall results summary table###
resultsSummary = data.frame(matrix(data = 0, ncol = 8, nrow = length(CRSpecies)*nrow(TurbineData)))
names(resultsSummary) = c("Species", "Turbine", "Option", "Mean", "SD","CV", "Median", "IQR")

#######################################
#########Start sampling loop###########
#######################################
for (s in 1 : length (CRSpecies)){
  
  species.dat = subset (BirdData, Species == CRSpecies[s])
  species.count = subset (CountData, Species == CRSpecies[s])
  
  Flap_Glide = ifelse (species.dat$Flight == "Flapping", 1 -> Flap_Glide, 2/pi -> Flap_Glide)
  
  ##input flight curves for the species
  ht<-paste(CRSpecies[s],"ht", sep="_")
  ht<-paste(ht, "csv", sep=".")
  ht<-paste("data",ht, sep="\\")
  FlightHeightSpec = read.table(ht, header = F, sep = ",") #and change in option2 code
  flight.boot<-1:dim(FlightHeightSpec)[2]
  flight.boot.sample<-sample(flight.boot, iter, replace=T)
  
  
  ######set species specific bird parameter vectors######
  sampledWingSpan<-numeric()
  sampledbirdLength<-numeric()
  sampledbirdHeight<-numeric()
  sampledbirdSpeed<-numeric()
  sampledNocturnal<-numeric()
  sampledAvoidanceBasic<-numeric()
  sampledAvoidanceExtended<-numeric()
  
  ######set count parameter vectors######
  sampledJan<-numeric()
  sampledFeb<-numeric()
  sampledMar<-numeric()
  sampledApr<-numeric()
  sampledMay<-numeric()
  sampledJun<-numeric()
  sampledJul<-numeric()
  sampledAug<-numeric()
  sampledSep<-numeric()
  sampledOct<-numeric()
  sampledNov<-numeric()
  sampledDec<-numeric()
  
  ###CREATE BIRD PARAMETER DATA FRAME###
  sampledBirdParams = data.frame(matrix(data = 0, ncol = 7, nrow = iter))
  names(sampledBirdParams) = c("AvoidanceBasic", "AvoidanceExtended",  "WingSpan", "BodyLength", "PCH", "FlightSpeed", "NocturnalActivity")
  
  ###CREATE COUNT/DENSITY DATA FRAME###
  sampledSpeciesCount = data.frame(matrix(data = 0, ncol = 12, nrow = iter))
  names(sampledSpeciesCount) = c("Jan", "Feb", "Mar", "Apr", "May",   "Jun", "Jul",   "Aug", "Sep", "Oct", "Nov", "Dec")
  
  ###CRAETE DATA FRAME FOR DENSITY DATA###
  densitySummary=data.frame(matrix(data = 0, ncol = nrow(TurbineData)*3, nrow = iter))
  ##add names of columns later in turbine loop###
  
  
  for ( t in 1:nrow(TurbineData))  {
    
    ######set turbine parameter vectors######
    
    sampledBladeWidth<-numeric()
    sampledRotorRadius<-numeric()
    sampledHubHeight<-numeric()
    sampledRotorPitch<-numeric()
    sampledRotorSpeed<-numeric()
    
    ######set operational vectors######
    sampledJanOp<-numeric()
    sampledFebOp<-numeric()
    sampledMarOp<-numeric()
    sampledAprOp<-numeric()
    sampledMayOp<-numeric()
    sampledJunOp<-numeric()
    sampledJulOp<-numeric()
    sampledAugOp<-numeric()
    sampledSepOp<-numeric()
    sampledOctOp<-numeric()
    sampledNovOp<-numeric()
    sampledDecOp<-numeric()
    
    ###CREATE TURBINE DATA FRAME###
    sampledTurbine = data.frame(matrix(data = 0, ncol = 17, nrow = iter))
    names(sampledTurbine) = c("RotorSpeed", "RotorRadius", "HubHeight", "BladeWidth", "Pitch", "JanOp", "FebOp", "MarOp", "AprOp", "MayOp",   "JunOp", "JulOp",   "AugOp", "SepOp", "OctOp", "NovOp", "DecOp")

    
    ## create results tables
    tab1 = data.frame(matrix(data = 0, ncol = 12, nrow = iter))
    names(tab1) = c("Jan", "Feb", "Mar", "Apr", "May", 	"Jun", "Jul", 	"Aug", "Sep", "Oct", "Nov", "Dec")
    
    tab2 = data.frame(matrix(data = 0, ncol = 12, nrow = iter))
    names(tab2) = c("Jan", "Feb", "Mar", "Apr", "May",   "Jun", "Jul", 	"Aug", "Sep", "Oct", "Nov", "Dec")
    
    tab3 = data.frame(matrix(data = 0, ncol = 12, nrow = iter))
    names(tab3) = c("Jan", "Feb", "Mar", "Apr", "May",   "Jun", "Jul",   "Aug", "Sep", "Oct", "Nov", "Dec")
    
    ###set vectors to store PCol and CollInt###
    sampledPColl<-data.frame(matrix(data = 0, ncol = 1, nrow = iter))
    names(sampledPColl)="PColl"
    sampledCollInt<-data.frame(matrix(data = 0, ncol = 1, nrow = iter))
    names(sampledCollInt)="CollInt"
        
for (i in 1:iter){
  
  #######################################
  ####SAMPLE BIRD PARAMETERS AND SAVE####
  #######################################
  source("scripts\\samplebirdparams.txt")

  
  ##########################################
  ####SAMPLE TURBINE PARAMETERS AND SAVE####
  ##########################################
  source("scripts\\sampleturbineparams.txt")
  
  
  ##########################################
  ########SAMPLE COUNT/DENSITY DATA AND SAVE########
  ##########################################
  source("scripts\\samplemonthlycounts.txt")
  
  
  #######################################
  ######Start collision Risk model#######
  #######################################
  
  ############## STEP ONE - Calculate the collision risk in the absence of avoidance action
  
  source("scripts\\ProbabilityCollision.txt")
  
  
  ############## STEP TWO - Calculate Flux Factor - the number of birds passing a turbine in each month
  
  ## First calculate turbine frontal area
  
  NTurbines = round (TPower / TurbineData$TurbineModel[t]) ### Number of turbines of given Output required to produce target output
  TotalFrontalArea = NTurbines * pi * sampledRotorRadius[i] ^2
  
  #### Calculate the total number of birds passing through the wind farm in each month
  
  
  for (h in 1:nrow(hours)) { 
    
    hours$Flux[h] = sampledbirdSpeed[i] * sampledCountIter[h]/ (2 * sampledRotorRadius[i]) * TotalFrontalArea *
      (hours$Day[h] + sampledNocturnal[i] * hours$Night[h]) * 3600/1000000
    
  }
  
  
  ############## STEP THREE - Calculate Large Array Correction Factor
  
  ## calculate number of turbine rows - manually enter if appropriate
  
  NTurbRows = NTurbines ^ 0.5
  
  MeanOperational = mean(c(sampledJanOp[i],sampledFebOp[i],sampledMarOp[i],sampledAprOp[i],sampledMayOp[i],sampledJunOp[i],sampledJulOp[i],sampledAugOp[i],sampledSepOp[i],sampledOctOp[i],sampledNovOp[i],sampledDecOp[i]))
  
  CollRiskSinglePassage = NTurbines * (pi * sampledRotorRadius[i]^2)/(2 * sampledRotorRadius[i] * WFWidth * 1000) * 			(P_Collision/100) * (MeanOperational/100) * (1-sampledAvoidanceBasic[i])
  
  L_ArrayCF = 1 - (NTurbRows - 1) / (2*NTurbRows) * CollRiskSinglePassage + 
    (NTurbRows - 1) * (2*NTurbRows)/(6 * NTurbRows^2) * (CollRiskSinglePassage ^2)
  
  

  #######################		Do model using option 1 - Site specific flight height information	###############################
  
  source("scripts\\Option1.txt")
  
  ## add results to overall species/turbine results table
  tab1[i,]=Option1_CollisionRate[,2]
  
  #store P_Coll
  sampledPColl[i,]<-P_Collision/100

 
  #######################		Do model using option 2 - modelled flight height distribution		###############################
  
  source("scripts\\Option2.txt")
## add results to overall species/turbine results table
tab2[i,]=Option2_CollisionRate[,2]
  
 
  #######################		Do model using option 3 - modelled flight height distribution		###############################
  #######################		taking account of variation in risk along the rotor blades		###############################
  
  source("scripts\\Option3.txt")
## add results to overall species/turbine results table
tab3[i,]=Option3_CollisionRate[,2]  

#Store Collision Integral
sampledCollInt[i,]<-CollInt
 
##progress bar for iterations##
#setTxtProgressBar(pb, s*t+i)
setTxtProgressBar(pb, (s*nrow(TurbineData)-(nrow(TurbineData)-t))*iter-(iter-i))

} 

###relabel sampledTurbine by turbine name###
assign(paste(TurbineData$TurbineModel[t],"params", sep="_"), sampledTurbine)

###relabel results table
assign(paste(CRSpecies[s],TurbineData$TurbineModel[t],"opt1", "results", sep="_"), tab1)
assign(paste(CRSpecies[s],TurbineData$TurbineModel[t],"opt2", "results", sep="_"), tab2)
assign(paste(CRSpecies[s],TurbineData$TurbineModel[t],"opt3", "results", sep="_"), tab3)

###output monthly summaries###
###OPTION 1###
monthlySummaryOpt1 = data.frame(matrix(data = 0, ncol = 6, nrow = 12))
names(monthlySummaryOpt1) = c("Month", "Mean", "SD", "CV", "Median", "IQR")
monthlySummaryOpt1[,1]=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",  "Aug", "Sep", "Oct", "Nov", "Dec")
monthlySummaryOpt1[1,2:6]=c(mean(tab1$Jan),sd(tab1$Jan), CV(mean(tab1$Jan),sd(tab1$Jan)), median(tab1$Jan),IQR(tab1$Jan))
monthlySummaryOpt1[2,2:6]=c(mean(tab1$Feb),sd(tab1$Feb),CV(mean(tab1$Feb),sd(tab1$Feb)), median(tab1$Feb),IQR(tab1$Feb))
monthlySummaryOpt1[3,2:6]=c(mean(tab1$Mar),sd(tab1$Mar),CV(mean(tab1$Mar),sd(tab1$Mar)), median(tab1$Mar),IQR(tab1$Mar))
monthlySummaryOpt1[4,2:6]=c(mean(tab1$Apr),sd(tab1$Apr),CV(mean(tab1$Apr),sd(tab1$Apr)), median(tab1$Apr),IQR(tab1$Apr))
monthlySummaryOpt1[5,2:6]=c(mean(tab1$May),sd(tab1$May),CV(mean(tab1$May),sd(tab1$May)), median(tab1$May),IQR(tab1$May))
monthlySummaryOpt1[6,2:6]=c(mean(tab1$Jun),sd(tab1$Jun),CV(mean(tab1$Jun),sd(tab1$Jun)), median(tab1$Jun),IQR(tab1$Jun))
monthlySummaryOpt1[7,2:6]=c(mean(tab1$Jul),sd(tab1$Jul),CV(mean(tab1$Jul),sd(tab1$Jul)), median(tab1$Jul),IQR(tab1$Jul))
monthlySummaryOpt1[8,2:6]=c(mean(tab1$Aug),sd(tab1$Aug),CV(mean(tab1$Aug),sd(tab1$Aug)), median(tab1$Aug),IQR(tab1$Aug))
monthlySummaryOpt1[9,2:6]=c(mean(tab1$Sep),sd(tab1$Sep),CV(mean(tab1$Sep),sd(tab1$Sep)), median(tab1$Sep),IQR(tab1$Sep))
monthlySummaryOpt1[10,2:6]=c(mean(tab1$Oct),sd(tab1$Oct),CV(mean(tab1$Oct),sd(tab1$Oct)), median(tab1$Oct),IQR(tab1$Oct))
monthlySummaryOpt1[11,2:6]=c(mean(tab1$Nov),sd(tab1$Nov),CV(mean(tab1$Nov),sd(tab1$Nov)), median(tab1$Nov),IQR(tab1$Nov))
monthlySummaryOpt1[12,2:6]=c(mean(tab1$Dec),sd(tab1$Dec),CV(mean(tab1$Dec),sd(tab1$Dec)), median(tab1$Dec),IQR(tab1$Dec))

###OPTION 2###
monthlySummaryOpt2 = data.frame(matrix(data = 0, ncol = 6, nrow = 12))
names(monthlySummaryOpt2) = c("Month", "Mean", "SD","CV", "Median", "IQR")
monthlySummaryOpt2[,1]=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",  "Aug", "Sep", "Oct", "Nov", "Dec")
monthlySummaryOpt2[1,2:6]=c(mean(tab2$Jan),sd(tab2$Jan),CV(mean(tab2$Jan),sd(tab2$Jan)), median(tab2$Jan),IQR(tab2$Jan))
monthlySummaryOpt2[2,2:6]=c(mean(tab2$Feb),sd(tab2$Feb),CV(mean(tab2$Feb),sd(tab2$Feb)), median(tab2$Feb),IQR(tab2$Feb))
monthlySummaryOpt2[3,2:6]=c(mean(tab2$Mar),sd(tab2$Mar), CV(mean(tab2$Mar),sd(tab2$Mar)), median(tab2$Mar),IQR(tab2$Mar))
monthlySummaryOpt2[4,2:6]=c(mean(tab2$Apr),sd(tab2$Apr),CV(mean(tab2$Apr),sd(tab2$Apr)), median(tab2$Apr),IQR(tab2$Apr))
monthlySummaryOpt2[5,2:6]=c(mean(tab2$May),sd(tab2$May),CV(mean(tab2$May),sd(tab2$May)), median(tab2$May),IQR(tab2$May))
monthlySummaryOpt2[6,2:6]=c(mean(tab2$Jun),sd(tab2$Jun),CV(mean(tab2$Jun),sd(tab2$Jun)), median(tab2$Jun),IQR(tab2$Jun))
monthlySummaryOpt2[7,2:6]=c(mean(tab2$Jul),sd(tab2$Jul),CV(mean(tab2$Jul),sd(tab2$Jul)), median(tab2$Jul),IQR(tab2$Jul))
monthlySummaryOpt2[8,2:6]=c(mean(tab2$Aug),sd(tab2$Aug),CV(mean(tab2$Aug),sd(tab2$Aug)), median(tab2$Aug),IQR(tab2$Aug))
monthlySummaryOpt2[9,2:6]=c(mean(tab2$Sep),sd(tab2$Sep),CV(mean(tab2$Sep),sd(tab2$Sep)), median(tab2$Sep),IQR(tab2$Sep))
monthlySummaryOpt2[10,2:6]=c(mean(tab2$Oct),sd(tab2$Oct),CV(mean(tab2$Oct),sd(tab2$Oct)), median(tab2$Oct),IQR(tab2$Oct))
monthlySummaryOpt2[11,2:6]=c(mean(tab2$Nov),sd(tab2$Nov),CV(mean(tab2$Nov),sd(tab2$Nov)), median(tab2$Nov),IQR(tab2$Nov))
monthlySummaryOpt2[12,2:6]=c(mean(tab2$Dec),sd(tab2$Dec),CV(mean(tab2$Dec),sd(tab2$Dec)), median(tab2$Dec),IQR(tab2$Dec))

###OPTION 3###
monthlySummaryOpt3 = data.frame(matrix(data = 0, ncol = 6, nrow = 12))
names(monthlySummaryOpt3) = c("Month", "Mean", "SD", "CV", "Median", "IQR")
monthlySummaryOpt3[,1]=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",  "Aug", "Sep", "Oct", "Nov", "Dec")
monthlySummaryOpt3[1,2:6]=c(mean(tab3$Jan),sd(tab3$Jan),CV(mean(tab3$Jan),sd(tab3$Jan)), median(tab3$Jan),IQR(tab3$Jan))
monthlySummaryOpt3[2,2:6]=c(mean(tab3$Feb),sd(tab3$Feb),CV(mean(tab3$Feb),sd(tab3$Feb)), median(tab3$Feb),IQR(tab3$Feb))
monthlySummaryOpt3[3,2:6]=c(mean(tab3$Mar),sd(tab3$Mar),CV(mean(tab3$Mar),sd(tab3$Mar)), median(tab3$Mar),IQR(tab3$Mar))
monthlySummaryOpt3[4,2:6]=c(mean(tab3$Apr),sd(tab3$Apr),CV(mean(tab3$Apr),sd(tab3$Apr)), median(tab3$Apr),IQR(tab3$Apr))
monthlySummaryOpt3[5,2:6]=c(mean(tab3$May),sd(tab3$May),CV(mean(tab3$May),sd(tab3$May)), median(tab3$May),IQR(tab3$May))
monthlySummaryOpt3[6,2:6]=c(mean(tab3$Jun),sd(tab3$Jun),CV(mean(tab3$Jun),sd(tab3$Jun)), median(tab3$Jun),IQR(tab3$Jun))
monthlySummaryOpt3[7,2:6]=c(mean(tab3$Jul),sd(tab3$Jul),CV(mean(tab3$Jul),sd(tab3$Jul)), median(tab3$Jul),IQR(tab3$Jul))
monthlySummaryOpt3[8,2:6]=c(mean(tab3$Aug),sd(tab3$Aug),CV(mean(tab3$Aug),sd(tab3$Aug)), median(tab3$Aug),IQR(tab3$Aug))
monthlySummaryOpt3[9,2:6]=c(mean(tab3$Sep),sd(tab3$Sep),CV(mean(tab3$Sep),sd(tab3$Sep)), median(tab3$Sep),IQR(tab3$Sep))
monthlySummaryOpt3[10,2:6]=c(mean(tab3$Oct),sd(tab3$Oct),CV(mean(tab3$Oct),sd(tab3$Oct)), median(tab3$Oct),IQR(tab3$Oct))
monthlySummaryOpt3[11,2:6]=c(mean(tab3$Nov),sd(tab3$Nov),CV(mean(tab3$Nov),sd(tab3$Nov)), median(tab3$Nov),IQR(tab3$Nov))
monthlySummaryOpt3[12,2:6]=c(mean(tab3$Dec),sd(tab3$Dec),CV(mean(tab3$Dec),sd(tab3$Dec)), median(tab3$Dec),IQR(tab3$Dec))

###SAVE MONTHLY SUMMARIES###
fileName<-paste(TurbineData$TurbineModel[t],CRSpecies[s],"monthlySummaryOpt1.csv", sep="_")
write.csv (monthlySummaryOpt1, paste(results_folder,"tables",  fileName, sep="\\"))
fileName<-paste(TurbineData$TurbineModel[t],CRSpecies[s],"monthlySummaryOpt2.csv", sep="_")
write.csv (monthlySummaryOpt2, paste(results_folder,"tables",  fileName, sep="\\"))
fileName<-paste(TurbineData$TurbineModel[t],CRSpecies[s],"monthlySummaryOpt3.csv", sep="_")
write.csv (monthlySummaryOpt3, paste(results_folder,"tables",  fileName, sep="\\"))

###DATA TO DENSITY SUMMARY TABLE###
densitySummary[,(t-1)*3+1]=rowSums(tab1)
densitySummary[,(t-1)*3+2]=rowSums(tab2)
densitySummary[,(t-1)*3+3]=rowSums(tab3)

###make 3 panel figure of boxplots and save###

fileName<-paste(TurbineData$TurbineModel[t],CRSpecies[s], sep="_")
fileName<-paste(fileName, ".png", sep="")
png(paste(results_folder, "figures", fileName, sep="\\"),width=500,height=900,res=100)
par(mfrow = c( 3, 1))

boxplot(tab1, outline=F, ylim=c(0,max(max(tab1), max(tab2), max(tab3))),ylab="Number of collisions", main="Option 1")
boxplot(tab2, outline=F, ylim=c(0,max(max(tab1), max(tab2), max(tab3))),ylab="Number of collisions", main="Option 2")
boxplot(tab3, outline=F, ylim=c(0,max(max(tab1), max(tab2), max(tab3))),ylab="Number of collisions", main="Option 3")

dev.off()

###make density plots and save###
fileName<-paste(TurbineData$TurbineModel[t],CRSpecies[s], "density",sep="_")
fileName<-paste(fileName, ".png", sep="")
png(paste(results_folder, "figures", fileName, sep="\\"),width=800,height=600,res=100)

plot(density(rowSums(tab1)), main="", xlab="Number of Collisions", ylab ="Probability Density", col="chocolate",xlim=(c(0, max(max(density(rowSums(tab1))$x),max(density(rowSums(tab2))$x),max(density(rowSums(tab3))$x)))), ylim=(c(0, max(max(density(rowSums(tab1))$y),max(density(rowSums(tab2))$y),max(density(rowSums(tab3))$y)))))
lines(density(rowSums(tab2)), col="darkgoldenrod", lty=2)
lines(density(rowSums(tab3)), col="darkorange4", lty=3)


legend("topright", c("Option 1", "Option 2", "Option 3"), cex=0.8, lty=1:3, col = c("chocolate", "darkgoldenrod", "darkorange4"))

dev.off()

##add results summary to summary table###
resultsSummary[(s*nrow(TurbineData)-(nrow(TurbineData)-t))*3-2,]<-c(CRSpecies[s], TurbineData$TurbineModel[t], 1, mean(rowSums(tab1)),sd(rowSums(tab1)), CV(mean(rowSums(tab1)),sd(rowSums(tab1))), median(rowSums(tab1)), IQR(rowSums(tab1))) #option 1
resultsSummary[(s*nrow(TurbineData)-(nrow(TurbineData)-t))*3-1,]<-c(CRSpecies[s], TurbineData$TurbineModel[t], 2, mean(rowSums(tab2)),sd(rowSums(tab2)), CV(mean(rowSums(tab2)),sd(rowSums(tab2))), median(rowSums(tab2)), IQR(rowSums(tab2))) #option 2
resultsSummary[(s*nrow(TurbineData)-(nrow(TurbineData)-t))*3-0,]<-c(CRSpecies[s], TurbineData$TurbineModel[t], 3, mean(rowSums(tab3)),sd(rowSums(tab3)), CV(mean(rowSums(tab3)),sd(rowSums(tab3))), median(rowSums(tab3)), IQR(rowSums(tab3))) #option 3
#resultsSummary[t*s*3-0,]<-c(CRSpecies[s], TurbineData$TurbineModel[t], 3, mean(rowSums(tab3)),sd(rowSums(tab3)), median(rowSums(tab3)), IQR(rowSums(tab3))) #option 3

###create summary tables of input parameters###

###BIRD PARAMETERS###
sampledBirdParamsSummary = data.frame(matrix(data = 0, ncol = 5, nrow = 7))
names(sampledBirdParamsSummary)=c("Parameter","Mean", "SD", "Median", "IQR")
sampledBirdParamsSummary[,1] = c("AvoidanceBasic", "AvoidanceExtended",  "WingSpan", "BodyLength", "PCH", "FlightSpeed", "NocturnalActivity")

##write summaries###
sampledBirdParamsSummary[1,2:5]=c(mean(sampledBirdParams$AvoidanceBasic), sd(sampledBirdParams$AvoidanceBasic), median(sampledBirdParams$AvoidanceBasic), IQR(sampledBirdParams$AvoidanceBasic))
sampledBirdParamsSummary[2,2:5]=c(mean(sampledBirdParams$AvoidanceExtended), sd(sampledBirdParams$AvoidanceExtended), median(sampledBirdParams$AvoidanceExtended), IQR(sampledBirdParams$AvoidanceExtended))
sampledBirdParamsSummary[3,2:5]=c(mean(sampledBirdParams$WingSpan), sd(sampledBirdParams$WingSpan), median(sampledBirdParams$WingSpan), IQR(sampledBirdParams$WingSpan))
sampledBirdParamsSummary[4,2:5]=c(mean(sampledBirdParams$BodyLength), sd(sampledBirdParams$BodyLength), median(sampledBirdParams$BodyLength), IQR(sampledBirdParams$BodyLength))
sampledBirdParamsSummary[5,2:5]=c(mean(sampledBirdParams$PCH), sd(sampledBirdParams$PCH), median(sampledBirdParams$PCH), IQR(sampledBirdParams$PCH))
sampledBirdParamsSummary[6,2:5]=c(mean(sampledBirdParams$FlightSpeed), sd(sampledBirdParams$FlightSpeed), median(sampledBirdParams$FlightSpeed), IQR(sampledBirdParams$FlightSpeed))
sampledBirdParamsSummary[7,2:5]=c(mean(sampledBirdParams$NocturnalActivity), sd(sampledBirdParams$NocturnalActivity), median(sampledBirdParams$NocturnalActivity), IQR(sampledBirdParams$NocturnalActivity))

###output parameter table###
fileName<-paste(TurbineData$TurbineModel[t],CRSpecies[s],"sampledBirdParameters.csv", sep="_")
write.csv (sampledBirdParamsSummary, paste(results_folder,"tables",  fileName, sep="\\"))


###TURBINE PARAMETERS###
sampledTurbineParamsSummary = data.frame(matrix(data = 0, ncol = 5, nrow = 17))
names(sampledTurbineParamsSummary)=c("Parameter","Mean", "SD", "Median", "IQR")
sampledTurbineParamsSummary[,1] = c("RotorSpeed", "RotorRadius", "HubHeight", "BladeWidth", "Pitch", "JanOp", "FebOp", "MarOp", "AprOp", "MayOp",   "JunOp", "JulOp",   "AugOp", "SepOp", "OctOp", "NovOp", "DecOp")

##write summaries###
sampledTurbineParamsSummary[1,2:5]=c(mean(sampledTurbine$RotorSpeed), sd(sampledTurbine$RotorSpeed), median(sampledTurbine$RotorSpeed), IQR(sampledTurbine$RotorSpeed))
sampledTurbineParamsSummary[2,2:5]=c(mean(sampledTurbine$RotorRadius), sd(sampledTurbine$RotorRadius), median(sampledTurbine$RotorRadius), IQR(sampledTurbine$RotorRadius))
sampledTurbineParamsSummary[3,2:5]=c(mean(sampledTurbine$HubHeight), sd(sampledTurbine$HubHeight), median(sampledTurbine$HubHeight), IQR(sampledTurbine$HubHeight))
sampledTurbineParamsSummary[4,2:5]=c(mean(sampledTurbine$BladeWidth), sd(sampledTurbine$BladeWidth), median(sampledTurbine$BladeWidth), IQR(sampledTurbine$BladeWidth))
sampledTurbineParamsSummary[5,2:5]=c(mean(sampledTurbine$Pitch), sd(sampledTurbine$Pitch), median(sampledTurbine$Pitch), IQR(sampledTurbine$Pitch))
sampledTurbineParamsSummary[6,2:5]=c(mean(sampledTurbine$JanOp), sd(sampledTurbine$JanOp), median(sampledTurbine$JanOp), IQR(sampledTurbine$JanOp))
sampledTurbineParamsSummary[7,2:5]=c(mean(sampledTurbine$FebOp), sd(sampledTurbine$FebOp), median(sampledTurbine$FebOp), IQR(sampledTurbine$FebOp))
sampledTurbineParamsSummary[8,2:5]=c(mean(sampledTurbine$MarOp), sd(sampledTurbine$MarOp), median(sampledTurbine$MarOp), IQR(sampledTurbine$MarOp))
sampledTurbineParamsSummary[9,2:5]=c(mean(sampledTurbine$AprOp), sd(sampledTurbine$AprOp), median(sampledTurbine$AprOp), IQR(sampledTurbine$AprOp))
sampledTurbineParamsSummary[10,2:5]=c(mean(sampledTurbine$MayOp), sd(sampledTurbine$MayOp), median(sampledTurbine$MayOp), IQR(sampledTurbine$MayOp))
sampledTurbineParamsSummary[11,2:5]=c(mean(sampledTurbine$JunOp), sd(sampledTurbine$JunOp), median(sampledTurbine$JunOp), IQR(sampledTurbine$JunOp))
sampledTurbineParamsSummary[12,2:5]=c(mean(sampledTurbine$JulOp), sd(sampledTurbine$JulOp), median(sampledTurbine$JulOp), IQR(sampledTurbine$JulOp))
sampledTurbineParamsSummary[13,2:5]=c(mean(sampledTurbine$AugOp), sd(sampledTurbine$AugOp), median(sampledTurbine$AugOp), IQR(sampledTurbine$AugOp))
sampledTurbineParamsSummary[14,2:5]=c(mean(sampledTurbine$SepOp), sd(sampledTurbine$SepOp), median(sampledTurbine$SepOp), IQR(sampledTurbine$SepOp))
sampledTurbineParamsSummary[15,2:5]=c(mean(sampledTurbine$OctOp), sd(sampledTurbine$OctOp), median(sampledTurbine$OctOp), IQR(sampledTurbine$OctOp))
sampledTurbineParamsSummary[16,2:5]=c(mean(sampledTurbine$NovOp), sd(sampledTurbine$NovOp), median(sampledTurbine$NovOp), IQR(sampledTurbine$NovOp))
sampledTurbineParamsSummary[17,2:5]=c(mean(sampledTurbine$DecOp), sd(sampledTurbine$DecOp), median(sampledTurbine$DecOp), IQR(sampledTurbine$DecOp))

###output parameter table###
fileName<-paste(TurbineData$TurbineModel[t],CRSpecies[s],"sampledTurbineParameters.csv", sep="_")
write.csv (sampledTurbineParamsSummary, paste(results_folder, "tables",fileName, sep="\\"))

}

###output species plots of density by option with curves for turbine model###
###PLOT DENSITY BY OPTION (useful if several turbine models)###

if (nrow(TurbineData)>1)  {

option1<-numeric()
option2<-numeric()
option3<-numeric()
max1<-0
max2<-0
max3<-0
max1y<-0
max2y<-0
max3y<-0

for (j in 1:nrow(TurbineData)){
  option1<-cbind(option1,densitySummary[,(j-1)*3+1])
  ifelse (max(density(densitySummary[,(j-1)*3+1])$x)>max1,max1<-max(density(densitySummary[,(j-1)*3+1])$x),max1)
  ifelse (max(density(densitySummary[,(j-1)*3+1])$y)>max1y,max1y<-max(density(densitySummary[,(j-1)*3+1])$y),max1y)
  option2<-cbind(option2,densitySummary[,(j-1)*3+2])
  ifelse (max(density(densitySummary[,(j-1)*3+2])$x)>max2,max2<-max(density(densitySummary[,(j-1)*3+2])$x),max2)
  ifelse (max(density(densitySummary[,(j-1)*3+2])$y)>max2y,max2y<-max(density(densitySummary[,(j-1)*3+2])$y),max2y)
  option3<-cbind(option3,densitySummary[,(j-1)*3+3])
  ifelse (max(density(densitySummary[,(j-1)*3+3])$x)>max3,max3<-max(density(densitySummary[,(j-1)*3+3])$x),max3)
  ifelse (max(density(densitySummary[,(j-1)*3+3])$y)>max3y,max3y<-max(density(densitySummary[,(j-1)*3+3])$y),max3y)
}
  
fileName<-CRSpecies[s]
fileName<-paste(fileName, ".png", sep="")
png(paste(results_folder, "figures", fileName, sep="\\"),width=500,height=900,res=100)
par(mfrow = c(3, 1))

plot(density(option1[,1]), main="Option 1", xlab="Number of Collisions", ylab ="Probability Density", xlim=c(0,max1), ylim=(c(0, max1y)))
names<-paste("Turbine model",TurbineData$TurbineModel[1], sep=":")
for (i in 2:nrow(TurbineData)){
  lines(density(option1[,i]), lty=i)
  names<-c(names,paste("Turbine model",TurbineData$TurbineModel[i], sep=":"))
}
legend("topright", names, cex=0.8, lty=1:nrow(TurbineData))

plot(density(option2[,1]), main="Option 2", xlab="Number of Collisions", ylab ="Probability Density", xlim=c(0,max2), ylim=(c(0, max2y)))
names<-paste("Turbine model",TurbineData$TurbineModel[1], sep=":")
for (i in 2:nrow(TurbineData)){
  lines(density(option2[,i]), lty=i)
  names<-c(names,paste("Turbine model",TurbineData$TurbineModel[i], sep=":"))
}
legend("topright", names, cex=0.8, lty=1:nrow(TurbineData))

plot(density(option3[,1]), main="Option 3", xlab="Number of Collisions", ylab ="Probability Density",xlim=c(0,max3), ylim=(c(0, max3y)))
names<-paste("Turbine model",TurbineData$TurbineModel[1], sep=":")
for (i in 2:nrow(TurbineData)){
  lines(density(option3[,i]), lty=i)
  names<-c(names,paste("Turbine model",TurbineData$TurbineModel[i], sep=":"))
}
legend("topright", names, cex=0.8, lty=1:nrow(TurbineData))

dev.off()
}

###relabel sampledBirdParams by species name###
assign(paste(CRSpecies[s],"params", sep="_"), sampledBirdParams)

###relabel sampledSpeciesCount by species name###
assign(paste(CRSpecies[s],"counts", sep="_"), sampledSpeciesCount)


}

##output input data##
write.csv(BirdData, paste(results_folder,"input", "BirdData.csv", sep="\\"))
write.csv(CountData, paste(results_folder,"input", "CountData.csv", sep="\\"))
write.csv(TurbineData, paste(results_folder,"input", "TurbineData.csv", sep="\\"))

###output results table###
write.csv (resultsSummary, paste(results_folder,"tables", "CollisionEstimates.csv", sep="\\"))


end.time <- Sys.time()
run.time <- end.time - start.time
run.time

sink(paste(results_folder,"run.time.txt", sep="\\"))
print(run.time)
print(paste("The model ran", iter,"iterations", sep=" "))
print("The following species were modelled:")
print(CRSpecies)
print("The following turbines were modelled:")
print(TurbineData$TurbineModel)
sink()



# Comparision with Band model outputs -------------------------------------

  library(tidyverse)

  masdenEstimates <- bind_cols(monthlySummaryOpt1, monthlySummaryOpt2, monthlySummaryOpt3) %>%
                          select(Month, Mean, Mean1, Mean2) %>% rename(Option1 = Mean, Option2 = Mean1, Option3 = Mean2)
  
  # pre-prepared by running Final_Report_SOSS02_Band5SpreadsheetWorkedExample1.xlsm
  bandEstimates <- read_csv("band_estimates_gannet.csv")
  
  bind_cols(masdenEstimates, bandEstimates) %>% mutate(diffs1 = Option1 - Option1_Avoidance98,
                                                       diffs2 = Option2 - Option2_Avoidance98,
                                                       diffs3 = Option3 - Option3_Avoidance98,
                                                       propBias1 = diffs1/Option1_Avoidance98,
                                                       propBias2 = diffs2/Option2_Avoidance98,
                                                       propBias3 = diffs3/Option3_Avoidance98
                                                       )
  
  
  
  
                        
                        

  


