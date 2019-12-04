## Author: A.D. Wright
## Project: NCRN Amphibians - Multi-Region Community Occupancy Model
## Code: Data Management - Automated

#rm(list = ls())
#options(max.print = 1000)

## TABLE OF CONTENTS
  ## PACKAGES, WORKING DIRECTORY, & DATA                20
  ## DATA MANIUPLATION OF SPECIES DETECTION DATA        65
  ## DATA MANIPULATION OF SURVEY & HABITAT DATA         228
  ## MERGE SPP AND HABITAT DATA & SUMMARY STATISTICS    351
  ## DATA MANIUPLATION OF NON-NEARMI DATA               430
  ## MERGE NEARMI AND NPS DATA                          794
  ## RESHAPING DETECTION DATA FOR JAGS MODEL ANALYSIS   845
  ## CREATING COVARIATE DATA FOR JAGS ANALYSIS          1050
  ## JAGS ANALYSIS DATA                                 1252

#########
## Part - PACKAGES, WORKING DIRECTORy, & DATA
#########

##
#### Install Packages
##

#The following code checks to see if a package is already installed, installs package (if necessary), and then loads the package in to the library for use:
    #if(!require(_package-name_)) {install.packages("_package-name_");require(_package-name_)}
  #Reshape - Used for data manipulation
if(!require(reshape)) {install.packages("reshape");require(reshape)}
  #xlsx - Used to import excel files
if(!require(readxl)) {install.packages("readxl");require(readxl)}
  #tidyverse
if(!require(tidyverse)) {install.packages("tidyverse");require(tidyverse)}
  #lubridate - used for date manipulation
if(!require(lubridate)) {install.packages("lubridate");require(lubridate)}


##
#### Set Working Directory to Data Location
##

## Set working directory
  ##setwd()

##
#### Import Data
##

#Bring in necessary data
  #Species Detection/Survey Data (All years - NEARMI collected)
frogRaw <- as.data.frame(read_excel("NCR_ALL.xlsx", sheet = "NCRSpecies_ALL"))
siteVisit <- as.data.frame(read_excel("NCR_ALL.xlsx", sheet = "NCRSurveys_ALL"))
  #Species Detection/Survey Data (2005-2013 @ Rock Creek/MANA - NOT NEARMI collected)
frogRaw_ROCR <- read.csv("ROCR_ACSExport.csv")
frogRaw_MANA <- read.csv("MANA_ACSExport.csv")
  #Wetland Permanecy Info
permData <- read.csv('NCRWetlandPermanencyInfo.csv') 
  #Precipitation Data (standardized - raw data not shared to keep wetland location data confidential)
load('precipitation.R')
  #Connectivity Data (standardized - raw data not shared to keep wetland location data confidential)
load('connectivity.R')

#########
## Part - DATA MANIUPLATION OF SPECIES DETECTION DATA (NEARMI DATA)
#########

##
#### Visual Inspection of Data & Data Structure
## 

head(frogRaw)
str(frogRaw)

##
#### Reformat dataframe structure
## 

#Columns of interest
frogRaw <- frogRaw %>% select(2:11)
  #Some basic information on this data frame
    # "UnitName" - National Park
    # "PointName" - Sampling Site (Wetland)
    # "SurveyDate" - m/d/y of sampling event
    # "SpeciesCode" - 4-letter acronym of each species found at each site during each sampling event
    # "SpeciesAgeID" -  1=adult, 3=juvenile, 4-6=metamorph, 7=larvae, 8=hatchling, 9=egg mass
    # "DetectionID" -  1=handled, 2=visual, 3=auditory
    # "SCount#_#" - 4 columns, for example, "SCounts1_1" is Observer 1 Pass 1

#Standardize column classes (integer, factor, etc.)
  #UnitName
frogRaw$UnitName <- as.character(frogRaw$UnitName)
  #PointName
frogRaw$PointName <- as.character(frogRaw$PointName)
  #SpeciesCode
frogRaw$SpeciesCode <- as.character(frogRaw$SpeciesCode)
  #Counts
    #Need to convert to character (from factor) first, then to integer. Otherwise, the conversion is not accurate.
frogRaw$SCount1 <- as.integer(as.character(frogRaw$SCount1))
frogRaw$SCount1_2 <- as.integer(as.character(frogRaw$SCount1_2))
frogRaw$SCount2 <- as.integer(as.character(frogRaw$SCount2))
frogRaw$SCount2_2 <- as.integer(as.character(frogRaw$SCount2_2))


##
#### Turn "Date" into 3 columns "Year", "Month" & "Julian Day"
## 

  #Extract Year 
frogRaw$Year <- year(frogRaw$SDate)
  #Extract Month 
frogRaw$Month <- month(frogRaw$SDate)
  #Extract Julian Date 
frogRaw$JulianDate <- yday(frogRaw$SDate)

##
#### In "UnitName", convert full Park Name to its 4-letter acronym
##

  #ANTI and WOTR have never been sampled, so their full anmes might be entered differently in data if ever sampled
frogRaw$UnitName <- plyr::mapvalues(frogRaw$UnitName, 
                                    from = c("Antietam National Battlefield","Catoctin Mountain National Park","C & O Canal National Historic Park",
                                             "George Washington Memorial Parkway","Harper's Ferry National Historic Park","Manassas National Battlefield",
                                             "Monocacy National Battlefield","National Capitol Parks East","Prince William Forest National Park",
                                             "Rock Creek National Park","Wolf Trap National Park"), 
                                    to = c('ANTI','CATO','CHOH','GWMP','HAFE','MANA','MONO','NACE','PRWI','ROCR','WOTR'),
                                    warn_missing = F)
  #Convert from character to factor
frogRaw$UnitName <- as.factor(frogRaw$UnitName)


##
#### Reorganize dataset to include Park, Wetland, Year, Month, Julian Date, Species Code, Observer Counts
##

frogRaw <- frogRaw %>% select(UnitName, PointName, Year, Month, JulianDate, SpeciesCode, SCount1, SCount1_2, SCount2, SCount2_2)


##
#### Need to combine 1st and 2nd pass of each observer (SCount1_1 + SCount1_2 = SCount1) 
#### Need to create split each row into two rows: one for "SCount1" and the other for "SCount2"
##

##Splitting each visit/site into two rows 
  #First, you need to duplicate each row and assign it an observer (1 or 2)
frogRaw.Orig <- frogRaw
frogRaw <- frogRaw.Orig[rep(rownames(frogRaw.Orig),2),]
frogRaw$Obs[1:nrow(frogRaw.Orig)] <- 1
frogRaw$Obs[(nrow(frogRaw.Orig)+1):nrow(frogRaw)] <- 2
  #Create new column to hold the SCount data now referring to Pass 1 or 2. The observer data is now in its own column.
frogRaw$SCount_1 <- NA
frogRaw$SCount_2 <- NA
  #Create for loop to assign correct pass data to new column based on the observer
for(i in 1:nrow(frogRaw)){
  if(frogRaw$Obs[i] == 1){
    frogRaw$SCount_1[i] <- frogRaw$SCount1[i] 
    frogRaw$SCount_2[i] <- frogRaw$SCount1_2[i] 
    }else
      if(frogRaw$Obs[i] == 2){
        frogRaw$SCount_1[i] <- frogRaw$SCount2[i] 
        frogRaw$SCount_2[i] <- frogRaw$SCount2_2[i] 
      }
}

## Re-organize data to exlude columns containing both observer and pass data
frogRaw <- frogRaw %>% select(UnitName, PointName, Year, Month, JulianDate, SpeciesCode, Obs, SCount_1, SCount_2)

##Now, combine passes for the observer into one value (detected or not detected)
  #This will include changing anything >=1 to 1, make new column called "Occ"
  #If it was detected, in either pass, it is recorded as detected. If not detected in either pass, it is not-detected
  #First, need to replace all NAs within SCount_1 & SCount_2 with a 0
frogRaw$SCount_1[is.na(frogRaw$SCount_1)] <- 0
frogRaw$SCount_2[is.na(frogRaw$SCount_2)] <- 0
  #Now, a for loop determines occupancy based on counts
frogRaw$Occ <- NA
for(i in 1:nrow(frogRaw)){
  if(frogRaw$SCount_1[i] >= 1 | frogRaw$SCount_2[i] >= 1){
    frogRaw$Occ[i] <- 1
  }else{
    frogRaw$Occ[i] <- 0
  }
}

#In the step above, we are treating all NAs as true 0's. 


## Now need to condense dataframe
  #Remove all rows when a species wasn't detected (Occ = 0)
frogRaw <- frogRaw %>% filter(Occ == 1)
  #Remove all rows in which the species was listed as unknown
frogRaw <- frogRaw %>% filter(SpeciesCode != "UNK")
  #Reorganize to remove the columns "SCount_1" and "SCount_2"
occData <- frogRaw %>% select(UnitName, PointName, Year, Month, JulianDate, SpeciesCode, Obs, Occ)
  #Sort by Park, Site, Year, Julian Date, Observer
occData <- occData %>% arrange(Year, UnitName, PointName, JulianDate, Obs)
  #Rename columns to be more informative
occData <- occData %>% rename(Park = 'UnitName', Site = 'PointName', JDay = 'JulianDate', Spp ='SpeciesCode')
  #Remove duplicated rows (we are not considerent different life stages (see column "SpeciesAgeID"), so we can remove duplicates based on that trait)
occData <- distinct(occData)


##
#### Need to ignore and consolidate various species
##

unique(occData$Spp)
length(unique(occData$Spp))
## Evan recommends:
  # Consolidate HYLA, HCHR, HVER      === HYLA
  # Consolidate PSEU, PSSP with PCRU  === PCRU
  # Consolidate BAME, BFOW, and BUFO  === BUFO
  # Ignore AMBY, ANUR, EBIS, ELON, EURY, PCIN, PRUB, RANA, RPIP, 
      #CAUD, EGUT
# To do this, need to replace names of consolidated spp with new names(see above)
occData[occData$Spp == "HCHR" | occData$Spp == "HVER" ,6] <- "HYLA"
occData[occData$Spp == "PSEU" | occData$Spp == "PSSP" ,6] <- "PCRU"
occData[occData$Spp == "BFOW" | occData$Spp == "BAME" ,6] <- "BUFO"
# And remove captures of species in ignore section
occData <- occData[!(occData$Spp == "AMBY" | occData$Spp == "ANUR" | occData$Spp == "EBIS" | occData$Spp == "ELON"| occData$Spp == "EURY"
               | occData$Spp == "PCIN"| occData$Spp == "PRUB"| occData$Spp == "RANA"| occData$Spp == "RPIP" | occData$Spp == "CAUD" | occData$Spp == "EGUT" | occData$Spp == "DFUS"
               | occData$Spp == "GPOR"),]              
#Now need to delete duplicate rows, bc we combined some species
occData <- distinct(occData)



#########
## Part - DATA MANIPULATION OF SURVEY & HABITAT DATA (GENERAL)
#########


##
#### Visual Inspection of Data & Data Structure
## 

head(siteVisit)
str(siteVisit)


#Standardize column classes
siteVisit <- siteVisit %>% select(SiteRecID, UnitName, PointName, SurveyRecID, SDate, Visit, WaterPresent, Conductivity, MaxLength, MaxWidth, MaxDepth) %>% 
                    mutate(SiteRecID = as.integer(SiteRecID), UnitName = as.character(UnitName), PointName = as.character(PointName), SurveyRecID = as.integer(SurveyRecID),
                           Visit = as.integer(Visit), WaterPresent = factor(WaterPresent), Conductivity = as.numeric(Conductivity), MaxLength = as.numeric(MaxLength), 
                           MaxWidth = as.numeric(MaxWidth), MaxDepth = as.numeric(MaxDepth)
                          )


##
#### Turn "Date" into 3 columns "Year", "Month" & "Julian Day"
## 

  #Extract Year 
siteVisit$Year <- year(siteVisit$SDate)
  #Extract Month 
siteVisit$Month <- month(siteVisit$SDate)
  #Extract Julian Date 
siteVisit$JulianDate <- yday(siteVisit$SDate)


##
#### In "UnitName", convert full Park Name to its 4-letter acronym
##

#ANTI and WOTR have never been sampled, so their full anmes might be entered differently in data if ever sampled
siteVisit$UnitName <- plyr::mapvalues(siteVisit$UnitName, 
                                      from = c("Antietam National Battlefield","Catoctin Mountain National Park","C & O Canal National Historic Park",
                                             "George Washington Memorial Parkway","Harper's Ferry National Historic Park","Manassas National Battlefield",
                                             "Monocacy National Battlefield","National Capitol Parks East","Prince William Forest National Park",
                                             "Rock Creek National Park","Wolf Trap National Park"), 
                                      to = c('ANTI','CATO','CHOH','GWMP','HAFE','MANA','MONO','NACE','PRWI','ROCR','WOTR'),
                                      warn_missing = F)
#Convert from character to factor
siteVisit <- siteVisit %>% mutate(UnitName = factor(UnitName))

##
#### Need to reorganize SiteRecID to be in numeric order according to park
##

## Since SiteRecID is not in order as it relates to Parks (newer sites at older Parks get high numbers which throws the data array out of order)
  #Organize data frame by Park, and then SiteRecID
siteVisit <- siteVisit[order(siteVisit$UnitName,siteVisit$SiteRecID),]
  #Create new column
siteVisit$SiteRecIDOrder <- NA
siteVisit$SiteRecIDOrder[1] <- 1
for(i in 2:nrow(siteVisit)){
  if(siteVisit$SiteRecID[i] == siteVisit$SiteRecID[i-1]){
    siteVisit$SiteRecIDOrder[i] <- siteVisit$SiteRecIDOrder[i-1] 
  }else{
    siteVisit$SiteRecIDOrder[i] <- siteVisit$SiteRecIDOrder[i-1] + 1
  }
}


##
#### Need to modify Visit column to represent observers as individual visits, and to account for abscence of water
##

# Need to 1) Remove Survey Periods when water is not present, and 2) Correct the Field when water was present but recorded as "N"
    #You can tell when it had water but isn't showing, bc Max L/Max W/Max D will be greater than 0.
  #Correct wrongly recorded data
siteVisit$WaterPresent[siteVisit$WaterPresent == "N" & siteVisit$MaxLength > 0] <- "Y"
siteVisit$WaterPresent[siteVisit$WaterPresent == "NULL" & siteVisit$MaxLength > 0] <- "Y"
  #Remove survey visits when water is not present
siteVisit <- siteVisit %>% filter(WaterPresent == "Y")
  #Drop leftover factor levels
siteVisit$WaterPresent <- droplevels(siteVisit$WaterPresent)


#Need to get rid of 0's & NULL's
  #I double checked, none of those surveys resulted in captures. Many times it didn't have water, which means NO survey.
siteVisit <- siteVisit %>% filter(!is.na(Visit))
siteVisit <- siteVisit %>% filter(Visit != 0)


#Now that we have removed visits bc of no water, we need to rewrite "Visit" as those deleted no longer count as visits
  #Order Data by Site RecID, Year, and Julian Date
siteVisit <- siteVisit %>% arrange(SiteRecID, Year, JulianDate)
  #Now reorder Visits of sites/years to account for deleted visits (which should not be accounted for BC they don't count as a visit)
siteVisit$VisitC <- NA
siteVisit$VisitC[1] <- 1
for(i in 2:nrow(siteVisit)){
  if(siteVisit$SiteRecID[i] == siteVisit$SiteRecID[i-1] &&
    siteVisit$Year[i] == siteVisit$Year[i-1]){
    siteVisit$VisitC[i] <- siteVisit$VisitC[i-1] + 1 
  } else {
    siteVisit$VisitC[i] <- 1 
  }
}


##
#### Reorganize Dataset
##

siteVisit <- siteVisit %>% 
  select(UnitName, PointName, SiteRecID, SiteRecIDOrder, SurveyRecID, Year, Month, JulianDate, VisitC, WaterPresent, MaxLength, MaxWidth, MaxDepth, Conductivity)
colnames(siteVisit) <- c("Park","Site","SiteID","SiteIDOrder", "SurvID","Year","Month","JDay","Visit", "Water","MaxL","MaxW","MaxD","Cond")


##
#### Save formatted survey dataframe
##

surData <- siteVisit

#save(surData, file="surData.R")
#Saved on _DATE_


#########
## Part - MERGE SPP AND HABITAT DATA & SUMMARY STATISTICS
#########

##
#### Need to remove sites MD017 and MD142 (apparently were too big to survey, so were never revisited after 2005)
##

occData <- occData %>% filter(Site != "MD017", Site != "MD142")
surData <- surData %>% filter(Site != "MD017", Site != "MD142")

## Also need to remove sites that were all ruled out (from the original mapping - don't fit into the wetland category)
head(permData)
colnames(permData)[3] <- 'Site'
permData$Site <- as.character(permData$Site)
ruleOuts <- permData %>% filter(Ruled.Out. == 'x' | Ruled.Out. == 'X')


## Remove data from species and survey data
for(i in 1:nrow(ruleOuts)){
  occData <- occData[!(occData$Site == ruleOuts$Site[i]),]
  surData <- surData[!(surData$Site == ruleOuts$Site[i]),]
}

     
##
#### Merging the two data sets together
##

# Left outer join: 
dummy <- merge(occData, surData, by=c("Park","Site","Year","Month","JDay"),all.x=T)

# Double check there aren't any duplicate rows, that has been problem in the past
  # Problem in the past is that surData has some Survey's that were recorded twice
    #By recorded twice, I mean same data in all the columns but has a unique "SurvID"
nrow(dummy) - nrow(occData)
    #Great, it worked!
  #Could also check for duplicates using....
dummy[duplicated(dummy[,c(1:7)]),]

#Need to delete Obs where water wasn't present but species recorded anyways
  #This only happened on 2 occasions
#To check for these instances:
dummy[is.na(dummy$Water),]
#Really we're searching where the join didn't attach something to the left object
  #It seems like they surveyed it when it was dry (see the Survey Notes)
  #Bc it was considered dry, the survey of that site in that vist was exluded (See Zipkin 2012)
#To delete this instances:
dummy <- dummy[!(is.na(dummy$Water)),]


# Need to combine visit X observer. For example, 3 visits with 2 observers should really be six visits
  #Reorder rows
dummy <- dummy %>% arrange(Year, Park, Site, Spp, JDay, Obs)
  #Create VisxObs
dummy$VISxOBS <- NA
  #Loop to set value
for(i in 1:nrow(dummy)){
  if(dummy$Obs[i] == 1 && dummy$Visit[i] == 1){
    dummy$VISxOBS[i] <- 1
  }else if (dummy$Obs[i] == 1 && dummy$Visit[i] != 1){
    dummy$VISxOBS[i] <- dummy$Visit[i] + (dummy$Visit[i]-1)
  }else {
    dummy$VISxOBS[i] <- dummy$Visit[i] * 2
  }
}
  #Rearrange columns
dummy <- dummy %>% select(Park, Site, SiteID, SiteIDOrder, Year, Month, JDay, SurvID, Visit, Obs, VISxOBS, Spp, Occ, Water, MaxL, MaxW, MaxD, Cond)


##
#### Save formatted merged dataframe (if necessary)
##

presData <- dummy
#NOT SAVED YET, WILL VERIFY WHEN IT IS SAVED
#save(presData, file="presData.R")


#########
## Part - DATA MANIUPLATION OF SPECIES DETECTION DATA (NOT NEARMI DATA)
#########

## This section is static, as it organizes data from 2005-2013 and does not need to be updated on an annual basis.


##
#### Data Manipulation
##

#Visually inspect data
head(frogRaw_MANA)
head(frogRaw_ROCR)
str(frogRaw_MANA)
str(frogRaw_ROCR)

#Select columns of interest
dataMANA <- frogRaw_MANA[,c('PoolName','Date','Observer1','Observer2',
                            'X.RSYL','X.AMAC','X.AOPA','X.ALAT','X.RSYL_obs2','X.AMAC_obs2','X.AOPA_obs2','X.ALAT_obs2',
                            'X.RSYL.Larvae','X.AMAC.Larvae','X.AOPA.Larvae','X.ALAT.Larvae',
                            'X.RSYL.Larvae_obs2','X.AMAC.Larvae_obs2','X.AOPA.Larvae_obs2','X.ALAT.Larvae_obs2',
                            'Width','Length','Depth_m', 'Water.Present','COND'
)]

dataROCR <- frogRaw_ROCR[,c('PoolName','Date','Observer1','Observer2',
                            'X.RSYL','X.AMAC','X.AOPA','X.ALAT','X.RSYL_obs2','X.AMAC_obs2','X.AOPA_obs2','X.ALAT_obs2',
                            'X.RSYL.Larvae','X.AMAC.Larvae','X.AOPA.Larvae','X.ALAT.Larvae',
                            'X.RSYL.Larvae_obs2','X.AMAC.Larvae_obs2','X.AOPA.Larvae_obs2','X.ALAT.Larvae_obs2',
                            'Width','Length','Depth_m', 'Water.Present','COND'
)]

## Edit Site/Park names
#Park
dataMANA$Park <- 'MANA'
dataROCR$Park <- 'ROCR'
#Site & Site ID
## These were cross-referenced by other existing data sets to get correct name
#MANA
dataMANA$Site <- as.character(dataMANA$PoolName)
dataMANA$SiteID <- NA
dataMANA$SiteIDOrder <- NA
##
#### SiteIDOrder **
##
dataMANA[dataMANA$Site == 'MANABench Pool','SiteID'] <- 432
dataMANA[dataMANA$Site == 'MANABench Pool','SiteIDOrder'] <- 250
dataMANA[dataMANA$Site == 'MANABench Pool','Site'] <- "Bench Pool"
dataMANA[dataMANA$Site == 'MANABrawner','SiteID'] <- 435
dataMANA[dataMANA$Site == 'MANABrawner','SiteIDOrder'] <- 251
dataMANA[dataMANA$Site == 'MANABrawner','Site'] <- "Brawner Pool"
dataMANA[dataMANA$Site == 'MANACar Pool','SiteID'] <- 436
dataMANA[dataMANA$Site == 'MANACar Pool','SiteIDOrder'] <- 252
dataMANA[dataMANA$Site == 'MANACar Pool','Site'] <- "Car Pool"
dataMANA[dataMANA$Site == 'MANADeep Cut','SiteID'] <- 437
dataMANA[dataMANA$Site == 'MANADeep Cut','SiteIDOrder'] <- 253
dataMANA[dataMANA$Site == 'MANADeep Cut','Site'] <- "Deep Cut Pool"
dataMANA[dataMANA$Site == 'MANADuck Box 1','SiteID'] <- 438
dataMANA[dataMANA$Site == 'MANADuck Box 1','SiteIDOrder'] <- 254
dataMANA[dataMANA$Site == 'MANADuck Box 1','Site'] <- "Duck Box 1 Pool"
dataMANA[dataMANA$Site == 'MANAOxbow 1','SiteID'] <- 439
dataMANA[dataMANA$Site == 'MANAOxbow 1','SiteIDOrder'] <- 255
dataMANA[dataMANA$Site == 'MANAOxbow 1','Site'] <- "Oxbow 1 Pool"
dataMANA[dataMANA$Site == 'MANAOxbow 2','SiteID'] <- 440
dataMANA[dataMANA$Site == 'MANAOxbow 2','SiteIDOrder'] <- 256
dataMANA[dataMANA$Site == 'MANAOxbow 2','Site'] <- "Oxbow 2 Pool"
dataMANA[dataMANA$Site == 'MANAQuarry','SiteID'] <- 441
dataMANA[dataMANA$Site == 'MANAQuarry','SiteIDOrder'] <- 257
dataMANA[dataMANA$Site == 'MANAQuarry','Site'] <- "Quarry Pool"
dataMANA[dataMANA$Site == 'MANARust','SiteID'] <- 442
dataMANA[dataMANA$Site == 'MANARust','SiteIDOrder'] <- 258
dataMANA[dataMANA$Site == 'MANARust','Site'] <- "Rust Pool"
dataMANA$Park <- as.factor(dataMANA$Park)
dataMANA$Site <- as.factor(dataMANA$Site)
#ROCR
dataROCR$Site <- as.character(dataROCR$PoolName)
#dataROCR[dataROCR$Site == 'ROCRH','SiteID'] <- NA
#dataROCR[dataROCR$Site == 'ROCRH','Site'] <- "H Pool"
dataROCR <- dataROCR[!(dataROCR$Site == 'ROCRH'),] 
#Remove H pool above? ONly sampled twice in 2005
dataROCR[dataROCR$Site == 'ROCRMaintenance','SiteID'] <- 773
dataROCR[dataROCR$Site == 'ROCRMaintenance','SiteIDOrder'] <- 469
dataROCR[dataROCR$Site == 'ROCRMaintenance','Site'] <- "Maintenance Pool"
dataROCR[dataROCR$Site == 'ROCRMiddle','SiteID'] <- 774
dataROCR[dataROCR$Site == 'ROCRMiddle','SiteIDOrder'] <- 470
dataROCR[dataROCR$Site == 'ROCRMiddle','Site'] <- "Middle Pool"
dataROCR[dataROCR$Site == 'ROCROxbow','SiteID'] <- 775
dataROCR[dataROCR$Site == 'ROCROxbow','SiteIDOrder'] <- 471
dataROCR[dataROCR$Site == 'ROCROxbow','Site'] <- "Oxbow Pool"
dataROCR[dataROCR$Site == 'ROCRParkside','SiteID'] <- 776
dataROCR[dataROCR$Site == 'ROCRParkside','SiteIDOrder'] <- 472
dataROCR[dataROCR$Site == 'ROCRParkside','Site'] <- "Parkside Pool"
#dataROCR[dataROCR$Site == 'ROCRpool7','SiteID'] <- NA
#dataROCR[dataROCR$Site == 'ROCRpool7','Site'] <- "Pool 7"
dataROCR <- dataROCR[!(dataROCR$Site == 'ROCRpool7'),]
#Remove Pool 7 above? ONly sampled twice in 2005
dataROCR[dataROCR$Site == 'ROCRRileySpring','SiteID'] <- 777
dataROCR[dataROCR$Site == 'ROCRRileySpring','SiteIDOrder'] <- 473
dataROCR[dataROCR$Site == 'ROCRRileySpring','Site'] <- "Riley Spring Pool"
dataROCR[dataROCR$Site == 'ROCRSandbag','SiteID'] <- 778
dataROCR[dataROCR$Site == 'ROCRSandbag','SiteIDOrder'] <- 474
dataROCR[dataROCR$Site == 'ROCRSandbag','Site'] <- "Sandbag Pool"
dataROCR[dataROCR$Site == 'ROCRSkunkCabbage','SiteID'] <- 779
dataROCR[dataROCR$Site == 'ROCRSkunkCabbage','SiteIDOrder'] <- 475
dataROCR[dataROCR$Site == 'ROCRSkunkCabbage','Site'] <- "Skunk Cabbage Pool"
dataROCR[dataROCR$Site == 'ROCRTrail','SiteID'] <- 779
dataROCR[dataROCR$Site == 'ROCRTrail','SiteIDOrder'] <- 475
dataROCR[dataROCR$Site == 'ROCRTrail','Site'] <- "Skunk Cabbage Pool"
# This was not a coding error, apparently some sites at ROCR have had multiple names in past. ROCRTrail == ROCRSkunkCabbage
dataROCR[dataROCR$Site == 'ROCRWeirPond(back)','SiteID'] <- 780
dataROCR[dataROCR$Site == 'ROCRWeirPond(back)','SiteIDOrder'] <- 476
dataROCR[dataROCR$Site == 'ROCRWeirPond(back)','Site'] <- "Wier Pond"
dataROCR[dataROCR$Site == 'ROCRWestBeach','SiteID'] <- 781
dataROCR[dataROCR$Site == 'ROCRWestBeach','SiteIDOrder'] <- 477
dataROCR[dataROCR$Site == 'ROCRWestBeach','Site'] <- "West Beach Pool"
dataROCR$Park <- as.factor(dataROCR$Park)
dataROCR$Site <- as.factor(dataROCR$Site)



#Strip dates into seperate components
#MANA
dataMANA$Date_Formatted <- strptime(dataMANA$Date, format="%d-%b-%y")
#Extract Year & Add to 1900 BC the formatted year is relevant to 1900
dataMANA$Year <- dataMANA$Date_Formatted$year + 1900
#Extract Month & Add to 1 BC in this format month ranges from 0-11
dataMANA$Month <- dataMANA$Date_Formatted$mon + 1
#Extract Julian Date & Add to 1 BC in this format julian day includes 0
dataMANA$JulianDate <- dataMANA$Date_Formatted$yday + 1
#ROCR
dataROCR$Date_Formatted <- strptime(dataROCR$Date, format="%d-%b-%y")
#Extract Year & Add to 1900 BC the formatted year is relevant to 1900
dataROCR$Year <- dataROCR$Date_Formatted$year + 1900
#Extract Month & Add to 1 BC in this format month ranges from 0-11
dataROCR$Month <- dataROCR$Date_Formatted$mon + 1
#Extract Julian Date & Add to 1 BC in this format julian day includes 0
dataROCR$JulianDate <- dataROCR$Date_Formatted$yday + 1


## REMOVE VISITS WHEN WATER IS NOT PRESENT
#MANA
dataMANA <- dataMANA[dataMANA$Water.Present == TRUE,]
dataMANA[dataMANA$Water.Present == TRUE,'Water.Present'] <- 'Y'
#ROCR
dataROCR <- dataROCR[dataROCR$Water.Present == TRUE,]
dataROCR[dataROCR$Water.Present == TRUE,'Water.Present'] <- 'Y'

#Order by data and Add unique identifier for each survey
#MANA
dataMANA <- dataMANA[order(dataMANA$Site,dataMANA$Year, dataMANA$JulianDate),]
dataMANA$SurvID <- seq(1,nrow(dataMANA))
#ROCR
dataROCR <- dataROCR[order(dataROCR$Site,dataROCR$Year, dataROCR$JulianDate),]
dataROCR$SurvID <- seq(1,nrow(dataROCR))

#Extract Conductivity Data
head(dataROCR)
cond_ROCR <- dataROCR[,c('SiteID','SiteIDOrder','SurvID','COND')]
cond_MANA <- dataMANA[,c('SiteID','SiteIDOrder','SurvID','COND')]

#Create Visit # 
#MANA
dataMANA$Visit <- NA
dataMANA$Visit[1] <- 1
for(i in 2:nrow(dataMANA)){
  if(dataMANA$Site[i] == dataMANA$Site[i-1] && dataMANA$Year[i] == dataMANA$Year[i-1]){
    dataMANA$Visit[i] <- dataMANA$Visit[i-1] + 1
  }else{
    dataMANA$Visit[i] <- 1
  }
}
#ROCR
dataROCR$Visit <- NA
dataROCR$Visit[1] <- 1
for(i in 2:nrow(dataROCR)){
  if(dataROCR$Site[i] == dataROCR$Site[i-1] && dataROCR$Year[i] == dataROCR$Year[i-1]){
    dataROCR$Visit[i] <- dataROCR$Visit[i-1] + 1
  }else{
    dataROCR$Visit[i] <- 1
  }
}


#Not all visits had two observers, so need to account for that
#Calculate # of obs per visit, duplicate rows accordingly, assign observer code, assign count to correct observer
#MANA
dataMANA$Obss <- 2
dataMANA[dataMANA$Observer2 == "",'Obss'] <- 1
dataMANA <- dataMANA[rep(row.names(dataMANA), dataMANA$Obss),]
dataMANA <- dataMANA[order(dataMANA$SurvID),]
dataMANA$Obs <- 1
for(i in 2:nrow(dataMANA)){
  if(dataMANA$SurvID[i] == dataMANA$SurvID[i-1]){
    dataMANA$Obs[i] <- 2
  }else{
    dataMANA$Obs[i] <- 1
  }
}
dataMANA$RSYL_A <- NA
dataMANA$RSYL_L <- NA
dataMANA$AMAC_A <- NA
dataMANA$AMAC_L <- NA
dataMANA$AOPA_A <- NA
dataMANA$AOPA_L <- NA
dataMANA$ALAT_A <- NA
dataMANA$ALAT_L <- NA
for(i in 1: nrow(dataMANA)){
  if(dataMANA$Obs[i] == 1){
    dataMANA$RSYL_A[i] <- dataMANA$X.RSYL[i]
    dataMANA$RSYL_L[i] <- dataMANA$X.RSYL.Larvae[i]
    dataMANA$AMAC_A[i] <- dataMANA$X.AMAC[i]
    dataMANA$AMAC_L[i] <- dataMANA$X.AMAC.Larvae[i]
    dataMANA$AOPA_A[i] <- dataMANA$X.AOPA[i] 
    dataMANA$AOPA_L[i] <- dataMANA$X.AOPA.Larvae[i]
    dataMANA$ALAT_A[i] <- dataMANA$X.ALAT[i]
    dataMANA$ALAT_L[i] <- dataMANA$X.ALAT.Larvae[i]
  }else{
    dataMANA$RSYL_A[i] <- dataMANA$X.RSYL_obs2[i]
    dataMANA$RSYL_L[i] <- dataMANA$X.RSYL.Larvae_obs2[i] 
    dataMANA$AMAC_A[i] <- dataMANA$X.AMAC_obs2[i]
    dataMANA$AMAC_L[i] <- dataMANA$X.AMAC.Larvae_obs2[i]
    dataMANA$AOPA_A[i] <- dataMANA$X.AOPA_obs2[i] 
    dataMANA$AOPA_L[i] <- dataMANA$X.AOPA.Larvae_obs2[i]
    dataMANA$ALAT_A[i] <- dataMANA$X.ALAT_obs2[i]
    dataMANA$ALAT_L[i] <- dataMANA$X.ALAT.Larvae_obs2[i]
  }
}
#Reorganize Data Frame
dataMANA <- dataMANA[,c('Park','Site','SiteID','SiteIDOrder','SurvID', 'Year','Month','JulianDate','Visit','Obss','Obs','RSYL_A','RSYL_L','AMAC_A','AMAC_L','AOPA_A','AOPA_L','ALAT_A','ALAT_L','Width','Length','Depth_m','Water.Present','COND')]
#ROCR
#Something to note for ROCR is that there are some instances when no observers were listed at all (But had observations for the first observer). There were never any instances when
#An observer 2 was not recorded and there were observations listed.
dataROCR$Obss <- 2
dataROCR[dataROCR$Observer2 == "",'Obss'] <- 1
dataROCR <- dataROCR[rep(row.names(dataROCR), dataROCR$Obss),]
dataROCR <- dataROCR[order(dataROCR$SurvID),]
dataROCR$Obs <- 1
for(i in 2:nrow(dataROCR)){
  if(dataROCR$SurvID[i] == dataROCR$SurvID[i-1]){
    dataROCR$Obs[i] <- 2
  }else{
    dataROCR$Obs[i] <- 1
  }
}
dataROCR$RSYL_A <- NA
dataROCR$RSYL_L <- NA
dataROCR$AMAC_A <- NA
dataROCR$AMAC_L <- NA
dataROCR$AOPA_A <- NA
dataROCR$AOPA_L <- NA
dataROCR$ALAT_A <- NA
dataROCR$ALAT_L <- NA
for(i in 1: nrow(dataROCR)){
  if(dataROCR$Obs[i] == 1){
    dataROCR$RSYL_A[i] <- dataROCR$X.RSYL[i]
    dataROCR$RSYL_L[i] <- dataROCR$X.RSYL.Larvae[i]
    dataROCR$AMAC_A[i] <- dataROCR$X.AMAC[i]
    dataROCR$AMAC_L[i] <- dataROCR$X.AMAC.Larvae[i]
    dataROCR$AOPA_A[i] <- dataROCR$X.AOPA[i] 
    dataROCR$AOPA_L[i] <- dataROCR$X.AOPA.Larvae[i]
    dataROCR$ALAT_A[i] <- dataROCR$X.ALAT[i]
    dataROCR$ALAT_L[i] <- dataROCR$X.ALAT.Larvae[i]
  }else{
    dataROCR$RSYL_A[i] <- dataROCR$X.RSYL_obs2[i]
    dataROCR$RSYL_L[i] <- dataROCR$X.RSYL.Larvae_obs2[i] 
    dataROCR$AMAC_A[i] <- dataROCR$X.AMAC_obs2[i]
    dataROCR$AMAC_L[i] <- dataROCR$X.AMAC.Larvae_obs2[i]
    dataROCR$AOPA_A[i] <- dataROCR$X.AOPA_obs2[i] 
    dataROCR$AOPA_L[i] <- dataROCR$X.AOPA.Larvae_obs2[i]
    dataROCR$ALAT_A[i] <- dataROCR$X.ALAT_obs2[i]
    dataROCR$ALAT_L[i] <- dataROCR$X.ALAT.Larvae_obs2[i]
  }
}
#Reorganize Data Frame
dataROCR <- dataROCR[,c('Park','Site','SiteID','SiteIDOrder','SurvID', 'Year','Month','JulianDate','Visit','Obss','Obs','RSYL_A','RSYL_L','AMAC_A','AMAC_L','AOPA_A','AOPA_L','ALAT_A','ALAT_L','Width','Length','Depth_m','Water.Present','COND')]


#Create Visit # for each observer (VISxOBS) bc each are treated as separate replicate 
#MANA
dataMANA$VISxOBS <- NA
dataMANA$VISxOBS[1] <- 1
for(i in 2:nrow(dataMANA)){
  if(dataMANA$Site[i] == dataMANA$Site[i-1] && dataMANA$Year[i] == dataMANA$Year[i-1]){
    dataMANA$VISxOBS[i] <- dataMANA$VISxOBS[i-1] + 1
  }else{
    dataMANA$VISxOBS[i] <- 1
  }
}
#ROCR
dataROCR$VISxOBS  <- NA
dataROCR$VISxOBS [1] <- 1
for(i in 2:nrow(dataROCR)){
  if(dataROCR$Site[i] == dataROCR$Site[i-1] && dataROCR$Year[i] == dataROCR$Year[i-1]){
    dataROCR$VISxOBS [i] <- dataROCR$VISxOBS [i-1] + 1
  }else{
    dataROCR$VISxOBS [i] <- 1
  }
}


#Create sepearate rows for each species
#MANA
dataMANA <- dataMANA[rep(row.names(dataMANA), each=4),]
dataMANA$Spp <- rep(c('RSYL','AMAC','AOPA','ALAT'), nrow(dataMANA)/4)
dataMANA$Count_A <- NA
dataMANA$Count_L <- NA
for(i in 1:nrow(dataMANA)){
  if(dataMANA$Spp[i] == "RSYL"){
    dataMANA$Count_A[i] <- dataMANA$RSYL_A[i]
    dataMANA$Count_L[i] <- dataMANA$RSYL_L[i]
  }else
    if(dataMANA$Spp[i] == "AMAC"){
      dataMANA$Count_A[i] <- dataMANA$AMAC_A[i]
      dataMANA$Count_L[i] <- dataMANA$AMAC_L[i]
    }else
      if(dataMANA$Spp[i] == "AOPA"){
        dataMANA$Count_A[i] <- dataMANA$AOPA_A[i]
        dataMANA$Count_L[i] <- dataMANA$AOPA_L[i]
      }else{
        dataMANA$Count_A[i] <- dataMANA$ALAT_A[i]
        dataMANA$Count_L[i] <- dataMANA$ALAT_L[i]
      }
}
dataMANA <- dataMANA[,c('Park','Site','SiteID','SiteIDOrder','SurvID','Year','Month','JulianDate','Visit','Obss','Obs','VISxOBS','Spp','Count_A','Count_L','Width','Length','Depth_m','Water.Present','COND')]
#ROCR
dataROCR <- dataROCR[rep(row.names(dataROCR), each=4),]
dataROCR$Spp <- rep(c('RSYL','AMAC','AOPA','ALAT'), nrow(dataROCR)/4)
dataROCR$Count_A <- NA
dataROCR$Count_L <- NA
for(i in 1:nrow(dataROCR)){
  if(dataROCR$Spp[i] == "RSYL"){
    dataROCR$Count_A[i] <- dataROCR$RSYL_A[i]
    dataROCR$Count_L[i] <- dataROCR$RSYL_L[i]
  }else
    if(dataROCR$Spp[i] == "AMAC"){
      dataROCR$Count_A[i] <- dataROCR$AMAC_A[i]
      dataROCR$Count_L[i] <- dataROCR$AMAC_L[i]
    }else
      if(dataROCR$Spp[i] == "AOPA"){
        dataROCR$Count_A[i] <- dataROCR$AOPA_A[i]
        dataROCR$Count_L[i] <- dataROCR$AOPA_L[i]
      }else{
        dataROCR$Count_A[i] <- dataROCR$ALAT_A[i]
        dataROCR$Count_L[i] <- dataROCR$ALAT_L[i]
      }
}
dataROCR <- dataROCR[,c('Park','Site','SiteID','SiteIDOrder','SurvID','Year','Month','JulianDate','Visit','Obss','Obs','VISxOBS','Spp','Count_A','Count_L','Width','Length','Depth_m','Water.Present','COND')]


#Combine larval and adults and convert from abundance to detection data (1 or 0)
#MANA
dataMANA$Count_A[dataMANA$Count_A == -99 | is.na(dataMANA$Count_A)] <- 0
dataMANA$Count_L[dataMANA$Count_L == -99 | is.na(dataMANA$Count_L)] <- 0
dataMANA$Count <- dataMANA$Count_A + dataMANA$Count_L
dataMANA$Occ <- ifelse(dataMANA$Count > 0, 1, 0)
dataMANA <- dataMANA[,c('Park','Site','SiteID','SiteIDOrder','SurvID','Year','Month','JulianDate','Visit','Obss','Obs','VISxOBS','Spp','Occ','Width','Length','Depth_m','Water.Present','COND')]
#ROCR
dataROCR$Count_A[dataROCR$Count_A == -99 | is.na(dataROCR$Count_A)] <- 0
dataROCR$Count_L[dataROCR$Count_L == -99 | is.na(dataROCR$Count_L)] <- 0
dataROCR$Count <- dataROCR$Count_A + dataROCR$Count_L
dataROCR$Occ <- ifelse(dataROCR$Count > 0, 1, 0)
dataROCR <- dataROCR[,c('Park','Site','SiteID','SiteIDOrder','SurvID','Year','Month','JulianDate','Visit','Obss','Obs','VISxOBS','Spp','Occ','Width','Length','Depth_m','Water.Present','COND')]


#########
## Part - MERGE NEARMI AND NPS DATA
#########

#Reorganze to match presData
#Add column for nObs to presDAta
head(presData)
presData$nObs <- 2
presData <- presData %>% select(Park, Site, SiteID, SiteIDOrder, Year, Month, JDay, SurvID, Visit, nObs, Obs, VISxOBS, Spp, Occ, Water, MaxL, MaxW, MaxD, Cond)
#MANA
head(dataMANA)
presMANA <- dataMANA %>% select(Park, Site, SiteID, SiteIDOrder, Year, Month, JulianDate, SurvID, Visit, Obss, Obs, VISxOBS, Spp, Occ, Water.Present, Length, Width, Depth_m, COND)
colnames(presMANA) <- colnames(presData)
rownames(presMANA) <- 1:nrow(presMANA)
head(presMANA)
#ROCR
head(dataROCR)
presROCR <- dataROCR %>% select(Park, Site, SiteID, SiteIDOrder, Year, Month, JulianDate, SurvID, Visit, Obss, Obs, VISxOBS, Spp, Occ, Water.Present, Length, Width, Depth_m, COND)
colnames(presROCR) <- colnames(presData)
rownames(presROCR) <- 1:nrow(presROCR)
head(presROCR)

#Reorganize to match surData
head(surData)
#Add column for nObs to surData
surData$nObs <- 2
surData <- surData %>% select(Park, Site, SiteID, SiteIDOrder, Year, Month, JDay, SurvID, Visit, nObs, Water, MaxL, MaxW, MaxD, Cond)
#MANA
surMANA <- presMANA[!duplicated(presMANA$SurvID),c(1:10,15:19)]
#ROCR
surROCR <- presROCR[!duplicated(presROCR$SurvID),c(1:10,15:19)]

#Get rid of 0's in presROCR & presMANA
#MANA 
presMANA <- presMANA %>% filter(Occ == 1)
#ROCR
presROCR <- presROCR %>% filter(Occ == 1)


##
#### Merge w/ NEARMI Data
##

#Merge with presData
presData <- rbind(presData,presMANA,presROCR)

#Merge with surData
surData <- rbind(surData,surMANA,surROCR)
surData <- surData %>% arrange(SiteID, Year, JDay)


#########
## Part - RESHAPING DETECTION DATA FOR JAGS MODEL ANALYSIS 
#########

##
#### Need to account for visits in which there were not detected species (true 0's), bc presData only has prescene info
##

## Need to do to be able to discern btn true 0's and NA's when we create the detection histories in steps below

#First, determine # of reps of site for each year 
  #NOW THAT ROCR AND MANA DATA ARE ADDED, YOU'LL NEED TO MULTIPLE BY 'Obss' INSTEAD OF @. BC NOT ALL VISITS HAVE 2 OBSERVERS. But this won't work with tapply, bc it multiplies the max, 
  #Create a new object, replicate surData by nObs, then create new dummy variable just like VISxOBS and then take the max of that
surData_Visits <- surData[rep(row.names(surData),surData$nObs),]
surData_Visits$rep <- NA
surData_Visits$rep[1] <- 1
for(i in 2:nrow(surData_Visits)){
  if(surData_Visits$SiteID[i] == surData_Visits$SiteID[i-1] && surData_Visits$Year[i] == surData_Visits$Year[i-1]){
    surData_Visits$rep[i] <- surData_Visits$rep[i-1] + 1
  }else{
    surData_Visits$rep[i] <- 1
  }
}
#no.VisxSitexYear <- tapply(surData_Visits$rep,list(surData_Visits$SiteID,surData_Visits$Year),max)
no.VisxSitexYear <- tapply(surData_Visits$rep,list(surData_Visits$SiteIDOrder,surData_Visits$Year),max)
# Write CSV of effort to Andrew to determine sampling for 2017
effortYears <- no.VisxSitexYear
effortYears[is.na(effortYears)] <- 0
## Test random site make sure that this worked
testX <- as.integer(runif(1,1,297))
effortYears[testX,]
surData %>% filter(SiteIDOrder == rownames(effortYears)[testX])
#write.csv(effortYears, "effortYears.csv")

##Also, need to account for sites that captured no species at all
#Need to account for sites surveyed that had no captures
#Subset rows from surData not in presData
missingData <- surData %>% filter(!surData$SiteID %in% presData$SiteID)
#Create Dummy columns to match presData
missingData$VISxOBS <- 1
missingData$Occ <- 0 
missingData$Obs <- 1
missingData$Spp <- "ACRE" #Picked a random species, doesn't really matter
#Reorder to match presData
missingData <- missingData %>% select(colnames(presData))
#Remove duplicate rows, only need the Site to be listed once
missingData <- missingData %>% distinct(SiteID, .keep_all = T)
#Now combine with presData
presData <- bind_rows(presData, missingData)

##
#### Summary Statistics of data
##


#How many species?
presData$Spp <- as.factor(presData$Spp)
statData <- presData
statData <- statData[statData$Occ == 1,]
statData$Spp <- droplevels(statData$Spp)
uspecies <- unique(statData$Spp)
nSpp <- length(uspecies)

#How many observations and species per park?
table(statData$Spp,statData$Park)
table(statData$Park,statData$Spp)
nObsPark <- tapply(statData$Occ,statData$Park,sum)
## For this year
statData_2017 <- statData[statData$Year == 2017,]
#t <- table(statData_2017$Spp,statData_2017$Park)
#write.table(t,file="obs-by-park.txt")

#How many counts per species?
countSpp <- tapply(statData$Occ, statData$Spp,sum)

#How many parks?
uPark <- unique(statData$Park)
nPark <- length(uPark)

#How many sampling sites? 
nSite <- length(unique(surData$SiteID))
#Make sure you go by SiteID and not Site bc some wetlands at different parks have same name
#Find out which ones with the code below
# look <- surData[,1:3]
# looka <- look[!duplicated(look),]
# lookc <- looka[order(looka$Site),]
# lookb <- looka[duplicated(looka[,2]),]

#How many sampling occassions?
dim(surData)
head(surData)


##
#### Reshaping data for JAGS
##

#Reshape the presence data into a four dimenionsal array (j=SiteID, k=Visit, t=Year, i=Spp, r=Park)
junk.melt <- melt(presData, id.var=c("SiteIDOrder", "VISxOBS","Year", "Spp", "Park"),measure.var="Occ")
detData <- cast(junk.melt, SiteIDOrder ~ VISxOBS ~ Year ~ Spp ~ Park)


##
#### Data augmentation - Create all zero encounter histories to add to the detection array to the 4th DIM (Spp)
##

# Create augmented version of detData which incorporates the 50 additional species
# Number of zero encounter histories to be added
nZeroes <- 15
# Create augmented version of detData which incorporates the 50 additional species
detAug <- array(0, dim=c(dim(detData)[1],dim(detData)[2],dim(detData)[3],dim(detData)[4]+nZeroes,dim(detData)[5]))
#Now replace original species in detAug with the data for these species in detData
detAug[,,,1:dim(detData)[4],] <- detData


# Need to define sites in array that align with region (minJ & maxJ). This determines the looping structure within the JAGS model
  #Order sites in the order they are in the array
siteDummy <- surData %>% arrange(SiteIDOrder) %>% distinct(SiteIDOrder, .keep_all=T) 
  #Create sequential dummy variable
siteDummy$DummyID <- seq(1,nrow(siteDummy))
  #Create minJ and MaxJ (the first time and last time a site would appear in the detData array) - this determines the looping structure of the JAGS model
    #This is why we originall created SiteIDOrder, bc SiteID is not in sequential order by Park (it's ordered by when it was first entered in the database)
minJ <- siteDummy %>% group_by(Park) %>% arrange(SiteIDOrder) %>% slice(1) %>% ungroup() %>% pull(DummyID)
maxJ <- siteDummy %>% group_by(Park) %>% arrange(SiteIDOrder) %>% slice(n()) %>% ungroup() %>% pull(DummyID)

# Need to define first year that each site was surveyed
  # Using for loop and effortYears
minYsite <- NA
for (i in 1:dim(effortYears)[1]){
  minYsite[i] <- min(which(effortYears[i,]!= 0))
}
  # Need to define first year that each site was surveyed
minY <- rep(NA, dim(detData)[5])
for(i in 1:dim(detData)[5]){
  minY[i] <- min(minYsite[minJ[i]:maxJ[i]])
}

#Using objects created from steps above to discern btw true 0's and true NA's
  #Convert all NA's to 0
detAug[is.na(detAug[,,,,])] <- 0
  #Now convert sites of parks that were not sampled (e.g. CATO sites should be NA in CHOH dimension)
for(i in 1:dim(detAug)[5]){
  for(j in 1:dim(detAug)[1]){
    if(j < minJ[i] || j > maxJ[i]){
      detAug[j,,,,i] <- NA
    }}}

#Now convert species of ROCR to MANA not sampled to NA
    #The only species surveyed are AMAC,AOPA, and RSYL, need to ignore the rest
sppOmit <- data.frame(Name = levels(uspecies), Rank = 1:nSpp) %>% filter(!Name %in% c('AMAC','AOPA','RSYL')) %>% pull(Rank)
  #MANA
detAug[,,1:8,sppOmit,which(levels(uPark) == 'MANA')] <- NA
  #ROCR
detAug[,,1:11,sppOmit,which(levels(uPark) == 'ROCR')] <- NA

#Assigning NA's to vists of sites when not surveyed
for(r in 1:dim(detAug)[5]){
  for(i in 1:dim(detAug)[4]){
    for(y in 1:dim(detAug)[3]){
      for(j in minJ[r]:maxJ[r]){
        for(k in 1:dim(detAug)[2]){
          if(is.na(k <= no.VisxSitexYear[j,y])){
            detAug[j,k,y,i,r] <- NA
          }else{
          if(k <= no.VisxSitexYear[j,y]){
            detAug[j,k,y,i,r] <- detAug[j,k,y,i,r]
          }else{
            detAug[j,k,y,i,r] <- NA
          }
          }
        }
      }
    }
  }
}

#To verify that it worked, double check some data
no.VisxSitexYear[minJ[2]:maxJ[2],1]
detAug[minJ[2]:(minJ[2]+30),,1,12,2]
presData[presData$Spp == "RPAL" & presData$Park == "CHOH" & presData$Year == 2005,]
  #Yes

presData[presData$Spp == "AMAC" & presData$Park == "ROCR" & presData$Year == 2005,]
detAug[minJ[9]:maxJ[9],,1,2,9]
no.VisxSitexYear[minJ[9]:maxJ[9],1]
  #Yes
#Should be all NA
detAug[,,1,1,9]
  #Yes

#Did it work?
dim(detData)
detData[minJ[2]:maxJ[2],,1,2,2]
detAug[minJ[2]:maxJ[2],,1,2,2]
presData[presData$Spp == "AMAC" & presData$Park == "CHOH" & presData$Year == "2005",]
#Yes

##
#### Save formatted detection array
##

#NOT SAVED YET, WILL VERIFY WHEN IT IS SAVED
#save(detData, file="detData.R")
#save(detAug, file="detAug.R")

#########
## Part - CREATING COVARIATE DATA FOR JAGS ANALYSIS
#########


##
#### Hydroperiod (Occupancy)
##


wetSites <- siteDummy
head(permData)
head(wetSites)
  #Need to match columns based on SiteID (replace SiteRecID in permData with SiteID)
colnames(permData)[1] <- 'SiteID'
permData <- permData %>% select(SiteID,Permanency,Lat,Long)
  #Merge datasets
wetPerm <- merge(wetSites, permData, all.x=T, by='SiteID')
head(wetPerm)
  #How many sites are missing this Permancency classification from original file?
wetPerm %>% filter(Permanency == 'U')
  #Change to numeric
wetPerm$HydroState <- 0
    # Temporary = 1
    # Semi-perm = 2
    # Permanent = 3
    # Unknown   = 1, If Unkown, categorize at Temporary
wetPerm$HydroState <- plyr::mapvalues(wetPerm$Permanency, 
                            from = c('T','S','P','U'), 
                            to = c(1,2,3,1),
                            warn_missing = F)
  #Drop factor levels
wetPerm$HydroState <- as.numeric(as.character(wetPerm$HydroState))
  #Order by SiteIDOrder
wetPerm <- wetPerm %>% arrange(SiteIDOrder)
  #Convert to a matrix
Hydro_state <- matrix(0, nrow=nrow(wetPerm), ncol=3)
for(i in 1:nrow(Hydro_state)){
  Hydro_state[i,wetPerm[i,'HydroState']] <- 1
}


##
#### JDay (Detection)
##

#Reshape Julian Date into a three dimenionsal array (j=SiteID, k=Visit)
surExpand <- surData_Visits
junk.melt <- melt(surExpand, id.var=c("SiteIDOrder", "rep", "Year"),measure.var="JDay")
cov_JDay <- cast(junk.melt, SiteIDOrder ~ rep ~ Year)

#All years standardized around 0 
#Julian date
JDay <- cov_JDay

#standardize covariate
min(JDay,na.rm=T)
max(JDay,na.rm=T)
JDayMean <- mean(JDay, na.rm=T)
JDaySD <- sd(JDay, na.rm=T)
JDayS <- (JDay-JDayMean)/JDaySD
plot(JDayS,JDayS)
mean(JDayS,na.rm=T)
sd(JDayS,na.rm=T)
min(JDayS,na.rm=T)
max(JDayS,na.rm=T)


##
#### MaxArea (Occupancy) 
##

  #Replace -99.00 (missing data) with NA.
surExpand <- surExpand %>% mutate(MaxL = ifelse(MaxL == -99.00, NA, MaxL))
surExpand <- surExpand %>% mutate(MaxW = ifelse(MaxW == -99.00, NA, MaxW))
  #Calculate area
surExpand <- surExpand %>% mutate(MaxArea = MaxL*MaxW)


  #Reshape MaxArea into a vector (Min)
junk.melt <- melt(surExpand, id.var=c("SiteIDOrder", "rep", "Year"),measure.var="MaxArea")
cov_MaxArea <- cast(junk.melt, SiteIDOrder ~ rep ~ Year)

  #Estimate the minimum area across sampling occassions within each year
cov_MaxArea[cov_MaxArea == 0] <- NA
cov_MaxArea[is.na(cov_MaxArea)] <- 100000
Area <- apply(cov_MaxArea,c(1,3),min)
Area[Area == 100000] <- NA
#To verify it's working...
head(Area)
head(effortYears)

  #standardize covariate
min(Area,na.rm=T)
max(Area,na.rm=T)
AreaMean <- mean(Area, na.rm=T)
AreaSD <- sd(Area, na.rm=T)
Site_area_S <- (Area-AreaMean)/AreaSD
Site_area <- Area
plot(Site_area_S,Site_area_S)
min(Site_area_S,na.rm=T)
max(Site_area_S,na.rm=T)
mean(Site_area_S,na.rm=T)
sd(Site_area_S,na.rm=T)


##
#### Conductivity
##

  #Replace -99.00 (missing data) with NA
surExpand <- surExpand %>% mutate(Cond = ifelse(Cond == -99.00, NA, Cond))


#Reshape conductivity into a three dimenionsal array (j=SiteID, k=Visit)
junk.melt <- melt(surExpand, id.var=c("SiteIDOrder", "rep", "Year"),measure.var="Cond")
cov_Cond <- cast(junk.melt, SiteIDOrder ~ rep ~ Year)

#Take mean conductivity measurement for year
Cond <- apply(cov_Cond,c(1,3),max,na.rm=T)
Cond[Cond == '-Inf'] <- NA

#standardize covariate
min(Cond,na.rm=T)
max(Cond,na.rm=T)
CondMean <- mean(Cond, na.rm=T)
CondSD <- sd(Cond, na.rm=T)
CondS <- (Cond-CondMean)/CondSD
plot(CondS,CondS)
min(CondS,na.rm=T)
max(CondS,na.rm=T)
mean(CondS,na.rm=T)
sd(CondS,na.rm=T)


##
#### Park_area meters squared (Park-level Richness) 
##

#From Greg (GIS specialist at NCRN)
              #CATO CHOH GWMP HAFE MANA MONO NACE PRWI ROCR
Park_area <- c(23309492,84344501,21519543,14978494,20546224,6563586,45999348,50751086,11786628)

#standardize 
min(Park_area,na.rm=T)
max(Park_area,na.rm=T)
Park_areaMean <- mean(Park_area, na.rm=T)
Park_areaSD <- sd(Park_area, na.rm=T)
Park_area_S <- (Park_area-Park_areaMean)/Park_areaSD
plot(Park_area_S,Park_area_S)
min(Park_area_S,na.rm=T)
max(Park_area_S,na.rm=T)
mean(Park_area_S,na.rm=T)
sd(Park_area_S,na.rm=T)


##
#### Isolation (Park-level Richness) 
##

#From Greg (GIS specialist at NCRN)
          #CATO    CHOH GWMP HAFE MANA MONO NACE PRWI ROCR
urban <- c(14.48,15.28,50.66,18.37,36.12,32.43,66.63,33.43,93.44)
water <- c(00.70,11.13,25.38,05.82,00.14,03.26,08.08,00.77,00.00)
isol <- urban+water
Isol <- isol/100

#standardize 
min(Isol,na.rm=T)
max(Isol,na.rm=T)
IsolMean <- mean(Isol, na.rm=T)
IsolSD <- sd(Isol, na.rm=T)
IsolS <- (Isol-IsolMean)/IsolSD
plot(IsolS,IsolS)
min(IsolS,na.rm=T)
max(IsolS,na.rm=T)
mean(IsolS,na.rm=T)
sd(IsolS,na.rm=T)


##
#### Percent Forest-cover (Park-level Richness) 
##

#From Greg (GIS specialist at NCRN)
              #CATO CHOH GWMP HAFE MANA MONO NACE PRWI ROCR
forest_cover <- c(95,67,43,68,35,19,43,92,63)
Forest <- forest_cover/100

#standardize 
min(Forest,na.rm=T)
max(Forest,na.rm=T)
ForestMean <- mean(Forest, na.rm=T)
ForestSD <- sd(Forest, na.rm=T)
ForestS <- (Forest-ForestMean)/ForestSD
plot(ForestS,ForestS)
min(ForestS,na.rm=T)
max(ForestS,na.rm=T)
mean(ForestS,na.rm=T)
sd(ForestS,na.rm=T)


#########
## Part - JAGS ANALYSIS DATA
#########

  #Detection Data
X <- detAug
  #Covariate Data (Occupancy, Detection)
Precip <- precip
Hydro_state <- Hydro_state
JDay <- JDayS
Site_area <- Site_area_S
Cond <- CondS
Conn <- Conn
  #Coviariate Data (Park)
Park_area <- Park_area_S 
Isol <- IsolS
Forest_cov <- ForestS
  #Number of sites
J <- dim(detAug)[1]
  #Vectors containing first and last (j) of each region
minJ <- minJ
maxJ <- maxJ
  #Matrix of number of replicates per site by year
effortYears[effortYears == 0] <- 1
K <- effortYears
  #Number of Years
Y <- dim(detAug)[3]
minY <- minY
minY[5] <- 11 #The previous years at MANA were exlcuded bc the selections of those sites was not probabilisitc
  #Number of detected spp
I <- dim(detData)[4]
  #Number of added species
M <- nZeroes
  #Number of regions
R <- dim(detAug)[5]

#Save all necessary data objects for analysis
savedOn <- Sys.Date() 
fileName <- paste("data_for_MRCM-analysis_",savedOn,".R", sep = "") 
save(X, Hydro_state, JDay, JDayS, Site_area, Site_area_S, Cond, CondS, Conn, ConnS, precip, Park_area, Park_area_S,
     Isol, IsolS, Forest, ForestS, J, minJ, maxJ, K, Y, minY, I, M, R, file = fileName)



