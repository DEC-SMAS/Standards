# Author: Charles Stoll
# Date: 2018.01.08
# 
# Script designed to identify water quality exceedances 
# by comparing water chemistry result values to NYS WQS standard values
#
# NOTE: When incorporating field measured values into chemstriy table (e.g. Cond, DO, PH, % Sat, Salinty, Water temperature)
# Make sure to fill in the sys_sample_code, fraction, result values/unit fields, sample_date, detect_flag, cas_rn, lab_sdg/sample_delivery_group, 
# and sample_matrix fields, otherwise the script will remove these records from the table
#

rm(list=ls())
gc()
options(scipen=999)

################################################################
# Set primary variables for script
################################################################
# Import libraries
library(plyr)
library(readxl)
library(reshape2)
library(stringr)

# Primary directory
main_directory <- file.path("C:",
                            "NYBackup",
                            "Standards")

# Folder where input data will be taken from
inputdata_Location <- file.path(main_directory, 
                                "Input")

# Folder where output files will be placed
outputdata_Location <- file.path(main_directory, 
                                 "Output") 
  
# Input data tables
## Site location file w/water classifications
site_cls <- c(RIVMILE="character")
site_info <- read.csv(file=file.path(inputdata_Location,
                                     "Stream_Bio_Unit_Visited_Sites_2017-2018_List.csv"),
                      header = TRUE, 
                      sep = ",", 
                      colClasses=site_cls, 
                      stringsAsFactors = FALSE)

## Chemistry results table
chem_cls = c(RIVMILE="character")#, sample_date="character")
chem_data <- read.csv(file=file.path(inputdata_Location,
                                     "2018_Ramapo_CSedits_f_SWEVI_2.csv"),
                      header = TRUE, 
                      sep = ",", 
                      colClasses=chem_cls, 
                      stringsAsFactors = FALSE, 
                      as.is = TRUE)
                            

## Reformat date field
chem_data$sample_date<-as.character(as.POSIXct(chem_data$sample_date, 
                                               format='%m/%d/%Y'))
# Standards tables
##---------------------------------------------------------------------------------##
# These tables are NOT comprehensive and require the ability to make routine edits
##---------------------------------------------------------------------------------##

## Standards table directory
Standards_Table_Folder_location <- file.path(main_directory,
                                             "Parameter_Standards_Tables")

## Class A water quality standards table 
standardsA <- read.csv(file=file.path(Standards_Table_Folder_location,
                                      "ClassA_Standards.csv"), 
                       header = TRUE, 
                       sep = ",",
                       stringsAsFactors = FALSE, 
                       as.is = TRUE)
standardsA$Aquatic.Chronic. <- as.character(standardsA$Aquatic.Chronic.)
standardsA$Aquatic..Acute. <- as.character(standardsA$Aquatic..Acute.)

## Class B water quality standards table
standardsB <- read.csv(file=file.path(Standards_Table_Folder_location,
                                      "ClassB_Standards.csv"), 
                       header = TRUE, 
                       sep = ",",
                       stringsAsFactors = FALSE, 
                       as.is = TRUE)
standardsB$Aquatic.Chronic. <- as.character(standardsB$Aquatic.Chronic.)
standardsB$Aquatic..Acute. <- as.character(standardsB$Aquatic..Acute.)

## Class C water quality standards table
standardsC <- read.csv(file=file.path(Standards_Table_Folder_location,
                                      "ClassC_Standards.csv"),
                       header = TRUE, 
                       sep = ",",
                       stringsAsFactors = FALSE, 
                       as.is = TRUE)  
standardsC$Aquatic.Chronic. <- as.character(standardsC$Aquatic.Chronic.)
standardsC$Aquatic..Acute. <- as.character(standardsC$Aquatic..Acute.)

## Class D water quality standards table
standardsD <- read.csv(file=file.path(Standards_Table_Folder_location,
                                      "ClassD_Standards.csv"), 
                       header = TRUE, 
                       sep = ",",
                       stringsAsFactors = FALSE, 
                       as.is = TRUE)
standardsD$Aquatic.Chronic. <- as.character(standardsD$Aquatic.Chronic.)
standardsD$Aquatic..Acute. <- as.character(standardsD$Aquatic..Acute.)

##< Table of dissolved oxygen standards
diss_oxygen <- read.csv(file=file.path(Standards_Table_Folder_location,
                                       "Dissolved_Oxygen_Standards.csv"), 
                        header = TRUE, 
                        sep = ",",
                        stringsAsFactors = FALSE, 
                        as.is = TRUE)

##< Table of dissolved solids standards
diss_solids<- read.csv(file=file.path(Standards_Table_Folder_location,
                                      "Dissolved_Solids_Standards.csv"), 
                       header = TRUE, 
                       sep = ",",
                       stringsAsFactors = FALSE, 
                       as.is = TRUE) 

## Table of aquatic chronic ammonia standards for class A,B,C waterbodies w/Trout or Trout Spawning classification
abc_T_TS_ammonia <- read.csv(file=file.path(Standards_Table_Folder_location,
                                            "Ammonia",
                                            "ClassABC_Ammonia_AquatChron_W_T_TS_specification.csv"), 
                             header = TRUE, 
                             sep = ",",
                             stringsAsFactors = FALSE, 
                             as.is = TRUE)

## Table of aquatic chronic ammonia standards for class A,B,C waterbodies w/Trout or Trout Spawning classification
abc_wOut_T_TS_ammonia <- read.csv(file=file.path(Standards_Table_Folder_location,
                                                 "Ammonia/ClassABC_Ammonia_AquatChron_Wout_T_TS_specification.csv"), 
                                  header = TRUE, 
                                  sep = ",",
                                  stringsAsFactors = FALSE, 
                                  as.is = TRUE) 

## Table of aquatic chronic ammonia standards for class A,B,C waterbodies w/Trout or Trout Spawning classification
D_wOut_TS_ammonia <- read.csv(file=file.path(Standards_Table_Folder_location,
                                             "Ammonia/ClassD_Ammonia_AquatChron_Wout_T_TS_specification.csv"), 
                              header = TRUE, 
                              sep = ",",
                              stringsAsFactors = FALSE, 
                              as.is = TRUE)

# Name of output tables
## Table output of parameters where units are different than ug/L; the script should adjust for these, however, if there are unit conversion errors this will serve as a reference point
check_output <- file.path(outputdata_Location,
                          "mismatched_units.csv") 
## Final output file name
finalOutput_csv <- file.path(outputdata_Location,
                             "SWEVI_OUTPUT_WITH_WQ_STANDARDS_AND_COMPARISONS.csv") 

# Location where lab analysis was performed; this defines table formatting
analysis_lab <- "ALS" ##Possible inputs: "USGS" ; "ALS"

################################################################
# Clean up chemistry table
################################################################

if (analysis_lab=="ALS"){
  # Rename fields to standardize the table
  names(chem_data)[names(chem_data)=="chemical_name"] <-"Parameter.Names"
  names(chem_data)[names(chem_data)=="sample_date"] <-"SAMPLE_DATE"
  names(chem_data)[names(chem_data)=="result_value"] <-"VALUE"
  names(chem_data)[names(chem_data)=="result_unit"] <-"UNITS"
  
  # Create a sample column using "BASIN", "LOCATION", "RIVMILE", "SAMPLE_DATE", "SAMPLE_TIME", "lab_sample_id" fields 
  chem_data$sample <- do.call(paste,c(chem_data[c("BASIN", "LOCATION", "RIVMILE", "SAMPLE_DATE")],sep="_"))#Removed "sample_delivery_group", "lab_sample_id"
  
  # Create a parameter sample column using "sample", "Parameter. Names" and fraction
  chem_data$p_sample <- do.call(paste,c(chem_data[c("sample", "Parameter.Names", "fraction")],sep="_"))
  
  # Create a compare column using cas-rn and fraction to catch chem fractions (i.e. Dissolved/Total)
  chem_data$compare_col <- do.call(paste,c(chem_data[c("cas_rn", "fraction")],sep="-"))
  
  # Rename chemistry dataframe for scripting consistency
  data<-chem_data
  
  # Limit Data df variable to only the columns needed for comparison
  keep<-c("sample", "compare_col", "BASIN", "LOCATION", "RIVMILE", "SAMPLE_DATE", "cas_rn", "fraction", "Parameter.Names", "VALUE", "UNITS", "p_sample")
  data <- data[keep]
 
  # Housekeeping
  rm(keep)
  
}#<- Closes ALS Data clean

if (analysis_lab=="USGS"){
  
  # Convert date field to date type
  chem_data$DATES <- as.character(chem_data$DATES)
  chem_data$DATES <- as.Date(chem_data$DATES,"%Y%m%d")
  
  # Rename parameter field to Parameter.Names
  names(chem_data)[names(chem_data)=="PSNAM"] <-"Parameter.Names"
  
  # Convert units column to a character
  chem_data$UNITS<-as.character(chem_data$UNITS)
  
  # Remove data records with a REMRK as these reflect issues with detection limits
  data_detect_issues <- subset(chem_data, chem_data$REMRK==">" | chem_data$REMRK=="<")
  chem_data <- subset(chem_data, chem_data$REMRK=="" | chem_data$REMRK=="A" | chem_data$REMRK=="E" | chem_data$REMRK=="M")
  
  # Copy chem_data to new variable data for script consistency between anlysis lab input
  data <- chem_data
  rm(chem_data)
  
}#<- Closes USGS Data clean

################################################################
# Get sample location waterbody classifications from site file
################################################################

if (analysis_lab=="ALS"){
  
  # Create a classification field from WQ standards field
  site_info$CLASSIFICA<-ifelse(site_info$WQ_STANDARD=="A" | site_info$WQ_STANDARD=="A(T)" | site_info$WQ_STANDARD=="AT" | site_info$WQ_STANDARD=="AS" | site_info$WQ_STANDARD=="A-S"| site_info$WQ_STANDARD=="AST" | site_info$WQ_STANDARD=="A-ST" | site_info$WQ_STANDARD=="A-S(T)" | site_info$WQ_STANDARD=="ASTS" | site_info$WQ_STANDARD=="A-STS" | site_info$WQ_STANDARD=="AS(TS)" | site_info$WQ_STANDARD=="A-S(TS)" | site_info$WQ_STANDARD=="A(TS)" | site_info$WQ_STANDARD=="ATS" | 
                                 site_info$WQ_STANDARD=="AA" | site_info$WQ_STANDARD=="AA(T)" | site_info$WQ_STANDARD=="AAT" |  site_info$WQ_STANDARD=="AA(TS)" | site_info$WQ_STANDARD=="AATS" |site_info$WQ_STANDARD=="AAS" | site_info$WQ_STANDARD=="AA-S" | site_info$WQ_STANDARD=="AA-ST" | site_info$WQ_STANDARD=="AA-S(T)" | site_info$WQ_STANDARD=="AAST" | site_info$WQ_STANDARD=="AAS(T)" | site_info$WQ_STANDARD=="AA-STS" | site_info$WQ_STANDARD=="AA-S(TS)" | 
                                 site_info$WQ_STANDARD=="AASTS" | site_info$WQ_STANDARD=="AAS(TS)", "A", 
                               ifelse(site_info$WQ_STANDARD=="B" | site_info$WQ_STANDARD=="B(T)" | site_info$WQ_STANDARD=="B(TS)" | site_info$WQ_STANDARD=="BT" | site_info$WQ_STANDARD=="BTS", "B", 
                                      ifelse(site_info$WQ_STANDARD=="C" | site_info$WQ_STANDARD=="C(T)" | site_info$WQ_STANDARD=="C(TS)" | site_info$WQ_STANDARD=="CT" | site_info$WQ_STANDARD=="CTS", "C", 
                                             ifelse(site_info$WQ_STANDARD=="D", "D", "NA"))))
  # Create a standards field from the WQ standards field
  site_info$STANDARD<-ifelse(site_info$WQ_STANDARD=="A", "A", ifelse(site_info$WQ_STANDARD=="A(T)" | site_info$WQ_STANDARD=="AT", "A(T)", ifelse(site_info$WQ_STANDARD=="A(TS)" | site_info$WQ_STANDARD=="ATS", "A(TS)", ifelse(
    site_info$WQ_STANDARD=="AA", "AA", ifelse(site_info$WQ_STANDARD=="AA(T)" | site_info$WQ_STANDARD=="AAT", "AA(T)", ifelse(site_info$WQ_STANDARD=="AA(TS)" | site_info$WQ_STANDARD=="AATS", "AA(TS)", ifelse(
      site_info$WQ_STANDARD=="AAS" | site_info$WQ_STANDARD=="AA-S", "AA-S", ifelse(site_info$WQ_STANDARD=="AS" | site_info$WQ_STANDARD=="A-S", "A-S", ifelse(site_info$WQ_STANDARD=="AST" | site_info$WQ_STANDARD=="A-S(T)" | site_info$WQ_STANDARD=="AS(T)" | site_info$WQ_STANDARD=="A-ST", "A-S(T)", ifelse(
        site_info$WQ_STANDARD=="AAST" | site_info$WQ_STANDARD=="AA-S(T)" | site_info$WQ_STANDARD=="AAS(T)" | site_info$WQ_STANDARD=="AA-ST", "AA-S(T)", ifelse(site_info$WQ_STANDARD=="AASTS" | site_info$WQ_STANDARD=="AA-S(TS)" | site_info$WQ_STANDARD=="AAS(TS)" | site_info$WQ_STANDARD=="AA-STS", "AA-S(TS)", ifelse(
          site_info$WQ_STANDARD=="ASTS" | site_info$WQ_STANDARD=="A-S(TS)" | site_info$WQ_STANDARD=="AS(TS)" | site_info$WQ_STANDARD=="A-STS", site_info$STANDARD<-"A-S(TS)", ifelse(site_info$WQ_STANDARD=="B", site_info$STANDARD<-"B", ifelse(site_info$WQ_STANDARD=="B(T)" | site_info$WQ_STANDARD=="BT", "B(T)", ifelse(
            site_info$WQ_STANDARD=="B(TS)" | site_info$WQ_STANDARD=="BTS", site_info$STANDARD<-"B(TS)", ifelse(site_info$WQ_STANDARD=="C", "C", ifelse(site_info$WQ_STANDARD=="C(T)" | site_info$WQ_STANDARD=="CT", "C(T)", ifelse(site_info$WQ_STANDARD=="C(TS)" | site_info$WQ_STANDARD=="CTS", "C(TS)", ifelse(site_info$WQ_STANDARD=="D", "D", "NA")))))))))))))))))))
  
  # Restrict the class table to only the columns we need
  keep <- c("BASIN","LOCATION","RIVMILE","CLASSIFICA","STANDARD", "PWL_ID")
  class <- site_info[keep]
  rm(keep, site_info)
  
  # Convert the LOCATION & RIVMILE to a factor
  class$LOCATION <- as.factor(class$LOCATION)
  class$RIVMILE <- as.factor(class$RIVMILE)
  
  # Merge class with the chem table
  data <- merge(data,class,by=c("BASIN","LOCATION","RIVMILE"),all.x = TRUE)
  
}#<- Closses ALS classification function

if (analysis_lab=="USGS"){
  
  # Restrict the class table to only the columns we need
  keep <- c("LOCATION","RIVMILE","STATION_ID","CLASSIFICA","STANDARD", "PWL_ID")
  class <- site_info[keep]
  rm(keep, site_info)
  
  # Rename the STATION.ID to STAID
  names(class)[names(class)=="STATION_ID"]<-"STAID"
  
  # Convert the class$RIVMILE to a factor
  class$RIVMILE <- as.factor(class$RIVMILE)
  
  # Merge class with the data table
  data <- merge(data,class,by=c("STAID"))
  
  
  # Reduce table to required columns
  keep<-c("LOCATION","RIVMILE","STAID","LOCAL","DATES","TIMES","MEDIM","STYPE","SAMPL","LABNO","PRJCT","ASTAT","LATDD","LNGDD",
          "PCODE","Parameter.Names","UNITS","VALUE","REMRK","DQIND","ANENT","CLASSIFICA","STANDARD", "PWL_ID")
  data <- data[keep]
  rm(keep)
  
  # Create a sample column using LOCATION, RIVMILE, DATES, TIMES, MEDIM, & STYPE fields 
  data$sample <- do.call(paste,c(data[c("STAID", "LOCATION","RIVMILE","DATES", "TIMES", "MEDIM", "STYPE")],sep="_"))
  
  # Create a parameter sample column using "sample" and "Parameter. Names" 
  data$p_sample <- do.call(paste,c(data[c("sample", "Parameter.Names")],sep="_"))
  
  
}#<- Closses USGS classification function

# Housekeeping
rm(class)

################################################################
# Read standards tables and assign specific parameter variables
################################################################

# Create variable for specific parameters in standards table
if (analysis_lab=="ALS"){
  parameter_column <- "cas_rn"
  fraction_column <- "fraction"  
  compare_column <- "compare_col"
  water_ammonia_as_N <- "7664-41-7"
  water_cadmium <- "7440-43-9"
  water_copper <- "7440-50-8"
  water_dissolved_oxygen <- "7782-44-7"
  water_dissolved_solids <- "TDS"
  water_fluoride <- "16984-48-8"
  water_hardness <- "HARD"
  water_lead <- "7439-92-1"
  water_nickel <- "7440-02-0"
  water_ph <- "PH"
  water_silver <- "7440-22-4"
  water_temperature <- "TEMP"
  water_zinc <- "7440-66-6" 
}#<- Closes assignment of variables to water quality parameters using cas_rn
if (analysis_lab=="USGS"){
  parameter_column <- "PCODE"
  water_ammonia_as_N <- "610"
  water_cadmium <- "1025"
  water_copper <- "1040"
  water_dissolved_oxygen <- "300"
  water_dissolved_solids <- "70300"
  water_fluoride <- "951"
  water_hardness <- "903"
  water_lead <- "1049"
  water_nickel <- "1065"
  water_ph <- "400"
  water_silver <- "1075"
  water_temperature <- "10"
  water_zinc <- "1090"
}#<- Closes assignment of variables to water quality parameters using PCODE

################################################################
# Check for unit consistency between data and standards tables
################################################################

if (analysis_lab=="ALS"){
  
  # Merge with data table to pull out parameters from the data table that are in the standards table
  check <- merge(data,standardsA,by=c("cas_rn"))
  
  # Make sure matching units are in the same format so that they are not considered different 
  check$UNITS <- as.character(check$UNITS)
  check$Units <- as.character(check$Units)
  check$UNITS <- tolower(check$UNITS)
  check$Units <- tolower(check$Units)
  
  # Assign a value indicating units that do not match
  check$check <- ifelse(check$UNITS==check$Units,1,0)
  
  # Isolate the records with different units
  check <- check[check$check==0,]
  # Rename parameter field to Parameter.Names
  names(check)[names(check)=="Parameter.Names.x"] <-"ALS.Parameter.Names"
  names(check)[names(check)=="Parameter.Names.y"] <-"USGS.Parameter.Names"
  check <- unique(check[c("sample", "cas_rn","ALS.Parameter.Names", "USGS.Parameter.Names", "UNITS","Units")])
}#<- Closes unit check and mismatched csv output function

if (analysis_lab=="USGS"){
  
  # Merge with data table to pull out parameters from the data table that are in the standards table
  check <- merge(data, standardsA, by=c("PCODE", "Parameter.Names"))
  
  # Make sure matching units are in the same format so that they are not considered different 
  check$UNITS <- as.character(check$UNITS)
  check$Units <- as.character(check$Units)
  check$UNITS <- tolower(check$UNITS)
  check$Units <- tolower(check$Units)
  check$Units <- ifelse(check$Units==" ug/l","ug/l",check$Units)
  check$check <- ifelse(check$UNITS==check$Units,1,0)
  
  # Isolate the records with different units
  check <- check[check$check==0,]
  names(check)[names(check)=="Parameter.Names"] <-"USGS.Parameter.Names"
  check <- unique(check[c("PCODE","USGS.Parameter.Names", "UNITS","Units")])
  }#<- Closes unit check and mismatched csv output function

# Print inconsistencies for the user review
#print(check)
write.csv(check,file=check_output, row.names=FALSE)

# Housekeeping
rm(check,check_output)

################################################################
# Convert units when reported different than standards table
################################################################

if (analysis_lab=="ALS"){  
  
     ##################################################
     ### LONGER LOOP METHOD SERVING AS ALTERNATIVE  ###
     ### TO IFELSE() STATEMENT                      ###
     ### NOT SURE WHY IFELSE() CAUSES TABLE ERRORS  ###
     ### IFELSE() STATEMENTS ARE BELOW LOOP METHOD  ###
     ##################################################

  # Use loop to update row values and adjust for unit conversions
  unique_rows <- unique(data$p_sample)
  n_unique_rows <- length(unique_rows)
  l <- 0
  for (i in 1:n_unique_rows) {
    sample <- data[data$p_sample == unique_rows[i],]
    if (sample$UNITS=="mg/l"){
      sample$VALUE <- (1000*sample$VALUE)
      sample$UNITS <- "ug/l"
      }#<- Closes function to convert mg/l records to ug/l
    if (sample$UNITS=="ng/l"){
      sample$VALUE <- (0.001*sample$VALUE)
      sample$UNITS <- "ug/l"
    }#<- Closes function to convert ng/l records to ug/l
    if (l > 0){
      data2 <- merge(data2, sample, all=TRUE)
    }#<- Closes function merging subsequent loop results to variable
    if (l == 0){
      l<-l+1
      data2 <- sample
    }#<- Closes function assigning first loop results to variable
  }#<- Closes function to update record units to ug/l
  
  ######################## EDITS FOR FRACTION COMPARE ######################## 
  # Merge adjusted values into data table
  data <- merge(data, data2, by=c("BASIN", "LOCATION", "RIVMILE", "sample", "compare_col", "SAMPLE_DATE", "cas_rn", "fraction", "Parameter.Names", "p_sample", "CLASSIFICA", "STANDARD", "PWL_ID"))
  data$VALUE.x <- data$VALUE.y
  data$UNITS.x <- data$UNITS.y
  
  # Keep only desired columns
  keep <- c("BASIN", "LOCATION", "RIVMILE", "SAMPLE_DATE", "cas_rn", "fraction", "Parameter.Names", "CLASSIFICA", "STANDARD", "PWL_ID", "VALUE.x", "UNITS.x", "sample", "compare_col", "p_sample")
  data <- data[keep]
  rm(keep, data2)
  
  # Rename VALUE and UNITS fields for table consistency
  names(data)[names(data)=="VALUE.x"]<-"VALUE"
  names(data)[names(data)=="UNITS.x"]<-"UNITS"
  

     ####################################################
     ### IFELSE() STATEMENTS BELOW CAUSE TABLE ERRORS ###
     ####################################################
     #
     # if (analysis_lab=="ALS"){
     #   # Multiply result value times 1000 when reported as mg/L
     #   data$VALUE <- ifelse(data$UNITS=="mg/l",(1000*data$VALUE), data$VALUE)
     #   data$UNITS <- ifelse(data$UNITS=="mg/l","ug/l",data$UNITS)
     # 
     #   # Multiply result value times 0.001 when reported as ng/L (nanograms per Liter)
     #   
     #   data$UNITS <- ifelse(data$UNITS=="ng/l",(0.001*data$UNITS), data$UNITS)
     #   data$UNITS <- ifelse(data$UNITS=="ng/l","ug/l",data$UNITS)
     #   
     #   # Output data table w/classifications before comparing to standards
     #   write.csv(data,file=data_output,row.names=FALSE)
     # }
     #
     ####################################################
}#<- Closes ALS UNITS update function

if (analysis_lab=="USGS"){
  
  # Multiply result value times 1000 when reported as mg/L as N (miligrams per Liter as Nitrogen)
  data$VALUE<-ifelse(data$UNITS=="mg/l as N",(1000*data$VALUE),data$VALUE)
  data$UNITS<-ifelse(data$UNITS=="mg/l as N","ug/l",data$UNITS)
  
  # Multiply result value times 1000 when reported as mg/L as N (miligrams per Liter as Nitrogen)
  data$VALUE<-ifelse(data$UNITS=="mg/l CaCO3" & data$PCODE=="903",(1000*data$VALUE),data$VALUE)
  data$UNITS<-ifelse(data$UNITS=="mg/l CaCO3" & data$PCODE=="903","ug/l CaCO3",data$UNITS)
  
  # Not working with NH4+. However, this snipit of code will convert if needed
  # data$VALUE<-ifelse(data$UNITS=="mg/l NH4",(1000*data$VALUE),data$VALUE)
  # data$UNITS<-ifelse(data$UNITS=="mg/l NH4","ug/l",data$UNITS)
  
  # Multiply result value times 1000 when reported as mg/L (miligrams per Liter)
  data$VALUE<-ifelse(data$UNITS=="mg/l",(1000*data$VALUE),data$VALUE)
  data$UNITS<-ifelse(data$UNITS=="mg/l","ug/l",data$UNITS)
  
  # Multiply result value times 0.001 when reported as ng/L (nanograms per Liter)
  data$VALUE<-ifelse(data$UNITS=="ng/l",(0.001*data$VALUE),data$VALUE)
  data$UNITS<-ifelse(data$UNITS=="ng/l","ug/l",data$UNITS)
  
}#<- Closes USGS UNITS update function

################################################################
# Subset data by waterbody classification 
################################################################
# Create data subset of class A data and distinct samples in classA data
classA <- subset(data,data$CLASSIFICA == "A")
samplesA <- unique(classA$sample)
nsamplesA <- length(samplesA)
nA=0
# Create df of class B data
classB <- subset(data,data$CLASSIFICA == "B")
samplesB <- unique(classB$sample)
nsamplesB <- length(samplesB)
nB=0
# Create df of class C data
classC <- subset(data,data$CLASSIFICA == "C")
samplesC <- unique(classC$sample)
nsamplesC <- length(samplesC)
nC=0
# Create df of class D data
classD <- subset(data,data$CLASSIFICA == "D")
samplesD <- unique(classD$sample)
nsamplesD <- length(samplesD)
nD=0

# Create list from subsets
data_subset_classes <- list("classA","classB","classC","classD")

################################################################
# Populate standards table formulas and compare data table to standards
################################################################

# Create counting variable to merge loop output together
c <- 0 #<- Variable to compile loop sample into df 
n <- 0 #<- Variable to count the number of loops used to compare records; should match nrows of data

# First loop through list of classification types
for(classes in data_subset_classes){
  if (classes=="classA"){
    nsam <- nsamplesA
    standsample <- standardsA
  }#<- Closes Class A assignment of class object length and standards table variables
  if (classes=="classB"){
    nsam <- nsamplesB
    standsample <- standardsB
  }#<- Closes Class B assignment of class object length and standards table variables
  if (classes=="classC"){
    nsam <- nsamplesC
    standsample <- standardsC
  }#<- Closes Class C assignment of class object length and standards table variables
  if (classes=="classD"){
    nsam <- nsamplesD
    standsample <- standardsD
  }#<- Closes Class D assignment of class object length and standards table variables
  
  # Create compare column in the standards table
  standsample$compare_col <- do.call(paste,c(standsample[c("cas_rn", "fraction")],sep="-"))
  
  # Second loop through the number of samples associated with classification type
  for(i in 1:nsam){
    
    # Subset data to just one sample
    if(classes=="classA"){
      sample <- classA[classA$sample == samplesA[i],]
    }#<- Closes Class A sample subset function
    if(classes=="classB"){
      sample <- classB[classB$sample == samplesB[i],]
    }#<- Closes Class B sample subset function
    if(classes=="classC"){
      sample <- classC[classC$sample == samplesC[i],]
    }#<- Closes Class C sample subset function 
    if(classes=="classD"){
      sample <- classD[classD$sample == samplesD[i],]
    }#<- Closes Class D sample subset function
    
    # Calculate the sample unique standards that require sample hardness
    if(water_hardness %in% sample[[parameter_column]]){
      hardness <- sample$VALUE[sample[[parameter_column]]==water_hardness]#<-introduced variables
      cadmiumAC <- (0.85)*(exp((0.7852*(log(hardness*0.001)))-2.715))
      cadmiumAA <- (0.85)*(exp((1.128*(log(hardness*0.001)))-3.6867))
      copperAC <- (0.96)*(exp((0.8545*(log(hardness*0.001)))-1.702))
      copperAA <- (0.96)*(exp((0.9422*(log(hardness*0.001)))-1.7))
      fluorideAC <- (0.02)*(exp((0.907*(log(hardness*0.001)))+7.394))
      fluorideAA <- (0.1)*(exp((0.907*(log(hardness*0.001)))+7.394))
      leadAC <- (1.46203-((log(hardness*0.001))*(0.145712)))*(exp((1.273*(log(hardness*0.001)))-4.297))
      leadAA <- (1.46203-((log(hardness*0.001))*(0.145712)))*(exp((1.273*(log(hardness*0.001)))-1.052))
      nickelAC <- (0.997)*(exp((0.846*(log(hardness*0.001)))+0.0584))
      nickelAA <- (0.998)*(exp((0.846*(log(hardness*0.001)))+2.255))
      silverAA <- (exp(1.72*(log(hardness*0.001))-6.52))
      zincAC <- (exp(0.85*(log(hardness*0.001))+0.50))
      zincAA <- (0.978*(exp((0.8473*(log(hardness*0.001)))+0.884)))
      
      # Replace Aquatic Chronic formulas in the standards table with calculated values
      standardsAC <- c(cadmiumAC,copperAC,fluorideAC,leadAC,nickelAC,zincAC)
      paramsAC <- c(paste(water_cadmium,"D",sep="-"),
                    paste(water_copper,"D",sep="-"),
                    paste(water_fluoride,"T",sep="-"),
                    paste(water_lead,"D",sep="-"),
                    paste(water_nickel,"D",sep="-"),
                    paste(water_zinc,"D",sep="-"))
      
      nstandardsAC <- length(standardsAC)
      for(j in 1:nstandardsAC){  
        
######################## EDITS FOR FRACTION COMPARE ######################## 
        standsample$Aquatic.Chronic. <- ifelse(standsample[[compare_column]]==paramsAC[j], standardsAC[j], standsample$Aquatic.Chronic.)
      }#<- Closes function to udate aquatic chronic hardness dependent formulas w/calculated values in standard table
      
      # Housekeeping
      rm(j,standardsAC,paramsAC,nstandardsAC)
      
      # Replace Aquatic Acute formulas in the standards table with calculated values
      if (classes == "ClassD"){
        standardsAA <- c(cadmiumAA,copperAA,fluorideAA,leadAA,nickelAA,zincAA)
        paramsAA <- c(paste(water_cadmium,"D",sep="-"),
                      paste(water_copper,"D",sep="-"),
                      paste(water_fluoride,"T",sep="-"),
                      paste(water_lead,"D",sep="-"),
                      paste(water_nickel,"D",sep="-"),
                      paste(water_zinc,"D",sep="-"))
        
        nstandardsAA <- length(standardsAA)
      }#<- Closes function to assign aquatic acute hardness dependent standard values in class D standard table
      if (classes != "ClassD"){
        standardsAA <- c(cadmiumAA,copperAA,leadAA,nickelAA,silverAA,zincAA)
        paramsAA <- c(paste(water_cadmium,"D",sep="-"),
                      paste(water_copper,"D",sep="-"),
                      paste(water_lead,"D",sep="-"),
                      paste(water_nickel,"D",sep="-"),
                      paste(water_silver,"T",sep="-"),
                      paste(water_zinc,"D",sep="-"))
        
        nstandardsAA <- length(standardsAA)
      }#<- Closes function to assign aquatic acute hardness dependent standard values in class A, B, & C standard tables
      for(j in 1:nstandardsAA){
        standsample$Aquatic..Acute. <- ifelse(standsample[[compare_column]]==paramsAA[j], standardsAA[j], standsample$Aquatic..Acute.)
      }#<- Closes function to update aquatic acute hardness dependent formulas w/calculated values in standard tables
      
      # Housekeeping
      rm(j,standardsAA,paramsAA,nstandardsAA)
      rm(hardness,cadmiumAC,cadmiumAA,copperAC,copperAA,fluorideAC,fluorideAA,leadAC,leadAA,nickelAC,nickelAA,silverAA,zincAC,zincAA)
 
    }#<- Closes function to update hardness dependent formulas in standard tables
  
    # Convert standard fields to numeric field
    standsample$Aquatic.Chronic. <- as.numeric(standsample$Aquatic.Chronic.)
    standsample$Aquatic..Acute. <- as.numeric(standsample$Aquatic..Acute.)
    standsample$Health..Water.Source. <- as.numeric(standsample$Health..Water.Source.)
    standsample$Health.Fish.Consumption. <- as.numeric(standsample$Health.Fish.Consumption.)
    standsample$Wildlife <- as.numeric(standsample$Wildlife, na.strings="")
    standsample$Aesthetic.Water.Source. <- as.numeric(standsample$Aesthetic.Water.Source.)
    standsample$Aesthetic.Food.Source. <- as.numeric(standsample$Aesthetic.Food.Source.)
    standsample$Recreation <- as.numeric(standsample$Recreation)

    # Merge the standards and sample table and compare values to standards for each sample
    if(analysis_lab=="ALS"){
######## START ########  EDITS FOR ADDING IN FRACTION FIELD INTO COMPARISONS ########   
      # Restrict standards table to desired fields based on analysis lab
      keep <- c("compare_col", "cas_rn", "fraction", "Health..Water.Source.", "Health.Fish.Consumption.", "Aquatic.Chronic.", 
                "Aquatic..Acute.", "Wildlife", "Aesthetic.Water.Source.", "Aesthetic.Food.Source.", 
                "Recreation")#  
      standsample <- standsample[keep]
      rm(keep)
      
      # Merge sample df with standards table keeping only matching records
      sampleoutput <- merge(sample, standsample, by=c("compare_col", "cas_rn", "fraction"))#
      
      # Compare result values to matching standards and write determination into corresponding violation determination field
      sampleoutput$HWS <- ifelse(is.na(sampleoutput$Health..Water.Source.), "NS", ifelse(sampleoutput$VALUE>sampleoutput$Health..Water.Source.,"V", "NV"))
      sampleoutput$HFS <- ifelse(is.na(sampleoutput$Health.Fish.Consumption.), "NS", ifelse(sampleoutput$VALUE>sampleoutput$Health.Fish.Consumption.,"V", "NV"))
      sampleoutput$AC <- ifelse(is.na(sampleoutput$Aquatic.Chronic.), "NS", ifelse(sampleoutput$VALUE>sampleoutput$Aquatic.Chronic.,"V", "NV"))
      sampleoutput$AA <- ifelse(is.na(sampleoutput$Aquatic..Acute.), "NS", ifelse(sampleoutput$VALUE>sampleoutput$Aquatic..Acute.,"V", "NV"))
      sampleoutput$W <- ifelse(is.na(sampleoutput$Wildlife), "NS", ifelse(sampleoutput$VALUE>sampleoutput$Wildlife,"V", "NV"))
      sampleoutput$EWS <- ifelse(is.na(sampleoutput$Aesthetic.Water.Source.), "NS", ifelse(sampleoutput$VALUE>sampleoutput$Aesthetic.Water.Source.,"V", "NV"))
      sampleoutput$EFS <- ifelse(is.na(sampleoutput$Aesthetic.Food.Source.), "NS", ifelse(sampleoutput$VALUE>sampleoutput$Aesthetic.Food.Source.,"V", "NV"))
      sampleoutput$R <- ifelse(is.na(sampleoutput$Recreation), "NS", ifelse(sampleoutput$VALUE>sampleoutput$Recreation,"V", "NV"))
      
      # Merge compared parameter records back with sample group and keep all additional parameters
      sampleoutput <- merge(sampleoutput,sample,by=c("BASIN","LOCATION","RIVMILE","SAMPLE_DATE","CLASSIFICA","STANDARD","PWL_ID","UNITS", "VALUE","sample","p_sample", "compare_col"),all=TRUE)
      sampleoutput$Parameter.Names.x <- sampleoutput$Parameter.Names.y
      names(sampleoutput)[names(sampleoutput)=="Parameter.Names.x"] <- "Parameter.Names"
      sampleoutput$fraction.x <- sampleoutput$fraction.y
      names(sampleoutput)[names(sampleoutput)=="fraction.x"] <- "fraction"
      sampleoutput$cas_rn.x <- sampleoutput$cas_rn.y
      names(sampleoutput)[names(sampleoutput)=="cas_rn.x"] <- "cas_rn"
      keep <- c("BASIN","LOCATION","RIVMILE","SAMPLE_DATE","Parameter.Names","compare_col", "cas_rn", "fraction", "CLASSIFICA","STANDARD","PWL_ID","UNITS","VALUE","sample","p_sample",
                "Health..Water.Source.", "Health.Fish.Consumption.", "Aquatic.Chronic.", "Aquatic..Acute.", "Wildlife", "Aesthetic.Water.Source.", "Aesthetic.Food.Source.", "Recreation","HWS","HFS","AC","AA","W","EWS","EFS","R")
      sampleoutput <- sampleoutput[keep]
      rm(keep)
######## END ########  EDITS FOR ADDING IN FRACTION FIELD INTO COMPARISONS ########  
    }#<- Closes ALS comparison function
  
    if(analysis_lab=="USGS"){
      
      # Restrict standards table to desired fields based on analysis lab
      keep <- c("PCODE", "Health..Water.Source.", "Health.Fish.Consumption.", "Aquatic.Chronic.", 
                "Aquatic..Acute.", "Wildlife", "Aesthetic.Water.Source.", "Aesthetic.Food.Source.", 
                "Recreation")
      standsample <- standsample[keep]
      rm(keep)
      
      # Merge sample df with standards table keeping only matching records
      sampleoutput <- merge(sample, standsample, by=c("PCODE"), all=FALSE)
      
      # Compare result values to matching standards and write determination into corresponding violation determination field
      sampleoutput$HWS <- ifelse(is.na(sampleoutput$Health..Water.Source.), "NS",
                                 ifelse(sampleoutput$VALUE>sampleoutput$Health..Water.Source.,"V", "NV"))
      sampleoutput$HFS <- ifelse(is.na(sampleoutput$Health.Fish.Consumption.), "NS",
                                 ifelse(sampleoutput$VALUE>sampleoutput$Health.Fish.Consumption.,"V", "NV"))
      sampleoutput$AC <- ifelse(is.na(sampleoutput$Aquatic.Chronic.), "NS",
                                ifelse(sampleoutput$VALUE>sampleoutput$Aquatic.Chronic.,"V", "NV"))
      sampleoutput$AA <- ifelse(is.na(sampleoutput$Aquatic..Acute.), "NS",
                                ifelse(sampleoutput$VALUE>sampleoutput$Aquatic..Acute.,"V", "NV"))
      sampleoutput$W <- ifelse(is.na(sampleoutput$Wildlife), "NS",
                               ifelse(sampleoutput$VALUE>sampleoutput$Wildlife,"V", "NV"))
      sampleoutput$EWS <- ifelse(is.na(sampleoutput$Aesthetic.Water.Source.), "NS",
                                 ifelse(sampleoutput$VALUE>sampleoutput$Aesthetic.Water.Source.,"V", "NV"))
      sampleoutput$EFS <- ifelse(is.na(sampleoutput$Aesthetic.Food.Source.), "NS",
                                 ifelse(sampleoutput$VALUE>sampleoutput$Aesthetic.Food.Source.,"V", "NV"))
      sampleoutput$R <- ifelse(is.na(sampleoutput$Recreation), "NS",
                               ifelse(sampleoutput$VALUE>sampleoutput$Recreation,"V", "NV"))
      
      # Merge compared parameter records back with sample group and keep all additional parameters
      sampleoutput <- merge(sampleoutput, sample, by=c("LOCATION", "RIVMILE", "STAID", "LOCAL", "DATES", 
                                                       "TIMES", "MEDIM", "STYPE", "SAMPL", "LABNO", "PRJCT", 
                                                       "ASTAT", "LATDD", "LNGDD", "PCODE", "Parameter.Names", 
                                                       "DQIND", "ANENT", "CLASSIFICA", "STANDARD", "PWL_ID", 
                                                       "UNITS", "REMRK", "VALUE", "sample", "p_sample"), all=TRUE)

    }#<- Closes USGS comparison function
    
    # Compare measured pH to standards
    if(water_ph %in% sample[[parameter_column]]){
      
      # Create variables to compare pH to pH high/low standard values
      if(analysis_lab=="ALS"){
        
        pH_value <- sample$VALUE[sample[[parameter_column]]==water_ph]
        pH_HWS_high_standard <- standsample$Health..Water.Source.[standsample[[parameter_column]]=="PH.1"]
        pH_HWS_low_standard <- standsample$Health..Water.Source.[standsample[[parameter_column]]=="PH.2"]
        pH_AC_high_standard <- standsample$Aquatic.Chronic.[standsample[[parameter_column]]=="PH.1"]
        pH_AC_low_standard <- standsample$Aquatic.Chronic.[standsample[[parameter_column]]=="PH.2"]
        pH_AA_high_standard <- standsample$Aquatic..Acute.[standsample[[parameter_column]]=="PH.1"]
        pH_AA_low_standard <- standsample$Aquatic..Acute.[standsample[[parameter_column]]=="PH.2"]
        
      }#<- Closes function to assign ph variables with ALS data
      if(analysis_lab=="USGS"){
        
        pH_value <- sample$VALUE[sample[[parameter_column]]==water_ph]
        pH_HWS_high_standard <- standsample$Health..Water.Source.[standsample[[parameter_column]]=="400.1"]
        pH_HWS_low_standard <- standsample$Health..Water.Source.[standsample[[parameter_column]]=="400.2"]
        pH_AC_high_standard <- standsample$Aquatic.Chronic.[standsample[[parameter_column]]=="400.1"]
        pH_AC_low_standard <- standsample$Aquatic.Chronic.[standsample[[parameter_column]]=="400.2"]
        pH_AA_high_standard <- standsample$Aquatic..Acute.[standsample[[parameter_column]]=="400.1"]
        pH_AA_low_standard <- standsample$Aquatic..Acute.[standsample[[parameter_column]]=="400.2"]
        
      }#<- Closes function to assign ph variables with USGS data
      
      # Write the standards values into the output table
      # Populate aquatic chronic pH standards
      if (classes=="classA"| classes=="classB" | classes=="classC"){
        pH_AC_standard <- paste(pH_AC_high_standard, pH_AC_low_standard, sep="-")
        sampleoutput$Aquatic.Chronic.[sampleoutput[[parameter_column]]==water_ph & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_ph]==sample$p_sample[sample[[parameter_column]]==water_ph]] <- pH_AC_standard
      }#<- Closes function to write pH high/low aquatic chronic standards into ouptut table
      
      # Populate health water source pH standards
      if (classes=="classA"){
        pH_HWS_standard<-paste(pH_HWS_high_standard,pH_HWS_low_standard,sep="-")
        sampleoutput$Health..Water.Source.[sampleoutput[[parameter_column]]==water_ph & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_ph]==sample$p_sample[sample[[parameter_column]]==water_ph]]<-pH_HWS_standard
      }#<- Closes function to write pH high/low health water source standards into ouptut table
      
      # Populate aquatic acute pH standards
      if (classes=="classD"){
        pH_AA_standard<-paste(pH_AA_high_standard,pH_AA_low_standard,sep="-")
        sampleoutput$Aquatic..Acute.[sampleoutput[[parameter_column]]==water_ph & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_ph]==sample$p_sample[sample[[parameter_column]]==water_ph]]<-pH_AA_standard
      }#<- Closes function to write pH high/low aquatic acute standards into ouptut table
      
      # Compare pH to standards and write determination into output table
      sampleoutput$AC[sampleoutput[[parameter_column]]==water_ph & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_ph]==sample$p_sample[sample[[parameter_column]]==water_ph]] <- ifelse(is.na(sampleoutput$Aquatic.Chronic.[sampleoutput[[parameter_column]]==water_ph & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_ph]==sample$p_sample[sample[[parameter_column]]==water_ph]]), "NS",
                                                                                                                                                                                                       ifelse(pH_value>pH_AC_high_standard, "V", 
                                                                                                                                                                                                              ifelse(pH_value<pH_AC_low_standard, "V", "NV")))
      
      sampleoutput$HWS[sampleoutput[[parameter_column]]==water_ph & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_ph]==sample$p_sample[sample[[parameter_column]]==water_ph]] <- ifelse(is.na(sampleoutput$Health..Water.Source.[sampleoutput[[parameter_column]]==water_ph & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_ph]==sample$p_sample[sample[[parameter_column]]==water_ph]]), "NS", 
                                                                                                                                                                                                        ifelse(pH_value>pH_HWS_high_standard, "V", 
                                                                                                                                                                                                               ifelse(pH_value<pH_HWS_low_standard, "V", "NV")))
      
      sampleoutput$AA[sampleoutput[[parameter_column]]==water_ph & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_ph]==sample$p_sample[sample[[parameter_column]]==water_ph]] <- ifelse(is.na(sampleoutput$Aquatic..Acute.[sampleoutput[[parameter_column]]==water_ph & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_ph]==sample$p_sample[sample[[parameter_column]]==water_ph]]), "NS", 
                                                                                                                                                                                                       ifelse(pH_value>pH_HWS_high_standard, "V", 
                                                                                                                                                                                                              ifelse(pH_value<pH_HWS_low_standard, "V", "NV")))
      
      # Populate fields that do not have a pH standard
      sampleoutput$HFS[sampleoutput[[parameter_column]]==water_ph & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_ph]==sample$p_sample[sample[[parameter_column]]==water_ph]] <- "NS"
      sampleoutput$AA[sampleoutput[[parameter_column]]==water_ph & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_ph]==sample$p_sample[sample[[parameter_column]]==water_ph]] <- "NS"
      sampleoutput$W[sampleoutput[[parameter_column]]==water_ph & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_ph]==sample$p_sample[sample[[parameter_column]]==water_ph]]<-"NS"
      sampleoutput$EWS[sampleoutput[[parameter_column]]==water_ph & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_ph]==sample$p_sample[sample[[parameter_column]]==water_ph]] <- "NS"
      sampleoutput$EFS[sampleoutput[[parameter_column]]==water_ph & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_ph]==sample$p_sample[sample[[parameter_column]]==water_ph]] <- "NS"
      sampleoutput$R[sampleoutput[[parameter_column]]==water_ph & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_ph]==sample$p_sample[sample[[parameter_column]]==water_ph]] <- "NS"
      
    }#<-closes the pH function
    
    # Compare dissolved oxygen to standards
    if(water_dissolved_oxygen %in% sample[[parameter_column]]){ 
      
      # Check for class A waterbodies and compare to D.O. standard
      if(sample$STANDARD[sample[[parameter_column]]==water_dissolved_oxygen]=="A"){
        sampleoutput$AC[sampleoutput[[parameter_column]]==water_dissolved_oxygen & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_oxygen]==sample$p_sample[sample[[parameter_column]]==water_dissolved_oxygen]] <- ifelse(sample$VALUE[sample[[parameter_column]]==water_dissolved_oxygen & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_oxygen]==sample$p_sample[sample[[parameter_column]]==water_dissolved_oxygen]]<diss_oxygen$A, "V", "NV")
        sampleoutput$Aquatic.Chronic.[sampleoutput[[parameter_column]]==water_dissolved_oxygen & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_oxygen]==sample$p_sample[sample[[parameter_column]]==water_dissolved_oxygen]] <- diss_oxygen$A
      }#<- Closes function to compare D.O. values to standards for class A waterbodies
      if(sample$STANDARD[sample[[parameter_column]]==water_dissolved_oxygen]=="AT" | sample$STANDARD[sample[[parameter_column]]==water_dissolved_oxygen]=="A(T)"){
        sampleoutput$AC[sampleoutput[[parameter_column]]==water_dissolved_oxygen & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_oxygen]==sample$p_sample[sample[[parameter_column]]==water_dissolved_oxygen]] <- ifelse(sample$VALUE[sample[[parameter_column]]==water_dissolved_oxygen & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_oxygen]==sample$p_sample[sample[[parameter_column]]==water_dissolved_oxygen]]<diss_oxygen$A.T., "V", "NV")
        sampleoutput$Aquatic.Chronic.[sampleoutput[[parameter_column]]==water_dissolved_oxygen & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_oxygen]==sample$p_sample[sample[[parameter_column]]==water_dissolved_oxygen]] <- diss_oxygen$A.T.
      }#<- Closes function to compare D.O. values to standards for class A trout waterbodies
      if(sample$STANDARD[sample[[parameter_column]]==water_dissolved_oxygen]=="ATS" | sample$STANDARD[sample[[parameter_column]]==water_dissolved_oxygen]=="A(TS)"){
        sampleoutput$AC[sampleoutput[[parameter_column]]==water_dissolved_oxygen & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_oxygen]==sample$p_sample[sample[[parameter_column]]==water_dissolved_oxygen]] <- ifelse(sample$VALUE[sample[[parameter_column]]==water_dissolved_oxygen & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_oxygen]==sample$p_sample[sample[[parameter_column]]==water_dissolved_oxygen]]<diss_oxygen$A.TS., "V", "NV")
        sampleoutput$Aquatic.Chronic.[sampleoutput[[parameter_column]]==water_dissolved_oxygen & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_oxygen]==sample$p_sample[sample[[parameter_column]]==water_dissolved_oxygen]] <- diss_oxygen$A.TS.
      }#<- Closes function to compare D.O. values to standards for class A trout spawning waterbodies
      ### class AA
      if(sample$STANDARD[sample[[parameter_column]]==water_dissolved_oxygen]=="AA"){
        sampleoutput$AC[sampleoutput[[parameter_column]]==water_dissolved_oxygen & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_oxygen]==sample$p_sample[sample[[parameter_column]]==water_dissolved_oxygen]] <- ifelse(sample$VALUE[sample[[parameter_column]]==water_dissolved_oxygen & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_oxygen]==sample$p_sample[sample[[parameter_column]]==water_dissolved_oxygen]]<diss_oxygen$AA, "V", "NV")
        sampleoutput$Aquatic.Chronic.[sampleoutput[[parameter_column]]==water_dissolved_oxygen & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_oxygen]==sample$p_sample[sample[[parameter_column]]==water_dissolved_oxygen]] <- diss_oxygen$AA
      }#<- Closes function to compare D.O. values to standards for class AA waterbodies
      if(sample$STANDARD[sample[[parameter_column]]==water_dissolved_oxygen]=="AAT" | sample$STANDARD[sample[[parameter_column]]==water_dissolved_oxygen]=="AA(T)"){
        sampleoutput$AC[sampleoutput[[parameter_column]]==water_dissolved_oxygen & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_oxygen]==sample$p_sample[sample[[parameter_column]]==water_dissolved_oxygen]] <- ifelse(sample$VALUE[sample[[parameter_column]]==water_dissolved_oxygen & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_oxygen]==sample$p_sample[sample[[parameter_column]]==water_dissolved_oxygen]]<diss_oxygen$AA.T, "V", "NV")
        sampleoutput$Aquatic.Chronic.[sampleoutput[[parameter_column]]==water_dissolved_oxygen & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_oxygen]==sample$p_sample[sample[[parameter_column]]==water_dissolved_oxygen]] <- diss_oxygen$AA.T
      }#<- Closes function to compare D.O. values to standards for class AA trout waterbodies
      if(sample$STANDARD[sample[[parameter_column]]==water_dissolved_oxygen]=="AATS" | sample$STANDARD[sample[[parameter_column]]==water_dissolved_oxygen]=="AA(TS)"){
        sampleoutput$AC[sampleoutput[[parameter_column]]==water_dissolved_oxygen & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_oxygen]==sample$p_sample[sample[[parameter_column]]==water_dissolved_oxygen]] <- ifelse(sample$VALUE[sample[[parameter_column]]==water_dissolved_oxygen & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_oxygen]==sample$p_sample[sample[[parameter_column]]==water_dissolved_oxygen]]<diss_oxygen$AA.TS., "V", "NV")
        sampleoutput$Aquatic.Chronic.[sampleoutput[[parameter_column]]==water_dissolved_oxygen & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_oxygen]==sample$p_sample[sample[[parameter_column]]==water_dissolved_oxygen]] <- diss_oxygen$AA.TS.
      }#<- Closes function to compare D.O. values to standards for class AA trout spawning waterbodies
      ### class A-S
      if(sample$STANDARD[sample[[parameter_column]]==water_dissolved_oxygen]=="AS" | sample$STANDARD[sample[[parameter_column]]==water_dissolved_oxygen]=="A-S"){
        sampleoutput$AC[sampleoutput[[parameter_column]]==water_dissolved_oxygen & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_oxygen]==sample$p_sample[sample[[parameter_column]]==water_dissolved_oxygen]] <- ifelse(sample$VALUE[sample[[parameter_column]]==water_dissolved_oxygen & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_oxygen]==sample$p_sample[sample[[parameter_column]]==water_dissolved_oxygen]]<diss_oxygen$A.S, "V", "NV")
        sampleoutput$Aquatic.Chronic.[sampleoutput[[parameter_column]]==water_dissolved_oxygen & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_oxygen]==sample$p_sample[sample[[parameter_column]]==water_dissolved_oxygen]] <- diss_oxygen$A.S
      }#<- Closes function to compare D.O. values to standards for class A-S waterbodies
      if(sample$STANDARD[sample[[parameter_column]]==water_dissolved_oxygen]=="AST" | sample$STANDARD[sample[[parameter_column]]==water_dissolved_oxygen]=="AS(T)" |
         sample$STANDARD[sample[[parameter_column]]==water_dissolved_oxygen]=="A-ST" | sample$STANDARD[sample[[parameter_column]]==water_dissolved_oxygen]=="A-S(T)"){
        sampleoutput$AC[sampleoutput[[parameter_column]]==water_dissolved_oxygen & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_oxygen]==sample$p_sample[sample[[parameter_column]]==water_dissolved_oxygen]] <- ifelse(sample$VALUE[sample[[parameter_column]]==water_dissolved_oxygen & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_oxygen]==sample$p_sample[sample[[parameter_column]]==water_dissolved_oxygen]]<diss_oxygen$A.S.T., "V", "NV")
        sampleoutput$Aquatic.Chronic.[sampleoutput[[parameter_column]]==water_dissolved_oxygen & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_oxygen]==sample$p_sample[sample[[parameter_column]]==water_dissolved_oxygen]] <- diss_oxygen$A.S.T.
      }#<- Closes function to compare D.O. values to standards for class A-S trout waterbodies
      if(sample$STANDARD[sample[[parameter_column]]==water_dissolved_oxygen]=="ASTS" | sample$STANDARD[sample[[parameter_column]]==water_dissolved_oxygen]=="A-STS" |
         sample$STANDARD[sample[[parameter_column]]==water_dissolved_oxygen]=="AS(TS)" |  sample$STANDARD[sample[[parameter_column]]==water_dissolved_oxygen]=="A-S(TS)"){
        sampleoutput$AC[sampleoutput[[parameter_column]]==water_dissolved_oxygen & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_oxygen]==sample$p_sample[sample[[parameter_column]]==water_dissolved_oxygen]] <- ifelse(sample$VALUE[sample[[parameter_column]]==water_dissolved_oxygen & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_oxygen]==sample$p_sample[sample[[parameter_column]]==water_dissolved_oxygen]]<diss_oxygen$A.S.TS., "V", "NV")
        sampleoutput$Aquatic.Chronic.[sampleoutput[[parameter_column]]==water_dissolved_oxygen & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_oxygen]==sample$p_sample[sample[[parameter_column]]==water_dissolved_oxygen]] <- diss_oxygen$A.S.TS.
      }#<- Closes function to compare D.O. values to standards for class A-S trout spawning waterbodies
      ### class AA-S
      if(sample$STANDARD[sample[[parameter_column]]==water_dissolved_oxygen]=="AAS" | sample$STANDARD[sample[[parameter_column]]==water_dissolved_oxygen]=="AA-S"){
        sampleoutput$AC[sampleoutput[[parameter_column]]==water_dissolved_oxygen & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_oxygen]==sample$p_sample[sample[[parameter_column]]==water_dissolved_oxygen]] <- ifelse(sample$VALUE[sample[[parameter_column]]==water_dissolved_oxygen & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_oxygen]==sample$p_sample[sample[[parameter_column]]==water_dissolved_oxygen]]<diss_oxygen$AA.S, "V", "NV")
        sampleoutput$Aquatic.Chronic.[sampleoutput[[parameter_column]]==water_dissolved_oxygen & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_oxygen]==sample$p_sample[sample[[parameter_column]]==water_dissolved_oxygen]] <- diss_oxygen$AA.S
      }#<- Closes function to compare D.O. values to standards for class AA-S waterbodies
      if(sample$STANDARD[sample[[parameter_column]]==water_dissolved_oxygen]=="AAST" | sample$STANDARD[sample[[parameter_column]]==water_dissolved_oxygen]=="AA-S(T)" | 
         sample$STANDARD[sample[[parameter_column]]==water_dissolved_oxygen]=="AAS(T)" | sample$STANDARD[sample[[parameter_column]]==water_dissolved_oxygen]=="AA-ST"){
        sampleoutput$AC[sampleoutput[[parameter_column]]==water_dissolved_oxygen & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_oxygen]==sample$p_sample[sample[[parameter_column]]==water_dissolved_oxygen]] <- ifelse(sample$VALUE[sample[[parameter_column]]==water_dissolved_oxygen]<diss_oxygen$AA.S.T., "V", "NV")
        sampleoutput$Aquatic.Chronic.[sampleoutput[[parameter_column]]==water_dissolved_oxygen & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_oxygen]==sample$p_sample[sample[[parameter_column]]==water_dissolved_oxygen]] <- diss_oxygen$AA.S.T.
      }#<- Closes function to compare D.O. values to standards for class AA-S trout waterbodies
      if(sample$STANDARD[sample[[parameter_column]]==water_dissolved_oxygen]=="AASTS" | sample$STANDARD[sample[[parameter_column]]==water_dissolved_oxygen]=="AA-S(TS)" | 
         sample$STANDARD[sample[[parameter_column]]==water_dissolved_oxygen]=="AAS(TS)" | sample$STANDARD[sample[[parameter_column]]==water_dissolved_oxygen]=="AA-STS"){
        sampleoutput$AC[sampleoutput[[parameter_column]]==water_dissolved_oxygen & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_oxygen]==sample$p_sample[sample[[parameter_column]]==water_dissolved_oxygen]] <- ifelse(sample$VALUE[sample[[parameter_column]]==water_dissolved_oxygen]<diss_oxygen$AA.S.TS., "V", "NV")
        sampleoutput$Aquatic.Chronic.[sampleoutput[[parameter_column]]==water_dissolved_oxygen & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_oxygen]==sample$p_sample[sample[[parameter_column]]==water_dissolved_oxygen]] <- diss_oxygen$AA.S.TS.
      }#<- Closes function to compare D.O. values to standards for class AA-S trout spawning waterbodies
      
      # Check for class B waterbodies and compare to D.O. standard
      if(sample$STANDARD[sample[[parameter_column]]==water_dissolved_oxygen]=="B"){
        sampleoutput$AC[sampleoutput[[parameter_column]]==water_dissolved_oxygen & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_oxygen]==sample$p_sample[sample[[parameter_column]]==water_dissolved_oxygen]] <- ifelse(sample$VALUE[sample[[parameter_column]]==water_dissolved_oxygen]<diss_oxygen$B, "V", "NV")
        sampleoutput$Aquatic.Chronic.[sampleoutput[[parameter_column]]==water_dissolved_oxygen & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_oxygen]==sample$p_sample[sample[[parameter_column]]==water_dissolved_oxygen]] <- diss_oxygen$B
      }#<- Closes function to compare D.O. values to standards for class B waterbodies
      if(sample$STANDARD[sample[[parameter_column]]==water_dissolved_oxygen]=="B(T)" | sample$STANDARD[sample[[parameter_column]]==water_dissolved_oxygen]=="BT"){
        sampleoutput$AC[sampleoutput[[parameter_column]]==water_dissolved_oxygen & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_oxygen]==sample$p_sample[sample[[parameter_column]]==water_dissolved_oxygen]] <- ifelse(sample$VALUE[sample[[parameter_column]]==water_dissolved_oxygen]<diss_oxygen$B.T., "V", "NV")
        sampleoutput$Aquatic.Chronic.[sampleoutput[[parameter_column]]==water_dissolved_oxygen & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_oxygen]==sample$p_sample[sample[[parameter_column]]==water_dissolved_oxygen]] <- diss_oxygen$B.T.
      }#<- Closes function to compare D.O. values to standards for class B trout waterbodies
      if(sample$STANDARD[sample[[parameter_column]]==water_dissolved_oxygen]=="B(TS)" | sample$STANDARD[sample[[parameter_column]]==water_dissolved_oxygen]=="BTS"){
        sampleoutput$AC[sampleoutput[[parameter_column]]==water_dissolved_oxygen & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_oxygen]==sample$p_sample[sample[[parameter_column]]==water_dissolved_oxygen]] <- ifelse(sample$VALUE[sample[[parameter_column]]==water_dissolved_oxygen & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_oxygen]==sample$p_sample[sample[[parameter_column]]==water_dissolved_oxygen]]<diss_oxygen$B.TS., "V", "NV")
        sampleoutput$Aquatic.Chronic.[sampleoutput[[parameter_column]]==water_dissolved_oxygen & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_oxygen]==sample$p_sample[sample[[parameter_column]]==water_dissolved_oxygen]] <- diss_oxygen$B.TS.
      }#<- Closes function to compare D.O. values to standards for class B trout spawning waterbodies
      
      # Check for class C waterbodies and compare to D.O. standard
      if(sample$STANDARD[sample[[parameter_column]]==water_dissolved_oxygen]=="C"){
        sampleoutput$AC[sampleoutput[[parameter_column]]==water_dissolved_oxygen & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_oxygen]==sample$p_sample[sample[[parameter_column]]==water_dissolved_oxygen]] <- ifelse(sample$VALUE[sample[[parameter_column]]==water_dissolved_oxygen]<diss_oxygen$C, "V", "NV")
        sampleoutput$Aquatic.Chronic.[sampleoutput[[parameter_column]]==water_dissolved_oxygen & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_oxygen]==sample$p_sample[sample[[parameter_column]]==water_dissolved_oxygen]] <- diss_oxygen$C
      }#<- Closes function to compare D.O. values to standards for class C waterbodies
      if(sample$STANDARD[sample[[parameter_column]]==water_dissolved_oxygen]=="C(T)" | sample$STANDARD[sample[[parameter_column]]==water_dissolved_oxygen]=="CT"){
          sampleoutput$AC[sampleoutput[[parameter_column]]==water_dissolved_oxygen & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_oxygen]==sample$p_sample[sample[[parameter_column]]==water_dissolved_oxygen]] <- ifelse(sample$VALUE[sample[[parameter_column]]==water_dissolved_oxygen]<diss_oxygen$C.T., "V", "NV")
          sampleoutput$Aquatic.Chronic.[sampleoutput[[parameter_column]]==water_dissolved_oxygen & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_oxygen]==sample$p_sample[sample[[parameter_column]]==water_dissolved_oxygen]] <- diss_oxygen$C.T.
      }#<- Closes function to compare D.O. values to standards for class C trout waterbodies
      if(sample$STANDARD[sample[[parameter_column]]==water_dissolved_oxygen]=="C(TS)" | sample$STANDARD[sample[[parameter_column]]==water_dissolved_oxygen]=="CTS"){
        sampleoutput$AC[sampleoutput[[parameter_column]]==water_dissolved_oxygen & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_oxygen]==sample$p_sample[sample[[parameter_column]]==water_dissolved_oxygen]] <- ifelse(sample$VALUE[sampleoutput[[parameter_column]]==water_dissolved_oxygen]<diss_oxygen$C.TS., "V", "NV")
        sampleoutput$Aquatic.Chronic.[sampleoutput[[parameter_column]]==water_dissolved_oxygen & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_oxygen]==sample$p_sample[sample[[parameter_column]]==water_dissolved_oxygen]] <- diss_oxygen$C.TS.
      }#<- Closes function to compare D.O. values to standards for class C trout spawning waterbodies
      
      # Check for class D waterbodies and compare to D.O. standard
      if(sample$STANDARD[sample[[parameter_column]]==water_dissolved_oxygen]=="D"){
        sampleoutput$AC[sampleoutput[[parameter_column]]==water_dissolved_oxygen & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_oxygen]==sample$p_sample[sample[[parameter_column]]==water_dissolved_oxygen]] <- ifelse(sample$VALUE[sampleoutput[[parameter_column]]==water_dissolved_oxygen]<diss_oxygen$D, "V", "NV")
        sampleoutput$AA[sampleoutput[[parameter_column]]==water_dissolved_oxygen & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_oxygen]==sample$p_sample[sample[[parameter_column]]==water_dissolved_oxygen]] <- ifelse(sample$VALUE[sampleoutput[[parameter_column]]==water_dissolved_oxygen]<diss_oxygen$D, "V", "NV")
        sampleoutput$Aquatic.Chronic.[sampleoutput[[parameter_column]]==water_dissolved_oxygen & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_oxygen]==sample$p_sample[sample[[parameter_column]]==water_dissolved_oxygen]] <- diss_oxygen$D
        sampleoutput$Aquatic..Acute.[sampleoutput[[parameter_column]]==water_dissolved_oxygen & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_oxygen]==sample$p_sample[sample[[parameter_column]]==water_dissolved_oxygen]] <- diss_oxygen$D
      }#<- Closes function to compare D.O. values to standards for class D waterbodies
      
      # Fill in the fields that do not have a D.O. standard
      sampleoutput$HWS[(sampleoutput[[parameter_column]]==water_dissolved_oxygen & sampleoutput$p_sample==sample$p_sample)] <- "NS"
      sampleoutput$HFS[(sampleoutput[[parameter_column]]==water_dissolved_oxygen & sampleoutput$p_sample==sample$p_sample)] <- "NS"
      sampleoutput$W[(sampleoutput[[parameter_column]]==water_dissolved_oxygen & sampleoutput$p_sample==sample$p_sample)] <- "NS"
      sampleoutput$EWS[(sampleoutput[[parameter_column]]==water_dissolved_oxygen & sampleoutput$p_sample==sample$p_sample)] <- "NS"
      sampleoutput$EFS[(sampleoutput[[parameter_column]]==water_dissolved_oxygen & sampleoutput$p_sample==sample$p_sample)] <- "NS"
      sampleoutput$R[(sampleoutput[[parameter_column]]==water_dissolved_oxygen & sampleoutput$p_sample==sample$p_sample)] <- "NS"
      
    }#<-- Closes dissolved oxygen function
    
    # Compare dissolved solids to standards
    if(water_dissolved_solids %in% sample[[parameter_column]]){ 
      
      # Check for class A waterbodies and compare to TDS standard
      if(sample$STANDARD[sample[[parameter_column]]==water_dissolved_solids]=="A"){
        sampleoutput$AC[sampleoutput[[parameter_column]]==water_dissolved_solids & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_solids]==sample$p_sample[sample[[parameter_column]]==water_dissolved_solids]] <- ifelse(sample$VALUE[sample[[parameter_column]]==water_dissolved_solids & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_solids]==sample$p_sample[sample[[parameter_column]]==water_dissolved_solids]]>diss_solids$A, "V", "NV")
        sampleoutput$Aquatic.Chronic.[sampleoutput[[parameter_column]]==water_dissolved_solids & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_solids]==sample$p_sample[sample[[parameter_column]]==water_dissolved_solids]] <- diss_solids$A
      }#<- Closes function to compare TDS values to standards for class A waterbodies
      if(sample$STANDARD[sample[[parameter_column]]==water_dissolved_solids]=="AT" | sample$STANDARD[sample[[parameter_column]]==water_dissolved_solids]=="A(T)"){
        sampleoutput$AC[sampleoutput[[parameter_column]]==water_dissolved_solids & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_solids]==sample$p_sample[sample[[parameter_column]]==water_dissolved_solids]] <- ifelse(sample$VALUE[sample[[parameter_column]]==water_dissolved_solids & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_solids]==sample$p_sample[sample[[parameter_column]]==water_dissolved_solids]]>diss_solids$A.T., "V", "NV")
        sampleoutput$Aquatic.Chronic.[sampleoutput[[parameter_column]]==water_dissolved_solids & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_solids]==sample$p_sample[sample[[parameter_column]]==water_dissolved_solids]] <- diss_solids$A.T.
      }#<- Closes function to compare TDS values to standards for class A trout waterbodies
      if(sample$STANDARD[sample[[parameter_column]]==water_dissolved_solids]=="ATS" | sample$STANDARD[sample[[parameter_column]]==water_dissolved_solids]=="A(TS)"){
        sampleoutput$AC[sampleoutput[[parameter_column]]==water_dissolved_solids & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_solids]==sample$p_sample[sample[[parameter_column]]==water_dissolved_solids]] <- ifelse(sample$VALUE[sample[[parameter_column]]==water_dissolved_solids & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_solids]==sample$p_sample[sample[[parameter_column]]==water_dissolved_solids]]>diss_solids$A.TS., "V", "NV")
        sampleoutput$Aquatic.Chronic.[sampleoutput[[parameter_column]]==water_dissolved_solids & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_solids]==sample$p_sample[sample[[parameter_column]]==water_dissolved_solids]] <- diss_solids$A.TS.
      }#<- Closes function to compare TDS values to standards for class A trout spawning waterbodies
      ### class AA
      if(sample$STANDARD[sample[[parameter_column]]==water_dissolved_solids]=="AA"){
        sampleoutput$AC[sampleoutput[[parameter_column]]==water_dissolved_solids & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_solids]==sample$p_sample[sample[[parameter_column]]==water_dissolved_solids]] <- ifelse(sample$VALUE[sample[[parameter_column]]==water_dissolved_solids & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_solids]==sample$p_sample[sample[[parameter_column]]==water_dissolved_solids]]>diss_solids$AA, "V", "NV")
        sampleoutput$Aquatic.Chronic.[sampleoutput[[parameter_column]]==water_dissolved_solids & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_solids]==sample$p_sample[sample[[parameter_column]]==water_dissolved_solids]] <- diss_solids$AA
      }#<- Closes function to compare TDS values to standards for class AA waterbodies
      if(sample$STANDARD[sample[[parameter_column]]==water_dissolved_solids]=="AAT" | sample$STANDARD[sample[[parameter_column]]==water_dissolved_solids]=="AA(T)"){
        sampleoutput$AC[sampleoutput[[parameter_column]]==water_dissolved_solids & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_solids]==sample$p_sample[sample[[parameter_column]]==water_dissolved_solids]] <- ifelse(sample$VALUE[sample[[parameter_column]]==water_dissolved_solids & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_solids]==sample$p_sample[sample[[parameter_column]]==water_dissolved_solids]]>diss_solids$AA.T, "V", "NV")
        sampleoutput$Aquatic.Chronic.[sampleoutput[[parameter_column]]==water_dissolved_solids & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_solids]==sample$p_sample[sample[[parameter_column]]==water_dissolved_solids]] <- diss_solids$AA.T
      }#<- Closes function to compare TDS values to standards for class AA trout waterbodies
      if(sample$STANDARD[sample[[parameter_column]]==water_dissolved_solids]=="AATS" | sample$STANDARD[sample[[parameter_column]]==water_dissolved_solids]=="AA(TS)"){
        sampleoutput$AC[sampleoutput[[parameter_column]]==water_dissolved_solids & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_solids]==sample$p_sample[sample[[parameter_column]]==water_dissolved_solids]] <- ifelse(sample$VALUE[sample[[parameter_column]]==water_dissolved_solids & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_solids]==sample$p_sample[sample[[parameter_column]]==water_dissolved_solids]]>diss_solids$AA.TS., "V", "NV")
        sampleoutput$Aquatic.Chronic.[sampleoutput[[parameter_column]]==water_dissolved_solids & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_solids]==sample$p_sample[sample[[parameter_column]]==water_dissolved_solids]] <- diss_solids$AA.TS.
      }#<- Closes function to compare TDS values to standards for class AA trout spawning waterbodies
      ### class A-S
      if(sample$STANDARD[sample[[parameter_column]]==water_dissolved_solids]=="AS" | sample$STANDARD[sample[[parameter_column]]==water_dissolved_solids]=="A-S"){
        sampleoutput$AC[sampleoutput[[parameter_column]]==water_dissolved_solids & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_solids]==sample$p_sample[sample[[parameter_column]]==water_dissolved_solids]] <- ifelse(sample$VALUE[sample[[parameter_column]]==water_dissolved_solids & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_solids]==sample$p_sample[sample[[parameter_column]]==water_dissolved_solids]]>diss_solids$A.S, "V", "NV")
        sampleoutput$Aquatic.Chronic.[sampleoutput[[parameter_column]]==water_dissolved_solids & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_solids]==sample$p_sample[sample[[parameter_column]]==water_dissolved_solids]] <- diss_solids$A.S
      }#<- Closes function to compare TDS values to standards for class A-S waterbodies
      if(sample$STANDARD[sample[[parameter_column]]==water_dissolved_solids]=="AST" | sample$STANDARD[sample[[parameter_column]]==water_dissolved_solids]=="AS(T)" |
         sample$STANDARD[sample[[parameter_column]]==water_dissolved_solids]=="A-ST" | sample$STANDARD[sample[[parameter_column]]==water_dissolved_solids]=="A-S(T)"){
        sampleoutput$AC[sampleoutput[[parameter_column]]==water_dissolved_solids & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_solids]==sample$p_sample[sample[[parameter_column]]==water_dissolved_solids]] <- ifelse(sample$VALUE[sample[[parameter_column]]==water_dissolved_solids & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_solids]==sample$p_sample[sample[[parameter_column]]==water_dissolved_solids]]>diss_solids$A.S.T., "V", "NV")
        sampleoutput$Aquatic.Chronic.[sampleoutput[[parameter_column]]==water_dissolved_solids & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_solids]==sample$p_sample[sample[[parameter_column]]==water_dissolved_solids]] <- diss_solids$A.S.T.
      }#<- Closes function to compare TDS values to standards for class A-S trout waterbodies
      if(sample$STANDARD[sample[[parameter_column]]==water_dissolved_solids]=="ASTS" | sample$STANDARD[sample[[parameter_column]]==water_dissolved_solids]=="A-STS" |
         sample$STANDARD[sample[[parameter_column]]==water_dissolved_solids]=="AS(TS)" |  sample$STANDARD[sample[[parameter_column]]==water_dissolved_solids]=="A-S(TS)"){
        sampleoutput$AC[sampleoutput[[parameter_column]]==water_dissolved_solids & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_solids]==sample$p_sample[sample[[parameter_column]]==water_dissolved_solids]] <- ifelse(sample$VALUE[sample[[parameter_column]]==water_dissolved_solids & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_solids]==sample$p_sample[sample[[parameter_column]]==water_dissolved_solids]]>diss_solids$A.S.TS., "V", "NV")
        sampleoutput$Aquatic.Chronic.[sampleoutput[[parameter_column]]==water_dissolved_solids & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_solids]==sample$p_sample[sample[[parameter_column]]==water_dissolved_solids]] <- diss_solids$A.S.TS.
      }#<- Closes function to compare TDS values to standards for class A-S trout spawning waterbodies
      ### class AA-S
      if(sample$STANDARD[sample[[parameter_column]]==water_dissolved_solids]=="AAS" | sample$STANDARD[sample[[parameter_column]]==water_dissolved_solids]=="AA-S"){
        sampleoutput$AC[sampleoutput[[parameter_column]]==water_dissolved_solids & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_solids]==sample$p_sample[sample[[parameter_column]]==water_dissolved_solids]] <- ifelse(sample$VALUE[sample[[parameter_column]]==water_dissolved_solids & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_solids]==sample$p_sample[sample[[parameter_column]]==water_dissolved_solids]]>diss_solids$AA.S, "V", "NV")
        sampleoutput$Aquatic.Chronic.[sampleoutput[[parameter_column]]==water_dissolved_solids & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_solids]==sample$p_sample[sample[[parameter_column]]==water_dissolved_solids]] <- diss_solids$AA.S
      }#<- Closes function to compare TDS values to standards for class AA-S waterbodies
      if(sample$STANDARD[sample[[parameter_column]]==water_dissolved_solids]=="AAST" | sample$STANDARD[sample[[parameter_column]]==water_dissolved_solids]=="AA-S(T)" | 
         sample$STANDARD[sample[[parameter_column]]==water_dissolved_solids]=="AAS(T)" | sample$STANDARD[sample[[parameter_column]]==water_dissolved_solids]=="AA-ST"){
        sampleoutput$AC[sampleoutput[[parameter_column]]==water_dissolved_solids & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_solids]==sample$p_sample[sample[[parameter_column]]==water_dissolved_solids]] <- ifelse(sample$VALUE[sample[[parameter_column]]==water_dissolved_solids]>diss_solids$AA.S.T., "V", "NV")
        sampleoutput$Aquatic.Chronic.[sampleoutput[[parameter_column]]==water_dissolved_solids & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_solids]==sample$p_sample[sample[[parameter_column]]==water_dissolved_solids]] <- diss_solids$AA.S.T.
      }#<- Closes function to compare TDS values to standards for class AA-S trout waterbodies
      if(sample$STANDARD[sample[[parameter_column]]==water_dissolved_solids]=="AASTS" | sample$STANDARD[sample[[parameter_column]]==water_dissolved_solids]=="AA-S(TS)" | 
         sample$STANDARD[sample[[parameter_column]]==water_dissolved_solids]=="AAS(TS)" | sample$STANDARD[sample[[parameter_column]]==water_dissolved_solids]=="AA-STS"){
        sampleoutput$AC[sampleoutput[[parameter_column]]==water_dissolved_solids & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_solids]==sample$p_sample[sample[[parameter_column]]==water_dissolved_solids]] <- ifelse(sample$VALUE[sample[[parameter_column]]==water_dissolved_solids]>diss_solids$AA.S.TS., "V", "NV")
        sampleoutput$Aquatic.Chronic.[sampleoutput[[parameter_column]]==water_dissolved_solids & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_solids]==sample$p_sample[sample[[parameter_column]]==water_dissolved_solids]] <- diss_solids$AA.S.TS.
      }#<- Closes function to compare TDS values to standards for class AA-S trout spawning waterbodies
      
      # Check for class B waterbodies and compare to TDS standard
      if(sample$STANDARD[sample[[parameter_column]]==water_dissolved_solids]=="B"){
        sampleoutput$AC[sampleoutput[[parameter_column]]==water_dissolved_solids & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_solids]==sample$p_sample[sample[[parameter_column]]==water_dissolved_solids]] <- ifelse(sample$VALUE[sample[[parameter_column]]==water_dissolved_solids]>diss_solids$B, "V", "NV")
        sampleoutput$Aquatic.Chronic.[sampleoutput[[parameter_column]]==water_dissolved_solids & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_solids]==sample$p_sample[sample[[parameter_column]]==water_dissolved_solids]] <- diss_solids$B
      }#<- Closes function to compare TDS values to standards for class B waterbodies
      if(sample$STANDARD[sample[[parameter_column]]==water_dissolved_solids]=="B(T)" | sample$STANDARD[sample[[parameter_column]]==water_dissolved_solids]=="BT"){
        sampleoutput$AC[sampleoutput[[parameter_column]]==water_dissolved_solids & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_solids]==sample$p_sample[sample[[parameter_column]]==water_dissolved_solids]] <- ifelse(sample$VALUE[sample[[parameter_column]]==water_dissolved_solids]>diss_solids$B.T., "V", "NV")
        sampleoutput$Aquatic.Chronic.[sampleoutput[[parameter_column]]==water_dissolved_solids & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_solids]==sample$p_sample[sample[[parameter_column]]==water_dissolved_solids]] <- diss_solids$B.T.
      }#<- Closes function to compare TDS values to standards for class B trout waterbodies
      if(sample$STANDARD[sample[[parameter_column]]==water_dissolved_solids]=="B(TS)" | sample$STANDARD[sample[[parameter_column]]==water_dissolved_solids]=="BTS"){
        sampleoutput$AC[sampleoutput[[parameter_column]]==water_dissolved_solids & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_solids]==sample$p_sample[sample[[parameter_column]]==water_dissolved_solids]] <- ifelse(sample$VALUE[sample[[parameter_column]]==water_dissolved_solids & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_solids]==sample$p_sample[sample[[parameter_column]]==water_dissolved_solids]]>diss_solids$B.TS., "V", "NV")
        sampleoutput$Aquatic.Chronic.[sampleoutput[[parameter_column]]==water_dissolved_solids & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_solids]==sample$p_sample[sample[[parameter_column]]==water_dissolved_solids]] <- diss_solids$B.TS.
      }#<- Closes function to compare TDS values to standards for class B trout spawning waterbodies
      
      # Check for class C waterbodies and compare to TDS standard
      if(sample$STANDARD[sample[[parameter_column]]==water_dissolved_solids]=="C"){
        sampleoutput$AC[sampleoutput[[parameter_column]]==water_dissolved_solids & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_solids]==sample$p_sample[sample[[parameter_column]]==water_dissolved_solids]] <- ifelse(sample$VALUE[sample[[parameter_column]]==water_dissolved_solids]>diss_solids$C, "V", "NV")
        sampleoutput$Aquatic.Chronic.[sampleoutput[[parameter_column]]==water_dissolved_solids & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_solids]==sample$p_sample[sample[[parameter_column]]==water_dissolved_solids]] <- diss_solids$C
      }#<- Closes function to compare TDS values to standards for class C waterbodies
      if(sample$STANDARD[sample[[parameter_column]]==water_dissolved_solids]=="C(T)" | sample$STANDARD[sample[[parameter_column]]==water_dissolved_solids]=="CT"){
        sampleoutput$AC[sampleoutput[[parameter_column]]==water_dissolved_solids & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_solids]==sample$p_sample[sample[[parameter_column]]==water_dissolved_solids]] <- ifelse(sample$VALUE[sample[[parameter_column]]==water_dissolved_solids]>diss_solids$C.T., "V", "NV")
        sampleoutput$Aquatic.Chronic.[sampleoutput[[parameter_column]]==water_dissolved_solids & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_solids]==sample$p_sample[sample[[parameter_column]]==water_dissolved_solids]] <- diss_solids$C.T.
      }#<- Closes function to compare TDS values to standards for class C trout waterbodies
      if(sample$STANDARD[sample[[parameter_column]]==water_dissolved_solids]=="C(TS)" | sample$STANDARD[sample[[parameter_column]]==water_dissolved_solids]=="CTS"){
        sampleoutput$AC[sampleoutput[[parameter_column]]==water_dissolved_solids & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_solids]==sample$p_sample[sample[[parameter_column]]==water_dissolved_solids]] <- ifelse(sample$VALUE[sampleoutput[[parameter_column]]==water_dissolved_solids]>diss_solids$C.TS., "V", "NV")
        sampleoutput$Aquatic.Chronic.[sampleoutput[[parameter_column]]==water_dissolved_solids & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_dissolved_solids]==sample$p_sample[sample[[parameter_column]]==water_dissolved_solids]] <- diss_solids$C.TS.
      }#<- Closes function to compare TDS values to standards for class C trout spawning waterbodies
      
      # Fill in the fields that do not have a TDS standard
      sampleoutput$HWS[(sampleoutput[[parameter_column]]==water_dissolved_solids & sampleoutput$p_sample==sample$p_sample)] <- "NS"
      sampleoutput$HFS[(sampleoutput[[parameter_column]]==water_dissolved_solids & sampleoutput$p_sample==sample$p_sample)] <- "NS"
      sampleoutput$AA[(sampleoutput[[parameter_column]]==water_dissolved_solids & sampleoutput$p_sample==sample$p_sample)] <- "NS"
      sampleoutput$W[(sampleoutput[[parameter_column]]==water_dissolved_solids & sampleoutput$p_sample==sample$p_sample)] <- "NS"
      sampleoutput$EWS[(sampleoutput[[parameter_column]]==water_dissolved_solids & sampleoutput$p_sample==sample$p_sample)] <- "NS"
      sampleoutput$EFS[(sampleoutput[[parameter_column]]==water_dissolved_solids & sampleoutput$p_sample==sample$p_sample)] <- "NS"
      sampleoutput$R[(sampleoutput[[parameter_column]]==water_dissolved_solids & sampleoutput$p_sample==sample$p_sample)] <- "NS"
      
    }#<-- Closes dissolved solids function
    
    # Compare Ammonia to standards
    if(water_ammonia_as_N %in% sample[[parameter_column]]){
      Ammonia_total <- sample$VALUE[sample[[parameter_column]]==water_ammonia_as_N]
      
      #first calculate freshwater Un-ionized Ammonia (NH3) from known Ammonia mg/l as N (Total Ammonia Nitrogen (NH3+NH4+ = N))
      #########
      ## UIA = Un-ionized Ammonia (NH3)
      ## f_NH3 = fraction of un-ionized ammonia
      ## pH_value = potential Hydrogen (standard units;moles per liter)
      ## T = temperature (degrees Celcius)
      #########
      ## (f)NH3 = (1 / (1 + 10^(pK - pH))) 
      ## where pK = 0.09018 + (2729.92/273.2 + T)
      #########
      
      # Get temperature values to plug into the formula
      if(water_temperature %in% sample[[parameter_column]]){
        water_temp <- sample$VALUE[sample[[parameter_column]]==water_temperature]
        pK <- (0.09018+(2729.92/(273.2+water_temp)))
        f_NH3 <- (1/(1+(10^(pK-pH_value))))
        
        # Multiply f_NH3 by Ammonia mg/l as N to get the Un-ionized Ammonia (UIA)
        UIA <- f_NH3*Ammonia_total
        
        # Compare UIA for Class A, B, & C Non-Trout and Non-Trout spawning streams to aquatic standards
        if(sample$STANDARD[sample[[parameter_column]]==water_ammonia_as_N]=="A" | sample$STANDARD[sample[[parameter_column]]==water_ammonia_as_N]=="AA" | 
           sample$STANDARD[sample[[parameter_column]]==water_ammonia_as_N]=="AS" | sample$STANDARD[sample[[parameter_column]]==water_ammonia_as_N]=="A-S" | 
           sample$STANDARD[sample[[parameter_column]]==water_ammonia_as_N]=="AAS" | sample$STANDARD[sample[[parameter_column]]==water_ammonia_as_N]=="AA-S" | 
           sample$STANDARD[sample[[parameter_column]]==water_ammonia_as_N]=="B" | sample$STANDARD[sample[[parameter_column]]==water_ammonia_as_N]=="C")
          {
          
          # Identify the columns in the standards table to use in the linear interpolation
          
          # Isolate standard table rows using pH
          r_x1 <- ifelse(pH_value>=6.5 & pH_value<6.75, 6.5, ifelse(pH_value>=6.75 & pH_value<7, 6.75, ifelse(pH_value>=7 & pH_value<7.25, 7, ifelse(
            pH_value>=7.25 & pH_value<7.5, r_x1<-7.25, ifelse(pH_value>=7.5 & pH_value<7.75, 7.5, ifelse(pH_value>=7.75 & pH_value<8, 7.75, ifelse(
              pH_value>=8 & pH_value<=9, 8, "pH out of bounds")))))))
          r_x2 <- ifelse(pH_value>=6.5 & pH_value<6.75, 6.75, ifelse(pH_value>=6.75 & pH_value<7, 7, ifelse(pH_value>=7 & pH_value<7.25, 7.25, ifelse(
            pH_value>=7.25 & pH_value<7.5, 7.5, ifelse(pH_value>=7.5 & pH_value<7.75, 7.75, ifelse(pH_value>=7.75 & pH_value<8, 8, ifelse(
              pH_value>=8 & pH_value<=9, 9, "pH out of bounds")))))))
          
          # Isolate standard table columns and values using water temperature column and pH row assignment values 
          c_y1 <- ifelse(water_temp>=0 & water_temp<5, abc_wOut_T_TS_ammonia$X0.C[abc_wOut_T_TS_ammonia$pH==r_x1], ifelse(
            water_temp>=5 & water_temp<10, abc_wOut_T_TS_ammonia$X5.C[abc_wOut_T_TS_ammonia$pH==r_x1], ifelse(
              water_temp>=10 & water_temp<15, abc_wOut_T_TS_ammonia$X10.C[abc_wOut_T_TS_ammonia$pH==r_x1], ifelse(
                water_temp>=15 & water_temp<20, abc_wOut_T_TS_ammonia$X15.C[abc_wOut_T_TS_ammonia$pH==r_x1], ifelse(
                  water_temp>=20 & water_temp<=30, abc_wOut_T_TS_ammonia$X20.C[abc_wOut_T_TS_ammonia$pH==r_x1] , "water temp out of bounds")))))
          
          c_y2 <- ifelse(water_temp>=0 & water_temp<5, abc_wOut_T_TS_ammonia$X0.C[abc_wOut_T_TS_ammonia$pH==r_x2], ifelse(
            water_temp>=5 & water_temp<10, abc_wOut_T_TS_ammonia$X5.C[abc_wOut_T_TS_ammonia$pH==r_x2], ifelse(
              water_temp>=10 & water_temp<15, abc_wOut_T_TS_ammonia$X10.C[abc_wOut_T_TS_ammonia$pH==r_x2], ifelse(
                water_temp>=15 & water_temp<20, abc_wOut_T_TS_ammonia$X15.C[abc_wOut_T_TS_ammonia$pH==r_x2], ifelse(
                  water_temp>=20 & water_temp<=30, abc_wOut_T_TS_ammonia$X20.C[abc_wOut_T_TS_ammonia$pH==r_x2] , "water temp out of bounds")))))
          
          c_y3 <- ifelse(water_temp>=0 & water_temp<5, 0, ifelse(water_temp>=5 & water_temp<10, 5, ifelse(
            water_temp>=10 & water_temp<15, 10, ifelse(water_temp>=15 & water_temp<20, 15, ifelse(
              water_temp>=20 & water_temp<=30, 20 , "water temp out of bounds")))))
          
          c_y4 <- ifelse(water_temp>=0 & water_temp<5, 5, ifelse(water_temp>=5 & water_temp<10, 10, ifelse(
            water_temp>=10 & water_temp<15, 15, ifelse(water_temp>15 & water_temp<20, 20, ifelse(
              water_temp>=20 & water_temp<=30, 30 , "water temp out of bounds")))))
          
          c_y5 <- ifelse(water_temp>=0 & water_temp<5, abc_wOut_T_TS_ammonia$X5.C[abc_wOut_T_TS_ammonia$pH==r_x1], ifelse(
            water_temp>=5 & water_temp<10, abc_wOut_T_TS_ammonia$X10.C[abc_wOut_T_TS_ammonia$pH==r_x1], ifelse(
              water_temp>=10 & water_temp<15, abc_wOut_T_TS_ammonia$X15.C[abc_wOut_T_TS_ammonia$pH==r_x1], ifelse(
                water_temp>=15 & water_temp<20, abc_wOut_T_TS_ammonia$X20.C[abc_wOut_T_TS_ammonia$pH==r_x1], ifelse(
                  water_temp>=20 & water_temp<=30, abc_wOut_T_TS_ammonia$X30.C[abc_wOut_T_TS_ammonia$pH==r_x1], "water temp out of bounds")))))
          
          #########
          ### interpolation formula = [(pH value based) y=y1+(((x-x1)/(x2-x1))*(y2-y1)) | (water temp based) x=x1+(((y-y1)/(y2-y1))*(x2-x1))]
          #########
          ### for pH Ex: y=y1+(((x-x1)/(x2-x1))*(y2-y1)) | y=c_y1+(((pH-r_x1)/(r_x2-r_x1))*(c_y2-c_y1)) 
          ### where, x=pH (pH); x1=next<pH in table (r_x1); x2=next>pH in table (r_x2); y=interpolated standard value 
          ### based on pH; y1=standard value based on next<water temp (c_y1); y2=standard value based on next>water temp (c_y2)
          ### 
          ### for temp Ex: x=x1+(((y-y1)/(y2-y1))*(x2-x1)) | x=c_y1+(((water temp-c_y3)/(c_y4-c_y3))*(c_y5-c_y1))
          ### where, y=water temp; x1=standard value from next<water temp column in line with next<pH row; 
          ### x2=standard value from next>water temp column in line with next<pH row; x=interpolated standard value based on water temp; x1=standard value 
          ### in next<water temp column in line with next<pH row; x2=standard value based on next>water temp
          #########
          
          # Interpolate for pH in standards table
          if (is.numeric(r_x1) & is.numeric(r_x2) & is.numeric(c_y1) & is.numeric(c_y2)){
          pH_interpol <- (c_y1+(((pH_value-r_x1)/(r_x2-r_x1))*(c_y2-c_y1)))
          temp_interpol <- (c_y1+(((water_temp-c_y3)/(c_y4-c_y3))*(c_y5-c_y1)))
          interpol_standard <- ((pH_interpol+temp_interpol)/2)
          sampleoutput$Aquatic.Chronic.[sampleoutput[[parameter_column]]==water_ammonia_as_N & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_ammonia_as_N]==sample$p_sample[sample[[parameter_column]]==water_ammonia_as_N]] <- interpol_standard
          
          # Compare UIA to interpolated standard value
          sampleoutput$AC[sampleoutput[[parameter_column]]==water_ammonia_as_N & 
                            sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_ammonia_as_N]==sample$p_sample[sample[[parameter_column]]==water_ammonia_as_N]] <- ifelse(UIA>interpol_standard, "V", 
                                                                                                                                                                                            ifelse(UIA<interpol_standard, "NV", 
                                                                                                                                                                                                   ifelse(is.na(UIA), "UIA error, calculated as NA", 
                                                                                                                                                                                                          ifelse(is.na(interpol_standard), "interpolation error, standard is NA", 
                                                                                                                                                                                                                 ifelse(UIA==interpol_standard,"UIA equals standard", "error with UIA comparison, UIA neither >, <, or = interpol standard")))))
          }
          if (!is.numeric(r_x1) | !is.numeric(r_x2) | !is.numeric(c_y1) | !is.numeric(c_y2)){
            # Non-numeric interpolated standard value
            sampleoutput$AC[sampleoutput[[parameter_column]]==water_ammonia_as_N & 
                              sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_ammonia_as_N]==sample$p_sample[sample[[parameter_column]]==water_ammonia_as_N]] <- "interpolation error, standard is NA"
          }
          }#<-- Closes Class A, B, & C Non-Trout and Non-Trout spawning streams ammonia sample comparison function
     
        # Compare UIA for Class A, B, & C Trout and Trout spawning streams to aquatic standards
        if(sample$STANDARD[sample[[parameter_column]]==water_ammonia_as_N]=="AT" | sample$STANDARD[sample[[parameter_column]]==water_ammonia_as_N]=="A(T)" | 
           sample$STANDARD[sample[[parameter_column]]==water_ammonia_as_N]=="ATS" | sample$STANDARD[sample[[parameter_column]]==water_ammonia_as_N]=="A(TS)" | 
           sample$STANDARD[sample[[parameter_column]]==water_ammonia_as_N]=="AAT" | sample$STANDARD[sample[[parameter_column]]==water_ammonia_as_N]=="AA(T)" | 
           sample$STANDARD[sample[[parameter_column]]==water_ammonia_as_N]=="AATS" | sample$STANDARD[sample[[parameter_column]]==water_ammonia_as_N]=="AA(TS)" | 
           sample$STANDARD[sample[[parameter_column]]==water_ammonia_as_N]=="AST" | sample$STANDARD[sample[[parameter_column]]==water_ammonia_as_N]=="AS(T)" |
           sample$STANDARD[sample[[parameter_column]]==water_ammonia_as_N]=="A-ST" | sample$STANDARD[sample[[parameter_column]]==water_ammonia_as_N]=="A-S(T)" | 
           sample$STANDARD[sample[[parameter_column]]==water_ammonia_as_N]=="ASTS" | sample$STANDARD[sample[[parameter_column]]==water_ammonia_as_N]=="A-STS" |
           sample$STANDARD[sample[[parameter_column]]==water_ammonia_as_N]=="AS(TS)" |  sample$STANDARD[sample[[parameter_column]]==water_ammonia_as_N]=="A-S(TS)" |  
           sample$STANDARD[sample[[parameter_column]]==water_ammonia_as_N]=="AAST" | sample$STANDARD[sample[[parameter_column]]==water_ammonia_as_N]=="AA-S(T)" | 
           sample$STANDARD[sample[[parameter_column]]==water_ammonia_as_N]=="AAS(T)" | sample$STANDARD[sample[[parameter_column]]==water_ammonia_as_N]=="AA-ST" | 
           sample$STANDARD[sample[[parameter_column]]==water_ammonia_as_N]=="AASTS" | sample$STANDARD[sample[[parameter_column]]==water_ammonia_as_N]=="AA-S(TS)" | 
           sample$STANDARD[sample[[parameter_column]]==water_ammonia_as_N]=="AAS(TS)" | sample$STANDARD[sample[[parameter_column]]==water_ammonia_as_N]=="AA-STS" | 
           sample$STANDARD[sample[[parameter_column]]==water_ammonia_as_N]=="B(T)" | sample$STANDARD[sample[[parameter_column]]==water_ammonia_as_N]=="BT" | 
           sample$STANDARD[sample[[parameter_column]]==water_ammonia_as_N]=="B(TS)" | sample$STANDARD[sample[[parameter_column]]==water_ammonia_as_N]=="BTS" | 
           sample$STANDARD[sample[[parameter_column]]==water_ammonia_as_N]=="C(T)" | sample$STANDARD[sample[[parameter_column]]==water_ammonia_as_N]=="CT" |
           sample$STANDARD[sample[[parameter_column]]==water_ammonia_as_N]=="C(TS)" | sample$STANDARD[sample[[parameter_column]]==water_ammonia_as_N]=="CTS")
        {
          
          # Identify the columns in the standards table to use in the linear interpolation
          
          # Isolate table rows using pH
          r_x1 <- ifelse(pH_value>=6.5 & pH_value<6.75, 6.5, ifelse(pH_value>=6.75 & pH_value<7, 6.75, ifelse(pH_value>=7 & pH_value<7.25, 7, ifelse(
            pH_value>=7.25 & pH_value<7.5, 7.25, ifelse(pH_value>=7.5 & pH_value<7.75, 7.5, ifelse(pH_value>=7.75 & pH_value<8, 7.75, ifelse(
              pH_value>=8 & pH_value<=9, 8, "pH out of bounds")))))))
          r_x2 <- ifelse(pH_value>=6.5 & pH_value<6.75, 6.75, ifelse(pH_value>=6.75 & pH_value<7, 7, ifelse(pH_value>=7 & pH_value<7.25, 7.25, ifelse(
            pH_value>=7.25 & pH_value<7.5, 7.5, ifelse(pH_value>=7.5 & pH_value<7.75, 7.75, ifelse(pH_value>=7.75 & pH_value<8, 8, ifelse(
              pH_value>=8 & pH_value<=9, 9, "pH out of bounds")))))))
          
          # Isolate standard table columns and values using water temperature column and pH row assignment values 
          c_y1<- ifelse(water_temp>=0 & water_temp<5, abc_T_TS_ammonia$X0.C[abc_T_TS_ammonia$pH==r_x1], ifelse(
            water_temp>=5 & water_temp<10, abc_T_TS_ammonia$X5.C[abc_T_TS_ammonia$pH==r_x1], ifelse(
              water_temp>=10 & water_temp<15, abc_T_TS_ammonia$X10.C[abc_T_TS_ammonia$pH==r_x1], ifelse(
                water_temp>=15 & water_temp<=30, abc_T_TS_ammonia$X15.C[abc_T_TS_ammonia$pH==r_x1], "water temp out of bounds"))))
          
          c_y2<-ifelse(water_temp>=0 & water_temp<5, abc_T_TS_ammonia$X0.C[abc_T_TS_ammonia$pH==r_x2], ifelse(
            water_temp>=5 & water_temp<10, abc_T_TS_ammonia$X5.C[abc_T_TS_ammonia$pH==r_x2], ifelse(
              water_temp>=10 & water_temp<15, abc_T_TS_ammonia$X10.C[abc_T_TS_ammonia$pH==r_x2], ifelse(
                water_temp>=15 & water_temp<=30, abc_T_TS_ammonia$X15.C[abc_T_TS_ammonia$pH==r_x2], "water temp out of bounds"))))
          
          c_y3<- ifelse(water_temp>=0 & water_temp<5, 0, ifelse(water_temp>=5 & water_temp<10, 5, ifelse(
            water_temp>=10 & water_temp<15, 10, ifelse(water_temp>=15 & water_temp<=30, 15, "water temp out of bounds"))))
          
          c_y4<-ifelse(water_temp>=0 & water_temp<5, 5, ifelse(water_temp>=5 & water_temp<10, 10, ifelse(
            water_temp>=10 & water_temp<15, 15, ifelse(water_temp>=15 & water_temp<=30, 30, "water temp out of bounds"))))
          
          c_y5<-ifelse(water_temp>=0 & water_temp<5, abc_T_TS_ammonia$X5.C[abc_T_TS_ammonia$pH==r_x1], ifelse(
            water_temp>=5 & water_temp<10, abc_T_TS_ammonia$X10.C[abc_T_TS_ammonia$pH==r_x1], ifelse(
              water_temp>=10 & water_temp<15, abc_T_TS_ammonia$X15.C[abc_T_TS_ammonia$pH==r_x1], ifelse(
                water_temp>=15 & water_temp<=30, abc_T_TS_ammonia$X30.C[abc_T_TS_ammonia$pH==r_x1], "water temp out of bounds"))))
          
          #########
          ### interpolation formula: [(pH_value based) y=y1+(((x-x1)/(x2-x1))*(y2-y1)) | (water_temperature based) x=x1+(((y-y1)/(y2-y1))*(x2-x1))]
          #########
          ### for pH Ex: y=y1+(((x-x1)/(x2-x1))*(y2-y1)) | y=c_y1+(((pH-r_x1)/(r_x2-r_x1))*(c_y2-c_y1)) 
          ### where, x=pH (pH); x1=next<pH in table (r_x1); x2=next>pH in table (r_x2); y=interpolated standard value 
          ### based on pH; y1=standard value based on next<water temp (c_y1); y2=standard value based on next>water temp (c_y2)
          ### 
          ### for temp Ex: x=x1+(((y-y1)/(y2-y1))*(x2-x1)) | x=c_y1+(((water temp-c_y3)/(c_y4-c_y3))*(c_y5-c_y1))
          ### where, y=water temp; x1=standard value from next<water temp column in line with next<pH row; 
          ### x2=standard value from next>water temp column in line with next<pH row; x=interpolated standard value based on water temp; x1=standard value 
          ### in next<water temp column in line with next<pH row; x2=standard value based on next>water temp
          #########
          
          # Interpolate for pH in standards table
          if (is.numeric(r_x1) & is.numeric(r_x2) & is.numeric(c_y1) & is.numeric(c_y2)){
            pH_interpol <- (c_y1+(((pH_value-r_x1)/(r_x2-r_x1))*(c_y2-c_y1)))
            temp_interpol <- (c_y1+(((water_temp-c_y3)/(c_y4-c_y3))*(c_y5-c_y1)))
            interpol_standard <- ((pH_interpol+temp_interpol)/2)
            sampleoutput$Aquatic.Chronic.[sampleoutput[[parameter_column]]==water_ammonia_as_N & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_ammonia_as_N]==sample$p_sample[sample[[parameter_column]]==water_ammonia_as_N]] <- interpol_standard
            
            # Compare UIA to interpolated standard value
            sampleoutput$AC[sampleoutput[[parameter_column]]==water_ammonia_as_N & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_ammonia_as_N]==sample$p_sample[sample[[parameter_column]]==water_ammonia_as_N]] <- ifelse(UIA>interpol_standard, "V", 
                                                                                                                                                                                                                                           ifelse(UIA<interpol_standard, "NV", 
                                                                                                                                                                                                                                                  ifelse(is.na(UIA), "UIA error, calculated as NA", 
                                                                                                                                                                                                                                                         ifelse(is.na(interpol_standard), "interpolation error, standard is NA", 
                                                                                                                                                                                                                                                                ifelse(UIA==interpol_standard, "UIA equals standard", "error with UIA comparison, UIA neither >, <, or = interpol standard")))))
            }
          if (!is.numeric(r_x1) | !is.numeric(r_x2) | !is.numeric(c_y1) | !is.numeric(c_y2)){
            sampleoutput$AC[sampleoutput[[parameter_column]]==water_ammonia_as_N & 
                              sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_ammonia_as_N]==sample$p_sample[sample[[parameter_column]]==water_ammonia_as_N]] <- "interpolation error, standard is NA"
            }
          
          }#<-- Closes class A, B, & C Trout and Trout Spawning streams ammonia sample comparison function
        
        # Compare UIA for Class D Non-Trout and Non-Trout spawning streams to aquatic standards
        if(sample$STANDARD[sample[[parameter_column]]==water_ammonia_as_N]=="D")
          {
          
          # Identify the columns in the standards table to use in the linear interpolation
          
          # Isolate table rows using pH
          r_x1 <- ifelse(pH_value>=6.5 & pH_value<6.75, 6.5, ifelse(pH_value>=6.75 & pH_value<7, 6.75, ifelse(pH_value>=7 & pH_value<7.25, 7, ifelse(
            pH_value>=7.25 & pH_value<7.5, 7.25, ifelse(pH_value>=7.5 & pH_value<7.75, 7.5, ifelse(pH_value>=7.75 & pH_value<8, 7.75, ifelse(
              pH_value>=8 & pH_value<=9, 8, "pH out of bounds")))))))
          r_x2 <- ifelse(pH_value>=6.5 & pH_value<6.75, 6.75, ifelse(pH_value>=6.75 & pH_value<7, 7, ifelse(pH_value>=7 & pH_value<7.25, 7.25, ifelse(
            pH_value>=7.25 & pH_value<7.5, 7.5, ifelse(pH_value>=7.5 & pH_value<7.75, 7.75, ifelse(pH_value>=7.75 & pH_value<8, 8, ifelse(
              pH_value>=8 & pH_value<=9, 9, "pH out of bounds")))))))
          
          # Isolate standard table columns and values using water temperature column and pH row assignment values 
          c_y1 <- ifelse(water_temp>=0 & water_temp<5, abc_wOut_T_TS_ammonia$X0.C[abc_wOut_T_TS_ammonia$pH==r_x1], ifelse(
            water_temp>=5 & water_temp<10, abc_wOut_T_TS_ammonia$X5.C[abc_wOut_T_TS_ammonia$pH==r_x1], ifelse(
              water_temp>=10 & water_temp<15, abc_wOut_T_TS_ammonia$X10.C[abc_wOut_T_TS_ammonia$pH==r_x1], ifelse(
                water_temp>=15 & water_temp<20, abc_wOut_T_TS_ammonia$X15.C[abc_wOut_T_TS_ammonia$pH==r_x1], ifelse(
                  water_temp>=20 & water_temp<25, abc_wOut_T_TS_ammonia$X20.C[abc_wOut_T_TS_ammonia$pH==r_x1], ifelse(
                    water_temp>=25 & water_temp<=30, abc_wOut_T_TS_ammonia$X25.C[abc_wOut_T_TS_ammonia$pH==r_x1] , "water temp out of bounds"))))))
          
          c_y2 <- ifelse(water_temp>=0 & water_temp<5, abc_wOut_T_TS_ammonia$X0.C[abc_wOut_T_TS_ammonia$pH==r_x2], ifelse(
            water_temp>=5 & water_temp<10, abc_wOut_T_TS_ammonia$X5.C[abc_wOut_T_TS_ammonia$pH==r_x2], ifelse(
              water_temp>=10 & water_temp<15, abc_wOut_T_TS_ammonia$X10.C[abc_wOut_T_TS_ammonia$pH==r_x2], ifelse(
                water_temp>=15 & water_temp<20, abc_wOut_T_TS_ammonia$X15.C[abc_wOut_T_TS_ammonia$pH==r_x2], ifelse(
                  water_temp>=20 & water_temp<25, abc_wOut_T_TS_ammonia$X20.C[abc_wOut_T_TS_ammonia$pH==r_x2], ifelse(
                    water_temp>=25 & water_temp<=30, abc_wOut_T_TS_ammonia$X25.C[abc_wOut_T_TS_ammonia$pH==r_x2], "water temp out of bounds"))))))
          
          c_y3<- ifelse(water_temp>=0 & water_temp<5, 0, ifelse(water_temp>=5 & water_temp<10, 5, ifelse(
            water_temp>=10 & water_temp<15, 10, ifelse(water_temp>=15 & water_temp<20, 15, ifelse(
              water_temp>=20 & water_temp<25, 20, ifelse(water_temp>=25 & water_temp<=30, 25, "water temp out of bounds"))))))
          
          c_y4<-ifelse(water_temp>=0 & water_temp<5, 5, ifelse(water_temp>=5 & water_temp<10, 10, ifelse(
            water_temp>=10 & water_temp<15, 15, ifelse(water_temp>15 & water_temp<20, 20, ifelse(
              water_temp>=20 & water_temp<25, 25, ifelse(water_temp>=25 & water_temp<=30, 30, "water temp out of bounds"))))))
          
          c_y5<-ifelse(water_temp>=0 & water_temp<5, abc_wOut_T_TS_ammonia$X5.C[abc_wOut_T_TS_ammonia$pH==r_x1], ifelse(
            water_temp>=5 & water_temp<10, abc_wOut_T_TS_ammonia$X10.C[abc_wOut_T_TS_ammonia$pH==r_x1], ifelse(
              water_temp>=10 & water_temp<15, abc_wOut_T_TS_ammonia$X15.C[abc_wOut_T_TS_ammonia$pH==r_x1], ifelse(
                water_temp>=15 & water_temp<20, abc_wOut_T_TS_ammonia$X20.C[abc_wOut_T_TS_ammonia$pH==r_x1], ifelse(
                  water_temp>=20 & water_temp<25, abc_wOut_T_TS_ammonia$X25.C[abc_wOut_T_TS_ammonia$pH==r_x1], ifelse(
                    water_temp>=25 & water_temp<=30, abc_wOut_T_TS_ammonia$X30.C[abc_wOut_T_TS_ammonia$pH==r_x1], "water temp out of bounds"))))))
          
          #########
          ### interpolation formula = [(pH value based) y=y1+(((x-x1)/(x2-x1))*(y2-y1)) | (water temp based) x=x1+(((y-y1)/(y2-y1))*(x2-x1))]
          #########
          ### for pH Ex: y=y1+(((x-x1)/(x2-x1))*(y2-y1)) || y=c_y1+(((pH-r_x1)/(r_x2-r_x1))*(c_y2-c_y1)) 
          ### where, x=pH (pH); x1=next<pH in table (r_x1); x2=next>pH in table (r_x2); y=interpolated standard value 
          ### based on pH; y1=standard value based on next<water temp (c_y1); y2=standard value based on next>water temp (c_y2)
          ### 
          ### for temp Ex: x=x1+(((y-y1)/(y2-y1))*(x2-x1)) || x=c_y1+(((water temp-c_y3)/(c_y4-c_y3))*(c_y5-c_y1))
          ### where, y=water temp; x1=standard value from next<water temp column in line with next<pH row; 
          ### x2=standard value from next>water temp column in line with next<pH row; x=interpolated standard value based on water temp; x1=standard value 
          ### in next<water temp column in line with next<pH row; x2=standard value based on next>water temp
          #########
          
          # Interpolate for pH in standards table
          if (!is.numeric(r_x1) | !is.numeric(r_x2) | !is.numeric(c_y1) | !is.numeric(c_y2)){
            pH_interpol <- (c_y1+(((pH_value-r_x1)/(r_x2-r_x1))*(c_y2-c_y1)))
            temp_interpol <- (c_y1+(((water_temp-c_y3)/(c_y4-c_y3))*(c_y5-c_y1)))
            interpol_standard <- ((pH_interpol+temp_interpol)/2)
            sampleoutput$Aquatic.Chronic.[sampleoutput[[parameter_column]]==water_ammonia_as_N & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_ammonia_as_N]==sample$p_sample[sample[[parameter_column]]==water_ammonia_as_N]] <- interpol_standard
            
            # Compare UIA to interpolated standard value
            sampleoutput$AC[sampleoutput[[parameter_column]]==water_ammonia_as_N & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_ammonia_as_N]==sample$p_sample[sample[[parameter_column]]==water_ammonia_as_N]] <- ifelse(UIA>interpol_standard, "V", 
                                                                                                                                                                                                                                           ifelse(UIA<interpol_standard, "NV", 
                                                                                                                                                                                                                                                  ifelse(is.na(UIA), "UIA error, calculated as NA", 
                                                                                                                                                                                                                                                         ifelse(is.na(interpol_standard), "interpolation error, standard is NA", 
                                                                                                                                                                                                                                                                ifelse(UIA==interpol_standard,"UIA equals standard", "error with UIA comparison, UIA neither >, <, or = interpol standard")))))
          }
          if (!is.numeric(r_x1) | !is.numeric(r_x2) | !is.numeric(c_y1) | !is.numeric(c_y2)){
            sampleoutput$AC[sampleoutput[[parameter_column]]==water_ammonia_as_N & sampleoutput$p_sample[sampleoutput[[parameter_column]]==water_ammonia_as_N]==sample$p_sample[sample[[parameter_column]]==water_ammonia_as_N]] <- "interpolation error, standard is NA"
          }
          }#<-- Closes Class D Non-Trout and Non-Trout Spawning streams ammonia sample comparison function
        
        }#<- Closes water temperature function in ammonia comparison
      
      }#<- Closes water ammonia function

    # Merge all sampleoutput tables together for final output
    if (c == 0){
      n <- n+1
      finalOutput <- sampleoutput
      c <- c+1
    }#<-Closes count zero function used to create initial final output
    if (c > 0){
      n <- n+1
      finalOutput <- merge(finalOutput, sampleoutput, all=TRUE)
    }#<-Closes count function used to merge each sampleoutput into the final output
  }#<- Closes sample ID subset nested loop
}#<- Closes classes subset outer loop

################################################################
# Merge data frames, clean up headers, and output final table
################################################################
rm(data, sample, sampleoutput, data_subset_classes, classA, classB, classC, classD, 
   abc_T_TS_ammonia, abc_wOut_T_TS_ammonia, D_wOut_TS_ammonia, diss_oxygen, 
   diss_solids, standardsA, standardsB, standardsC, standardsD, standsample)

# Merge finalOutput df with original data set
if (analysis_lab == "ALS"){
  # Merge table with original table to retain all required fields
  if (nrow(chem_data)==nrow(finalOutput)){
    finalOutput <- join(finalOutput, chem_data)#, all = TRUE)
    
    # Use a list of field names to reduce and reorder fields for final output
    keep <- c("Project_name",
              "SiteID",
              "BASIN", 
              "LOCATION", 
              "RIVMILE",
              "SAMPLE_DATE",
              "cas_rn",
              "Parameter.Names",
              "fraction",
              "VALUE",
              "UNITS",
              "Health..Water.Source.",
              "Health.Fish.Consumption.", 
              "Aquatic.Chronic.",  
              "Aquatic..Acute.",
              "Wildlife",
              "Aesthetic.Water.Source.",
              "Aesthetic.Food.Source.",
              "Recreation",
              "HWS",
              "HFS",
              "AC",
              "AA",
              "W",
              "EWS",
              "EFS",
              "R")
    
    finalOutput <- finalOutput[keep]
    
    # Rename VALUE and UNITS to result_value and result_unit fields to standardize header
    names(finalOutput)[names(finalOutput)=="VALUE"] <- "result_value"
    names(finalOutput)[names(finalOutput)=="UNITS"] <- "result_unit"
    names(finalOutput)[names(finalOutput)=="Parameter.Names"] <- "chemical_name" 
    
    # Housekeeping
    rm(chem_data)
  }
  if (nrow(chem_data)!=nrow(finalOutput)){
    print("THERE IS A DIFFERENT NUMBER OF RECORDS BETWEEN FINALOUTPUT AND CHEMISTRY DATA. REVIEW TABLES AND SCRIPT TO VERIFY OUTPUT IS CORRECT.")
  }
}#<- Closes function to merge analyzed data with parent table to get unused columns

# Fill in fields that did not have standards
finalOutput$Health..Water.Source. <- ifelse(is.na(finalOutput$Health..Water.Source.), "NS", finalOutput$Health..Water.Source.)
finalOutput$Health.Fish.Consumption. <- ifelse(is.na(finalOutput$Health.Fish.Consumption.), "NS", finalOutput$Health.Fish.Consumption.)
finalOutput$Aquatic.Chronic. <- ifelse(is.na(finalOutput$Aquatic.Chronic.), "NS", finalOutput$Aquatic.Chronic.)
finalOutput$Aquatic..Acute. <- ifelse(is.na(finalOutput$Aquatic..Acute.), "NS", finalOutput$Aquatic..Acute.)
finalOutput$Wildlife <- ifelse(is.na(finalOutput$Wildlife), "NS", finalOutput$Wildlife)
finalOutput$Aesthetic.Water.Source. <- ifelse(is.na(finalOutput$Aesthetic.Water.Source.), "NS", finalOutput$Aesthetic.Water.Source.)
finalOutput$Aesthetic.Food.Source. <- ifelse(is.na(finalOutput$Aesthetic.Food.Source.), "NS", finalOutput$Aesthetic.Food.Source.)
finalOutput$Recreation <- ifelse(is.na(finalOutput$Recreation), "NS", finalOutput$Recreation)

finalOutput$HWS <- ifelse(is.na(finalOutput$HWS), "NS", ifelse(finalOutput$HWS=="NV", "NV", ifelse(finalOutput$HWS=="V","V", ifelse(finalOutput$HWS=="NS", "NS", "NS"))))
finalOutput$HFS <- ifelse(is.na(finalOutput$HFS), "NS", ifelse(finalOutput$HFS=="NV", "NV", ifelse(finalOutput$HFS=="V","V", ifelse(finalOutput$HFS=="NS", "NS", "NS"))))
finalOutput$AC <- ifelse(is.na(finalOutput$AC), "NS", ifelse(finalOutput$AC=="NV", "NV", ifelse(finalOutput$AC=="V","V", ifelse(finalOutput$AC=="NS", "NS", finalOutput$AC))))
finalOutput$AA <- ifelse(is.na(finalOutput$AA), "NS", ifelse(finalOutput$AA=="NV", "NV", ifelse(finalOutput$AA=="V","V", ifelse(finalOutput$AA=="NS", "NS", finalOutput$AA))))
finalOutput$W <- ifelse(is.na(finalOutput$W), "NS", ifelse(finalOutput$W=="NV", "NV", ifelse(finalOutput$W=="V","V", ifelse(finalOutput$W=="NS", "NS", "NS"))))
finalOutput$EWS <- ifelse(is.na(finalOutput$EWS), "NS", ifelse(finalOutput$EWS=="NV", "NV", ifelse(finalOutput$EWS=="V","V", ifelse(finalOutput$EWS=="NS", "NS", "NS"))))
finalOutput$EFS <- ifelse(is.na(finalOutput$EFS), "NS", ifelse(finalOutput$EFS=="NV", "NV", ifelse(finalOutput$EFS=="V","V", ifelse(finalOutput$EFS=="NS", "NS", "NS"))))
finalOutput$R <- ifelse(is.na(finalOutput$R), "NS", ifelse(finalOutput$R=="NV", "NV", ifelse(finalOutput$R=="V","V", ifelse(finalOutput$R=="NS", "NS", "NS"))))

################################################################
# Write final output
################################################################
if (analysis_lab == "ALS"){
  # keep<-c("BASIN", "LOCATION", "RIVMILE", "CLASSIFICA", "STANDARD", "PWL_ID", "SAMPLE_DATE", "cas_rn", "chemical_name", "result_value",	
  #         "result_unit", "Health..Water.Source.", "Health.Fish.Consumption.", "Aquatic.Chronic.", 
  #         "Aquatic..Acute.", "Wildlife", "Aesthetic.Water.Source.",  "Aesthetic.Food.Source.", "Recreation", 
  #         "HWS", "HFS", "AC", "AA", "W", "EWS", "EFS", "R")
  # 
  # finalOutput <- finalOutput[keep]  
  # 
  # # Housekeeping
  # rm(keep)
}#<- Closes ALS analysis lab based column assignment in final output

if (analysis_lab == "USGS"){
  keep <- c("LOCATION","RIVMILE","STAID","LOCAL","DATES","TIMES","MEDIM","STYPE","SAMPL","LABNO","PRJCT","ASTAT","LATDD","LNGDD","PCODE","Parameter.Names","UNITS","VALUE","REMRK","DQIND","ANENT","CLASSIFICA","STANDARD",
            "Health..Water.Source.","Health.Fish.Consumption.","Aquatic.Chronic.","Aquatic..Acute.","Wildlife","Aesthetic.Water.Source.","Aesthetic.Food.Source.","Recreation","HWS","HFS","AC","AA","W","EWS","EFS", "R")
  finalOutput <- finalOutput[keep]
  rm(keep)
}#<- Closes USGS analysis lab based column assignment in final output

# Write final output table
write.csv(finalOutput,file=finalOutput_csv,row.names=FALSE)

################################################################
# Final clean up
################################################################
#tidy up
rm(list=ls())
