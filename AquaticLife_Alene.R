#Aquatic Life Calculations
#ALene Onion
#March 2019

#restrict to class A,B,C
AQU<-intensive[intensive$simpleWC=="A"|intensive$simpleWC=="B"|intensive$simpleWC=="C",]

#identifying ammonia WQS violations

#first pull ammonia, ph, water temp values side by side
      ## Table of aquatic chronic ammonia standards for class A,B,C waterbodies w/Trout or Trout Spawning classification
      WQST <- read.csv("sections/data/ClassABC_Ammonia_AquatChron_W_T_TS_specification.csv", stringsAsFactors=FALSE) 
      ## Table of aquatic chronic ammonia standards for class A,B,C waterbodies w/Trout or Trout Spawning classification
      WQS <- read.csv("sections/data/ClassABC_Ammonia_AquatChron_Wout_T_TS_specification.csv", stringsAsFactors=FALSE) 
      
      #pull the samples with ammonia
      samples<-AQU[AQU$Characteristic.Name=="AMMONIA",]
      samples<-unique(samples[c('LAKE_ID','SAMPLE_DATE')])
      ammonia<-merge(samples,AQU,by=c('LAKE_ID','SAMPLE_DATE'),all.x = TRUE)
      ammonia<-ammonia[ammonia$Characteristic.Name=="AMMONIA"|ammonia$Characteristic.Name=="TEMPERATURE, WATER"|ammonia$Characteristic.Name=="PH",]
      ammonia<-ammonia[!is.na(ammonia$Characteristic.Name),]
      rm(samples)
      
      #pull out temp and ph
      water_temp<-ammonia[ammonia$Characteristic.Name=="TEMPERATURE, WATER",]
      water_temp<-unique(water_temp[c('LAKE_ID','SAMPLE_DATE','SAMPLE_ID','Result.Value','Depth')])
      names(water_temp)[names(water_temp)=="Result.Value"]<-"water_temp"
      water_temp<-water_temp[!is.na(water_temp$LAKE_ID),]
      
      pH_value<-ammonia[ammonia$Characteristic.Name=="PH",]
      pH_value<-unique(pH_value[c('LAKE_ID','SAMPLE_DATE','SAMPLE_ID','Result.Value','Depth')])
      names(pH_value)[names(pH_value)=="Result.Value"]<-"pH_value"
      pH_value<-pH_value[!is.na(pH_value$LAKE_ID),]
      
      ammonia<-ammonia[ammonia$Characteristic.Name=="AMMONIA",]
      ammonia<-ammonia[!is.na(ammonia$Characteristic.Name),]
      ammonia<-unique(ammonia[c('LAKE_ID','WATER','LOCATION_ID','SAMPLE_ID','SAMPLE_NAME','INFO_TYPE',
                                'SAMPLE_DATE','TIME','Characteristic.Name','Result.Value','Result.Unit','START_DEPTH',
                                'END_DEPTH','Waterbody_Classification','DATA_PROVIDER','basin','simpleT','Depth')])
      
      #pull out the min and max depths to apply to OW and BS samples
      library(dplyr)
      maxtemp<- water_temp %>%
        group_by(SAMPLE_ID) %>%
        summarize(Depth = max(Depth)) %>%
        ungroup()
      maxtemp<-merge(maxtemp,water_temp,by=c('SAMPLE_ID','Depth'),all.x = TRUE)
      maxtemp<-merge(maxtemp,pH_value,all.x = TRUE)
      maxtemp$INFO_TYPE<-"BS"
      maxtemp$SAMPLE_ID<-NULL
      maxtemp$Depth<-NULL
      
      mintemp<- water_temp %>%
        group_by(SAMPLE_ID) %>%
        summarize(Depth = min(Depth)) %>%
        ungroup()
      mintemp<-merge(mintemp,water_temp,by=c('SAMPLE_ID','Depth'),all.x = TRUE)
      mintemp<-merge(mintemp,pH_value,all = TRUE)
      mintemp$INFO_TYPE<-"OW"
      mintemp$SAMPLE_ID<-NULL
      mintemp$Depth<-NULL
      
      water_temp<-merge(maxtemp,mintemp,all=TRUE)
      water_temp<-water_temp[!is.na(water_temp$water_temp),]
      
      ammonia<-merge(ammonia,water_temp,by=c('LAKE_ID','SAMPLE_DATE','INFO_TYPE'),all.x = TRUE)
      rm(list=c('water_temp','pH_value','maxtemp','mintemp'))

#calculate freshwater Un-ionized Ammonia (NH3) from known Ammonia mg/l as N (Total Ammonia Nitrogen (NH3+NH4+ = N))
      #########
      ## UIA = Un-ionized Ammonia (NH3)
      ## f_NH3 = fraction of un-ionized ammonia
      ## pH_value = potential Hydrogen (standard units;moles per liter)
      ## T = temperature (degrees Celcius)
      #########
      ## (f)NH3 = (1 / (1 + 10^(pK - pH))) 
      ## where pK = 0.09018 + (2729.92/273.2 + T)
      #########
      
      ammonia$pK<-0.09018 + (2729.92/(ammonia$water_temp + 273.2))
      ammonia$f_NH3<- (1/(1+(10^(ammonia$pK-ammonia$pH_value))))
      ammonia$UIA <- ammonia$f_NH3*ammonia$Result.Value
      #this is calculated mg/L - convert to ug/L to compare to the standard
      ammonia$UIA<-ammonia$UIA*1000
      

#identify WQ violations for non-trout streams
      AQ<-ammonia[is.na(ammonia$simpleT),]
      
      # Identify the columns in the standards table to use in the linear interpolation
      
      #identify the bracket for the ph
      #lower bracket
      AQ$r_x1<-floor(AQ$pH_value/0.25)*0.25
      AQ$r_x1<- ifelse((AQ$r_x1>8 & AQ$r_x1<9),8,AQ$r_x1)
      AQ$r_x1<- ifelse((AQ$r_x1<6.5|AQ$r_x1>9),NA,AQ$r_x1)
      #remove rows with NA in r_x1 because pH out of range (captured by pH standard)
      AQ<-AQ[!is.na(AQ$r_x1),]
      #upper bracket
      AQ$r_x2<-(floor(AQ$pH_value/0.25)*0.25)+0.25
      AQ$r_x2<- ifelse((AQ$r_x2>8 & AQ$r_x2<=9.25),9,AQ$r_x2)
      
      #identify bracket for water temp
      #lower bracket
      AQ$c_y3<-floor(AQ$water_temp/5)*5
      AQ$c_y3<-ifelse(AQ$c_y3>20,20,AQ$c_y3)
      #upper bracket
      AQ$c_y4<-(floor(AQ$water_temp/5)*5)+5
      AQ$c_y4<-ifelse((AQ$c_y4>20),30,AQ$c_y4)
      
      #pulling the standard
      samples<-unique(AQ$SAMPLE_ID)
      nsamples<-length(samples)
      
      #create empty table
      temp<-AQ[1,]
      temp[1,]<-NA
      temp$c_y1<-NA
      temp$c_y2<-NA
      temp$c_y5<-NA
      
      for(i in 1:nsamples){
        temp1<-AQ[AQ$SAMPLE_ID==samples[i],]
        r_x1<-temp1$r_x1[1]
        r_x2<-temp1$r_x2[1]
        c_y3<-temp1$c_y3[1]
        c_y4<-temp1$c_y3[1]
        #using lower pH bracket
        temp1$c_y1<-WQS[WQS$pH==r_x1,][WQS[WQS$pH==r_x1,]$temp==c_y3,]$standard
        #using upper pH bracket
        temp1$c_y2<-WQS[WQS$pH==r_x2,][WQS[WQS$pH==r_x1,]$temp==c_y3,]$standard
        #using upper temp bracket
        temp1$c_y5<-WQS[WQS$pH==r_x1,][WQS[WQS$pH==r_x1,]$temp==c_y4,]$standard
        temp<-merge(temp,temp1,all=TRUE)
        rm(list=c('temp1','r_x1','c_y3','r_x2','c_y4'))
      }
      temp<-temp[!is.na(temp$c_y1),]
      AQ<-temp
      rm(list=c('i','nsamples','samples','temp'))

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
      AQ$pH_interpol <- (AQ$c_y1+(((AQ$pH_value-AQ$r_x1)/(AQ$r_x2-AQ$r_x1))*(AQ$c_y2-AQ$c_y1)))
      AQ$temp_interpol <- (AQ$c_y1+(((AQ$water_temp-AQ$c_y3)/(AQ$c_y4-AQ$c_y3))*(AQ$c_y5-AQ$c_y1)))
      AQ$interpol_standard <- ((AQ$pH_interpol+AQ$temp_interpol)/2)

      #comparing UIA to interpolated standard value
      AQ$PIaquatic<-NA
      AQ$PIaquatic<-ifelse(AQ$UIA>=AQ$interpol_standard,1,NA)

      #truncate the AQ file and add to trend
      AQ<-unique(AQ[c("SAMPLE_ID","Characteristic.Name","Result.Value","Result.Unit","LAKE_ID","LOCATION_ID","SAMPLE_NAME","INFO_TYPE","SAMPLE_DATE","TIME","START_DEPTH","END_DEPTH","WATER","Waterbody_Classification","DATA_PROVIDER","basin","PIaquatic")])

#Identify impacts for trout streams
      #identify WQ violations for non-trout streams
      AQT<-ammonia[!is.na(ammonia$simpleT),]
      
      # Identify the columns in the standards table to use in the linear interpolation
      
      #identify the bracket for the ph
      #lower bracket
      AQT$r_x1<-floor(AQT$pH_value/0.25)*0.25
      AQT$r_x1<- ifelse((AQT$r_x1>8 & AQT$r_x1<9),8,AQT$r_x1)
      AQT$r_x1<- ifelse((AQT$r_x1<6.5|AQT$r_x1>9),NA,AQT$r_x1)
      #remove rows with NA in r_x1 because pH out of range (captured by pH standard)
      AQT<-AQT[!is.na(AQT$r_x1),]
      #upper bracket
      AQT$r_x2<-(floor(AQT$pH_value/0.25)*0.25)+0.25
      AQT$r_x2<- ifelse((AQT$r_x2>8 & AQT$r_x2<=9.25),9,AQT$r_x2)
      
      #identify bracket for water temp
      #lower bracket
      AQT$c_y3<-floor(AQT$water_temp/5)*5
      AQT$c_y3<-ifelse(AQT$c_y3>15,15,AQT$c_y3)
      #upper bracket
      AQT$c_y4<-(floor(AQT$water_temp/5)*5)+5
      AQT$c_y4<-ifelse((AQT$c_y4>15),30,AQT$c_y4)
      
      #pulling the standard
      samples<-unique(AQT$SAMPLE_ID)
      nsamples<-length(samples)
      
      #create empty table
      temp<-AQT[1,]
      temp[1,]<-NA
      temp$c_y1<-NA
      temp$c_y2<-NA
      temp$c_y5<-NA
      
      for(i in 1:nsamples){
        temp1<-AQT[AQT$SAMPLE_ID==samples[i],]
        r_x1<-temp1$r_x1[1]
        r_x2<-temp1$r_x2[1]
        c_y3<-temp1$c_y3[1]
        c_y4<-temp1$c_y3[1]
        #using lower pH bracket
        temp1$c_y1<-WQST[WQST$pH==r_x1,][WQST[WQST$pH==r_x1,]$temp==c_y3,]$standard
        #using upper pH bracket
        temp1$c_y2<-WQST[WQST$pH==r_x2,][WQST[WQST$pH==r_x1,]$temp==c_y3,]$standard
        #using upper temp bracket
        temp1$c_y5<-WQST[WQST$pH==r_x1,][WQST[WQST$pH==r_x1,]$temp==c_y4,]$standard
        temp<-merge(temp,temp1,all=TRUE)
        rm(list=c('temp1','r_x1','c_y3','r_x2','c_y4'))
      }
      temp<-temp[!is.na(temp$c_y1),]
      AQT<-temp
      rm(list=c('i','nsamples','samples','temp'))
      
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
      AQT$pH_interpol <- (AQT$c_y1+(((AQT$pH_value-AQT$r_x1)/(AQT$r_x2-AQT$r_x1))*(AQT$c_y2-AQT$c_y1)))
      AQT$temp_interpol <- (AQT$c_y1+(((AQT$water_temp-AQT$c_y3)/(AQT$c_y4-AQT$c_y3))*(AQT$c_y5-AQT$c_y1)))
      AQT$interpol_standard <- ((AQT$pH_interpol+AQT$temp_interpol)/2)
      
      #comparing UIA to interpolated standard value
      AQT$PIaquatic<-NA
      AQT$PIaquatic<-ifelse(AQT$UIA>=AQT$interpol_standard,1,NA)
      
      #truncate the AQ file and add to trend
      AQT<-unique(AQT[c("SAMPLE_ID","Characteristic.Name","Result.Value","Result.Unit","LAKE_ID","LOCATION_ID","SAMPLE_NAME","INFO_TYPE","SAMPLE_DATE","TIME","START_DEPTH","END_DEPTH","WATER","Waterbody_Classification","DATA_PROVIDER","basin","PIaquatic")])
      
      rm(list=c('WQS','WQST','ammonia'))
      
      #merging tables 
      AQ<-merge(AQ,AQT,all = TRUE)
      rm(AQT)

#calculate epi do and hypo do and max and min pH values
      thermoclines<-read.csv("sections/data/thermoclines.csv", stringsAsFactors=FALSE)
      AQU<-merge(AQU,thermoclines,by=c('LAKE_ID'),all.x = TRUE)

      #find max value in hypolimnion
      hypoDO<-AQU[AQU$Characteristic.Name=="DISSOLVED OXYGEN (DO)",]
      hypoDO<-AQU[AQU$Depth>AQU$thermocline,]
      library(dplyr)
      hypoDO<- hypoDO %>%
        group_by(SAMPLE_ID) %>%
        summarize(Result.Value = max(Result.Value)) %>%
        ungroup()
      hypoDO$Characteristic.Name <-"HYPO_DO"

      #find max value in epilimnion
      epiDO<-AQU[AQU$Characteristic.Name=="DISSOLVED OXYGEN (DO)",]
      #pull out those that don't have a thermocline so can find max value in whole water column
      epiDO1<-epiDO[epiDO$thermocline==0,]
      epiDO2<-epiDO[epiDO$thermocline!=0,]
      epiDO2<-epiDO2[epiDO2$Depth<epiDO2$thermocline,]
      epiDO<-merge(epiDO1,epiDO2,all=TRUE)
      rm(list=c('epiDO1','epiDO2'))
      library(dplyr)
      epiDO<- epiDO %>%
        group_by(SAMPLE_ID) %>%
        summarize(Result.Value = max(Result.Value)) %>%
        ungroup()
      epiDO$Characteristic.Name <-"EPI_DO"
      
      #find max pH value
      maxph<-AQU[AQU$Characteristic.Name=="PH",]
      library(dplyr)
      maxph<-maxph %>%
        group_by(SAMPLE_ID) %>%
        summarize(Result.Value = max(Result.Value)) %>%
        ungroup()
      maxph$Characteristic.Name <- "PH"
 
      #find min pH value
      minph<-AQU[AQU$Characteristic.Name=="PH",]
      library(dplyr)
      minph<-minph %>%
        group_by(SAMPLE_ID) %>%
        summarize(Result.Value = min(Result.Value)) %>%
        ungroup()
      minph$Characteristic.Name <- "PH"
      
      
      #merge together with other needed fields
      samples<-unique(AQU[c('SAMPLE_ID','LAKE_ID','SAMPLE_NAME','LOCATION_ID','SAMPLE_DATE','TIME','START_DEPTH','END_DEPTH','Depth','WATER','Waterbody_Classification','INFO_TYPE','DATA_PROVIDER','basin','simpleWC','simpleT')])
      hypoDO<-merge(hypoDO,samples,by=c('SAMPLE_ID'),all.x=TRUE)
      epiDO<-merge(epiDO,samples,by=c('SAMPLE_ID'),all.x=TRUE)
      maxph<-merge(maxph,samples,by=c('SAMPLE_ID'),all.x=TRUE)
      minph<-merge(minph,samples,by=c('SAMPLE_ID'),all.x=TRUE)
      #remove duplicate values (when the same ph or DO is read at multiple depths)
      library(dplyr)
      maxph<-maxph %>%
        group_by(SAMPLE_ID,LAKE_ID,SAMPLE_NAME,LOCATION_ID,SAMPLE_DATE,TIME,Characteristic.Name,Result.Value,START_DEPTH,END_DEPTH,WATER,Waterbody_Classification,INFO_TYPE,DATA_PROVIDER,basin,simpleWC,simpleT) %>%
        summarize(Depth = max(Depth)) %>%
        ungroup()
      minph<-minph %>%
        group_by(SAMPLE_ID,LAKE_ID,SAMPLE_NAME,LOCATION_ID,SAMPLE_DATE,TIME,Characteristic.Name,Result.Value,START_DEPTH,END_DEPTH,WATER,Waterbody_Classification,INFO_TYPE,DATA_PROVIDER,basin,simpleWC,simpleT) %>%
        summarize(Depth = max(Depth)) %>%
        ungroup()
      hypoDO<-hypoDO %>%
        group_by(SAMPLE_ID,LAKE_ID,SAMPLE_NAME,LOCATION_ID,SAMPLE_DATE,TIME,Characteristic.Name,Result.Value,START_DEPTH,END_DEPTH,WATER,Waterbody_Classification,INFO_TYPE,DATA_PROVIDER,basin,simpleWC,simpleT) %>%
        summarize(Depth = max(Depth)) %>%
        ungroup()
      epiDO<-epiDO %>%
        group_by(SAMPLE_ID,LAKE_ID,SAMPLE_NAME,LOCATION_ID,SAMPLE_DATE,TIME,Characteristic.Name,Result.Value,START_DEPTH,END_DEPTH,WATER,Waterbody_Classification,INFO_TYPE,DATA_PROVIDER,basin,simpleWC,simpleT) %>%
        summarize(Depth = max(Depth)) %>%
        ungroup()
      #merge files
      AQ2<-merge(hypoDO,epiDO,all=TRUE)
      AQ2<-merge(AQ2,maxph,all=TRUE)
      AQ2<-merge(AQ2,minph,all=TRUE)
      AQ2<-AQ2[!is.na(AQ2$Characteristic.Name),]
      rm(list=c('epiDO','hypoDO','samples','thermoclines','maxph','minph'))
      
      #add thresholds
      short<-unique(thresholds[c('Designated_Use','Characteristic.Name','threshold','Characteristic.Name2','threshold2')])
      AQ2<-merge(AQ2,short,by=c('Characteristic.Name'),all.x=TRUE)
      rm(short)
      AQ2<-AQ2[AQ2$Designated_Use=="aquatic_life",]
      AQ2<-AQ2[!is.na(AQ2$threshold),]
      AQ2<-AQ2[!is.na(AQ2$Result.Value),]
      
      #identify impacts
      AQ2$PIaquatic<-NA
      AQ2$PIaquatic<-ifelse(AQ2$Characteristic.Name=="PH" & AQ2$Result.Value<AQ2$threshold,1,AQ2$PIaquatic)
      AQ2$PIaquatic<-ifelse(AQ2$Characteristic.Name=="PH" & AQ2$Result.Value>AQ2$threshold2,1,AQ2$PIaquatic)
      AQ2$PIaquatic<-ifelse(AQ2$Characteristic.Name=="HYPO_DO",
                            ifelse(AQ2$simpleT=="T" | AQ2$simpleT=="TS",
                              ifelse(AQ2$Result.Value<AQ2$threshold,1,AQ2$PIaquatic),
                              AQ2$PIaquatic),
                            AQ2$PIaquatic)
      AQ2$PIaquatic<-ifelse(AQ2$Characteristic.Name=="EPI_DO" & AQ2$Result.Value<AQ2$threshold,1,AQ2$PIaquatic)
      
#merge two aquatic impact files together
      AQU<-merge(AQ,AQ2,all=TRUE)
      rm(list=c('AQ','AQ2'))
      
      #truncate to impacted samples
      AQU<-AQU[!is.na(AQU$PIaquatic),]
      #truncate to necessary fields
      AQU<-unique(AQU[c("SAMPLE_ID","Characteristic.Name","Result.Value","Result.Unit","LAKE_ID","LOCATION_ID","SAMPLE_NAME","INFO_TYPE","SAMPLE_DATE","TIME","START_DEPTH","END_DEPTH","Depth","WATER","Waterbody_Classification","DATA_PROVIDER","basin","PIaquatic")])

      #merge with trend file
      trend<-merge(trend,AQU,all=TRUE)
      trend<-trend[!is.na(trend$Characteristic.Name),]
      rm(AQU)
      