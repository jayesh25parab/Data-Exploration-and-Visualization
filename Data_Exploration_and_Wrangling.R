#####################################################################################################
# Step 1: Loading Data into R
#####################################################################################################
crashes_complete <- read.csv('Crashes_Last_Five_Years.csv')
#View(crashes_complete)

#####################################################################################################
# Step 2: Subsetting the required columns and removing the rest for efficiency
#####################################################################################################
crashes <- crashes_complete[c("OBJECTID","ACCIDENT_NO","ACCIDENT_DATE","ACCIDENT_TIME","ACCIDENT_TYPE",
                     "DAY_OF_WEEK","HIT_RUN_FLAG","LIGHT_CONDITION","POLICE_ATTEND","ROAD_GEOMETRY",
                     "SPEED_ZONE","RUN_OFFROAD","LONGITUDE","LATITUDE","LGA_NAME","REGION_NAME",
                     "TOTAL_PERSONS","INJ_OR_FATAL","FATALITY","SERIOUSINJURY","OTHERINJURY",
                     "NONINJURED","MALES","FEMALES","BICYCLIST","PASSENGER","DRIVER","PEDESTRIAN",
                     "PILLION","MOTORIST","UNKNOWN","ALCOHOL_RELATED","UNLICENCSED","NO_OF_VEHICLES",
                     "HEAVYVEHICLE","PASSENGERVEHICLE","MOTORCYCLE","PUBLICVEHICLE","DEG_URBAN_NAME",
                     "RMA")]
#View(crashes)


#####################################################################################################
# Step 3: Adding missing values to DAY_OF_WEEK
#####################################################################################################

summary_table <- table(crashes$DAY_OF_WEEK)
View(summary_table)
crashes$DAY_OF_WEEK <- weekdays(as.Date(crashes$ACCIDENT_DATE))
summary_table <- table(crashes$DAY_OF_WEEK)
View(summary_table)

#####################################################################################################
# Step 4: Verifying whether TOTAL_PERSONS = INJ_OR_FATAL + NONINJURED
#####################################################################################################

person_count_checking <- crashes[!(crashes$TOTAL_PERSONS == crashes$INJ_OR_FATAL + crashes$NONINJURED),]
View(person_count_checking)

#####################################################################################################
# Step 5: Verifying whether INJ_OR_FATAL = FATALITY + SERIOUSINJURY + OTHERINJURY
#####################################################################################################

injury_count_check <- crashes[!(crashes$INJ_OR_FATAL== crashes$FATALITY + crashes$SERIOUSINJURY + 
                     crashes$OTHERINJURY),]
View(injury_count_check)

#####################################################################################################
# Step 6: Verifying whether TOTAL_PERSONS = MALES + FEMALES + UNKNOWN
#####################################################################################################

gender_count_checking<-crashes[!(crashes$TOTAL_PERSONS == crashes$MALES + crashes$FEMALES + 
                     crashes$UNKNOWN),]
View(gender_count_checking)
crashes$UNKNOWN <- crashes$TOTAL_PERSONS - (crashes$MALES + crashes$FEMALES)
colnames(crashes)[colnames(crashes)=="UNKNOWN"] <- "UNKNOWN_GENDER"
crashes <- crashes[,c("OBJECTID","ACCIDENT_NO","ACCIDENT_DATE","ACCIDENT_TIME","ACCIDENT_TYPE",
                       "DAY_OF_WEEK","HIT_RUN_FLAG","LIGHT_CONDITION","POLICE_ATTEND","ROAD_GEOMETRY",
                       "SPEED_ZONE","RUN_OFFROAD","LONGITUDE","LATITUDE","LGA_NAME","REGION_NAME",
                       "TOTAL_PERSONS","INJ_OR_FATAL","FATALITY","SERIOUSINJURY","OTHERINJURY",
                       "NONINJURED","MALES","FEMALES","UNKNOWN_GENDER","BICYCLIST","PASSENGER","DRIVER","PEDESTRIAN",
                       "PILLION","MOTORIST","ALCOHOL_RELATED","UNLICENCSED","NO_OF_VEHICLES",
                       "HEAVYVEHICLE","PASSENGERVEHICLE","MOTORCYCLE","PUBLICVEHICLE","DEG_URBAN_NAME",
                       "RMA")]
View(crashes)
gender_count_checking <- crashes[!(crashes$TOTAL_PERSONS== crashes$MALES + crashes$FEMALES + 
                     crashes$UNKNOWN_GENDER),]
View(gender_count_checking)

#####################################################################################################
# Step 7: Verifying whether TOTAL_PERSONS = BICYCLIST + PASSENGER + DRIVER + PEDESTRIAN + PILLION
#####################################################################################################

person_count_checking <- crashes[!(crashes$TOTAL_PERSONS == crashes$BICYCLIST + crashes$PASSENGER + 
                     crashes$DRIVER + crashes$PEDESTRIAN + crashes$PILLION + crashes$MOTORIST),]
View(person_count_checking)
crashes$UNKNOWN_PERSON_TYPE <- crashes$TOTAL_PERSONS - (crashes$BICYCLIST + crashes$PASSENGER + 
                               crashes$DRIVER + crashes$PEDESTRIAN + crashes$PILLION + 
                               crashes$MOTORIST)
View(crashes)
crashes <- crashes[,c("OBJECTID","ACCIDENT_NO","ACCIDENT_DATE","ACCIDENT_TIME","ACCIDENT_TYPE",
                      "DAY_OF_WEEK","HIT_RUN_FLAG","LIGHT_CONDITION","POLICE_ATTEND","ROAD_GEOMETRY",
                      "SPEED_ZONE","RUN_OFFROAD","LONGITUDE","LATITUDE","LGA_NAME","REGION_NAME",
                      "TOTAL_PERSONS","INJ_OR_FATAL","FATALITY","SERIOUSINJURY","OTHERINJURY",
                      "NONINJURED","MALES","FEMALES","UNKNOWN_GENDER","BICYCLIST","PASSENGER",
                      "DRIVER","PEDESTRIAN","PILLION","MOTORIST","UNKNOWN_PERSON_TYPE",
                      "ALCOHOL_RELATED","UNLICENCSED","NO_OF_VEHICLES",
                      "HEAVYVEHICLE","PASSENGERVEHICLE","MOTORCYCLE","PUBLICVEHICLE",
                      "DEG_URBAN_NAME","RMA")]
View(crashes)
person_count_checking <- crashes[!(crashes$TOTAL_PERSONS == crashes$BICYCLIST + crashes$PASSENGER + 
                     crashes$DRIVER + crashes$PEDESTRIAN + crashes$PILLION + crashes$MOTORIST +
                     crashes$UNKNOWN_PERSON_TYPE),]
View(person_count_checking)

##########################################################################################################
# Step 8: Verifying whether NO_OF_VEHICLES = HEAVYVEHICLE + PASSENGERVEHICLE + MOTORCYCLE + PUBLICVEHICLE 
##########################################################################################################
vehicle_count_checking <- crashes[!(crashes$NO_OF_VEHICLES == crashes$HEAVYVEHICLE + crashes$PASSENGERVEHICLE + 
                     crashes$MOTORCYCLE + crashes$PUBLICVEHICLE),]
View(vehicle_count_checking)
newdata<-crashes[(crashes$BICYCLIST > 0),]
View(newdata)
# Assuming number of bicycles involved should be equal to bicyclist
crashes$BICYCLE <- crashes$BICYCLIST
View(crashes)
bicycle_checking <- crashes[!(crashes$BICYCLIST == crashes$BICYCLE),]
View(bicycle_checking)
# Assuming number of motorcycle involved should be equal to motorist
crashes$MOTORCYCLE <- crashes$MOTORIST
View(crashes)
motorcycle_checking <- crashes[!(crashes$MOTORIST == crashes$MOTORCYCLE),]
View(motorcycle_checking)
vehicle_count_checking <- crashes[!(crashes$NO_OF_VEHICLES == crashes$HEAVYVEHICLE + crashes$PASSENGERVEHICLE + 
                     crashes$MOTORCYCLE + crashes$PUBLICVEHICLE + crashes$BICYCLE),]
View(vehicle_count_checking)
crashes$UNKNOWN_VEHICLE_TYPE <- crashes$NO_OF_VEHICLES - (crashes$HEAVYVEHICLE + crashes$PASSENGERVEHICLE + 
                                                          crashes$MOTORCYCLE + crashes$PUBLICVEHICLE + 
                                                          crashes$BICYCLE)
View(crashes)
crashes <- crashes[,c("OBJECTID","ACCIDENT_NO","ACCIDENT_DATE","ACCIDENT_TIME","ACCIDENT_TYPE",
                      "DAY_OF_WEEK","HIT_RUN_FLAG","LIGHT_CONDITION","POLICE_ATTEND","ROAD_GEOMETRY",
                      "SPEED_ZONE","RUN_OFFROAD","LONGITUDE","LATITUDE","LGA_NAME","REGION_NAME",
                      "TOTAL_PERSONS","INJ_OR_FATAL","FATALITY","SERIOUSINJURY","OTHERINJURY",
                      "NONINJURED","MALES","FEMALES","UNKNOWN_GENDER","BICYCLIST","PASSENGER",
                      "DRIVER","PEDESTRIAN","PILLION","MOTORIST","UNKNOWN_PERSON_TYPE",
                      "ALCOHOL_RELATED","UNLICENCSED","NO_OF_VEHICLES",
                      "HEAVYVEHICLE","PASSENGERVEHICLE","MOTORCYCLE","PUBLICVEHICLE","BICYCLE",
                      "UNKNOWN_VEHICLE_TYPE","DEG_URBAN_NAME","RMA")]
View(crashes)

#write.csv(crashes, "C:/Users/JATESH/Documents/Data Visualization/crashes.csv", row.names=F)

##########################################################################################################
# Step 09: Filling Region name for empty values
##########################################################################################################
crashes_1 <- subset(crashes , crashes$REGION_NAME == "")
View(crashes_1)
crashes_2 <- subset(crashes , crashes$REGION_NAME != "")
View(crashes_2)

library(sqldf)
crashes_1$REGION_NAME=" " # to avoid conflicts numeric/characters
crashes_1 <- sqldf(c("UPDATE crashes_1
             SET REGION_NAME = (SELECT crashes_2.REGION_NAME from crashes_2
              where crashes_1.LGA_NAME = crashes_2.LGA_NAME) 
              WHERE EXISTS (SELECT 1
                           FROM crashes_2
                     WHERE crashes_1.LGA_NAME = crashes_2.LGA_NAME
                      )"
             , "select * from main.crashes_1"))
View(crashes_1)

crashes <- rbind(crashes_1, crashes_2)
View(crashes)
crashes_1 <- subset(crashes , crashes$REGION_NAME == "")
View(crashes_1)
crashes_final <-crashes
#crashes <- crashes_final_1
#write.csv(crashes, "C:/Users/JATESH/Documents/Data Visualization/crashes.csv", row.names=F)
#########################################################################################################
# Step 10: Replacing blank values in RMA with "Unknown Road Type"
##########################################################################################################
summary_table <- table(crashes$RMA)
View(summary_table)
crashes$RMA <- sub("^$", "Unknown Road Type", crashes$RMA)
summary_table <- table(crashes$RMA)
View(summary_table)
##########################################################################################################
# Step 11: Replacing "Other speed limit" in SPEED_ZONE with "Unknown Speed"
# Assuming other speed limit is as good as speed not known
##########################################################################################################
summary_table <- table(crashes$SPEED_ZONE)
View(summary_table)
crashes$SPEED_ZONE <- sub("Other speed limit", "Unknown Speed", crashes$SPEED_ZONE)
summary_table <- table(crashes$SPEED_ZONE)
View(summary_table)
crashes$SPEED_ZONE <- sub("Unknown", "Unknown Speed", crashes$SPEED_ZONE)
summary_table <- table(crashes$SPEED_ZONE)
View(summary_table)
##########################################################################################################
# Step 12: Replacing "Unk." in LIGHT_CONDITION with "Unknown"
# Reducing the ambiguity of Unk.
##########################################################################################################
crashes$LIGHT_CONDITION <- sub("Unk.", "Unknown", crashes$LIGHT_CONDITION)
summary_table <- table(crashes$LIGHT_CONDITION)
View(summary_table)
crashes_final <- crashes
View(crashes)
##########################################################################################################
# Step 13: Formatting the LGA_NAME to remove parenthesis and making it uniform
# Eg:- (Falls Creek) to Falls Creek
##########################################################################################################
#library(sqldf)
#names_with_bracket <-sqldf("select * from crashes where LGA_NAME LIKE '%(%'")
#View(names_with_bracket)
#distinct_names_with_bracket <-sqldf("select distinct LGA_NAME from names_with_bracket")
#View(distinct_names_with_bracket)

crashes$LGA_NAME <-  gsub("\\(|\\)", "", crashes$LGA_NAME)


#####################################################################################################
# Step 14: Converting the new dataframe to csv file 
#####################################################################################################

write.csv(crashes, "C:/Users/JATESH/Documents/Data Visualization/crashes.csv", row.names=F)

#####################################################################################################
# Step 15: Checking Latitude and Longitude
#####################################################################################################
library(leaflet)
leaflet(data = crashes) %>% addTiles() %>%
  addMarkers(
    ~LONGITUDE,
    ~LATITUDE,
    popup = ~as.character(OBJECTID),
    clusterOptions = markerClusterOptions()
  )

#####################################################################################################
# Step 16: Checking whether TOTAL_PERSONS == 0
# As if TOTAL_PERSONS = 0 it does not make sense
#####################################################################################################

total_person_checking<-crashes[(crashes$TOTAL_PERSONS == 0),]
View(total_person_checking)

#####################################################################################################
# Step 17: Checking whether MALES + FEMALES + UNKNOWN_GENDER == 0
# As if MALES + FEMALES + UNKNOWN_GENDER == 0 it does not make sense
#####################################################################################################

total_person_checking<-crashes[((crashes$MALES + crashes$FEMALES + crashes$UNKNOWN_GENDER)  == 0),]
View(total_person_checking)

#####################################################################################################
# Step 18: Checking whether INJ_OR_FATAL + NONINJURED == 0
# As if INJ_OR_FATAL + NONINJURED == 0 it does not make sense
#####################################################################################################

total_person_checking<-crashes[((crashes$INJ_OR_FATAL + crashes$NONINJURED)  == 0),]
View(total_person_checking)

#####################################################################################################
# Step 19: Checking whether FATALITY + SERIOUSINJURY + OTHERINJURY == 0
# As if FATALITY + SERIOUSINJURY + OTHERINJURY == 0 it does not make sense
#####################################################################################################

total_person_checking<-crashes[((crashes$FATALITY + crashes$SERIOUSINJURY + 
                                   crashes$OTHERINJURY + crashes$NONINJURED)  == 0),]
View(total_person_checking)

#####################################################################################################
# Step 20: Checking whether BICYCLIST + PASSENGER + DRIVER + PEDESTRIAN + 
#         PILLION + UNKNOWN_PERSON_TYPE == 0
# As if BICYCLIST + PASSENGER + DRIVER + PEDESTRIAN + 
#         PILLION + UNKNOWN_PERSON_TYPE == 0 it does not make sense
#####################################################################################################

total_person_checking<-crashes[((crashes$BICYCLIST + crashes$PASSENGER + 
                                   crashes$DRIVER + crashes$PEDESTRIAN + 
                                   crashes$PILLION + crashes$MOTORIST + 
                                   crashes$UNKNOWN_PERSON_TYPE)  == 0),]
View(total_person_checking)

#####################################################################################################
# Step 21: Checking whether NO_OF_VEHICLES == 0
# As if NO_OF_VEHICLES = 0 it does not make sense
#####################################################################################################

total_vehicle_checking <- crashes[(crashes$NO_OF_VEHICLES == 0),]
View(total_vehicle_checking)

#####################################################################################################
# Step 22: Checking whether HEAVYVEHICLE + PASSENGERVEHICLE + BICYCLE
#           MOTORCYCLE + PUBLICVEHICLE + UNKNOWN_VEHICLE_TYPE == 0
# As if HEAVYVEHICLE + PASSENGERVEHICLE + BICYCLE
#           MOTORCYCLE + PUBLICVEHICLE + UNKNOWN_VEHICLE_TYPE == 0 it does not make sense
#####################################################################################################

total_vehicle_checking <- crashes[((crashes$HEAVYVEHICLE + crashes$PASSENGERVEHICLE + 
                                   crashes$BICYCLE + crashes$UNKNOWN_VEHICLE_TYPE +
                                   crashes$MOTORCYCLE + crashes$PUBLICVEHICLE)  == 0),]
View(total_vehicle_checking)

#####################################################################################################
# Step 23: Checking whether columns are numeric
#####################################################################################################

is.numeric(crashes$OBJECTID)
is.numeric(crashes$LONGITUDE)
is.numeric(crashes$LATITUDE)
is.numeric(crashes$TOTAL_PERSONS)
is.numeric(crashes$INJ_OR_FATAL)
is.numeric(crashes$FATALITY)
is.numeric(crashes$SERIOUSINJURY)
is.numeric(crashes$OTHERINJURY)
is.numeric(crashes$NONINJURED)
is.numeric(crashes$MALES)
is.numeric(crashes$FEMALES)
is.numeric(crashes$UNKNOWN_GENDER)
is.numeric(crashes$BICYCLIST)
is.numeric(crashes$PASSENGER)
is.numeric(crashes$DRIVER)
is.numeric(crashes$PEDESTRIAN)
is.numeric(crashes$PILLION)
is.numeric(crashes$MOTORIST)
is.numeric(crashes$UNKNOWN_PERSON_TYPE)
is.numeric(crashes$UNLICENCSED)
is.numeric(crashes$NO_OF_VEHICLES)
is.numeric(crashes$HEAVYVEHICLE)
is.numeric(crashes$PASSENGERVEHICLE)
is.numeric(crashes$MOTORCYCLE)
is.numeric(crashes$PUBLICVEHICLE)
is.numeric(crashes$BICYCLE)
is.numeric(crashes$UNKNOWN_VEHICLE_TYPE)

is.date(crashes$ACCIDENT_DATE)


typeof(crashes$ACCIDENT_DATE)
is.character(crashes$ALCOHOL_RELATED)
is.String(crashes$ALCOHOL_RELATED)



#####################################################################################################
# Step 24: Changing in crashes$ACCIDENT_TYPE "collision with some other object" to "Collision with some other object"
#####################################################################################################

crashes$ACCIDENT_TYPE <- sub("collision with some other object", "Collision with some other object", crashes$ACCIDENT_TYPE)
View(crashes)

#####################################################################################################
# Step 25: Changing in crashes$ACCIDENT_TYPE "collision with some other object" to "Collision with some other object"
#####################################################################################################

crashes$SPEED_ZONE <- sub("30km/hr", "30 km/hr", crashes$SPEED_ZONE)
View(crashes)
write.csv(crashes, "C:/Users/JATESH/Documents/Data Visualization/crashes.csv", row.names=F)
#####################################################################################################
# Step 26: Data exploration
#####################################################################################################
#####################################################################################################
# Step 27: Checking accidents by Light Condition
#####################################################################################################
crashes <- read.csv('crashes.csv')
View(crashes)
counts <- table(crashes$LIGHT_CONDITION )
View(counts)
barplot(counts, main="Accidents",
        xlab="Number of Accidents")
#####################################################################################################
# Step 28: Checking alcohol related accidents 
#####################################################################################################
counts_1 <- table(crashes$ALCOHOL_RELATED )
View(counts_1)
barplot(counts_1, main="Accidents",
        xlab="Number of Accidents")
#####################################################################################################
# Step 29: Checking police involved accidents 
#####################################################################################################
counts_2 <- table(crashes$POLICE_ATTEND )
View(counts_2)
barplot(counts_2, main="Accidents",
        xlab="Number of Accidents")
#####################################################################################################
# Step 30: Checking hit and run accidents 
#####################################################################################################
counts_3 <- table(crashes$HIT_RUN_FLAG )
View(counts_3)
barplot(counts_3, main="Accidents",
        xlab="Number of Accidents")
#####################################################################################################
# Step 31: Checking run offroad accidents 
#####################################################################################################
counts_4 <- table(crashes$RUN_OFFROAD )
View(counts_4)
barplot(counts_4, main="Accidents",
        xlab="Number of Accidents")