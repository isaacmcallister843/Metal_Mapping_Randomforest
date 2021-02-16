#Setting directory
setwd("C:/Users/dmcallister/Desktop/Chidliak_Processed/Spreadsheets/CSV")

#Libraries

#Inputing dataset
chid <- read.csv("Chidliak_Complete_Dat_R.csv",na.strings = c(""))
soil <- read.csv("Chidliak_soil_wide.csv",na.strings = c(""))
#Parshing dataset
#Removing the all NA rows and columns that are in the csv.
chid_mod <- chid[(3:(nrow(chid)-2)), c(1,2,3,12,24:92) ]

# So much data we cant use since there is very little in what is complete
#Renaming columns
colnames(chid_mod)[c(1,4)] <- c("Sample","pH")

#For this dataset we are only intersted in the correlation between vegetation and soil 
#Filtering by only complete rows
chid_mod <- chid_mod[complete.cases(chid_mod),]

#Reseting rownames
rownames(chid_mod) <- 1:nrow(chid_mod)

#Removing all the < and convert all the chars to numeric

for(i in 4:ncol(chid_mod)){
  chid_mod[,i] <- gsub("<","",chid_mod[,i])
  chid_mod[,i] <- as.numeric(chid_mod[,i])
}


#Converting to factor type
chid_mod$Sample <-  as.character(chid_mod$Sample)
chid_mod$Depth <- as.character(chid_mod$Depth)
chid_mod$Type <-  as.character(chid_mod$Type)

# -------- Grabbing the soil data
soil_copy <- soil[,c(1,5:10)]

new_vec = data.frame()

for (i in 1:nrow(chid_mod)){
  for (j in 1:nrow(soil_copy)){
    if (chid_mod$Sample[i] == soil_copy$plot[j]){
      new_vec = rbind(new_vec, soil_copy[j,])
    }
  }
}

chid_final <- cbind(new_vec[, 2:ncol(new_vec)], chid_mod)
chid_final <- chid_final[, c(7,5,6,1:5,8:ncol(chid_final))]
chid_final <- chid_final[,-c(8,10)]
write.csv(chid_final, "samples_processed.csv")

# ------------------ Hall Samples
# ---- Data Importing 
Hall_1 <- read.csv("Hall Samples for Mapping_1.csv", na.strings = c(""))
# Hall_2 is going to be used for the lat and long values
Hall_2 <- read.csv("Hall Samples for Mapping_2.csv", na.strings = c(""))

# --- Clean up
Hall_1_mod <- Hall_1[, c(2,16:130)]
Hall_1_mod <- Hall_1_mod[Hall_1_mod$INDEX_UNIQUE_ID != 'n/a', ]
Hall_1_mod <- Hall_1_mod[complete.cases(Hall_1_mod$Al2O3_.), ]

# --- Remove all completely blank columns
remove_vec = c()
for (i in 1:ncol(Hall_1_mod)){
  if (!(FALSE %in% is.na(Hall_1_mod[,i]))){
    remove_vec = c(remove_vec, i)
  }
}

Hall_1_mod <- Hall_1_mod[, -remove_vec]

# --- New Data frame for just soil concentrations
names = c()
for (i in 1:length(colnames(Hall_1_mod))){
  if ("PPM." %in% strsplit(colnames(Hall_1_mod)[i], "_")[[1]]){
    names = c(names, colnames(Hall_1_mod)[i])
  }
}

ppm_df <- Hall_1_mod[, c("INDEX_UNIQUE_ID", names)]


# --- Adding Lat and Long Values 
# This is such a bad way to - do this, there is definitely a way easier method.
# It runs fast. In the future maybe try match function 

# Go through the index list and remove the PFG.SSD., we are just left with one number that 
# we will use to compare indexes and order the lists
ppm_df$ind <- as.numeric(gsub("PGD.SSD.", "",ppm_df$INDEX_UNIQUE_ID))

# Repeat for Hall_2
Hall_2$ind <- as.numeric(gsub("PGD.SSD.", "",Hall_2$UNIQUE_ID))

# ----- Combining sets
# Now we have columns with a numeric indicator for each observation 
# Order by the indicator (ind) and then get the lat and long values
Hall_2 <-  Hall_2[order(Hall_2$ind),]
lat_long <- Hall_2[,c("ind", "Latitude", "Longitude")]

# Order the soil dataset
ppm_df <- ppm_df[order(ppm_df$ind),]
ppm_df_mod  <- ppm_df[2:nrow(ppm_df),]

# Cut down the dataset. We are assuming they have the same values and we arent missing any samples. 
# loop finds out when they are equal then breaks the loop
for (i in 1:length(ppm_df_mod$ind)){
  if(ppm_df_mod$ind[i] == lat_long$ind[i]){
    break
  }
}
convert <- lat_long[c(i:length(ppm_df_mod$ind)),]

# Copy the lat and longs
ppm_df_mod$lat <- convert$Latitude
ppm_df_mod$long <- convert$Longitude

# Reorder and remove the ind values, we are done
ppm_df_mod <- ppm_df_mod[, c(1, 49, 50, 2:47)]

# ----- Final Processing 
for (k in 2:ncol(ppm_df_mod)){
  ppm_df_mod[,k] <- as.numeric(gsub("<","", ppm_df_mod[,k]))
}
rownames(ppm_df_mod) <- 1:nrow(ppm_df_mod)
# Writing to a csv

write.csv(ppm_df_mod, "Hallsoil_ppm_lat_long.csv")





