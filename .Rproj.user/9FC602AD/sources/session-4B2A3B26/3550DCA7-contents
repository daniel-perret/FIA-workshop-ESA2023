#### ESA Portland 2023 - FIA Workshop
#### Bailey McLaughlin
#### Here, we will extract data from the Forest Inventory and Analysis Database to demonstrate how a
#### relational database structure can be operationalized to link together real data for analysis.


### Research Question: Where, in the state of Oregon, are LIVE sualpine fir trees distributed?
### Research Objective: Use FIA data to identify plots within the state of Oregon where live subalpine 
#occurs and was most recently sampled. Create a map to visualize the most recent plot-level 
#occurrences of subalpine fir in Oregon.


## Step 1: Logistics and getting started.

#Clear workspace and clean up memory:
rm(list=ls())
gc(reset=TRUE)

#Load in required libraries:
library(data.table)
library(dplyr)


## Step 2: Download the FIA data tables needed to reach objective.

#Download tables, direct from Forest Service webpage:

#OR_PLOT table contains information about all FIA plots in the state:
#Note: here I pull in a subset of available variables, relevant to the research objective written above, using the 
#'select' function. In the PLOT table, there are up to 72 accessible variables, providing a wealth of information that
#could be relevant! Review the complete table for more detailed information about available data.
OR_PLOT <- fread('http://apps.fs.usda.gov/fia/datamart/CSV/OR_PLOT.CSV', 
                 select=c("CN", "INVYR", "MEASYEAR", "STATECD", 
                          "UNITCD", "COUNTYCD", "PLOT", "LAT", "LON"))

OR_PLOT <- all.fia$PLOT %>% 
  select("CN", "INVYR", "MEASYEAR", "STATECD", 
         "UNITCD", "COUNTYCD", "PLOT", "LAT", "LON")

#OR_TREE table contains caninformation about all trees present on plots within the state: 
#Note: Up to 207 variables available within TREE table, only a subset selected here.
OR_TREE <- fread('http://apps.fs.usda.gov/fia/datamart/CSV/OR_TREE.CSV', 
                 select=c("CN", "PLT_CN", "INVYR",
                          "SUBP", "PLOT", "TREE", "STATUSCD", "SPCD", "DIA"))

OR_TREE <- all.fia$TREE %>% 
  select("CN", "PLT_CN", "INVYR",
         "SUBP", "PLOT", "TREE", "STATUSCD", "SPCD", "DIA")

#REF_SPECIES is an index table that helps identify the trees/seedlings present on plots in the state:
#Note: Up to 79 variables available within REF_SPECIES table, only a subset selected here.
REF_SPECIES <- fread('http://apps.fs.usda.gov/fia/datamart/CSV/REF_SPECIES.CSV', 
                     select=c("SPCD", "COMMON_NAME", "GENUS", "SPECIES", "SPECIES_SYMBOL"))

#Open tables and explore. Look at the structure and the variable names (columns) that were brought in. 


## Step 3: clean the data, to get it ready to be linked for analysis.

#PLOT Table:
#CN in PLOT table corresponds to PLT_CN in other tables, and this is the 'unique identifier' by which tables are joined. 
#So, we will rename the variable to match the variable name in complementary tables.
names(OR_PLOT)[names(OR_PLOT)=="CN"] <- "PLT_CN" 
str(OR_PLOT)

#TREE Table:
#Rename CN field to CN_TREE to identify data table it came from.
names(OR_TREE)[names(OR_TREE)=="CN"] <- "CN_TREE"
str(OR_TREE)

#REF_SPECIES Table:
#Change characters to factors in data table.
REF_SPECIES$COMMON_NAME <- as.factor(REF_SPECIES$COMMON_NAME)
REF_SPECIES$GENUS <- as.factor(REF_SPECIES$GENUS)
REF_SPECIES$SPECIES <- as.factor(REF_SPECIES$SPECIES)
str(REF_SPECIES)


## Step 4: Data Joining, link tables together.

#Join all TREE data:
#Note: We are using the left_join function in the dplyr package.
OR_DATA <- left_join(OR_TREE, OR_PLOT, by=c("PLT_CN", "INVYR", "PLOT"))
OR_DATA <- left_join(OR_DATA, REF_SPECIES, by="SPCD")

#Restrict data to only live trees, STATUSCD==1:
OR_DATA <- subset(OR_DATA, STATUSCD==1)

#Open OR_DATA and explore table. Let's look at what we have included here. Examine the variables/column names. 
#Overall: this table contains all LIVE trees (TREE table) present on all Forest Service plots within Oregon (PLOT 
#table), identifiable to species (REF_SPECIES).


## Step 5: Pull out subalpine fir records.

#FIA SPECIES_SYMBOL code for subalpine fir is 'ABLA':
fir <- OR_DATA[OR_DATA$SPECIES_SYMBOL=="ABLA",]


## Step 6: Identify unique PLOT records in which fir is found.

#Unique PLOTS are identifiable from a combination of variables. So, assign an ID to a unique plot combo, to identify 
#"unique" plots. 
fir.plots <- fir %>% 
  group_by(PLOT, STATECD, UNITCD, COUNTYCD) %>%  
  mutate(ID=cur_group_id())


## Step 7: Find the most recent collection year (or, MEASYEAR) in which subalpine fir was found on a unique plot.

#Pull out the most recent collection year, MEASYEAR, for each unique plot (ID assigned in previous step). 
fir.recent <- fir.plots %>%
  group_by(ID, SPECIES_SYMBOL) %>%
  slice(which.max(MEASYEAR))


## Step 8: Map the data to visualize.

#Get map data:
US <- map_data("state")

#Restruct region to only Oregon, the area of interest:
OR <- US[US$region=="oregon",]

#Create a very simple map using ggplot:
fir_map <- ggplot() +
  geom_polygon(data=OR, aes(x=long, y=lat, group=group), fill = "white", colour = "black") +
  geom_point(data=fir.recent, aes(x=LON, y=LAT), colour="dark green")

fir_map
#There you have it! Please note that this is a very simple map to get a broad sense of the distribution. 
#details, aesthetics, labels, projections, etc. can be added to clean this up and beautify it later. 








