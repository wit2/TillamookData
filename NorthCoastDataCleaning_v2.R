#Author: Anna Withington
#Contact: anna.withington@gmail.com
#Date: May 3, 2018
#Topic: Merge lists of DO data, create master list

#Problem: There are two separate spreadsheets of data sources for North Coast DO that
#have lack of consistency within and between
#Objective 1: Clarify inconsistencies so that a non-redundant master list of data sources can be generated

require(dplyr)
require(stringr)
require(devtools)
install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer"), INSTALL_opts = "--no-multiarch")
require(tabulizer)
require(lubridate)

# Reading data----
#this is a list that includes TMDL DO sampling outside of the north coast region.
dta1 <- readr::read_csv("\\\\deqhq1\\tmdl\\PSU TMDL Status and Trend Study\\North Coast dissolved oxygen - Anna Withington\\CSVs\\Copy of TMDL_CnDataTracking.csv")
#this is a list specific to the North Coast
dta2 <- readr::read_csv("\\\\deqhq1\\tmdl\\PSU TMDL Status and Trend Study\\North Coast dissolved oxygen - Anna Withington\\CSVs\\TillamookContinuousDatasets2012-17_YJ (003).csv")


# Cleaning data----
#remove spaces from column names
names(dta1) <- gsub(" ", "_", names(dta1))
names(dta2) <- gsub(" ", "_", names(dta2))

#put all column names in lowercase
dta1 <- select_all(dta1, tolower)
dta2 <- select_all(dta2, tolower)

#removes pesky ? to convert to NA
dta1[][dta1[] == "?"] <- NA
dta2[][dta2[] == "?"] <- NA

#remove dashes for easier matching later
dta1$`month(s)_sampled` <- gsub("-", " ", dta1$`month(s)_sampled`)
dta2$`month(s)_sampled` <- gsub("-", " ", dta2$`month(s)_sampled`)

#removes sites that are outside of the North Coast Basin
dta1 <- dta1%>%
  filter(!project %in% c("Yaquina River DO", "Upper Deschutes", "Tenmile", "Little Deschutes", "Siletz River DO",
                         "Powder", "Flat Creek DO", "Long Tom River", "Salmon River DO", "South Coast Subbasin Temperature"))

#retains sites that have DO information
dta1 <- dta1 %>%
  filter(str_detect(cn_parameters, "DO"))
dta2 <- dta2%>%
  filter(str_detect(cn_parameters, "DO"))


# Brings in site data from Tillamook Estuary Sloughs DO 2012 QAPP and Nehalem and Nestucca River Basins TMDL DO 2013 QAPP----
tillamooktabs <- extract_tables("\\\\deqhq1\\AWITHIN\\QAPPs\\DEQ07-LAB-0023-QAPP_2012.pdf")
nehalnestutabs <- extract_tables("\\\\deqhq1\\AWITHIN\\QAPPs\\DEQ11-LAB-0022-QAPP_nehaiem2013.pdf")

#creates a dataframe with the site data from the QAPP
tillamooksites <- tillamooktabs[[6]]
#rename column names to make merging easier later
tillamooksites[1, c(1,2,3,4,5,6)] <- c("lasar_id", "site_description", "elevation", "river_mile", "lat", "long")
colnames(tillamooksites) <- tillamooksites [1,]
#removes top row(header) and empty row accidentally created in import
tillamooksites = tillamooksites[-c(1, 8),]
#adds back the deleted site component to the site description
tillamooksites[6,2] <- "Trask River at Netarts Rd. (Hwy 6)"
#converting to dataframe solves later issue with lasar id being interpreted as character
tillamooksites <- data.frame(tillamooksites)

#same process as above, for the nehalem/nestucca QAPP site table
nsites <- nehalnestutabs[[6]]
nsites[1, c(1,2,3,4,5)] <- c("lasar_id", "site_description", "river_mile", "lat", "long")
colnames(nsites) <- nsites[1,]
nsites = nsites[-1,]
nsites[2,2] <- "Nehalem River at Roy Creek Campground"
nsites[9,2] <- "Nestucca River at first bridge ramp u/s of Beaver"
nsites[11,2] <- "Nestucca at Rocky Bend Campground"
nsites[15,2] <- "Nestucca River at Glen Alder Campground"
#reassigns out of place lat/long
nsites[11,c(4,5)] <- nsites[13, c(4,5)]
#deletes empty rows
nsites <- nsites[-c(1,8,10,12,13,14),]
nsites <- data.frame(nsites)


# This step identifies entries with no LASAR IDs or LASAR IDs in neither the Tillamook nor Nehalem/Nestucca projects----
dta1 <- dta1 %>%
  mutate_if(is.character, as.factor)
dta2 <- dta2 %>%
  mutate_if(is.character, as.factor)
not_tillamook <- anti_join(dta1, tillamooksites, "lasar_id")
neither_till_nor_n <- anti_join(not_tillamook, nsites, "lasar_id")
neither_till_nor_n
#it appears that there is an additional project, indicated by LASAR IDs 38603, 38604 38598 etc that are not in either existing QAPP
#this table also contains data that does not have a LASAR ID, which may be impossible to resolve/use data from


# Step to join tables----
#joins the two tables. keeps all observations from both tables. overlaps matching columns.
join1<- full_join(dta1, dta2, by = c("lasar_id", "project", "month(s)_sampled", "year(s)_sampled", "site_description",
                                     "cn_parameters", "logger_id", "cn_equipment", "audit_data?", "qc_data?", "link_to_data"))

# Step to remove duplicates----

#in order to be removed from the dataset, the entry must be duplicated across the Month, Year, Lasar ID, logger ID, AND link to data. 
# 81 observations were not duplicated accross these columns and added to the new dataframe
no_duplicates <- join1[!duplicated(join1[c(2,3,5,8,10)]),]

# 43 duplicated observations were removed.
#This will be retained in a separate table so observations can be double checked and no data will be lost.
removed <- join1 [duplicated(join1[c(2,3,5,8,10)]),]

union(join1, no_duplicates)
#returns 121, indicating three complete duplicates across both tables


# This step generates a project name based on LASAR ID----
new_project_name <- no_duplicates %>%
  mutate(formal_project = 
           ifelse(lasar_id %in% tillamooksites$lasar_id,
                  "Tillamook Estuary Sloughs DO 2012",
                  ifelse(lasar_id %in% nsites$lasar_id, "Nehalem and Nestucca River Basins TMDL DO 2013",
                         "unknown")))

#rearranges the columns so the new project name can be inspected and compared more easily
col_order <- c("year(s)_sampled", "month(s)_sampled", "project", "formal_project", "lasar_id", "site_description", "cn_parameters",
               "link_to_data", "cn_equipment", "sub_id", "logger_id", "audit_data?", "qc_data?", "#1_coc_submitted?", "#2_data_processed?", "#3_loaded_into_awqms?",
               "#4_released_from_element?", "notes")
new_project_name_ordered <- new_project_name[, col_order]


#Reassign spring summer to april october
new_project_name_ordered$`month(s)_sampled`[new_project_name_ordered$`month(s)_sampled` == "Spring Summer Fall"] <- "April October"


#adds month sampling started column
new_month_start <- new_project_name_ordered %>%
  mutate(month_sample_start = word(new_project_name_ordered$`month(s)_sampled`, 1))
new_month_start <- data.frame(new_month_start)

#hmm not working
new_month_start$month_sample_start <- as.Date(new_month_start$month_sample_start, "%B")
glimpse(new_month_start)


#adds month sampling ended column
new_month_end <- new_month_start %>%
  mutate(month_sample_end = word(new_month_start$`month(s)_sampled`, -1))


new_unique_id <- new_month_end%>%
  mutate(ID = 
           ifelse(lasar_id %in% tillamooksites$lasar_id,
                  "Tillamook Estuary Sloughs DO 2012",
                  ifelse(lasar_id %in% nsites$lasar_id, "Nehalem and Nestucca River Basins TMDL DO 2013",
                         "unknown")))



write.csv(new_project_name_ordered, file = ("C:\\Users\\awithin\\Desktop\\masterlist.csv"))
