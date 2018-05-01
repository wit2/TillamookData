library(dplyr)
library(stringr)

#Problem: There are two separate lists of data sources for north coast DO that don't match up
#and have lack of consistency within and between
#Objective 1: Clarify inconsistencies so that a non-redundant master list of data sources can be generated

#this is a list that includes TMDL DO sampling outside of the north coast region.
dta1 <- readr::read_csv("T:\\PSU TMDL Status and Trend Study\\North Coast dissolved oxygen - Anna Withington\\CSVs\\Copy of TMDL_CnDataTracking.csv")
#this is a list authored by Adam
dta2 <- readr::read_csv("T:\\PSU TMDL Status and Trend Study\\North Coast dissolved oxygen - Anna Withington\\CSVs\\TillamookContinuousDatasets2012-17_YJ (003).csv")

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


#this is temporary - I wanted to export files to examine them quickly side-by-side in excel
A <- dta2%>%
  select(`year(s)_sampled`, `month(s)_sampled`, project, lasar_id)%>%
  arrange(`year(s)_sampled`, lasar_id)
TMDL <- dta1 %>%
  select(`year(s)_sampled`, `month(s)_sampled`, project, lasar_id)%>%
  arrange(`year(s)_sampled`, lasar_id)
write.csv(A, file = ("C:\\Users\\awithin\\Desktop\\A.csv"))
write.csv(TMDL, file = ("C:\\Users\\awithin\\Desktop\\TMDL.csv"))


#At this step, tables are extracted from the two QAPPs in order to get the formal site descriptions
library(devtools)
install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer"), INSTALL_opts = "--no-multiarch")
library(tabulizer)

tillamooktabs <- extract_tables("\\\\deqhq1\\AWITHIN\\QAPPs\\DEQ07-LAB-0023-QAPP_2012.pdf")
nehalnestutabs <- extract_tables("E:\\QAPPs\\DEQ11-LAB-0022-QAPP_nehaiem2013.pdf")

#creates a dataframe with the site data from the QAPP
tillamooksites <- tillamooktabs[[6]]
#rename column names to make merging easier later
tillamooksites[1, c(1,2,3,4,5,6)] <- c("lasar_id", "site_description", "elevation", "river_mile", "lat", "long")
colnames(tillamooksites) <- tillamooksites [1,]
#removes top row(header) and empty row accidentally created in import
tillamooksites = tillamooksites[-c(1, 8),]
#adds back the deleted site component to the site description
tillamooksites[6,2] <- "Trask River at Netarts Rd. (Hwy 6)"
tillamooksites
#converting to dataframe solves later issue with lasar id being interpreted as character
tillamooksites <- data.frame(tillamooksites)

nsites <- nehalnestutabs[[6]]
nsites[1, c(1,2,3,4,5)] <- c("lasar_id", "site_description", "river_mile", "lat", "long")
nsites
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



glimpse(dta1)
dta1 <- dta1%>%
  mutate_if(is.character, as.factor)
dta2 <- dta2%>%
  mutate_if(is.character, as.factor)
not_tillamook <- anti_join(dta1, tillamooksites, "lasar_id")
neither_till_nor_n <- anti_join(not_tillamook, nsites, "lasar_id")
neither_till_nor_n
#it appears that there is an additional project

join1<- full_join(dta1, dta2, by = c("lasar_id", "project", "month(s)_sampled", "year(s)_sampled", "site_description",
                                     "cn_parameters", "logger_id", "cn_equipment", "audit_data?", "qc_data?", "link_to_data"))

df [!duplicated(df[c(1,4)]),]
no_duplicates <- join1 [!duplicated(join1[c(2,3,5)]),]
no_duplicates <- arrange(no_duplicates, `year(s)_sampled`, lasar_id)
write.csv(no_duplicates, file = ("C:\\Users\\awithin\\Desktop\\no_dupes.csv"))
anti_join(join1, no_duplicates, by = c("lasar_id", "year(s)_sampled", "month(s)_sampled"))
#returns no rows. I'm not sure I'm doing this right to get the results I want.