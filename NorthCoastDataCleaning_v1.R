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

#removes sites that are outside of the North Coast Basin
dta1_1 <- dta1%>%
  filter(!project %in% c("Yaquina River DO", "Upper Deschutes", "Tenmile", "Little Deschutes", "Siletz River DO",
                         "Powder", "Flat Creek DO", "Long Tom River", "Salmon River DO", "South Coast Subbasin Temperature"))

#retains sites that have DO information
dta1_1 <- dta1_1 %>%
  filter(str_detect(cn_parameters, "DO"))
dta2 <- dta2%>%
  filter(str_detect(cn_parameters, "DO"))

#gives the overall number of distinct, LASAR-numbered sites in each table.
#Where different, indicates sampling sites not represented in one table
count(distinct(dta1_1, lasar_id))
count(distinct(dta1_1, site_description))
count(distinct(dta2, lasar_id))
count(distinct(dta2, site_description))
#if more site descriptions than lasar_ids, some sites are improperly described.


#these create lists that show that compare site descriptions and project names to lasar ids
project_names_1 <- dta1_1%>%
  select(lasar_id, project, site_description)%>%
  arrange(lasar_id)
project_names_2 <- dta2%>%
  select(lasar_id, project, site_description)%>%
  arrange(lasar_id)

#This is a partial solution; it renames IDs by the first entry. I'd like to rename by the correct QAPP-listed ID.
#NAs are excluded from this dataframe; they will have to be added back in.
#The rest of the lasar IDs now only have one site description each.
dta2_2a <- dta2%>%
  filter(!is.na(lasar_id))%>%
    group_by(lasar_id)%>%
     mutate(site_desc = first(site_description))%>%
      arrange(lasar_id)

#this is a really hacky way of doing this but I wanted to get a full data table with it working right away.
dta2_2b <- dta2%>%
  filter(is.na(lasar_id))%>%
    mutate(site_desc = site_description)

dta2_2 <- full_join(dta2_2a, dta2_2b)

#this displays the old site description and the new site description for each lasar ID to check for any major discrepancies.
dta2_2 %>%
  select(lasar_id, site_description, site_desc)

A <- dta2%>%
  select(`year(s)_sampled`, `month(s)_sampled`, project, lasar_id)%>%
  arrange(`year(s)_sampled`, lasar_id)

TMDL <- dta1_1 %>%
  select(`year(s)_sampled`, `month(s)_sampled`, project, lasar_id)%>%
  arrange(`year(s)_sampled`, lasar_id)

write.csv(A, file = ("C:\\Users\\awithin\\Desktop\\A.csv"))
write.csv(TMDL, file = ("C:\\Users\\awithin\\Desktop\\TMDL.csv"))
