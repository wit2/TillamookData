library(dplyr)

#Problem: There are two separate lists of data sources for north coast DO that don't match up
#and have lack of consistency within and between that is confusing
#Objective 1: Clarify inconsistencies so that a non-redundant master list of data sources can be generated

#this is a list that includes TMDL DO sampling outside of the north coast region.
dta1 <- readr::read_csv("T:\\PSU TMDL Status and Trend Study\\North Coast dissolved oxygen - Anna Withington\\CSVs\\Copy of TMDL_CnDataTracking.csv")
#this is a list authored by Adam that is only north coast
dta2 <- readr::read_csv("T:\\PSU TMDL Status and Trend Study\\North Coast dissolved oxygen - Anna Withington\\CSVs\\TillamookContinuousDatasets2012-17_YJ (003).csv")

#remove spaces from column names
names(dta1) <- gsub(" ", "_", names(dta1))
names(dta2) <- gsub(" ", "_", names(dta2))

#put all column names in lowercase
dta1 <- select_all(dta1, tolower)
dta2 <- select_all(dta2, tolower)

#removes sites that are outside of the North Coast Basin
dtan <- dta1%>%
  filter(!project %in% c("Yaquina River DO", "Upper Deschutes", "Tenmile", "Little Deschutes", "Siletz River DO",
                         "Powder", "Flat Creek DO", "Long Tom River", "Salmon River DO"))

#gives the overall number of distinct, LASAR-numbered sites in each table.
#Where different, indicates sampling sites not represented in one table
count(distinct(dtan, lasar_id))
count(distinct(dta2, lasar_id))

#more site descriptions than lasar_ids in both tables
count(distinct(dtan, site_description))
count(distinct(dta2, site_description))

#these create lists that show that compare site descriptions and project names to lasar ids
project_names_1 <- dtan%>%
  select(lasar_id, project, site_description)
project_names_2 <- dta2%>%
  select(lasar_id, project, site_description)

#This is a partial solution. NAs are excluded from this dataframe; they will have to be added back in.
#The rest of the lasar IDs now only have one site description each.
dta2_2 <- dta2%>%
  filter(!is.na(lasar_id))%>%
    group_by(lasar_id)%>%
     mutate(site_desc = first(site_description))%>%
      arrange(lasar_id)

#this displays the old site description and the new site description for each lasar ID to check for any major discrepancies.
dta2_2 %>%
  select(lasar_id, site_description, site_desc)
