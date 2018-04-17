library(dplyr)
library(magrittr)

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

#gives the overall number of distinct, LASAR-numbered sites in each table
tally(distinct(dtan, lasar_id))
tally(distinct(dta2, lasar_id))

#more site descriptions than lasar_ids in each table
tally(distinct(dtan, site_description))
tally(distinct(dta2, site_description))

#these create lists that show that compare site descriptions and project names to lasar ids
project_names_1 <- dtan%>%
  select(lasar_id, project, site_description)
project_names_2 <- dta2%>%
  select(lasar_id, project, site_description)

#this doesnt work.
#dta2%>%
 #group_by(lasar_id)%>%
  #mutate(new_site_desc = site_description)
