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

A <- dta2%>%
  select(`year(s)_sampled`, `month(s)_sampled`, project, lasar_id)%>%
  arrange(`year(s)_sampled`, lasar_id)

TMDL <- dta1_1 %>%
  select(`year(s)_sampled`, `month(s)_sampled`, project, lasar_id)%>%
  arrange(`year(s)_sampled`, lasar_id)

write.csv(A, file = ("C:\\Users\\awithin\\Desktop\\A.csv"))
write.csv(TMDL, file = ("C:\\Users\\awithin\\Desktop\\TMDL.csv"))

library(devtools)
install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer"), INSTALL_opts = "--no-multiarch")
library(tabulizer)

tillamooktabs <- extract_tables("\\\\deqhq1\\AWITHIN\\QAPPs\\DEQ07-LAB-0023-QAPP_2012.pdf")
?extract_tables
nehalnestutabs <- extract_tables("E:\\QAPPs\\DEQ11-LAB-0022-QAPP_nehaiem2013.pdf")
