library(dplyr)
library(purrr)

# ERS
# https://www.ers.usda.gov/data-products/farm-income-and-wealth-statistics/data-files-u-s-and-state-level-farm-income-and-wealth-statistics/

# data was collected from the following site
# https://data.ers.usda.gov/reports.aspx?ID=17830
path <- here::here("data","VA_State_US.xlsx")

tabs <- readxl::excel_sheets(path)
#remove unecessary tab
tabs <- tabs[2:length(tabs)]

#farm income and wealth statistics
iws <- lapply(tabs,function(x) readxl::read_excel(path, sheet = x, skip = 0, col_names = F))%>%
  set_names(nm=tabs)

iws <- map(iws, .f = ~.x %>% janitor::remove_empty(which = c("cols","rows"))%>%
              filter(!(is.na(...1)| stringr::str_detect(string= ...1, pattern = "Footnotes") |
                         stringr::str_detect(string= ...1,pattern = "USDA/ERS Farm Income and Wealth Statistics")|
                         stringr::str_detect(string= ...1,pattern = "Value added to the U.S. economy by the agricultural sector")))%>%
              janitor::row_to_names(row_number = 1)%>%
              rename_with(.cols=1, ~"Field")%>%
              group_by(Field)%>%mutate(n=row_number())%>%ungroup()%>%
              mutate(Field = case_when((n==1 & Field=="Home consumption")~"Home consumption for Crops",
                                      (n==1 & Field=="Inventory adjustment")~"Inventory adjustment for Crops",
                                      (n==2 & Field=="Home consumption")~"Home consumption for Animals",
                                      (n==2 & Field=="Inventory adjustment")~"Inventory adjustment for Animals",
                                     Field == "Net farm income" ~ "Net Income",
                                      TRUE ~ Field))%>%
              select(-n)%>%
              tidyr::pivot_longer(!Field, names_to = "Year", values_to = "Values"))%>%
              bind_rows(.id = "Table")%>%
              mutate(Values = as.numeric(Values) * 1000, #data was originally represented in thousands of dollars
                     Type = substr(Year,5,6),
                     Year = substr(Year,1,4),
                     Type = case_when(Type == "F"~ "Forecast",
                                      TRUE~ "Historical"))
# Tabular Examples of Data ----

iws%>%
  filter(Table == "United States",
         Field == "Electricity")%>%
  Thematic::tabGT(table_title = "Historical and Predicted Values for Electricity Cost",
                  table_subtitle = "Data pertains to the entire United States")

iws%>%
  filter(Table == "Tennessee",
         Field == "Net Income")%>%
  Thematic::tabGT(table_title = "Historical Values for Net Income",
                  table_subtitle = "Data pertains only to Tenessee")
