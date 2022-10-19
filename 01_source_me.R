library("data.table")
library("tidyverse")
library("lubridate")
library("here")
library("readxl")
# Functions------------------
source(here::here("R","functions.R"))
# "constants"... that change every year------------
current_year <- 2022
# process data for occupation outlook page--------
jo_raw <- vroom::vroom(here("raw_data",
                        list.files(here("raw_data"), pattern = "JO", ignore.case = TRUE)),
                   locale = readr::locale(encoding = "latin1"),
                   skip=3,
                   col_select = -1)%>%
  filter(Industry=="All industries",
         NOC!="#T")%>%
  select(-Industry)%>%
  pivot_longer(cols=-c(NOC, Description, Variable, `Geographic Area`),
               names_to = "date",
               values_to = "value")%>%
  mutate(date=as.numeric(date))%>%
  clean_tbbl()

employment_raw <- vroom::vroom(here("raw_data",
                                list.files(here("raw_data"), pattern = "Emp")),
                           locale = readr::locale(encoding = "latin1"),
                           skip=3,
                           col_select = -1)%>%
  filter(Industry=="All industries",
         NOC!="#T")%>%
  select(-Industry)%>%
  pivot_longer(cols=-c(NOC, Description, Variable, `Geographic Area`),
               names_to = "date",
               values_to = "value")%>%
  mutate(date=as.numeric(date))%>%
  clean_tbbl()

noc_mapping <- vroom::vroom(here::here("raw_data","noc_mapping.csv"))

long <- bind_rows(jo_raw, employment_raw)%>%
  full_join(noc_mapping)

two_to_one <- unique(noc_mapping[c("noc1","noc2")])
three_to_one <- unique(noc_mapping[c("noc1","noc2","noc3")])

noc1 <- long%>%
  group_by(noc1, geographic_area)%>%
  nest()%>%
  mutate(data=map(data, agg_var_by_year))%>%
  mutate(noc2=NA,
         noc3=NA,
         noc4=NA)

noc2 <- long%>%
  group_by(noc2, geographic_area)%>%
  nest()%>%
  mutate(data=map(data, agg_var_by_year))%>%
  inner_join(two_to_one)%>%
  mutate(noc3=NA,
         noc4=NA)

noc3 <- long%>%
  group_by(noc3, geographic_area)%>%
  nest()%>%
  mutate(data=map(data, agg_var_by_year))%>%
  inner_join(three_to_one)%>%
  mutate(noc4=NA)

noc4 <- long%>%
  group_by(noc4, geographic_area)%>%
  nest()%>%
  mutate(data=map(data, agg_var_by_year))%>%
  inner_join(noc_mapping)

occupation_outlook <- bind_rows(noc1, noc2, noc3, noc4)%>%
  mutate(job_openings=map_dbl(data, get_sum, "job_openings"),
         employment_level=map_dbl(data, get_current, "employment"),
         annual_employment_growth=map_dbl(data, get_cagr),
         job_openings_as_share_of_employment=job_openings/employment_level,
         expansion_demand=map_dbl(data, get_sum, "expansion_demand"),
         replacement_demand=map_dbl(data, get_sum, "replacement_demand")
         )%>%
  select(-data, -noc)%>%
  pivot_longer(cols=job_openings:replacement_demand, names_to = "name", values_to = "value")%>%
  rapply(as.character, classes = "factor", how = "replace")%>%
  tibble()%>%
  mutate(across(where(is.character), make_title))%>%
  rename(Geographic_Area=geographic_area,
         NOC1=noc1,
         NOC2=noc2,
         NOC3=noc3,
         NOC4=noc4)

write_csv(occupation_outlook, here::here("shiny_data","occupation_outlook.csv"))

#highlights part 1---------------
#table
jo_tab <- jo_raw%>%
  filter(geographic_area=="british_columbia",
         date>current_year)%>%
  group_by(variable)%>%
  summarize(value=round(sum(value),-3))%>%
  filter(variable %in% c("job_openings", "expansion_demand","replacement_demand"))%>%
  pivot_wider(names_from = variable, values_from = value)%>%
  mutate(decline_unemployment=round(job_openings*.0189, -3))#WTF???

ds_tab <- vroom::vroom(here("raw_data",
                        list.files(here("raw_data"), pattern = "DS")),
                   locale = readr::locale(encoding = "latin1"),
                   skip=3,
                   col_select = -1)%>%
  pivot_longer(cols=-c(NOC, Description, Industry, Variable, `Geographic Area`),
               names_to = "date",
               values_to = "value")%>%
  clean_tbbl()%>%
  mutate(date=as.numeric(as.character(date)))%>%
  filter(date>current_year,
        noc=="#t",
         geographic_area=="british_columbia",
         variable %in% c("new_entrants",
                         "net_international_in-migration",
                         "net_interregional_in-migration"))%>%
  group_by(variable)%>%
  summarize(value=round(sum(value),-3))%>%
  pivot_wider(names_from = variable, values_from = value)%>%
  rename(young_people_starting_work=new_entrants,
         immigrants=`net_international_in-migration`,
         migrants_from_other_provinces=`net_interregional_in-migration`)

tab <- bind_cols(jo_tab, ds_tab)%>%
    mutate(additional_supply_requirement = job_openings - immigrants - migrants_from_other_provinces - young_people_starting_work- decline_unemployment,
           total_supply_additions = young_people_starting_work+ immigrants + migrants_from_other_provinces+ additional_supply_requirement
           )%>%
  pivot_longer(cols=everything())

jo_tab <- tab%>%
  filter(name %in% c("job_openings","expansion_demand","replacement_demand"))%>%
  arrange(desc(value))%>%
  mutate(percent=scales::percent(2*value/sum(value)),
         name=str_to_title(str_replace_all(name,"_"," "))
         )

ds_tab <- tab%>%
  filter(! name %in% c("job_openings","expansion_demand","replacement_demand"))%>%
  arrange(desc(value))%>%
  mutate(percent=scales::percent(value/pull(tab[tab$name=="job_openings", "value"]), accuracy = 1),
         name=str_to_title(str_replace_all(name,"_"," "))
  )

bind_rows(jo_tab, ds_tab)%>%
  write_csv(here::here("shiny_data","highlights_tab.csv"))
#pie

temp <- read_excel(here::here("raw_data","Education 2022E.xlsx"))



