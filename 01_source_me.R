tictoc::tic()
library("data.table")
library("tidyverse")
library("lubridate")
library("here")
library("readxl")
library("wrapR")
# Functions------------------
source(here::here("R","functions.R"))
#read in dataframes--------------
jo_raw <- vroom::vroom(here("raw_data",
                            list.files(here("raw_data"), pattern = "JO", ignore.case = TRUE)),
                       locale = readr::locale(encoding = "latin1"),
                       skip=3,
                       col_select = -1)

employment_raw <- vroom::vroom(here("raw_data",
                                    list.files(here("raw_data"), pattern = "Emp")),
                               locale = readr::locale(encoding = "latin1"),
                               skip=3,
                               col_select = -1)

ds_raw <- vroom::vroom(here("raw_data",
                        list.files(here("raw_data"), pattern = "DS")),
                   locale = readr::locale(encoding = "latin1"),
                   skip=3,
                   col_select = -1)


# occupation outlook--------

jo_not_total_noc <-jo_raw%>%
  filter(Industry=="All industries",
         NOC!="#T")%>%
  select(-Industry)%>%
  pivot_longer(cols=-c(NOC, Description, Variable, `Geographic Area`),
               names_to = "date",
               values_to = "value")%>%
  mutate(date=as.numeric(date))%>%
  clean_tbbl()

current_year <- min(jo_not_total_noc$date)

emp_not_total_noc <- employment_raw%>%
  filter(Industry=="All industries",
         NOC!="#T")%>%
  select(-Industry)%>%
  pivot_longer(cols=-c(NOC, Description, Variable, `Geographic Area`),
               names_to = "date",
               values_to = "value")%>%
  mutate(date=as.numeric(date))%>%
  clean_tbbl()

noc_mapping <- vroom::vroom(here::here("raw_data","noc_mapping.csv"))

long <- bind_rows(jo_not_total_noc, emp_not_total_noc)%>%
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
  camel_to_title()%>%
  rename(Geographic_Area=geographic_area,
         NOC1=noc1,
         NOC2=noc2,
         NOC3=noc3,
         NOC4=noc4)

write_csv(occupation_outlook, here::here("shiny_data","occupation_outlook.csv"))

#ds_and_jo----------------

jo_total_noc <- jo_raw%>%
  filter(Industry=="All industries",
         NOC=="#T")%>%
  select(-Industry)%>%
  pivot_longer(cols=-c(NOC, Description, Variable, `Geographic Area`),
               names_to = "date",
               values_to = "value")%>%
  mutate(date=as.numeric(date))%>%
  clean_tbbl()%>%
  filter(date>current_year)%>%
  filter(variable %in% c("job_openings", "expansion_demand","replacement_demand"))%>%
  pivot_wider(names_from = variable, values_from = value)%>%
#  mutate(decline_unemployment=round(job_openings*.0189, -3))%>% #WTF???
  select(-noc,-description)%>%
  pivot_longer(cols=-c(date, geographic_area))

ds_total_noc <- ds_raw%>%
  pivot_longer(cols=-c(NOC, Description, Industry, Variable, `Geographic Area`),
               names_to = "date",
               values_to = "value")%>%
  clean_tbbl()%>%
  mutate(date=as.numeric(as.character(date)))%>%
  filter(date>current_year,
         noc=="#t",
         variable %in% c("deaths",
                         "retirements",
                         "new_entrants",
                         "net_international_in-migration",
                         "net_interregional_in-migration"))%>%
  pivot_wider(names_from = variable, values_from = value)%>%
  rename(young_people_starting_work=new_entrants,
         immigrants=`net_international_in-migration`,
         migrants_from_other_provinces=`net_interregional_in-migration`)%>%
  select(-noc, -description, -industry)%>%
  pivot_longer(cols=-c(date,geographic_area))

ds_and_jo <- bind_rows(jo_total_noc, ds_total_noc)%>%
  pivot_wider(names_from = name, values_from = value)%>%
  mutate(additional_supply_requirement = job_openings - immigrants - migrants_from_other_provinces - young_people_starting_work,
         labour_force_exits = -1 * (deaths + retirements),
         total_supply_additions = young_people_starting_work+ immigrants + migrants_from_other_provinces+ additional_supply_requirement
  )%>%
  pivot_longer(cols=-c(date, geographic_area))

ds_and_jo%>%
  camel_to_title()%>%
  write_csv(here::here("shiny_data","ds_and_jo.csv"))

jo_total <- jo_total_noc%>%
  filter(geographic_area=="british_columbia",
         name=="job_openings",
         date>current_year)%>%
  summarize(value=round(sum(value),-3))%>%
  pull(value)

jo_tab <- ds_and_jo%>%
  filter(name %in% c("job_openings","expansion_demand","replacement_demand"),
         geographic_area=="british_columbia")%>%
  group_by(name)%>%
  summarize(value=round(sum(value), -3))%>%
  arrange(desc(value))%>%
  mutate(percent=scales::percent(2*value/sum(value)),
         name=str_to_title(str_replace_all(name,"_"," "))
  )

ds_tab <- ds_and_jo%>%
  filter(name %in% c("total_supply_additions",
                     "young_people_starting_work",
                     "immigrants",
                     "migrants_from_other_provinces",
                     "additional_supply_requirement"),
         geographic_area=="british_columbia")%>%
  group_by(name)%>%
  summarize(value=round(sum(value), -3))%>%
  arrange(desc(value))%>%
  mutate(percent=scales::percent(value/jo_total, accuracy = 1),
         name=str_to_title(str_replace_all(name,"_"," "))
  )

bind_rows(jo_tab, ds_tab)%>%
  write_csv(here::here("shiny_data","ds_and_jo_tab.csv"))

#regional page------------

emp_total_noc <- employment_raw%>%
  filter(Industry=="All industries",
         NOC=="#T")%>%
  select(-Industry)%>%
  pivot_longer(cols=-c(NOC, Description, Variable, `Geographic Area`),
               names_to = "date",
               values_to = "value")%>%
  mutate(date=as.numeric(date))%>%
  clean_tbbl()%>%
  select(-noc,-description)%>%
  group_by(geographic_area)%>%
  nest()%>%
  mutate(current_employment=map_dbl(data, get_current, "employment"),
         employment_growth=map_dbl(data, get_cagr)
         )%>%
  select(-data)%>%
  pivot_longer(cols=-geographic_area)

regional <- jo_total_noc%>%
  filter(name %in% c("expansion_demand", "job_openings", "replacement_demand"))%>%
  group_by(geographic_area, name)%>%
  summarize(value=sum(value))%>%
  bind_rows(emp_total_noc)%>%
  bind_rows(aest::aest_bc_reg_pop())%>%
  pivot_wider()%>%
  mutate(across(expansion_demand:current_employment, ~ .x/population, .names = "{.col}_per_capita"))%>%
  select(-population)%>%
  pivot_longer(cols=-geographic_area)

write_csv(regional, here::here("shiny_data","regional.csv"))
tictoc::toc()


