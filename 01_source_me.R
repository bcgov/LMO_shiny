tictoc::tic()
#libraries------------
library("data.table")
library("tidytable")
library("lubridate")
library("here")
library("readxl")
library("readr")
library("stringr")
library("wrapR")
#constants------------
round_small <- 2 #round to 2 digits
round_medium <- -1 #round to nearest 10
round_large <- -3 #round to nearest 1000
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

typical_education <- read_excel(here::here("raw_data","Occupational Characteristics based on LMO 2022E 2022-Aug.xlsx"), skip=3, sheet = "Characteristics")%>%
  janitor::clean_names()%>%
  select(noc=noc_2016, Typical_Education=typical_education_background_2022_editon)

wages <- read_excel(here::here("raw_data","2021 Wages.xlsx"))%>%
  janitor::clean_names()%>%
  rename(noc=noc_2016)%>%
  mutate(across(contains("wage"), ~if_else(.x > 3000, round(.x/1730,2), .x)))%>% #converts annual salary to hourly wage rate
  mutate(across(contains("wage"), ~ na_if(., 0))) #replaces 0 wage with NA

interests <- read_excel(here::here("raw_data","Occupational Characteristics based on LMO 2022E 2022-Aug.xlsx"),
                        sheet="Characteristics",
                        skip=3)%>%
  janitor::clean_names()%>%
  select(noc=noc_2016,
         interests)

whos_hoo <- read_excel(here::here("raw_data","HOO list 2022E.xlsx"))%>%
  janitor::clean_names()%>%
  mutate(hoo= if_else(high_opportunity_occupation=="Yes", TRUE, FALSE))%>%
  select(-high_opportunity_occupation)

# we need current_year ASAP, so this code is ahead of where it is used in dashboard--------
#'jo_raw and emp_raw tibbles are wide and contain unwanted aggregates.
#' e.g. when we are breaking down by noc we want it to be for all industries but we drop the aggregate NOC.
#' when we are breaking down by industry we want it to be for all NOC, but we drop the all industry aggregate.

long_by_noc <- bind_long_by(jo_raw, employment_raw, "noc")%>%
  select(-industry)
long_by_industry <- bind_long_by(jo_raw, employment_raw, "industry")%>%
  select(-noc,-description)

current_year <- min(long_by_noc$date)

# #ds_and_jo----------------

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
  pivot_longer(cols=-c(date, geographic_area))

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
  summarize(value=round(sum(value), round_large))%>%
  pull(value)

jo_tab <- ds_and_jo%>%
  filter(name %in% c("job_openings","expansion_demand","replacement_demand"),
         geographic_area=="british_columbia")%>%
  group_by(name)%>%
  summarize(value=round(sum(value), round_large))%>%
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
  summarize(value=round(sum(value), round_large))%>%
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
         employment_growth=map_dbl(data, get_ten_cagr)
  )%>%
  select(-data)%>%
  pivot_longer(cols=-geographic_area)

regional <- jo_total_noc%>%
  filter(name %in% c("expansion_demand", "job_openings", "replacement_demand"))%>%
  group_by(geographic_area, name)%>%
  summarize(value=sum(value))%>%
  bind_rows(emp_total_noc)%>%
  bind_rows(bc_reg_pop())%>%
  pivot_wider()%>%
  mutate(employment_growth=employment_growth/100)%>%
  mutate(across(c(expansion_demand, replacement_demand, current_employment), ~ .x/population, .names = "{.col}_per_capita"))%>%
  select(-population)%>%
  pivot_longer(cols=-geographic_area)

write_csv(regional, here::here("shiny_data","regional.csv"))

#industry outlook-------------
industry_mapping <- vroom::vroom(here::here("raw_data","industry_to_agg_mapping.csv"), delim=",")%>%
  distinct()

long_and_industry_mapping <- long_by_industry%>%
  full_join(industry_mapping)

by_aggregate <- group_nest_agg(long_and_industry_mapping, aggregate_industry)%>%
  mutate(industry=NA)

by_industry <- group_nest_agg(long_and_industry_mapping, industry)%>%
  left_join(industry_mapping, multiple = "all")

industry_outlook <- bind_rows(by_aggregate, by_industry)%>%
  get_measures()

write_csv(industry_outlook, here::here("shiny_data","industry_outlook.csv"))

#occupation outlook plot-----------------
noc_mapping <- vroom::vroom(here::here("raw_data","noc_mapping.csv"))

long_and_noc_mapping <- long_by_noc%>%
  full_join(noc_mapping)

two_to_one <- unique(noc_mapping[c("noc1","noc2")])
three_to_one <- unique(noc_mapping[c("noc1","noc2","noc3")])

occupation_outlook_no_ed <- aggregate_by_noc(long_and_noc_mapping)

occupation_outlook <- occupation_outlook_no_ed%>%
  left_join(typical_education)%>%
  select(-noc)

write_csv(occupation_outlook, here::here("shiny_data","occupation_outlook.csv"))

#occupation outlook table------------------------
#' this was tricky... typical education only provided at 4 digit level... So we partition the data by typical
#' education and then do the aggregation by noc for each of the typical education levels.


occupation_outlook_table <- long_and_noc_mapping%>%
  left_join(typical_education)%>%
  group_by(`Typical_Education`)%>%
  nest()%>%
  mutate(data=map(data, aggregate_by_noc))%>%
  unnest(data)

oot_no_noc <- occupation_outlook_table%>%
  select(-noc)

write_csv(oot_no_noc, here::here("shiny_data","occupation_outlook_table.csv"))

# Job openings 500 occupations------------

wages_and_interests <- wages%>%
  left_join(noc_mapping)%>%
  left_join(typical_education)%>%
  group_by(`Typical_Education`)%>%
  nest()%>%
  mutate(data=map(data, get_mean_wages))%>%
  unnest(data)%>%
  mutate(number_noc=word(noc, 1, sep = "_"))%>%
  left_join(interests, by=c("number_noc"="noc"))%>%
  mutate(noc = case_when(is.na(noc) & is.na(NOC4) & is.na(NOC3) & is.na(NOC2) ~NOC1,
                           is.na(noc) & is.na(NOC4) & is.na(NOC3) ~ NOC2,
                           is.na(noc) & is.na(NOC4) ~ NOC3,
                           TRUE ~ NOC4),
           noc = word(noc, 1, sep = "_"))%>%
  select(-number_noc)


just_jo <- occupation_outlook_table%>%
  filter(name=="Job Openings")%>%
  mutate(noc = case_when(is.na(noc) & is.na(NOC4) & is.na(NOC3) & is.na(NOC2) ~NOC1,
                         is.na(noc) & is.na(NOC4) & is.na(NOC3) ~ NOC2,
                         is.na(noc) & is.na(NOC4) ~ NOC3,
                         TRUE ~ NOC4),
         noc = word(noc, 1, sep = " "))


jo_500 <- just_jo%>%
  left_join(wages_and_interests, by=c("noc"="noc","Typical_Education"="Typical_Education"))%>%
  select(-ends_with(".y"))%>%
  rename(NOC1=NOC1.x,
         NOC2=NOC2.x,
         NOC3=NOC3.x,
         NOC4=NOC4.x)%>%
  left_join(whos_hoo,
            by=c("noc"="noc",
                 "Geographic_Area"="geographic_area")
            )%>%
  select(-noc)

write_csv(jo_500, here::here("shiny_data","jo_500.csv"))

tictoc::toc()


