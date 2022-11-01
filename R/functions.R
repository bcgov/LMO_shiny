agg_var_by_year <- function(tbbl){
  dt <- tbbl%>%
    data.table()
  dt[ , .(value = sum(value)), by = c("date", "variable")]
}
get_sum <- function(tbbl, var){
  round(sum(tbbl[variable == var & date > current_year, value]), round_medium)
}
get_current <- function(tbbl, var){
  x <- tbbl[tbbl$variable==var & tbbl$date==current_year, "value"]%>%
    pull()
  round(x, round_medium)
}
get_ff_cagr <- function(tbbl){
  next_year <- tbbl[tbbl$variable=="employment" & tbbl$date==current_year+1, "value"]
  five_years <- tbbl[tbbl$variable=="employment" & tbbl$date==current_year+5, "value"]
  x <- 100*((five_years/next_year)^(.2)-1)%>%
    pull()
  round(x, round_small)
}
get_sf_cagr <- function(tbbl){
  five_years <- tbbl[tbbl$variable=="employment" & tbbl$date==current_year+6, "value"]
  ten_years <- tbbl[tbbl$variable=="employment" & tbbl$date==current_year+10, "value"]
  x <- 100*((ten_years/five_years)^(.2)-1)%>%
    pull()
  round(x, round_small)
}

get_ten_cagr <- function(tbbl){
  next_year <- tbbl[tbbl$variable=="employment" & tbbl$date==current_year+1, "value"]
  ten_years <- tbbl[tbbl$variable=="employment" & tbbl$date==current_year+10, "value"]
  x <- 100*((ten_years/next_year)^(.1)-1)%>%
    pull()
  round(x, round_small)
}

#'jo_raw and emp_raw tibbles are wide and contain unwanted aggregates.
#' e.g. when we are breaking down by noc we want it to be for all industries but we drop the NOC aggregate.
#' when we are breaking down by industry we want it to be for all NOC, but we drop the industry aggregate.
make_long_by <- function(tbbl, var){
  if(var=="noc"){
    tbbl <- tbbl%>%
      filter(Industry=="All industries",
             NOC!="#T")
  } else if(var=="industry"){
    tbbl <- tbbl%>%
      filter(Industry!="All industries",
             NOC=="#T")
   } else {
    stop("var must be either noc or industry")
  }
  tbbl%>%
    pivot_longer(cols=-c(NOC, Industry, Description, Variable, `Geographic Area`),
                 names_to = "date",
                 values_to = "value")%>%
    mutate(date=as.numeric(date))%>%
    clean_tbbl()
}

bind_long_by <- function(tbbl1, tbbl2, var){
  bind_rows(make_long_by(tbbl1, var), make_long_by(tbbl2, var))
}

group_nest_agg <- function(tbbl, var){
  tbbl%>%
    group_by({{  var  }}, geographic_area)%>%
    nest()%>%
    mutate(data=map(data, agg_var_by_year))
}

get_measures <- function(tbbl){
  tbbl <- tbbl%>%
    mutate(job_openings=map_dbl(data, get_sum, "job_openings"),
           employment_level=map_dbl(data, get_current, "employment"),
           first_five_cagr=map_dbl(data, get_ff_cagr),
           second_five_cagr=map_dbl(data, get_sf_cagr),
           ten_year_cagr=map_dbl(data, get_ten_cagr),
           job_openings_as_share_of_employment = round(100*job_openings/employment_level, round_small),
           expansion_demand=map_dbl(data, get_sum, "expansion_demand"),
           replacement_demand=map_dbl(data, get_sum, "replacement_demand"),
           annual_replacement_rate=round(replacement_demand/employment_level*10, round_small)
    )%>%
    select(-data)%>%
    pivot_longer(cols=job_openings:annual_replacement_rate, names_to = "name", values_to = "value")%>%
    camel_to_title()%>%
    rename(Geographic_Area=geographic_area)
}

aggregate_by_noc <- function(tbbl){
  noc1 <- group_nest_agg(tbbl, noc1)%>%
    mutate(noc2=NA,
           noc3=NA,
           noc4=NA)
  noc2 <- group_nest_agg(tbbl, noc2)%>%
    inner_join(two_to_one)%>%
    mutate(noc3=NA,
           noc4=NA)
  noc3 <- group_nest_agg(tbbl, noc3)%>%
    inner_join(three_to_one)%>%
    mutate(noc4=NA)
  noc4 <- group_nest_agg(tbbl, noc4)%>%
    inner_join(noc_mapping)
  bind_rows(noc1, noc2, noc3, noc4)%>%
    get_measures()%>%
    rename(NOC1=noc1,
           NOC2=noc2,
           NOC3=noc3,
           NOC4=noc4)
}

make_title <- function(strng){
  strng <- str_to_title(str_replace_all(strng,"_"," "))
}

fix_col_names <- function(tbbl){
  colnames(tbbl) <- str_to_title(str_replace_all(colnames(tbbl),"_"," "))
  colnames(tbbl) <- str_replace_all(colnames(tbbl),"Noc","NOC")
  tbbl
}

noc_mean_wage <- function(tbbl, noc){
  tbbl%>%
    group_by({{  noc  }})%>%
    summarize(across(contains("wage"), ~round(mean(., na.rm=TRUE), round_small)))
}

get_mean_wages <- function(tbbl){
  noc1 <- noc_mean_wage(tbbl, noc1)%>%
    mutate(noc2=NA,
           noc3=NA,
           noc4=NA)
  noc2 <- noc_mean_wage(tbbl, noc2)%>%
    inner_join(two_to_one)%>%
    mutate(noc3=NA,
           noc4=NA)
  noc3 <- noc_mean_wage(tbbl, noc3)%>%
    inner_join(three_to_one)%>%
    mutate(noc4=NA)
  noc4 <- noc_mean_wage(tbbl, noc4)%>%
    inner_join(noc_mapping)
  bind_rows(noc1, noc2, noc3, noc4)%>%
    rename(NOC1=noc1,
           NOC2=noc2,
           NOC3=noc3,
           NOC4=noc4)
}
