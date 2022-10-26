agg_var_by_year <- function(tbbl){
  dt <- tbbl%>%
    data.table()
  dt[ , .(value = sum(value)), by = c("date", "variable")]
}
get_sum <- function(tbbl, var){
  round(sum(tbbl[variable == var & date > current_year, value]),-2)
}
get_current <- function(tbbl, var){
  x <- tbbl[tbbl$variable==var & tbbl$date==current_year, "value"]%>%
    pull()
  round(x,-2)
}
get_ff_cagr <- function(tbbl){
  next_year <- tbbl[tbbl$variable=="employment" & tbbl$date==current_year+1, "value"]
  five_years <- tbbl[tbbl$variable=="employment" & tbbl$date==current_year+5, "value"]
  x <- 100*((five_years/next_year)^(.2)-1)%>%
    pull()
  round(x,2)
}
get_sf_cagr <- function(tbbl){
  five_years <- tbbl[tbbl$variable=="employment" & tbbl$date==current_year+6, "value"]
  ten_years <- tbbl[tbbl$variable=="employment" & tbbl$date==current_year+10, "value"]
  x <- 100*((ten_years/five_years)^(.2)-1)%>%
    pull()
  round(x,2)
}

get_ten_cagr <- function(tbbl){
  next_year <- tbbl[tbbl$variable=="employment" & tbbl$date==current_year+1, "value"]
  ten_years <- tbbl[tbbl$variable=="employment" & tbbl$date==current_year+10, "value"]
  x <- 100*((ten_years/next_year)^(.1)-1)%>%
    pull()
  round(x,2)
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
           job_openings_as_share_of_employment = round(job_openings/employment_level, 2),
           expansion_demand=map_dbl(data, get_sum, "expansion_demand"),
           replacement_demand=map_dbl(data, get_sum, "replacement_demand"),
           annual_replacement_rate=round(replacement_demand/employment_level*10, 2)
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

# # this function take a tbbl with two columns, start and finish and returns either start OR a sequence between start and finish.
# fill_range <- function(tbbl){
#   if(is.na(tbbl$finish)){
#     tbbl$start
#   }else{
#     seq(tbbl$start, tbbl$finish)
#   }
# }
#
# #calculates aggregates for Occupation dataframe.
# common_aggregates <- function(tbbl){
#   first_year <- tbbl%>%
#     filter(date == year1)
#   all_other_years <- tbbl%>%
#     filter(date %in% c(as.numeric(year1 + 1):year3))
#   first_year_employment <- sum(first_year$employment)
#   expansion <- sum(all_other_years$expansion_demand)
#   replacement <- sum(all_other_years$replacement_demand)
#   job_openings <- sum(all_other_years$job_openings)
#
#   tibble(`Employment year1` = first_year_employment,
#          `Expansion year1-year3` = expansion,
#          `Replacement year1-year3` = replacement,
#          `Job Openings year1-year3` = job_openings)
# }
#
# # calculates common aggregates, then adds cagrs and annual replacement rate.
# all_aggregates <- function(tbbl){
#   ca <- common_aggregates(tbbl)
#   first_year <- tbbl%>%
#     filter(date == year1)
#   first_year_employment <- sum(first_year$employment)
#   second_year <- tbbl%>%
#     filter(date == year2)
#   second_year_employment <- sum(second_year$employment)
#   third_year <- tbbl%>%
#     filter(date == year3)
#   third_year_employment <- sum(third_year$employment)
#
#   cagr1 <- 100 * ((second_year_employment/first_year_employment)^(1 / 5) - 1)
#   cagr2 <- 100 * ((third_year_employment/second_year_employment)^(1 / 5) - 1)
#   cagr3 <- 100 * ((third_year_employment/first_year_employment)^(1 / 10) - 1)
#
#   ave_replace <- tbbl%>%
#     group_by(date)%>%
#     summarize(employment = sum(employment),
#               replacement_demand = sum(replacement_demand))%>%
#     mutate(replacement_rate = replacement_demand / employment)%>%
#     summarize(mean(replacement_rate)*100)%>%
#     pull()
#
#   bind_cols(ca,
#             `Employment Growth year1-year2` = cagr1,
#             `Employment Growth year2-year3` = cagr2,
#             `Employment Growth year1-year3` = cagr3,
#             `Annual Replacement Rate` = ave_replace
#   )
# }
#
# # aggregates dataframe jobs_employment by geographic_area and some other var
# aggregate_jobs_employment_by <- function(var){
#   jobs_employment%>%
#     group_by(geographic_area, {{ var  }})%>%
#     nest()%>%
#     mutate(aggregated = map(data, all_aggregates))%>%
#     select(-data)%>%
#     unnest(aggregated)%>%
#     pivot_longer(cols = -c(geographic_area, {{  var  }}))%>%
#     rename(variable = name,
#            level_value = {{  var  }})%>%
#     mutate(level = rlang::englue("{{  var  }}"))
# }
#
# get_ten_obs <- function(vec){
#   if_else(is.factor(vec),
#           paste(head(levels(vec), n=10), collapse = ", "),
#           paste(head(vec, n=10), collapse = ", "))
# }
#
# get_levels <- function(vec){
#   length(unique(vec))
# }
#
# # creates summary of a dataframe (columns and column types)
# col_names_type_example <- function(df){
#   cname <- colnames(get(df))
#   ctype <- sapply(get(df), class)
#   clevels <- sapply(get(df), get_levels)
#   cexample <- sapply(get(df), get_ten_obs)
#   tbbl <- tibble(column = cname, type = ctype, levels= clevels, ten_values = cexample)
# }
# nest_to_string <- function(tbbl){
#   tbbl%>%
#     pull()%>%
#     toString()
# }
#
# get_sum <- function(tbbl, var){
#   sum(tbbl[variable==var, value])
# }
#
# get_current <- function(tbbl, var){
#   tbbl[tbbl$variable==var & tbbl$date==current_year, "value"]%>%
#     pull()
# }
#
# get_cagr <- function(tbbl){
#   next_year <- tbbl[tbbl$variable=="employment" & tbbl$date==current_year+1, "value"]
#   ten_years <- tbbl[tbbl$variable=="employment" & tbbl$date==current_year+10, "value"]
#   100*((ten_years/next_year)^(.1)-1)%>%
#     pull()
# }
#
# agg_var_by_year <- function(tbbl){
#   dt <- tbbl%>%
#     data.table()
#   dt[ , .(value = sum(value)), by = c("date", "variable")]
# }
#
#
