#' Gender composition by agency title names and title codes
#'
#' @description
#' The output is a table of all the civil service titles and civil service title codes in a specified agency, and their corresponding gender makeup.
#'
#' @details
#' TASKS that use this:
#' \itemize{
#' \item FDNY Salaries by Gender Composition
#' \item NYPD Salaries by Gender Composition
#' \item DOITT Salaries by Gender Composition
#' \item DSNY Salaries by Gender Composition
#' }
#'
#' NOTE: This function will ONLY work if the data input is generated by \code{payeqfunctions::clean_data()} due to the naming scheme.
#'
#'
#' @import dplyr
#' @importFrom tidyr spread
#'
#' @param data dataframe. Pass the full or subset of the cleaned dataset using \code{payeqfunctions::clean_data()} to this
#' @param interested_agency string. Full name of the agency as it shows in the dataset
#' @param exportable boolean. Option to generate a table excluding all rows with < 5 gender in the title. Default is \code{FALSE}
#' @return Dataframe of gender composition per title at the specified agency
#' @export
#' @examples
#' \dontrun{
#' # FDNY Salaries by Gender Composition
#' gender_summary(data = cleaned_dataset, interested_agency = "FIRE DEPARTMENT")
#' # DSNY Salaries by Gender Composition w/ exportable option
#' gender_summary(data = cleaned_dataset, interested_agency = "DEPARTMENT OF SANITATION", exportable = TRUE)
#' }

gender_summary <- function(data, interested_agency, exportable = FALSE){
  # Suppress summarise info
  options(dplyr.summarise.inform = FALSE)

  temp_data <- data %>%
    filter(agency == interested_agency)
  #%>%mutate(title_name_code = paste0(civil_service_title_name," (",civil_service_title_code,")"))

  temp_gender <- temp_data %>%
    group_by(civil_service_title_name, civil_service_title_code, gender) %>%
    summarise(num_gender = n()) %>%
    spread(key = gender, value = num_gender) %>%
    replace(is.na(.),0) %>%
    ungroup %>%
    mutate(perc_Male = Male/rowSums(select(.,-c(civil_service_title_name, civil_service_title_code))),
           perc_Female = Female/rowSums(select(.,-c(civil_service_title_name, civil_service_title_code))))

  output <- temp_data %>%
    group_by(civil_service_title_name, civil_service_title_code) %>%
    summarise(med_salary = median(base_salary),
              mean_salary = mean(base_salary),
              total_count = n()) %>%
    filter(total_count >= 5) %>%
    left_join(temp_gender, by = c("civil_service_title_name", "civil_service_title_code")) %>%
    mutate(across(where(is.numeric),round,2)) %>%
    arrange(desc(med_salary))

  if(exportable){
    output <- output %>%
      select(civil_service_title_name, civil_service_title_code,med_salary,mean_salary,Male,Female) %>%
      filter(Male > 4 & Female > 4)
  }

  return(output)
}

#' Race / Ethnicity composition
#'
#' @description
#' This is compilation of code that does a race_eth breakdown.
#'
#' @details
#' NOTE: This function will ONLY work if the data input is generated by \code{payeqfunctions::clean_data()} due to the naming scheme.
#'
#' TASKS that use this:
#' \itemize{
#' \item Race/Ethnicity Breakdown for Lowest-Paying Civil Service Titles
#' \item Race/Ethnicity Breakdown for Highest-Paying Civil Service Titles
#' }
#'
#' @import dplyr
#' @importFrom tidyr spread
#'
#' @param data string. Pass the full or subset of the cleaned dataset using \code{payeqfunctions::clean_data()} to this
#' @param interested_agency string. Full name of the agency as it shows in the dataset
#' @param high_low string. Option for the highest paid or lowest paid titles. Default is \code{"NULL"}, and accepts \code{"high"} and \code{"low"} for filtering to the highest and lowest paid titles respectively
#' @param spread_format string. Option to have the output wide or long. Default is \code{"wide"}, and accepts \code{"long"} for wide or long data respectively
#' @return Dataframe of race / ethnicity composition per title at the specified agency
#' @export
#' @examples
#' \dontrun{
#' # Race/Ethnicity Breakdown for Lowest-Paying Civil Service Titles
#' race_eth_summary(data = cleaned_dataset, high_low = "low", spread_format = "long")
#' # Race/Ethnicity Breakdown for Highest-Paying Civil Service Titles
#' race_eth_summary(data = cleaned_dataset, high_low = "high", spread_format = "long")
#' # Race/Ethnicity Breakdown by Title in an Agency
#' race_eth_summary(data = cleaned_dataset, interested_agency = "FIRE DEPARTMENT")
#' # DOP Salaries and Race
#' race_eth_summary(data = cleaned_dataset, interested_agency = "DEPARTMENT OF PROBATION",spread_format = "wide")
#' }

race_eth_summary <- function(data, interested_agency = NULL, high_low = NULL, spread_format = "wide", exportable = FALSE){
  # Suppress summarise info
  options(dplyr.summarise.inform = FALSE)

  if(!is.null(interested_agency)){
    temp_data <- data %>%
      filter(agency == interested_agency)
    } else{
    temp_data <- data
  }

  temp_race_eth <- temp_data %>%
    group_by(civil_service_title_name, civil_service_title_code, race_eth) %>%
    {if(spread_format == "wide")
    summarise(.,num_race_eth = n()) %>%
        spread(key = race_eth, value = num_race_eth) else
          summarise(., num_race_eth = n(),
                    med_salary = median(base_salary),
                    mean_salary = mean(base_salary))} %>%
    mutate(perc_race_eth = num_race_eth/sum(num_race_eth)) %>%
    replace(is.na(.),0)

  temp_white_nonwhite <- temp_data %>%
    group_by(civil_service_title_name, civil_service_title_code, nonwhite) %>%
    {if(spread_format == "wide")
      summarise(.,num_nonwhite = n()) %>%
      spread(.,key = nonwhite, value = num_nonwhite) %>%
        rename("NonWhite" = `1`) %>%
        ungroup %>%
        mutate(perc_NonWhite = NonWhite/rowSums(select(.,-c(civil_service_title_code,civil_service_title_name)))) %>%
        select(-`0`)
      else
        summarise(., num_nonwhite = n(),
                  med_salary = median(base_salary),
                  mean_salary = mean(base_salary))} %>%
    replace(is.na(.),0)

  if(!is.null(high_low)){
    output <- temp_data %>%
      group_by(civil_service_title_name, civil_service_title_code) %>%
      summarize(count=n(),
                median_pay_overall = median(base_salary),
                mean_pay_overall = mean(base_salary)) %>%
      filter(count >= 5, median_pay_overall > 10) %>%
      select(civil_service_title_name, civil_service_title_code, median_pay_overall, mean_pay_overall) %>%
      {if(high_low == "high")
        arrange(.,desc(median_pay_overall)) else
          arrange(.,median_pay_overall)} %>%
      ungroup() %>%
      left_join(temp_race_eth, by = c("civil_service_title_name", "civil_service_title_code")) %>%
      mutate(across(where(is.numeric),round,2))
  } else(
    output <- temp_data %>%
      group_by(civil_service_title_name, civil_service_title_code) %>%
      summarise(med_salary = median(base_salary),
                mean_salary = mean(base_salary),
                total_count = n()) %>%
      filter(total_count >= 5) %>%
      left_join(merge(temp_race_eth,temp_white_nonwhite), by = c("civil_service_title_code","civil_service_title_name")) %>%
      mutate(across(where(is.numeric),round,2)) %>%
      arrange(desc(med_salary))
  )

#  if(exportable){
#    output <- output %>%
#      select(title_name_code,med_salary,mean_salary,Male,Female) %>%
#      filter(Male > 4 & Female > 4)
#  }

  return(output)
}

#' Proportion of a variable of interest in titles
#'
#' @description
#' This gives a summarised table of titles with their percentage of the variable of interest. Binning is another layer of grouping available.
#'
#' @details
#' TASKS that use this:
#' \itemize{
#' \item Median Salary for Civil Service Title Codes by Share of Non-White Employees per Title
#' \item Median Salary for Civil Service Title Codes by Share of Female Employees per Title
#' \item Median Salary for Civil Service Title Codes by Share of Non-White Female Employees per Title
#' }
#'
#' NOTE: This function will ONLY work if the data input is generated by \code{payeqfunctions::clean_data()} due to the naming scheme.
#'
#'
#' @import dplyr
#'
#' @param data dataframe. Pass the full or subset of the cleaned dataset using \code{payeqfunctions::clean_data()} to this function
#' @param share_var string. Variable name that is being filtered.
#' @param share_var_val string. The value the filtered variable should take.
#' @param binning Default is TRUE. When TRUE, returns a binned (with a binwidth of 0.1) table. When FALSE, returns a dataset of all the titles in the passed data with the percentage of the variable of interest
#' @return Dataframe of titles with details or a binned version.
#' @export
#' @examples
#' \dontrun{
#' # Median Salary for Civil Service Title Codes by Share of Non-White Employees per Title
#' title_code_share(data = cleaned_dataset, share_var = "nonwhite", share_var_val = 1) # Binned nonwhite proportions for titles
#' # Median Salary for Civil Service Titltle Codes by Share of Female Employees per Title
#' title_code_share(data = cleaned_dataset, share_var = "gender", share_var_val = "Female")
#' # Median Salary for Civil Service Title Codes by Share of Non-White Female Employees per Title
#' title_code_share(data = DOB_cleaned_dataset, share_var = "nonwhite_female", share_var_val = 1, binning = FALSE) # Nonbinned nonwhite female proportions for titles in the DOB
#' }

title_code_share <- function(data,share_var,share_var_val, binning = TRUE){
  # Suppress summarise info
  options(dplyr.summarise.inform = FALSE)

  output <- data %>%
    filter(!!as.symbol(share_var) == share_var_val) %>%
    group_by(civil_service_title_code, !!as.symbol(share_var)) %>%
    summarise(n_count = n()) %>%
    left_join(data %>%
                group_by(civil_service_title_code) %>%
                summarise(n_workers = n()),.) %>%
    mutate(n_count = ifelse(is.na(n_count),0,n_count),
           perc_n = n_count/n_workers) %>%
    left_join(data,., by = "civil_service_title_code")

  if(binning){
    output <- output %>%
      mutate(n_bin = cut(perc_n,seq(0,1,by = 0.1),include.lowest = TRUE)) %>%
      group_by(n_bin) %>%
      summarise(median_salary = median(base_salary),
                num_workers = n())
  }

  return(output)
}


#' Agency most populous titles and their composition
#' @description
#' Finding the most populous title(s) in every agency and the title's general demographic breakdowns.
#'
#' @details
#' TASKS that use this:
#' \itemize{
#' \item Agency Findings
#' }
#'
#' NOTE: This function will ONLY work if the data input is generated by \code{payeqfunctions::clean_data()} due to the naming scheme.
#'
#' @import dplyr
#'
#' @param data dataframe. Pass the full or subset of the cleaned dataset using \code{payeqfunctions::clean_data()} to this function
#' @param top_n_titles numeric. Number of top populous titles per agency. Default is set to 1
#' @return Dataframe of most populous titles per agency with their details
#' @export
#' @examples
#' \dontrun{
#' # Agency Findings
#' agencies_populous_title_stats(data = cleaned_dataset)
#' }

agencies_populous_title_stats <- function(data, top_n_titles = 1){
  # Suppress summarise info
  options(dplyr.summarise.inform = FALSE)

  temp_data <- data %>%
    group_by(agency) %>%
    mutate(agency_workers = n(),
           concate_id = paste(agency, civil_service_title_name, civil_service_title_code)) %>%
    ungroup()

  most_populous_profiles <- temp_data %>%
    group_by(agency, civil_service_title_name, civil_service_title_code) %>%
    summarise(num_workers_title = n(),
              perc_workers_title = num_workers_title / first(agency_workers),
              median_salary_title = median(base_salary),
              mean_salary_title = mean(base_salary),
              median_age_title = median(age_years),
              median_years_on_jobs_title = median(years_from_start)) %>%
    mutate(concate_id = paste(agency, civil_service_title_name, civil_service_title_code)) %>%
    arrange(desc(num_workers_title)) %>%
    group_by(agency) %>%
    slice(1:top_n_titles) %>%
    ungroup()

  filtered_data <- temp_data %>%
    filter(concate_id %in% (most_populous_profiles %>%
                              pull(concate_id)))

  # race_eth
  data_race <- filtered_data %>%
    group_by(agency, civil_service_title_name, civil_service_title_code, race_eth) %>%
    summarise(num_race_eth = n()) %>%
    arrange(desc(num_race_eth)) %>%
    slice(1) %>%
    ungroup()

  # gender
  data_gender <- filtered_data %>%
    group_by(agency, civil_service_title_name, civil_service_title_code, gender) %>%
    summarise(num_gender = n()) %>%
    arrange(desc(num_gender)) %>%
    slice(1) %>%
    ungroup()

  # concatenate
  data_add <- merge(data_race,data_gender)

  output <- most_populous_profiles %>%
    left_join(data_add, by = c("agency", "civil_service_title_name", "civil_service_title_code")) %>%
    mutate(perc_race_eth = num_race_eth / num_workers_title,
           perc_gender = num_gender / num_workers_title) %>%
    mutate(across(where(is.numeric),round,2))

  return(output)

  ## Melissa's original code for Agency Findings
  #
  #most_populous_profiles <- data_group_2 %>%
  #  group_by(Agency, Civil_Service_Title_Code,Civil_Service_Title_Name) %>%
  #  summarize(count=n()) %>%
  #  ungroup() %>% group_by(Agency) %>%
  #  mutate(perc_employees=count/sum(count)) %>%
  #  filter(count>=5) %>%
  #  ungroup() %>%
  #  group_by(Agency) %>%
  #  arrange(desc(count), .by_group=TRUE) %>%
  #  top_n(1, count)
  #
  #most_populous_profiles_summary <- data.frame()
  #
  #for (x in 1:nrow(most_populous_profiles)) {
  #  dt <- data_group_2 %>%
  #    filter(Civil_Service_Title_Code==unlist(most_populous_profiles[x,2]) & Agency==unlist(most_populous_profiles[x,1])) %>%
  #    group_by(Agency, Civil_Service_Title_Code, Civil_Service_Title_Name) %>%
  #    summarize(count_employees=n(), med_salary=median(Base_Salary), med_age=median(Age), med_yrs_on_job=median(Years_of_experience))
  #
  #
  #  dt_race <- data_group_2 %>%
  #    filter(Civil_Service_Title_Code==unlist(most_populous_profiles[x,2]) & Agency==unlist(most_populous_profiles[x,1])) %>%
  #    group_by(race_eth) %>%
  #    summarize(count_race=n()) %>%
  #    mutate(perc_race=count_race/sum(count_race)) %>%
  #    arrange(desc(count_race)) %>% top_n(1)
  #
  #  dt_gender <- data_group_2 %>%
  #    filter(Civil_Service_Title_Code==unlist(most_populous_profiles[x,2]) & Agency==unlist(most_populous_profiles[x,1])) %>%
  #    group_by(Gender) %>%
  #    summarize(count_gender=n()) %>%
  #    mutate(perc_gender=count_gender/sum(count_gender)) %>%
  #    arrange(desc(count_gender)) %>% top_n(1)
  #
  #  dt_race_gender <- dt %>% bind_cols(dt_race) %>% bind_cols(dt_gender)
  #
  #
  #  most_populous_profiles_summary <- most_populous_profiles_summary %>% bind_rows(dt_race_gender)
  #
  #}
  #
  #most_populous_profiles_summary %>% left_join(most_populous_profiles %>% select(Agency, perc_employees), by = "Agency")
}



