#' Cleaning the dataset
#'
#' @description
#' Cleans the dataset and adds calculated columns.
#'
#' @details
#' It will rename all the column names, then clean and set data types the correct ones.
#'
#' The newly created columns are as follows:
#' \enumerate{
#' \item `days_from_start`: `year`-12-31 - `start_date`
#' \item `years_from_start`: `days_from_start`/365
#' \item `age`: `year`-12-31 - `dob`
#' \item `age_years`: \code{floor(age)}
#' \item `age_above18`: `age_years` - 18
#' \item `race_eth`: As defined in the DCAS worker's profile report
#' \item `nonwhite`: 1 if `race` is neither "White" or "Unknown or Choose Not to Disclose" or if `ethnicity` is "Hispanic or Latino".
#' \item `nonwhite_female`: 1 if `nonwhite` is 1 and if `gender` is "Female"
#' }
#'
#' This function will return a cleaned and tidied pay dataset.
#'
#' By default, dataset is filtered for "Full-Time", "Competitive", and "Non-Competitive" employees who make over $15,000 and are over 16 years of age.
#'
#' @import dplyr
#' @importFrom lubridate year
#' @import readxl
#' @param filepath string. Path to the pay equity file. Default is set as "C:/Users/Public/Desktop/Local Law 18 Materials/1- 22 Data Elements Dataset.csv"
#' @param dataset dataframe. Input the preprocessed dataframe. Either filepath or dataset params must be populated.
#' @param year numeric. YYYY Year of dataset. Used for calculating years from start and age \bold{(REQUIRED)}
#' @param column_names string. Accepts a vector of column names in the order they should appear. Default is \code{"default"}. WARNING: If the default does not work, or is changed, other functions in this package may not function
#' @param filter_employee_status string. Filter options, default is \code{filter = "full-time"}. Accepts \code{"part-time"} for filtering to a part-time prefixed settings. If anything else is inputted, then it will return the full dataset without any employee_status filters.
#' @param uniform_filepath string. Add filepath for the uniform titles for the uniform title column
#' @return Dataframe of cleaned dataset
#' @export

clean_data <- function(filepath = "C:/Users/Public/Desktop/Local Law 18 Materials/1- 22 Data Elements Dataset.csv",dataset,year,column_names = "default",filter_employee_status = "full-time",uniform_filepath){
  # Create a temporary dataframe with all the data
  if(!base::missing(dataset)){
    temp_df <- dataset
  } else{
    temp_df <- tryCatch(read.csv(filepath,
                                 header = T),
                        error=function(cond) {
                          message(cond)
                        })
  }

  # Stop if year is missing
  if(base:missing(year)){
    stop("Please provide a value for year.")
  }

  # Change column names
  if(column_names == "default"){
    colnames(temp_df) <- c('agency','start_date','civil_service_title_code','civil_service_title_name',
                           'min_salary','max_salary','business_title','title_classification',
                           'job_category','career_level_title_suffix','career_level_title_level',
                           'base_salary','salary_pay_band','DCAS_OGC','DCAS_OGN','managerial',
                           'highest_educ_level','gender','race','ethnicity','dob','provisional_status',
                           'employee_status','personnel_status_change_desc','prev_employed')
  } else{
    colnames(temp_df) <- column_names
  }


  # Find max year in dataset
  #max_year <- year(max(as.Date(temp_df$start_date))) #(this does not work b/c some start dates are after the dataset's date)
  max_year <- year

  # Clean and export
  output <- temp_df %>%
    mutate(business_title = as.character(business_title),
           career_level_title_suffix = as.factor(career_level_title_suffix),
           DCAS_OGC = as.factor(DCAS_OGC),
           dob = as.Date(dob),
           start_date = as.Date(start_date),
           managerial = as.factor(ifelse(managerial == 'Y','Y','N')),
           days_from_start = as.numeric(as.Date(paste0(max_year,"-12-31")) - start_date),
           years_from_start = round(days_from_start / 365,0),
           age = as.numeric(as.numeric(as.Date(paste0(max_year,"-12-31")) - dob))/365,
           age_years = floor(age),
           age_above18 = (age_years - 18),
           gender = relevel(as.factor(gender),ref = "Male"),
           race_eth = factor(case_when(
             ethnicity == "Hispanic or Latino" ~ as.character(ethnicity),
             ethnicity == "Unknown or Choose Not to Disclose" ~ "Ethnicity Unknown or Choose Not to Disclose",
             race == "Native Hawaiian or Pacific Islander" ~ "NH Asian",
             TRUE ~ case_when(
               race == "American Indian or Alaska Native"|
                 race == "Two or more races"|
                 race == "Unknown or Choose Not to Disclose" ~ "NH SOR or Race UCND",
               TRUE ~ paste("NH",as.character(race))
             )),
             levels = c("NH White","NH Black or African American","Hispanic or Latino","NH Asian",
                        "Ethnicity Unknown or Choose Not to Disclose","NH SOR or Race UCND")),
           min_salary = ifelse(min_salary < 15000 & employee_status == "Full-Time",NA,min_salary),
           max_salary = ifelse(min_salary < 15000 & employee_status == "Full-Time",NA,max_salary),
           nonwhite = ifelse(race != "White" & race != "Unknown or Choose Not to Disclose" | ethnicity == "Hispanic or Latino", 1, 0),
           nonwhite_female = ifelse(nonwhite == 1 & gender == "Female",1,0)
    )

  if(filter_employee_status == "full-time"){
    output <- output %>%
      filter(employee_status == "Full-Time",
             base_salary >= 15000,
             age_years >= 16,
             title_classification == "Competitive" | title_classification == "Non-Competitive")
  } else if(filter_employee_status == "part-time"){
    output <- output %>%
      filter(employee_status == "Part-Time",
             base_salary >= 13,
             age_years >= 1,
             title_classification == "Competitive" | title_classification == "Non-Competitive")
  }

  if(!base::missing(uniform_filepath)){
    uniform_titles <- read_excel(uniform_filepath)

    output <- output %>%
      mutate(uniform = ifelse(civil_service_title_code %in% uniform_titles$TC,"yes","no"))
  }
  return(output)
}
