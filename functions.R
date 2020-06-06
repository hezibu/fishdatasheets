library(tidyverse)

## First thing's first
# Login to lab.belmaker@gmail.com account and run this. 
# It will pop up the browser - select the lab.belmaker@gmail account and accept everything.

googledrive::drive_auth()

# function to create folder name from  LOCATION DATE
create_folder_name <- function(location,date){
  paste(location,date)
}


# function to create a folder in Google Sheets named folder name
create_main_directory <- function(folder_name) {
  googledrive::drive_mkdir(name = folder_name,
                           path = "~/Data Sheets/")
}

# fuction to locate the folder for further use - DEPRECATED??

locate_directory <- function(folder_name) {
  id <- googledrive::drive_find(folder_name,n_max = 1)$id
  return(googledrive::as_id(id))
}

# function to generate permissions

grant_permission <- function(sheet_id,vector_of_emails){
  lapply(vector_of_emails, function(email) {
    if (is.na(email)) { warning("No Email Assigned to Surveyor, permission not granted")
      } else {
    googledrive::drive_share(file = sheet_id,role = "writer",type = "user",
                             emailAddress = email)
      }
  }
  )
}


# function to create a folder within the sampling day folder. This
# folder will store the metadata sheets

create_meta_dir <- function(folder_name) {
  dir <- str_glue("~/Data Sheets/{folder_name}/")
  googledrive::drive_mkdir(name = "Metadata",
                           path = dir)
}

#### functions to create sampling metadata sheet: ####


# function to get data from input file

take_data <- function(input_sheet){
  input <-  read_csv(input_sheet)
  if (any(is.na(input$`First Observer`))) warning("Missing First Observer, please check input sheet")
  if (any(is.na(input$`First Observer`))) warning("Missing Second Observer, please check input sheet")
  input <- input %>% mutate_at(.vars = c("Country","Project","Location","Date"),.funs = first)
  input <- input %>% mutate(Date = lubridate::dmy(Date))
  return(input)
}

# function to create metadata sheet from data

create_meta_sheet <- function(input_data){
  red <- tibble(`Date` = input_data$Date,
                `Location` = input_data$Location,
                `Start` = NA,
                `End` = NA,
                `Sea cond` = NA,
                `Country` = input_data$Country,
                `Project` = input_data$Project,
                `TripID` = NA,
                `Dive` = input_data$Dive,
                `Reserve` = NA,
                `First Observer` = input_data$`First Observer`,
                `Second Observer` = input_data$`Second Observer`,
                `SiteID` = input_data$Sites,
                `Lat(N)` = NA,
                `Lon(E)` = NA
  )
  return(red)
}

# function to store metadata sheet in metadata folder

upload_meta_sheet <- function(meta_table, folder_name){
  sheet_name <- paste(folder_name,"- metadata")
  sheet <- googlesheets4::gs4_create(name = sheet_name,sheets = list(metadata = meta_table))
  googledrive::drive_mv(file = sheet, path = str_glue("~/Data Sheets/{folder_name}/Metadata/",))
}

# function to get surveyors names, emails, and number of dives in current day:

get_surveyors_data <- function(input_data) {
  id <- googledrive::drive_get("~/Data Sheets/Skeleton Folder/Surveyors")$id
  email_lookup_table <- googlesheets4::read_sheet(ss = id,sheet = 1) 
  
  surveyors <- input_data %>%
    count(`First Observer`,`Second Observer`) %>%
    mutate(sheet_name =  str_glue("{`First Observer`} and {`Second Observer`}")) %>% 
    left_join(rename(.data = email_lookup_table, `First Observer` = FullName), by = "First Observer") %>% 
    left_join(rename(.data = email_lookup_table, `Second Observer` = FullName),by = "Second Observer") %>% 
    mutate(emails = map2(.x = `email.x`,.y = `email.y`, .f = function(x,y) c(x,y))) %>% 
    select(`First Observer`,`Second Observer`,sheet_name,n,emails)
  return(surveyors)
}
  

# functions to create observers tables, place them in folder name, 
# create individual sheets for each transect, and give editting permissions to surveyors
# ... This is a long one...

create_observer_tables <- function(surveyors_data,folder_name) {
  pmap(.l = list(surveyors_data$`First Observer`,surveyors_data$`Second Observer`,
                 surveyors_data$sheet_name,surveyors_data$n,surveyors_data$emails), 
       .f = function(observer1,observer2,sheet_name,number_of_dives,emails){
    spreadsheet <- googledrive::drive_cp(file = "~/Data Sheets/Skeleton Folder/Skeleton",
                                         path = str_glue("~/Data Sheets/{folder_name}/"),
                                         name = sheet_name)
    for (i in number_of_dives:1){
      for (transect_letter in LETTERS[5:1]){
        googlesheets4::sheet_copy(from_ss = spreadsheet,from_sheet = "Observer Table - MASTER",to_ss = spreadsheet,
                                  to_sheet = str_glue("Dive {i} - Transect {transect_letter}"))
        googlesheets4::range_write(ss = spreadsheet,sheet = str_glue("Dive {i} - Transect {transect_letter}"),
                                   col_names = F, range = "B2:B3",
                                   data = as.data.frame(c(observer1,observer2)),reformat = F)
      }
    }
    googlesheets4::sheet_delete(ss = spreadsheet,sheet = "Observer Table - MASTER")
    grant_permission(sheet_id = spreadsheet,vector_of_emails = emails)
       }
  )
}


# function to create a metadata + species table from a spreadsheet
# another long one...

read_observer_table <- function(spreadsheet) {
  
  columns <- c("Observer", "Species", "Amount", "Length", "Distance", "Confidence")
  
  transects <- googlesheets4::sheet_properties(spreadsheet) %>% 
    filter(str_detect(string = name,pattern = "Dive ")) %>% 
    .$name
  
  all_transects_of_surveyor_pair <- lapply(transects, function(transectID){
    metadata <- googlesheets4::read_sheet(spreadsheet,sheet = transectID,col_types = 'c')[,1:2] %>%
      filter(!is.na(Metadata)) %>% 
      spread(key = Metadata,value = Value, fill = NA) %>% 
      select(-c(1:25)) %>%
      mutate_at(.vars = c("Depth End","Depth Start","Site","Visibilty"),
                .funs = as.numeric) %>% 
      mutate(column_to_join_by = TRUE)
    
    writeLines("Stalling because Google doesn't allow too many requests.\nWe will resume in 10 seconds...
    \nDo you have any unnecessary sheets?\nPerhaps you should consider deleting them")
    Sys.sleep(10)
    
    obs_data_both <- googlesheets4::read_sheet(spreadsheet,transectID)[,-c(1:2)]
    
    
    obs_data_first <-  obs_data_both %>% 
      select(1:6) %>% 
      rename_all( ~ columns)
    
    obs_data_second <- obs_data_both %>% 
      select(7:12) %>% 
      rename_all( ~ columns)
    
    obs_data_both <- bind_rows(obs_data_first,obs_data_second) %>% 
      filter(!is.na(Observer)&!is.na(Species)&!is.na(Amount)) %>% 
      mutate(column_to_join_by = TRUE)
    
    right_join(metadata,obs_data_both)
  }
  )
  all_transects_of_surveyor_pair <- bind_rows(all_transects_of_surveyor_pair)
  return(all_transects_of_surveyor_pair)
}

# function to get all observation tables and stack them as one big table 

get_all_observer_tables <- function(folder_name) {
  spreadsheets <- googledrive::drive_ls(str_glue("~/Data Sheets/{folder_name}/")) %>% 
    filter(name  != "Metadata") %>% 
  .$id
  
  all_tables <- lapply(spreadsheets, read_observer_table)
  return(bind_rows(all_tables))
}


# function to merge metadata file with observer tables based on
# a combination of site + first observer + second observer

join_day_metadata_with_observer_tables <- function(folder_name,big_observer_table) {
  
  metadata_id <- googledrive::drive_get(
    path = str_glue("~/Data Sheets/{folder_name}/Metadata/{folder_name} - metadata"))$id
  meta_table <- googlesheets4::read_sheet(metadata_id,"metadata") %>% 
    mutate(meta_to_site_identifier = paste0(`First Observer`,`Second Observer`,SiteID))

  results <- big_observer_table %>% 
    mutate(meta_to_site_identifier = paste0(`First Observer`,`Second Observer`,Site)) %>% 
    right_join(meta_table) %>% 
    filter(!is.na(Observer)&!is.na(Species)&!is.na(Amount))
  
  return(results)

}

clean_data <- function(all_data,list_of_column){
  all_data %>% 
    select(list_of_column) %>% 
    return()
}


