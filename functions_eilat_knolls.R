library(tidyverse)

## First thing's first
# Login to lab.belmaker@gmail.com account and run this. 
# It will pop up the browser - select the lab.belmaker@gmail account and accept everything.

googledrive::drive_auth()


create_expedition_directory <- function(expedition_name){
  googledrive::drive_mkdir(name = expedition_name,
                           path = "~/Data Sheets/")
}


# function to create folder name from  LOCATION DATE
create_folder_name <- function(location,date){
  paste(location,date)
}


# function to create a folder in Google Sheets named folder name
create_main_directory <- function(expedition_name,folder_name) {
  googledrive::drive_mkdir(name = folder_name,
                           path = str_glue("~/Data Sheets/{expedition_name}/"))
}

# function to generate permissions

grant_permission <- function(spreadsheet_id,vector_of_emails){
  lapply(vector_of_emails, function(email) {
    if (is.na(email)) { warning("No Email Assigned to Surveyor, permission not granted")
    } else {
      googledrive::drive_share(file = spreadsheet_id,role = "writer",type = "user",
                               emailAddress = email)
    }
  }
  )
}


# function to create a folder within the sampling day folder. This
# folder will store the metadata sheets

create_meta_dir <- function(expedition_name,folder_name) {
  dir <- str_glue("~/Data Sheets/{expedition_name}/{folder_name}/")
  googledrive::drive_mkdir(name = "Metadata",
                           path = dir)
}

#### functions to create sampling metadata sheet: ####


# function to get data from input file

take_data <- function(input_sheet){
  input <-  read_csv(input_sheet)
  if (any(is.na(input$`First Observer`))) warning("Missing First Observer, please check input sheet")
  if (any(is.na(input$`Second Observer`))) warning("Missing Second Observer, please check input sheet")
  input <- input %>% mutate_at(.vars = c("Country","Project","Location","Date"),.funs = first)
  input <- input %>% mutate(Date = lubridate::dmy(Date))
  return(input)
}

# function to create metadata sheet from data

create_knolls_meta_sheet <- function(input_data){
  red <- tibble(`Date` = input_data$Date,
                `Location` = input_data$Location,
                `Start` = NA,
                `End` = NA,
                `Country` = input_data$Country,
                `Project` = input_data$Project,
                `Reserve` = NA,
                `First Observer` = input_data$`First Observer`,
                `Second Observer` = input_data$`Second Observer`,
                `KnollID` = input_data$Knoll,
                `Lat(N)` = NA,
                `Lon(E)` = NA
  )
  return(red)
}

# function to store metadata sheet in metadata folder

upload_meta_sheet <- function(meta_table, expedition_name, folder_name){
  sheet_name <- paste(folder_name,"- metadata")
  sheet <- googlesheets4::gs4_create(name = sheet_name,sheets = list(metadata = meta_table))
  googledrive::drive_mv(file = sheet, 
                        path = str_glue("~/Data Sheets/{expedition_name}/{folder_name}/Metadata/"))
}

# function to get surveyors names, emails, and number of dives in current day:

get_knolls_surveyors_data <- function(input_data) {
  id <- googledrive::drive_get("~/Data Sheets/Skeleton Folder/Surveyors")$id
  email_lookup_table <- googlesheets4::read_sheet(ss = id,sheet = 1) 
  
  surveyors <- input_data %>%
    group_by(`First Observer`,`Second Observer`) %>%
    summarize(knolls = list(Knoll)) %>%
    mutate(sheet_name =  str_glue("{`First Observer`} and {`Second Observer`}")) %>% 
    left_join(rename(.data = email_lookup_table, `First Observer` = FullName), by = "First Observer") %>% 
    left_join(rename(.data = email_lookup_table, `Second Observer` = FullName),by = "Second Observer") %>% 
    mutate(emails = map2(.x = `email.x`,.y = `email.y`, .f = function(x,y) c(x,y))) %>% 
    select(`First Observer`,`Second Observer`,sheet_name,knolls,emails)
  return(surveyors)
}


# functions to create observers tables, place them in folder name, 
# create individual sheets for each transect, and give editting permissions to surveyors
# ... This is a long one...

create_knolls_observer_tables <- function(surveyors_data,expedition_name,folder_name) {
  pmap(.l = list(surveyors_data$`First Observer`,surveyors_data$`Second Observer`,
                 surveyors_data$sheet_name,
                 surveyors_data$knolls,
                 surveyors_data$emails), 
       .f = function(observer1,observer2,sheet_name,knolls,emails){
         spreadsheet <- googledrive::drive_cp(file = "~/Data Sheets/Skeleton Folder/Eilat Skeleton - Knolls",
                                              path = str_glue("~/Data Sheets/{expedition_name}/{folder_name}/"),
                                              name = sheet_name)
         lapply(knolls, function(knoll) {
           googlesheets4::sheet_copy(from_ss = spreadsheet,from_sheet = "Observer Table - MASTER",to_ss = spreadsheet,
                                     to_sheet = str_glue("Knoll {knoll}"))
           googlesheets4::range_write(ss = spreadsheet,sheet = str_glue("Knoll {knoll}"),
                                      col_names = F, range = "B2:B3",
                                      data = as.data.frame(c(observer1,observer2)),reformat = F)
         })
         googlesheets4::sheet_delete(ss = spreadsheet,sheet = "Observer Table - MASTER")
         grant_permission(spreadsheet_id = spreadsheet,vector_of_emails = emails)
       }
  )
}


# function to create a metadata + species table from a spreadsheet
# another long one...

read_knolls_observer_table <- function(spreadsheet) {
  
  columns <- c("Observer", "Species", "Amount", "Length", "Confidence")
  
  knolls <- googlesheets4::sheet_properties(spreadsheet) %>% 
    filter(str_detect(string = name,pattern = "Knoll ")) %>% 
    .$name
  
  all_knolls_of_surveyor_pair <- lapply(knolls, function(knollID){
    metadata <- googlesheets4::read_sheet(spreadsheet,sheet = knollID,col_types = 'c')[,1:2] %>%
      filter(!is.na(Metadata)) %>% 
      spread(key = Metadata,value = Value, fill = NA) %>%
      mutate("Knoll" = as.numeric(Knoll)) %>% 
      mutate(column_to_join_by = TRUE)
    
    writeLines("Stalling because Google doesn't allow too many requests.\nWe will resume in 10 seconds...
    \nDo you have any unnecessary sheets?\nPerhaps you should consider deleting them")
    Sys.sleep(10)
    
    obs_data_both <- googlesheets4::read_sheet(spreadsheet,knollID)[,-c(1:2)]
    
    
    obs_data_first <-  obs_data_both %>% 
      select(1:5) %>% 
      rename_all( ~ columns)
    
    obs_data_second <- obs_data_both %>% 
      select(6:10) %>% 
      rename_all( ~ columns)
    
    obs_data_both <- bind_rows(obs_data_first,obs_data_second) %>% 
      filter(!is.na(Observer)&!is.na(Species)&!is.na(Amount)) %>% 
      mutate(column_to_join_by = TRUE)
    
    right_join(metadata,obs_data_both)
  }
  )
  all_knolls_of_surveyor_pair <- bind_rows(all_knolls_of_surveyor_pair)
  return(all_knolls_of_surveyor_pair)
}

# function to get all observation tables and stack them as one big table 

get_all_knolls_observer_tables <- function(expedition_name,folder_name) {
  spreadsheets <- googledrive::drive_ls(str_glue("~/Data Sheets/{expedition_name}/{folder_name}/")) %>% 
    filter(name  != "Metadata") %>% 
    .$id
  
  all_tables <- lapply(spreadsheets, read_knolls_observer_table)
  return(bind_rows(all_tables))
}


# function to merge metadata file with observer tables based on
# a combination of site + first observer + second observer

join_knolls_day_metadata_with_observer_tables <- function(expedition_name,folder_name,big_observer_table) {
  
  metadata_id <- googledrive::drive_get(
    path = str_glue("~/Data Sheets/{expedition_name}/{folder_name}/Metadata/{folder_name} - metadata"))$id
  meta_table <- googlesheets4::read_sheet(metadata_id,"metadata") %>% 
    mutate(meta_to_site_identifier = paste0(`First Observer`,`Second Observer`,KnollID))
  
  results <- big_observer_table %>% 
    mutate(meta_to_site_identifier = paste0(`First Observer`,`Second Observer`,Knoll)) %>% 
    right_join(meta_table) %>% 
    filter(!is.na(Observer)&!is.na(Species)&!is.na(Amount))
  
  return(results)
  
}

clean_data <- function(all_data,list_of_column){
  all_data %>% 
    select(list_of_column) %>% 
    return()
}


