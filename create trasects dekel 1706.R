source("functions_eilat_transects.R")


transpedition <- "Eilat Transects June 2020"

create_expedition_directory(transpedition)

dekel_folder <- create_folder_name("Dekel",Sys.Date())

#2. Create folder for sheets
create_main_directory(expedition_name = transpedition, folder_name = dekel_folder)


# 3. Create Folder for metadata
create_meta_dir(expedition_name = transpedition, folder_name = dekel_folder)

# 4. Read metadata, create metadata table, and finally upload it to metadata folder
take_data("eilat dekel 1706 transects.csv") %>% 
  create_transects_meta_sheet() %>% 
  upload_meta_sheet(expedition_name = transpedition, folder_name = dekel_folder)

# 5. Generate Observer Tables 
take_data("eilat dekel 1706 transects.csv") %>% 
  get_transects_surveyors_data() %>% 
  create_transects_observer_tables(expedition_name = transpedition, folder_name = dekel_folder)

# 6. Use the link received on user email to feed data

# ----------------

tables <-  get_all_knolls_observer_tables(expedition_name = expedition, folder_name = folder)

join_knolls_day_metadata_with_observer_tables(expedition_name = expedition,
                                              folder_name = folder,
                                              big_observer_table = tables)

