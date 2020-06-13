source("functions_eilat_knolls.R")


expedition <- "Eilat June 2020"

create_expedition_directory("Eilat June 2020")

folder <- create_folder_name("Test",Sys.Date())

create_main_directory(expedition_name = expedition, folder_name = folder)


# 3. Create Folder for metadata
create_meta_dir(expedition_name = expedition, folder_name = folder)

# 4. Read metadata, create metadata table, and finally upload it to metadata folder
take_data("eilat knoll test.csv") %>% 
  create_knolls_meta_sheet() %>% 
  upload_meta_sheet(expedition_name = expedition, folder_name = folder)

# 5. Generate Observer Tables 
take_data("eilat knoll test.csv") %>% 
  get_knolls_surveyors_data() %>% 
  create_knolls_observer_tables(expedition_name = expedition, folder_name = folder)

# 6. Use the link received on user email to feed data

# ----------------

tables <-  get_all_knolls_observer_tables(expedition_name = expedition, folder_name = folder)

join_knolls_day_metadata_with_observer_tables(expedition_name = expedition,
                                              folder_name = folder,
                                              big_observer_table = tables)

