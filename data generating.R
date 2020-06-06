source("functions.R")

# Workflow for data generating

# 0. Edit "input file" to include necessary data


# 1. Create name from LOCATION and DATE
test <- create_folder_name("My Room",Sys.Date())

# 2. Create Folder with name
create_main_directory(test)

# 3. Create Folder for metadata
create_meta_dir(test)

# 4. Read metadata, create metadata table, and finally upload it to metadata folder
take_data("input file.csv") %>% 
  create_meta_sheet() %>% 
  upload_meta_sheet(test)

# 5. Generate Observer Tables 
take_data("input file.csv") %>% 
  get_surveyors_data() %>% 
  create_observer_tables(test)

# 6. Use the link received on user email to feed data