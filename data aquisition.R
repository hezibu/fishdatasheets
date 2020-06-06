source("functions.R")

# Worflow for data aquisition

# 0. Make sure empty sheets are deleted as it wastes time to read them.


# 1. Create name from LOCATION and DATE
test <- create_folder_name("My Room",Sys.Date())

# 2. Get Observer  Tables from folder
tables <- get_all_observer_tables(test)

# 2.a. Be Patient


# 3. Merge with metadata sheet
full_data <- join_day_metadata_with_observer_tables(test,tables) 

# 4. Select what columns to continue working with:

columns_to_keep <- c("Location","Date","Site","First Observer","Second Observer","Transect","Depth Start",
                     "Depth End","Visibilty","Species","Amount","Length","Confidence")

data <- clean_data(full_data,columns_to_keep)

data