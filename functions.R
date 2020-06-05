library(tidyverse)

## First thing's first
# Login to lab.belmaker@gmail.com account and run this. 
# It will pop up the browser - select the lab.belmaker@gmail account and accept everything.

googledrive::drive_auth()

# function to create a folder in Google Sheets named LOCATION DATE

location <- "Tonga"
date <- "10/03/2021"
googledrive::drive_find()
googledrive::drive_browse()


googledrive::drive_mkdir(name = paste(location,date),
                         path = googledrive::as_id("https://drive.google.com/drive/u/3/folders/1LNvaAGj7SK5sJ9xvFUKQIPQRZAZKMj7F")
)

googlesheets4::gs4_create(name = "Test")
