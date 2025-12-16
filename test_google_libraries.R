if(!("googlesheets4" %in% installed.packages())) {install.packages("googlesheets4")}


## all USC report data at https://drive.google.com/drive/folders/14GGZ1IjaJrDOZPrlZYKAMZlmtzH5AFxK

milw_shutoffs_raw_url <- "https://docs.google.com/spreadsheets/d/1dF_iZ0jzc9uUDFCmg4txYRaZH2dwpWV2/edit?gid=269564554#gid=269564554"

milw_restore_raw_url <- "https://docs.google.com/spreadsheets/d/11bfTb5AyK-v0XyzQLZe4H6S6QlJdRMVM/edit?rtpof=true&gid=1390840314#gid=1390840314"

googlesheets4::read_sheet(milw_shutoffs_raw_url)

googlesheets4::read_sheet(milw_restore_raw_url)


## both attempts get 

# Error in `gs4_get_impl_()`:
#   ! Client error: (400) FAILED_PRECONDITION
# • Request can not be executed in the current system state, such as deleting a non-empty directory.
# • This operation is not supported for this document

## I tried fiddling w/ urls

googlesheets4::as_sheets_id(milw_restore_raw_url) ## also doesn't work

if(!("googledrive" %in% installed.packages())) {install.packages("googledrive")}

googledrive::drive_find(n_max = 30) ## searches my google drive, less helpful


## this works 
milw_drive <- googledrive::drive_ls("https://drive.google.com/drive/folders/1Xq7Sj4v7QKed6OMjkJWog_hjJINrUZ1f")

googledrive::drive_download(milw_drive$id[1], path = here::here("data", "milwaukee_rolls", milw_drive$name[1]))
