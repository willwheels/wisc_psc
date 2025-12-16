library(dplyr)

milw_drive <- googledrive::drive_ls("https://drive.google.com/drive/folders/1Xq7Sj4v7QKed6OMjkJWog_hjJINrUZ1f")


### DON'T AUTORUN BELOW THIS UNTIL GOOGLE AUTHENTICATES


if(!file.exists(here::here("data", "milwaukee_rolls", milw_drive$name[1]))) {
  
  googledrive::drive_download(milw_drive$id[1], path = here::here("data", "milwaukee_rolls", milw_drive$name[1]))
  
  }

if(!file.exists(here::here("data", "milwaukee_rolls", milw_drive$name[2]))) {
  
  googledrive::drive_download(milw_drive$id[2], path = here::here("data", "milwaukee_rolls", milw_drive$name[2]))
  
  }

milw_shutoffs <- readxl::read_xlsx(here::here("data", "milwaukee_rolls", milw_drive$name[1]),
                                   skip = 9)

milw_shutoffs <- milw_shutoffs |>
  janitor::clean_names() |>
  mutate(shutoff_date2 = lubridate::ymd(create_date),
         shutoff_year = lubridate::year(shutoff_date2))

milw_shutoffs |>
  group_by(class_description) |>
  count()

milw_shutoffs |>
  group_by(type_desc) |>
  count()

monthly_shutoffs <- milw_shutoffs |>
  mutate(shutoff_month = lubridate::month(shutoff_date2)) |>
  group_by(shutoff_year, shutoff_month) |>
  count() |>
  arrange(shutoff_year, shutoff_month)

milw_restores <- readxl::read_xlsx(here::here("data", "milwaukee_rolls", milw_drive$name[2]),
                                   skip = 8)

milw_restores <- milw_restores |>
  janitor::clean_names() |>
  mutate(restore_date2 = lubridate::ymd(create_date),
         restore_year = lubridate::year(restore_date2))

milw_restores |>
  group_by(class_description) |>
  count()

milw_restores |>
  group_by(type_desc) |>
  count()

milw_restores |>
  group_by(restore_year) |>
  count() |>
  arrange(restore_year)


milw_shutoffs |>
  group_by(premise_address) |>
  count(sort = TRUE)

milw_shutoffs |>
  select(premise_address) |>
  distinct() |>
  count(sort = TRUE)

milw_shutoffs_23 <- milw_shutoffs |>
  filter(shutoff_year == 2023) |>
  group_by(premise_address) |>
  mutate(count_shutoff = n()) |>
  ungroup()
