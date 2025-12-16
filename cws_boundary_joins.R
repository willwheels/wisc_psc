

options(timeout = 150)

if(!file.exists(here::here("data", "CWS_Boundaries_Latest.zip"))) {
  
  download.file("https://github.com/USEPA/ORD_SAB_Model/raw/refs/heads/main/Version_History/CWS_Boundaries_Latest.zip",
                destfile = here::here("data", "CWS_Boundaries_Latest.zip"))
  
  unzip(here::here("data", "CWS_Boundaries_Latest.zip"), 
        exdir = here::here("data", "cws_boundaries"))
  
}


