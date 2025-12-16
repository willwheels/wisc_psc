library(selenider)


default_url <- ("https://apps.psc.wi.gov/ARS/WEGSqueries/default.aspx")

session <- selenider_session(
  "chromote",
  timeout = 60,
  options = chromote_options(headless = FALSE) 
  
)

open_url(default_url)




session |>
  find_element("//*[@id='form1']/p[3]/a")
