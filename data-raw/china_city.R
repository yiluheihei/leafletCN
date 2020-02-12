download.file("https://github.com/pzhaonet/ncovr/raw/master/inst/china_city_list.csv",
              destfile = "data-raw/china_city_list.csv")
china_cities <- readr::read_csv("data-raw/china_city_list.csv")
# data correction
china_cities <- dplyr::mutate(china_cities,
              Province_EN = dplyr::case_when(
                Province_EN == "anhui" ~ "Anhui",
                Province_EN == "guizhou" ~ "Guizhou",
                Province_EN == "hubei" ~ "Hubei",
                Province_EN == "xinjiang" ~ "Xinjiang",
                TRUE ~ Province_EN
              ))
usethis::use_data(china_cities, internal = TRUE, overwrite = TRUE)
