library(magrittr)
library(dplyr)
library(purrr)

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
readr::write_csv(china_cities, "data-raw/china_city_list.csv", )
usethis::use_data(china_cities, internal = TRUE, overwrite = TRUE)


# correct province name
province <- regionNames("china")
ncov <- ncovr::get_ncov()
# ncov province
ncov_province <- ncov$provinceName
need_correct_province <- setdiff(province, ncov_province)
province[match(need_correct_province, province)] <-
  c("台湾", "香港", "澳门")

# cities does not need info: taiwan, xianggang, aomen
province_has_cities <-
  setdiff(province,
          c("台湾", "香港", "澳门"))
cities_list <- purrr::map(province_has_cities, regionNames)
names(cities_list) <- province_has_cities
province_cities <-
  purrr::map2(cities_list, province_has_cities,
              ~ data.frame(Province = .y, City_Admaster = .x)) %>%
  dplyr::bind_rows() %>%
  mutate(Province = substr(Province, 1, 2)) %>%
  mutate(Province = case_when(
    Province == "黑龙" ~ "黑龙江",
    Province == "内蒙" ~ "内蒙古",
    TRUE ~ Province
  ))
province_cities$Province_EN <- china_cities$Province_EN[
  match(province_cities$Province, china_cities$Province)]

## correct city name
# ncov cities
ncov_cities_list <- purrr::map(province_has_cities,
                               get_province_cites, ncov = ncov)
names(ncov_cities_list) <- province_has_cities
city_distg <- purrr::map2(cities_list, ncov_cities_list, distinguish_cities)

# 404 cities is consistent
map(city_distg, ~.x$consistent) %>% lengths %>% sum

#' distinguish cities between consistent and inconsistent
#'
distinguish_cities <- function(city, ncov_city) {
  index <- purrr::map_lgl(ncov_city, ~ any(grepl(.x, city)))
  res <- list(consistent_ncov = ncov_city[index],
              consistent = city[index],
              inconsistent = ncov_city[!index])

  res
}

city_corrected <- tibble::tribble(
  ~ origin,           ~corrected,               ~Province,
  "伊犁州",           "伊犁哈萨克自治州",    "新疆维吾尔自治区",
  "昌吉州",           "昌吉回族自治州",      "新疆维吾尔自治区",
  "巴州",             "巴音郭楞蒙古自治州",  "新疆维吾尔自治区",
  "兵团第八师石河子市","石河子市",           "新疆维吾尔自治区",
  "兵团第六师五家渠市","五家渠市",           "新疆维吾尔自治区",
  "海北州",            "海北藏族自治州",       "青海省",
  "甘孜州",            "甘孜藏族自治州",       "四川省",
  "凉山州",           "凉山彝族自治州",        "四川省",
  "阿坝州",            "阿坝藏族羌族自治州",   "四川省",
  "大理州",            "大理白族自治州",       "云南省",
  "红河州",            "红河哈尼族彝族自治州", "云南省",
  "德宏州",            "德宏傣族景颇族自治州", "云南省",
  "楚雄州",            "楚雄彝族自治州",       "云南省",
  "文山州",            "文山壮族苗族自治州",   "云南省",
  "湘西自治州",        "湘西土家族苗族自治州", "湖南省",
  "韩城",              "韩城(渭南)",           "陕西省",
  "杨凌",             "杨凌(咸阳)",            "陕西省",
  "公主岭",            "(四平市)",             "吉林省",
  "梅河口",           "(通化市)",              "吉林省",
  "恩施州",            "恩施土家族苗族自治州", "湖北省",
  "黔南州",           "黔南布依族苗族自治州",  "贵州省",
  "黔东南州",          "黔东南苗族侗族自治州", "贵州省",
  "黔西南州",         "黔西南布依族苗族自治州","贵州省",
  "赣江新区",          "(新区)",               "江西省",
  "两江新区",         "(新区)",                "重庆市",
  "高新区",           "(高新区)",              "重庆市",
  "石柱县",           "石柱土家族自治县",      "重庆市",
  "彭水县",           "彭水苗族土家族自治县",  "重庆市",
  "秀山县",           "秀山土家族苗族自治县",  "重庆市",
  "万盛经开区",        "(开发区)",             "重庆市",
  "宁东",                "(宁东)",             "宁夏回族自治区"
)

uncertain_dat <- tibble::tribble(
  ~city,              ~province,
  "兵团第四师",    "新疆维吾尔自治区",
  "兵团第九师",    "新疆维吾尔自治区",
  "兵团第七师",    "新疆维吾尔自治区",
  "兵团第十二师",  "新疆维吾尔自治区",
  "待明确地区",    "甘肃省",
  "待明确地区",    "云南省",
  "待明确地区",    "广东省",
  "待明确地区",    "河南省",
  "待明确地区",    "北京市",
  "外地来京人员",  "北京市",
  "待明确地区",    "天津市",
  "外地来津人员",  "天津市",
  "外地来沪人员",   "上海市"
)

#' extract province cities in ncov
#'
#' @param ncov ncov data
#' @param p province name
get_province_cites <- function(ncov, p) {
  province_dat <- dplyr::filter(ncov$area, provinceName == p)
  cities <- province_dat$cities[[1]]$cityName
}
