download.file("https://github.com/wenccro/chinaMapJsonData/archive/master.zip",
              destfile = "data-raw/map.zip")
untar("data-raw/map.zip", exdir = "data-raw/")
map_files <- list.files("data-raw/chinaMapJsonData-master/")
# tar map files: proivnce map files
province_map_files <- setdiff(map_files, c("README.md", "datas.json"))


# windows platform: use winrar to extract rar files
unrar_path <- shQuote("C:\\Program Files\\WinRAR\\UnRAR.exe")
# province_name <- tools::file_path_sans_ext(province_map_files)
if (!exists("data-raw/map")) dir.create("data-raw/map")
# sapply(file.path("data-raw/map", province_name),
#   dir.create)
cmds <- paste(unrar_path,
              "x",
              file.path("data-raw/ -master",  province_map_files),
              "data-raw/map")
sapply(cmds, shell)


# copy the province and country json file
province <- unique(china_cities$Province)
province_en <- unique(china_cities$Province_EN)
province_dir <- list.files("data-raw/map/", include.dirs = TRUE)

# province index corresponding to province dir
province_index <- sapply(province, function(x) which(grepl(x, province_dir)))
province_dir <- province_dir[province_index]

province_from_json <- file.path(
  "data-raw/map",
  province_dir,
  "datas.json"
)
province_to_json <- file.path(
  "data-raw/map",
  paste0(province_en, ".json")
)
file.copy(province_from_json, province_to_json)


# china json
file.copy("data-raw/chinaMapJsonData-master/datas.json",
          "data-raw/map/china.json")

## city json
mapNames <- data.frame(
  name = c(province_dir, "中国"),
  name_en = c(province_en, "china"),
  label = c(province, "中国"),
  files = c(paste0(province_en, ".json"),"china.json"),
  stringsAsFactors = FALSE
)
usethis::use_data(mapNames, overwrite = TRUE)

# just for test
file.copy(province_to_json, "inst/geojson/")

