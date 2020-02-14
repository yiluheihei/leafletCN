# download and tar origin geojson file ------------------------------------

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

# copy the province and country json file ---------------------------------

# reference china cities file, function get_china_cities is defined in
# china.city.R
china_cities <- get_china_cities()
province <- unique(china_cities$Province)
province_en <- unique(china_cities$Province_EN)
province_name_full <- jsonlite::fromJSON("inst/geojson/china.json")$
  features$properties$name

# province index corresponding to province dir
province_index <- sapply(province, function(x) which(grepl(x, province_name_full)))
province_name_full <- province_name_full[province_index]

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
# for test, should be used as internal data
file.copy(province_to_json, "inst/geojson/")

# manulally fix Sichuan.json on https://mapshaper.org/
# it will raise an error while visulazie Sichuan.json using geojsonMap
# Error in createPolygonsComment(p) : rgeos_PolyCreateComment: orphaned hole, cannot find containing polygon for hole at index 3.
#
# Thus, the data is preprocessed on https://mapshaper.org/: load data, remove two lines, and export geojson data to replance the original Sichuan.json

# reference city to json file ---------------------------------------------

mapNames <- data.frame(
  name = c(province_name_full, "中国"),
  name_en = c(province_en, "china"),
  label = c(province, "中国"),
  files = c(paste0(province_en, ".json"),"china.json"),
  stringsAsFactors = FALSE
)

# does not work properly for chinise character
# save(mapNames, file = "../R/sysrda.data")
usethis::use_data(mapNames, internal = TRUE, overwrite = TRUE)

