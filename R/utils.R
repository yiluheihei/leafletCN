# .onAttach <- function(libname, pkgname ){
#   library(leaflet)
#   library(sp)
# }
globalVariables(c("leafletcn.map.names", ".triList"))

## Encoding solution
encodingSolution = function(str){
  iconv(str, "UTF-8", "UTF-8")
}

## read function
readGeoLocal = function(city){
  # query = toLabel(city)
  if(!city %in% c(mapNames$name, mapNames$label, "china", "world")){
    stop(paste0("\n",
                city,
                ": this mapType cannot found!\n",
                "Please check the mapType name or use icnov to convert encoding.\n",
                "Valid mapTypes: regionNames()\n",
                "Encoding convert: ?iconv"))
  }

  file = paste0(
    "geojson/",
    mapNames$files[mapNames$name == city | mapNames$label == city | mapNames$name_en == city]
  )
  filePath = system.file(file,package = "leafletCN")

  # output = rgdal::readOGR(filePath, "OGRGeoJSON")
  output = read.geoShape(filePath)

  # for taiwan
  index <- mapNames$name == city | mapNames$label == city | mapNames$name_en == city
  city_info <- mapNames[index, ]
  if (city_info$name_en == "Taiwan"){
    output$name <- city_info$label
  }
  if(.Platform$OS.type == "windows"){
    output$name = encodingSolution(output$name)
  }

  return(fix_orphaned_hole(output))
}

## .triList
## Use first two words to match
toLabel = function(city){
  labels = sapply(city, function(x){
    if(tolower(substr(x,1,1)) %in% letters){
      return(tolower(x))
    }else if(x == .triList[[5]] | grepl(paste0(.triList[[5]],.triList[[7]][1]), x)){
      warning("Using Jilin Province instead of Jilin City!")
      return(.triList[[5]])
    } else if(grepl(.triList[[5]], x) & !grepl(paste0(.triList[[5]],.triList[[7]][1]), x)){
      return(paste0(.triList[[5]],.triList[[7]][2]))
    }
    else if(x == .triList[[6]] | grepl(paste0(.triList[[6]],.triList[[7]][1]), x)){
      warning("Using Hainan Province instead of Hainan City!")
      return(.triList[[6]])
    }else if(grepl(.triList[[6]], x) & !grepl(paste0(.triList[[6]],.triList[[7]][1]), x)){
      return(paste0(.triList[[6]],.triList[[7]][2]))
    }else if(grepl(.triList[[1]], x)|
       grepl(.triList[[2]], x)|
       grepl(.triList[[3]], x)|
       grepl(.triList[[4]], x)
    ){
      return(substr(x, 1, 3))
    }else{
      return(substr(x, 1, 2))
    }
  })
  return(labels)
}

## Fork from echarts
evalFormula = function(x, data) {
  # x = ~value; data = mapData
  if (!inherits(x, 'formula')) return(x)
  if (length(x) != 2) stop('The formula must be one-sided: ', deparse(x))
  x_formula = terms.formula(x)
  if (length(attr(x_formula, "term.labels")) == 1){
    eval(x[[2]], data, environment(x))
  }else{
    as.data.frame(sapply(attr(x_formula, "term.labels"),function(tmpTerm){
      return(eval(as.name(tmpTerm), data, environment(x)))
    }),stringsAsFactors=F)
  }
}

#' Fix orphaned hole, ensure each polygon having an outer edge and an inner
#' hole
#'
#' https://cran.r-project.org/web/packages/maptools/vignettes/combine_maptools.pdf
#' https://github.com/MatMatt/MODIS/commit/1b14974063b371a69987e5ee218ee66f132b2d61#diff-786518131335adf2d5c6c59e7f1665a1
fix_orphaned_hole <- function(x) {
  polys <- slot(x, "polygons")
  fixed <- lapply(polys, maptools::checkPolygonsHoles)

  fixed_sp <- sp::SpatialPolygons(
    fixed,
    proj4string = sp::CRS((sp::proj4string(x)))
  )

  if (inherits(x, "SpatialPolygonsDataFrame")) {
    fixed_sp <- sp::SpatialPolygonsDataFrame(fixed_sp, x@data)
  }

  fixed_sp
}
