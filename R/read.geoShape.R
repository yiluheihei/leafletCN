##' @title Read geoshape file into R
##'
##' @description Function for reading geojson shape file into R, come out to be a SpatialPolygonsDataFrame object
##'
##' @usage
##' read.geoShape(txt)
##'
##' @param txt   a JSON string, URL or file
##'
##' @examples
##' if(require(sp)){
##'   filePath = system.file("geojson/china.json",package = "leafletCN")
##'   map = read.geoShape(filePath)
##'   plot(map)
##' }
##' @export
read.geoShape <- function (txt) {
  raw = jsonlite::fromJSON(txt)

  # remove the redundant data
  datPart <- raw$features$properties
  real_indx <- !sapply(datPart$name, function(x) x == "" || is.null(x) || is.na(x))
  datPart = datPart[real_indx, ]
  ployList = lapply(raw$features$geometry$coordinates[real_indx], function(x) {
    if (class(x) == "array") {
      a = as.vector(x)
      dim(a) = c(length(a)/2, 2)
      Sr = sp::Polygon(a)
      Sp = sp::Polygons(list(Sr), "namei")
      return(Sp)
    }
    else {
      if (any(sapply(x, class) == "list")) {
        whilei = 0
        while (any(sapply(x, class) == "list")) {
          whilei = whilei + 1
          if (whilei == 10)
            break
          index = which(sapply(x, class) == "list")[1]
          x = append(x[-index], x[[index]])
        }
      }
      Sr = lapply(x, function(y) {
        a = as.vector(y)
        dim(a) = c(length(a)/2, 2)
        return(sp::Polygon(a))
      })
      Sp = sp::Polygons(Sr, "namei")
      return(Sp)
    }
  })
  for (i in 1:length(ployList)) {
    ployList[[i]]@ID = as.character(i)
  }
  ployPart = sp::SpatialPolygons(ployList, 1:length(ployList))
  # datPart = raw$features$properties
  if (any(sapply(datPart, class) == "list")) {
    # index = which(sapply(datPart, class) == "list")
    # outlist = lapply(index, function(x) {
    #   # fix for missing data
    #   fix <- lapply(datPart[, x], function(y) {
    #     if (length(y)) {
    #       return(y)
    #     } else {
    #       return(NaN)
    #     }
    #   })
    #
    #   out = do.call(rbind, fix)
    #   colnames(out) = paste0(names(datPart)[x], 1:dim(out)[2])
    #   return(out)
    # })
    # datPart = cbind(datPart, do.call(cbind, outlist))
    # datPart = datPart[, -index]
    datPart <-
      purrr::map_df(datPart, function(x) {
        if (class(x) == "list") {
          lapply(x, function(y) {
            if (length(y)) {
              y
            } else {
              "na"
            }
          })
        } else {
          if (class(x) == "data.frame") {
            if (length(x) == 1) x <- x[[1]]
            else x <- apply(x, 1, as.list)
          }
          x
        }
      })
  }
  rownames(datPart) = row.names(ployPart)
  ex_1.7 = sp::SpatialPolygonsDataFrame(ployPart, datPart)
  return(ex_1.7)
}
