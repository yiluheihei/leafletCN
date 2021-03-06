% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/leafletGeo.R
\name{leafletGeo}
\alias{leafletGeo}
\title{Create a sp object from a data.frame}
\usage{
leafletGeo(mapName, dat = NULL, namevar = NULL, valuevar = NULL)
}
\arguments{
\item{mapName}{mapName for loading, eg. 'china', 'city', ...}

\item{dat}{a data.frame contain regions and values}

\item{namevar}{show which feature is chosen for name variable}

\item{valuevar}{show which featue is chosen for value variable}
}
\description{
Function for creating a sp object from a data.frame and
mapType
}
\examples{
if(require(leaflet)){
  dat = data.frame(regionNames("china"),
                                runif(35))
  map = leafletGeo("china", dat)

   pal <- colorNumeric(
     palette = "Blues",
     domain = map$value)

  leaflet(map) \%>\% addTiles() \%>\%
     addPolygons(stroke = TRUE,
     smoothFactor = 1,
     fillOpacity = 0.7,
     weight = 1,
     color = ~pal(value),
     popup = ~htmltools::htmlEscape(popup)
     ) \%>\%
   addLegend("bottomright", pal = pal, values = ~value,
                        title = "legendTitle",
                 labFormat = leaflet::labelFormat(prefix = ""),
                 opacity = 1)
}
}
