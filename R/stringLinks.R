#' Function to generate proj4 and WKT string
#'
#' @param prj A string representing the shorthand name of the desired projection
#' @param x0 A numeric value specifying the longitude offset
#' @param lat0 A numerical value for centeral latitude
#' @param lat1 A numerical value for standard parallel 1
#' @param lat2 A numerical value for standard parallel 2
#' @param lon0 A numerical value for centeral longitude
#' @param k0 A numerical value for scale factor
#' @param datum A string for the datum used with the coordinates (currently only 'WGS84', 'ETRS89' and 'NAD83' supported)
#' @param unit A string for horizontal coordinate system units (currently only 'm' and 'ft' supported)
#'
#' @returns A list of strings named with `PROJ` & `WKT`
#' @keywords internal
stringLinks <- function(prj, x0=NA_real_, lat0=NA_real_, lat1=NA_real_, lat2=NA_real_, lon0=NA_real_, k0=NA_real_,datum = "WGS84", unit = "m") {
  PROJstr <- "+proj="
  WKTstr <- 'PROJCS["Customized_'

  if(is.na(datum) || is.na(unit)){
    stop("No reference ellipse or unit is provided")
  }

  if (datum == "WGS84") {
    datum_str <- " +datum=WGS84"
    gcs_str <- '\n GEOGCS["GCS_WGS_1984",\n  DATUM["D_WGS_1984",\n   SPHEROID["WGS_1984",6378137.0,298.257223563]],\n  PRIMEM["Greenwich",0.0],\n  UNIT["Degree",0.0174532925199433]],'
  } else if (datum == "ETRS89") {
    datum_str <- " +ellps=GRS80"
    gcs_str <- '\n GEOGCS["GCS_ETRS_1989",\n  DATUM["D_ETRS_1989",\n   SPHEROID["GRS_1980",6378137.0,298.257222101]],\n  PRIMEM["Greenwich",0.0],\n  UNIT["Degree",0.0174532925199433]],'
  } else if (datum == "NAD83") {
    datum_str <- " +datum=NAD83"
    gcs_str <- '\n GEOGCS["GCS_North_American_1983",\n  DATUM["D_North_American_1983",\n   SPHEROID["GRS_1980",6378137.0,298.257222101]],\n  PRIMEM["Greenwich",0.0],\n  UNIT["Degree",0.0174532925199433]],'
  } else {
    stop("Sorry, currently your reference ellipse is not supported")
  }

  if (prj == "latlong") {
    PROJstr <- paste0(PROJstr, "eqc")
  } else {
    PROJstr <- paste0(PROJstr, prj)
  }

  WKTstr <- switch(prj,
                   "aeqd" = paste0(WKTstr, 'Azimuthal_Equidistant",', gcs_str, '\n PROJECTION["Azimuthal_Equidistant"],'),
                   "laea" = paste0(WKTstr, 'Lambert_Azimuthal",', gcs_str, '\n PROJECTION["Lambert_Azimuthal_Equal_Area"],'),
                   "stere" = paste0(WKTstr, 'Stereographic",', gcs_str, '\n PROJECTION["Stereographic"],'),
                   "aea" = paste0(WKTstr, 'Albers",', gcs_str, '\n PROJECTION["Albers"],'),
                   "eqdc" = paste0(WKTstr, 'Equidistant_Conic",', gcs_str, '\n PROJECTION["Equidistant_Conic"],'),
                   "lcc" = paste0(WKTstr, 'Lambert_Conformal_Conic",', gcs_str, '\n PROJECTION["Lambert_Conformal_Conic"],'),
                   "cea" = paste0(WKTstr, 'Cylindrical_Equal_Area",', gcs_str, '\n PROJECTION["Cylindrical_Equal_Area"],'),
                   "merc" = paste0(WKTstr, 'Mercator",', gcs_str, '\n PROJECTION["Mercator"],'),
                   "eqc" = paste0(WKTstr, 'Equidistant_Cylindrical",', gcs_str, '\n PROJECTION["Equidistant_Cylindrical"],'),
                   "tcea" = paste0(WKTstr, 'Transverse_Cylindrical_Equal_Area",', gcs_str, '\n PROJECTION["Transverse_Cylindrical_Equal_Area"],'),
                   "tmerc" = paste0(WKTstr, 'Transverse_Mercator",', gcs_str, '\n PROJECTION["Transverse_Mercator"],'),
                   "cass" = paste0(WKTstr, 'Cassini",', gcs_str, '\n PROJECTION["Cassini"],'),
                   "moll" = paste0(WKTstr, 'Mollweide",', gcs_str, '\n PROJECTION["Mollweide"],'),
                   "hammer" = paste0(WKTstr, 'Hammer_Aitoff",', gcs_str, '\n PROJECTION["Hammer_Aitoff"],'),
                   "eck4" = paste0(WKTstr, 'Eckert_IV",', gcs_str, '\n PROJECTION["Eckert_IV"],'),
                   "eqearth" = paste0(WKTstr, 'Equal_Earth",', gcs_str, '\n PROJECTION["Equal_Earth"],'),
                   "wag4" = paste0(WKTstr, 'Wagner_IV",', gcs_str, '\n PROJECTION["Wagner_IV"],'),
                   "wag7" = paste0(WKTstr, 'Wagner_VII",', gcs_str, '\n PROJECTION["Wagner_VII"],'),
                   "robin" = paste0(WKTstr, 'Robinson",', gcs_str, '\n PROJECTION["Robinson"],'),
                   "natearth" = paste0(WKTstr, 'Natural_Earth",', gcs_str, '\n PROJECTION["Natural_Earth"],'),
                   "wintri" = paste0(WKTstr, 'Winkel_Tripel",', gcs_str, '\n PROJECTION["Winkel_Tripel"],'),
                   "patterson" = paste0(WKTstr, 'Patterson",', gcs_str, '\n PROJECTION["Patterson"],'),
                   "latlong" = paste0(WKTstr, 'Plate_Carree",', gcs_str, '\n PROJECTION["Plate_Carree"],'),
                   "mill" = paste0(WKTstr, 'Miller_Cylindrical",', gcs_str, '\n PROJECTION["Miller_Cylindrical"],'),
                   "tpeqd" = paste0(WKTstr, 'Two_Point_Equidistant",', gcs_str, '\n PROJECTION["Two_Point_Equidistant"],'),
                   "ortho" = paste0(WKTstr, 'Orthographic",', gcs_str, '\n PROJECTION["Orthographic"],')
  )

  if (!is.na(x0)) {
    PROJstr <- paste0(PROJstr, " +x_0=", x0)
    WKTstr <- paste0(WKTstr, '\n PARAMETER["False_Easting",', x0, '],\n PARAMETER["False_Northing",0.0],')
  } else {
    WKTstr <- paste0(WKTstr, '\n PARAMETER["False_Easting",0.0],\n PARAMETER["False_Northing",0.0],')
  }

  lat0 <- round(lat0 * 1e7) / 1e7
  lat1 <- round(lat1 * 1e7) / 1e7
  lat2 <- round(lat2 * 1e7) / 1e7
  lon0 <- round(lon0 * 1e7) / 1e7

  # Other proj parameters
  switch(prj,
         "aeqd" = ,
         "ortho" = ,
         "laea" = {
           PROJstr <- paste0(PROJstr, " +lon_0=", lon0, " +lat_0=", lat0)
           WKTstr <- paste0(WKTstr, '\n PARAMETER["Central_Meridian",', lon0, '],\n PARAMETER["Latitude_Of_Origin",', lat0, '],')
         },
         "stere" = {
           if (is.na(k0)) {
             PROJstr <- paste0(PROJstr, " +lon_0=", lon0, " +lat_0=", lat0)
             WKTstr <- paste0(WKTstr, '\n PARAMETER["Central_Meridian",', lon0, '],\n PARAMETER["Scale_Factor",1.0],\n PARAMETER["Latitude_Of_Origin",', lat0, '],')
           } else {
             PROJstr <- paste0(PROJstr, " +lon_0=", lon0, " +lat_0=", lat0, " +k_0=", k0)
             WKTstr <- paste0(WKTstr, '\n PARAMETER["Central_Meridian",', lon0, '],\n PARAMETER["Scale_Factor",', k0, '],\n PARAMETER["Latitude_Of_Origin",', lat0, '],')
           }
         },
         "aea" = ,
         "eqdc" = ,
         "lcc" = {
           PROJstr <- paste0(PROJstr, " +lon_0=", lon0, " +lat_1=", lat1, " +lat_2=", lat2, " +lat_0=", lat0)
           WKTstr <- paste0(WKTstr, '\n PARAMETER["Central_Meridian",', lon0, '],\n PARAMETER["Standard_Parallel_1",', lat1, '],\n PARAMETER["Standard_Parallel_2",', lat2, '],\n PARAMETER["Latitude_Of_Origin",', lat0, '],')
         },
         "cea" = ,
         "eqc" = ,
         "merc" = {
           PROJstr <- paste0(PROJstr, " +lon_0=", lon0, " +lat_ts=", lat1)
           WKTstr <- paste0(WKTstr, '\n PARAMETER["Central_Meridian",', lon0, '],\n PARAMETER["Standard_Parallel_1",', lat1, '],')
         },
         "tcea" = ,
         "tmerc" = ,
         "cass" = {
           if (is.na(k0)) {
             PROJstr <- paste0(PROJstr, " +lon_0=", lon0)
             WKTstr <- paste0(WKTstr, '\n PARAMETER["Central_Meridian",', lon0, '],\n PARAMETER["Scale_Factor",1.0],\n PARAMETER["Latitude_Of_Origin",0.0],')
           } else {
             PROJstr <- paste0(PROJstr, " +lon_0=", lon0, " +k_0=", k0)
             WKTstr <- paste0(WKTstr, '\n PARAMETER["Central_Meridian",', lon0, '],\n PARAMETER["Scale_Factor",', k0, '],\n PARAMETER["Latitude_Of_Origin",0.0],')
           }
         },
         "moll" = ,
         "hammer" = ,
         "eck4" = ,
         "eqearth" = ,
         "wag4" = ,
         "wag7" = ,
         "robin" = ,
         "natearth" = ,
         "patterson" = ,
         "latlong" = ,
         "mill" = {
           PROJstr <- paste0(PROJstr, " +lon_0=", lon0)
           WKTstr <- paste0(WKTstr, '\n PARAMETER["Central_Meridian",', lon0, '],')
         },
         "wintri" = {
           PROJstr <- paste0(PROJstr, " +lon_0=", lon0)
           WKTstr <- paste0(WKTstr, '\n PARAMETER["Central_Meridian",', lon0, '],\n PARAMETER["Standard_Parallel_1",50.467],')
         }
  )

  if (unit == "m") {
    PROJstr <- paste0(PROJstr, datum_str, " +units=m +no_defs")
    WKTstr <- paste0(WKTstr, '\n UNIT["Meter",1.0]]')
  } else if (unit == "ft") {
    PROJstr <- paste0(PROJstr, datum_str, " +units=ft +no_defs")
    WKTstr <- paste0(WKTstr, '\n UNIT["Foot",0.3048]]')
  } else {
    stop("Sorry, currently only 'm' & 'ft' supported as proj unit")
  }

  return(list(PROJ = PROJstr, WKT = WKTstr))
}
