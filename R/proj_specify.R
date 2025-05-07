# note that world proj is not supported here
# please use proj_world directly
proj_specify <- function(input_sf, prj, output_type = "proj4", datum = "WGS84",unit = "m", x0 = NA_real_) {
  
  new_boundary = st_bbox(input_sf)
  lonmax = new_boundary$xmax
  lonmin = new_boundary$xmin
  latmax = new_boundary$ymax
  latmin = new_boundary$ymin
  lat0 = (latmax + latmin) / 2
  lon0 = (lonmax + lonmin) / 2
  interval <- (latmax - latmin) / 6
  lat1 = latmin + interval
  lat2 = latmax - interval
  k0 = NA_real_
  
  PROJstr <- "+proj="
  WKTstr <- 'PROJCS[\\\"ProjWiz_Custom_'
  # FORMATING GEOGRAPHIC\GEODETIC DATUM
  # Assuming document.getElementById("datum").value is replaced by a variable 'datum' passed to the function
  # datum <- document.getElementById("datum").value # This needs to be passed as an argument
  # Example usage: stringLinks("aeqd", 0, 0, 0, 0, 0, 1, datum = "WGS84", unit = "m")
  
  #Checking if datum and unit are defined, if not return ""
  
  if(is.na(datum) || is.na(unit)){
    stop("No reference ellipse or unit is provided")
  }
  
  # PROJ and WKT strings
  if (datum == "WGS84") {
    datum_str <- " +datum=WGS84"
    gcs_str <- '</br>&nbsp;GEOGCS[\\\"GCS_WGS_1984\\\",</br>&nbsp;&nbsp;DATUM[\\\"D_WGS_1984\\\",</br>&nbsp;&nbsp;&nbsp;SPHEROID[\\\"WGS_1984\\\",6378137.0,298.257223563]],</br>&nbsp;&nbsp;PRIMEM[\\\"Greenwich\\\",0.0],</br>&nbsp;&nbsp;UNIT[\\\"Degree\\\",0.0174532925199433]],'
  } else if (datum == "ETRS89") {
    datum_str <- " +ellps=GRS80"
    gcs_str <- '</br>&nbsp;GEOGCS[\\\"GCS_ETRS_1989\\\",</br>&nbsp;&nbsp;DATUM[\\\"D_ETRS_1989\\\",</br>&nbsp;&nbsp;&nbsp;SPHEROID[\\\"GRS_1980\\\",6378137.0,298.257222101]],</br>&nbsp;&nbsp;PRIMEM[\\\"Greenwich\\\",0.0],</br>&nbsp;&nbsp;UNIT[\\\"Degree\\\",0.0174532925199433]],'
  } else if (datum == "NAD83") {
    datum_str <- " +datum=NAD83"
    gcs_str <- '</br>&nbsp;GEOGCS[\\\"GCS_North_American_1983\\\",</br>&nbsp;&nbsp;DATUM[\\\"D_North_American_1983\\\",</br>&nbsp;&nbsp;&nbsp;SPHEROID[\\\"GRS_1980\\\",6378137.0,298.257222101]],</br>&nbsp;&nbsp;PRIMEM[\\\"Greenwich\\\",0.0],</br>&nbsp;&nbsp;UNIT[\\\"Degree\\\",0.0174532925199433]],'
  } else {
    stop("Sorry, currently your reference ellipse is not supported")
  }
  # FORMATING PROJECTION
  # PROJ string
  if (prj == "latlong") {
    PROJstr <- paste0(PROJstr, "eqc")
  } else {
    PROJstr <- paste0(PROJstr, prj)
  }
  
  # WKT string
  WKTstr <- switch(prj,
                   "aeqd" = paste0(WKTstr, 'Azimuthal_Equidistant\\\",', gcs_str, '</br>&nbsp;PROJECTION[\\\"Azimuthal_Equidistant\\\"],'),
                   "laea" = paste0(WKTstr, 'Lambert_Azimuthal\\\",', gcs_str, '</br>&nbsp;PROJECTION[\\\"Lambert_Azimuthal_Equal_Area\\\"],'),
                   "stere" = paste0(WKTstr, 'Stereographic\\\",', gcs_str, '</br>&nbsp;PROJECTION[\\\"Stereographic\\\"],'),
                   "aea" = paste0(WKTstr, 'Albers\\\",', gcs_str, '</br>&nbsp;PROJECTION[\\\"Albers\\\"],'),
                   "eqdc" = paste0(WKTstr, 'Equidistant_Conic\\\",', gcs_str, '</br>&nbsp;PROJECTION[\\\"Equidistant_Conic\\\"],'),
                   "lcc" = paste0(WKTstr, 'Lambert_Conformal_Conic\\\",', gcs_str, '</br>&nbsp;PROJECTION[\\\"Lambert_Conformal_Conic\\\"],'),
                   "cea" = paste0(WKTstr, 'Cylindrical_Equal_Area\\\",', gcs_str, '</br>&nbsp;PROJECTION[\\\"Cylindrical_Equal_Area\\\"],'),
                   "merc" = paste0(WKTstr, 'Mercator\\\",', gcs_str, '</br>&nbsp;PROJECTION[\\\"Mercator\\\"],'),
                   "eqc" = paste0(WKTstr, 'Equidistant_Cylindrical\\\",', gcs_str, '</br>&nbsp;PROJECTION[\\\"Equidistant_Cylindrical\\\"],'),
                   "tcea" = paste0(WKTstr, 'Transverse_Cylindrical_Equal_Area\\\",', gcs_str, '</br>&nbsp;PROJECTION[\\\"Transverse_Cylindrical_Equal_Area\\\"],'),
                   "tmerc" = paste0(WKTstr, 'Transverse_Mercator\\\",', gcs_str, '</br>&nbsp;PROJECTION[\\\"Transverse_Mercator\\\"],'),
                   "cass" = paste0(WKTstr, 'Cassini\\\",', gcs_str, '</br>&nbsp;PROJECTION[\\\"Cassini\\\"],'),
                   "moll" = paste0(WKTstr, 'Mollweide\\\",', gcs_str, '</br>&nbsp;PROJECTION[\\\"Mollweide\\\"],'),
                   "hammer" = paste0(WKTstr, 'Hammer_Aitoff\\\",', gcs_str, '</br>&nbsp;PROJECTION[\\\"Hammer_Aitoff\\\"],'),
                   "eck4" = paste0(WKTstr, 'Eckert_IV\\\",', gcs_str, '</br>&nbsp;PROJECTION[\\\"Eckert_IV\\\"],'),
                   "eqearth" = paste0(WKTstr, 'Equal_Earth\\\",', gcs_str, '</br>&nbsp;PROJECTION[\\\"Equal_Earth\\\"],'),
                   "wag4" = paste0(WKTstr, 'Wagner_IV\\\",', gcs_str, '</br>&nbsp;PROJECTION[\\\"Wagner_IV\\\"],'),
                   "wag7" = paste0(WKTstr, 'Wagner_VII\\\",', gcs_str, '</br>&nbsp;PROJECTION[\\\"Wagner_VII\\\"],'),
                   "robin" = paste0(WKTstr, 'Robinson\\\",', gcs_str, '</br>&nbsp;PROJECTION[\\\"Robinson\\\"],'),
                   "natearth" = paste0(WKTstr, 'Natural_Earth\\\",', gcs_str, '</br>&nbsp;PROJECTION[\\\"Natural_Earth\\\"],'),
                   "wintri" = paste0(WKTstr, 'Winkel_Tripel\\\",', gcs_str, '</br>&nbsp;PROJECTION[\\\"Winkel_Tripel\\\"],'),
                   "patterson" = paste0(WKTstr, 'Patterson\\\",', gcs_str, '</br>&nbsp;PROJECTION[\\\"Patterson\\\"],'),
                   "latlong" = paste0(WKTstr, 'Plate_Carree\\\",', gcs_str, '</br>&nbsp;PROJECTION[\\\"Plate_Carree\\\"],'),
                   "mill" = paste0(WKTstr, 'Miller_Cylindrical\\\",', gcs_str, '</br>&nbsp;PROJECTION[\\\"Miller_Cylindrical\\\"],'),
                   "tpeqd" = paste0(WKTstr, 'Two_Point_Equidistant\\\",', gcs_str, '</br>&nbsp;PROJECTION[\\\"Two_Point_Equidistant\\\"],'),
                   "ortho" = paste0(WKTstr, 'Orthographic\\\",', gcs_str, '</br>&nbsp;PROJECTION[\\\"Orthographic\\\"],')
  )
  
  if (!is.na(x0)) {
    PROJstr <- paste0(PROJstr, " +x_0=", x0)
    WKTstr <- paste0(WKTstr, '</br>&nbsp;PARAMETER[\\\"False_Easting\\\",', x0, '],</br>&nbsp;PARAMETER[\\\"False_Northing\\\",0.0],')
  } else {
    WKTstr <- paste0(WKTstr, '</br>&nbsp;PARAMETER[\\\"False_Easting\\\",0.0],</br>&nbsp;PARAMETER[\\\"False_Northing\\\",0.0],')
  }
  
  # Format output values
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
           WKTstr <- paste0(WKTstr, '</br>&nbsp;PARAMETER[\\\"Central_Meridian\\\",', lon0, '],</br>&nbsp;PARAMETER[\\\"Latitude_Of_Origin\\\",', lat0, '],')
         },
         "stere" = {
           if (is.na(k0)) {
             PROJstr <- paste0(PROJstr, " +lon_0=", lon0, " +lat_0=", lat0)
             WKTstr <- paste0(WKTstr, '</br>&nbsp;PARAMETER[\\\"Central_Meridian\\\",', lon0, '],</br>&nbsp;PARAMETER[\\\"Scale_Factor\\\",1.0],</br>&nbsp;PARAMETER[\\\"Latitude_Of_Origin\\\",', lat0, '],')
           } else {
             PROJstr <- paste0(PROJstr, " +lon_0=", lon0, " +lat_0=", lat0, " +k_0=", k0)
             WKTstr <- paste0(WKTstr, '</br>&nbsp;PARAMETER[\\\"Central_Meridian\\\",', lon0, '],</br>&nbsp;PARAMETER[\\\"Scale_Factor\\\",', k0, '],</br>&nbsp;PARAMETER[\\\"Latitude_Of_Origin\\\",', lat0, '],')
           }
         },
         "aea" = ,
         "eqdc" = ,
         "lcc" = {
           PROJstr <- paste0(PROJstr, " +lon_0=", lon0, " +lat_1=", lat1, " +lat_2=", lat2, " +lat_0=", lat0)
           WKTstr <- paste0(WKTstr, '</br>&nbsp;PARAMETER[\\\"Central_Meridian\\\",', lon0, '],</br>&nbsp;PARAMETER[\\\"Standard_Parallel_1\\\",', lat1, '],</br>&nbsp;PARAMETER[\\\"Standard_Parallel_2\\\",', lat2, '],</br>&nbsp;PARAMETER[\\\"Latitude_Of_Origin\\\",', lat0, '],')
         },
         "cea" = ,
         "eqc" = ,
         "merc" = {
           PROJstr <- paste0(PROJstr, " +lon_0=", lon0, " +lat_ts=", lat1)
           WKTstr <- paste0(WKTstr, '</br>&nbsp;PARAMETER[\\\"Central_Meridian\\\",', lon0, '],</br>&nbsp;PARAMETER[\\\"Standard_Parallel_1\\\",', lat1, '],')
         },
         "tcea" = ,
         "tmerc" = ,
         "cass" = {
           if (is.na(k0)) {
             PROJstr <- paste0(PROJstr, " +lon_0=", lon0)
             WKTstr <- paste0(WKTstr, '</br>&nbsp;PARAMETER[\\\"Central_Meridian\\\",', lon0, '],</br>&nbsp;PARAMETER[\\\"Scale_Factor\\\",1.0],</br>&nbsp;PARAMETER[\\\"Latitude_Of_Origin\\\",0.0],')
           } else {
             PROJstr <- paste0(PROJstr, " +lon_0=", lon0, " +k_0=", k0)
             WKTstr <- paste0(WKTstr, '</br>&nbsp;PARAMETER[\\\"Central_Meridian\\\",', lon0, '],</br>&nbsp;PARAMETER[\\\"Scale_Factor\\\",', k0, '],</br>&nbsp;PARAMETER[\\\"Latitude_Of_Origin\\\",0.0],')
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
           WKTstr <- paste0(WKTstr, '</br>&nbsp;PARAMETER[\\\"Central_Meridian\\\",', lon0, '],')
         },
         "wintri" = {
           PROJstr <- paste0(PROJstr, " +lon_0=", lon0)
           WKTstr <- paste0(WKTstr, '</br>&nbsp;PARAMETER[\\\"Central_Meridian\\\",', lon0, '],</br>&nbsp;PARAMETER[\\\"Standard_Parallel_1\\\",50.467],')
         },
         "tpeqd" = {
           PROJstr <- paste0(PROJstr, " +lat_1=", lat0, " +lon_1=", lat1, " +lat_2=", lat2, " +lon_2=", lon0)
           WKTstr <- paste0(WKTstr, '</br>&nbsp;PARAMETER[\\\"Latitude_Of_1st_Point\\\",', lat0, '],</br>&nbsp;PARAMETER[\\\"Latitude_Of_2nd_Point\\\",', lat2, '],</br>&nbsp;PARAMETER[\\\"Longitude_Of_1st_Point\\\",', lat1, '],</br>&nbsp;PARAMETER[\\\"Longitude_Of_2nd_Point\\\",', lon0, '],')
         }
  )
  
  if (unit == "m") {
    PROJstr <- paste0(PROJstr, datum_str, " +units=m +no_defs")
    WKTstr <- paste0(WKTstr, '</br>&nbsp;UNIT[\\\"Meter\\\",1.0]]')
  } else if (unit == "ft") {
    PROJstr <- paste0(PROJstr, datum_str, " +units=ft +no_defs")
    WKTstr <- paste0(WKTstr, '</br>&nbsp;UNIT[\\\"Foot\\\",0.3048]]')
  } else {
    stop("Sorry, currently only 'm' & 'ft' supported as proj unit")
  }
  if(output_type == "proj4") {
    return(PROJstr)
  } else {
    return(WKTstr)
  }
}

