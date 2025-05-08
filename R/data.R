#' World Map projection list
#'
#' @format
#' A list with popular world projections:
#'    - `equal_area`: Projections to keep the area property approximately correct
#'        - `point_polar`: Equal-area world map projections with poles represented as points
#'        - `line_polar`: Equal-area world map projections with poles represented as lines
#'    - `compromise`: Projections compromising shape and area
#'        - `round_boudnary`: Boundary on east and west as curve
#'        - `rectangular`: Boundary as rectange
#'
#' @source <https://github.com/ProjectionWizard/projectionwizard.github.io/blob/263b9ff09128e371ee923ab57bfc1ed41bbdc4ba/outputFormat.js#L115>
world_proj_list <- list(
  "equal_area" = list(
    # Equal-area world map projections with poles represented as points
    "point_polar" = list(
      "Mollweide"="moll", # popular for atlases
      "Hammer_Aitoff"= "hammer" # curved parallels
    ),
    # Equal-area world map projections with poles represented as lines
    "line_polar"=list(
      "Equal_Earth"= "eqearth", # looks good
      "Eckert_IV"= "eck4", # low mean angular distortion
      "Wagner_IV" = "wag4",
      "Wagner_VII"= "wag7"
    )
  ),
  "compromise" = list(
    # Compromise world map projections
    "round_boudnary" = list(
      "Natural_Earth"= "natearth", # looks good
      "Robinson" = "robin", # commonly used
      "Winkel_Tripel" = "wintri"
    ),
    "rectangular" = list(
      "Miller_cylindrical_I" = "mill", # most recommended
      "Patterson"= "patterson", # take less space to print
      "Plate_Carree" = "latlong" # easier to calculate
    )
  )
)
