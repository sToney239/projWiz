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
      "Plate_Carrée" = "latlong" # easier to calculate
    )
  )
)

# strongly recommend to go to "https://projectionwizard.org" to select one
# desired_proj = "latlong"
# desired_proj = world_proj_list$compromise$round_boudnary$Natural_Earth

proj_world <- function(world_proj, lng) {
  stringLinks(world_proj, NaN, NaN, NaN, NaN, lng, NaN)
}


