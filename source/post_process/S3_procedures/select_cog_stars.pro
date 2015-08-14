FUNCTION SELECT_COG_STARS, image, xStars, yStars, fluxStars, fwhm

  ;Star(s) used to generate a curve of growth must have the following properties
  ;1. Not saturated (otherwise curve of growth is all wrong)
  ;2. Isolated (so that increase in aperture is only incorporates THAT star's flux)
  ;3. ??????THERE'S PROBABLY SOMETHING ELSE???????

  saturated = TEST_SATURATED(image, xStars, yStars, 6000.0, 4*fwhm)
  crowded   = TEST_CROWDED(xStars, yStars, 12*fwhm)
  COGstars  = (~saturated AND ~crowded)

  RETURN, COGstars
END