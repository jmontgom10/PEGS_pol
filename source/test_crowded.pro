FUNCTION TEST_CROWDED, x, y, neighborLimit

  numStars = N_ELEMENTS(x)
  ;First compute the inter-star separations
  distances = FLTARR(numStars, numStars)                              ;Create array to star inter-star separations
  FOR i = 0, numStars - 1 DO BEGIN                                    ;Loop through ALL the provided stars
    distances[*,i] =  SQRT((x - x[i])^2 + (y - y[i])^2)               ;Compute inter-star separations
  ENDFOR
  
  ;Next look for neighbors closer than 5*FWHM
  crowdedStars = LONARR(numStars)                                    ;Assume all the stars pass the test
  FOR i = 1, numStars - 1 DO BEGIN                                   ;Loop through stars and check nearest neighbors
    IF i EQ 0 THEN $
      searchInds = LINDGEN(numStars - 1) + 1
    IF (i GT 0) AND (i LT numStars - 1) THEN $
      searchInds = [LINDGEN(i), i + 1 + LINDGEN(numStars - 1 - i)]
    IF i EQ numStars - 1 THEN $
      searchInds = LINDGEN(numStars - 1)

    IF MIN(distances[i, searchInds]) LT neighborLimit $              ;Check if any too close neighbors
      THEN crowdedStars[i] = 1                                       ;If the nearest neighbor is too close, mark as bad
  ENDFOR
RETURN, crowdedStars
END