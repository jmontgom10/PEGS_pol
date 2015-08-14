FUNCTION TEST_CROWDED, x, y, neighborLimit

  numStars = N_ELEMENTS(x)
  ;First compute the inter-star separations
  distances = FLTARR(numStars, numStars)                              ;Create array to star inter-star separations
  FOR i = 0, numStars - 1 DO BEGIN                                    ;Loop through ALL the provided stars
    distances[*,i] =  SQRT((x - x[i])^2 + (y - y[i])^2)               ;Compute inter-star separations
  ENDFOR
  
  ;Next look for neighbors closer than 5*FWHM
  crowdedStars = INTARR(numStars)                                    ;Assume all the stars pass the test
  FOR i = 1, numStars - 1 DO BEGIN                                    ;Loop through stars and check nearest neighbors
    IF MIN(distances[i:numStars-1,i-1]) LT neighborLimit $             ;Check if any too close neighbors
      THEN crowdedStars[i] = 1                                        ;If the nearest neighbor is too close, mark as bad
  ENDFOR
RETURN, crowdedStars
END