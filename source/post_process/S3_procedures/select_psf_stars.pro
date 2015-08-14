FUNCTION SELECT_PSF_STARS, image, xStars, yStars, fluxStars, fwhm
  
  ; A. double check that the star list is brightness sorted
  brightSort = REVERSE(SORT(fluxStars))
  x     = xStars[brightSort]
  y     = yStars[brightSort]
  flux  = fluxStars[brightSort]

  ; B. rejects bright stars with saturated pixels, low flux, and crowded
  saturated = TEST_SATURATED(image, xStars, yStars, 6000.0, 4*fwhm)
  lowFlux   = TEST_LOW_FLUX(image, xStars, yStars, 800.0, 2*fwhm)
  crowded   = TEST_CROWDED(xStars, yStars, 8*fwhm)
  goodStars = WHERE(~saturated AND ~lowFlux AND ~crowded, numGood)
  x         = x[goodStars]
  y         = y[goodStars]
  flux      = flux[goodStars]

  ; C. checks to be sure no PSF stars are within 12 FWHM of any other PSF star
  PSFstars = INTARR(numGood)
  PSFstars[MIN(WHERE(~saturated))] = 1                                ;Set the brightest unsaturated star as first star
  FOR i = 0, numGood - 1 DO BEGIN
    IF saturated[i] OR crowded[i] THEN CONTINUE                       ;Skip over saturated stars
    indPSFstars  = WHERE(PSFstars)
    distPSFstars = SQRT((x[indPSFstars] - xStars[i])^2 + $
                        (y[indPSFstars] - xStars[i])^2)
    IF MIN(distPSFstars) GT 12*fwhm THEN BEGIN
      ; D. does gaussian fit to trial PSF star, rejecting any with FWHMs too small or too wide
      thisX  = x[i]                                                   ;Round star locations to nearest pixel
      thisY  = y[i]                                                   ;Round star locations to nearest pixel
      delXY  = 2*FWHM                                                 ;Define array width and height
      subImg = image[ROUND(thisX-delXY):ROUND(thisX+delXY), $         ;Grab array centered around the star
        ROUND(thisY-delXY):ROUND(thisY+delXY)]
      gfit   = GAUSS2DFIT(subImg, coeffs, /TILT)
      xStar  = coeffs[4] + (ROUND(thisX-delXY))                       ;Pick off the fitted star x position
      yStar  = coeffs[5] + (ROUND(thisY-delXY))                       ;Pick off the fitted star y position
      starFitDist = SQRT((thisX - xStar)^2 + (thisY - yStar)^2)       ;Compute offset of fitted gaussian
      thisFWHM    = (2*SQRT(2*ALOG(2)))*SQRT(coeffs[2]*coeffs[3])     ;Compute FWHM for THIS star
      IF (starFitDist LT 0.5*FWHM) AND $                              ;Check that given position and fit position match
        (thisFWHM/FWHM) LT 2.0 AND $                                  ;Check that THIS FWHM isn't too wide
        (thisFWHM/FWHM) GT 0.5 THEN $                                 ;Check that THIS FWHM isn't too narrow
        PSFstars[i] = 1                                               ;Mark this star as a good PSF star
    ENDIF
  ENDFOR

  RETURN, PSFstars
END