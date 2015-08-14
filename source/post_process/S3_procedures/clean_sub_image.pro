FUNCTION CLEAN_SUB_IMAGE, x

  
  
  
  
  ;    ******THERE MUST BE A SIMPLER WAY******
  ;    phpadu = 8.21
  ;    ronois = phpadu* skyNoise                                         ;Assume read-out-roise is just the sky-noise
  ;    apr    = 2.5*PSF_FWHM
  ;    skyrad = [1.5, 2.5]*apr
  ;    APER, subImg, xStars, yStars, starFlux, $
  ;      errf, skyf, skyerrf, phpadu, apr, skyrad, badpix, /FLUX, /SILENT
  ;    GETPSF, subImg, xStars,yStars, starFlux, skyf, ronois, $
  ;      phpadu, gauss, psf, idpsf, psfrad, $
  ;      fitrad,psfname, DEBUG = debug
  ;************************************************
  
  numStars     = N_ELEMENTS(fluxStars)                             ;Count the stars in the subImg
  imgSz        = SIZE(subImg, /DIMENSIONS)                            ;Measure the image size
  cleanImg     = subImg                                               ;Alias the input image
  removedStars = INTARR(numStars)                                   ;BOOL array for which stars have been removed
  FOR i = 0, numStars - 1 DO BEGIN
;    gfit        = GAUSS2DFIT(cleanImg, coeffs, /TILT)
;    xStar       = coeffs[4]
;    yStar       = coeffs[5]
;    starOnImage = ((xStar GT -1) AND (xStar LT imgSz[0])) AND $
;      ((yStar GT - 1) AND (yStar LT imgSz[1]))
;    IF starOnImage THEN BEGIN
;    stop
;      foundStarDist  = SQRT((xStars - xStar)^2 + (yStars - yStar)^2)
;      thisStar       = WHERE(foundStarDist EQ MIN(foundStarDist))
;      centerDistance = SQRT((0.5*imgSz[0] - xStar)^2 + (0.5*imgSz[1] - yStar)^2)
;      IF (foundStarDist[thisStar] LT 0.5*PSF_FWHM) AND $              ;Check if the fit star is an expected star
;        (removedStars[thisStar] EQ 0) THEN BEGIN                      ;And has not yet been removed
;        IF (centerDistance LT 0.5*PSF_FWHM) THEN BEGIN                ;Check if this is the central star
;          photoStar              = gfit                               ;Store the photometric image for later
;          cleanImg              -= gfit                                 ;Remove the fitted star from image
;          removedStars[thisStar] = 1                                    ;Mark this star as found
;        ENDIF ELSE BEGIN
;          cleanImg              -= gfit                                 ;Clean out the fitted star
;          removedStars[thisStar] = 1
;        ENDELSE
;      ENDIF
;    ENDIF ELSE BEGIN
;      zoomStar = MIN(WHERE(~removedStars, count))                    ;Pick a star to zoom in on
      centerDistance = SQRT((0.5*imgSz[0] - xStars[i])^2 + $
        (0.5*imgSz[1] - yStars[i])^2)
      IF (centerDistance LT 0.5*PSF_FWHM) THEN BEGIN
        photoL    = xStars[i] - 3*PSF_FWHM > 0                        ;Left side cut index
        photoR    = xStars[i] + 3*PSF_FWHM < (imgSz[0] - 1)           ;Right side cut index
        photoB    = yStars[i] - 3*PSF_FWHM > 0                        ;Bottom side cut index
        photoT    = yStars[i] + 3*PSF_FWHM < (imgSz[1] - 1)           ;Top side cut index
        gfit      = GAUSS2DFIT(cleanImg[photoL:photoR,photoB:photoT], coeffs, /TILT)
        photoStar = gfit
      ENDIF
      cutL = xStars[i] - PSF_FWHM > 0                                 ;Left side cut index
      cutR = xStars[i] + PSF_FWHM < (imgSz[0] - 1)                    ;Right side cut index
      cutB = yStars[i] - PSF_FWHM > 0                                 ;Bottom side cut index
      cutT = yStars[i] + PSF_FWHM < (imgSz[1] - 1)                    ;Top side cut index
      gfit = GAUSS2DFIT(cleanImg[cutL:cutR,cutB:cutT], coeffs, /TILT)
      cleanImg[cutL:cutR,cutB:cutT] -= gfit                             ;Remove fitted star
;      removedStars[zoomStar] = 1                                        ;Mark this star as removed
;    ENDELSE
  ENDFOR
  cleanImg[photoL:photoR,photoB:photoT] += photoStar

  done = 0
  WHILE ~done DO BEGIN
    gfit = GAUSS2DFIT(cleanImg, coeffs, /TILT)  
    xStar = coeffs[4]
    yStar = coeffs[5]
    centerDistance = SQRT((0.5*imgSz[0] - xStar)^2 + $
      (0.5*imgSz[1] - yStar)^2)
    IF (centerDistance LT 0.5*PSF_FWHM) THEN done = 1 ELSE cleanImg -= gfit
  ENDWHILE

;    radDist = SQRT((xStar - 8*PSF_FWHM)^2 $
;      + (yStar - 8*PSF_FWHM)^2)
;    IF radDist LT 0.5*PSF_FWHM THEN BEGIN                     ;If this is the photometry star
;      photoFit  = gfit                                          ;Store for later restoration
;      cleanImg -= gfit                                          ;Remove the central star from image
;    ENDIF ELSE BEGIN
;      cleanImg -= gfit                                          ;Remove the peripheral stars from the image
;    ENDELSE
;    stop
;      IF MIN(foundStarDist) LE 0.5*PSF_FWHM THEN numStarsRemoved++
;    ENDWHILE
;    cleanImg +=  photoFit                                         ;Recontribute the removal of the central star
;  ENDIF

  RETURN, cleanImg
END