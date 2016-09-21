FUNCTION GET_OPTIMUM_APERTURES, image, xStars, yStars, starradii, skyradii, badpix

  phpadu    = 8.21                                                    ;This value can be found on Mimir website
  ronois    = 17.8                                                    ;(elec) This value is from Mimir website
;  ronois   = 3.1                                                     ;(ADU) This value is from GPIPS code "S4_PSF_fit"
;  badpix    = [-300L, 6000L]
;  badpix    = [-300L, 12000L]
  
;  x = xStars                                                          ;Alias the xPos vector
;  y = yStars                                                          ;Alias the yPos vector
;  
;  saturated = TEST_SATURATED(image, x, y, $                           ;Test for saturated stars
;    MAX(badpix), CEIL(SQRT(2)*starradii[1]))
;  unSatInd = WHERE(~saturated, numStars)                              ;grab the Unsaturated indices
  
  ;Don't cull the star list, just the saturated star list for later use?
;  x = x[unSatInd]                                                     ;Remove saturated stars from the list
;  y = y[unSatInd]

  ;Assume already tested for crowding
;  crowdedStars = TEST_CROWDED(x, y, CEIL(2*starradii[1]))             ;Find crowded stars
;  unCrowdInd   = WHERE(~crowdedStars, numUnCrowd)
;  
;  x = x[unCrowdInd]                                                   ;Remove the crowded stars from the list
;  y = y[unCrowdInd]

;  IF FLOAT(numUnCrowd)/FLOAT(numStars) LT 0.7 THEN BEGIN
;    PRINT, "Star field is too crowded." + new_line() $
;      + "Return and raise star cutoff"
;    RETURN, -1
;  ENDIF
  
  nStars      = N_ELEMENTS(xStars)
  optimumAprs = FLTARR(nStars)
  FOR i = 0, nStars - 1 DO BEGIN
;    spanRad  = 3.0                                          ;Number of FWHM the aperture should span
;    deltaRad = spanRad*PSF_FWHM/11.0                        ;Starting aperture increments (spans 3 FWHM)
;    midRad   = 2.0*PSF_FWHM + 5.5*deltaRad                  ;Starting midrange aperture
    skipStar = 0
    minRad   = starradii[0]
    maxRad   = starradii[1]

    FOR iLoop = 0, 2 DO BEGIN
      IF (skipStar EQ 1) THEN CONTINUE
      ;Begin each loop by computing the apertures for which to measure the flux
      deltaRad = ((maxRad - minRad)/11E)
      SNRapr   = minRad + deltaRad*FINDGEN(12)

      ;Grab the aperture photometry
      APER, image, xStars[i], yStars[i], $
        flux, errap, sky, skyerr, phpadu, SNRapr, skyradii, badpix, /SILENT, /FLUX
      
      ;And use the result to compute the signal-to-noise ratio at each aperture
      SNRs = flux/errap                                     ;Compute the signal-to-noise

;      ;Plot for debugging
;      wdelete, 21
;      window, 21
;      PLOT, SNRapr, SNRs, PSYM = -4, YRANGE=[MIN(SNRs), MAX(SNRs)]
;      stop
      
      ;Identify where there was success in finding the aperture radius
      goodRad   = WHERE(flux NE 0, numGood)
      maxSNRind = (WHERE(SNRs EQ MAX(SNRs), numMax))[0]
      IF (numGood GT 0) AND $
         (numMax LE 2) AND $
         (maxSNRind EQ 11) THEN BEGIN                       ;If the maximum SNR is at the largest APR
        minRad    = SNRapr[maxSNRind]                       ;Compute the bottom of the new apr range
        maxRad    = 2*maxRad                                ;and the top too
        IF maxRad GE 40 THEN skipStar = 1                   ;Skip stars with aperature greater than 40 pixels
      ENDIF ELSE IF (numGood GT 0) AND $
        (numMax LE 2) AND $
        (maxSNRind GE 1) THEN BEGIN                         ;If the maximum SNR was successfully identified...
        minRad    = SNRapr[maxSNRind-1]                     ;Compute the bottom of the new apr range
        maxRad    = SNRapr[maxSNRind+1]                     ;and the top too
        IF (maxRad - minRad) LE 3.0 THEN done = 1           ;Declare success if we've found max within 3 pixels
      ENDIF ELSE BEGIN
        ;I should recompute minRad and maxRad to be the smallest and greatest good apertures, respectively...
        stop
;        minRad = SNRapr[MIN(goodRad)]
;        maxRad = SNRapr[MAX(goodRad)]
        optimumAprs[i] = !VALUES.F_NAN
        skipStar       = 1
      ENDELSE
    ENDFOR
    
    IF (skipStar EQ 1) THEN CONTINUE
    
    reducedChi2 = FLTARR(3)
    FOR degree = 1, 3 DO BEGIN
      polyCoeffs            = POLY_FIT(SNRapr, SNRs, degree, YBAND = yBand, YFIT = yFit)
      reducedChi2[degree-1] = TOTAL(((SNRs - yFit)/yBand)^2)/FLOAT(12 - degree)
    ENDFOR

    ;Compute the coefficients for the best fitting polynomial
    bestDegree = (WHERE(reducedChi2 EQ MIN(reducedChi2)))[0] + 1
    polyCoeffs = POLY_FIT(SNRapr, SNRs, bestDegree, YBAND = yBand, YFIT = yFit)

    IF bestDegree GT 1 THEN BEGIN
      ;Compute the derivatives of this polynomial and find the maximum of the fit
      deriv1PolyCoeffs = (1E + FINDGEN(bestDegree))*polyCoeffs[1:*]
      roots            = FZ_ROOTS(deriv1PolyCoeffs)
      maximumRootInd   = 0                                  ;Default to assume that there is only one root (e.g. a line)
    ENDIF
    
    ;Now search for the roots that make sense as a maximum
    IF TOTAL(ABS(IMAGINARY(roots))) GT 0 THEN STOP
    roots       = REAL_PART(roots)
    IF bestDegree GT 2 THEN BEGIN                           ;If polynomial is greater than 2nd order, then check for local minima
      deriv2PolyCoeffs = (1E + FINDGEN(bestDegree-1))*deriv1PolyCoeffs[1:*]
      numRoots         = N_ELEMENTS(roots)
      maximumRoot      = BYTARR(numRoots)
      FOR iRoot = 0, numRoots - 1 DO BEGIN
        deriv2Value = deriv2PolyCoeffs[0]                   ;Reset the second derivative value to the constant term
        FOR iDeg = 1, bestDegree - 2 DO BEGIN
          deriv2Value += deriv2PolyCoeffs[iDeg]*roots[iRoot];Accumulate the polynomial terms
        ENDFOR
        maximumRoot[iRoot] = deriv2Value LT 0               ;Check second derivative to find if this root is a maximum
      ENDFOR
    ENDIF
    
    maximumRootInds = WHERE(maximumRoot, numMax)
    IF numMax GT 0 THEN BEGIN
      bestAprCandidates = roots[maximumRootInds]
      APER, image, xStars[i], yStars[i], $
        flux, errap, sky, skyerr, phpadu, bestAprCandidates, skyradii, badpix, /SILENT, /FLUX
      
      ;And use the result to compute the signal-to-noise ratio at each aperture
      SNRs = flux/errap                                     ;Compute the signal-to-noise
      bestAprInd = WHERE(SNRs EQ MAX(SNRs), numMax)         ;Locate and save the be`st aperture
      IF numMax GT 0 THEN bestApr = bestAprCandidates[bestAprInd] $
        ELSE STOP
    ENDIF ELSE STOP

;    SNRapr     = bestApr + deltaRad*(FINDGEN(12)/11.0 - 0.5)
;    PLOT, SNRapr, dydx, PSYM = 4
;    OPLOT, SNRapr, (lineCoeffs[0] + SNRapr*lineCoeffs[1])
;    OPLOT, [bestApr], [lineCoeffs[0] + bestApr*lineCoeffs[1]], PSYM=7, COLOR=255l
;    stop

    ;Save the final result to return to the user
    optimumAprs[i] = bestApr
  ENDFOR
  
  ; Find any stars that got skipped and mark them as bad
  badStars = WHERE(optimumAprs LT 0.01, numBad)
  IF numBad GT 0 THEN optimumAprs[badStars] = !VALUES.F_NAN
  
  RETURN, optimumAprs

END