PRO S2_SELECT_PHOTOMETRY_MAGNITUDE_RANGE, event
  ;
  ; Event handler for the checkbox to activate/de-activate slider
  ;
  
  WIDGET_CONTROL, event.ID, GET_UVALUE=displayStruc
  WIDGET_CONTROL, event.top, GET_UVALUE=groupStruc                    ;Retrieve the group summary structure
  wS2magRangeSlider    = WIDGET_INFO(event.top, FIND_BY_UNAME='S2_MAG_RANGE')
  wS2magRangeSensitive = WIDGET_INFO(wS2magRangeSlider, /SENSITIVE)   ;Determine if the slider widget is sensitive or not
  displayWID           = WIDGET_INFO(event.top,FIND_BY_UNAME='IMAGE_DISPLAY_WINDOW');Grab the display window WID
  
  ;Parse the display structure
  IF N_ELEMENTS(displayStruc) GT 0 THEN BEGIN
    badStars    = displayStruc.badStars
    displayHead = displayStruc.displayHead
  ENDIF
  
  ;If the dual slider is desensitized, then sensitize it and change button value
  IF wS2magRangeSensitive EQ 0 THEN BEGIN
    ;Sensitize the button
    WIDGET_CONTROL, wS2magRangeSlider, SENSITIVE = 1
    WIDGET_CONTROL, event.ID, SET_VALUE='Slider Active'
    
    ;Get the display widget and make it active
    WIDGET_CONTROL, displayWID, GET_VALUE = displayWindowIndex          ;Grab the display window index
    WSET, displayWindowIndex                                            ;Set that display window as active
    
    intensityFile = FILE_SEARCH($                                       ;Read in the intensity image
    groupStruc.analysis_dir + 'S11_Full_Field_Polarimetry', $
    + '*_I.fits', COUNT = nFiles)
    
  IF nFiles GT 1 THEN BEGIN
    PRINT, 'TOO MANY FILES FOUND.'
    STOP
  ENDIF ELSE IF nFiles EQ 1 THEN BEGIN
    ; Read in and parse the image
    displayImg = READFITS(intensityFile, displayHead, /SILENT)
    sz           = SIZE(displayImg, /DIMENSIONS) ;Grab the dimensions of the image array
    nx           = sz[0]
    ny           = sz[1]
  ENDIF ELSE BEGIN
    PRINT, 'NO FILES FOUND.'
    STOP
  ENDELSE
    
    ;Redisplay the 2MASS image
    SKY, displayImg, skyMode, skyNoise, /SILENT
    TVIM, displayImg, RANGE = skyMode + [-1, +100]*skyNoise
    
    ;Extract astrometry from the display image header
    EXTAST, displayHead, dispAstr
    EXTAST, groupStruc.displayHeader, astr2MASS
    
    ;Oplot the image boundaries and stars
    AD2XY, groupStruc.starInfo.RAJ2000, groupStruc.starInfo.DEJ2000, $    ;Convert star positions to pixel coordinates
      dispAstr, xStars, yStars
    AD2XY, groupStruc.starInfo.RAJ2000, groupStruc.starInfo.DEJ2000, $
      astr2MASS, x2MASS, y2MASS
      
    ;Restrict stars to those well within the image
    starsInRange = xStars GT 50 AND $
      xStars LT nx - 51 AND $
      yStars GT 50 AND $
      yStars LT ny - 51
      
    ;Display star candidates
    greenColorInd = RGB_TO_DECOMPOSED([0,255,0])
    OPLOT, (xStars-1), (yStars-1), $                                      ;Show the user where the selected stars are
      PSYM = 6, COLOR = greenColorInd
      
    IF N_ELEMENTS(badStars) EQ 0 THEN BEGIN                               ;If there is no stored mask, build it and store it
      maskFile  = groupStruc.analysis_dir + 'S2_Ski_Jump_Fixes' + PATH_SEP() + 'Masking_files' + PATH_SEP() + 'mask2MASS.fits'
      mask2MASS = READFITS(maskFile)                                      ;Load in the 2MASS mask
      
      ;Test for stars in (or near) the galaxy mask
      numStars      = N_ELEMENTS(xStars)                                  ;Count the number of stars
      starsNearMask = BYTARR(numStars)                                    ;Initalize an array for the tracking stars near mask
      sz2MASS       = SIZE(groupStruc.displayImage, /DIMENSIONS)          ;Get the size of the displayImage
      MAKE_2D, FINDGEN(sz2MASS[0]), FINDGEN(sz2MASS[1]), xx2MASS, yy2MASS ;Make a 2D array for all image pixel locations
      FOR i = 0, numStars - 1 DO BEGIN                                    ;Loop through each of the preliminary star selections
        distFromStar     = SQRT((xx2MASS - x2MASS[i])^2E + $              ;Compute the distance from each star
          (yy2MASS - y2MASS[i])^2E)
        starsNearMask[i] = TOTAL((distFromStar LT 5) AND mask2MASS) GE 1 ;Check if the star is within 10 pixels of the mask
      ENDFOR
      
      ;Mark all stars that should NOT be used for photometry
      badStars = (starsNearMask OR ~starsInRange)
      WIDGET_CONTROL, event.ID, SET_UVALUE = {badStars:badStars, displayHead:displayHead}
    ENDIF
    
  ENDIF
  
  ;IF the dual slider is sensitized, then desensiize it and change the button value
  IF wS2magRangeSensitive EQ 1 THEN BEGIN
    WIDGET_CONTROL, wS2magRangeSlider, SENSITIVE = 0
    WIDGET_CONTROL, event.ID, SET_VALUE='Slider Inactive'
    WIDGET_CONTROL, event.ID, GET_UVALUE=displayStruc
    
    ;Parse the display structure
    IF N_ELEMENTS(displayStruc) GT 0 THEN BEGIN
      badStars    = displayStruc.badStars
      displayHead = displayStruc.displayHead
    ENDIF
    
    stateStorageWID = WIDGET_INFO(wS2magRangeSlider, /CHILD)
    WIDGET_CONTROL, stateStorageWID, GET_UVALUE=state, /NO_COPY       ;Grab the dual-slider "state" variable
    
    
    ;; Value label(s)
    ;--JDM 20150118-- figure out how many significant figures are needed for the display string
    minSigFigs      = 3                                               ;Force to display at least 3 sig-figs
    negativeSigns   = state.value NE ABS(state.value)                 ;Determine which values need a negative sign
    valueMagnitudes = ALOG10(ABS(state.value))                        ;This computes the magnitude of the values
    positiveMag     = valueMagnitudes EQ ABS(valueMagnitudes)         ;Determine if this number is greater than 1
    negativeMag     = ~positiveMag
    smallPosMag     = (CEIL(valueMagnitudes) LT minSigFigs)*positiveMag   ;Determine if the sig-figs span the decimal point
    mediumPosMag    = (CEIL(valueMagnitudes) EQ minSigFigs)*positiveMag   ;Determine if there is trailing decimal point
    largePosMag     = valueMagnitudes GE minSigFigs                       ;Determine if there are trailing numbers
    
    stringLength    = minSigFigs $                                    ;Begin with at least the minimum string length
      + 1*negativeSigns $                                             ;Account for a negative sign
      + (FLOOR(ABS(valueMagnitudes)) + 2)*negativeMag $               ;Account for leading zeros and decimal point
      + 1*smallPosMag $                                               ;Account spanning the decimal point
      + 2*mediumPosMag $                                              ;Account for a trailing decimal point
      + (CEIL(valueMagnitudes) - minSigFigs)*largePosMag              ;Account for trailing numbers
    IF TOTAL(ABS(stringLength) GT 10) GT 1 then stop
    
    decimalLength   = (stringLength - negativeSigns - 2)*negativeMag $;Negative magnitudes only leading zero, decimal, and sign
      + (minSigFigs - CEIL(valueMagnitudes))*smallPosMag $            ;Small positive magnitudes have only a few leading digits
      + 1*mediumPosMag $                                              ;Medium positive magnitudes have one trailing decimals
      + 0*largePosMag                                                 ;Large positive magnitudes have no trailing decimals
      
    valueIsSpecial = WHERE(ABS(state.value) EQ 1 $
      OR state.value EQ 0, countIsSpecial)                            ;Treat the special cases of 0, and +/-1
    IF countIsSpecial GT 0 THEN BEGIN
      stringLength[valueIsSpecial]  = 3 + negativeSigns[valueIsSpecial]
      decimalLength[valueIsSpecial] = 1
    ENDIF
    stringLength    = STRTRIM(stringLength, 2)                        ;Convert string length value to a string
    decimalLength   = STRTRIM(decimalLength, 2)                       ;Convert decimal length value to a string
    
    ;***** need to account for different types of (integer) values, but this should work for now *****
    formatStrings   = '(F' + stringLength + '.' + decimalLength + ')'
    
    formatString = '("Photometry will be performed using stars between ",' $
      + formatStrings[0] + '," and ",' $
      + formatStrings[1] + '," magnitudes.")'
    PRINT_TEXT2, event, $
      STRING(state.value, FORMAT=formatString)
      
    ;Flag the stars to use
    IF groupStruc.NIRband EQ 'H' THEN magIndex = 5 $                    ;Setup the structure tag for this band
    ELSE IF groupStruc.NIRband EQ 'Ks' THEN magIndex = 7
    
    ; Restrict stars to given photometric range
    groupStruc.photStarFlags = groupStruc.starInfo.(magIndex) GE state.value[0] $
      AND groupStruc.starInfo.(magIndex) LE state.value[1]
      
    ; Exclude stars at bad positions
    groupStruc.photStarFlags = (groupStruc.photStarFlags AND ~badStars)
    
    ; Save the photometry flags to disk
    UPDATE_GROUP_SUMMARY, event, groupStruc, /SAVE                    ;Store the updated data to disk
    WIDGET_CONTROL, stateStorageWID, SET_UVALUE=state, /NO_COPY       ;Restore the dual-slider "state" variable
    
  ENDIF
  
END

PRO S2_MAGNITUDE_RANGE, event;, stash
  ;
  ; Event handler for magnitude range slider
  ;
  
  displayWID = WIDGET_INFO(event.top,FIND_BY_UNAME='IMAGE_DISPLAY_WINDOW');Grab the display window WID
  tlb_wid    = WIDGET_INFO(event.top,FIND_BY_UNAME='WID_BASE')        ;Grab the top-level-base WID
  wS2magRangeSlider = WIDGET_INFO(event.top, FIND_BY_UNAME='S2_MAG_RANGE')
  wS2magSliderCheck = WIDGET_INFO(event.top, FIND_BY_UNAME='S2_SELECT_PHOTOMETRY_MAGNITUDE_RANGE')
  stateStorageWID   = WIDGET_INFO(wS2magRangeSlider, /CHILD)
  WIDGET_CONTROL, tlb_wid, GET_UVALUE=groupStruc                      ;Grab the group summary structure
  WIDGET_CONTROL, displayWID, GET_VALUE = displayWindowIndex          ;Grab the display window index
  WIDGET_CONTROL, stateStorageWID, GET_UVALUE=state, /NO_COPY         ;Grab the widget "state" variable
  WIDGET_CONTROL, wS2magSliderCheck, GET_UVALUE=displayStruc
  
  ;Parse the display structure
  IF N_ELEMENTS(displayStruc) GT 0 THEN BEGIN
    badStars    = displayStruc.badStars
    displayHead = displayStruc.displayHead
  ENDIF
  
  ;APPLY CHANGES TO SELECTED STARS
  IF groupStruc.NIRband EQ 'H' THEN magIndex = 5 $
  ELSE IF groupStruc.NIRband EQ 'Ks' THEN magIndex = 7
  
  ; Restrict stars to given photometric range
  groupStruc.photStarFlags = groupStruc.starInfo.(magIndex) GE state.value[0] $
    AND groupStruc.starInfo.(magIndex) LE state.value[1]
    
  ; Exclude stars at bad positions
  groupStruc.photStarFlags = (groupStruc.photStarFlags AND ~badStars)
  
  WSET, displayWindowIndex                                            ;Set that display window as active
  
  useStarInds = WHERE(groupStruc.photStarFlags, numUse, $             ;Find the stars in the selected magnitude range
    COMPLEMENT = noUseStarInds, NCOMPLEMENT = numNoUse)
    
  EXTAST, displayHead, astr
  AD2XY, groupStruc.starInfo.RAJ2000, groupStruc.starInfo.DEJ2000, $
    astr, xStars, yStars
    
  IF numUse GT 0 THEN BEGIN
    OPLOT, (xStars[useStarInds]-1), (yStars[useStarInds]-1), $        ;Show the user where the selected stars are
      PSYM = 6, COLOR = RGB_TO_DECOMPOSED([0,255,0])
  ENDIF
  IF numNoUse GT 0 THEN BEGIN
    OPLOT, (xStars[noUseStarInds]-1), (yStars[noUseStarInds]-1), $    ;Show the user where the unselected stars are
      PSYM = 6, COLOR = RGB_TO_DECOMPOSED([255,0,0])
  ENDIF
  
  ;Restore "state" variable
  WIDGET_CONTROL, stateStorageWID, SET_UVALUE=state, /NO_COPY         ;Grab the widget "state" variable
  
END

PRO S2_MEASURE_PHOTOMETRY, event

  PRINT_TEXT2, event, "Photometry started..."
  ;Start by getting all the data we need to do the matching
  tlb_wid    = WIDGET_INFO(event.top, FIND_BY_UNAME='WID_BASE')       ;Retrieve the top-level base ID
  displayWID = WIDGET_INFO(event.top, FIND_BY_UNAME='IMAGE_DISPLAY_WINDOW')
  WIDGET_CONTROL, tlb_wid, GET_UVALUE=groupStruc                      ;Retrieve the groupStruc
  WIDGET_CONTROL, displayWID, GET_VALUE=displayWindowIndex
  
  NIRbands     = ['J','H','Ks']                                       ;Possible NIR bands
  bandNumber   = WHERE(NIRbands EQ groupStruc.NIRband, count)         ;Look for a matching NIR band
  IF count EQ 0 THEN STOP                                             ;Check that a match was actually found
  testBand   = TAG_EXIST(groupStruc.starInfo, $                       ;Find the tag containing magnitudes for this band
    STRMID(groupStruc.NIRband,0,1) + 'MAG', INDEX = magTag)
  testSig    = TAG_EXIST(groupStruc.starInfo, $                       ;Find the tag containing magnitude uncertainties for this band
    'E_' + STRMID(groupStruc.NIRband,0,1) + 'MAG', INDEX = sigMagTag)
    
  intensityFile = FILE_SEARCH($                                       ;Read in the intensity image
    groupStruc.analysis_dir + 'S11_Full_Field_Polarimetry', $
    + '*_I.fits', COUNT = nFiles)
    
  IF nFiles EQ 0 THEN BEGIN
    PRINT, 'NO FILES FOUND.'
    STOP
  ENDIF ELSE IF nFiles EQ 1 THEN BEGIN
    ; Read in and parse the image
    intensityImg = READFITS(intensityFile, header, /SILENT)
    sz           = SIZE(intensityImg, /DIMENSIONS) ;Grab the dimensions of the image array
    nx           = sz[0]
    ny           = sz[1]
  ENDIF ELSE IF nFile GT 1 THEN BEGIN
    PRINT, 'TOO MANY FILES FOUND.'
    STOP
  ENDIF
  
  ;  apr      = 2.5*PSF_FWHM
  ;  skyradii = [1.5, 2.5]*apr
  phpadu   = 8.21                                                     ;This value can be found on Mimir website
  ronois   = 17.8                                                     ;(elec) This value is from Mimir website
  ;  ronois   = 3.1                                                   ;(ADU) This value is from GPIPS code "S4_PSF_fit"
  badpix    = [-300L, 6000L]
  badpixCOG = [-300L, 1000000L]
  
  EXTAST, header, astr, noparams                                      ;Extract image astrometry
  GETROT, astr, rotAngle, cdelt
  plateScale   = SQRT(ABS(cdelt[0]*cdelt[1]))*3600E
  
  ;Begin by generating a curve-of-growth (COG) from the largest number of stars
  ;within 1 magnitune of each other and within the brightest two magnitudes
  starMags      = groupStruc.starInfo.(magTag)
  AD2XY, groupStruc.starInfo.RAJ2000, groupStruc.starinfo.DEJ2000, $  ;Convert 2MASS positions to (x,y) pixel coordinates
    astr, xStars, yStars
    
  ;****************************************************
  ;********* APERTURE PHOTOMETRY OF ALL STARS *********
  ;****************************************************
  crowdedStars = TEST_CROWDED(xStars, yStars, 1.5*20)
  photStarInds = WHERE(groupStruc.photStarFlags and ~crowdedStars, numPhotStars)
  IF numPhotStars GT 0 THEN BEGIN
    photStarInfo = groupStruc.starInfo[photStarInds]
    AD2XY, photStarInfo.RAJ2000, photStarInfo.DEJ2000, $                ;Convert 2MASS positions to (x,y) pixel coordinates
      astr, xPhot, yPhot
  ENDIF ELSE MESSAGE, 'No photometry stars found'
  
  ;**** REFINE STAR POSITIONS ****
  useStar   = BYTARR(numPhotStars)                                    ;Reset the "useStar" to track which stars were well fit
  FWHMs     = FLTARR(numPhotStars)                                    ;Initalize an array for storing star FWHMs
  failedFit = 0                                                       ;Set a counter for the number of failed Gaussian star fits
  FOR j = 0, numPhotStars - 1 DO BEGIN
    ;Cut out a subarray for a more precise positioning
    xOff     = (xPhot[j] - 19) > 0
    xRt      = (xOff  + 40) < (sz[0] - 1)
    yOff     = (yPhot[j] - 19) > 0
    yTop     = (yOff + 40)  < (sz[1] - 1)
    subArray = intensityImg[xOff:xRt, yOff:yTop]
    
    result   = GAUSS2DFIT(subArray, A, /TILT)                         ;Gaussian fit the star
    inArray  = (A[4] GT 5) AND (A[4] LT 34) $                         ;If the fit is located in the center of the array
      AND (A[5] GT 5) AND (A[5] LT 34)
    okShape  = (A[2] GT 0.8) AND (A[2] LT 5) $                        ;and if its gaussian width is reasonable (not a hot pixel)
      AND (A[3] GT 0.8) AND (A[3] LT 5)
      
    methodDifference = 0                                              ;Reset the method difference variable
    IF inArray AND okShape THEN BEGIN
      FWHMs[j] = SQRT(ABS(A[2]*A[3]))*2.355                           ;Compute the FWHM for this star
      GCNTRD, subArray, A[4], A[5], xcen, ycen,  FWHMs[j]             ;Centroid this star (using estimated FWHM)
      methodDifference = SQRT((xCen - A[4])^2 + (yCen - A[5])^2)      ;Compute difference between the two locating methods
      IF (methodDifference LE 1) $                                    ;If the two methods have a consensus,
        AND FINITE(methodDifference) THEN BEGIN                       ;then update the star positions
        xStars[j]  = xOff + xcen
        yStars[j]  = yOff + ycen
        useStar[j] = 1                                                ;Mark this star as one of the stars to use
        failedFit  = 0                                                ;If the fit was successful, then reset the failed fit counter
        ; TVIM, subarray
        ; OPLOT, [xcen], [ycen], PSYM=6, color=255L
        ; stop
      ENDIF
    ENDIF
    IF ~inArray OR ~okShape $                                         ;If any one of the tests failed,
      OR (methodDifference GT 1) OR ~FINITE(methodDifference) $       ;then increment the failedFit counter
      THEN failedFit++
    IF failedFit GE 2 THEN BREAK                                      ;If the "failed fit"
  ENDFOR
  
  ;Cull photometry stars to those with accurately refined positions
  useInds = WHERE(useStar, numUse)                                    ;Determine which stars were well fit
  IF numUse GT 0 THEN BEGIN
    photStarInfo = photStarInfo[useInds]                              ;Cull the 2MASS data
    xPhot        = xPhot[useInds]                                     ;Cull the list to only the well fit stars
    yPhot        = yPhot[useInds]
  ENDIF ELSE STOP
  
  ;**** FIND THE OPTIMUM APERTURE FOR EACH STAR ****
  ;**** THIS IS A MYSTERY, BUT I AM GOING TO USE AN UPPER LIMIT OF 30,000 ADU as a bad-pix value ***
  badpix    = [-300L, 300000L]
  
  COG_FWHM = GET_FWHM(intensityImg, xPhot, yPhot, 3.0, badpix[1])    ;Estimate the curve-of-growth FWHM
  
  ;Figure out the optimum apertures for estimating the COG
  largestApr  = 6*COG_FWHM[0]                                         ;Set the largest aperture used for COG measurements
  skyradii    = [1.2, SQRT(4 + 1.2^2)]*largestApr                     ;Forces Npix(sky) = 4*Npix(star) (largest aperture)
  rCritical   = MAX(skyradii) + 3*COG_FWHM[0]                         ;Compute grouping radius
  starradii   = [1.5, 15.0]
  
  PRINT_TEXT2, event, 'Computing aperatures at which photometric S/N is greatest for photometry each star'
  optimumAprs = GET_OPTIMUM_APERTURES(intensityImg, xPhot, yPhot, $
    starradii, skyradii, badpix)
    
  ;Cull photometry stars to those with accurately determined "optimum apertures"
  useInds = WHERE(FINITE(optimumAprs) $             ;Determine which stars were well fit
    AND (optimumAprs GT 1) $
    AND (optimumAprs LT 6), numUse)
  IF numUse GT 1 THEN BEGIN
    photStarInfo = photStarInfo[useInds]            ;Cull the 2MASS data
    xPhot        = xPhot[useInds]                   ;Cull the list to only the well fit stars
    yPhot        = yPhot[useInds]
    optimumAprs  = optimumAprs[useInds]
  ENDIF ELSE BEGIN
    PRINT, 'No stars seem to have reasonable PSFs'
    STOP
  ENDELSE
  
  ;Compute the range of apertures to include in COG analysis
  minApr  = MIN(optimumAprs)
  maxApr  = 2.5*MAX(optimumAprs)
  aprIncr = (maxApr/minApr)^(1.0/11.0)
  COGapr  = minApr*aprIncr^FINDGEN(12)              ;Generate a list of apertures for COG
  
  ;**** GENERATE A CURVE OF GROWTH ****
  PRINT_TEXT2, event, 'Generating a King model curve-of-growth'
  kingParams = GENERATE_COG(intensityImg, xPhot, yPhot, $
    COGapr, skyradii, badpixCOG)
    
  PRINT_TEXT2, event, 'S(r;Ri,A,B,C,D) = B*M(r;A) + (1-B)*[C*G(r;Ri) + (1-C)*H(r;D*Ri)]'
  PRINT_TEXT2, event, ' '
  PRINT_TEXT2, event, 'Where'
  PRINT_TEXT2, event, 'M(r;A)    = [(A-1)/pi]*(1 + r^2)^(-A)          --- Moffat function'
  PRINT_TEXT2, event, 'G(r;Ri)   = [1/(2*pi*Ri^2)]*Exp[-r^2/(2*Ri^2)] --- Gaussian function'
  PRINT_TEXT2, event, 'H(r;D*Ri) = [1/(2*pi*(D*Ri)^2)]*Exp[-r/(D*Ri)] --- exponential function'
  
  
  ;Use the "set_sig_figs" functon to display these number strings
  parameterNames   = ['Ri','A ','B ','C ','D ']
  parameterStrings = SIG_FIG_STRING(kingParams, [3,6,3,3,3])
  
  FOR i = 0, N_ELEMENTS(KingParams) - 1 DO BEGIN
    parameterString = parameterNames[i] + ' = ' + parameterStrings[i]
    PRINT_TEXT2, event, parameterString
  ENDFOR
  
  ;**** COMPUTE APERTURE CORRECTIONS USING THE CURVE OF GROWTH ****
  PRINT_TEXT2, event, 'Computing aperture corrections for each photometry star'
  aprCorrections = GET_APERTURE_CORRECTION(kingParams, optimumAprs)
  
  ;Use APER to estimate magnitudes and fluxes of all the Mimir stars in the image
  ;Carefully loop through stars so that you can use a separate, optimal aperture for each.
  nPhotStars  = N_ELEMENTS(xPhot)
  instMags    = FLTARR(nPhotStars)
  sigInstMags = FLTARR(nPhotStars)
  FOR i = 0, nPhotStars - 1 DO BEGIN
    APER, intensityImg, xPhot[i], yPhot[i], $
      mag, errap, sky, skyerr, phpadu, optimumAprs[i], skyradii, badpix, /EXACT, /SILENT
    ;Remove auto-applied zero-point from APER (we will solve zero point later)
    instMags[i]    = mag + aprCorrections[i] - 25.0
    sigInstMags[i] = errap
  ENDFOR
  
  ;*******************************************************************
  ;************ SAVE BASIC PHOTOMETRY TO DISK! ***********************
  ;*******************************************************************
  photPath = groupStruc.analysis_dir + 'S11_Full_Field_Polarimetry' $
    + PATH_SEP() + groupStruc.NIRband + '_photometry.dat'
  photHeader = '2MASS_ID               RAJ2000         DEJ2000        Mimir_mag  sig_Mimir_mag'
  WRITECOL, photPath, $
    photStarInfo._2MASS, $
    photStarInfo.RAJ2000, photStarInfo.DEJ2000, $
    instMags, $
    sigInstMags, $
    HEADER = photHeader
    
  ;*******************************************************************
  ;******** NOW THAT EVERYTHING HAS BEEN COMPUTED, *******************
  ;******** LET'S SHOW THE USER WHAT WE FOUND.     *******************
  ;*******************************************************************
  
  WSET, displayWindowIndex
  xOriginal = !X.MARGIN
  !X.MARGIN = xOriginal*0.75
  !P.MULTI  = [0,2,2]
  
  ;**** DELTA-RA AND DELTA-DEC HISTOGRAMS ****
  XY2AD, (xPhot), (yPhot), astr, photRAs, photDecs
  deltaRA    = (photStarInfo.RAJ2000 - photRAs)*3600D*1E6             ;Compute positional difference in arcsec
  deltaDec   = (photStarInfo.DEJ2000 - photDecs)*3600D*1E6
  binSize    = 0.5
  numBinsRA  = 0
  numBinsDec = 0
  
  numStars = N_ELEMENTS(xStars)
  minBins  = numStars < 4                                             ;Require at least three bins
  WHILE (numBinsRA LT minBins) OR (numBinsDec LT minBins) DO BEGIN    ;Iterate bin size until enough bins found
    PLOTHIST, deltaRA, histRA, numRA, BIN=binSize, /NOPLOT
    PLOTHIST, deltaDec, histDec, numDec, BIN=binSize, /NOPLOT
    numBinsRA  = TOTAL(numRA GT 0)
    numBinsDec = TOTAL(numDec GT 0)
    binSize   /= 1.2                                                  ;Decrease bin size
  ENDWHILE
  binSize *=1.2                                                       ;Undo the last decrement
  
  histRA  = [MIN(histRA) - binSize, histRA, MAX(histRA) + binSize]    ;Add a null element on either end of the histograms
  numRA   = [0,numRA,0]
  histDec = [MIN(histDec) - binSize, histDec, MAX(histDec) + binSize] ;Add a null element on either end of the histograms
  numDec  = [0,numDec,0]
  
  PLOT, histRA, numRA, PSYM=10, $                                     ;Plot the RA and Dec histograms
    THICK = 2, XTITLE = 'D RA (arcesc x 1E-6)', YTITLE = 'Number'
  PLOT, histDec, numDec, PSYM=10, $
    THICK = 2, XTITLE = 'D Dec (arcsec x 1E-6)', YTITLE = 'Number'
    
  ;**** DELTA-MAG HISTOGRAM ****
  binSize    = 0.5
  numBinsMag = 0
  WHILE (numBinsMag LT 4) DO BEGIN
    PLOTHIST, instMags - photStarInfo.(magTag), histMag, numMag, BIN=binSize, /NOPLOT
    numBinsMag = TOTAL(numMag GT 0)
    binSize  /= 1.2
  ENDWHILE
  binSize *=1.2                                                       ;Undo the last decrement
  
  histMag = [MIN(histMag) - binSize, histMag, MAX(histMag) + binSize] ;Add a null element on either end of the histograms
  numMag  = [0,numMag,0]
  
  PLOT, histMag, numMag, PSYM=10, $                                 ;Plot the magnitude histogram
    THICK = 2, XTITLE = 'D mag', YTITLE = 'Number'
    
  ;**** CURVE OF GROWTH ****
  ;Disambiguate the King mmodel parameters
  Ri = kingParams[0]                                                  ;Gaussian standard-deviation width
  A  = kingParams[1]                                                  ;Moffat denominator exponent
  B  = kingParams[2]                                                  ;Weighting for the Moffat component
  C  = kingParams[3]                                                  ;Weighting for the Gaussian (and exponential component)
  D  = kingParams[4]                                                  ;Scale radius for the exponential component
  
  nModelApr  = 50                                                     ;Set the number of model apertures to use
  Napr       = N_ELEMENTS(COGapr)                                     ;Count the number of actual apertures used
  modelApr   = MIN(COGapr)  $                                         ;Compute the model apertures to use
    + 1.2*(MAX(COGapr) - MIN(COGapr))*FINDGEN(nModelApr)/(nModelApr - 1)
    
  COGphotometry = COG_APER(intensityImg, xPhot, yPhot, $
    COGapr, skyradii, badpixCOG)
    
  ;Compute the expected delta-magnitude at that incremental aperture
  ;using the fitted King model parameters
  delMags1   = 0.0*modelApr
  FOR i = 0, nModelApr - 2 DO BEGIN
    numerLimits = [0,modelApr[i+1]]
    denomLimits = [0,modelApr[i]]
    numerator   = INTEGRATED_KING_MODEL(kingParams, numerLimits)
    denominator = INTEGRATED_KING_MODEL(kingParams, denomLimits)
    kingCOG     = -2.5*ALOG10(numerator/denominator)
    delMags1[i] = kingCOG
  ENDFOR
  
  ;******* I'VE SET THE UNITS TO BE Dmag/Dpixel **********
  ;Compute the magnitude differences used to get the COG
  xCOGdata = 0.5*(COGapr[1:Napr-1] + COGapr[0:Napr-2])
  yCOGdata = COGphotometry.deltaMags/(COGapr[1:Napr-1] - COGapr[0:Napr-2])
  yErr     = COGphotometry.sigDeltaMags/(COGapr[1:Napr-1] - COGapr[0:Napr-2])
  
  ;Compute the COG model values
  xCOGcurve = 0.5*(modelApr[1:nModelApr-1] + modelApr[0:nModelApr-2])
  yCOGcurve = delMags1/(modelApr[1:nModelApr-1] - modelApr[0:nModelApr-2])
  
  ;Establish a plotting region
  PLOT, [min(xCOGcurve), max(xCOGcurve)], $
    [min([yCOGdata - yErr, yCOGcurve]), max([[yCOGdata + yErr, yCOGcurve]])], $
    /NODATA, $
    XTITLE = "Aperture (pixels)", $
    YTITLE = "COG (mag/pixel)"
    
  OPLOT, xCOGdata, yCOGdata, PSYM = 4, THICK = 2
  ERRPLOT, xCOGdata, yCOGdata + yErr, yCOGdata - yErr, THICK = 2
  
  ;Plot the fittted King Model COG curve
  OPLOT, xCOGcurve, yCOGcurve, $
    THICK = 2, COLOR=RGB_TO_DECOMPOSED([0,255,0])
    
  ;Reset plotting values
  !P.MULTI  = 0
  !X.MARGIN = xOriginal
  
  PRINT_TEXT2, event, 'Completed measuring photometry of 2MASS PSC entries'
  
END

PRO S2_USE_2BAND_PHOTOMETRY, event
  ;Retrieve WID for path text box
  wS2pathTextBox  = WIDGET_INFO(event.top, FIND_BY_UNAME='S2_2ND_BAND_PATH')
  wS2browseButton = WIDGET_INFO(event.top, FIND_BY_UNAME='S2_BROWSE_FOR_2ND_BAND')
  
  IF event.select THEN BEGIN
    ;Activate the text box and browse button
    WIDGET_CONTROL, wS2pathTextBox, SENSITIVE=1
    WIDGET_CONTROL, wS2browseButton, SENSITIVE=1
  ENDIF ELSE BEGIN
    ;Clear the text box
    WIDGET_CONTROL, wS2pathTextBox, SET_VALUE='PPOL DIRECTORY GOES HERE'
    
    ;Deactivate the text box and browse button
    WIDGET_CONTROL, wS2pathTextBox, SENSITIVE=0
    WIDGET_CONTROL, wS2browseButton, SENSITIVE=0
  ENDELSE
END

PRO S2_BROWSE_FOR_2ND_BAND, event
  ;Retrieve WID for path text box
  wS2pathTextBox = WIDGET_INFO(event.top, FIND_BY_UNAME='S2_2ND_BAND_PATH')
  
  ;Have the user specify where the data is being stored
  dir = DIALOG_PICKFILE(TITLE='Select PPOL directory for 2ND band', /DIRECTORY)
  
  ;Update the text box
  WIDGET_CONTROL, wS2pathTextBox, SET_VALUE=dir
  
END

PRO S2_CALIBRATE_PHOTOMETRY, event
  ;Retrieve WID for path text box
  wS2pathTextBox  = WIDGET_INFO(event.top, FIND_BY_UNAME='S2_2ND_BAND_PATH')
  wS2browseButton = WIDGET_INFO(event.top, FIND_BY_UNAME='S2_BROWSE_FOR_2ND_BAND')
  
  displayWID = WIDGET_INFO(event.top,FIND_BY_UNAME='IMAGE_DISPLAY_WINDOW');Grab the display window WID
  tlb_wid    = WIDGET_INFO(event.top,FIND_BY_UNAME='WID_BASE')        ;Grab the top-level-base WID
  WIDGET_CONTROL, tlb_wid, GET_UVALUE=groupStruc                      ;Grab the group summary structure
  WIDGET_CONTROL, displayWID, GET_VALUE = displayWindowIndex          ;Grab the display window index
  
  ;Check if the path for a 2nd waveband is sensitive
  wS2pathSensitive = WIDGET_INFO(wS2pathTextBox, /SENSITIVE)
  
  ;Construct path to mask header
  maskPath = groupStruc.analysis_dir + $
    'S2_Ski_Jump_Fixes' + PATH_SEP() + $
    'Masking_files' + PATH_SEP() + 'maskInfo.dat'
    
  ;Read header if present
  IF FILE_TEST(maskPath) THEN BEGIN
    maskHeader = READHEAD(maskPath)
    galRA      = SXPAR(maskHeader, 'RA_MASK')
    galDec     = SXPAR(maskHeader, 'DEC_MASK')
  ENDIF ELSE BEGIN
    PRINT_TEXT2, event, 'Could not find model parameters. Run galaxy model.'
    RETURN
  ENDELSE
  
  IF wS2pathSensitive EQ 1 THEN BEGIN
    ;Get the proposed path to the second band
    WIDGET_CONTROL, wS2pathTextBox, GET_VALUE = band2analysis_dir
    
    ;Test that this is a DIFFERENT path
    IF band2analysis_dir EQ groupStruc.analysis_dir THEN BEGIN
      PRINT_TEXT2, event, 'Path for 2nd band is same as current analysis directory.'
      RETURN
    ENDIF
    
    ;Test that this is a genuine PPOL path
    testPath = band2analysis_dir + 'S1_Image_Groups_and_Meta_Groups' + PATH_SEP() + 'Group_Summary.sav'
    IF ~FILE_TEST(testPath) THEN BEGIN
      PRINT_TEXT2, event, 'This does not appear to be a PPOL directory.'
      RETURN
    ENDIF
    
    PRINT_TEXT2, event, $
      + NEW_LINE() + '***!!!WARNING!!!***' $
      + NEW_LINE() + 'This procedure assumes image alignment' $
      + NEW_LINE() + 'was performed in previous step.' $
      + NEW_LINE() $
      + NEW_LINE() + 'If this is not true, then re-run the previous step' $
      + NEW_LINE() + 'using the "2-band" option to specify the PPOL directory' $
      + NEW_LINE() + 'of the complementary waveband images.' $
      + NEW_LINE() + '***!!!WARNING!!!***'

    ;Build paths to photometry files
    pathBand1Phot = groupStruc.analysis_dir + $
      'S11_Full_Field_Polarimetry' + $
      PATH_SEP() + groupStruc.NIRband + '_photometry.dat'
    pathBand2Phot = band2analysis_dir + $
      'S11_Full_Field_Polarimetry' + $
      PATH_SEP()
    pathBand2Phot = FILE_SEARCH(pathBand2Phot, '*_photometry.dat', COUNT = band2Found)
    
    ;Check that photometry files actually exist
    IF FILE_TEST(pathBand1Phot) AND (band2Found EQ 1) THEN BEGIN
      READCOL, pathBand1Phot, ID1, RA1, Dec1, mag1, s_mag1, $
        FORMAT='A,F,F,F,F', COMMENT = ';'
      READCOL, pathBand2Phot, ID2, RA2, Dec2, mag2, s_mag2, $
        FORMAT='A,F,F,F,F', COMMENT = ';'
        
      PRINT_TEXT2, event, 'Read photometry from two bands.'
    ENDIF ELSE BEGIN
      PRINT_TEXT2, event, 'Could not find both photometry files.'
      RETURN
    ENDELSE
    
    ;Read in intensity images
    pathBand1Img = groupStruc.analysis_dir + $
      'S11_Full_Field_Polarimetry' + PATH_SEP()
    band1IntenFile = (FILE_SEARCH(pathBand1Img, '*_I.fits', COUNT = numBand1))[0]
    band1sIfile    = (FILE_SEARCH(pathBand1Img, '*_sI.fits', COUNT = numBand1))[0]
    
    pathBand2Img = band2analysis_dir + $
      'S11_Full_Field_Polarimetry' + PATH_SEP()
    band2IntenFile = (FILE_SEARCH(pathBand2Img, '*_I.fits', COUNT = numBand2))[0]
    band2sIfile    = (FILE_SEARCH(pathBand2Img, '*_sI.fits', COUNT = numBand1))[0]
    
    IF (numBand1 EQ 1) AND (numBand2 EQ 1) THEN BEGIN
      ;Read in the intensity images
      band1Img = READFITS(band1IntenFile, band1Header)
      band1Sig = READFITS(band1sIfile)
      band2Img = READFITS(band2IntenFile, band2Header)
      band2Sig = READFITS(band2sIfile)
    ENDIF ELSE BEGIN
      PRINT_TEXT2, event, 'Could not uniquely identify both intensity images'
      RETURN
    ENDELSE
    
    ;Grab the starInfo structure index of band1 and band2
    ;Use the groupStruc.NIRband element for band1
    band1    = (STRMID(groupStruc.NIRband, 0, 1))[0]
    testBand = TAG_EXIST(groupStruc.starInfo, $                         ;Find the tag containing magnitudes for this band
      STRMID(band1,0,1) + 'MAG', INDEX = magTag1)
    testSig  = TAG_EXIST(groupStruc.starInfo, $                         ;Find the tag containing magnitude uncertainties for this band
      'E_' + STRMID(band1,0,1) + 'MAG', INDEX = sigMagTag1)
      
    ;Use the first letter of the photometry file for band2
    band2    = (STRMID(FILE_BASENAME(pathBand2Phot), 0, 1))[0]
    testBand = TAG_EXIST(groupStruc.starInfo, $                         ;Find the tag containing magnitudes for this band
      band2 + 'MAG', INDEX = magTag2)
    testSig  = TAG_EXIST(groupStruc.starInfo, $                         ;Find the tag containing magnitude uncertainties for this band
      'E_' + band2 + 'MAG', INDEX = sigMagTag2)
      
    ;Now perform 2 band photometric reduction
    ;Start by matching stars between band1 and band2
    MATCH, ID1, ID2, ind1, ind2, COUNT=numMatch
    
    IF numMatch GT 0 THEN BEGIN
      PRINT_TEXT2, event, STRING(numMatch, FORMAT='("Found", I3, " matching stars.")')
      
      ;Cull and match the list of star from band1 and band2
      ;*** BAND 1 ***
      ID1    = ID1[ind1]
      RA1    = RA1[ind1]
      Dec1   = Dec1[ind1]
      mag1   = mag1[ind1]
      s_mag1 = s_mag1[ind1]
      
      ;*** BAND 2 ***
      ID2    = ID2[ind2]
      RA2    = RA2[ind2]
      Dec2   = Dec2[ind2]
      mag2   = mag2[ind2]
      s_mag2 = s_mag2[ind2]
      
    ENDIF ELSE BEGIN
      PRINT_TEXT2, event, 'Could not match any stars.'
      RETURN
    ENDELSE
    
    ;Now try to match on 2MASS ID from groupStruc.starInfo
    starIDs  = strtrim(groupStruc.starInfo._2MASS, 2)
    MATCH, ID1, starIDs, ind1, starInds, COUNT=numMatch
    
    ;Now grab the photometry information for the matched stars
    starInfo = groupStruc.starInfo[starInds]
    
    ;Figure out which band is a shorter wavelength
    NIRbands   = ['J', 'H', 'K']
    band1Ind   = WHERE((STRMID(band1, 0, 1))[0] EQ NIRbands)
    band2Ind   = WHERE((STRMID(band2, 0, 1))[0] EQ NIRbands)
    
    ;Assign SIGNS to magnitude differences accordingly
    IF band1Ind LT band2Ind THEN BEGIN
      sign1 = +1 & sign2 = -1
    ENDIF ELSE IF band1Ind GT band2Ind THEN BEGIN
      sign1 = -1 & sign2 = +1
    ENDIF ELSE BEGIN
      PRINT_TEXT2, event, 'Both bands appear to be the same wavelength'
    ENDELSE
    
    
    ;Compute the regression quantities
    yRegress     = starInfo.(magTag1) - mag1
    sig_yRegress = SQRT(s_mag1^2 + starInfo.(sigMagTag1)^2)
    
    xRegress     = sign1*mag1 + sign2*mag2
    sig_xRegress = SQRT(s_mag1^2 + s_mag2^2)
    
;    ;Perform the regression using FITEXY
;    FITEXY, xRegress, yRegress, A_intercept, B_slope, $
;      X_SIG = sig_xRegress, Y_SIG = sig_yRegress, $
;      sigma_A_B, chi_sq, q
      
    ;Get ready to display results in the plot window
    WSET, displayWindowIndex
    xmin   = FLOOR(5.0*MIN(xRegress - sig_xRegress))/5.0
    xmax   = CEIL(5.0*MAX(xRegress + sig_xRegress))/5.0
    ymin   = FLOOR(5.0*MIN(yRegress - sig_yRegress))/5.0
    ymax   = CEIL(5.0*MAX(yRegress + sig_yRegress))/5.0
    PLOT, [xmin, xmax], [ymin, ymax], /NODATA, $
      XRANGE = [xmin, xmax], YRANGE = [ymin, ymax], $
      XSTYLE = 1, YSTYLE = 1, $
      XTITLE = band1 + '_Mimir - ' + band2 + '_Mimir', $
      YTITLE = band1 + '_2MASS - ' + band1 + '_Mimir
    OPLOTERROR, xRegress, yRegress, sig_xRegress, sig_yRegress, PSYM=4, THICK = 2

;    OPLOT, !X.CRANGE, A_intercept + B_slope*!X.CRANGE, THICK=2, COLOR=RGB_TO_DECOMPOSED([255, 0, 0])
;    
;    ;Report results to user
;    PRINT_TEXT2, event, 'Best fit linear regression has'
;    PRINT_TEXT2, event, NEW_LINE() + 'FITEXY:'
;    PRINT_TEXT2, event, STRING(A_intercept, FORMAT='("INTERCEPT = ", F6.3)')
;    PRINT_TEXT2, event, STRING(B_slope,     FORMAT='("SLOPE     = ", F6.3)')
    
    ;******
    ;Use an MCMC method to more accurately sample the posterior distribution
    ;******
    PRINT_TEXT2, event, NEW_LINE() + 'Performing MCMC regression for intensity calibration constants...'
    LINMIX_ERR, xRegress, yRegress, POST, XSIG=sig_xRegress, YSIG=sig_yRegress,$
      NGAUSS=3, /METRO, /SILENT
    alpha = median(POST[*].alpha)
    beta  = median(POST[*].beta)
    postSamples  = TRANSPOSE([[post.alpha],[post.beta]])
    cov          = CORRELATE(postSamples, /COVARIANCE)
    s_alpha      = cov[0,0]
    s_beta       = cov[1,1]
    s_alpha_beta = cov[0,1]
    
    ;Overplot the best fit line
    OPLOT, !X.CRANGE, alpha + beta*!X.CRANGE, THICK=2, COLOR=RGB_TO_DECOMPOSED([255, 0, 0])
    
    ;Update the user on the resulting regression values
    PRINT_TEXT2, event, 'Best fit linear regression has'
    PRINT_TEXT2, event, STRING(alpha, s_alpha, FORMAT='("INTERCEPT = ", F6.3, " +/- ", F6.3)')
    PRINT_TEXT2, event, STRING(beta,  s_beta,  FORMAT='("SLOPE     = ", F6.3, " +/- ", F6.3)')
    
    ;Apply regression correction to intensity image
    ;Compute median color value in galaxy
    ;Start by identifying "galaxy region"
    
    ;Force images to be same size by cropping larger image
    sz1 = SIZE(band1Img, /DIMENSIONS)
    sz2 = SIZE(band2Img, /DIMENSIONS)
    
    ;Trim the x-dimension
    IF sz1[0] GT sz2[0] THEN BEGIN
      band1Img = band1Img[0:sz2[0]-1,*]
      SXADDPAR, band1Header, 'NAXIS1', sz2[0]
    ENDIF ELSE BEGIN
      band2Img = band2Img[0:sz1[0]-1,*]
      SXADDPAR, band2Header, 'NAXIS1', sz1[0]
    ENDELSE
    
    ;Trim the y-dimension
    IF sz1[1] GT sz2[1] THEN BEGIN
      band1Img = band1Img[*,0:sz2[1]-1]
      SXADDPAR, band1Header, 'NAXIS2', sz2[1]
    ENDIF ELSE BEGIN
      band2Img = band2Img[*,0:sz1[1]-1]
      SXADDPAR, band2Header, 'NAXIS2', sz1[1]
    ENDELSE
    
    ;Extract astrometry from both images
    EXTAST, band1Header, astr1
    EXTAST, band2Header, astr2
    AD2XY, galRA, galDec, astr1, galX1, galY1
    AD2XY, galRA, galDec, astr2, galX2, galY2
    
    ;Compute the instrumental color image
    fluxRatio = band1Img/band2Img
    badInds   = WHERE((fluxRatio LT 1e-6) OR ~FINITE(fluxRatio), numBad)
    IF numBad GT 0 THEN fluxRatio[badInds] = 1e-6
    instColorImg = sign2*2.5*ALOG10(fluxRatio)

    ;Compute uncertainty in instrumental color image
    s_instColorImg = 0.4*ALOG(10E)*SQRT((band1Sig/band1Img)^2E + (band2Sig/band2Img)^2E)
    
    ;Estimate the sky level in each band
    SKY, band1Img, skymode1, skynoise1, /SILENT
    SKY, band2Img, skymode2, skynoise2, /SILENT
    
    ;Find pixels well above the sky noies
    brightPix1 = (band1Img GT 10*skynoise1)
    brightPix2 = (band2Img Gt 10*skynoise2)
    
    ;Label each region of the 'brightPix#" arrays
    labelImg1 = LABEL_REGION(brightPix1)
    labelImg2 = LABEL_REGION(brightPix2)
    
    ;Get the values of the labeled galaxy region in each image
    labelVal1 = labelImg1[ROUND(galX1), ROUND(galY1)]
    labelVal2 = labelImg2[ROUND(galX2), ROUND(galY2)]
    
    ;Create arrays ONLY selected the galaxy region
    galRegion1 = labelImg1 EQ labelVal1
    galRegion2 = labelImg2 EQ labelVal2
    
    ;Estimate the mode of the colorImg in the galaxy region
    galRegion = galRegion1 AND galRegion2
    galInds   = WHERE(galRegion, numGalPix)
    IF numGalPix GT 0 THEN BEGIN
      SKY, instColorImg[galInds], colorMode, colorSig, /SILENT
      PRINT_TEXT2, event, $
        NEW_LINE() + STRING(colorMode, FORMAT='("Appling color correction using color of", F6.2, "  (GREEN PLOT MARKER)")')
      OPLOT, [colorMode], [alpha + beta*colorMode], $
        PSYM = 4, SYMSIZE=2, THICK=2, COLOR=RGB_TO_DECOMPOSED([0, 255, 0])
    ENDIF ELSE BEGIN
      PRINT_TEXT2, event, 'Could not find galaxy region... something is amiss.'
      STOP
    ENDELSE
    
    ;Now compute the conversion factor to match 2MASS magnitudes
    ;Flux of 0-mag source in 2MASS (Jy)
    ;Taken from Cohen et al. (2003) -- http://adsabs.harvard.edu/abs/2003AJ....126.1090C
    bandZPflux = [1594.0, 1024.0, 666.8]
    zpFlux  = (bandZPflux[band1Ind]*1E6)[0]   ;Grab zero-point flux and convert to MJy
    GETROT, astr1, rot, CDelt
    pl_sc   = SQRT(ABS(CDelt[0])*ABS(CDelt[1]))*3600.0
    pixArea = pl_sc^2
    bscale  = zpFlux*10D^(-0.4D*(alpha + beta*colorMode))/pixArea
    
    ;Now compute the (complicated) uncertainty in the calibrated intensity
    s_beta_colorMode = beta*colorMode*SQRT((s_beta/beta)^2 + (colorSig/colorMode)^2)
    s_arg            = SQRT(s_alpha^2 + s_beta_colorMode^2)
    s_bscale         = ABS(bscale*0.4*ALOG(10)*s_arg)
    
    ;Add BSCALE (and SBSCALE) keywords to header, and write final image to disk
    band1Header1 = band1Header              ;Copy the band1 header (add params to this)
    SXADDPAR, band1Header1, 'BSCALE', bscale
    SXADDPAR, band1Header1, 'SBSCALE', s_bscale
    SXADDPAR, band1Header1, 'BUNIT', 'uJy/arcsec^2'
    outputFile = FILE_DIRNAME(band1IntenFile) $
      + PATH_SEP() + FILE_BASENAME(band1IntenFile, '.FITS') + '_cal.fits'
    WRITEFITS, outputFile, band1Img, band1Header1
    WAIT, 5.0
    
    ;*********************************************************
    ;* Continue processing to compute calibrated color image *
    ;*********************************************************
    ;Compute the regression quantities for the color map
    yRegress     = starInfo.(magTag1) - starInfo.(magTag2)
    sig_yRegress = SQRT(s_mag1^2 + starInfo.(sigMagTag1)^2)
    
    xRegress     = sign1*mag1 + sign2*mag2
    sig_xRegress = SQRT(s_mag1^2 + s_mag2^2)
    
    ;Compute plot data range
    xmin   = FLOOR(5.0*MIN(xRegress - sig_xRegress))/5.0
    xmax   = CEIL(5.0*MAX(xRegress + sig_xRegress))/5.0
    ymin   = FLOOR(5.0*MIN(yRegress - sig_yRegress))/5.0
    ymax   = CEIL(5.0*MAX(yRegress + sig_yRegress))/5.0

    ;Plot regression data and errorbars
    PLOT, [xmin, xmax], [ymin, ymax], /NODATA, $
      XRANGE = [xmin, xmax], YRANGE = [ymin, ymax], $
      XSTYLE = 1, YSTYLE = 1, $
      XTITLE = band1 + '_Mimir - ' + band2 + '_Mimir', $
      YTITLE = band1 + '_2MASS - ' + band2 + '_2MASS'
    OPLOTERROR, xRegress, yRegress, sig_xRegress, sig_yRegress, PSYM=4, THICK = 2
    
    ;******
    ;Use an MCMC method to more accurately sample the posterior distribution
    ;******
    PRINT_TEXT2, event, NEW_LINE() + 'Performing MCMC regression for color calibration constants...'
    LINMIX_ERR, xRegress, yRegress, POST, XSIG=sig_xRegress, YSIG=sig_yRegress,$
      NGAUSS=3, /METRO, /SILENT
    alpha = median(POST[*].alpha)
    beta  = median(POST[*].beta)
    postSamples  = TRANSPOSE([[post.alpha],[post.beta]])
    cov          = CORRELATE(postSamples, /COVARIANCE)
    s_alpha      = cov[0,0]
    s_beta       = cov[1,1]
    s_alpha_beta = cov[0,1]
    
    ;Overplot resulting line
    OPLOT, !X.CRANGE, alpha + beta*!X.CRANGE, THICK=2, COLOR=RGB_TO_DECOMPOSED([255, 0, 0])
    
    ;Report results to user
    PRINT_TEXT2, event, 'Best fit linear regression has'
    PRINT_TEXT2, event, STRING(alpha, s_alpha, FORMAT='("INTERCEPT = ", F6.3, " +/- ", F6.3)')
    PRINT_TEXT2, event, STRING(beta,  s_beta,  FORMAT='("SLOPE     = ", F6.3, " +/- ", F6.3)'
    
    ;Fill in the color of the galaxy where SNR > 10 in both bands
    galRegion = galRegion1 AND galRegion2
    galInds   = WHERE(galRegion, numGalPix, $
      COMPLEMENT=badInds, NCOMPLEMENT=numBadPix)
    IF numGalPix GT 0 THEN BEGIN
      ;Fill in color image
      colorImg = 0*instColorImg
      colorImg[galInds] = alpha + beta*instColorImg[galInds]

      ;Fill in color unctainty image
      s_colorImg = 0*instColorImg
      s_colorImg[galInds] = SQRT($
        (s_alpha)^2 + $
        (instColorImg[galInds]*s_beta)^2 + $
        (beta*s_instColorImg[galInds])^2)
      
    ENDIF
    IF numBadPix GT 0 THEN BEGIN
      colorImg[badInds] = !VALUES.F_NAN
    ENDIF
    
    ;Write color image to disk
    fileBase   = FILE_BASENAME(band1IntenFile, '.FITS')
    fileBase   = REVERSE(STRSPLIT(fileBase, '_', /EXTRACT))
    fileBase   = REVERSE(fileBase[2:*])
    fileBase_c = STRJOIN(fileBase, '_') + '_' + band1 + '-' + band2 + '_map.fits'
    outputFile = groupStruc.analysis_dir + $
      'S11_Full_Field_Polarimetry' + PATH_SEP() + $
      fileBase_c
    WRITEFITS, outputFile, colorImg, band1Header

    ;Write uncertainty in color image to disk
    fileBase_s = STRJOIN(fileBase, '_') + '_s_' + band1 + '-' + band2 + '_map.fits'
    outputFile = groupStruc.analysis_dir + $
      'S11_Full_Field_Polarimetry' + PATH_SEP() + $
      fileBase_s
    WRITEFITS, outputFile, s_colorImg

    PRINT_TEXT2, event, 'Completed two band photometric calibration.'
    
  ENDIF ELSE BEGIN
    ;
    ;This section of code simply computes a single zero-point offset and uses that to apply BSCALE
    ;
    ;Read in the file
    pathBand1Img = groupStruc.analysis_dir + $
      'S11_Full_Field_Polarimetry' + PATH_SEP()
    band1IntenFile = (FILE_SEARCH(pathBand1Img, '*_I.fits', COUNT = numBand1))[0]
    
    IF numBand1 EQ 1 THEN BEGIN
      band1Img       = READFITS(band1IntenFile, band1Header)
      EXTAST, band1Header, astr1
    ENDIF ELSE BEGIN
      PRINT_TEXT2, event, 'Could not find intensity image to calibrate.'
    ENDELSE
    
    PRINT_TEXT2, event, 'Reading in photometry from one band.'
    pathBand1 = groupStruc.analysis_dir + $
      'S11_Full_Field_Polarimetry' + $
      PATH_SEP() + groupStruc.NIRband + '_photometry.dat'
      
    ;Check that photometry file actually exists
    IF FILE_TEST(pathBand1) THEN BEGIN
      READCOL, pathBand1, ID1, RA1, Dec1, mag1, s_mag1, $
        FORMAT='A,F,F,F,F', COMMENT = ';'
    ENDIF ELSE BEGIN
      PRINT_TEXT2, event, 'Could not find photometry file.'
      RETURN
    ENDELSE
    
    ;Now perform 1 band photometric reduction
    starIDs  = strtrim(groupStruc.starInfo._2MASS, 2)
    MATCH, ID1, starIDs, ind1, starInds, COUNT=numMatch
    
    ;Now grab the photometry information for the matched stars
    ;Treat the Mimir values
    ID1      = ID1[ind1]
    RA1      = RA1[ind1]
    Dec1     = Dec1[ind1]
    mag1     = mag1[ind1]
    s_mag1   = s_mag1[ind1]
    
    ;Treat the 2MASS info
    starInfo = groupStruc.starInfo[starInds]
    
    ;Grab the starInfo structure index of band1 and band2
    ;Use the groupStruc.NIRband element for band1
    band1    = groupStruc.NIRband
    testBand = TAG_EXIST(groupStruc.starInfo, $                         ;Find the tag containing magnitudes for this band
      STRMID(band1,0,1) + 'MAG', INDEX = magTag1)
    testSig  = TAG_EXIST(groupStruc.starInfo, $                         ;Find the tag containing magnitude uncertainties for this band
      'E_' + STRMID(band1,0,1) + 'MAG', INDEX = sigMagTag1)
      
    ;Compute differences between 2MASS and Mimir magnitudes
    deltaMags    = mag1 - starInfo.(magTag1)
    sigDeltaMags = SQRT(s_mag1^2 + starInfo.(sigMagTag1)^2)
    
    ;Compute average magnitude offset
    wts      = 1.0/sigDeltaMags^2.0
    zpMag    = TOTAL(wts*deltaMags)/TOTAL(wts)
    sigZPmag = SQRT(1.0/TOTAL(wts))
    PRINT_TEXT2, event, $
      STRING(zpMag, sigZPmag, FORMAT='("Computed zero-point magnitude of ", F6.2, "+/- ", F6.4)')
      
    ;Parse band index
    NIRbands   = ['J', 'H', 'K']
    band1      = (STRMID(groupStruc.NIRband, 0, 1))[0]
    band1Ind   = WHERE((STRMID(band1, 0, 1))[0] EQ NIRbands)
    
    ;Now compute the conversion factor to match 2MASS magnitudes
    ;Flux of 0-mag source in 2MASS (Jy)
    ;Taken from Cohen et al. (2003) -- http://adsabs.harvard.edu/abs/2003AJ....126.1090C
    bandZPflux = [1594.0, 1024.0, 666.8]
    zpFlux  = (bandZPflux[band1Ind]*1E6)[0]   ;Grab zero-point flux and convert to MJy
    GETROT, astr1, rot, CDelt
    pl_sc   = SQRT(ABS(CDelt[0])*ABS(CDelt[1]))*3600.0
    pixArea = pl_sc^2
    bscale  = zpFlux*10D^(-0.4D*(zpMag))/pixArea
    
    ;Compute untertainty in bscale value
    s_bscale = bscale*(0.4*ALOG(10))*sigZPmag
    
    ;Add BSCALE keyword to header, and write final image to disk
    band1Header1 = band1Header              ;Copy the band1 header (add params to this)
    SXADDPAR, band1Header1, 'BSCALE', bscale
    SXADDPAR, band1Header1, 'SBSCALE', s_bscale
    SXADDPAR, band1Header1, 'BUNIT', 'uJy/arcsec^2'
    fileBase   = FILE_BASENAME(band1IntenFile, '.FITS')
    baseLen    = STRLEN(fileBase)
    fileBase   = STRMID(fileBase, 0, baseLen - 4)
    outputFile = groupStruc.analysis_dir + $
      'S11_Full_Field_Polarimetry' + PATH_SEP() + $
      fileBase + '_cal.fits'
    WRITEFITS, outputFile, band1Img, band1Header1
    
    PRINT_TEXT2, event, 'Single band photometric calibration complete.'
  ENDELSE
  
END
