;PRO S3A_FIND_ASTROMETRY_FILES, event
;
;  ;groupStruc.astroFlags KEY
;  ;0: no astrometry performed yet
;  ;1: good for photometry and supersky
;  ;2: good for supersky but not photometry
;  ;3: no good at all
;
;  numFileWID = WIDGET_INFO(event.top, FIND_BY_UNAME='S3A_NUM_FILES_TEXT') ;Retrieve the num-failed text widget ID
;  WIDGET_CONTROL, event.top, GET_UVALUE=groupStruc                    ;Retrieve the group summary structure
;
;  IF groupStruc.numS3failed EQ -1L THEN BEGIN
;    PRINT_TEXT2, event, 'Testing for failed astrometry images'
;    FOR i = 0, groupStruc.numGroups - 1 DO BEGIN
;      FOR j = 0, groupStruc.groupNumbers[i] - 1 DO BEGIN
;        S3filename = groupStruc.analysis_dir + PATH_SEP() + $         ;Setup the path to the file to be tested
;          'S3_Astrometry' + PATH_SEP() + FILE_BASENAME(groupStruc.groupImages[i,j])
;        S3fileTest = FILE_TEST(S3filename)                            ;Check if that file even exists (passed PPOL step 3)
;        IF S3fileTest THEN BEGIN
;          header = HEADFITS(S3filename)                               ;Check if the existing file has reliable astrometry
;          numStar = SXPAR(header, 'PPOLNSTR')
;          IF numStar LT 3 THEN groupStruc.astroFlags[i,j] = 0         ;Mark all files that fail these tests
;        ENDIF ELSE groupStruc.astroFlags[i,j] = 0
;      ENDFOR
;    ENDFOR
;    numFailed    = TOTAL(groupStruc.astroFlags EQ 0)                  ;Count the number of failed images
;    groupStruc.numS3failed = LONG(numFailed)                          ;Store the number of failed images
;    UPDATE_GROUP_SUMMARY, event, groupStruc                           ;Save the failed image number to disk
;  ENDIF ELSE numFailed = groupStruc.numS3failed                       ;Otherwise retrieve the previously found number
;;  formatString = '("Identified ",I3," images with failed astrometry")';Format the message to the user
;  WIDGET_CONTROL, numFileWID, SET_VALUE=STRING(numFailed, FORMAT='(I4)') ;Update the number failed text field
;;  PRINT_TEXT2, event, STRING(numFailed, FORMAT = formatString)
;END

PRO S3A_MAGNITUDE_RANGE, event;, stash

  displayWID = WIDGET_INFO(event.top,FIND_BY_UNAME='IMAGE_DISPLAY_WINDOW');Grab the display window WID
;  tlb_wid    = WIDGET_INFO(event.top,FIND_BY_UNAME='WID_BASE')       ;Grab the top-level-base WID
  WIDGET_CONTROL, event.top, GET_UVALUE=groupStruc                    ;Grab the group summary structure
  WIDGET_CONTROL, displayWID, GET_VALUE = displayWindowIndex          ;Grab the display window index
  wS3AmagRangeSlider = WIDGET_INFO(event.top, FIND_BY_UNAME='S3A_MAG_RANGE')
  stateStorageWID    = WIDGET_INFO(wS3AmagRangeSlider, /CHILD)
  WIDGET_CONTROL, stateStorageWID, GET_UVALUE=state, /NO_COPY         ;Grab the dual-slider "state" variable
  
  ;APPLY CHANGES TO SELECTED STARS
  IF groupStruc.NIRband EQ 'H' THEN magIndex = 5 $                    ;Setup the structure tag for this band
    ELSE IF groupStruc.NIRband EQ 'Ks' THEN magIndex = 7
  useStars = groupStruc.starInfo.(magIndex) GE state.value[0] $       ;Identify which stars to Use
         AND groupStruc.starInfo.(magIndex) LE state.value[1]
  
  WSET, displayWindowIndex                                            ;Set that display window as active
 
  useStarInds = WHERE(useStars, numUse, $                             ;Find the stars in the selected magnitude range
    COMPLEMENT = noUseStarInds, NCOMPLEMENT = numNoUse)
  EXTAST, groupStruc.displayHeader, astr                              ;Extract the image astrometry
  AD2XY, groupStruc.starInfo.RAJ2000, groupStruc.starInfo.DEJ2000,$   ;Convert star positions to pixel coordinates
    astr, xStars, yStars

  IF numUse GT 0 THEN BEGIN
    greenColorInd = RGB_TO_DECOMPOSED([0,255,0])
    OPLOT, (xStars[useStarInds]-1), (yStars[useStarInds]-1), $        ;Show the user where the selected stars are
      PSYM = 6, COLOR = greenColorInd
  ENDIF
  IF numNoUse GT 0 THEN BEGIN
    redColorInd = RGB_TO_DECOMPOSED([255,0,0])
    OPLOT, (xStars[noUseStarInds]-1), (yStars[noUseStarInds]-1), $    ;Show the user where the unselected stars are
      PSYM = 6, COLOR = redColorInd
  ENDIF

  ;Restore "state" variable
  WIDGET_CONTROL, stateStorageWID, SET_UVALUE=state, /NO_COPY         ;Grab the widget "state" variable

END


PRO S3A_SELECT_ASTROMETRY_MAGNITUDE_RANGE, event

;  tlb_wid = WIDGET_INFO(event.top, FIND_BY_UNAME='WID_BASE')         ;Retrieve the TLB widget ID
  WIDGET_CONTROL, event.top, GET_UVALUE=groupStruc                    ;Retrieve the group summary structure
  wS3AmagRangeSlider    = WIDGET_INFO(event.top, FIND_BY_UNAME='S3A_MAG_RANGE')
  wS3AmagRangeSensitive = WIDGET_INFO(wS3AmagRangeSlider, /SENSITIVE) ;Determine if the slider widget is sensitive or not
 
    ;If the dual slider is desensitized, then sensitize it and change button value
  IF wS3AmagRangeSensitive EQ 0 THEN BEGIN
    WIDGET_CONTROL, wS3AmagRangeSlider, SENSITIVE = 1
    WIDGET_CONTROL, event.ID, SET_VALUE='Slider Active'
  ENDIF

  ;IF the dual slider is sensitized, then desensiize it and change the button value
  IF wS3AmagRangeSensitive EQ 1 THEN BEGIN
    WIDGET_CONTROL, wS3AmagRangeSlider, SENSITIVE = 0
    WIDGET_CONTROL, event.ID, SET_VALUE='Slider Inactive'
    
    stateStorageWID = WIDGET_INFO(wS3AmagRangeSlider, /CHILD)
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
    
    
    formatString = '("Astrometry will be performed using stars between ",' $
                   + formatStrings[0] + '," and ",' $
                   + formatStrings[1] + '," magnitudes.")'
    PRINT_TEXT2, event, $
      STRING(state.value, FORMAT=formatString)
    
    ;Flag the stars to use
    IF groupStruc.NIRband EQ 'H' THEN magIndex = 5 $                    ;Setup the structure tag for this band
      ELSE IF groupStruc.NIRband EQ 'Ks' THEN magIndex = 7
    groupStruc.astroStarFlags = groupStruc.starInfo.(magIndex) GE state.value[0] $
                            AND groupStruc.starInfo.(magIndex) LE state.value[1]
    
    UPDATE_GROUP_SUMMARY, event, groupStruc                           ;Store the updated data to disk
    WIDGET_CONTROL, stateStorageWID, SET_UVALUE=state, /NO_COPY       ;Restore the dual-slider "state" variable
    
  ENDIF

END

PRO S3A_ASTROMETRY_REPAIR, event

  red   = RGB_TO_DECOMPOSED([255,0,0])
  green = RGB_TO_DECOMPOSED([0,255,0])
  displayWID      = WIDGET_INFO(event.top, FIND_BY_UNAME='IMAGE_DISPLAY_WINDOW')
  imageProgWID    = WIDGET_INFO(event.top, FIND_BY_UNAME='IMAGE_PROGRESS_BAR')
  groupProgWID    = WIDGET_INFO(event.top, FIND_BY_UNAME='GROUP_PROGRESS_BAR')
  begin_wid       = WIDGET_INFO(event.top, FIND_BY_UNAME='S3A_BEGIN_ASTROMETRY')
  save_wid        = WIDGET_INFO(event.top, FIND_BY_UNAME='S3A_SAVE_PROGRESS')
  acceptPhoto_wid = WIDGET_INFO(event.top, FIND_BY_UNAME='S3A_ACCEPT_PHOTO')
  acceptAstro_wid = WIDGET_INFO(event.top, FIND_BY_UNAME='S3A_ACCEPT_ASTRO')
  reject_wid      = WIDGET_INFO(event.top, FIND_BY_UNAME='S3A_REJECT_IMAGE')
  RAtext_wid      = WIDGET_INFO(event.top, FIND_BY_UNAME='S3A_RA_TEXT')
  DecText_wid     = WIDGET_INFO(event.top, FIND_BY_UNAME='S3A_DEC_TEXT')
  plateSc_wid     = WIDGET_INFO(event.top, FIND_BY_UNAME='S3A_PLATE_SCALE')
  rotAng_wid      = WIDGET_INFO(event.top, FIND_BY_UNAME='S3A_ROT_ANGLE')

  WIDGET_CONTROL, event.ID, SENSITIVE=0                              ;Desensitize the "Begin Astrometry" button
  WIDGET_CONTROL, event.top, GET_UVALUE=groupStruc                    ;Retrieve the group summary structure
  WIDGET_CONTROL, displayWID, GET_VALUE=windowIndex
  WSET, windowIndex                                                   ;Prepare to plot in display window

  astroStarInds = WHERE(groupStruc.astroStarFlags, numUse)            ;Find the stars to be used in astrometry
  IF numUse EQ 0 THEN BEGIN
    PRINT_TEXT2, event, 'There are no astrometry stars to use.'
    PRINT_TEXT2, event, 'Try a different magnitude range.'
    RETURN
  ENDIF

  astroStars      = groupStruc.starInfo[astroStarInds]                ;Save the astrometry star information
;  numFailed       = groupStruc.numS3failed                            ;Grab the total number of failed images
  failedImageInds = WHERE(groupStruc.astroFlags EQ 0, numRemaining)   ;Locate the failed images in the image list
  
  ;groupStruc.astroFlags KEY
  ;0: no astrometry performed yet
  ;1: good for photometry and supersky
  ;2: good for supersky but not photometry
  ;3: no good at all

  UPDATE_PROGRESSBAR, groupProgWID, /ERASE
  UPDATE_PROGRESSBAR, imageProgWID, /ERASE
  
  numGoodGroups = TOTAL(groupStruc.groupFlags)
  groupCount    = 0
  FOR i = 0, groupStruc.numGroups - 1 DO BEGIN
    IF groupStruc.groupFlags[i] EQ 0 THEN CONTINUE
    groupCount++
    IF TOTAL((groupStruc.astroFlags[i,*] GT 0)) EQ groupStruc.groupNumbers[i] THEN CONTINUE
    groupProgString = STRING((groupCount), numGoodGroups, FORMAT='("Group ",I2," of ",I2)')
    UPDATE_PROGRESSBAR, groupProgWID, 100*FLOAT(i+1)/numGoodGroups, DISPLAY_MESSAGE=groupProgString
    WAIT, 0.1
    FOR j = 0, groupStruc.groupNumbers[i] - 1 DO BEGIN
      IF groupStruc.astroFlags[i,j] NE 0 THEN CONTINUE                  ;Skip over finished images
      imageProgString = STRING((j+1), groupStruc.groupNumbers[i], FORMAT='("Image ",I2," of ",I2)')
      UPDATE_PROGRESSBAR, imageProgWID, 100*FLOAT(j+1)/groupStruc.groupNumbers[i], DISPLAY_MESSAGE=imageProgString
      WAIT, 0.04
      
      imgFile  = groupStruc.groupImages[i,j]                            ;Grab the BDP file path for this image
      filename = (REVERSE(STRSPLIT(imgFile, PATH_SEP(), /EXTRACT)))[0]  ;Grab the filename from the path
      S2file   = groupStruc.analysis_dir + $                            ;Construct the S2 file path
        'S2_Ski_Jump_Fixes' + PATH_SEP() + filename
      IF FILE_TEST(S2file) THEN imgFile = S2file                        ;If S2 file exists, then use it!
      
      img      = READFITS(imgFile, header)                              ;Read in failed image
      fixedImg = MODEL_BAD_PIXELS(img)                                  ;Fix bad pixels
      hist     = SXPAR(header, "HISTORY")                               ;Get the history info
      SXDELPAR, header,'HISTORY'                                        ;delete any previous history entries
      telRA    = STRSPLIT(SXPAR(header, 'TELRA'), ':', /EXTRACT)        ;Store the telescope RA pointing
      telRA    = 15D*TEN(telRA[0], telRA[1], telRA[2])                  ;Convert pointing to float (deg)
      telDec   = STRSPLIT(SXPAR(header, 'TELDEC'), ':', /EXTRACT)       ;Store the telescope Dec pointing
      telDec   = TEN(telDec[0], telDec[1], telDec[2])                   ;Convert pointing to float (deg)
      
      ;Estimate the astrometry
      sz      = SIZE(img, /DIMENSIONS)
      cdGuess = [[-0.000160833, 0],$                                    ;Store astrometry guess (0.579 arcsec/pix)
                 [0,  0.000160833]]
      MAKE_ASTR, astrGuess, CRPIX = 0.5*sz, CRVAL = [telRA, telDec], $  ;Initalize astrometry structure
        CD = cdGuess
      
      AD2XY, astroStars.RAJ2000, astroStars.DEJ2000, $                  ;Position astrometry stars using astrometry guess
             astrGuess, xGuess, yGuess
      useStar = (xGuess GT 80) AND (xGuess LT (sz[0] - 81)) $           ;Only use stars more than 80 pixels from image edge
        AND (yGuess GT 80) AND (yGuess LT (sz[1] - 81))
      useInds = WHERE(useStar, numUse)                                  ;Get the indices of the usable stars
      
      ;Display the *.fits image
      SKY, fixedImg, skyMode, skyNoise, /SILENT                         ;Estimate sky brightness and noise
      logRange    = FLTARR(2)
      logRange[0] = ALOG(1E-6) > ALOG(skyMode)                          ;Make sure bottom of logRange[0] > 0
      logRange[1] = ALOG(skyMode + 20*skyNoise)                         ;Set upper limit to 10-sigma
      TVIM, fixedImg, RANGE = skyMode + [-3,+10]*skyNoise               ;Display image  
      
      XYOUTS, 0.5, 0.96, FILE_BASENAME(groupStruc.groupImages[i,j]), /NORMAL, ALIGNMENT = 0.5
      XYOUTS, 0.5, 0.05, 'Click on highlighted star', /NORMAL, ALIGNMENT = 0.5
      
      OPLOT, xGuess, yGuess, PSYM=4, COLOR=red                          ;Overplot the astrometry stars
      OPLOT, [xGuess[useInds[0]]], [yGuess[useInds[0]]], $              ;Mark the brightest usable star
        PSYM=6, COLOR=green, SYMSIZE = 2
      CURSOR, xStar, yStar, /DATA, /DOWN                                ;Click on the bright star
  
      ;Cut out a subarray for a more precise positioning
      xOff     = (xStar - 19) > 0
      xRt      = (xOff  + 40) < (sz[0] - 1)
      yOff     = (yStar - 19) > 0
      yTop     = (yOff + 40)  < (sz[1] - 1)
      subArray = fixedImg[xOff:xRt, yOff:yTop]
      
      done = 0
      WHILE (done EQ 0) DO BEGIN
        TVIM, subArray, RANGE = [skyMode-skyNoise, MAX(subArray)]       ;Show the user a zoom-in of the star
        XYOUTS, 0.5, 0.06, 'Click on the star', /NORMAL, ALIGNMENT = 0.5
;        OPLOT, [20], [20], PSYM=6, COLOR=green
        
        ;Click on the star
        CURSOR, xStar, yStar, /DATA, /DOWN
        ;Centroid the star using GCTRD
        GCNTRD, subArray, xStar, yStar, xcen, ycen, 3.0 ;, /SILENT      ;Compute the centroid about the clicked position
        OPLOT, [xcen,xcen], [0,40], LINESTYLE = 2, THICK = 2            ;Draw cross-hairs on the estimated star position
        OPLOT, [0,40], [ycen,ycen], LINESTYLE = 2, THICK = 2
        
        POLYFILL, [0,0,1,1,0], [0,0.09,0.09,0,0], /NORMAL, COLOR=!P.BACKGROUND
        XYOUTS, 0.5, 0.06, 'Left click to accept centroid poisition', /NORMAL, ALIGNMENT = 0.5
        XYOUTS, 0.5, 0.02, 'Right click to do manual positioning', /NORMAL, ALIGNMENT = 0.5 
  
        CURSOR, junk1, junk2, /DOWN
  
        IF (!MOUSE.BUTTON EQ 1) THEN done = 1
        IF (!MOUSE.BUTTON EQ 4) THEN done = 2
        
      ENDWHILE
      
      IF (done EQ 2) THEN BEGIN                                         ;If the user did not accept, then manually position star
        done = 0
        WHILE (done EQ 0) DO BEGIN
          
  ;        FOR blinkCount = 0, 4 DO BEGIN
  ;          TVIM, subArray, RANGE = [skyMode-skyNoise, MAX(subArray)]   ;Show the image
  ;          XYOUTS, 0.5, 0.95, 'Manually locate this star', /NORMAL, ALIGNMENT = 0.5
  ;          IF ((blinkCount MOD 2) EQ 0 ) $                             ;Blink a warning message to the user
  ;            THEN XYOUTS, 0.5, 0.5, 'THIS WILL BE THE FINAL LOCATION', /NORMAL, $
  ;            ALIGNMENT = 0.5, CHARSIZE = 2, CHARTHICK = 2
  ;          WAIT, 0.4
  ;        ENDFOR
          
          TVIM, subArray, RANGE = [skyMode-skyNoise, MAX(subArray)]     ;Show the user a zoom-in of the star
          XYOUTS, 0.5, 0.06, 'Click on the star', /NORMAL, ALIGNMENT = 0.5
;          OPLOT, [20], [20], PSYM=6, COLOR=green
          
          ;Click on the star
          CURSOR, xcen, ycen, /DATA, /DOWN
          OPLOT, [xcen,xcen], [0,40], LINESTYLE = 2, THICK = 2          ;Draw cross-hairs on the estimated star position
          OPLOT, [0,40], [ycen,ycen], LINESTYLE = 2, THICK = 2
          
          POLYFILL, [0,0,1,1,0], [0,0.09,0.09,0,0], /NORMAL, COLOR=!P.BACKGROUND
          XYOUTS, 0.5, 0.06, 'Left click to accept poisition', /NORMAL, ALIGNMENT = 0.5
          XYOUTS, 0.5, 0.02, 'Right click to try again', /NORMAL, ALIGNMENT = 0.5
          
          CURSOR, junk1, junk2, /DOWN
          
          IF (!MOUSE.BUTTON EQ 1) THEN done = 1
  ;        IF (!MOUSE.BUTTON EQ 4) THEN done = 2
  ;        STOP
          
  ;        WIDGET_CONTROL, accept_wid, SENSITIVE=1                       ;Sensitize the accept button
  ;        WIDGET_CONTROL, reject_wid, SENSITIVE=1                       ;Sensitize the reject button
  ;        
  ;        decisionEvent = WIDGET_EVENT([accept_wid, reject_wid])        ;Wait for the user to accept or reject astrometry
  ;        IF decisionEvent.ID EQ accept_wid THEN done = 1               ;If the user approved, then continue        
        ENDWHILE
      ENDIF
      
      ;Store the brightest star (x, y) pixel position
      xStar = xcen + xOff                                               ;Recompute the star x-position
      yStar = ycen + yOff                                               ;Recompute the star y-position
      shiftX  = xStar - xGuess[useInds[0]]                              ;Compute x-offset correction
      shiftY  = yStar - yGuess[useInds[0]]                              ;Compute y-offset correction
      xGuess += shiftX                                                  ;Update estimated star x-positions
      yGuess += shiftY                                                  ;Update estimated star y-positions
      useStar = (xGuess GT 20) AND (xGuess LT (sz[0] - 21)) $           ;Update star usage flags
            AND (yGuess GT 20) AND (yGuess LT (sz[1] - 21))             ;based on new star (x,y) positions
      
      useInds = WHERE(useStar, numUse)
      IF ~(numUse GT 0) THEN STOP ELSE BEGIN                            ;If there are no usable stars, then stop
        xGuess      = xGuess[useInds]                                    ;Otherwise cull the star position estimates
        yGuess      = yGuess[useInds]                                    ;to only include the usable stars
        astroStars1 = astroStars[useInds]
      ENDELSE
      
      nStars = numUse                                                   ;Count the number of stars to find
      FWHMs  = FLTARR(nStars)                                           ;Array to store FWHM of each star
  
      xStars = xGuess                                                   ;Now that the stars have been "centroided"
      yStars = yGuess                                                   ;they are no longer "guesses"
  
      FOR k = 0, nStars - 1 DO BEGIN                                    ;Loop through each star and find its gcentrd value
        xOff     = (xGuess[k] - 20) > 0
        xRt      = (xOff  + 40) < (sz[0] - 1)
        yOff     = (yGuess[k] - 20) > 0
        yTop     = (yOff + 40)  < (sz[1] - 1)
        subArray = fixedImg[xOff:xRt, yOff:yTop]                        ;Cut out a subarry for fine tuning star position
        result   = GAUSS2DFIT(subArray, A, /TILT)                       ;Gaussian fit the star
        inArray  = (A[4] GT 5) AND (A[4] LT 35) $                       ;If the fit is located in the array
          AND (A[5] GT 5) AND (A[5] LT 35)
        okShape  = (A[2] GT 0.8) AND (A[2] LT 5) $                      ;and if its gaussian width is reasonable (not a hot pixel)
          AND (A[3] GT 0.8) AND (A[3] LT 5)
        
        IF inArray AND okShape THEN method = 'gaussianGuess' $          ;Define which method should be used first
          ELSE method = 'manualAssist'
          
        SWITCH method OF
          
          ;This method simply performs a gaussian fit and computes a gaussian centroid at the expected star position.
          ;If the two methods agree, then the we BREAK out of the SWITCH.
          ;If the two methods disagree or there are other problems, then we proceed with a user assisted centroid method.
          'gaussianGuess': BEGIN
            ;If everything is in order, then simply proceed with centroid
            FWHMs[k] = SQRT(ABS(A[2]*A[3]))*2.355                       ;Compute the FWHM for this star
            GCNTRD, subArray, A[4], A[5], xcen, ycen,  FWHMs[k]         ;Centroid this star (using estimated FWHM)
            methodDifference = SQRT((xCen - A[4])^2 + (yCen - A[5])^2)  ;Compute difference between the two locating methods
            IF (methodDifference LE 1) AND FINITE(methodDifference) $   ;If the two methods have a consensus,
              THEN BREAK                                                ;then we have our answer! (exit SWTICH blocks)
            END
          
          ;This method cuts out a much smaller subarray centered on the user's click for the star location.
          ;The smaller array generally has fewer problems fitting a 2D gaussian.
          ;A gaussian centroid is then performed (also centered about the user's click for the star location)
          ;using the 2D gaussian fit FWHM for the smoothing kernal.
          ;The results of these methods are then displayed to the user for approval or rejection.
          ;An approved fit will BREAK out of the SWITCH.
          ;If an approved fit is not found in 5 attempts, then the final method is used---SIMPLE USER SPECIFICATION.
          'manualAssist': BEGIN
            attemptCount = 0
            done         = 0
            WHILE (done EQ 0) DO BEGIN
              TVIM, ALOG(subArray), RANGE = logRange                    ;Show the image
              XYOUTS, 0.5, 0.95, 'Manual assist star locator', /NORMAL, ALIGNMENT = 0.5
              CURSOR, xcen1, ycen1, /DATA, /DOWN                        ;Click on the star
              
              xoff1      = xcen1 - 6                                    ;cut out a 13x13 array centered on the star
              xrt        = xoff1 + 12
              yoff1      = ycen1 - 6
              ytop       = yoff1 + 12
              subArray1  = subArray[xOff1:xRt, yOff1:yTop]
              result     = GAUSS2DFIT(subArray1, A, /TILT)              ;Gaussian fit the star
              okShape    = (A[2] GT 0.8) AND (A[2] LT 5) $              ;and if its gaussian width is reasonable (not a hot pixel)
                AND (A[3] GT 0.8) AND (A[3] LT 5)
              IF okShape THEN FWHMs[k] = SQRT(ABS(A[2]*A[3]))*2.355 $   ;Compute the FWHM for this star
                ELSE FWHMs[k] = 3.0                                     ;If shape is bad, then just use 3.0 as FWHM
              GCNTRD, subArray, xcen1, ycen1, xcen, ycen, FWHMs[k]      ;Centroid the star with a guess at the FWHM
              IF (xcen GT 0) AND (xcen LT 40) $                         ;Check if the centroid is ON image
                AND (ycen GT 0) AND (ycen LT 40) THEN BEGIN
                  
                OPLOT, [xcen,xcen], [0,40], LINESTYLE = 2, THICK = 2    ;Draw cross-hairs on the estimated star position
                OPLOT, [0,40], [ycen,ycen], LINESTYLE = 2, THICK = 2
                
                POLYFILL, [0,0,1,1,0], [0,0.09,0.09,0,0], /NORMAL, COLOR=!P.BACKGROUND
                XYOUTS, 0.5, 0.06, 'Left click to accept centroid poisition', /NORMAL, ALIGNMENT = 0.5
                XYOUTS, 0.5, 0.02, 'Right click to try again', /NORMAL, ALIGNMENT = 0.5
                
                CURSOR, junk1, junk2, /DOWN
                
                IF (!MOUSE.BUTTON EQ 1) THEN done = 1
                IF (!MOUSE.BUTTON EQ 4) THEN attemptCount++
                
  ;              WIDGET_CONTROL, accept_wid, SENSITIVE=1                 ;Sensitize the accept button
  ;              WIDGET_CONTROL, reject_wid, SENSITIVE=1                 ;Sensitize the reject button
  ;              
  ;              decisionEvent = WIDGET_EVENT([accept_wid, reject_wid])  ;Wait for the user to accept or reject astrometry
  ;              IF decisionEvent.ID EQ accept_wid THEN BEGIN
  ;                WIDGET_CONTROL, accept_wid, SENSITIVE=0               ;Desensitize the button
  ;                WIDGET_CONTROL, reject_wid, SENSITIVE=0               ;Desensitize the button
  ;                done = 1
  ;              ENDIF ELSE attemptCount++                               ;Increment the attempt counter if user didn't like fit
  ;              IF decisionEvent.ID EQ accept_wid THEN BREAK            ;If the user approved, then continue
  ;              
  ;              WIDGET_CONTROL, accept_wid, SENSITIVE=0                 ;Desensitize the button
  ;              WIDGET_CONTROL, reject_wid, SENSITIVE=0                 ;Desensitize the button
              ENDIF ELSE BEGIN                                          ;If the centroid if OFF image, then...
                IF attemptCount GT 4 THEN done = 2                      ;If this has been tried 5 times, close the loop
                attemptCount++                                          ;Increment the attempt counter if centroid is off image
              ENDELSE
;              STOP
            ENDWHILE
            IF (done EQ 1) THEN BREAK                                     ;Break out of SWITCH if star position was accepted
            END
            
          ;In this method, the user is simply asked to click on the star location.
          ;The user can also reject accidental clicks.
          ;The first accepted click-location will be used as the star location.
          'manualPosition': BEGIN
            
  ;          FOR blinkCount = 0, 4 DO BEGIN
  ;            TVIM, ALOG(subArray), RANGE = logRange                    ;Show the image
              XYOUTS, 0.5, 0.95, 'Fully manual star locator', /NORMAL, ALIGNMENT = 0.5
  ;            IF ((blinkCount MOD 2) EQ 0 ) $                           ;Blink a warning message to the user
  ;              THEN XYOUTS, 0.5, 0.5, 'THIS WILL THE FINAL LOCATION', /NORMAL, $
  ;              ALIGNMENT = 0.5, CHARSIZE = 2, CHARTHICK = 2
  ;            WAIT, 0.4
  ;          ENDFOR
            done = 0
            WHILE (done NE 1) DO BEGIN
              TVIM, ALOG(subArray), RANGE = logRange                    ;Show the image
              CURSOR, xcen, ycen, /DATA, /DOWN                          ;Click on the star
              
              OPLOT, [xcen,xcen], [0,40], LINESTYLE = 2, THICK = 2      ;Draw cross-hairs on the estimated star position
              OPLOT, [0,40], [ycen,ycen], LINESTYLE = 2, THICK = 2
              
              POLYFILL, [0,0,1,1,0], [0,0.09,0.09,0,0], /NORMAL, COLOR=!P.BACKGROUND
              XYOUTS, 0.5, 0.06, 'Left click to accept centroid poisition', /NORMAL, ALIGNMENT = 0.5
              XYOUTS, 0.5, 0.02, 'Right click to try again', /NORMAL, ALIGNMENT = 0.5
  
              CURSOR, junk1, junk2, /DOWN
              
              IF (!MOUSE.BUTTON EQ 1) THEN done = 1
              IF (!MOUSE.BUTTON EQ 4) THEN done = 2
              
  ;            WIDGET_CONTROL, accept_wid, SENSITIVE=1                   ;Sensitize the accept button
  ;            WIDGET_CONTROL, reject_wid, SENSITIVE=1                   ;Sensitize the reject button
  ;            
  ;            decisionEvent = WIDGET_EVENT([accept_wid, reject_wid])    ;Wait for the user to accept or reject positioning
  ;            IF decisionEvent.ID EQ accept_wid THEN done = 1
            ENDWHILE
            
          END
        ENDSWITCH
         
        xStars[k] = xOff + xcen                                         ;Update the star x-position
        yStars[k] = yOff + ycen                                         ;Update the star y-position
  ;      PRINT, xcen, ycen
      ENDFOR
  
  ;    mean_FWHM   = (MEDIAN_FILTERED_MEAN(FWHMs))[0]                    ;Compute the mean FWHM
  
      ;We can finally begin to compute the image astrometry using one of the following methods
      ;(1) If there are at least 6 stars, then a least squares method can reliably compute the transformation matrix
      ;(2) If there are between 3 and 5 stars, then the transformation matrix is computer for each set of 3 stars,
      ;    and an AVERAGE matrix is used as the final astrometry
      ;(3) If there are two stars, then the plate-scale and rotation angle are explicitly computed.
      ;    These two values are then used to compute a transformation matrix, assuming a right-handed coordinate system.
      IF numUse GE 6 THEN BEGIN                                         ;Begin least squares method of astrometry
        ;**** PERFORM LEAST SQUARES ASTROMETRY ****
        astr = JM_SOLVE_ASTRO(astroStars1.RAJ2000, astroStars1.DEJ2000, $
          xStars, yStars, NAXIS1 = sz[0], NAXIS2 = sz[1])
        crpix = [511, 512]
        XY2AD, crpix[0], crpix[1], astr, crval1, crval2
        astr.crpix = (crpix + 1)                                        ;FITS convention is offset 1 pixel from IDL
        astr.crval = [crval1, crval2]                                   ;Store the updated reference pixel values
        PUTAST, header, astr, EQUINOX = 2000                            ;Update the header with the new astrometry
      ENDIF ELSE IF numUse GE 3 THEN BEGIN                              ;Begin "averaging" method of astrometry
        ;**** PERFORM 3-5 STAR ASTROMETRY ****
        numTri = numUse*(numUse-1)*(numUse-2)/6
        big_cd = DBLARR(2,2,numTri)
        triCnt = 0                                                      ;Initalize a counter for looping through triangles
        FOR iStar = 0, numUse - 1 DO BEGIN                              ;Loop through all possible triangles
          FOR jStar = iStar+1, numUse - 1 DO BEGIN
            FOR kStar = jStar+1, numUse - 1 DO BEGIN
              these_stars = [iStar,jStar,kStar]                         ;Grab the indices of the stars in this triangle
              STARAST, astroStars1[these_stars].RAJ2000, $               ;Sove astrometry using this triangle of stars
                astroStars1[these_stars].DEJ2000, $
                xStars[these_stars], yStars[these_stars], $
                this_cd, PROJECTION = 'TAN'
              big_cd[*,*,triCnt] = this_cd                              ;Store the CD matrix
;              IF triCnt EQ 2 THEN STOP
              triCnt++                                                  ;Increment the triangle counter
            ENDFOR
          ENDFOR
        ENDFOR
        cd_matrix = DBLARR(2,2)                                         ;Initalize an array for mean CD matrix
        FOR iMat = 0, 1 DO BEGIN
          FOR jMat = 0, 1 DO BEGIN
            cd_matrix[iMat,jMat] = $                                    ;Compute the mean CD matrix
              (MEDIAN_FILTERED_MEAN(REFORM(big_cd[iMat,jMat, *])))[0]
          ENDFOR
        ENDFOR
        crpix      = [511, 512]                                         ;Define the center pixels
        centerDist = SQRT((xStars - crpix[0])^2 + $                     ;Compute star distances from the image center
                          (yStars - crpix[1])^2)
        centerStar = WHERE(centerDist EQ MIN(centerDist))               ;Grab the star closest to the image center
        deltaX     = crpix[0] - xStars[centerStar]                      ;Compute pixel x-offset from center
        deltaY     = crpix[1] - yStars[centerStar]                      ;Compute pixel y-offset from center
        deltaAD    = REFORM(cd_matrix##[[deltaX],[deltaY]])             ;Compute (RA, Dec) offsets from center
        deltaAD[0] = $                                                  ;Correct RA offset for distortion
          deltaAD[0]*COS(astroStars1[centerStar].DEJ2000*!DTOR)
  ;      crval      = [astroStars1[centerStar].RAJ2000, $                ;Re-compute the center value
  ;                    astroStars1[centerStar].DEJ2000]
        MAKE_ASTR, astr, CD = cd_matrix, CRPIX = [xStars[centerStar], yStars[centerStar]], $ ;Create final astrometry structure
          CRVAL = [astroStars1[centerStar].RAJ2000, astroStars1[centerStar].DEJ2000], CTYPE = ['RA---TAN','DEC--TAN']
        XY2AD, crpix[0], crpix[1], astr, crval1, crval2                 ;Recenter astrometry structure
        MAKE_ASTR, astr, CD = cd_matrix, CRPIX = (crpix+1), $           ;Create final astrometry structure
          CRVAL = [crval1, crval2], CTYPE = ['RA---TAN','DEC--TAN']
        PUTAST, header, astr, EQUINOX = 2000                            ;Store astrometry in header
      ENDIF ELSE IF numUse EQ 2 THEN BEGIN
        ;****PERFORM 2-STAR ASTROMETRY****
        yMaxInd     = WHERE(yStars EQ MAX(yStars), COMPLEMENT=yMinInd)
        dXpix       = xStars[yMaxInd] - xStars[yMinInd]                 ;Compute dx vector
        dYpix       = yStars[yMaxInd] - yStars[yMinInd]                 ;Compute dy vector
        dRA         = (astroStars1[yMaxInd].RAJ2000 - astroStars1[yMinInd].RAJ2000)*COS(MEAN(astroStars1.DEJ2000)*!DTOR)
        dDec        = (astroStars1[yMaxInd].DEJ2000 - astroStars1[yMinInd].DEJ2000)
        deltaPix    = SQRT(dXpix^2 + dYpix^2)                           ;Compute the pixel separation
        deltaTheta  = SQRT(dRA^2 + dDec^2)                              ;Compute angular separation (deg)
        plate_scale = deltaTheta/deltaPix                               ;Compute the plate scale (deg/pix)
        rotAnglePix = ATAN(dYpix, dXpix)*!RADEG                         ;Compute rotation angle of the two stars in image coordinates
        rotAngleEQ  = 180 - ATAN(dDec,  dRA)*!RADEG                     ;Compute rotation angle of the two stars in equatorial coords.
        CDmat1      = [[-plate_scale, 0E         ], $
                       [ 0E         , plate_scale]]
        relativeRot = (rotAngleEQ - rotAnglePix)*!DTOR
        rotMatrix   = [[COS(relativeRot), -SIN(relativeRot)], $
                       [SIN(relativeRot),  COS(relativeRot)]]
        CDmat       = CDmat1##rotMatrix
        crpix       = [511, 512]                                        ;Define the center pixels
        centerDist  = SQRT((xStars - crpix[0])^2 + $                    ;Compute star distances from the image center
          (yStars - crpix[1])^2)
        centerStar  = WHERE(centerDist EQ MIN(centerDist))              ;Grab the star closest to the image center
        deltaX      = crpix[0] - xStars[centerStar]                     ;Compute pixel x-offset from center
        deltaY      = crpix[1] - yStars[centerStar]                     ;Compute pixel y-offset from center
        deltaAD     = REFORM(CDmat##[[deltaX],[deltaY]])                ;Compute (RA, Dec) offsets from center
        deltaAD[0]  = $                                                 ;Correct RA offset for distortion
          deltaAD[0]*COS(astroStars1[centerStar].DEJ2000*!DTOR)
        MAKE_ASTR, astr, CD = CDmat, CRPIX = [xStars[centerStar], yStars[centerStar]], $ ;Create final astrometry structure
          CRVAL = [astroStars1[centerStar].RAJ2000, astroStars1[centerStar].DEJ2000], CTYPE = ['RA---TAN','DEC--TAN']
        XY2AD, crpix[0], crpix[1], astr, crval1, crval2                 ;Recenter astrometry structure
        MAKE_ASTR, astr, CD = CDmat, CRPIX = (crpix+1), $               ;Create final astrometry structure
          CRVAL = [crval1, crval2], CTYPE = ['RA---TAN','DEC--TAN']
        PUTAST, header, astr, EQUINOX = 2000                       
      ENDIF ELSE BEGIN
        ;****PERFORM 1-STAR ASTROMETRY*****
        STOP
      ENDELSE
  
  
      SXADDPAR, header, 'GPCOORD', 1                                    ;Add remainind header keywords
      SXADDPAR, header, 'PP3_LLQF', 0, 'Lower Left Quad
      SXADDPAR, header, 'PPOLNSTR', numUse, 'Number of stars used in manual astrometry'
    
      ;Restore the history to the header
      n_old = N_ELEMENTS(hist)
      FOR k= 0, n_old - 1 DO BEGIN
        SXADDPAR, header, "HISTORY", hist[k]
      ENDFOR
  
      TVIM, fixedImg, RANGE = skyMode + [-3,+10]*skyNoise               ;Display full image
      OPLOT, xGuess, yGuess, PSYM=6, COLOR=red 
      XYOUTS, 0.5, 0.96, FILE_BASENAME(imgFile), /NORMAL, ALIGNMENT = 0.5
  
      AD2XY, astroStars1.RAJ2000, astroStars1.DEJ2000, astr, xAstro, yAstro
      XYOUTS, 0.5, 0.05, 'Accept or Reject', /NORMAL, ALIGNMENT = 0.5
      OPLOT, xAstro, yAstro, PSYM=4, COLOR=green
      ARROWS, header, 0.9*sz[0], 0.72*sz[1], /DATA                      ;Show the North-East compas as sanity check
      
      ;Send all the astrometry information to the screen as text
      GETROT, astr, rotAngle, cdelt
      plateScale = SQRT(ABS(cdelt[0]*cdelt[1]))*3600D
      
      RAstring = STRING(SIXTY(astr.CRVAL[0]/15.0), FORMAT='(I2,":",I2,":",F4.1)')
      WIDGET_CONTROL, RAtext_wid, SET_VALUE=RAstring                    ;Display central RA
      
      DecString = STRING(SIXTY(astr.CRVAL[1]), FORMAT = '(I+3,":",I2,":",F4.1)')
      WIDGET_CONTROL, DecText_wid, SET_VALUE=DecString                  ;Display central Dec
      
      plateScale = STRING(plateScale, FORMAT='(D9.6)')
      WIDGET_CONTROL, plateSc_wid, SET_VALUE=plateScale                 ;Display the plate scale
      
      rotAngle = STRING(rotAngle, FORMAT='(F8.4)')
      WIDGET_CONTROL, rotAng_wid, SET_VALUE=rotAngle                    ;Display the rotation angle
  
      ;Ask the user to accept or reject the computed astrometry
      WIDGET_CONTROL, save_wid,  SENSITIVE=1                            ;Sensitize the "save" button
      WIDGET_CONTROL, acceptAstro_wid, SENSITIVE=1                      ;Sensitize the accept astro button
      WIDGET_CONTROL, acceptPhoto_wid, SENSITIVE=1                      ;Sensitize the accept photo button
      WIDGET_CONTROL, reject_wid, SENSITIVE=1                           ;Sensitize the reject button
      
      decisionEvent = WIDGET_EVENT([acceptPhoto_wid, acceptAstro_wid, $ ;Wait for the user to evaluate image quality
        reject_wid, save_wid])
;      STOP
      IF decisionEvent.ID EQ acceptPhoto_wid THEN BEGIN
        filename = groupStruc.analysis_dir + 'S3_Astrometry' $          ;Set the path for the file to be saved
          + PATH_SEP() + FILE_BASENAME(groupStruc.groupImages[i,j])
;        WRITEFITS, filename, fixedImg, header                           ;Write the file to disk
        WRITEHEAD, filename, header                                     ;Write the file to disk
        groupStruc.imageFlags[i,j] = 1                   ;Update image usage flag
        groupStruc.astroFlags[i,j] = 1                   ;Update the astrometry flag
;        UPDATE_GROUP_SUMMARY, event, groupStruc                        ;Write image usage flags to disk
      ENDIF ELSE IF decisionEvent.ID EQ acceptAstro_wid THEN BEGIN
        filename = groupStruc.analysis_dir + 'S3_Astrometry' $          ;Set the path for the file to be saved
          + PATH_SEP() + FILE_BASENAME(groupStruc.groupImages[i,j])
;        WRITEFITS, filename, fixedImg, header                           ;Write the file to disk
        WRITEHEAD, filename, header                                     ;Write the file to disk
        groupStruc.imageFlags[i,j] = 1                   ;Update image usage flag
        groupStruc.astroFlags[i,j] = 2                   ;Update the astrometry flag
      ENDIF ELSE IF decisionEvent.ID EQ reject_wid THEN BEGIN
        groupStruc.imageFlags[i,j] = 0                   ;Update image usage flag
        groupStruc.astroFlags[i,j] = 3                   ;Set the astrometry flag to '2'
;        UPDATE_GROUP_SUMMARY, event, groupStruc                        ;Update the group structure
      ENDIF ELSE IF decisionEvent.ID EQ save_wid THEN BEGIN
        filename = groupStruc.analysis_dir + 'S3_Astrometry' $          ;Set the path for the file to be saved
          + PATH_SEP() + FILE_BASENAME(groupStruc.groupImages[i,j])
;        WRITEFITS, filename, fixedImg, header                           ;Write the file to disk
        WRITEHEAD, filename, header                                     ;Write the file to disk
        groupStruc.imageFlags[i,j] = 1                   ;Update image usage flag
        groupStruc.astroFlags[i,j] = 1                   ;Update the astrometry flag
        UPDATE_GROUP_SUMMARY, event, groupStruc, /SAVE                  ;Write image usage flags to disk
        
        WIDGET_CONTROL, save_wid, SENSITIVE=0                           ;Desensitize the button
        WIDGET_CONTROL, acceptAstro_wid, SENSITIVE=0                    ;Desensitize the button
        WIDGET_CONTROL, acceptPhoto_wid, SENSITIVE=0                    ;Desensitize the button
        WIDGET_CONTROL, reject_wid, SENSITIVE=0                         ;Desensitize the button
        WIDGET_CONTROL, begin_wid, SENSITIVE=1                          ;Resensitize the "Begin Astrometry" button
        
        PRINT_TEXT2, event, 'Saving progress and returnning to main program.'
        RETURN
      ENDIF
  
      WIDGET_CONTROL, save_wid, SENSITIVE=0                             ;Desensitize the button
      WIDGET_CONTROL, acceptAstro_wid, SENSITIVE=0                      ;Desensitize the button
      WIDGET_CONTROL, acceptPhoto_wid, SENSITIVE=0                      ;Desensitize the button
      WIDGET_CONTROL, reject_wid, SENSITIVE=0                           ;Desensitize the button
    ENDFOR
  ENDFOR
  WIDGET_CONTROL, save_wid, SENSITIVE=0                               ;Desensitize the button
  WIDGET_CONTROL, acceptAstro_wid ,SENSITIVE=0                        ;Desensitize the button
  WIDGET_CONTROL, acceptPhoto_wid, SENSITIVE=0                        ;Desensitize the button
  WIDGET_CONTROL, reject_wid, SENSITIVE=0                             ;Desensitize the button
  WIDGET_CONTROL, event.ID, SENSITIVE=1                               ;Resensitize the "Begin Astrometry" button
  UPDATE_GROUP_SUMMARY, event, groupStruc, /SAVE                      ;Write image usage flags to disk
  groupNumber     = STRTRIM(groupStruc.numGroups, 2)
  imageNumber     = STRTRIM(groupStruc.groupNumbers[i-1], 2)
  groupProgString = 'Group ' + groupNumber + $
    ' of ' + groupNumber
  imageProgString = 'Image ' + imageNumber + $            ;Create the message to display in the progress bar
    ' of ' + imageNumber
  UPDATE_PROGRESSBAR, groupProgWID, $
    100, DISPLAY_MESSAGE=groupProgString
  UPDATE_PROGRESSBAR, imageProgWID, $                                 ;Update the progress bar to show the latest progress
    100, DISPLAY_MESSAGE=imageProgString
  PRINT_TEXT2, event, 'Finished all astrometry!'
END

PRO S3A_CHECK_ASTROMETRY, event
  
  red   = RGB_TO_DECOMPOSED([255,0,0])
  green = RGB_TO_DECOMPOSED([0,255,0])
  displayWID      = WIDGET_INFO(event.top, FIND_BY_UNAME='IMAGE_DISPLAY_WINDOW')
  imageProgWID    = WIDGET_INFO(event.top, FIND_BY_UNAME='IMAGE_PROGRESS_BAR')
  groupProgWID    = WIDGET_INFO(event.top, FIND_BY_UNAME='GROUP_PROGRESS_BAR')
  RAtext_wid      = WIDGET_INFO(event.top, FIND_BY_UNAME='S3A_RA_TEXT')
  DecText_wid     = WIDGET_INFO(event.top, FIND_BY_UNAME='S3A_DEC_TEXT')
  plateSc_wid     = WIDGET_INFO(event.top, FIND_BY_UNAME='S3A_PLATE_SCALE')
  rotAng_wid      = WIDGET_INFO(event.top, FIND_BY_UNAME='S3A_ROT_ANGLE')
  goodAstroWID    = WIDGET_INFO(event.top, FIND_BY_UNAME='S3A_CHECK_GOOD')
  badAstroWID     = WIDGET_INFO(event.top, FIND_BY_UNAME='S3A_CHECK_BAD')
  
;  WIDGET_CONTROL, event.ID, SENSITIVE=0                              ;Desensitize the "Begin Astrometry" button
  WIDGET_CONTROL, event.top, GET_UVALUE=groupStruc                    ;Retrieve the group summary structure
  WIDGET_CONTROL, displayWID, GET_VALUE=windowIndex
  WSET, windowIndex                                                   ;Prepare to plot in display window

  PRINT_TEXT2, event, 'Retrieving information for stars in field'
  ;Grab all the good stars within 12-arcmin of the median pointing.
  ;Retrieve the 2MASS star info
  this_mirror = ['UK', 'CDS', 'CA', 'Jp']
  ;
  nloop = 0L
  WHILE nloop LT 15 DO BEGIN                                       ; 5 minutes day at 20 sec per try
    ;
    vizier_flag = 0
    FOR imirror = 0, N_ELEMENTS(this_mirror) - 1 DO BEGIN
      IF(vizier_flag EQ 0) THEN BEGIN
        starInfo = mp_QueryVizier('2MASS-PSC', [groupStruc.medianRA,groupStruc.medianDec], [16,16], $
          MIRROR=this_mirror[imirror])
        test = size(starInfo)
        IF test[0] NE 0 THEN BEGIN
          vizier_flag = 1
          goodMirror  = imirror
        ENDIF
      ENDIF
    ENDFOR
    
    IF(vizier_flag EQ 0) THEN BEGIN
      info = 99   ;No Vizier servers available
      ;
      ; if no Vizier servers, wait 20s and retry
      ;
      PRINT, 'No Vizier Servers at ',SYSTIME(),' waiting 20s and retrying'
      WAIT, 20
      nloop++
    ENDIF ELSE nloop = 15                                ;Force the loop to close
  ENDWHILE
  PRINT_TEXT2, event, 'Succesful Vizier Query from ' + this_mirror[goodMirror]

  numGoodGroups = TOTAL(groupStruc.groupFlags)
  groupCount    = 0
  FOR i = 0, groupStruc.numGroups - 1 DO BEGIN
    IF groupStruc.groupFlags[i] EQ 0 THEN CONTINUE
    groupCount++
    groupProgString = STRING(groupCount, numGoodGroups, FORMAT='("Group ",I2," of ",I2)')
    UPDATE_PROGRESSBAR, groupProgWID, 100*FLOAT(i+1)/numGoodGroups, DISPLAY_MESSAGE=groupProgString
    FOR j = 0, groupStruc.groupNumbers[i] - 1 DO BEGIN
      imageProgString = STRING((j+1), groupStruc.groupNumbers[i], FORMAT='("Image ",I2," of ",I2)')
      UPDATE_PROGRESSBAR, imageProgWID, 100*FLOAT(j+1)/groupStruc.groupNumbers[i], DISPLAY_MESSAGE=imageProgString

      filename = groupStruc.analysis_dir + 'S3_Astrometry' $          ;Set the path for the file to be saved
        + PATH_SEP() + FILE_BASENAME(groupStruc.groupImages[i,j])
      
      fileExists = (FILE_INFO(filename)).exists
      fileFlag   = groupStruc.astroFlags[i,j]
      IF (~fileExists) AND (fileFlag EQ 3) THEN CONTINUE
      IF (~fileExists) AND (fileFlag NE 3) THEN STOP
      
      img = READFITS(filename, header, /SILENT)
      sz  = SIZE(img, /DIMENSIONS)
      
      G_SKY, img, skymode, skynoise, mfm_mean, mfm_sig, /SILENT
      TVIM, img, RANGE = skymode + [-3,+10]*skynoise
      XYOUTS, 0.5, 0.96, FILE_BASENAME(groupStruc.groupImages[i,j]), /NORMAL, ALIGNMENT = 0.5
      
      EXTAST, header, astr
      AD2XY, starInfo.RAJ2000, starInfo.DEJ2000, astr, $
        xStars, yStars
      keepStars = WHERE((xStars GT 0) AND $
                        (xStars LT (sz[0]-1)) AND $
                        (yStars GT 0) AND $
                        (yStars LT (sz[1]-1)), numStars)
      IF numStars GT 0 THEN BEGIN
        xStars = xStars[keepStars]
        yStars = yStars[keepStars]
        OPLOT, xStars, yStars, PSYM = 6, SYMSIZE = 0.5, COLOR = green
        ARROWS, header, 0.9*sz[0], 0.72*sz[1], /DATA                      ;Show the North-East compas as sanity check
      ENDIF ELSE BEGIN
        PRINT, 'This image sucks...'
        STOP
      ENDELSE
      
      ;Send all the astrometry information to the screen as text
      GETROT, astr, rotAngle, cdelt
      plateScale = SQRT(ABS(cdelt[0]*cdelt[1]))*3600D
      
      RAstring = STRING(SIXTY(astr.CRVAL[0]/15.0), FORMAT='(I2,":",I2,":",F4.1)')
      WIDGET_CONTROL, RAtext_wid, SET_VALUE=RAstring                    ;Display central RA
      
      DecString = STRING(SIXTY(astr.CRVAL[1]), FORMAT = '(I+3,":",I2,":",F4.1)')
      WIDGET_CONTROL, DecText_wid, SET_VALUE=DecString                  ;Display central Dec
      
      plateScale = STRING(plateScale, FORMAT='(D9.6)')
      WIDGET_CONTROL, plateSc_wid, SET_VALUE=plateScale                 ;Display the plate scale
      
      rotAngle = STRING(rotAngle, FORMAT='(F8.4)')
      WIDGET_CONTROL, rotAng_wid, SET_VALUE=rotAngle                    ;Display the rotation angle
      
      decisionEvent = WIDGET_EVENT([goodAstroWID, badAstroWID])         ;Querry the user on astrometry quality
      IF decisionEvent.ID EQ badAstroWID THEN BEGIN
        PRINT_TEXT2, event, 'Foudn file ' + FILE_BASENAME(groupStruc.groupImages[i,j]) + ' with bad astrometry'
        groupStruc.astroFlags[i,j] = 0                                  ;Reset the astrometry flag for recomputation...
      ENDIF
    ENDFOR
  ENDFOR
  
  UPDATE_GROUP_SUMMARY, event, groupStruc, /SAVE                        ;Write image usage flags to disk
  
END