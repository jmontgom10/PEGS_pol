PRO S1_AVERAGE_STOKES_IMAGES, event
  ; Collect information into a structure
  ; Grab the input directory from the top-level base UVALUE
  tlb_wid          = WIDGET_INFO(event.top, FIND_BY_UNAME='WID_BASE')
  groupProgBarWID  = WIDGET_INFO(event.top, FIND_BY_UNAME='GROUP_PROGRESS_BAR')
  imageProgBarWID  = WIDGET_INFO(event.top, FIND_BY_UNAME='IMAGE_PROGRESS_BAR')
  displayWindowWID = WIDGET_INFO(event.top, FIND_BY_UNAME='IMAGE_DISPLAY_WINDOW')
  WIDGET_CONTROL, tlb_wid, GET_UVALUE=groupStruc
  WIDGET_CONTROL, displayWindowWID, GET_VALUE=displayWindowIndex

  ; Grab the object name from the group structure
  object_name = groupStruc.objectName
 
  ; Begin by simplifying references to the input/output directories, etc...
  inDir   = groupStruc.analysis_dir + 'S11_Full_Field_Polarimetry' + PATH_SEP()
  outDir  = groupStruc.analysis_dir + 'S11B_Combined_Images' + PATH_SEP()
  IF ~FILE_TEST(outDir, /DIRECTORY) THEN FILE_MKDIR, outDir
  stokesPars  = ['I','U','Q']

  FOR i = 0, 2 DO BEGIN
    stokes = stokesPars[i]
    UPDATE_PROGRESSBAR, groupProgBarWID, /ERASE
    UPDATE_PROGRESSBAR, groupProgBarWID, (i+1)*(100E/3E), DISPLAY_MESSAGE = 'Stokes ' + stokesPars[i]

    IF (stokes EQ 'U') OR (stokes EQ 'Q') THEN BEGIN
      valueName    = "*_" + stokes + "_cor.fits"                      ;stokes value name
      sigmaName    = "*s" + stokes + "_cor.fits"                      ;stokes uncertainty name
      valueFiles   = FILE_SEARCH(inDir + valueName, COUNT = nValFiles);List of stokes value files
      sigmaFiles   = FILE_SEARCH(inDir + sigmaName, COUNT = nSigFiles);List of stokes uncertainty files
      sigma        = 1                                                ;flag for inverse variance weighting
    ENDIF ELSE BEGIN
      valueName  = "*Intensity.fits"
      valueFiles = FILE_SEARCH(inDir + valueName, COUNT = nValFiles)  ;List of intensity image files
      sigma      = 0                                                  ;Un-flag inverse variance weighting
    ENDELSE

    PRINT_TEXT2, event, "Generating combined header"
    
    ;****USE LOOP TO GO THROUGH ALL STOKES VARIABLES AND GENERATE A !!SINGLE!! HEADER****
    CREATE_COMBINED_HEADER, valueFiles, inDir, object_name, all_astr, combined_header
    
    ; Grab the astrometry from the newly created header
    EXTAST, combined_header, new_astr,  noparams
    new_naxis1 = new_astr.naxis[0]                                    ;store the new image size (add some padding... figure this out later)
    new_naxis2 = new_astr.naxis[1]
    new_crpix1 = new_astr.crpix[0]                                    ;store the new reference value
    new_crpix2 = new_astr.crpix[1]

    IF N_ELEMENTS(old_naxis1) GT 0 THEN BEGIN
      IF (new_naxis1 NE old_naxis1) OR (new_naxis2 NE old_naxis2) THEN BEGIN
        PRINT_TEXT2, event, 'These images do not have matching sizes. FIX THE CODE!'
        RETURN
      ENDIF
    ENDIF
    
    ;Create an image into which the combined data will be fed
    n_files   = N_ELEMENTS(valueFiles)                                ;Count the files
    stack_img = FLTARR(new_naxis1, new_naxis2, n_files)               ;store the images in massive arrays
    stack_sig = FLTARR(new_naxis1, new_naxis2, n_files)               ;store the uncertainties in massive arrays
    
    ;Find the x_offsets and y_offsets from the center image
    AD2XY, all_astr.crval[0], all_astr.crval[1], new_astr, mapX, mapY
;    x_offsets = ROUND(mapX - new_crpix1)
;    y_offsets = ROUND(mapY - new_crpix2)
;    

    

;    PRINT_TEXT2, event, "Stacking input images into 3D array at " + STRMID(SYSTIME(), 11, 8)
;    PRINT_TEXT2, event, "... Please be patient."
;    WAIT, 0.1
    
    ;Stack the images into a 3D array
    FOR j = 0, n_files - 1 DO BEGIN
      ;Read in the input image
      in_img  = READFITS(valueFiles[j], tmp_header, /SILENT)
      
      ;Find the boundaries to which the input image maps
;      lf_in = ROUND(new_crpix1 + x_offsets[j] - 0.5*all_astr[j].naxis[0])
;      rt_in = ROUND(new_crpix1 + x_offsets[j] + 0.5*all_astr[j].naxis[0])
;      bt_in = ROUND(new_crpix2 + y_offsets[j] - 0.5*all_astr[j].naxis[1])
;      tp_in = ROUND(new_crpix2 + y_offsets[j] + 0.5*all_astr[j].naxis[1])
      lf_in = ROUND(mapX[j] - (all_astr[j].crpix[0])) + 1
      rt_in = lf_in + all_astr[j].naxis[0]
      bt_in = ROUND(mapY[j] - (all_astr[j].crpix[1])) + 1
      tp_in = bt_in + all_astr[j].naxis[1]
      
      ;   PRINT, SIZE(stack_img, /dim)
      ;   print, lf_in, rt_in, bt_in, tp_in
      ;Fill in one layer of the stack
      stack_img[lf_in:rt_in - 1, bt_in:tp_in - 1, j] = in_img
      IF KEYWORD_SET(sigma) THEN BEGIN
        sig_im  = READFITS(sigmaFiles[j], /SILENT)
        stack_sig[lf_in:rt_in - 1, bt_in:tp_in - 1, j] = sig_im
      ENDIF
    ENDFOR
    
    PRINT_TEXT2, event, "Averaging Stokes " +  stokes + " started at " + STRMID(SYSTIME(), 11, 8)
    
    out_img     = FLTARR(new_naxis1, new_naxis2)
    out_sig     = FLTARR(new_naxis1, new_naxis2)
    FOR k = 0, new_naxis1 - 1 DO BEGIN
      FOR l = 0, new_naxis2 - 1 DO BEGIN
        IF KEYWORD_SET(sigma) THEN BEGIN
          ;Preliminary filter out +/- 1E6 values and zero values
          good_data = WHERE(ABS(stack_img[k,l,*]) NE 1E6 $
            AND stack_img[k,l,*] NE 0 $
            AND ABS(stack_sig[k,l,*]) NE 1E6 $
            AND stack_sig[k,l,*] NE 0 , count)
          IF count GT 1 THEN BEGIN
            ;Filter out > 3-sigma deviation values
            merit_values = MEDIAN_FILTERED_MEAN(REFORM(stack_img[k,l,good_data]))
            good_data    = good_data[WHERE((ABS(stack_img[k,l,good_data] - merit_values[0]) LT 3*merit_values[1]), count)]
            IF count GT 0 THEN BEGIN
              ;Finally weight an average and create a normalization map
              out_pix = TOTAL(stack_img[k,l,good_data]/(stack_sig[k,l,good_data])^2)
              sig_pix = TOTAL(1/(stack_sig[k,l,good_data])^2)
              ;Test if this is an acceptable value to inclue in the final data
              IF FINITE(out_pix) AND (out_pix NE 0) AND FINITE(sig_pix) THEN BEGIN
                out_img[k,l] = TOTAL(stack_img[k,l,good_data]/(stack_sig[k,l,good_data])^2)
                out_sig[k,l] = TOTAL(1/(stack_sig[k,l,good_data])^2)
              ENDIF
            ENDIF
          ENDIF
        ENDIF ELSE BEGIN
          ;Preliminary filter out +/- 1E6 values and zero values
          good_data = WHERE((ABS(stack_img[k,l,*]) NE 1E6) AND (stack_img[k,l,*] NE 0), count)
          IF count GT 1 THEN BEGIN
            ;Average the good data
            zz           = MEDIAN_FILTERED_MEAN(REFORM(stack_img[k,l,good_data]))
            out_img[k,l] = zz[0]
            out_sig[k,l] = zz[1]
          ENDIF
        ENDELSE
        IF ~FINITE(out_img[k,l]) OR ~FINITE(out_sig[k,l]) THEN STOP
      ENDFOR
      
      updatePercent = 100E*(k+1)/new_naxis1                           ;Update the progress bar
      UPDATE_PROGRESSBAR, imageProgBarWID, updatePercent, /PERCENTAGE
      WAIT, 0.01                                                       ;Wait for the display to update
      
      old_naxis1 = new_naxis1                                         ;Store the naxis values to check with the next image
      old_naxis2 = new_naxis2
    ENDFOR
    
    UPDATE_PROGRESSBAR, imageProgBarWID, 100E, /PERCENTAGE
    WAIT, 0.1

    PRINT_TEXT2, event, "Averaging Stokes " +  stokes + " finished at " + STRMID(SYSTIME(), 11, 8)
    
    ;Fill in empty data data in the sigma map and output images
    IF KEYWORD_SET(sigma) THEN BEGIN
      zero_in = ARRAY_INDICES(out_sig, WHERE(out_sig EQ 0))
      out_sig[zero_in[0,*],zero_in[1,*]] = -1E6
      out_img = out_img/out_sig
      out_img[zero_in[0,*],zero_in[1,*]] = -1E6
    ENDIF
    
    
    ;Write the final output images
    band         = STRTRIM(SXPAR(combined_header, 'BAND'), 2)
;    stokes       = STRTRIM(SXPAR(combined_header, 'STOKES'), 2)
    outValuePath = outDir + band + "band_"  + stokes + ".fits"        ;path for writing weighted average
    outSigmaPath = outDir + band + "band_s" + stokes + ".fits"        ;path for writing weighted uncertainty

    WRITEFITS, outValuePath, out_img, combined_header
    IF KEYWORD_SET(sigma) THEN WRITEFITS, outSigmaPath, SQRT(1/out_sig), combined_header
    IF ~KEYWORD_SET(sigma) THEN BEGIN
      WSET, displayWindowIndex
      SKY, out_img, skyMode, skySig
      TVIM, out_img, RANGE = skyMode + [-2,+10*SQRT(groupStruc.numGroups)]*skySig
    ENDIF
  ENDFOR

END


PRO S1_FINAL_ASTROMETRY, event

  tlb_wid          = WIDGET_INFO(event.top, FIND_BY_UNAME='WID_BASE')  ;Retrieve the TLB widget ID
  groupProgBarWID  = WIDGET_INFO(event.top, FIND_BY_UNAME='GROUP_PROGRESS_BAR')
  imageProgBarWID  = WIDGET_INFO(event.top, FIND_BY_UNAME='IMAGE_PROGRESS_BAR')
  displayWindowWID = WIDGET_INFO(event.top, FIND_BY_UNAME='IMAGE_DISPLAY_WINDOW')
  WIDGET_CONTROL, tlb_wid, GET_UVALUE=groupStruc
  WIDGET_CONTROL, displayWindowWID, GET_VALUE=displayWindowIndex
  
  inDir          = groupStruc.analysis_dir + 'S11B_Combined_Images' + PATH_SEP()
  intensity_file = inDir + '*I.fits'                                  ;Store search string for the intensity file
  intensity_file = FILE_SEARCH(intensity_file, COUNT=numFiles)        ;Explicitly search for the correct *.fits file
  numStars       = N_ELEMENTS(groupStruc.starInfo)                    ;Count the maximum number of possible astrometry stars
  
  UPDATE_PROGRESSBAR, groupProgBarWID, /ERASE                         ;Clear out any previous progress bar status
  UPDATE_PROGRESSBAR, imageProgBarWID, /ERASE
  
  astroImage = READFITS(intensity_file, astroHeader)                  ;Read in the image
  sz         = SIZE(astroImage, /DIMENSIONS)                          ;Get the image dimensions
  hist       = SXPAR(astroHeader, "HISTORY")                          ;Get the history info
  SXDELPAR, astroHeader,'HISTORY'                                     ;delete any previous history entries
  EXTAST, astroHeader, astr                                           ;Extract the initial astrometry
  AD2XY, groupStruc.starInfo.RAJ2000, groupStruc.starInfo.DEJ2000, $  ;Solve for initial guesses on star positions
    astr, xGuess, yGuess
    
  useStar = (xGuess GT 30) AND (xGuess LT (sz[0] - 31)) $           ;Only use stars more than 30 pixels from image edge
    AND (yGuess GT 30) AND (yGuess LT (sz[1] - 31))
  useInds = WHERE(useStar, numUse)                                  ;Get the indices of the usable stars
  
  IF numUse GT 0 THEN BEGIN
    astroStars = groupStruc.starInfo[useInds]                       ;Cull the 2MASS data
    xGuess     = xGuess[useInds]                                    ;Cull the list to only the on-image stars
    yGuess     = yGuess[useInds]
  ENDIF
  
  xStars    = xGuess                                                ;Alias the x-star positions for refinement
  yStars    = yGuess                                                ;Alias the y-star positions for refinement
  useStar   = BYTARR(numUse)                                        ;Reset the "useStar" to track which stars were well fit
  FWHMs     = FLTARR(numUse)                                        ;Initalize an array for storing star FWHMs
  failedFit = 0                                                     ;Set a counter for the number of failed Gaussian star fits
  FOR j = 0, numUse - 1 DO BEGIN
    ;Cut out a subarray for a more precise positioning
    xOff     = (xGuess[j] - 19) > 0
    xRt      = (xOff  + 40) < (sz[0] - 1)
    yOff     = (yGuess[j] - 19) > 0
    yTop     = (yOff + 40)  < (sz[1] - 1)
    subArray = astroImage[xOff:xRt, yOff:yTop]
    
    result   = GAUSS2DFIT(subArray, A, /TILT)                       ;Gaussian fit the star
    inArray  = (A[4] GT 5) AND (A[4] LT 34) $                       ;If the fit is located in the center of the array
      AND (A[5] GT 5) AND (A[5] LT 34)
    okShape  = (A[2] GT 0.8) AND (A[2] LT 5) $                      ;and if its gaussian width is reasonable (not a hot pixel)
      AND (A[3] GT 0.8) AND (A[3] LT 5)
      
    methodDifference = 0                                            ;Reset the method difference variable
    IF inArray AND okShape THEN BEGIN
      FWHMs[j] = SQRT(ABS(A[2]*A[3]))*2.355                         ;Compute the FWHM for this star
      GCNTRD, subArray, A[4], A[5], xcen, ycen,  FWHMs[j]           ;Centroid this star (using estimated FWHM)
      methodDifference = SQRT((xCen - A[4])^2 + (yCen - A[5])^2)    ;Compute difference between the two locating methods
      IF (methodDifference LE 1) $                                  ;If the two methods have a consensus,
        AND FINITE(methodDifference) THEN BEGIN                     ;then update the star positions
        xStars[j]  = xOff + xcen
        yStars[j]  = yOff + ycen
        useStar[j] = 1                                              ;Mark this star as one of the stars to use
        failedFit  = 0                                              ;If the fit was successful, then reset the failed fit counter
        ;          TVIM, subarray
        ;          OPLOT, [xcen], [ycen], PSYM=6, color=255L
        ;          stop
      ENDIF
    ENDIF
    IF ~inArray OR ~ okShape $                                      ;If any one of the tests failed,
      OR (methodDifference GT 1) OR ~FINITE(methodDifference) $     ;then increment the failedFit counter
      THEN failedFit++
    IF failedFit GE 2 THEN BREAK                                    ;If the "failed fit"
  ENDFOR
  
  useInds = WHERE(useStar, numUse)                                  ;Determine which stars were well fit
  IF numUse GT 0 THEN BEGIN
    astroStars = astroStars[useInds]                                ;Cull the 2MASS data
    xStars     = xStars[useInds]                                    ;Cull the list to only the well fit stars
    yStars     = yStars[useInds]
  ENDIF
  
  printString = STRING(numUse, FORMAT='("Successfully located ",I2," stars")')
  PRINT_TEXT2, event, printString
  
  ;Now that the star positions are known, update the astrometry
  IF numUse GE 6 THEN BEGIN                                       ;Begin least squares method of astrometry
    ;**** PERFORM LEAST SQUARES ASTROMETRY ****
    astr = JM_SOLVE_ASTRO(astroStars.RAJ2000, astroStars.DEJ2000, $
      xStars, yStars, NAXIS1 = sz[0], NAXIS2 = sz[1])
    crpix = [511, 512]
    XY2AD, crpix[0], crpix[1], astr, crval1, crval2
    astr.crpix = (crpix + 1)                                      ;FITS convention is offset 1 pixel from IDL
    astr.crval = [crval1, crval2]                                 ;Store the updated reference pixel values
    PUTAST, astroHeader, astr, EQUINOX = 2000                     ;Update the header with the new astrometry
  ENDIF ELSE IF numUse GE 3 THEN BEGIN                            ;Begin "averaging" method of astrometry
    ;**** PERFORM 3-5 STAR ASTROMETRY ****
    numTri = numUse*(numUse-1)*(numUse-2)/6
    big_cd = DBLARR(2,2,numTri)
    triCnt = 0                                                    ;Initalize a counter for looping through triangles
    FOR iStar = 0, numUse - 1 DO BEGIN                            ;Loop through all possible triangles
      FOR jStar = iStar+1, numUse - 1 DO BEGIN
        FOR kStar = jStar+1, numUse - 1 DO BEGIN
          these_stars = [iStar,jStar,kStar]                       ;Grab the indices of the stars in this triangle
          STARAST, astroStars[these_stars].RAJ2000, $             ;Sove astrometry using this triangle of stars
            astroStars[these_stars].DEJ2000, $
            xStars[these_stars], yStars[these_stars], $
            this_cd, PROJECTION = 'TAN'
          big_cd[*,*,triCnt] = this_cd                            ;Store the CD matrix
          triCnt++                                                ;Increment the triangle counter
        ENDFOR
      ENDFOR
    ENDFOR
    cd_matrix = DBLARR(2,2)                                       ;Initalize an array for mean CD matrix
    FOR iMat = 0, 1 DO BEGIN
      FOR jMat = 0, 1 DO BEGIN
        cd_matrix[iMat,jMat] = $                                  ;Compute the mean CD matrix
          (MEDIAN_FILTERED_MEAN(REFORM(big_cd[iMat,jMat, *])))[0]
      ENDFOR
    ENDFOR
    crpix      = [511, 512]                                       ;Define the center pixels
    centerDist = SQRT((xStars - crpix[0])^2 + $                   ;Compute star distances from the image center
      (yStars - crpix[1])^2)
    centerStar = WHERE(centerDist EQ MIN(centerDist))             ;Grab the star closest to the image center
    deltaX     = crpix[0] - xStars[centerStar]                    ;Compute pixel x-offset from center
    deltaY     = crpix[1] - yStars[centerStar]                    ;Compute pixel y-offset from center
    deltaAD    = REFORM(cd_matrix##[[deltaX],[deltaY]])           ;Compute (RA, Dec) offsets from center
    deltaAD[0] = $                                                ;Correct RA offset for distortion
      deltaAD[0]*COS(astroStars[centerStar].DEJ2000*!DTOR)
    ;      crval      = [astroStars[centerStar].RAJ2000, $        ;Re-compute the center value
    ;                    astroStars[centerStar].DEJ2000]
    MAKE_ASTR, astr, CD = cd_matrix, CRPIX = [xStars[centerStar], yStars[centerStar]], $ ;Create final astrometry structure
      CRVAL = [astroStars[centerStar].RAJ2000, astroStars[centerStar].DEJ2000], CTYPE = ['RA---TAN','DEC--TAN']
    XY2AD, crpix[0], crpix[1], astr, crval1, crval2               ;Recenter astrometry structure
    MAKE_ASTR, astr, CD = cd_matrix, CRPIX = (crpix+1), $         ;Create final astrometry structure
      CRVAL = [crval1, crval2], CTYPE = ['RA---TAN','DEC--TAN']
    PUTAST, astroHeader, astr, EQUINOX = 2000                     ;Store astrometry in header
  ENDIF ELSE IF numUse EQ 2 THEN BEGIN
    ;****PERFORM 2-STAR ASTROMETRY****
    yMaxInd     = WHERE(yStars EQ MAX(yStars), COMPLEMENT=yMinInd)
    dXpix       = xStars[yMaxInd] - xStars[yMinInd]               ;Compute dx vector
    dYpix       = yStars[yMaxInd] - yStars[yMinInd]               ;Compute dy vector
    dRA         = (astroStars[yMaxInd].RAJ2000 - astroStars[yMinInd].RAJ2000)*COS(MEAN(astroStars.DEJ2000)*!DTOR)
    dDec        = (astroStars[yMaxInd].DEJ2000 - astroStars[yMinInd].DEJ2000)
    deltaPix    = SQRT(dXpix^2 + dYpix^2)                         ;Compute the pixel separation
    deltaTheta  = SQRT(dRA^2 + dDec^2)                            ;Compute angular separation (deg)
    plate_scale = deltaTheta/deltaPix                             ;Compute the plate scale (deg/pix)
    rotAnglePix = ATAN(dYpix, dXpix)*!RADEG                       ;Compute rotation angle of the two stars in image coordinates
    rotAngleEQ  = 180 - ATAN(dDec,  dRA)*!RADEG                   ;Compute rotation angle of the two stars in equatorial coords.
    CDmat1      = [[-plate_scale, 0E         ], $
      [ 0E         , plate_scale]]
    relativeRot = (rotAngleEQ - rotAnglePix)*!DTOR
    rotMatrix   = [[COS(relativeRot), -SIN(relativeRot)], $
      [SIN(relativeRot),  COS(relativeRot)]]
    CDmat       = CDmat1##rotMatrix
    crpix       = [511, 512]                                      ;Define the center pixels
    centerDist  = SQRT((xStars - crpix[0])^2 + $                  ;Compute star distances from the image center
      (yStars - crpix[1])^2)
    centerStar  = WHERE(centerDist EQ MIN(centerDist))            ;Grab the star closest to the image center
    deltaX      = crpix[0] - xStars[centerStar]                   ;Compute pixel x-offset from center
    deltaY      = crpix[1] - yStars[centerStar]                   ;Compute pixel y-offset from center
    deltaAD     = REFORM(CDmat##[[deltaX],[deltaY]])              ;Compute (RA, Dec) offsets from center
    deltaAD[0]  = $                                               ;Correct RA offset for distortion
      deltaAD[0]*COS(astroStars[centerStar].DEJ2000*!DTOR)
    MAKE_ASTR, astr, CD = CDmat, CRPIX = [xStars[centerStar], yStars[centerStar]], $ ;Create final astrometry structure
      CRVAL = [astroStars[centerStar].RAJ2000, astroStars[centerStar].DEJ2000], CTYPE = ['RA---TAN','DEC--TAN']
    XY2AD, crpix[0], crpix[1], astr, crval1, crval2               ;Recenter astrometry structure
    MAKE_ASTR, astr, CD = CDmat, CRPIX = (crpix+1), $             ;Create final astrometry structure
      CRVAL = [crval1, crval2], CTYPE = ['RA---TAN','DEC--TAN']
    PUTAST, astroHeader, astr, EQUINOX = 2000
  ENDIF ELSE BEGIN
    ;****PERFORM 1-STAR ASTROMETRY*****
  ENDELSE

  ;Restore the history to the header
  n_old = N_ELEMENTS(hist)
  FOR j= 0, n_old - 1 DO BEGIN
    SXADDPAR, astroHeader, "HISTORY", hist[j]
  ENDFOR
  ;**********************************************************************************************  
  ;**********************ALSO UPDATE U AND Q IMAGES WITH THE SAME ASTROMETRY ********************
  ;**********************************************************************************************
  
  
  ;Compute plate scale and rotation angle
;  CD_det     = astr.cd[0,0]*astr.cd[1,1] - astr.cd[0,1]*astr.cd[1,0]  ;Compute the CD matrix determinant
;  IF CD_det LT 0 THEN sgn = -1 ELSE sgn = 1
;  plateScale = SQRT(ABS(astr.cd[0,0]*astr.cd[1,1]) $                  ;Compute the mean plate scale
;                  + ABS(astr.cd[0,1]*astr.cd[1,0]))
;  rot1       = ATAN(  sgn*astr.cd[0,1],  sgn*astr.cd[0,0] )           ;Compute the rotation angle of the x-axis
;  rot2       = ATAN( -astr.cd[1,0],  astr.cd[1,1] )                   ;Compute the rotating angle of the y-axis
;  rotAngle   = SQRT(ABS(rot1*rot2))*!RADEG                            ;Compute a geometric mean of the rotation angles

  GETROT, astr, rotAngle, cdelt
  plateScale = SQRT(ABS(cdelt[0]*cdelt[1]))*3600D
;  groupStruc.finalPlateScale = plateScale                             ;Update the group structure to include the final plate scale
;  UPDATE_GROUP_SUMMARY, event, groupStruc
  UPDATE_GROUP_SUMMARY, event, groupStruc, 'finalPlateScale', plateScale ;Update the group structure to include the final plate scale
  

  centRA_WID = WIDGET_INFO(event.top, $                               ;Retrieve the center RA text ID
    FIND_BY_UNAME='RA_TEXT')
  RAstring = STRING(SIXTY(astr.CRVAL[0]/15.0), FORMAT='(I2,":",I2,":",F4.1)')
  WIDGET_CONTROL, centRA_WID, SET_VALUE=RAstring                     ;Display central RA
  
  centDec_WID = WIDGET_INFO(event.top, $                              ;Retrieve the center Dec text ID
    FIND_BY_UNAME='DEC_TEXT')
  DecString = STRING(SIXTY(astr.CRVAL[1]), FORMAT = '(I+3,":",I2,":",F4.1)')
  WIDGET_CONTROL, centDec_WID, SET_VALUE=DecString                    ;Display central Dec
  
  pl_sc_WID = WIDGET_INFO(event.top, $                                ;Retrieve the plate scale text ID
    FIND_BY_UNAME='PLATE_SCALE')
  plateScale = STRING(plateScale, FORMAT='(D9.6)')
  WIDGET_CONTROL, pl_sc_WID, SET_VALUE=plateScale                     ;Display the plate scale
  
  rotAngleWID = WIDGET_INFO(event.top, $                              ;Retrieve the rotation angle text ID
    FIND_BY_UNAME='ROT_ANGLE')

  rotAngle = STRING(rotAngle, FORMAT='(F8.4)')
  WIDGET_CONTROL, rotAngleWID, SET_VALUE=rotAngle                     ;Display the rotation angle

  
  AD2XY, groupStruc.starInfo.RAJ2000, groupStruc.starInfo.DEJ2000, $
    astr, xStars, yStars
  
  WSET, displayWindowIndex
;  TVIM, astroImage
  OPLOT, xStars, yStars, PSYM=6, COLOR=RGB_TO_DECOMPOSED([0,255,0])   ;Overplot the inferred star locations
  ARROWS, astroHeader, 0.9, 0.75, /NORMAL                             ;Show the North-East compas as sanity check  
  WRITEFITS, intensity_file, astroImage, astroHeader                  ;Write the file to disk
  PRINT_TEXT2, event, 'Finished computing astrometry'
    
END