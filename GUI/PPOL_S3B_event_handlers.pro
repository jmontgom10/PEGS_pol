PRO SUPERSKY_GROUP, event, imageFiles, astrometryFiles, RA_cen, DEC_cen, $
  GROUP_NUMBER=group_number
  ;
  ; DPC 20140527
  ; JDM 20150824  Implemented vectorized IDL8 functionality (not backwards compatible)
  ; 
  ; creates a supersky from a set of dithered images
  ;
  ; saves the supersky
  ;
  ; then, divides supersky into each input image and saves result
  ;  
  imageProgressBarWID = WIDGET_INFO(event.top, FIND_BY_UNAME='IMAGE_PROGRESS_BAR')
  displayWID          = WIDGET_INFO(event.top, FIND_BY_UNAME='IMAGE_DISPLAY_WINDOW')
  WIDGET_CONTROL, displayWID, GET_VALUE=windowIndex         ;Retrieve display window WID
  WSET, windowIndex                                         ;Set plot window WID
  
  WIDGET_CONTROL, event.top, GET_UVALUE=groupStruc          ;Retrieve the groupStruc info structure
  skynoise = SXPAR(groupStruc.displayHeader, 'SIGMA')       ;Retrieve the skynoise from the structure
  
  ;Test that there are an identical number of image and astrometry files for this group
  IF N_ELEMENTS(imageFiles) NE N_ELEMENTS(astrometryFiles) THEN STOP
  nfiles = N_ELEMENTS(imageFiles)
  ;
  ; store the default Mimir image size
  ;
  nx = 1024
  ny = 1026
  ;
  ; read the images to group by HWP first
  ;
  hwp = FLTARR(nfiles)
  FOR i = 0, nfiles-1 do begin
    header = READHEAD(astrometryFiles[i])
    hwp[i] = SXPAR(header, "MB_HWP")
  ENDFOR
  ;
  ; how many images of each HWP angle?
  ;
  ind = SORT(hwp)
  hwp_uniq = hwp(ind[uniq(hwp[ind])])
  n_hwp = N_ELEMENTS(hwp_uniq)
  PRINT, "Found ",n_hwp," different HWP angles: ",hwp_uniq
  ;
  ; for each unique HWP angle, collect the dithered images to form superskies
  ;
  FOR ihwp = 0, n_hwp-1 DO BEGIN
    this_hwp = hwp_uniq[ihwp]
    PRINT, "For HWP angle = ",this_hwp
    ;
    ind = WHERE(HWP eq this_hwp, count)
    IF(count GT 0) THEN BEGIN
      PRINT, "  Found ", count, " images with this HWP"
      ;
      hwp_imageFiles = imageFiles[ind]
      hwp_astroFiles = astrometryFiles[ind]
      n_hwp_files = count
      ra_offs     = INTARR(n_hwp_files)
      dec_offs    = INTARR(n_hwp_files)
      ;
      FOR i = 0, n_hwp_files-1 DO BEGIN
        ;Read in the BDP image and the Step3 astrometry header info
        tmpArr = READFITS(hwp_imageFiles[i])
        header = READHEAD(hwp_astroFiles[i])
        ;
        IF(i EQ 0) THEN BEGIN
          ;Initalize arrays to store images and string headers
          image_mean   = FLTARR(n_hwp_files)
          sample_image = FLTARR(ny,ny)
          big_mask     = FLTARR(nx,ny,n_hwp_files)
          big_image    = FLTARR(nx,ny,n_hwp_files)
          big_header   = STRARR(250, n_hwp_files)
          n_header     = LONARR(n_hwp_files)
        ENDIF
        ;
        ; save header
        ;
        n_header[i] = N_ELEMENTS(header)
        big_header[0:n_header[i]-1,i] = header[*]
        ;
        ; compute mean of this image
        ;
        zz = MEDIAN_FILTERED_MEAN(tmpArr[50:nx-50:10,50:ny-50:10])
        image_mean[i] = zz[0]
        PRINT, 'Image mean for ',FILE_BASENAME(hwp_imageFiles[i]),' = ',zz[0]
        ;
        ; mark bad pixels and fix them
        ;
        median_tmpArr = MEDIAN(tmpArr,5)
        bad_pix       = ABS((tmpArr- median_tmpArr)/zz[1]) gt 4.0
        neighborCount = 0*bad_pix
        FOR dx = -1, 1 DO BEGIN
          FOR dy = -1, 1 DO BEGIN
            neighborCount += SHIFT(bad_pix, dx, dy)
          ENDFOR
        ENDFOR
        ;
        ;This line saves maps of the bad pixels clusters from clusters of 1 to 8
        ;FOR iNeighbor = 1, 8 DO WRITEFITS, STRTRIM(iNeighbor, 2)+'neighbors.fits', (neighborCount LT iNeighbor)
        ;
        bad_pix = bad_pix AND (neighborCount LT 4)
        bad_ind  = WHERE(bad_pix, numBad)
        IF numBad GT 0 THEN tmpArr[bad_ind] = -1E6
        tmpArr = MODEL_BAD_PIXELS(tmpArr)
        ;
        ; normalize image by mean; if non-zero or wierd, then stop
        ;
        IF(FINITE(zz[0]) AND zz[0] NE 0.0) THEN BEGIN
          tmpArr /= FLOAT(zz[0])
        ENDIF ELSE BEGIN
          PRINT, 'Error -> mean is strange. Stopping.'
          STOP
        ENDELSE
        ;
        ; extract astrometry from header
        ;
        EXTAST, header, astrom
        ;
        ; get x, y location of galaxy center in pixels
        ;
        AD2XY, RA_cen, Dec_cen, astrom, xc, yc
        ;
        ; determine shift of mask
        ;
        ra_off  = ROUND(xc - 512)
        dec_off = ROUND(yc - 513)
        PRINT, "mask shift = ", ra_off, ",", dec_off," in RA, Dec directions"
        ra_offs[i]  = ra_off
        dec_offs[i] = dec_off
        ;
        ; Generate a mask for this image using the information from maskInfo.dat
        ;
        this_mask = GENERATE_MASK(event, 1024, 1026, xc, yc, 0.579, 5.0*skynoise, /STARS)
        ;
        sample_image += (1.0 - this_mask)                   ;Accumulate a count of the number of pixels sampled
        masked_image  = tmpArr                              ;Make a copy of this image
        masked_image[WHERE(this_mask)] = !VALUES.F_NAN      ;Fill in masked pixels with NaN
        ;
        ;quick debugging examination
        ;        WINDOW, 0, XS=0.5*nx, YS=0.5*ny
        ;        sky, a, skymode, skynoise
        ;        TV, REBIN(BYTSCL(masked_image, MIN=skymode-skynoise, MAX=skymode+3*skynoise), 0.5*nx, 0.5*ny)
        ;        stop
        ;***************************
        ;
        subsampled_image = masked_image[50:nx-50:10,50:ny-50:10]
        ind              = WHERE((subsampled_image NE 0.0) AND FINITE(subsampled_image))
        subsampled_image = subsampled_image[ind]
        zz               = MEDIAN_FILTERED_MEAN(subsampled_image)
        ;
        ;Normalize the masked image by the mean of the image.
        ;
        masked_image    /= zz[0]
        big_mask[*,*,i]  = masked_image[*,*]
        big_image[*,*,i] = tmpArr[*,*]
        ;
      ENDFOR
      ;
      ;Generate the supersky image for this HWP angle
      printString = STRING((ihwp+1),n_hwp, FORMAT='("Generating supersky image for HWP #",I2," of ",I2)')
      PRINT_TEXT2, event, printString
      mfmImage = JM_MEDIAN_FILTERED_MEAN(big_mask, DIMENSION = 3)
      imageStr = STRING(FORMAT='("HWP ",I2," of ",I2)', (ihwp+1), n_hwp)
      UPDATE_PROGRESSBAR, imageProgressBarWID, $                    ;Update the progress bar to show the latest progress
        100*FLOAT(ihwp+1)/FLOAT(n_hwp), DISPLAY_MESSAGE = imageStr
      ;
      output = FIX_BAD_PIXELS(mfmImage.mean)                        ;Repair any anomalous pixels in the supersky image
      ;
      ; compute new mean
      ;
      zz = MEDIAN_FILTERED_MEAN(output[50:nx-50:10,50:ny-50:10])
      ;
      ; normalize
      ;
      output /= FLOAT(zz[0])
      ;
      ; compute median image
      ;
      median_output = MEDIAN(output,9)
      ;
      ; find all pixels in output deviating by more than 4 sigma from median and replace with
      ; their median values
      ;
      bad_image = ABS((output - median_output)/zz[1]) GT 4.0
      ind_bad = WHERE(bad_image, nbad)
      IF (nbad GT 0) THEN BEGIN
        output[ind_bad] = median_output[ind_bad]
      ENDIF
      ;
      ; fill all the unfilled pixels with NANS
      ;
      PRINT, 'New output mean, dispersion = ', zz[0], zz[1]
      NANind = WHERE(sample_image EQ 0, numUnknown)
      PRINT, 'There are ', numUnknown, ' pixels with no unmasked measurements.'
      if numUnknown GT 0 then begin
        output[NANind] = !VALUES.F_NAN                         ;Mark unsampled data with NANs
        deltaX         = MEAN(ra_offs)                         ;Compute the centroided poistion...
        deltaY         = MEAN(dec_offs)                        ;...relative to [512,513]
        angle          = groupStruc.GAL_PA + 90E               ;Rotate by PA
        rotSample      = ROT(sample_image, angle, 1E, $        ;Rotate the sample image
          (512 + deltaX), (513 + deltaY), CUBIC = -0.5, /PIVOT)
        minorAxis            = FIX(TOTAL((rotSample LT 1E)[(512 + deltaX),*]));Estimate the minor axis of the unsampled region
        smoothing_resolution = FIX(minorAxis/3) < 20
        ;
        ; find and fill funky values
        ;
        badPix     = ABS(output - 1.0) GT 0.15
        badInds    = WHERE(badPix, numBad)
        badXY      = ARRAY_INDICES(output, badInds)
        PRINT_TEXT2, event, 'Filling bad pixels near the image edges.'
        ;
        ;Loop through all the border inds
        ;
        FOR fillInd = 0, numBad - 1 DO BEGIN
          ;Grab the XY indices for this particular pixel
          thisInd = badXY[*,fillInd]
          thisX   = thisInd[0]
          thisY   = thisInd[1]
          ;
          ;Search for a large enough box to fill this pixel with a median
          ;
          delta = 2
          done  = 0
          WHILE ~done DO BEGIN
            ;Compute an acceptable set of box boundaries
            lf = (thisX - delta)
            IF lf LT 0 THEN BEGIN
              lf = 0
            ENDIF
            rt = lf + 2*delta
            IF rt GT (nx - 1) THEN BEGIN
              rt = nx  - 1
              lf = rt - 2*delta
            ENDIF
            bt = (thisY - delta)
            IF bt LT 0 THEN BEGIN
              bt = 0
            ENDIF
            tp = bt + 2*delta
            IF tp GT (ny - 1) THEN BEGIN
              tp = ny - 1
              bt = tp - 2*delta
            ENDIF
            ;
            ;Compute the median of this box (hopefully filtering out bad values
            ;
            nearbyMed   = MEDIAN(output[lf:rt,bt:tp], /EVEN)
            IF (ABS(nearbyMed - 1.0) GT 0.15) THEN delta++ ELSE BEGIN
              output[thisX,thisY] = nearbyMed
              done = 1
            ENDELSE
            IF delta GT 10 THEN BEGIN
              IF nearbyMed GT (1.0 + 0.15) THEN thisVal = (1.0 + 0.15)
              IF nearbyMED LT (1.0 - 0.15) THEN thisVal = (1.0 - 0.15)
              output[thisX,thisY] = thisVal
              done = 1
            ENDIF
          ENDWHILE
        ENDFOR
        ;
        ;Find all the NAN values and fill them in using a numerical "heat transfer" solution
        ;
        PRINT_TEXT2, event, 'Applying numerical heat transfer method to fill in unsampled pixels...'
        PRINT_TEXT2, event, '...using smoothing kernel width ' + STRTRIM(smoothing_resolution, 2)
        output = INPAINT_NANS(output, SMOOTHING_RESOLUTION = smoothing_resolution, COUNT=count)
        PRINT_TEXT2, event, 'Numerical inpainting completed in ' + STRTRIM(count, 2) + ' iterations.'
      ENDIF
      ;
      ;Estimate the supersky noise for use in the LEE-FILTER
      ;
      SKY, output, flatMode, flatNoise, /SILENT
      output = LEEFILT(output, 3, 1.5*flatnoise, /EXACT)
      ;
      ;Write the supersky image to disk
      out_path = groupStruc.analysis_dir + 'S3_Astrometry' + PATH_SEP() + 'Supersky_flats' + PATH_SEP() + $
        STRING(group_number, 10*this_hwp, FORMAT='("group",I02,"_HWP",I04)') + '_SuperSky.fits'
      WRITEFITS, out_path, output
      
      ;Display the output image to the user
      titleStr = STRING((ihwp+1), FORMAT='("Supersky for HWP ", I2)')
      TVIM, output, range = flatMode + [-3,+5]*flatNoise, TITLE = titleStr
      WAIT, 0.05
      ;***
      ;These lines of code will save several diagnostic images for debugging purposes
      ;***
      ;      out_path = 'supersky_images' + PATH_SEP() + $
      ;        STRING(group_number, 10*this_hwp, FORMAT='("group",I02,"_HWP",I04)') + '_pixelSampling.fits'
      ;      writefits, out_path, sample_image
      ;      out_path = 'supersky_images' + PATH_SEP() + $
      ;        STRING(group_number, 10*this_hwp, FORMAT='("group",I02,"_HWP",I04)') + '_anomalousPixels.fits'
      ;      writefits, out_path, bad_image
      ;
      ; and apply to the inputs
      ;
      new_names = FILE_BASENAME(hwp_imageFiles)
      new_paths = groupStruc.analysis_dir + 'S3_Astrometry' $
        + PATH_SEP() + new_names
      ;
      ;Loop through each HWP file and subtract the new supersky image from it
      FOR i = 0, n_hwp_files-1 DO BEGIN
        ;
        ; (Dan's original) division method:
        ;
        ;big_image[*,*,i] /= output[*,*]
        ;tmpArr            = reform(big_image[*,*,i])
        ;
        ; (Jordan's modified) division method:
        ; (this method has the advantage of rescaling
        ; saved image to its proper mean-value)
        ;
        ;tmpArr  = reform(big_image[*,*,i])
        ;tmpArr /= output[*,*]
        ;tmpArr *= image_mean[i]
        ;
        ;writefits, new_paths[i],a
        ;
        ; subtractive method
        ;
        tmpArr  = REFORM(big_image[*,*,i])
        tmpArr -= output
        tmpArr += 1.0
        tmpArr *= image_mean[i]
        ;
        ; rebuild header
        ;
        this_header = REFORM(big_header[0:n_header[i]-1,i])
        ;
        ; and add info about supersky by this program
        ;
        SXADDPAR, this_header, "HISTORY", 'Background flattened with SUPERSKY_GROUPS.pro'
        histStr = '  using sky scaled subtraction on ' + GETENV('COMPUTERNAME') + ' by ' + GETENV('USERNAME') + ' at ' + SYSTIME()
        SXADDPAR, this_header, "HISTORY", histStr
        WRITEFITS, new_paths[i], tmpArr, this_header
        ;
      ENDFOR
    ENDIF ELSE BEGIN
      PRINT, "  Found no images with this HWP"
    ENDELSE
  ENDFOR
  ;
END

PRO S3B_SUBTRACT_SUPERSKY, event

  tlb_wid = WIDGET_INFO(event.top, FIND_BY_UNAME='WID_BASE')          ;Retrieve the TLB widget ID
  WIDGET_CONTROL, tlb_wid, GET_UVALUE=groupStruc                      ;Retrieve the group summary structure
  
  ;Test if the directory structure is in place and build it if it is not
  testDir  = groupStruc.analysis_dir + 'S3_Astrometry'
  testDir1 = testDir + PATH_SEP() + 'Supersky_flats'
  IF ~FILE_TEST(testDir1, /DIRECTORY) THEN FILE_MKDIR, testDir1
  
  groupProgressBarWID = WIDGET_INFO(event.top, FIND_BY_UNAME='GROUP_PROGRESS_BAR')
  imageProgressBarWID = WIDGET_INFO(event.top, FIND_BY_UNAME='IMAGE_PROGRESS_BAR')

  ;Costruct some basic path strings
  S3A_path   = groupStruc.analysis_dir + 'S3_Astrometry'
  S2_path    = groupStruc.analysis_dir + 'S2_Ski_Jump_Fixes'
  maskPath   = S2_path + PATH_SEP() + 'Masking_files'
  maskfile   = maskPath + PATH_SEP() + 'maskInfo.dat'
  
  ;Read in the mask information file and extract the mask (RA, Dec) center
  maskHeader = READHEAD(maskFile)
  RA_cen     = SXPAR(maskHeader, 'RA_MASK')
  Dec_cen    = SXPAR(maskHeader, 'Dec_MASK')

  ;Count the number of groups for which to build supersky images
  numGoodGroups = TOTAL(groupStruc.groupFlags)
  groupCount    = 0
  
  ;Loop through each group in the PPOL project
  FOR i = 0, groupStruc.numGroups - 1 DO BEGIN
    ;Skip any groups with groupFlags[i]=0
    IF groupStruc.groupFlags[i] EQ 0 THEN CONTINUE
    groupCount++
    groupProgString = STRING(groupCount, numGoodGroups, FORMAT='("Group ",I2," of ",I2)')
    UPDATE_PROGRESSBAR, groupProgressBarWID, 100*FLOAT(i+1)/numGoodGroups, DISPLAY_MESSAGE=groupProgString
    WAIT, 0.05

    ;Count the number of files in this group with good astrometry    
    goodFiles     = WHERE(groupStruc.astroFlags[i,*] EQ 1 $
                      OR  groupStruc.astroFlags[i,*] EQ 2 $
                      AND groupStruc.groupImages[i,*] NE '', numGood)
    
    ;Check that at least one good file was found
    IF numGood GT 0 THEN BEGIN
      ;Build the filenames for all relevant files
      groupBDPfiles = REFORM(groupStruc.groupImages[i,goodFiles])
      groupS3files  = S3A_path + PATH_SEP() + $
        FILE_BASENAME(groupBDPfiles, '.fits') + '.head'
      FOR j = 0, numGood - 1 DO BEGIN
        S2file = groupStruc.analysis_dir + $
          'S2_Ski_Jump_Fixes' + PATH_SEP() + FILE_BASENAME(groupBDPfiles[j])
        IF FILE_TEST(S2file) THEN groupBDPfiles[j] = S2file
      ENDFOR
    ENDIF

    ;Apply the supersky procedure to THIS group
    SUPERSKY_GROUP, event, groupBDPfiles, groupS3files, RA_cen, Dec_cen, $
      GROUP_NUMBER=(i+1)
  
  ENDFOR

  UPDATE_PROGRESSBAR, imageProgressBarWID, 100E, /PERCENTAGE  
  PRINT_TEXT2, event, 'Supersky procedure complete!'

END


PRO S3B_REFINE_ASTROMETRY, event

  tlb_wid         = WIDGET_INFO(event.top, FIND_BY_UNAME='WID_BASE')  ;Retrieve the TLB widget ID
  groupProgBarWID = WIDGET_INFO(event.top, FIND_BY_UNAME='GROUP_PROGRESS_BAR')
  imageProgBarWID = WIDGET_INFO(event.top, FIND_BY_UNAME='IMAGE_PROGRESS_BAR')

  WIDGET_CONTROL, tlb_wid, GET_UVALUE=groupStruc                      ;Retrieve the group summary structure

  S3B_files = groupStruc.analysis_dir + 'S3B_Supersky_subtraction' $  ;Save the path to the supersky flattened images
    + PATH_SEP() +   'Supersky_subtracted' + PATH_SEP() + '*.fits'
  astroFiles = FILE_SEARCH(S3B_files, COUNT=numFiles)                 ;Explicitly search for all *.fits files in supersky directory
  numStars   = N_ELEMENTS(groupStruc.starInfo)                        ;Count the maximum number of possible astrometry stars
  
  nextPercentagePoint = 1B                                            ;Variable to track what the next progress update will be
  UPDATE_PROGRESSBAR, groupProgBarWID, /ERASE                         ;Clear out any previous progress bar status
  UPDATE_PROGRESSBAR, imageProgBarWID, 0E, /PERCENTAGE                ;Prepare the "Image progress bar" for use

;  FOR i = 0, numFiles - 1 DO BEGIN
;    astroImage = READFITS(astroFiles[i], astroHeader)                 ;Read in the image
;    sz         = SIZE(astroImage, /DIMENSIONS)                        ;Get the image dimensions
;    hist       = SXPAR(astroHeader, "HISTORY")                        ;Get the history info
;    SXDELPAR, astroHeader,'HISTORY'                                   ;delete any previous history entries    
;    EXTAST, astroHeader, astr                                         ;Extract the initial astrometry
;    AD2XY, groupStruc.starInfo.RAJ2000, groupStruc.starInfo.DEJ2000, $;Solve for initial guesses on star positions
;      astr, xGuess, yGuess
;    
;    useStar = (xGuess GT 30) AND (xGuess LT (sz[0] - 31)) $           ;Only use stars more than 30 pixels from image edge
;      AND (yGuess GT 30) AND (yGuess LT (sz[1] - 31))
;    useInds = WHERE(useStar, numUse)                                  ;Get the indices of the usable stars
;
;    IF numUse GT 0 THEN BEGIN
;      astroStars = groupStruc.starInfo[useInds]                       ;Cull the 2MASS data
;      xGuess     = xGuess[useInds]                                    ;Cull the list to only the on-image stars
;      yGuess     = yGuess[useInds]
;    ENDIF
;    
;    xStars    = xGuess                                                ;Alias the x-star positions for refinement
;    yStars    = yGuess                                                ;Alias the y-star positions for refinement
;    useStar   = BYTARR(numUse)                                        ;Reset the "useStar" to track which stars were well fit
;    FWHMs     = FLTARR(numUse)                                        ;Initalize an array for storing star FWHMs
;    failedFit = 0                                                     ;Set a counter for the number of failed Gaussian star fits
;    FOR j = 0, numUse - 1 DO BEGIN
;      ;Cut out a subarray for a more precise positioning
;      xOff     = (xGuess[j] - 19) > 0
;      xRt      = (xOff  + 40) < (sz[0] - 1)
;      yOff     = (yGuess[j] - 19) > 0
;      yTop     = (yOff + 40)  < (sz[1] - 1)
;      subArray = astroImage[xOff:xRt, yOff:yTop]
;      
;      result   = GAUSS2DFIT(subArray, A, /TILT)                       ;Gaussian fit the star
;      inArray  = (A[4] GT 5) AND (A[4] LT 34) $                       ;If the fit is located in the center of the array
;        AND (A[5] GT 5) AND (A[5] LT 34)
;      okShape  = (A[2] GT 0.8) AND (A[2] LT 5) $                      ;and if its gaussian width is reasonable (not a hot pixel)
;        AND (A[3] GT 0.8) AND (A[3] LT 5)
;      
;      methodDifference = 0                                            ;Reset the method difference variable
;      IF inArray AND okShape THEN BEGIN
;        FWHMs[j] = SQRT(ABS(A[2]*A[3]))*2.355                         ;Compute the FWHM for this star
;        GCNTRD, subArray, A[4], A[5], xcen, ycen,  FWHMs[j]           ;Centroid this star (using estimated FWHM)
;        methodDifference = SQRT((xCen - A[4])^2 + (yCen - A[5])^2)    ;Compute difference between the two locating methods
;        IF (methodDifference LE 1) $                                  ;If the two methods have a consensus,
;          AND FINITE(methodDifference) THEN BEGIN                     ;then update the star positions
;          xStars[j]  = xOff + xcen
;          yStars[j]  = yOff + ycen
;          useStar[j] = 1                                              ;Mark this star as one of the stars to use
;          failedFit  = 0                                              ;If the fit was successful, then reset the failed fit counter
;;          TVIM, subarray
;;          OPLOT, [xcen], [ycen], PSYM=6, color=255L
;;          stop
;        ENDIF
;      ENDIF
;      IF ~inArray OR ~ okShape $                                      ;If any one of the tests failed,
;        OR (methodDifference GT 1) OR ~FINITE(methodDifference) $     ;then increment the failedFit counter
;        THEN failedFit++
;      IF failedFit GE 2 THEN BREAK                                    ;If the "failed fit"
;    ENDFOR
;
;    useInds = WHERE(useStar, numUse)                                  ;Determine which stars were well fit
;    IF numUse GT 0 THEN BEGIN
;      astroStars = astroStars[useInds]                                ;Cull the 2MASS data
;      xStars     = xStars[useInds]                                    ;Cull the list to only the well fit stars
;      yStars     = yStars[useInds]
;    ENDIF
;
;    printString = STRING(numUse, FORMAT='("Successfully located ",I2," stars in image ")') + $
;      FILE_BASENAME(astroFiles[i])
;    PRINT_TEXT2, event, printString
;    
;    IF numUse GT SXPAR(astroHeader, 'PPOLNSTR') THEN BEGIN            ;Determine if this is an improvement in the astrometry
;      ;Now that the star positions are known, update the astrometry
;      IF numUse GE 6 THEN BEGIN                                       ;Begin least squares method of astrometry
;        ;**** PERFORM LEAST SQUARES ASTROMETRY ****
;        astr = JM_SOLVE_ASTRO(astroStars.RAJ2000, astroStars.DEJ2000, $
;          xStars, yStars, NAXIS1 = sz[0], NAXIS2 = sz[1])
;        crpix = [511, 512]
;        XY2AD, crpix[0], crpix[1], astr, crval1, crval2
;        astr.crpix = (crpix + 1)                                      ;FITS convention is offset 1 pixel from IDL
;        astr.crval = [crval1, crval2]                                 ;Store the updated reference pixel values
;        PUTAST, astroHeader, astr, EQUINOX = 2000                     ;Update the header with the new astrometry
;      ENDIF ELSE IF numUse GE 3 THEN BEGIN                            ;Begin "averaging" method of astrometry
;        ;**** PERFORM 3-5 STAR ASTROMETRY ****
;        numTri = numUse*(numUse-1)*(numUse-2)/6
;        big_cd = DBLARR(2,2,numTri)
;        triCnt = 0                                                    ;Initalize a counter for looping through triangles
;        FOR iStar = 0, numUse - 1 DO BEGIN                            ;Loop through all possible triangles
;          FOR jStar = iStar+1, numUse - 1 DO BEGIN
;            FOR kStar = jStar+1, numUse - 1 DO BEGIN
;              these_stars = [iStar,jStar,kStar]                       ;Grab the indices of the stars in this triangle
;              STARAST, astroStars[these_stars].RAJ2000, $             ;Sove astrometry using this triangle of stars
;                astroStars[these_stars].DEJ2000, $
;                xStars[these_stars], yStars[these_stars], $
;                this_cd, PROJECTION = 'TAN'
;              big_cd[*,*,triCnt] = this_cd                            ;Store the CD matrix
;              triCnt++                                                ;Increment the triangle counter
;            ENDFOR
;          ENDFOR
;        ENDFOR
;        cd_matrix = DBLARR(2,2)                                       ;Initalize an array for mean CD matrix
;        FOR iMat = 0, 1 DO BEGIN
;          FOR jMat = 0, 1 DO BEGIN
;            cd_matrix[iMat,jMat] = $                                  ;Compute the mean CD matrix
;              (MEDIAN_FILTERED_MEAN(REFORM(big_cd[iMat,jMat, *])))[0]
;          ENDFOR
;        ENDFOR
;        crpix      = [511, 512]                                       ;Define the center pixels
;        centerDist = SQRT((xStars - crpix[0])^2 + $                   ;Compute star distances from the image center
;          (yStars - crpix[1])^2)
;        centerStar = WHERE(centerDist EQ MIN(centerDist))             ;Grab the star closest to the image center
;        deltaX     = crpix[0] - xStars[centerStar]                    ;Compute pixel x-offset from center
;        deltaY     = crpix[1] - yStars[centerStar]                    ;Compute pixel y-offset from center
;        deltaAD    = REFORM(cd_matrix##[[deltaX],[deltaY]])           ;Compute (RA, Dec) offsets from center
;        deltaAD[0] = $                                                ;Correct RA offset for distortion
;          deltaAD[0]*COS(astroStars[centerStar].DEJ2000*!DTOR)
;        ;      crval      = [astroStars[centerStar].RAJ2000, $        ;Re-compute the center value
;        ;                    astroStars[centerStar].DEJ2000]
;        MAKE_ASTR, astr, CD = cd_matrix, CRPIX = [xStars[centerStar], yStars[centerStar]], $ ;Create final astrometry structure
;          CRVAL = [astroStars[centerStar].RAJ2000, astroStars[centerStar].DEJ2000], CTYPE = ['RA---TAN','DEC--TAN']
;        XY2AD, crpix[0], crpix[1], astr, crval1, crval2               ;Recenter astrometry structure
;        MAKE_ASTR, astr, CD = cd_matrix, CRPIX = (crpix+1), $         ;Create final astrometry structure
;          CRVAL = [crval1, crval2], CTYPE = ['RA---TAN','DEC--TAN']
;        PUTAST, astroHeader, astr, EQUINOX = 2000                     ;Store astrometry in header
;      ENDIF ELSE IF numUse EQ 2 THEN BEGIN
;        ;****PERFORM 2-STAR ASTROMETRY****
;        yMaxInd     = WHERE(yStars EQ MAX(yStars), COMPLEMENT=yMinInd)
;        dXpix       = xStars[yMaxInd] - xStars[yMinInd]               ;Compute dx vector
;        dYpix       = yStars[yMaxInd] - yStars[yMinInd]               ;Compute dy vector
;        dRA         = (astroStars[yMaxInd].RAJ2000 - astroStars[yMinInd].RAJ2000)*COS(MEAN(astroStars.DEJ2000)*!DTOR)
;        dDec        = (astroStars[yMaxInd].DEJ2000 - astroStars[yMinInd].DEJ2000)
;        deltaPix    = SQRT(dXpix^2 + dYpix^2)                         ;Compute the pixel separation
;        deltaTheta  = SQRT(dRA^2 + dDec^2)                            ;Compute angular separation (deg)
;        plate_scale = deltaTheta/deltaPix                             ;Compute the plate scale (deg/pix)
;        rotAnglePix = ATAN(dYpix, dXpix)*!RADEG                       ;Compute rotation angle of the two stars in image coordinates
;        rotAngleEQ  = 180 - ATAN(dDec,  dRA)*!RADEG                   ;Compute rotation angle of the two stars in equatorial coords.
;        CDmat1      = [[-plate_scale, 0E         ], $
;          [ 0E         , plate_scale]]
;        relativeRot = (rotAngleEQ - rotAnglePix)*!DTOR
;        rotMatrix   = [[COS(relativeRot), -SIN(relativeRot)], $
;          [SIN(relativeRot),  COS(relativeRot)]]
;        CDmat       = CDmat1##rotMatrix
;        crpix       = [511, 512]                                      ;Define the center pixels
;        centerDist  = SQRT((xStars - crpix[0])^2 + $                  ;Compute star distances from the image center
;          (yStars - crpix[1])^2)
;        centerStar  = WHERE(centerDist EQ MIN(centerDist))            ;Grab the star closest to the image center
;        deltaX      = crpix[0] - xStars[centerStar]                   ;Compute pixel x-offset from center
;        deltaY      = crpix[1] - yStars[centerStar]                   ;Compute pixel y-offset from center
;        deltaAD     = REFORM(CDmat##[[deltaX],[deltaY]])              ;Compute (RA, Dec) offsets from center
;        deltaAD[0]  = $                                               ;Correct RA offset for distortion
;          deltaAD[0]*COS(astroStars[centerStar].DEJ2000*!DTOR)
;        MAKE_ASTR, astr, CD = CDmat, CRPIX = [xStars[centerStar], yStars[centerStar]], $ ;Create final astrometry structure
;          CRVAL = [astroStars[centerStar].RAJ2000, astroStars[centerStar].DEJ2000], CTYPE = ['RA---TAN','DEC--TAN']
;        XY2AD, crpix[0], crpix[1], astr, crval1, crval2               ;Recenter astrometry structure
;        MAKE_ASTR, astr, CD = CDmat, CRPIX = (crpix+1), $             ;Create final astrometry structure
;          CRVAL = [crval1, crval2], CTYPE = ['RA---TAN','DEC--TAN']
;        PUTAST, astroHeader, astr, EQUINOX = 2000
;      ENDIF ELSE BEGIN
;        ;****PERFORM 1-STAR ASTROMETRY*****
;      ENDELSE
;      
;      
;;      SXADDPAR, astroHeader, 'GPCOORD', 1                             ;Add remainind header keywords
;;      SXADDPAR, astroHeader, 'PP3_LLQF', 0, 'Lower Left Quad
;      SXADDPAR, astroHeader, 'PPOLNSTR', numUse, 'Number of stars used in manual astrometry'
;      
;      ;Restore the history to the header
;      n_old = N_ELEMENTS(hist)
;      FOR j= 0, n_old - 1 DO BEGIN
;        SXADDPAR, astroHeader, "HISTORY", hist[j]
;      ENDFOR
;  
;      WRITEFITS, astroFiles[i], astroImage, astroHeader               ;Write the file to disk
;    ENDIF; ELSE STOP
;
;    progressPercentage = 100*FLOAT(i+1)/FLOAT(numFiles)               ;Compute progress
;    IF progressPercentage GT nextPercentagePoint THEN BEGIN           ;Determine if the bar needs to be updated
;      UPDATE_PROGRESSBAR, imageProgBarWID, progressPercentage, $      ;Update the progress bar if necessary
;        /PERCENTAGE
;      WAIT, 0.05
;      nextPercentagePoint++                                           ;Increment next update
;    ENDIF
;
;  ENDFOR
;
;  UPDATE_PROGRESSBAR, imageProgBarWID, 100, $                         ;Update the progress bar
;    /PERCENTAGE
;  
  ;Now that all the astrometry has been updated,
  ;it is time to compute the dither coverage boundaries.
  displayWID = WIDGET_INFO(event.top, FIND_BY_UNAME='IMAGE_DISPLAY_WINDOW')
  WIDGET_CONTROL, displayWID, GET_VALUE=windowIndex                   ;Retrieve display window WID
  WSET, windowIndex                                                   ;Set plot window WID
  
  ;Find all the astrometrically registered *.fits images
  S3files = groupStruc.analysis_dir + 'S3_Astrometry' + PATH_SEP() + '*.fits'
  S3files = FILE_SEARCH(S3files, COUNT=nfiles)
  
  hwp = FLTARR(nfiles)                                                ;Initalize array to store HWP angles
  FOR j = 0, nfiles - 1 DO BEGIN
    header = HEADFITS(S3files[j])                                     ;Loop through ALL images in group
    hwp[j] = SXPAR(header, "MB_HWP")                                  ;Save THIS image HWP rotation angle
  ENDFOR
  sortArr     = SORT(hwp)                                             ;Sort the HWP angles
  S3files     = S3files[sortArr]                                      ;Sort the files to match the HWP angles
  hwp         = hwp[sortArr]
  hwp_uniq    = hwp[UNIQ(hwp)]                                        ;Grab the uniq HWP angles
  n_hwp       = N_ELEMENTS(hwp_uniq)                                  ;Count the number of uniq HWP angles
  file_exists = FILE_TEST(S3files)                                    ;Test which files exist
  
  numImages = BYTARR(n_hwp)                                           ;A vector to keep track dither number per HWP
  FOR j = 0, n_hwp - 1 DO BEGIN                                       ;Loop through all the uniq HWP angles
    HWPind        = WHERE(hwp EQ hwp_uniq[j] AND file_exists, numHWP) ;Find all the images at this HWP angle
    numImages[j] = numHWP                                             ;Store the number of dithers at this HWP angle
  ENDFOR
  HWP_select = MIN(WHERE(numImages EQ MAX(numImages)))                ;Select the first HWP angle with the MOST dither positions
  HWP_inds   = WHERE(hwp EQ hwp[HWP_select], numImages)               ;Grab the indices for the selected HWP angle images
  
  FOR j = 0, numImages - 1 DO BEGIN                                   ;Loop throug all the images with the selected HWP
    tmpHeader = HEADFITS(S3files[HWP_inds[j]])                        ;Read in this file's header
    EXTAST, tmpHeader, astr, noparams                                 ;Extract astrometry from header
    IF (N_ELEMENTS(all_astr) EQ 0) THEN BEGIN                         ;Store astrometry (test for first case)
      all_astr = astr                                                 ;Store all the astrometry
    ENDIF ELSE BEGIN
      all_astr = [all_astr, astr]
    ENDELSE
  ENDFOR
  
  PRINT_TEXT2, event, STRING(hwp[HWP_select], FORMAT='("Using HWP=",F6.2," to determine dither coverage")')
  
  deltaRA  = 1004*0.579D/3600D                                        ;Usable image width in degrees
  deltaDec = 1018*0.579D/3600D                                        ;Usable image height in degrees

  leftRAs  = all_astr.crval[0] + 0.5*deltaRA/COS(all_astr.crval[1]*!DTOR)
  rightRAs = all_astr.crval[0] - 0.5*deltaRA/COS(all_astr.crval[1]*!DTOR)
  
  botDecs  = all_astr.crval[1] - 0.5*deltaDec
  topDecs  = all_astr.crval[1] + 0.5*deltaDec

  limitRA  = [MIN(leftRAs), MAX(rightRAs)]
  limitDec = [MAX(botDecs), MIN(topDecs)]
  
  EXTAST,  groupStruc.displayHeader, displayAstr                      ;Grab the astrometry from the 2MASS image
  AD2XY, groupStruc.starInfo.RAJ2000, groupStruc.starInfo.DEJ2000, $
    displayAstr, xStarPlot, yStarPlot
  AD2XY, limitRA, limitDec, displayAstr, limitX, limitY               ;Compute the coverage limits in pixel space
  AD2XY, all_astr.crval[0], all_astr.crval[1], $                      ;Compute position of each dither pointing (in pixel space)
    displayAstr, pointX, pointY
  lfLimit = MIN(limitX)
  rtLimit = MAX(limitX)
  btLimit = MIN(limitY)
  tpLimit = MAX(limitY)
  
;  groupStruc.coverageBoundaries = {RA:limitRA, Dec:limitDec}          ;Store the limits...
  UPDATE_GROUP_SUMMARY, event, groupStruc, $                          ;and update the group structure
    'coverageBoundaries', {RA:limitRA, Dec:limitDec}, /SAVE
  
  WIDGET_CONTROL, displayWID, GET_VALUE=windowIndex                   ;Retrieve display window WID
  WSET, windowIndex                                                   ;Set plot window WID
  SKY, groupStruc.displayImage, skyMode, skyNoise, /SILENT
  TVIM, groupStruc.displayImage, RANGE = skyMode + [-3, +100]*skyNoise;Reset image display and overplot stars
  OPLOT, xStarPlot, yStarPlot, PSYM = 6, COLOR = RGB_TO_DECOMPOSED([0,255,0])
  OPLOT, pointX, pointY, PSYM=4, COLOR=RGB_TO_DECOMPOSED([0,255,255]) ;Overplot the dither pointings
  PLOTS, [lfLimit,lfLimit,rtLimit,rtLimit,lfLimit], $                 ;Draw the boundary of the dither coverage
    [btLimit,tpLimit,tpLimit,btLimit,btLimit]

END

;PRO S3B_SWAP_FILES, event
;  
;  currentS3filesWID = WIDGET_INFO(event.top, FIND_BY_UNAME='S3B_S3_CURRENT_FILES')
;  tlb_wid           = WIDGET_INFO(event.top, FIND_BY_UNAME='WID_BASE');Retrieve the TLB widget ID
;  WIDGET_CONTROL, tlb_wid, GET_UVALUE=groupStruc                      ;Retrieve the group summary structure
;  
;  S3A_dir       = groupStruc.analysis_dir + 'S3_Astrometry'
;  S3A_backupDir = S3A_dir + PATH_SEP() + 'backup'
;  S3B_dir       = groupStruc.analysis_dir + 'S3B_Supersky_subtraction' $
;    + PATH_SEP() + 'Supersky_subtracted'
;  IF ~FILE_TEST(S3A_backupDir, /DIRECTORY) $                          ;Make sure a backup directory exists
;    THEN FILE_MKDIR, S3A_backupDir
;
;  IF groupStruc.currentS3files EQ 'not supersky flattened' THEN BEGIN
;    S3A_files = S3A_dir + PATH_SEP() + '*.fits'
;    S3B_files = S3B_dir + PATH_SEP() + '*.fits'
;    FILE_MOVE, S3A_files, S3A_backupDir                               ;Move the original files into the backup directory
;    FILE_MOVE, S3B_files, S3A_dir                                     ;Move the flattened files into the original directory
;    
;;    groupStruc.currentS3files = 'supersky flattened'
;    UPDATE_GROUP_SUMMARY, event, groupStruc, 'currentS3files', 'supersky flattened', /SAVE
;    WIDGET_CONTROL, currentS3filesWID, SET_VALUE='supersky flattened'
;    
;  ENDIF ELSE IF groupStruc.currentS3files EQ 'supersky flattened' THEN BEGIN
;    S3B_files = S3A_dir + PATH_SEP() + '*.fits'
;    S3A_Files = S3A_dir + PATH_SEP() + 'backup' + PATH_SEP() + '*.fits'
;    FILE_MOVE, S3B_files, S3B_dir                                     ;Restore the flattened files to the S3B directory
;    FILE_MOVE, S3A_files, S3A_dir                                     ;Restore the original files into the S3 directory
;    
;;    groupStruc.currentS3files = 'not supersky flattened'
;    UPDATE_GROUP_SUMMARY, event, groupStruc, 'currentS3files', 'not supersky flattened', /SAVE
;    WIDGET_CONTROL,currentS3filesWID, SET_VALUE='not supersky flattened'
;    
;  ENDIF
;
;  
;END