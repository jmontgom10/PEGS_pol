FUNCTION PARSE_HWPS, fileList, UNIQ_HWPS = hwp_uniq
  ; JDM 2060913
  ; 
  ; This function simply determines which unique HWPs are represented in a list of files.
  ; It returns a list of unique HWPs.

  ;Read the images to group by HWP
  nFiles = N_ELEMENTS(fileList)
  hwp = FLTARR(nFiles)
  FOR i = 0, nFiles-1 do begin
    thisFile = fileList[i]
    thisExt  = STRUPCASE((REVERSE(STRSPLIT(thisFile, '.', /EXTRACT)))[0])
    ;Use "READHEAD" for .head files and "HEADFITS" for .fits files
    IF thisExt EQ 'FIT' OR thisExt EQ 'FITS' THEN BEGIN
      header = HEADFITS(thisFile)
    ENDIF ELSE IF thisExt EQ 'HEAD' THEN BEGIN
      header = READHEAD(thisFile)
    ENDIF
    ;Store the HWP value for this file
    hwp[i] = SXPAR(header, "MB_HWP")
  ENDFOR
  ;
  ;Grab the unique HWP values to return if requested
  ;
  ind = SORT(hwp)
  hwp_uniq = hwp(ind[uniq(hwp[ind])])

  RETURN, hwp
END

FUNCTION MARK_LOCAL_OUTLIERS, inputImg, noise, SIG_CUT = sig_cut
  ;
  ; mark pixels outside noise threshold if local median
  ;
  
  ; Set default sigma cut threshold
  IF N_ELEMENTS(sig_cut) EQ 0 THEN sig_cut = 4.0
  
  ; Copy image for output
  outputImg = inputImg
  
  medianImg     = MEDIAN(inputImg,5)
  badPix        = ABS((inputImg - medianImg)/noise) gt 4.0
  neighborCount = 0*badPix
  FOR dx = -1, 1 DO BEGIN
    FOR dy = -1, 1 DO BEGIN
      neighborCount += SMART_SHIFT(badPix, dx, dy)
    ENDFOR
  ENDFOR

  ; M
  badPix = badPix AND (neighborCount LT 4)
  badInd  = WHERE(badPix, numBad)
  IF numBad GT 0 THEN outputImg[badInd] = -1E6

  RETURN, outputImg
END

FUNCTION MEDIAN_FILTER_SUPERSKY, inputImg, ACCEPT_RANGE = accept_range
  ;This function simply filters the bad pixels in the supersky image
  
  ;Set default acceptance range
  IF N_ELEMENTS(ACCEPT_RANGE) EQ 0 THEN accept_range = [0.85, 1.15]
  
  ;Store the min and max values of the acceptance range
  minAcc = MIN(accept_range)
  maxAcc = MAX(accept_range)
  
  ;Copy the inputImg
  outputImg = inputImg
  
  ;Get image dimensions
  sz = SIZE(inputImg, /DIMENSIONS)
  nx = sz[0]
  ny = sz[1]
  
  ;
  ; find and fill funky values
  ;
  badPix     = (inputImg LT minAcc) OR (inputImg GT maxAcc)
  badInds    = WHERE(badPix, numBad)
  badXY      = ARRAY_INDICES(inputImg, badInds)
  ;
  ;Loop through all the border inds
  ;
  IF numBad GT 0 THEN BEGIN
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
        nearbyMed   = MEDIAN(inputImg[lf:rt,bt:tp], /EVEN)
        IF (ABS(nearbyMed - 1.0) GT 0.15) THEN delta++ ELSE BEGIN
          outputImg[thisX,thisY] = nearbyMed
          done = 1
        ENDELSE
        IF delta GT 10 THEN BEGIN
          IF nearbyMed GT maxAcc THEN outputImg[thisX,thisY] = maxAcc
          IF nearbyMED LT minAcc THEN outputImg[thisX,thisY] = minAcc
          done = 1
        ENDIF
      ENDWHILE
    ENDFOR
  ENDIF

  
  RETURN, outputImg
END

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
  threshWID           = WIDGET_INFO(event.top, FIND_BY_UNAME='S3B_MASKING_THRESHOLD')
  displayWID          = WIDGET_INFO(event.top, FIND_BY_UNAME='IMAGE_DISPLAY_WINDOW')
  WIDGET_CONTROL, displayWID, GET_VALUE=windowIndex         ;Retrieve display window WID
  WSET, windowIndex                                         ;Set plot window WID
  
  WIDGET_CONTROL, event.top, GET_UVALUE = groupStruc        ;Retrieve the groupStruc info structure
  WIDGET_CONTROL, threshWID, GET_VALUE = maskMag            ;Retrieve the maskingThreshold (mag/arcsec^2)

  ;Compute masking level using display image header info
  IF groupStruc.NIRband EQ 'H' THEN BEGIN
    zpMag = SXPAR(groupStruc.displayHeader, 'HMAGZP')
  ENDIF ELSE IF groupStruc.NIRband EQ 'Ks' THEN BEGIN
    zpMag = SXPAR(groupStruc.displayHeader, 'KMAGZP')
  ENDIF
  maskThresh = 10.0^(-0.4*(maskMag - zpMag))
  
  ;Test that there are an identical number of image and astrometry files for this group
  IF N_ELEMENTS(imageFiles) NE N_ELEMENTS(astrometryFiles) THEN STOP
  nfiles = N_ELEMENTS(imageFiles)
  ;
  ; store the default Mimir image size
  ;
  nx = 1024
  ny = 1026
  ;
  ; get the unique HWPs in this file list
  ;
  HWP   = ROUND(10*PARSE_HWPS(astrometryFiles, UNIQ_HWPS = hwp_uniq))
  n_hwp = N_ELEMENTS(hwp_uniq)
  PRINT, "Found ",n_hwp," different HWP angles: ",hwp_uniq
  ;
  ; for each unique HWP angle, collect the dithered images to form superskies
  ;
  FOR ihwp = 0, n_hwp-1 DO BEGIN
    this_hwp = ROUND(10*hwp_uniq[ihwp])
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
          image_noise  = FLTARR(n_hwp_files)
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
        image_mean[i]  = zz[0]
        image_noise[i] = zz[1]
        PRINT, 'Image mean for ',FILE_BASENAME(hwp_imageFiles[i]),' = ',zz[0]
        ;
        ; mark local outlier pixels and fix them
        ;
        tmpArr = MARK_LOCAL_OUTLIERS(tmpArr, zz[1], SIG_CUT = 4.0)
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
        this_mask = GENERATE_MASK(event, 1024, 1026, xc, yc, 0.579, maskThresh, /STARS)
        ;
        sample_image += (1.0 - this_mask)                   ;Accumulate a count of the number of pixels sampled
        masked_image  = tmpArr                              ;Make a copy of this image
        masked_image[WHERE(this_mask)] = !VALUES.F_NAN      ;Fill in masked pixels with NaN
        ;
        ;quick debugging examination
        ;
        ;WINDOW, 0, XS=0.5*nx, YS=0.5*ny
        ;SKY, a, skymode, skynoise
        ;TV, REBIN(BYTSCL(masked_image, MIN=skymode-skynoise, MAX=skymode+3*skynoise), 0.5*nx, 0.5*ny)
        ;stop
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
      
      ;Fill in suprious values
      PRINT_TEXT2, event, 'Filling bad pixels near the image edges.'
        ;Compute masking level using display image header info
      IF groupStruc.NIRband EQ 'H' THEN BEGIN
        accept_range = [0.85, 1.20]
      ENDIF ELSE IF groupStruc.NIRband EQ 'Ks' THEN BEGIN
        accept_range = [0.85, 1.30]
      ENDIF
      output = MEDIAN_FILTER_SUPERSKY(output, ACCEPT_RANGE = accept_range)
      
      if numUnknown GT 0 then begin
        output[NANind] = !VALUES.F_NAN                         ;Mark unsampled data with NANs
        deltaX         = MEAN(ra_offs)                         ;Compute the centroided poistion...
        deltaY         = MEAN(dec_offs)                        ;...relative to [512,513]
        angle          = groupStruc.GAL_PA + 90E               ;Rotate by PA
        rotSample      = ROT(sample_image, angle, 1E, $        ;Rotate the sample image
          (512 + deltaX), (513 + deltaY), CUBIC = -0.5, /PIVOT)
        minorAxis            = FIX(TOTAL((rotSample LT 1E)[(512 + deltaX),*]));Estimate the minor axis of the unsampled region
        smoothing_resolution = FIX(minorAxis/3) < 30           ;Use a kernel no larger than 20 pixels...
        smoothing_resolution = smoothing_resolution > 3        ;...and no smaller than 3 pixels
        ;
        ;Find all the NAN values and fill them in using a numerical "heat transfer" solution
        ;
        PRINT_TEXT2, event, 'Applying numerical heat transfer method to fill in unsampled pixels...'
        PRINT_TEXT2, event, '...using smoothing kernel width ' + STRTRIM(smoothing_resolution, 2)
        output = INPAINT_NANS(output, SMOOTHING_RESOLUTION = smoothing_resolution, COUNT=count)
        PRINT_TEXT2, event, 'Numerical inpainting completed in ' + STRTRIM(count, 2) + ' iterations.'
      ENDIF
      ;
      ;Add the masking level, constituent file names, and background levels to the supersky header
      ;
      MKHDR, header, output
      ;Add the masking level to the header
      SXADDPAR, header, 'MASKMAG', maskMag
      FOR fileInd = 0, n_hwp_files-1 DO BEGIN
        ;Grab the basename of the constituent HWP files
        thisFile = FILE_BASENAME(hwp_imageFiles[fileInd])
        ;Add them to the header
        SXADDPAR, header, STRING(fileInd+1, FORMAT='("FILE",I02)'), thisFile
        ;Add background levels to the headers
        SXADDPAR, header, STRING(fileInd+1, FORMAT='("BKG",I02)'), image_mean[fileInd]
        ;Add noise levels to the headers
        SXADDPAR, header, STRING(fileInd+1, FORMAT='("SIG",I02)'), image_noise[fileInd]
      ENDFOR
      ;
      ;Write the supersky image to disk
      ;
      out_path = groupStruc.analysis_dir + 'S3_Astrometry' + PATH_SEP() + 'Supersky_flats' + PATH_SEP() + $
        STRING(group_number, this_hwp, FORMAT='("group",I02,"_HWP",I04)') + '_SuperSky.fits'
      WRITEFITS, out_path, output, header
      ;
      ;Display the output image to the user
      ;
      SKY, output,  flatMode, flatNoise
      titleStr = STRING((ihwp+1), FORMAT='("Supersky for HWP ", I2)')
      TVIM, output, range = flatMode + [-3,+5]*flatNoise, TITLE = titleStr
      WAIT, 0.05
      ;; ***
      ;; These lines of code will save several diagnostic images for debugging purposes
      ;; ***
      ; out_path = 'supersky_images' + PATH_SEP() + $
      ;   STRING(group_number, this_hwp, FORMAT='("group",I02,"_HWP",I04)') + '_pixelSampling.fits'
      ; writefits, out_path, sample_image
      ; out_path = 'supersky_images' + PATH_SEP() + $
      ;   STRING(group_number, this_hwp, FORMAT='("group",I02,"_HWP",I04)') + '_anomalousPixels.fits'
      ; writefits, out_path, bad_image
    ENDIF ELSE BEGIN
      PRINT, "  Found no images with this HWP"
    ENDELSE
  ENDFOR
  ;
END

PRO S3B_REDRAW_MASK, event
  ;This event manager will redraw the display 2MASS image with a masking contour overplotted

  ;Grab the group structure  
  tlb_wid    = WIDGET_INFO(event.top, FIND_BY_UNAME='WID_BASE')       ;Retrieve the TLB widget ID
  displayWID = WIDGET_INFO(event.top, FIND_BY_UNAME='IMAGE_DISPLAY_WINDOW')
  threshWID  = WIDGET_INFO(event.top, FIND_BY_UNAME='S3B_MASKING_THRESHOLD')
  WIDGET_CONTROL, tlb_wid, GET_UVALUE=groupStruc                      ;Retrieve the group summary structure
  
  ;Construct path to mask information
  S2_path    = groupStruc.analysis_dir + 'S2_Ski_Jump_Fixes'
  maskPath   = S2_path + PATH_SEP() + 'Masking_files'
  maskfile   = maskPath + PATH_SEP() + 'maskInfo.dat'
  
  ;Read in the mask information file and extract the mask (RA, Dec) center
  maskHeader = READHEAD(maskFile)
  RA_cen     = SXPAR(maskHeader, 'RA_MASK')
  Dec_cen    = SXPAR(maskHeader, 'Dec_MASK')
  
  ;Compute star locations for an overplot
  EXTAST,  groupStruc.displayHeader, displayAstr                      ;Grab the astrometry from the 2MASS image
  AD2XY, groupStruc.starInfo.RAJ2000, groupStruc.starInfo.DEJ2000, $
    displayAstr, xStarPlot, yStarPlot
  AD2XY, RA_cen, Dec_cen, displayAstr, gal_xcen, gal_ycen
   
  ;Retrieve masking level from widget
  WIDGET_CONTROL, threshWID, GET_VALUE = maskThresh
  WIDGET_CONTROL, threshWID, SET_VALUE = maskThresh
  
  ;Compute masking level using display image header info
  IF groupStruc.NIRband EQ 'H' THEN BEGIN
    zpMag = SXPAR(groupStruc.displayHeader, 'HMAGZP')
  ENDIF ELSE IF groupStruc.NIRband EQ 'Ks' THEN BEGIN
    zpMag = SXPAR(groupStruc.displayHeader, 'KMAGZP')
  ENDIF
  maskThresh = 10.0^(-0.4*(maskThresh - zpMag))

  ;Compute actual mask using that level
  sz        = SIZE(groupStruc.displayImage, /DIMENSIONS)
  this_mask = GENERATE_MASK(event, sz[0], sz[1], gal_xcen, gal_ycen, 1.0, maskThresh)
  
  ;Reset the main image display
  WIDGET_CONTROL, displayWID, GET_VALUE=windowIndex                   ;Retrieve display window WID
  WSET, windowIndex                                                   ;Set plot window WID
  SKY, groupStruc.displayImage, skyMode, skyNoise, /SILENT
  TVIM, groupStruc.displayImage, RANGE = skyMode + [-3, +25]*skyNoise;Reset image display and overplot stars
  OPLOT, xStarPlot, yStarPlot, PSYM = 6, COLOR = RGB_TO_DECOMPOSED([0,255,0])
  
  ;Overplot a masking contour
  CONTOUR, this_mask, LEVELS = [0.5], /OVERPLOT
END

PRO S3B_GENERATE_SUPERSKY, event

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
    
    ;Update group progress
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
      groupS3files  = groupStruc.analysis_dir + $
        'S3_Astrometry' + PATH_SEP() + FILE_BASENAME(groupBDPfiles, '.FITS') + '.head'
      S2files       = groupStruc.analysis_dir + $
        'S2_Ski_Jump_Fixes' + PATH_SEP() + FILE_BASENAME(groupBDPfiles)

      ;Test for the existence of a ski-jump file,
      ;and restrict superskies to non-ski-jump files
      goodFileInds  = WHERE(~FILE_TEST(S2files), numBad)
      groupBDPfiles = groupBDPfiles[goodFileInds]
      groupS3files  = groupS3files[goodFileInds]
    ENDIF ELSE CONTINUE

    ;Apply the supersky procedure to THIS group
    SUPERSKY_GROUP, event, groupBDPfiles, groupS3files, RA_cen, Dec_cen, $
      GROUP_NUMBER=(i+1)
  
  ENDFOR

;  UPDATE_PROGRESSBAR, imageProgressBarWID, 100E, /PERCENTAGE
  PRINT_TEXT2, event, 'Supersky procedure complete!'

END

PRO S3B_REPAIR_SUPERSKY, event
  ;This procedure will allow the user to make repairs to individual supersky files

  PRINT, 'NOT YET DONE.'
END

PRO S3B_SUBTRACT_SUPERSKY, event
  ;This procedure will
  ;1) read in the edited supersky files,
  ;2) smooth with a "Lee Filter",
  ;3) normalize/subtract sky from constituent images
  ;4) save flattened images to disk
  
  ;Start by retrieving any necessary information
  tlb_wid = WIDGET_INFO(event.top, FIND_BY_UNAME='WID_BASE')          ;Retrieve the TLB widget ID
  WIDGET_CONTROL, tlb_wid, GET_UVALUE=groupStruc                      ;Retrieve the group summary structure
  
  groupProgressBarWID = WIDGET_INFO(event.top, FIND_BY_UNAME='GROUP_PROGRESS_BAR')
  imageProgressBarWID = WIDGET_INFO(event.top, FIND_BY_UNAME='IMAGE_PROGRESS_BAR')

  PRINT_TEXT2, event, 'Starting supersky subtraction procedure'
  
  ;Costruct some basic path strings
  S3A_path   = groupStruc.analysis_dir + 'S3_Astrometry'
  S3B_path   = S3A_PATH + PATH_SEP() + 'Supersky_flats'
  S2_path    = groupStruc.analysis_dir + 'S2_Ski_Jump_Fixes'
  ;
  ; store the default Mimir image size
  ;
  nx = 1024
  ny = 1026
  
  ;Count the number of groups for which to build supersky images
  numGoodGroups = TOTAL(groupStruc.groupFlags)
  groupCount    = 0
  
  ;Loop through each group in the PPOL project
  FOR iGroup = 0, groupStruc.numGroups - 1 DO BEGIN
    ;Skip any groups with groupFlags[iGroup]=0
    IF groupStruc.groupFlags[iGroup] EQ 0 THEN CONTINUE
    
    ;Update group progress
    groupCount++
    groupProgString = STRING(groupCount, numGoodGroups, FORMAT='("Group ",I2," of ",I2)')
    UPDATE_PROGRESSBAR, groupProgressBarWID, 100*FLOAT(iGroup+1)/numGoodGroups, DISPLAY_MESSAGE=groupProgString
    WAIT, 0.05

    ;Grab the fileList for this group
    ;Count the number of files in this group with good astrometry    
    goodFiles     = WHERE(groupStruc.astroFlags[iGroup,*] EQ 1 $
                      OR  groupStruc.astroFlags[iGroup,*] EQ 2 $
                      AND groupStruc.groupImages[iGroup,*] NE '', numGood)

    ;Check that at least one good file was found
    IF numGood GT 0 THEN BEGIN
      ;Build the filenames for all relevant files
      groupBDPfiles = REFORM(groupStruc.groupImages[iGroup,goodFiles])
      groupS3files  = S3A_path + PATH_SEP() + FILE_BASENAME(groupBDPfiles, '.FITS') + '.head'
      S2files       = S2_path + PATH_SEP() + FILE_BASENAME(groupBDPfiles)

      ;Test for the existence of a ski-jump file,
      ;and restrict superskies to non-ski-jump files
      goodFileInds  = WHERE(~FILE_TEST(S2files), numGood)
      IF numGood GT 0 THEN BEGIN
        groupBDPfiles = groupBDPfiles[goodFileInds]
        groupS3files  = groupS3files[goodFileInds]
      ENDIF
    ENDIF ELSE CONTINUE

    ;Parse the HWP values for this group's BDP files
    HWP   = ROUND(10*PARSE_HWPS(groupS3files, UNIQ_HWPS = hwp_uniq))
    n_hwp = N_ELEMENTS(hwp_uniq)
    
    FOR ihwp = 0, n_hwp - 1 DO BEGIN
      ;Update the HWP grouping progress bar
      imageStr = STRING(FORMAT='("HWP ",I2," of ",I2)', (ihwp+1), n_hwp)
      UPDATE_PROGRESSBAR, imageProgressBarWID, $                    ;Update the progress bar to show the latest progress
        100*FLOAT(ihwp+1)/FLOAT(n_hwp), DISPLAY_MESSAGE = imageStr
      WAIT, 0.05
      
      ;Retrieve the unique HWP value
      this_hwp = ROUND(10*hwp_uniq[ihwp])
      
      ;Build the filename for that supersky file
      superskyFile = S3B_path + PATH_SEP() + $
        STRING(iGroup + 1, this_hwp, FORMAT='("group",I02,"_HWP",I04)') + '_SuperSky.fits'

      ;Grab the indices associated with this HWP value
      ind = WHERE(HWP eq this_hwp, hwp_count)

      ;Test for file constituents and...
      ;Test for the presence of the expected supersky flat
      IF(hwp_count GT 0) AND FILE_TEST(superskyFile) THEN BEGIN
        ;Grab the related BDP files and construct header and output paths
        HWPimgFiles  = groupBDPfiles[ind]
        HWPheadFiles = groupS3files[ind]
        outputPaths  = S3A_path + PATH_SEP() + FILE_BASENAME(HWPimgFiles)

        PRINT, "For HWP angle = ", this_hwp
        PRINT, "Found ", hwp_count, " images with this HWP"

        ;Read in the supersky
        superskyImg = READFITS(superskyFile, superskyHeader)

        ;Estimate the supersky noise for use in the LEE-FILTER
        SKY, superskyImg, flatMode, flatNoise, /SILENT
        superskyImg1     = LEEFILT(superskyImg, 2, 1.5*flatnoise, /EXACT)
        superskyMedStrip = MEDIAN(superskyImg[*,500:520], 5)

        ;Replace middle rows with Median filtered values
        superskyImg1[*,510:516] = superskyMedStrip[*, 10:16]
        
        ;Grab the filenames and background values stored in supersky
        constituentFiles = SXPAR(superskyHeader, 'FILE*')
        constituentBkgs  = SXPAR(superskyHeader, 'BKG*')
        constituentSigs  = SXPAR(superskyHeader, 'SIG*')
        
        ;Loop through the HWP files
        FOR k = 0, hwp_count - 1 DO BEGIN
          ;Match the current HWP file to its constituent file
          imgInd = WHERE(FILE_BASENAME(HWPimgFiles[k]) EQ constituentFiles, numMatch)
          IF numMatch EQ 1 THEN BEGIN
            ;Read in the constituent HWP file and background level
            thisImg  = READFITS(HWPimgFiles[k])
            thisHead = READHEAD(HWPheadFiles[k])
            thisBkg  = (constituentBkgs[imgInd])[0]
            thisSig  = (constituentSigs[imgInd])[0]
            ;
            ;Clean up the BDP image
            ;
            thisImg = MARK_LOCAL_OUTLIERS(thisImg, thisSig, SIG_CUT = 4.0)
            thisImg = MODEL_BAD_PIXELS(thisImg)
            ;
            ;Apply supersknormalization
            ;(several possible methods listed)
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
            tmpArr  = thisImg/thisBkg
            tmpArr -= superskyImg1
            tmpArr += 1.0
            tmpArr *= thisBkg
            ;
            ; replace edges with "bad" value
            ;
            tmpArr[0:8,*]       = -1E6
            tmpArr[nx-9:nx-1,*] = -1E6
            tmpArr[*,0:8]       = -1E6
            tmpArr[*,ny-9:ny-1] = -1E6
            ;
            ; and add info about supersky by this program
            ;
            SXADDPAR, thisHead, "HISTORY", 'Background flattened with PEGS_POL + SUPERSKY_GROUPS.pro'
            histStr = '  using sky scaled subtraction on ' + GETENV('COMPUTERNAME') + ' by ' + GETENV('USERNAME') + ' at ' + SYSTIME()
            SXADDPAR, thisHead, "HISTORY", histStr
            ;
            ; write final product to disk
            ;
            WRITEFITS, outputPaths[k], tmpArr, thisHead
          ENDIF ELSE BEGIN
            PRINT, 'No unique match was found for file ', FILE_BASENAME(HWPimgFiles[k])
            STOP
          ENDELSE
        ENDFOR
      ENDIF
    ENDFOR
  ENDFOR
  ;
  PRINT_TEXT2, event, 'Supersky subtraction procedure complete!'
  ;
END