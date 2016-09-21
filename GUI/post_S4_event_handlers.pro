PRO S4_SELECT_INPUT_IMAGES, event


  IF event.select THEN BEGIN
    originalImagesWID = WIDGET_INFO(event.top, FIND_BY_UNAME='S3_ORIGINAL_IMAGES')
    rotatedImagesWID  = WIDGET_INFO(event.top, FIND_BY_UNAME='S3_ROTATED_IMAGES')

    IF (event.id EQ originalImagesWID) AND event.select THEN BEGIN
      WIDGET_CONTROL, event.handler, SET_UVALUE=0
      PRINT_TEXT2, event, 'Processing will by applied to the unrotated images.'
    ENDIF ELSE IF (event.id EQ rotatedImagesWID) AND event.select THEN BEGIN
      WIDGET_CONTROL, event.handler, SET_UVALUE=1
      PRINT_TEXT2, event, 'Processing will by applied to the rotated images.'
    ENDIF
  ENDIF
END


PRO S4_SELECT_PROCESSING_METHOD, event

  WIDGET_CONTROL, event.ID, SET_UVALUE=event.tab

END

PRO S4_SET_REBIN_PIXELS, event

  WIDGET_CONTROL, event.TOP, GET_UVALUE=groupStruc                    ;Retrieve the group structure
  WIDGET_CONTROL, event.ID, GET_VALUE=rebinPix                        ;Retrieve the rebining scale

  rebinPix = FIX(rebinPix)                                            ;Convert the string into an integer
  okRange  = (rebinPix GE 1) AND (rebinPix LE 16)                     ;Test if the supplied value is usable
  IF ~okRange THEN BEGIN
    PRINT_TEXT2, event, 'Must select a rebin value between 1 and 16'
    RETURN
  ENDIF ELSE BEGIN                                                    ;If it is usable, then compute the new plate scale
    newPlateScaleWID = WIDGET_INFO(event.TOP, FIND_BY_UNAME='NEW_PLATE_SCALE')
    newPlateScale    = STRING(groupStruc.finalPlateScale*(FLOAT(rebinPix))[0], FORMAT='(F8.6)')
    WIDGET_CONTROL, newPlateScaleWID, SET_VALUE = newPlateScale
  ENDELSE
END

PRO S4_SET_SMALLEST_MESH_BIN, event

  WIDGET_CONTROL, event.ID, GET_VALUE=smallestRebin                   ;Retrieve the lowest re-binning level
  
  smallestRebin = FIX(smallestRebin)                                  ;Convert the string into an integer
  
  WIDGET_CONTROL, event.ID, SET_UVALUE=smallestRebin                  ;Store this parameter in the UVALUE of the widget
  
  printStr = STRING(smallestRebin, FORMAT='("Polarization map will have a minimum binning of ",I1)')
  PRINT_TEXT2, event, printStr
  
END

PRO S4_SET_NUMBER_REBIN_LEVELS, event

  smallestRebinWID = WIDGET_INFO(event.TOP, FIND_BY_UNAME='SMALLEST_MESH_BIN')
  WIDGET_CONTROL, smallestRebinWID, GET_VALUE=smallestRebin
  WIDGET_CONTROL, event.ID, GET_VALUE=numRebinLevels                  ;Retrieve the rebining scale
  
  smallestRebin  = FIX(smallestRebin)                                 ;Convert the string into an integer
  numRebinLevels = FIX(numRebinLevels)                                ;Convert the string into an integer
  
  WIDGET_CONTROL, event.ID, SET_UVALUE=numRebinLevels                 ;Store this parameter in the UVALUE of the widget
  
  maximumRebin   = smallestRebin*(2^(numRebinLevels - 1))

  printStr = STRING(maximumRebin, FORMAT='("Polarization map will have a maximum binning of ",I2)')
  PRINT_TEXT2, event, printStr
  
END

PRO S4_SET_MINIMUM_MESH_SNR, event

  WIDGET_CONTROL, event.ID, GET_VALUE=minSNR                          ;Retrieve the rebining scale
  
  minSNR = FLOAT(minSNR)                                              ;Convert the string into an number
  
  WIDGET_CONTROL, event.ID, SET_UVALUE=minSNR                         ;Store this parameter in the UVALUE of the widget

  printStr = STRING(minSNR, FORMAT='("Polarization map will have a minimum SNR of ",F4.1)')
  PRINT_TEXT2, event, printStr

END

PRO S4_START_PROCESSING, event

  methodTabWID    = WIDGET_INFO(event.top, FIND_BY_UNAME='S3_METHOD_TABS')
  inputImagesWID  = WIDGET_INFO(event.top, FIND_BY_UNAME='S3_SELECT_INPUT_IMAGES')
  imageProgBarWID = WIDGET_INFO(event.top, FIND_BY_UNAME='IMAGE_PROGRESS_BAR')
  WIDGET_CONTROL, methodTabWID, GET_UVALUE=selectedMethod             ;Retrieve the selected method
  WIDGET_CONTROL, event.top, GET_UVALUE=groupStruc                    ;Retrieve the group structure
  WIDGET_CONTROL, inputImagesWID, GET_UVALUE=inputImages
  
  ;Inform the user which method has been selected
  methodName = ['integer rebinning', 'adaptive mesh averaging', 'Gaussian smoothing with Nyquist rebinning']
  PRINT_TEXT2, event, 'Computing polarization maps using ' + methodName[selectedMethod]
  
  ;Define the paths in which to search for the input images and file masks, etc...
  inputDir   = groupStruc.analysis_dir + 'S11B_Combined_Images' + PATH_SEP()
  filePrefix = groupStruc.NIRband + 'band_'
  IF inputImages EQ 0 THEN fileSuffix = '.fits' $
    ELSE IF inputImages EQ 1 THEN fileSuffix = 'rot.fits'

  maskDir    = groupStruc.analysis_dir + 'S2_Ski_Jump_Fixes' + PATH_SEP() + 'Masking_files' + PATH_SEP()
  
  CASE selectedMethod OF
    
    ;****************************Rebin using integer pixel rebinning****************************
    0: BEGIN
      rebinPixWID = WIDGET_INFO(event.top, FIND_BY_UNAME='REBIN_PIXELS')
      WIDGET_CONTROL, rebinPixWID, GET_VALUE=rebinStr                 ;Read in the rebin pixel value
      rebinPix = FIX(rebinStr[0])                                     ;Convert rebin value into pixels

      IF rebinPix EQ 0 THEN BEGIN                                     ;Check that the rebin value has been set
        PRINT_TEXT2, event, 'You need to set a rebin pixel value'
        RETURN
      ENDIF
      
;      galMask = READFITS(maskDir + 'galMask' + fileSuffix, maskHeader)        ;Read in the mask image
      Iheader = HEADFITS(inputDir + filePrefix + 'I'  + fileSuffix )          ;Read in the intensity header
      Uimg    = READFITS(inputDir + filePrefix + 'U'  + fileSuffix, Uheader)  ;Read in the stokes images
      Qimg    = READFITS(inputDir + filePrefix + 'Q'  + fileSuffix, Qheader)
      sUimg   = READFITS(inputDir + filePrefix + 'sU' + fileSuffix, sUheader)
      sQimg   = READFITS(inputDir + filePrefix + 'sQ' + fileSuffix, sQheader)
      
      EXTAST, Iheader, astr                                           ;Extract the intensity image astrometry to use for all other images

      nx      = astr.naxis[0]                                         ;Save the image size
      ny      = astr.naxis[1]                                         ;All the images OUGHT to be the same size
      
      maskPath = groupStruc.analysis_dir + PATH_SEP() + $
        'S2_Ski_Jump_Fixes' + PATH_SEP() + $
        'Masking_files' + PATH_SEP() + $
        'maskInfo.dat'
      maskInfo = READHEAD(maskPath)
      AD2XY, SXPAR(maskInfo, 'RA_MASK'), SXPAR(maskInfo, 'DEC_MASK'), astr, xCen, yCen
      plateScale = groupStruc.finalPlateScale
      skynoise   = SXPAR(groupStruc.displayHeader, 'SIGMA')
      galMask    = GENERATE_MASK(event, nx, ny, xCen, yCen, plateScale, 2.5*skynoise, /ROTATED)
      
;      ;Pad the galaxy mask edges
;      rtPad   = nx - 1024                                             ;Compute how much data to pad on the right
;      tpPad   = ny - 1026                                             ;Compute how much data to pad on the left
;      galMask = [galMask, FLTARR(rtPad,1026)]                         ;Pad the right side of the mask
;      galMask = [[galMask], [FLTARR(nx,tpPad)]]                       ;Pad the top side of the mask
;      
;      ;Shift the mask to the correct position
;      galRA   = SXPAR(maskHeader, 'RA_CEN')                           ;Extract the galaxy RA center
;      galDec  = SXPAR(maskHeader, 'DEC_CEN')                          ;Extract the galaxy Dec center
;      AD2XY, galRA, galDec, astr, galX, galY                          ;Compute the galaxy (X,Y) in the intensity image
;      maskCen  = [512,513]                                            ;Galaxy center in the image mask image
;      xShift   = galX - maskCen[0]                                    ;Compute the amount to shift right or left
;      yShift   = galY - maskCen[1]                                    ;Compute the amount to shift up or down
;      galMask  = SHIFT(galMask, xShift, yShift)                       ;Shift the galaxy mask
      maskInds = WHERE(~galMask)                                      ;Find all the masked indices (galMask = 1 on galaxy)
      
      ;Now that the mask has been shifted, mask out all U and Q values
      PRINT_TEXT2, event, 'Masking points outside the galaxy and preparing to rebin.'
      Uimg[maskInds]  = !VALUES.F_NAN
      Qimg[maskInds]  = !VALUES.F_NAN
      SUimg[maskInds] = !VALUES.F_NAN
      sQimg[maskInds] = !VALUES.F_NAN
      
;      Uimg   = galMask*Uimg
;      Qimg   = galMask*Qimg
;      sUimg  = galMask*sUimg
;      sQimg  = galMask*sQimg

      rtTrim = nx MOD rebinPix                                        ;Compute how much needs to be trimmed from the edges
      tpTrim = ny MOD rebinPix
      
      Uimg   = Uimg[0:nx-rtTrim-1,0:ny-tpTrim-1]                      ;Actually trim the edges of the images
      Qimg   = Qimg[0:nx-rtTrim-1,0:ny-tpTrim-1]
      sUimg  = sUimg[0:nx-rtTrim-1,0:ny-tpTrim-1]
      sQimg  = sQimg[0:nx-rtTrim-1,0:ny-tpTrim-1]

      new_nx = (nx - rtTrim)/rebinPix                                 ;Compute the new image sizes
      new_ny = (ny - tpTrim)/rebinPix
      
      ;Compute the new astrometry to be written to the polarization maps
      PRINT_TEXT2, event, 'Computing astrometry of rebinned image.'
      crpix1     = new_nx/2
      crpix2     = new_ny/2
      old_crpix1 = crpix1*rebinPix - 1                                ;Subtracting one from the pixels
      old_crpix2 = crpix2*rebinPix - 1                                ;accounts for the IDL to FITS diference
      XY2AD, (old_crpix1-1), (old_crpix2-1), astr, crval1, crval2
      
      ;Insert the updated values into the astrometry
      astr.naxis = [new_nx, new_ny]
      astr.crpix = [crpix1, crpix2]
      astr.crval = [crval1, crval2]
      astr.cd   *= rebinPix
      
      SXDELPAR, Iheader, 'STOKES'                                     ;The output images won't be stokes images
      PUTAST, Iheader, astr                                           ;Update the header with the new astrometry
      
      ;Compute the weighted average
      ;These images are the inverse variance weights to apply in the averaging
      Uweight = 1/sUimg^2
      Qweight = 1/sQimg^2
      
      ;Compute the rebinned weight values
      UweightBinned = SMART_REBIN(Uweight, new_nx, new_ny)
      QweightBinned = SMART_REBIN(Qweight, new_nx, new_ny)
      
      ;Weight the image, average, and then re-normalize by the sum of the weights
      Urebin  = SMART_REBIN(Uweight*Uimg, new_nx, new_ny)/UweightBinned
      Qrebin  = SMART_REBIN(Qweight*Qimg, new_nx, new_ny)/QweightBinned

      ;Compute the uncertainty images based on the sum of the inverse variance weights
      sUrebin = 1/(rebinPix*SQRT(UweightBinned))
      sQrebin = 1/(rebinPix*SQRT(QweightBinned))
      
      ;Now that U and Q have been averaged, compute the polarization maps
      ;************************************************************************
      ;*********** THIS SHOULD BE REPLACED BY THE BEYSIAN TECHNIQUE ***********
      ;************************************************************************
      Pmap     = SQRT(Urebin^2 + Qrebin^2)                            ;An initial polarization map
      sPmap    = SQRT((Urebin*sUrebin)^2 + (Qrebin*sQrebin)^2)/Pmap   ;Compute the uncertainty in the map
      nullInds = WHERE(Pmap LT sPmap, numNull)                        ;Determine where S/N < 1
      IF numNull GT 0 THEN Pmap[nullInds] = sPmap[nullInds]           ;Set upper limits where appropriate
      Pmap = SQRT(Pmap^2 - sPmap^2)                                   ;De-bias in an attempt to account for uncertainties

      PAmap    = (720 + 0.5*ATAN(Urebin,Qrebin)*!RADEG) MOD 180       ;Now calculate the position angle map and its uncertainty

      noZeroPAs = WHERE(PAmap NE 0, noZeroCount)
      IF noZeroCount GT 0 THEN BEGIN                                  ;Figure out if the PA values need to be redistributed
        mean_std_PA = MEDIAN_FILTERED_MEAN(PAmap[noZeroPAs])
        IF ((ROUND(mean_std_PA[0]/90.0))*90.0 EQ 180.0) $             ;If PAs near to 0 or 180, then correct
        OR ((ROUND(mean_std_PA[0]/90.0))*90.0 EQ   0.0) THEN BEGIN
          wrapPAs = WHERE(PAmap GT 90.0, PAcount)                     ;Find the Pas that need correction
          IF PAcount GT 0 THEN PAmap[wrapPAs] -= 180.0                ;Reset those PAs onto the negative axis
        ENDIF
      ENDIF

      sPAmap  = 0.5*!RADEG*(sPmap/Pmap)                               ;I previously found that S/N sets PA uncertainty

      ;**** CALCULUS PERFORMED ON 2/2/2015 INDICATES A DIFFERENT FORMULA ****
      ;sPAmap = 0.5*!RADEG*SQRT((Urebin*sQrebin)^2 + (Qrebin*sUrebin)^2)/Pmap
      ;**********************************************************************


      PRINT_TEXT2, event, 'Saving polarization maps to disk.'
      rebinStr = '_'+rebinStr + 'x' + rebinStr
      WRITEFITS, inputDir + filePrefix + 'P'   + rebinStr + fileSuffix , Pmap,  Iheader
      WRITEFITS, inputDir + filePrefix + 'sP'  + rebinStr + fileSuffix, sPmap,  Iheader
      WRITEFITS, inputDir + filePrefix + 'PA'  + rebinStr + fileSuffix, PAmap,  Iheader
      WRITEFITS, inputDir + filePrefix + 'sPA' + rebinStr + fileSuffix, sPAmap, Iheader

    END
    
    ;****************************Use an adaptive mesh averaging****************************
    1: BEGIN
      smallBinWID     = WIDGET_INFO(event.top, FIND_BY_UNAME='SMALLEST_MESH_BIN')
      numRebinWID     = WIDGET_INFO(event.top, FIND_BY_UNAME='NUMBER_REBIN_LEVELS')
      minSNR_WID      = WIDGET_INFO(event.top, FIND_BY_UNAME='MINIMUM_MESH_SNR')
      WIDGET_CONTROL, smallBinWID, GET_UVALUE=smallestBin
      WIDGET_CONTROL, numRebinWID, GET_UVALUE=numRebinLevels
      WIDGET_CONTROL, minSNR_WID, GET_UVALUE=min_SNR                  ;Grab the user supplied SNR threshold
      smallestBin    = smallestBin[0]
      numRebinLevels = numRebinLevels[0]
      min_SNR        = min_SNR[0]

      Iheader = HEADFITS(inputDir + filePrefix + 'I'  + fileSuffix)   ;Read in the intensity header
      Uimg    = READFITS(inputDir + filePrefix + 'U'  + fileSuffix, Uheader)  ;Read in the stokes images
      Qimg    = READFITS(inputDir + filePrefix + 'Q'  + fileSuffix, Qheader)
      sUimg   = READFITS(inputDir + filePrefix + 'sU' + fileSuffix, sUheader)
      sQimg   = READFITS(inputDir + filePrefix + 'sQ' + fileSuffix, sQheader)

      EXTAST, Iheader, astr                                           ;Extract the intensity image astrometry to use for all other images
      
      nx      = astr.naxis[0]                                         ;Save the image size
      ny      = astr.naxis[1]                                         ;All the images OUGHT to be the same size

      
;      galMask = READFITS(maskDir + 'galMask' + fileSuffix, maskHeader);Read in the mask image
      ;****** THIS NEW METHOD SHOULD BE IMPLEMENTED EVERYWHERE,
      ;****** AND THE ACTUAL PLATE SCALE SHOULD BE USED.
      maskPath = groupStruc.analysis_dir + PATH_SEP() + $
                 'S2_Ski_Jump_Fixes' + PATH_SEP() + $
                 'Masking_files' + PATH_SEP() + $
                 'maskInfo.dat'
      maskInfo = READHEAD(maskPath)
      AD2XY, SXPAR(maskInfo, 'RA_MASK'), SXPAR(maskInfo, 'DEC_MASK'), astr, xCen, yCen
      plateScale = groupStruc.finalPlateScale
      skynoise   = SXPAR(groupStruc.displayHeader, 'SIGMA')
      galMask    = GENERATE_MASK(event, nx, ny, xCen, yCen, plateScale, 2.5*skynoise, /ROTATED)
            
      ;Pad the galaxy mask edges
;      maskSZ  = SIZE(galMask, /DIMENSIONS)
;      rtPad   = nx - maskSZ[0]                                        ;Compute how much data to pad on the right
;      tpPad   = ny - maskSZ[1]                                        ;Compute how much data to pad on the left
;      galMask = [galMask, FLTARR(rtPad,maskSZ[1])]                    ;Pad the right side of the mask
;      galMask = [[galMask], [FLTARR(nx,tpPad)]]                       ;Pad the top side of the mask
      
      ;Shift the mask to the correct position
;      galRA   = SXPAR(maskHeader, 'RA_CEN')                           ;Extract the galaxy RA center
;      galDec  = SXPAR(maskHeader, 'DEC_CEN')                          ;Extract the galaxy Dec center
;      AD2XY, galRA, galDec, astr, galX, galY                          ;Compute the galaxy (X,Y) in the intensity image
;      maskCen  = 0.5*maskSZ                                           ;Galaxy center in the image mask image
;      xShift   = ROUND(galX - maskCen[0])                             ;Compute the amount to shift right or left
;      yShift   = ROUND(galY - maskCen[1])                             ;Compute the amount to shift up or down
;      galMask  = SMART_SHIFT(galMask, xShift, yShift)                 ;Shift the galaxy mask
      maskInds = WHERE(~galMask)                                      ;Find all the masked indices (galMask = 1 on galaxy)

      ;Now that the mask has been shifted, mask out all U and Q values
      PRINT_TEXT2, event, 'Masking points outside the galaxy and preparing to rebin.'
      Uimg[maskInds]  = !VALUES.F_NAN
      Qimg[maskInds]  = !VALUES.F_NAN
      SUimg[maskInds] = !VALUES.F_NAN
      sQimg[maskInds] = !VALUES.F_NAN
      
      rebinPix = REVERSE(smallestBin*(2^(INDGEN(numRebinLevels))))    ;Establish the range of re-binning to be used (e.g. 2--16 pixels)

      maxRebin = MAX(rebinPix)
      rtTrim   = nx MOD maxRebin                                      ;Compute how much needs to be trimmed from the edges
      tpTrim   = ny MOD maxRebin
      nx1      = nx - rtTrim
      ny1      = ny - tpTrim
      
      Uimg     = Uimg[0:nx1-1,0:ny1-1]                                ;Actually trim the edges of the images
      Qimg     = Qimg[0:nx1-1,0:ny1-1]
      sUimg    = sUimg[0:nx1-1,0:ny1-1]
      sQimg    = sQimg[0:nx1-1,0:ny1-1]
      MAKE_2D, FINDGEN(nx1), FINDGEN(ny1), xx1, yy1                   ;Build 2D maps of the x and y pixel coordinates
      
      ;**** IT MAY EVENTUALLY BE WISE TO ADD AN EXTERNAL LOOP 
      ;**** TO STEP THROUGH AT LEAST FOUR DIFFERENT "DITHERS"
      ;**** IN HOW THE GRIDS ARE LAID DOWN ON THE DATA.
      
      
      ;The "polMaps" structure will hold the following data
      ;Each structure tag will contain the results for a separate rebinning level.
      ;Each tag will contain a 6-level floating point array holding the following elements
      ;** polMaps.levelX[i,j,0] = RA values of the (i,j) pixel at binning "levelX"
      ;** polMaps.levelX[i,j,1] = Dec values of the (i,j) pixel at binning "levelX"
      ;** polMaps.levelX[i,j,2] = Polarization percentage of the (i,j) pixel at binning "levelX"
      ;** polMaps.levelX[i,j,3] = sigma-P of the (i,j) pixel at binning "levelX"
      ;** polMaps.levelX[i,j,4] = Position angle of the (i,j) pixel at binning "levelX"
      ;** polMaps.levelX[i,j,5] = sigma-PA of the (i,j) pixel at binning "levelX"
      ;** polMaps.levelX[i,j,6] = SNR of the (i,j) pixel at binning "levelX"
      
      
;      structureLabel = 'level' + STRING(49B)                         ;Create the labels for the polMaps structure
      polMaps = CREATE_STRUCT('level0', $                             ;Create the first level tag for the polMaps structure
                              DBLARR(nx1/rebinPix[0], ny1/rebinPix[0], 7))
      FOR i = 1, numRebinLevels - 1 DO BEGIN                          ;Loop through remaining levels and add them to the structure
        polMaps = CREATE_STRUCT(polMaps, 'level' + STRING(i,FORMAT='(I1)'), $
                                DBLARR(nx1/rebinPix[i], ny1/rebinPix[i], 7))
      ENDFOR

      FOR i = 0, numRebinLevels - 1 DO BEGIN                          ;Now loop through each of the rebinning levels
        new_nx = nx1/rebinPix[i]                                      ;Compute the new image sizes
        new_ny = ny1/rebinPix[i]
        
        ;Compute the weighted average
        ;These images are the inverse variance weights to apply in the averaging
        Uweight = 1/sUimg^2
        Qweight = 1/sQimg^2
        
        ;Compute the rebinned weight values
        UweightBinned = SMART_REBIN(Uweight, new_nx, new_ny)
        QweightBinned = SMART_REBIN(Qweight, new_nx, new_ny)
        
        ;Weight the image, average, and then re-normalize by the sum of the weights
        Urebin  = SMART_REBIN(Uweight*Uimg, new_nx, new_ny)/UweightBinned
        Qrebin  = SMART_REBIN(Qweight*Qimg, new_nx, new_ny)/QweightBinned
        
        ;Compute the *unweighted* centroid position of the averaged pixels
        xxRebin = SMART_REBIN(xx1, new_nx, new_ny)
        yyRebin = SMART_REBIN(yy1, new_nx, new_ny)
        
        ;Compute the uncertainty images based on the sum of the inverse variance weights
        sUrebin = 1/(rebinPix[i]*SQRT(UweightBinned))
        sQrebin = 1/(rebinPix[i]*SQRT(QweightBinned))
        
        ;Now that U and Q have been averaged, compute the polarization maps
        ;************************************************************************
        ;*********** THIS SHOULD BE REPLACED BY THE BEYSIAN TECHNIQUE ***********
        ;************************************************************************
        Pmap1    = SQRT(Urebin^2 + Qrebin^2)                          ;An initial polarization map
        sPmap1   = SQRT((Urebin*sUrebin)^2 + (Qrebin*sQrebin)^2)/Pmap1;Compute the uncertainty in the map
        nullInds = WHERE(Pmap1 LT sPmap1, numNull)                    ;Determine where S/N < 1
        IF numNull GT 0 THEN Pmap1[nullInds] = sPmap1[nullInds]       ;Set upper limits where appropriate
        Pmap1 = SQRT(Pmap1^2 - sPmap1^2)                              ;De-bias in an attempt to account for uncertainties
        
        PAmap1 = (720 + 0.5*ATAN(Urebin,Qrebin)*!RADEG) MOD 180       ;Now calculate the position angle map and its uncertainty
        
        noZeroPAs = WHERE(PAmap1 NE 0, noZeroCount)
        IF noZeroCount GT 0 THEN BEGIN                                ;Figure out if the PA values need to be redistributed
          mean_std_PA = MEDIAN_FILTERED_MEAN(PAmap1[noZeroPAs])
          IF ((ROUND(mean_std_PA[0]/90.0))*90.0 EQ 180.0) $           ;If PAs near to 0 or 180, then correct
            OR ((ROUND(mean_std_PA[0]/90.0))*90.0 EQ   0.0) THEN BEGIN
            wrapPAs = WHERE(PAmap1 GT 90.0, PAcount)                  ;Find the Pas that need correction
            IF PAcount GT 0 THEN PAmap1[wrapPAs] -= 180.0             ;Reset those PAs onto the negative axis
          ENDIF
        ENDIF
        
;        sPAmap1 = 0.5*!RADEG*(sPmap1/Pmap1)                           ;I previously found that S/N sets PA uncertainty
        
        ;**** CALCULUS PERFORMED ON 2/2/2015 INDICATES A DIFFERENT FORMULA ****
        sPAmap1 = 0.5*!RADEG*SQRT((Urebin*sQrebin)^2 + (Qrebin*sUrebin)^2)/Pmap1
        ;**********************************************************************
        
        
        ;Compute the RA and Dec maps for this rebinning level
        ;*******
        crpix1     = new_nx/2
        crpix2     = new_ny/2
;        old_crpix1 = (crpix1-1)*rebinPix[i]                           ;Subtracting one from the pixels
;        old_crpix2 = (crpix2-1)*rebinPix[i]                           ;accounts for the IDL to FITS diference
        old_crpix1 = xxRebin[crpix1,0]
        old_crpix2 = yyRebin[0,crpix2]
        XY2AD, (old_crpix1 - 1), (old_crpix2 - 1), astr, crval1, crval2
        
        ;Insert the updated values into the astrometry
        astr1       = astr                                            ;Reset the astrometry values
        astr1.naxis = [new_nx, new_ny]                                ;Then update them with the newly computed values
        astr1.crpix = [crpix1, crpix2]
        astr1.crval = [crval1, crval2]
;        astr1.cd   *= rebinPix[i]
        
        XY2AD, xxRebin, yyRebin, astr, RAmap, DecMap
        
        tempCube = [[[RAmap]],  $
                    [[DecMap]], $
                    [[Pmap1]],  $
                    [[sPmap1]], $
                    [[PAmap1]], $
                    [[sPAmap1]], $
                    [[Pmap1/sPmap1]]]
        
        polMaps.(i) = tempCube
        
        ;****** I need to figure out how to do this right.
        ;****** I sohuld probably compute ALL rebinning levels and store them in a structure
        ;****** Then I can just recurse through each top-level-pixel to find the finest possible meshing.
        
        ;Loop through the new map and determine if the SNR requirement
        ;has been met at this binning level
;        FOR j = 0, new_nx - 1 DO BEGIN
;          FOR k = 0, new_ny - 1 DO BEGIN
;            IF ~FINITE(Pmap1[j,k]) THEN CONTINUE                      ;Skip NAN pixels
;            this_SNR = Pmap1[j,k]/sPmap1[j,k]                         ;Compute the SNR of this pixel
;            IF this_SNR GE min_SNR THEN BEGIN
;              lf                  = j*rebinPix[i]
;              rt                  = (j+1)*rebinPix[i] - 1
;              bt                  = k*rebinPix[i]
;              tp                  = (k+1)*rebinPix[i] - 1
;              Pmap[lf:rt,bt:tp]   = Pmap1[j,k]                        ;Assign the pixels values for succesful pixels
;              sPmap[lf:rt,bt:tp]  = sPmap1[j,k]
;              PAmap[lf:rt,bt:tp]  = PAmap1[j,k]
;              sPAmap[lf:rt,bt:tp] = sPAmap1[j,k]
;            ENDIF
;          ENDFOR
;        ENDFOR
        
      ENDFOR
      
      
     
      ;Now that polarization maps have been computed for ALL levels of rebinning,
      ;recruse through the levels on each super-pixel to determine
      ;what level of binning should be used in each part of that pixel.
      PRINT_TEXT2, event, 'Computing the ideal meshing'
      finalPolMaps   = DBLARR(nx1, ny1, 7)                            ;Build an array to store the final images

      super_nx  = nx1/maxRebin                                        ;Specify the dimensions of the super-pixel image
      super_ny  = ny1/maxRebin
      totalPts  = TOTAL(FINITE((polMaps.(0))[*,*,2]))
      completed = 0
      UPDATE_PROGRESSBAR, imageProgBarWID, 100E*completed/totalPts, /PERCENTAGE
      FOR j = 0, super_nx - 1 DO BEGIN
        FOR k = 0, super_ny - 1 DO BEGIN
          IF FINITE((polMaps.(0))[j,k,2]) THEN BEGIN                  ;Check if this super-pixel is located in the galaxy
;            ;(Re-)define a "level structure" which will store the success of each pixel and sub-pixels            
            SNR_core = CREATE_STRUCT('level0', (polMaps.(0))[j,k,6])  ;Create a cutout of the relevant SNR map (simplifies indexing)
            FOR i = 1, numRebinLevels - 1 DO BEGIN                    ;Loop through remaining levels and add them to the structure
              scale    = maxRebin/rebinPix[i]
              lf       = j*scale
              rt       = (j+1)*scale - 1
              bt       = k*scale
              tp       = (k+1)*scale - 1
              SNR_core = CREATE_STRUCT(SNR_core, 'level' + STRING(i,FORMAT='(I1)'), $
                                         (polMaps.(i))[lf:rt,bt:tp,6])
            ENDFOR
            rebinLevel = 0                                            ;Start searching at the top
            mesh_tree  = BUILD_MESH_TREE(SNR_core, min_SNR, $         ;This function recursively searches the SNR_core
              numRebinLevels, rebinLevel)                             ;and builds an adaptive-mesh tree for this super-pixel

            rebinLevel = 0                                            ;Start searching at the top
            
            MAP_SUPER_PIXEL, polMaps, j, k, rebinPix, rebinLevel, mesh_tree, $    ;Pass the relevant data to a routine for handling
              finalPolMaps, finalPolValues                            ;This routine will output the final maps and point data
            completed++
          ENDIF ELSE BEGIN                                            ;If it's a null value, then populate with "NAN"
            lf = j*maxRebin
            rt = lf + maxRebin - 1
            bt = k*maxRebin
            tp = bt + maxRebin - 1
            finalPolMaps[lf:rt,bt:tp,*] = !VALUES.D_NAN
          ENDELSE
        ENDFOR
        UPDATE_PROGRESSBAR, imageProgBarWID, 100E*completed/totalPts, /PERCENTAGE
        WAIT, 0.01
      ENDFOR

      UPDATE_PROGRESSBAR, imageProgBarWid, 100E, /PERCENTAGE
      
      PRINT_TEXT2, event, 'Saving polarization maps to disk.'
      rebinStr = '_AMR'
;      WRITEFITS, inputDir + filePrefix + 'RA.fits', FLOAT(finalPolMaps[*,*,0]), Iheader
;      WRITEFITS, inputDir + filePrefix + 'Dec.fits', FLOAT(finalPolMaps[*,*,1]), Iheader
      WRITEFITS, inputDir + filePrefix + 'P'   + rebinStr + fileSuffix, FLOAT(finalPolMaps[*,*,2]), Iheader
      WRITEFITS, inputDir + filePrefix + 'sP'  + rebinStr + fileSuffix, FLOAT(finalPolMaps[*,*,3]), Iheader
      WRITEFITS, inputDir + filePrefix + 'PA'  + rebinStr + fileSuffix, FLOAT(finalPolMaps[*,*,4]), Iheader
      WRITEFITS, inputDir + filePrefix + 'sPA' + rebinStr + fileSuffix, FLOAT(finalPolMaps[*,*,5]), Iheader
      WRITEFITS, inputDir + filePrefix + 'SNR' + rebinStr + fileSuffix, FLOAT(finalPolMaps[*,*,6]), Iheader
      
      goodPolValues = WHERE((finalPolValues[2,*] NE 0) $
                        AND (finalPolValues[6,*] GT min_SNR), numGood)
      IF numGood GT 0 THEN finalPolValues = finalPolValues[*,goodPolValues]
      
      PRINT_TEXT2, event, STRING(numGood, FORMAT='("Successfully found ",I5, " good data points")')
      
      ;First write a file callable by 'READCOL.PRO'
      polCatFile = inputDir + filePrefix + 'polCat.dat'
      OPENW, lun, polCatFile, /GET_LUN
      
      headerStr = [';  INDEX      RA           DEC        P         S_P      PA         S_PA    SNR', $
                   ';--------------------------------------------------------------------------------']
      PRINTF, lun, headerStr
      
      FOR i = 0, N_ELEMENTS(finalPolValues[0,*]) - 1 DO BEGIN
        PRINTF, lun, i, finalPolValues[*,i], $
          FORMAT = '(I8,D15.6,D+12.6,2(F10.5),F+10.5,2(F8.3))'
      ENDFOR
      CLOSE, lun
      
      ;Now write a DS9 region file
      PAs     = REFORM(finalPolValues[4,*]) + 720
      PAs     = (PAs MOD 180)
      deltaXs = 300*REFORM(finalPolValues[2,*])*SIN(PAs*!DTOR)
      deltaYs = 300*REFORM(finalPolValues[2,*])*COS(PAs*!DTOR)
      
      AD2XY, REFORM(finalPolValues[0,*]), REFORM(finalPolValues[1,*]), astr, Xs, Ys
      Xs += (0.5*(smallestBin-1))
      Ys += (0.5*(smallestBin-1))
      x1  = Xs - deltaXs
      x2  = Xs + deltaXs
      y1  = Ys + deltaYs
      y2  = Ys - deltaYs
      
      IF inputImages EQ 0 $
        THEN regionFile = inputDir + filePrefix + 'DS9_regions.reg' ELSE $
        IF inputImages EQ 1 THEN regionFile = inputDir + filePrefix + 'DS9_regions_rot.reg'
      OPENW, lun, regionFile, /GET_LUN
      
      header1 = '# Region file format: DS9 version 4.1'
      header2 = 'global color=green dashlist=8 3 width=3 font="helvetica 10 normal roman" select=1 highlite=1 dash=0 fixed=1 edit=1 move=0 delete=0 include=1 source=1'
      header3 = 'image'
      PRINTF, lun, header1
      PRINTF, lun, header2
      PRINTF, lun, header3
      FOR i = 0, N_ELEMENTS(Xs) - 1 DO BEGIN
        lineString = STRING(x1[i], y1[i], x2[i], y2[i], FORMAT = '("line(",3(F6.1,","),F6.1,")")')
        PRINTF, lun, lineString
      ENDFOR
      
      CLOSE, lun
      
    END
    
    ;****************************Use Gaussian smoothing and Nyquist rebinning****************************
    2: BEGIN
      stop
    END
    
  ENDCASE
  
  PRINT_TEXT2, event, 'Completed polarization mapping'
END