
PRO S1_USE_2BAND_PHOTOMETRY, event
  ;Retrieve WID for path text box
  wS1pathTextBox  = WIDGET_INFO(event.top, FIND_BY_UNAME='S1_2ND_BAND_PATH')
  wS1browseButton = WIDGET_INFO(event.top, FIND_BY_UNAME='S1_BROWSE_FOR_2ND_BAND')
  
  IF event.select THEN BEGIN
    ;Activate the text box and browse button
    WIDGET_CONTROL, wS1pathTextBox, SENSITIVE=1
    WIDGET_CONTROL, wS1browseButton, SENSITIVE=1
  ENDIF ELSE BEGIN
    ;Clear the text box
    WIDGET_CONTROL, wS1pathTextBox, SET_VALUE='PPOL DIRECTORY GOES HERE'
    
    ;Deactivate the text box and browse button
    WIDGET_CONTROL, wS1pathTextBox, SENSITIVE=0
    WIDGET_CONTROL, wS1browseButton, SENSITIVE=0
  ENDELSE
END

PRO S1_BROWSE_FOR_2ND_BAND, event
  ;Retrieve WID for path text box
  wS1pathTextBox = WIDGET_INFO(event.top, FIND_BY_UNAME='S1_2ND_BAND_PATH')
  
  ;Have the user specify where the data is being stored
  dir = DIALOG_PICKFILE(TITLE='Select PPOL directory for 2ND band', /DIRECTORY)
  
  ;Update the text box
  WIDGET_CONTROL, wS1pathTextBox, SET_VALUE=dir
  
END

PRO S1_AVERAGE_STOKES_IMAGES, event
  ; Collect information into a structure
  ; Grab the input directory from the top-level base UVALUE
  tlb_wid          = WIDGET_INFO(event.top, FIND_BY_UNAME='WID_BASE')
  groupProgBarWID  = WIDGET_INFO(event.top, FIND_BY_UNAME='GROUP_PROGRESS_BAR')
  imageProgBarWID  = WIDGET_INFO(event.top, FIND_BY_UNAME='IMAGE_PROGRESS_BAR')
  displayWindowWID = WIDGET_INFO(event.top, FIND_BY_UNAME='IMAGE_DISPLAY_WINDOW')

  ;Retrieve WID for path text box
  wS1pathTextBox  = WIDGET_INFO(event.top, FIND_BY_UNAME='S1_2ND_BAND_PATH')
  wS1browseButton = WIDGET_INFO(event.top, FIND_BY_UNAME='S1_BROWSE_FOR_2ND_BAND')

  ;Get the group structure and display window ID
  WIDGET_CONTROL, tlb_wid, GET_UVALUE=groupStruc
  WIDGET_CONTROL, displayWindowWID, GET_VALUE=displayWindowIndex
  
  UPDATE_PROGRESSBAR, groupProgBarWID, /ERASE                         ;Clear out any previous progress bar status
  UPDATE_PROGRESSBAR, imageProgBarWID, /ERASE
  
  ; Grab the object name from the group structure
  object_name = groupStruc.objectName
 
  ; Begin by simplifying references to the input/output directories, etc...
  outDir  = groupStruc.analysis_dir + 'S11B_Combined_Images' + PATH_SEP()
  IF ~FILE_TEST(outDir, /DIRECTORY) THEN FILE_MKDIR, outDir

  ;Check if 2 band alignment was requested
  wS1pathSensitive = WIDGET_INFO(wS1pathTextBox, /SENSITIVE)

  IF wS1pathSensitive THEN BEGIN
    ;*** Do 2-band alignment ***
    ;Get the proposed path to the second band
    WIDGET_CONTROL, wS1pathTextBox, GET_VALUE = band2analysis_dir
    
    ;Test that this is a DIFFERENT path
    IF band2analysis_dir EQ groupStruc.analysis_dir THEN BEGIN
      PRINT_TEXT2, event, 'Path for 2nd band is same as current analysis directory.'
      RETURN
    ENDIF
    
    ;Test that this is a genuine PPOL path
    testPath = band2analysis_dir + 'S1_Image_Groups_and_Meta_Groups' + PATH_SEP() + 'Group_Summary.sav'
    IF ~FILE_TEST(testPath) THEN BEGIN
      PRINT_TEXT, event, 'This does not appear to be a PPOL directory.'
      RETURN
    ENDIF

    ;Loop through each directory and request 16 HWP images per band
    nFiles     = 32
    nBands     = 2
    bandNames  = []
    fileBases  = []
    HWPfiles   = []
    bandFlags  = []
    startPaths = [groupStruc.analysis_dir, band2analysis_dir]
    FOR iBand = 0, 1 DO BEGIN
      ;Construct the path to background removed HWP images
      thisInDir = startPaths[iBand] + 'S11_Full_Field_Polarimetry' + PATH_SEP()

      done = 0
      WHILE ~done DO BEGIN
        ;Ask the user to provide the list of HWP images to handle.
        theseFiles = DIALOG_PICKFILE(TITLE='Select HWP images to process', $
          PATH = thisInDir, /MULTIPLE_FILES, FILTER = ['*.FIT', '*.FITS'], /FIX_FILTER)
          
        ;Test if 16 HWP were found
        IF N_ELEMENTS(theseFiles) EQ 16 THEN BEGIN
          ;Assume the user selected viable files,
          ;then test if those files are expected to work
          done = 1

          ;Loop through all the basenames and test if they match from the same group
          FOR iFile = 0, N_ELEMENTS(theseFiles) - 1 DO BEGIN
            ;Test if the image is the same size as the previous
            IF iFile EQ 0 THEN BEGIN
              ;Grab the first file name for this group
              fileBase0 = (STRSPLIT(FILE_BASENAME(theseFiles[0]), 'HWP', /EXTRACT, /REGEX))[0]
            ENDIF

            ;Test if this filebase is the same as others
            fileBase1 = (STRSPLIT(FILE_BASENAME(theseFiles[iFile]), 'HWP', /EXTRACT, /REGEX))[0]
            IF fileBase0 NE fileBase1 THEN BEGIN
              PRINT_TEXT2, event, 'These files are not from the same group.'
              done = 0
            ENDIF
          ENDFOR

        ENDIF ELSE IF N_ELEMENTS(theseFiles) EQ 1 AND theseFiles[0] EQ '' THEN BEGIN
          ;This means no files were selected, so return to the main GUI
          RETURN
        ENDIF ELSE BEGIN
          ;The wrong number of files were seleceted, so return to the main GUI
          PRINT_TEXT2, event, 'There should be 16 HWP files.'
          RETURN
        ENDELSE
      ENDWHILE
      ;If code gets here, then an acceptable set of files were selected
      ;Store the file paths and mark the band with which they are associated
      HWPfiles  = [HWPfiles, [theseFiles]]
      bandFlags = [bandFlags, [iBand + INTARR(16)]]
      
      ;Parse which band this is
      thisBand  = (STRMID(SXPAR(HEADFITS(theseFiles[0]), 'FILTNME2'), 0, 1))[0]
      bandNames = [bandNames, [thisBand]]
      
      ;Store the file basename for this band
      fileBases = [fileBases, fileBase0]
    ENDFOR
    
  ENDIF ELSE BEGIN
    ;Do 1-band alignment
    startPaths = [groupStruc.analysis_dir]
    thisInDir  = startPaths[0] + 'S11_Full_Field_Polarimetry' + PATH_SEP()

    ;All 16 images come from the first band, so simply mark them as such
    nFiles    = 16
    nBands    = 1
    bandFlags = INTARR(nFiles)
    
    ;Read in the HWP files
    done = 0
    WHILE ~done DO BEGIN
      ;Ask the user to provide the list of HWP images to handle.
      theseFiles = DIALOG_PICKFILE(TITLE='Select HWP images to process', $
        PATH = thisInDir, /MULTIPLE_FILES, FILTER = ['*.FIT', '*.FITS'], /FIX_FILTER)
        
      ;Test if 16 HWP were found
      IF N_ELEMENTS(theseFiles) EQ 16 THEN BEGIN
        ;Assume the user selected viable files,
        ;then test if those files are expected to work
        done = 1

        ;Loop through all the basenames and test if they match from the same group
        FOR iFile = 0, N_ELEMENTS(theseFiles) - 1 DO BEGIN
          ;Test if the image is the same size as the previous
          IF iFile EQ 0 THEN BEGIN
            ;Grab the first file name for this group
            fileBase0 = (STRSPLIT(FILE_BASENAME(theseFiles[0]), 'HWP', /EXTRACT, /REGEX))[0]
          ENDIF

          ;Test if this filebase is the same as others
          fileBase1 = (STRSPLIT(FILE_BASENAME(theseFiles[iFile]), 'HWP', /EXTRACT, /REGEX))[0]
          IF fileBase0 NE fileBase1 THEN BEGIN
            PRINT_TEXT2, event, 'These files are not from the same group.'
            done = 0
          ENDIF
        ENDFOR

      ENDIF ELSE IF N_ELEMENTS(theseFiles) EQ 1 AND theseFiles[0] EQ '' THEN BEGIN
        ;This means no files were selected, so return to the main GUI
        RETURN
      ENDIF ELSE BEGIN
        ;The wrong number of files were seleceted, so return to the main GUI
        PRINT_TEXT2, event, 'There should be 16 HWP files.'
        RETURN
      ENDELSE
      
      ;If code gets here, then an acceptable set of files were selected
      ;Parse which band this is
      bandNames = [STRMID(SXPAR(HEADFITS(theseFiles[0]), 'FILTNME2'), 0, 1)]
      fileBases = [fileBase0]
      HWPfiles  = theseFiles
    ENDWHILE
  ENDELSE

  imgBuf  = []
  headBuf = STRARR(500, nFiles)
  HWPang  = FLTARR(nFiles)
  HWPsky  = FLTARR(nFiles)
  FOR iFile = 0, nFiles - 1 DO BEGIN
    thisFile = HWPfiles[iFile]
    thisImg  = READFITS(thisFile, thisHead)
    headLen  = N_ELEMENTS(thishead)

    ;Grab the size of the very first file
    IF iFile EQ 0 THEN sz0 = SIZE(thisImg, /DIMENSIONS)
    
    ;Test if this image size matches the first image size
    sz1 = SIZE(thisImg, /DIMENSIONS)

    ;Handle x-dimension first
    IF (sz1[0] GT sz0[0]) THEN BEGIN
      ;Increase x-dimension of imgBuf
      imgBufSz = SIZE(imgBuf, /DIMENSIONS)
      padArr   = FLTARR(sz1[0] - sz0[0], sz0[1], imgBufSz[2])
      imgBuf   = [imgBuf, padArr]

      ;Increase sz0 value to match new imgBuf size
      PRINT_TEXT2, event, 'Increasing x-dimension to match image sizes.'
      sz0[0] = sz1[0]
    ENDIF ELSE IF (sz1[0] LT sz0[0]) THEN BEGIN
      ;Increase x-dimension of thisImg
      padArr  = FLTARR(sz0[0] - sz1[0], sz1[1])
      thisImg = [thisImg, padArr]
    ENDIF

    ;Handle y-dimension second
    IF (sz1[1] GT sz0[1]) THEN BEGIN
      ;Increase y-dimension of imgBuf
      imgBufSz = SIZE(imgBuf, /DIMENSIONS)
      padArr   = FLTARR(sz0[0], sz1[1] - sz0[1], imgBufSz[2])
      imgBuf   = [[imgBuf], [padArr]]

      ;Increase sz0 value to match new imgBuf size
      PRINT_TEXT2, event, 'Increasing y-dimension to match image sizes.'
      sz0[1] = sz1[1]
    ENDIF ELSE IF (sz1[1] LT sz0[1]) THEN BEGIN
      ;Increase y-dimension of thisImg
      padArr  = FLTARR(sz0[0], sz0[1] - sz1[1])
      thisImg = [[thisImg], [padArr]]
    END

    ;Store the image and header in the buffers
    imgBuf  = [[[imgBuf]], [[thisImg]]]
    headBuf[0:headLen-1, iFile] = thisHead

    ;Store HWP and sky value
    HWPang[iFile] = SXPAR(thisHead, 'PPOL_HWP')
    HWPsky[iFile] = SXPAR(thisHead, 'S11AVSKY')
  ENDFOR
  
  ;******
  ;Generate a list of acceptable stars for image alignment
  ;******
  PRINT_TEXT2, event, 'Composing list of good alignment stars.'

  ;Extract astrometry from the display image header
  EXTAST, groupStruc.displayHeader, astr2MASS
  
  ;Oplot the image boundaries and stars
  AD2XY, groupStruc.starInfo.RAJ2000, groupStruc.starInfo.DEJ2000,$   ;Convert star positions to pixel coordinates
    astr2MASS, x2MASS, y2MASS
    
  ;Get a list of non-galaxy stars
  maskFile  = groupStruc.analysis_dir + 'S2_Ski_Jump_Fixes' + PATH_SEP() + 'Masking_files' + PATH_SEP() + 'mask2MASS.fits'
  mask2MASS = READFITS(maskFile)                                      ;Load in the 2MASS mask
  
  ;Test for stars in (or near) the galaxy mask
  numStars      = N_ELEMENTS(x2MASS)                                  ;Count the number of stars
  starsNearMask = BYTARR(numStars)                                    ;Initalize an array for the tracking stars near mask
  sz2MASS       = SIZE(groupStruc.displayImage, /DIMENSIONS)          ;Get the size of the displayImage
  MAKE_2D, FINDGEN(sz2MASS[0]), FINDGEN(sz2MASS[1]), xx2MASS, yy2MASS ;Make a 2D array for all image pixel locations
  FOR i = 0, numStars - 1 DO BEGIN                                    ;Loop through each of the preliminary star selections
    distFromStar     = SQRT((xx2MASS - x2MASS[i])^2E + $              ;Compute the distance from each star
      (yy2MASS - y2MASS[i])^2E)
    starsNearMask[i] = TOTAL((distFromStar LT 5) AND mask2MASS) GE 1 ;Check if the star is within 10 pixels of the mask
  ENDFOR
  
  ;Test stars for crowdedness within 30 pixels
  starsCrowded = TEST_CROWDED(x2MASS, y2MASS, 30.0)
  
  ;Grab a list of good sources for image alignment
  goodStars = ~starsNearmask AND ~starsCrowded
  goodInds  = WHERE(goodStars, numGood)
  IF numGood GT 0 THEN BEGIN
    ;Build structure containing RAs and Decs for sources
    ;(These are already brightness sorted)
    sources = {$
      RA:groupStruc.starInfo[goodInds].RAJ2000, $
      Dec:groupStruc.starInfo[goodInds].DEJ2000}
  ENDIF ELSE BEGIN
    PRINT_TEXT2, event, 'Could not find any good stars for alignment... strange.'
    STOP
    RETURN
  ENDELSE
  
  ;Compute the optimal image offsets for all 16 HWP images
  img_offsets = GET_IMAGE_OFFSETS(imgBuf, headBuf, sources)

  ;Shift offsets to be centered on median position of all images
  img_offsets -= REBIN(MEDIAN(img_offsets, DIMENSION=2, /EVEN), 2, nFiles, /SAMPLE)

;  ; test code
;  x1 = 618 & y1 = 690
;  WINDOW, 0, xs = 600, ys = 600  
;  FOR iTest = 0, nFiles - 2 DO BEGIN
;    tmpArr1 = FSHIFT(imgBuf[*,*,iTest], img_offsets[0,iTest], img_offsets[1,iTest])
;    tmpArr2 = FSHIFT(imgBuf[*,*,iTest+1], img_offsets[0,iTest+1], img_offsets[1,iTest+1])
;    
;    ;Create tiles for comparison
;    tile1  = tmpArr1[x1-10:x1+10,y1-10:y1+10]
;    tile1 -= MIN(tile1)
;    tile1 /= MAX(tile1)
;    
;    tile2 = tmpArr2[x1-10:x1+10,y1-10:y1+10]
;    tile2 -= MIN(tile2)
;    tile2 /= MAX(tile2)
;    
;    ;Display difference for comparison
;    TVIM, tile1 - tile2
;
;    STOP
;  ENDFOR
  
  ;Look through the image stack and shift each image "in place"
  FOR iFile = 0, nFiles - 1 DO BEGIN
    imgBuf[*,*,iFile] = FSHIFT(imgBuf[*,*, iFile], img_offsets[0, iFile], img_offsets[1,iFile])
  ENDFOR

  ;Now that all the HWP images are aligned, compute IPPA and stokes Images for each band
  FOR iBand = 0, nBands - 1 DO BEGIN
    ;Where should these file be written?
    thisInDir = startPaths[iBand] + 'S11_Full_Field_Polarimetry' + PATH_SEP()
    
    ;Grab the indices associated with this band
    inds = WHERE(bandFlags EQ iBand)
    
    ;Compute the modulated HWP angles for assigning IPPA
    modAng = ROUND(10*HWPang) MOD 900
    
    ;Assign each image to an IPPA value
    ind_0   = WHERE((modAng EQ 0) AND (bandFlags EQ iBand), n_0)
    ind_45  = WHERE((modAng EQ 225) AND (bandFlags EQ iBand), n_45)
    ind_90  = WHERE((modAng EQ 450) AND (bandFlags EQ iBand), n_90)
    ind_135 = WHERE((modAng EQ 675) AND (bandFlags EQ iBand), n_135)
    
    ;Compute Stokes values and uncertainty
    I_IPPA  = FLTARR(sz0[0], sz0[1], 4)
    s_Iippa = FLTARR(sz0[0], sz0[1], 4)
    
    ;Loop through the four IPPA values
    FOR ippa = 0, 3 DO BEGIN
      ;Grab the indices for this IPPA
      CASE ippa OF
        0: ind = ind_0
        1: ind = ind_45
        2: ind = ind_90
        3: ind = ind_135
      ENDCASE
      
      ;Double check that indices were properly retrieved.
      IF N_ELEMENTS(ind) EQ 0 THEN STOP
      
      ;Grab the HWP images and sky values for this IPPA
      thisStack = imgBuf[*,*,ind]
      thisSky   = HWPsky[ind]
      
      ;Compute the mean and uncertainty image
      ;assuming Poisson statistics and a gain of 8.21
      ;The factor of 0.5 in front of thisSig is to account for
      ;averaging four values -- sqrt(1.0/4.0) = 0.5
      thisMean = MEAN(thisStack, DIMENSION = 3)
      thisSig  = 0.5E*SQRT((MEAN(thisSky) + thisMean)/8.21)
      
      ;Store the mean array in the I_IPPA stack
      ;and the uncertainty in the s_Iippa stack
      I_IPPA[*,*,ippa]  = thisMean
      s_Iippa[*,*,ippa] = thisSig
    ENDFOR
    
    ;Now compute Stokes parameters using the I_IPPA images
    ;Stokes Q
    A      = I_IPPA[*,*,0] - I_IPPA[*,*,2]
    B      = I_IPPA[*,*,0] + I_IPPA[*,*,2]
    Qimg   = A/B
    s_AB   = SQRT(s_Iippa[*,*,0]^2 + s_Iippa[*,*,2]^2)
    s_Qimg = ABS(s_AB/B)*SQRT(1E + Qimg^2)
    
    ;Stokes U
    A      = I_IPPA[*,*,1] - I_IPPA[*,*,3]
    B      = I_IPPA[*,*,1] + I_IPPA[*,*,3]
    Uimg   = A/B
    Uimg  *= -1
    s_AB   = SQRT(s_Iippa[*,*,1]^2 + s_Iippa[*,*,3]^2)
    s_Uimg = ABS(s_AB/B)*SQRT(1E + Uimg^2)
    
    ;Stokes I
    Iimg   = 0.5*TOTAL(I_IPPA, 3)
    s_Iimg = 0.5*SQRT(TOTAL(s_Iippa^2, 3))

    ;******
    ;Find the calibration data on the disk drive
    ;******
    
    ;Compute average pixel positions to apply instrumental corrections
    ;Locate the PPOL code directory
    FINDPRO, 'MSP_PPOL_GUI', DirList=PPOL_dir, /NOPRINT
    calDir = PPOL_dir[0] + PATH_SEP() $
      + bandNames[iBand] $
      + '-Pol_Pinst_Default' + PATH_SEP()
      
    ;Build the paths to the instrumental correction files
    PinstFile   = calDir + 'P_inst_values.dat'
    QinstFile   = calDir + 'q_inst.fits'
    s_QinstFile = calDir + 'q_sig_inst.fits'
    UinstFile   = calDir + 'u_inst.fits'
    s_UinstFile = calDir + 'u_sig_inst.fits'
    
    ;Read the calibration data from the calibration file
    PinstData = READHEAD(PinstFile)
    
    ;Parse the Polarization Efficiency
    PE   = SXPAR(PinstData, 'P_EFFIC')
    s_PE = SXPAR(PinstData, 'S_P_EFF')
    
    ;Read in the U and Q instrumental correction images
    Qinst   = READFITS(QinstFile)/100E
    s_Qinst = READFITS(s_QinstFile)/100E
    Uinst   = READFITS(UinstFile)/100E
    s_Uinst = READFITS(s_UinstFile)/100E
    
    ;Grab the name of the HWP_00 image that should be there
    headerFile = thisInDir + fileBases[iBand] + 'HWP_00_bg.fits'
    outHead    = HEADFITS(headerFile)
    
    ;Compute average image offsets to map image pixels to detector locations
    mpp_xoff = SXPAR(outHead, 'MPP_XOFF')  ; amount to add to x pix to get detector x pix
    mpp_yoff = SXPAR(outHead, 'MPP_YOFF')

    ;Compute amount these images were shifted for alignment
    meanShift = MEAN(img_offsets[*, inds], DIMENSION=2)
    
    ;Add the shifting amount to the computed xoff, yoff values
    mpp_xoff += meanShift[0]
    mpp_yoff += meanShift[1]
    
    ;Build a map of image pixel locations
    MAKE_2D, INDGEN(sz0[0]), INDGEN(sz0[1]), xx, yy
    ;
    ; image pixel's mean location on the detector array
    ;
    starx = (0 > (xx + ROUND(mpp_xoff))) < 1023
    stary = (0 > (yy + ROUND(mpp_yoff))) < 1023
    ;
    ; instrumental correction terms at that location
    ;
    this_u_inst     = Uinst[starx, stary]
    this_q_inst     = Qinst[starx, stary]
    this_sig_u_inst = s_Uinst[starx, stary]
    this_sig_q_inst = s_Qinst[starx, stary]
    ;
    ; perform the correction of the star's raw U, Q to the final U, Q
    ;
    Qcor0   = (Qimg - this_q_inst)
    Ucor0   = (Uimg - this_u_inst)
    s_Qcor0 = sqrt(this_sig_q_inst^2 + s_Qimg^2)
    s_Ucor0 = sqrt(this_sig_u_inst^2 + s_Uimg^2)
    ;
    ; apply instrumental efficiency correction
    ;
    Qcor1   = Qcor0/PE
    Ucor1   = Ucor0/PE

    s_Qcor1 = ABS(Qcor1)*SQRT((s_Qcor0/Qcor0)^2 + (s_PE/PE)^2)
    s_Ucor1 = ABS(Ucor1)*SQRT((s_Ucor0/Ucor0)^2 + (s_PE/PE)^2)

    ;Grab the PA offset angles, uncertainties, and time stamps
    dPA    = SXPAR(PinstData, 'P_OFF_*')
    s_dPA  = SXPAR(PinstData, 'SP_OF_*')
    dPA_yr = SXPAR(PinstData, 'ENDYR_*')
    
    ;Parse the decimal date of the observations being processed
    thisDate    = STRSPLIT((STRSPLIT(SXPAR(outHead, 'DATE-OBS'), 'T', /EXTRACT))[0], '-', /EXTRACT)
    decimalDate = 1.0*thisDate[0] + thisDate[1]/12.0
    
    ;Determine the time-block of these observations
    previousDateInds = WHERE(dPA_yr lt decimalDate, numPreviousDates)
    IF numPreviousDates GT 0 THEN BEGIN
      dateInd = MAX(previousDateInds)
    ENDIF ELSE BEGIN
      PRINT, 'Could not find a proper date.'
      STOP
    ENDELSE
    
    ;Grab the specific dPA and s_dPA values
    dPA   = dPA[dateInd]
    s_dPA = s_dPA[dateInd]
    
    ;Write new image size to header
    sz0 = SIZE(Iimg, /DIMENSIONS)
    SXADDPAR, thisHead, 'NAXIS1', sz0[0]
    SXADDPAR, thisHead, 'NAXIS2', sz0[1]
    
    ;Write dPA values to header
    SXADDPAR, outHead, 'DELTAPA', dPA
    SXADDPAR, outHead, 'S_DPA', s_dPA

    PRINT_TEXT2, event, 'Completed polarimetric analysis.'

    PRINT_TEXT2, event, 'Starting final astrometric registration.'

    numStars = N_ELEMENTS(groupStruc.starInfo)                        ;Count the maximum number of possible astrometry stars
    hist     = SXPAR(outHead, "HISTORY")                              ;Get the history info
    SXDELPAR, outHead,'HISTORY'                                       ;Delete any previous history entries
    EXTAST, outHead, astr                                             ;Extract the initial astrometry
    AD2XY, groupStruc.starInfo.RAJ2000, groupStruc.starInfo.DEJ2000, $  ;Solve for initial guesses on star positions
      astr, xGuess, yGuess
      
    useStar = (xGuess GT 30) AND (xGuess LT (sz0[0] - 31)) $          ;Only use stars more than 30 pixels from image edge
      AND (yGuess GT 30) AND (yGuess LT (sz0[1] - 31))
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
      xRt      = (xOff  + 40) < (sz0[0] - 1)
      yOff     = (yGuess[j] - 19) > 0
      yTop     = (yOff + 40)  < (sz0[1] - 1)
      subArray = Iimg[xOff:xRt, yOff:yTop]
      
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
          ; TVIM, subarray
          ; OPLOT, [xcen], [ycen], PSYM=6, color=255L
          ; stop
        ENDIF
      ENDIF
      IF ~inArray OR ~ okShape $                                      ;If any one of the tests failed,
        OR (methodDifference GT 1) OR ~FINITE(methodDifference) $     ;then increment the failedFit counter
        THEN failedFit++
      IF failedFit GE 2 $                                             ;If two or more consecutive stars were not fit,
        THEN BREAK                                                    ;then break out of the loop.
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
    IF numUse GE 6 THEN BEGIN                                         ;Begin least squares method of astrometry
      ;**** PERFORM LEAST SQUARES ASTROMETRY ****
      astr = JM_SOLVE_ASTRO(astroStars.RAJ2000, astroStars.DEJ2000, $
        xStars, yStars, NAXIS1 = sz0[0], NAXIS2 = sz0[1])
      crpix = [511, 512]
      XY2AD, crpix[0], crpix[1], astr, crval1, crval2
      astr.crpix = (crpix + 1)                                        ;FITS convention is offset 1 pixel from IDL
      astr.crval = [crval1, crval2]                                   ;Store the updated reference pixel values
      PUTAST, outHead, astr, EQUINOX = 2000                           ;Update the header with the new astrometry
    ENDIF ELSE IF numUse GE 3 THEN BEGIN                              ;Begin "averaging" method of astrometry
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
      PUTAST, outHead, astr, EQUINOX = 2000                     ;Store astrometry in header
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
      PUTAST, outHead, astr, EQUINOX = 2000
    ENDIF ELSE BEGIN
      ;****PERFORM 1-STAR ASTROMETRY*****
    ENDELSE
    
    ;Restore the history to the header
    n_old = N_ELEMENTS(hist)
    FOR j= 0, n_old - 1 DO BEGIN
      SXADDPAR, outHead, "HISTORY", hist[j]
    ENDFOR
    ;**********************************************************************************************
    ;**********************ALSO UPDATE U AND Q IMAGES WITH THE SAME ASTROMETRY ********************
    ;**********************************************************************************************
    
    
    IF iBand EQ 0 THEN BEGIN
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
      UPDATE_GROUP_SUMMARY, event, groupStruc, 'finalPlateScale', plateScale ;Update the group structure to include the final plate scale
      UPDATE_GROUP_SUMMARY, event, groupStruc, /SAVE
    ENDIF

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
    SKY, Iimg, skyMode, skyNoise, /SILENT
    TVIM, Iimg, RANGE = skyMode + [-3, 100]*skyNoise, TITLE=bandNames[iBand] + '-band'
    OPLOT, xStars, yStars, PSYM=6, COLOR=RGB_TO_DECOMPOSED([0,255,0])   ;Overplot the inferred star locations
    ARROWS, outHead, 0.9, 0.75, /NORMAL                                 ;Show the North-East compas as sanity check
    PRINT_TEXT2, event, 'Finished computing astrometry'
    WAIT, 1.5
    
    ;Replace any NaN inds with zeros
    nanInds = WHERE(~FINITE(Iimg) OR ~FINITE(s_Iimg), numNaN)
    IF numNaN GT 0 THEN BEGIN
      Iimg[nanInds]   = 0.0
      s_Iimg[nanInds] = 0.0
    ENDIF

    nanInds = WHERE(~FINITE(Qcor1) OR ~FINITE(s_Qcor1), numNaN)
    IF numNaN GT 0 THEN BEGIN
      Qcor1[nanInds]   = 0.0
      s_Qcor1[nanInds] = 0.0
    ENDIF

    nanInds = WHERE(~FINITE(Ucor1) OR ~FINITE(s_Ucor1), numNaN)
    IF numNaN GT 0 THEN BEGIN
      Ucor1[nanInds]   = 0.0
      s_Ucor1[nanInds] = 0.0
    ENDIF
    
    ;******
    ;Now write ALL the computed files to disk
    ;******
    
    ;Temporarily store images for tests
    if iBand eq 0 then I1 = Iimg
    if iBand eq 1 then I2 = Iimg
    
    ;Write the intensity files to disk
    outPath = thisInDir + fileBases[iBand]
    Ipath   = outPath + 'I.fits'
    WRITEFITS, Ipath, Iimg, outHead
    
    sIpath = outPath + 'sI.fits'
    WRITEFITS, sIpath, s_Iimg, outHead
    
    ;Write the stokes images to disk
    Qpath  = outPath + 'Q.fits'
    WRITEFITS, Qpath, Qcor1, outHead
    
    sQpath = outPath + 'sQ.fits'
    WRITEFITS, sQpath, s_Qcor1, outHead
    
    Upath  = outPath + 'U.fits'
    WRITEFITS, Upath, Ucor1, outHead
    
    sUpath = outPath + 'sU.fits'
    WRITEFITS, sUpath, s_Ucor1, outHead
  ENDFOR

END