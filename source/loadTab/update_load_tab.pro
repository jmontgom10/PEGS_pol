PRO UPDATE_LOAD_TAB, event, groupStruc

  ;Grab the relevant widget-IDs to display the relevant group summary data to the user
  objectNameWID     = WIDGET_INFO(event.top, FIND_BY_UNAME='LOAD_NAME_TEXT')
  pointingWID       = WIDGET_INFO(event.top, FIND_BY_UNAME='LOAD_POINTING_TEXT')
  numGroupsWID      = WIDGET_INFO(event.top, FIND_BY_UNAME='LOAD_NUMBER_OF_GROUPS_TEXT')
  NIRbandWID        = WIDGET_INFO(event.top, FIND_BY_UNAME='LOAD_BAND_TEXT')
  stepCompleteWID   = WIDGET_INFO(event.top, FIND_BY_UNAME='LOAD_STEP_COMPLETE_TEXT')
  currentS3filesWID = WIDGET_INFO(event.top, FIND_BY_UNAME='S3B_S3_CURRENT_FILES')
  imageDisplayWID   = WIDGET_INFO(event.top, FIND_BY_UNAME='IMAGE_DISPLAY_WINDOW')
  PPOLassistWID     = WIDGET_INFO(event.top, FIND_BY_UNAME='PPOL_ASSIST_TAB')
  postProcessWID    = WIDGET_INFO(event.top, FIND_BY_UNAME='POST_PROCESSING_TAB')
  
  
  ;Generate the strings to display
;  medianRA_arr  = SIXTY(groupStruc.medianRA)
;  medianDec_arr = SIXTY(groupStruc.medianDec)
  medianPos   = STRING(FORMAT='("("2(I02,":"),I02,", ",+I03,":",I02,":",F04.1,")")', $
     SIXTY(groupStruc.medianRA/15D), SIXTY(groupStruc.medianDec))
  numGroups    = STRING(FORMAT='(I02)', groupStruc.numGroups)
  
  IF groupStruc.objectName EQ '' THEN BEGIN
    RENAME_OBJECT, event, objectName                                  ;Update name if empty
    groupStruc.objectName = objectName                                ;Update group structure with new name
  ENDIF
  
  WIDGET_CONTROL, objectNameWID, SET_VALUE=groupStruc.objectName      ;Update text fields
  WIDGET_CONTROL, pointingWID, SET_VALUE=medianPos
  WIDGET_CONTROL, numGroupsWID, SET_VALUE=numGroups
  WIDGET_CONTROL, NIRbandWID, SET_VALUE=groupStruc.NIRband
  WIDGET_CONTROL, stepCompleteWID, SET_VALUE=groupStruc.stepCompletion
  WIDGET_CONTROL, currentS3filesWID, SET_VALUE=groupStruc.currentS3files
  
  
  
  ;Update the astrometry star slider minimum--maximum range
  IF groupStruc.NIRband EQ 'H' THEN magIndex = 5 $
    ELSE IF groupStruc.NIRband EQ 'Ks' THEN magIndex = 7

  minMag             = MIN(groupStruc.starInfo.(magIndex), MAX = maxMag)
  wS3AmagRangeSlider = WIDGET_INFO(event.top, FIND_BY_UNAME='S3A_MAG_RANGE')
  stateStorageWID    = WIDGET_INFO(wS3AmagRangeSlider, /CHILD)
  WIDGET_CONTROL, stateStorageWID, GET_UVALUE=state, /NO_COPY
  state.min   = minMag
  state.max   = maxMag
  state.width = maxMag - minMag
  IF FINITE(state.max_difference) $
    THEN magRange = minMag + [0, state.max_difference] $
    ELSE magRange = [minMag, maxMag]
  WIDGET_CONTROL, stateStorageWID, SET_UVALUE=state, /NO_COPY
  WIDGET_CONTROL, wS3AmagRangeSlider, set_value=magRange

  ;Update the photometry star slider minimum--maximum range
  wS4magRangeSlider  = WIDGET_INFO(event.top, FIND_BY_UNAME='S4_MAG_RANGE')
  stateStorageWID    = WIDGET_INFO(wS4magRangeSlider, /CHILD)
  WIDGET_CONTROL, stateStorageWID, GET_UVALUE=state, /NO_COPY
  state.min   = minMag
  state.max   = maxMag
  state.width = maxMag - minMag
  IF FINITE(state.max_difference) $
    THEN magRange = minMag + [0, state.max_difference] $
    ELSE magRange = [minMag, maxMag]
  WIDGET_CONTROL, stateStorageWID, SET_UVALUE=state, /NO_COPY
  WIDGET_CONTROL, wS4magRangeSlider, set_value=magRange

  IF FINITE(groupStruc.finalPlateScale) THEN BEGIN
    plateScaleWID1 = WIDGET_INFO(event.top, FIND_BY_UNAME='PLATE_SCALE')
    plateScaleWID2 = WIDGET_INFO(event.top, FIND_BY_UNAME='OLD_PLATE_SCALE')
    plateScaleStr  = STRING(groupStruc.finalPlateScale, FORMAT='(F8.6)')
    WIDGET_CONTROL, plateScaleWID1, SET_VALUE=plateScaleStr
    WIDGET_CONTROL, plateScaleWID2, SET_VALUE=plateScaleStr
  ENDIF
  
  S2dir       = groupStruc.analysis_dir + 'S2_Ski_Jump_Fixes'         ;Test if the S2 directory structure is present
  NIRfilename = S2dir + PATH_SEP() + 'Masking_files' + PATH_SEP() + $ ;Test if the mask file is present
    groupStruc.objectName + '_' + groupStruc.NIRband + 'band.fits'
  IF ~FILE_TEST(S2dir, /DIRECTORY) THEN BEGIN                         ;If the directories don't exist, then create them
    FILE_MKDIR, S2dir
    FILE_MKDIR, S2dir + PATH_SEP() + 'Masking_files'
    FILE_MKDIR, S2dir + PATH_SEP() + 'Supersky_flats'
    FILE_MKDIR, S2dir + PATH_SEP() + 'Supersky_subtracted'
  ENDIF

  ;Test if the 2MASS image has already been downloaded.
  IF ~FILE_TEST(NIRfilename) THEN BEGIN
    PRINT_TEXT2, event, 'Contacting 2MASS-LGA cutout service'
    ;Download a 2MASS tile image if it is not already done
    ;This code uses the IRSA "cutouts" service
    ;(http://irsa.ipac.caltech.edu/applications/Cutouts/docs/CutoutsProgramInterface.html)
    IF groupStruc.medianDec LT 10 THEN decStr = STRING(groupStruc.medianDec, FORMAT = '(D+8.5)') $
      ELSE decStr = STRING(groupStruc.medianDec, FORMAT = '(D+9.5)')
    IF groupStruc.medianRA LT 10 THEN raStr = STRING(groupStruc.medianRA, FORMAT = '(D07.5)') $
      ELSE IF groupStruc.medianRA LT 100 THEN raStr = STRING(groupStruc.medianRA, FORMAT = '(D08.5)') $
      ELSE  raStr = STRING(groupStruc.medianRA, FORMAT = '(D09.5)')
    
    locStr  = 'locstr=' + raStr + '+' + decStr
    LGA_URL = 'http://irsa.ipac.caltech.edu/cgi-bin/Cutouts/nph-cutouts?mission=LGA&min_size=1&max_size=3600&units=arcmin&' + locStr + $
      '&sizeX=16.0&ntable_cutouts=3&cutouttbl1=LGA_J_band&cutouttbl2=LGA_H_band&cutouttbl3=LGA_K_band&mode=PI'
    LGA_XML = WEBGET(LGA_URL)

    ;If successful XML retrieval, then download LGA image
    IF LGA_XML.text[1] EQ '<result status="ok">' THEN BEGIN
      cutoutStart = WHERE(STRMATCH(LGA_XML.text, '*<cutouts>*'))
      cutoutEnd   = WHERE(STRMATCH(LGA_XML.text, '*</cutouts>*'))
      cutoutText  = LGA_XML.text[cutoutStart:cutoutEnd]
      FITS_ind    = WHERE(STRMATCH(cutoutText, '*_' + $
        STRMID(STRLOWCASE(groupStruc.NIRband),0,1) + '.fits*'), numFound)
      
      ; Test if any images were found
      IF numFound GT 0 THEN BEGIN
        FITS_URL   = (STRSPLIT(cutoutText[FITS_ind], ' ', /EXTRACT))[1]
        FITS_image = WEBGET(FITS_URL)
        
        S2dir        = groupStruc.analysis_dir + 'S2_Ski_Jump_Fixes'
        maskDir      = S2dir + PATH_SEP() + 'Masking_files'
        IF ~FILE_TEST(maskDir, /DIRECTORY) THEN FILE_MKDIR, maskDir
        NIRfilename = S2dir + PATH_SEP() + 'Masking_files' + PATH_SEP() + $
          groupStruc.objectName + '_' + groupStruc.NIRband + 'band.fits'
        WRITEFITS, NIRfilename, $                                     ;Save this file on the disk
          FITS_image.image, FITS_image.imageheader
      ENDIF
        PRINT_TEXT2, event, 'Could not find a *.fits cutout image'
    ENDIF

    ;Otherwise try the 2MASS server
    IF numFound EQ 0 THEN BEGIN
      ;Download a 2MASS tile image if it is not already done
      PRINT_TEXT2, event, '2MASS MOT not yet working'
      ;*****************************************************************
      ;
      ;The code below is not yet working (downloaded fits are messed up)
      ;
      ;*****************************************************************
      ;This code uses the IRSA SIA service
      ;(http://irsa.ipac.caltech.edu/applications/2MASS/IM/docs/siahelp.html)
;      IF groupStruc.medianDec LT 10 THEN decStr = STRING(groupStruc.medianDec, FORMAT = '(D+8.5)') $
;      ELSE decStr = STRING(groupStruc.medianDec, FORMAT = '(D+9.5)')
;      IF groupStruc.medianRA LT 10 THEN raStr = STRING(groupStruc.medianRA, FORMAT = '(D07.5)') $
;      ELSE IF groupStruc.medianRA LT 100 THEN raStr = STRING(groupStruc.medianRA, FORMAT = '(D08.5)') $
;      ELSE  raStr = STRING(groupStruc.medianRA, FORMAT = '(D09.5)')
;      
;      locStr       = 'POS=' + raStr + ',' + decStr
;      radius       = SQRT(2)*16.0/60.0                                  ;Circular ROI spanning 16-arcmin SQUARE ROI
;      TWO_MASS_URL = 'http://irsa.ipac.caltech.edu/cgi-bin/2MASS/IM/nph-im_sia?' + $
;        locStr + '&SIZE=' + STRING(FORMAT='(D09.5)', radius) + $
;        '&band=' + STRMID(STRUPCASE(groupStruc.NIRband),0,1) + '&FORMAT=image/fits'
;      TWO_MASS_XML = WEBGET(TWO_MASS_URL)
;      
;      ;If successful XML retrieval, then download the 2MASS images
;      IF two_mass_xml.text[8] EQ '<INFO name="QUERY_STATUS" value="OK" />' THEN BEGIN
;        PRINT_TEXT2, event, 'Starting 2MASS mosaic tool'
;        
;        
;        imgURLs  = WHERE(STRMATCH(two_mass_XML.text, '*nph-im*'), numFound)
;          
;        ;Loop through the found images and download them all...
;        FOR i = 0, numFound - 1 DO BEGIN
;          imgURL     = two_mass_XML.text[imgURLs[i]]
;          imgURL     = (STRSPLIT(imgURL, '[', /EXTRACT))[2]
;          imgURL     = (STRSPLIT(imgURL, ']', /EXTRACT))[0]
;
;          FITS_image = WEBGET(imgURL, /COPYFILE)
;          S2dir        = groupStruc.analysis_dir + 'S2_Ski_Jump_Fixes'
;          maskDir      = S2dir + PATH_SEP() + 'Masking_files'
;          IF ~FILE_TEST(maskDir, /DIRECTORY) THEN FILE_MKDIR, maskDir
;          NIRfilename = S2dir + PATH_SEP() + 'Masking_files' + PATH_SEP() + $
;            groupStruc.objectName + '_' + groupStruc.NIRband + 'band.fits'
;          WRITEFITS, NIRfilename, $                                     ;Save this file on the disk
;            FITS_image.image, FITS_image.imageheader
;         ENDFOR
;      ENDIF ELSE BEGIN
;        PRINT_TEXT2, event, 'No images found... skipping image procedure'
;        RETURN
;      ENDELSE
    ENDIF
  ENDIF
  
  ;Load and display 2MASS tile image
  WIDGET_CONTROL, imageDisplayWID, GET_VALUE = displayWindowIndex
  displayImage = READFITS(NIRfilename,  displayHeader)

  ;Add the "displayImage" and "displayHeader" to the group structure
;  groupStruc = { $
;    PEGS_POL_dir:groupStruc.PEGS_POL_dir, $
;    analysis_dir:groupStruc.analysis_dir, $
;    displayImage:displayImage, $
;    displayHeader:displayHeader, $
;    objectName:groupStruc.objectName, $
;    medianRA:groupStruc.medianRA, $
;    medianDec:groupStruc.medianDec, $
;    starInfo:groupStruc.starInfo, $
;    astroStarFlags:groupStruc.astroStarFlags, $
;    photStarFlags:groupStruc.photStarFlags, $
;    coverageBoundaries:groupStruc.coverageBoundaries, $
;    NIRband:groupStruc.NIRband, $
;    stepCompletion:groupStruc.stepCompletion, $
;;    numS3failed:groupStruc.numS3failed, $
;    currentS3files:groupStruc.currentS3files, $
;    numGroups:groupStruc.numGroups, $
;    groupNames:groupStruc.groupNames, $
;    groupNumbers:groupStruc.groupNumbers, $
;    groupFlags:groupStruc.groupFlags, $
;    groupImages:groupStruc.groupImages, $
;    imageFlags:groupStruc.imageFlags, $
;    astroFlags:groupStruc.astroFlags, $
;    finalPlateScale:groupStruc.finalPlateScale $
;    }

;  UPDATE_GROUP_SUMMARY, event, groupStruc

  ;Check if an image was successfully downloaded.
  IF N_ELEMENTS(displayHeader) GT 0 THEN BEGIN
    skyNoise = SXPAR(displayHeader, 'SIGMA')
    ;Check if the SIGMA parameter of the downloaded image tells us about the sky noise
    IF skyNoise EQ 0 THEN BEGIN
      SKY, displayImage, skyMode, skyNoise, /SILENT         ;Estimate the skyNoise from the image values.
    ENDIF
  ENDIF

  IF skyNoise EQ 0 THEN BEGIN
    skyParam = STRMID(groupStruc.NIRband,0,1) + 'SKYSIG'
    skyNoise = SXPAR(displayHeader, skyParam)
  ENDIF
  
  WSET, displayWindowIndex
  TVIM, displayImage, RANGE = [-3, +100]*skyNoise
  
  EXTAST, displayHeader, astr                                             ;Extract the image astrometry
  AD2XY, groupStruc.starInfo.RAJ2000, groupStruc.starInfo.DEJ2000, $      ;Convert star positions to pixel coordinates
    astr, xStars, yStars
  greenColorInd = RGB_TO_DECOMPOSED([0,255,0])
  OPLOT, (xStars-1), (yStars-1), PSYM = 6, COLOR = greenColorInd          ;Show the user where the 2MASS stars are

  WIDGET_CONTROL, PPOLassistWID, SENSITIVE = 1                            ;Sensitize the disabled processign tabs
  WIDGET_CONTROL, postProcessWID, SENSITIVE = 1
    
END