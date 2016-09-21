;When you create and use a widget application, you do the following things:
;1. Construct the Widget Hierarchy
;2. Provide an Event-Handling Routine
;3. Realize the Widgets
;4. Register the Program with the XMANAGER
;5. Interact with the Application
;6. Destroy Widgets

;At the highest level, creating a widget application consists of the following steps:
;1. Creating routines to react to widget events.
;2. Creating the widgets that make up the application’s interface.
;3. Realizing the widgets.
;4. Calling XMANAGER to manage events flowing from the widget interface.

;If you are using XMANAGER
;to manage your widget application (as in most cases you should), calling
;XMANAGER with CATCH=0 will cause XMANAGER to halt when it
;encounters an error.
;Setting CATCH=0 is useful during debugging, but finished programs should
;run with the default setting (CATCH=1) and refrain from setting it explicitly.
;
;CATCH is only effective if XMANAGER is blocking to dispatch errors.
;During debugging, make sure to call XMANAGER with NO_BLOCK=0 (the
;default).
;Setting NO_BLOCK=0 is useful during debugging, but in many cases you will
;want your finished program to set NO_BLOCK=1 in order to allow other
;widget programs (and the IDL command line) to remain active while your
;application is running.

PRO PEGS_POL_GUI
  
  ;Change this string to update the version number
  versionString = 'PEGS POL v0.6'
  
  DEVICE, DECOMPOSED = 1
  FINDPRO, 'PEGS_POL_GUI', DIRLIST=launchDirectory, /NOPRINT
  iconList = FILE_SEARCH(launchDirectory + PATH_SEP() + 'icons' + PATH_SEP(), '*.ico')

  ; Create the top-level base (TLB) and the tabs.
  wTLB         = WIDGET_BASE(/COLUMN, /BASE_ALIGN_TOP, UNAME='WID_BASE', BITMAP=iconList, $
    TITLE = versionString)
  wAnalysisWidgets  = WIDGET_BASE(wTLB, COLUMN=2, UNAME='ANALYSIS_WIDGETS_BASE')
  wPrePostTabs      = WIDGET_TAB(wAnalysisWidgets, LOCATION=2)
  wLoadProject      = WIDGET_BASE(wPrePostTabs, TITLE='Load PPOL Project', /ROW)
  wPreBase          = WIDGET_BASE(wPrePostTabs, TITLE='PPOL assist')
  wPreTabs          = WIDGET_TAB(wPreBase, LOCATION=0, XSIZE = 500, SENSITIVE=0, UNAME = 'PPOL_ASSIST_TAB')
  wPostBase         = WIDGET_BASE(wPrePostTabs, TITLE='Post processing')
  wPostTabs         = WIDGET_TAB(wPostBase, LOCATION=0, XSIZE = 500, SENSITIVE=0, UNAME = 'POST_PROCESSING_TAB')
  tabBaseGeometry   = WIDGET_INFO(wPrePostTabs, /GEOMETRY)
  wProgressBase     = WIDGET_BASE(wAnalysisWidgets, /COLUMN)
  wGroupProgLabel   = WIDGET_LABEL(wProgressBase, VALUE='Group Progress')
  wGroupProgressBar = WIDGET_DRAW(wProgressBase, XSIZE=tabBaseGeometry.xsize, YSIZE=25, UNAME='GROUP_PROGRESS_BAR')
  wImageProgLabel   = WIDGET_LABEL(wProgressBase, VALUE='Image Progress')
  wImageProgressBar = WIDGET_DRAW(wProgressBase, XSIZE=tabBaseGeometry.xsize, YSIZE=25, UNAME='IMAGE_PROGRESS_BAR')
  
  ;***LOAD PPOL PROJECT TAB***************************************************************

  wLoadSummaryBase       = WIDGET_BASE(wLoadProject, /COLUMN)
  wLoadTabRow1           = WIDGET_BASE(wLoadSummaryBase, /ROW)
  wLoadOpenButton        = WIDGET_BUTTON(wLoadTabRow1, VALUE='Open Project', EVENT_PRO='OPEN_PPOL_PROJECT')
  wBlank                 = WIDGET_LABEL(wLoadTabRow1, VALUE=' ')
  wLoadEditFlagsButton   = WIDGET_BUTTON(wLoadTabRow1, VALUE='Edit Groups', EVENT_PRO='OPEN_EDIT_FLAGS')
  
  wLoadTabRow2           = WIDGET_BASE(wLoadSummaryBase, /ROW)
  wLoadObjectNameLabel   = WIDGET_LABEL(wLoadTabRow2, VALUE='Object Name')
  wLoadObjectNameText    = WIDGET_TEXT(wLoadTabRow2, VALUE='', XSIZE=20, UNAME='LOAD_NAME_TEXT')
  wLoadObjectNameButton  = WIDGET_BUTTON(wLoadTabRow2, VALUE='Rename Object', EVENT_PRO='RENAME_OBJECT')
  
  wLoadTabRow3           = WIDGET_BASE(wLoadSummaryBase, /ROW)
  wLoadObjectPointLabel  = WIDGET_LABEL(wLoadTabRow3, VALUE='Median Pointing');, YSIZE=30)
  wLoadObjectPointText   = WIDGET_TEXT(wLoadTabRow3, VALUE='', XSIZE=20, UNAME='LOAD_POINTING_TEXT')

;  wLoadDataLabelsBase    = WIDGET_BASE(wLoadSummaryBase, /ROW)
  wLoadTabRow4           = WIDGET_BASE(wLoadSummaryBase, /ROW)
  wLoadNumGroupsLabel    = WIDGET_LABEL(wLoadTabRow4, VALUE='Groups   ');, YSIZE=30)
  wLoadStepCompleteLabel = WIDGET_LABEL(wLoadTabRow4, VALUE='Completed');, YSIZE=30)
  wLoadHKbandLabel       = WIDGET_LABEL(wLoadTabRow4, VALUE='NIR band  ');, YSIZE=30)
  
  wLoadTabRow5           = WIDGET_BASE(wLoadSummaryBase, /ROW)
;  wblankSpace            = WIDGET_LABEL(wLoadTabRow4, VALUE='')
;  wblankSpace            = WIDGET_LABEL(wLoadTabRow4, VALUE='')
  wLoadNumGroupsText     = WIDGET_TEXT(wLoadTabRow5, VALUE='', XSIZE = 6, UNAME='LOAD_NUMBER_OF_GROUPS_TEXT')
  wLoabProjCompleteText  = WIDGET_TEXT(wLoadTabRow5, VALUE='', XSIZE = 6, UNAME='LOAD_STEP_COMPLETE_TEXT')
  wLoadHKbandtext        = WIDGET_TEXT(wLoadTabRow5, VALUE='', XSIZE = 6, UNAME='LOAD_BAND_TEXT')
  
  ;****************************************************************************
  ;****************************** PPOL ASSIST *********************************
  ;****************************************************************************
  
  ;***STEP 2 TAB***************************************************************
  wS2skiJump          = WIDGET_BASE(wPreTabs, TITLE='Ski Jump Repair', /ROW)
  wS2ButtonBase       = WIDGET_BASE(wS2skiJump, /COLUMN, /ALIGN_LEFT, /BASE_ALIGN_LEFT)
  
  wS2row1             = WIDGET_BASE(wS2ButtonBase, /ROW)
  wS2Label1           = WIDGET_LABEL(wS2row1, VALUE='1. Get PA from IRAC image')
  wS2GetPA            = WIDGET_BUTTON(wS2row1, VALUE='Get PA', EVENT_PRO='S2_GET_IRAC_PA')

  wS2row2             = WIDGET_BASE(wS2ButtonBase, /ROW)
  wS2label2           = WIDGET_LABEL(wS2row2, VALUE='Contour mininimum')
  wS2contourSlider    = CW_DUAL_SLIDER(wS2row2, minimum=0E, maximum=8E, value=[1E,3E], $
    XSIZE = 175, UNAME='S2_CONTOUR_RANGE', TITLE='Contour Range', SENSITIVE = 0)
  wS2buttonBox        = WIDGET_BASE(wS2row2, /ROW)
  wS2redrawButton     = WIDGET_BUTTON(wS2buttonBox, VALUE='Redraw', UNAME='S2_REDRAW_CONTOURS')
  wS2continueButton   = WIDGET_BUTTON(wS2buttonBox, VALUE='Continue', UNAME='S2_CONTINUE')
  
  wS2row3             = WIDGET_BASE(wS2ButtonBase, /ROW)
  wS2label3           = WIDGET_LABEL(wS2row3, VALUE='2. Build a galaxy mask')
  wS2MakeMask         = WIDGET_BUTTON(wS2row3, VALUE='Build Galaxy Mask', EVENT_PRO='S2_BUILD_GALAXY_MASK')
  wS2IRAC_PA_base     = WIDGET_BASE(wS2row3, /NONEXCLUSIVE, /ALIGN_CENTER, /BASE_ALIGN_CENTER)
  wS2IRAC_PA_checkbox = WIDGET_BUTTON(wS2IRAC_PA_base, VALUE='use IRAC PA', $
    UNAME = 'S2_IRAC_PA', EVENT_PRO='S2_IRAC_PA')

  wS2row4             = WIDGET_BASE(wS2ButtonBase, /ROW)
  wS2label4           = WIDGET_LABEL(wS2row4, VALUE='3. Find and flatten the ski jumps', YSIZE=30)
  wS2skiJumpRepair    = WIDGET_BUTTON(wS2row4, VALUE='Repair Ski Jumps',  EVENT_PRO='S2_PEGS_SKI_JUMP_REPAIR')
  
  
  ;***STEP 3.A TAB*************************************************************
  wS3Arepair          = WIDGET_BASE(wPreTabs, TITLE='Image Astrometry', /ROW)
  wS3AbuttonBase      = WIDGET_BASE(wS3Arepair, /COLUMN, /ALIGN_LEFT, /BASE_ALIGN_LEFT)
  
;  wS3Arow1            = WIDGET_BASE(wS3AbuttonBase, /ROW)
;  wS3Alabel1          = WIDGET_LABEL(wS3Arow1, VALUE='1. Number of files with failed astrometry')
;  wS3Atext1           = WIDGET_TEXT(wS3Arow1, VALUE='    ??', xsize=6, UNAME='S3A_NUM_FILES_TEXT')
;  wS3AfindFailedFiles = WIDGET_BUTTON(wS3Arow1, VALUE='Find Files', EVENT_PRO='S3A_FIND_ASTROMETRY_FILES')

  wS3Arow2            = WIDGET_BASE(wS3AbuttonBase, /ROW)
  wS3Alabel2          = WIDGET_LABEL(wS3Arow2, VALUE='1. Select astrometry stars', YSIZE=30)
  wS3AmagRangeSlider  = CW_DUAL_SLIDER(wS3Arow2, minimum=0E, maximum=14E, value=FLOAT([1,13]), $
    XSIZE = 175, EVENT_PRO='S3A_MAGNITUDE_RANGE', UNAME='S3A_MAG_RANGE', TITLE='Magnitude Range', SENSITIVE = 0)
  wS3AcheckboxBase    = WIDGET_BASE(wS3Arow2, /NONEXCLUSIVE, /ALIGN_CENTER, /BASE_ALIGN_CENTER)
  wS3Acheckbox        = WIDGET_BUTTON(wS3AcheckboxBase, VALUE='Slider Inactive', $
    EVENT_PRO='S3A_SELECT_ASTROMETRY_MAGNITUDE_RANGE')

  wS3Arow3            = WIDGET_BASE(wS3AbuttonBase, /ROW)
  wS3Alabel3          = WIDGET_LABEL(wS3Arow3, VALUE='2. Begin manual astrometry repair', YSIZE=30)
  wS3APerformAstrBase = WIDGET_BASE(wS3Arow3, /COLUMN)
  wS3ABeginAstrRepair = WIDGET_BUTTON(wS3APerformAstrBase, VALUE='Astrometry Repair', $
    UNAME='S3A_BEGIN_ASTROMETRY', EVENT_PRO='S3A_ASTROMETRY_REPAIR')
  wS3AcheckAstroBase  = WIDGET_BASE(wS3APerformAstrBase, /ROW)
  wS3AAcceptPhoto     = WIDGET_BUTTON(wS3AcheckAstroBase, VALUE='Accept Phot.', UNAME='S3A_ACCEPT_PHOTO', SENSITIVE=0)
  wS3AAcceptAstro     = WIDGET_BUTTON(wS3AcheckAstroBase, VALUE='Accept Astr.', UNAME='S3A_ACCEPT_ASTRO', SENSITIVE=0)
  wS3AsaveRejectBase  = WIDGET_BASE(wS3APerformAstrBase, /ROW)
  wS3ARejectAstro     = WIDGET_BUTTON(wS3AsaveRejectBase, VALUE='Reject image', UNAME='S3A_REJECT_IMAGE', SENSITIVE=0)
  wS3ASaveProgress    = WIDGET_BUTTON(wS3AsaveRejectBase, VALUE='Save and Stop', UNAME='S3A_SAVE_PROGRESS', SENSITIVE=0)
  
  wS3Arow4            = WIDGET_BASE(wS3AbuttonBase, /ROW)
  wS3Alabel4          = WIDGET_LABEL(wS3Arow4, VALUE = '3. Check Astrometry')
  wS3AcheckButtons    = WIDGET_BASE(wS3Arow4, /ROW)
  wS3AcheckAstro      = WIDGET_BUTTON(wS3AcheckButtons, VALUE = 'Start', EVENT_PRO='S3A_CHECK_ASTROMETRY')
  wS3AgoodAstro       = WIDGET_BUTTON(wS3AcheckButtons, VALUE = 'Good', UNAME='S3A_CHECK_GOOD')
  wS3AbadAstro        = WIDGET_BUTTON(wS3AcheckButtons, VALUE = 'Bad', UNAME='S3A_CHECK_BAD')
  
  wS3Arow5            = WIDGET_BASE(wS3AbuttonBase, /ROW)
  wS3Alabel5          = WIDGET_LABEL(wS3Arow5, VALUE='Solved astrometry information')
  wS3AinfoBase        = WIDGET_BASE(wS3Arow5, COLUMN=2, UNAME='S3A_ASTROMETRY_BASE')
  wCenterRALabel      = WIDGET_LABEL(wS3AinfoBase, VALUE='RA')
  wCenterRAText       = WIDGET_TEXT(wS3AinfoBase, XSIZE=14, UNAME='S3A_RA_TEXT')
  wPlateScaleLabel    = WIDGET_LABEL(wS3AinfoBase, VALUE='arcsec/pix')
  wPlateScaleText     = WIDGET_TEXT(wS3AinfoBase, XSIZE=14, UNAME='S3A_PLATE_SCALE')
  wCenterDecLabel     = WIDGET_LABEL(wS3AinfoBase, VALUE='Dec')
  wCenterDecText      = WIDGET_TEXT(wS3AinfoBase, XSIZE=14, UNAME='S3A_DEC_TEXT')
  wRotAngleLabel      = WIDGET_LABEL(wS3AinfoBase, VALUE='Rot. (deg.)')
  wRotAngleText       = WIDGET_TEXT(wS3AinfoBase, XSIZE=14, UNAME='S3A_ROT_ANGLE')
  
  
  ;***STEP 3.B TAB*************************************************************
  wS3Bsupersky         = WIDGET_BASE(wPreTabs, TITLE='Supersky Subtraction', /ROW, /ALIGN_CENTER)
  wS3BbuttonBase       = WIDGET_BASE(wS3Bsupersky, /COLUMN, /ALIGN_LEFT, /BASE_ALIGN_LEFT)

  wS3Brow1             = WIDGET_BASE(wS3BbuttonBase, /ROW)
  wS3Blabel1           = WIDGET_LABEL(wS3Brow1, VALUE='1. Set Mask Threshold')
  wS3BthresholdSlider  = CW_FSLIDER(wS3Brow1, minimum=17E, maximum=21E, value=19E, $
    UNAME='S3B_MASKING_THRESHOLD', TITLE='Threshold (mag/arcsec^2)', /EDIT, FORMAT='(G19.4)')
  wS3BredrawButton     = WIDGET_BUTTON(wS3Brow1, VALUE='Redraw', UNAME='S3B_REDRAW_MASK', EVENT_PRO='S3B_REDRAW_MASK')

  wS3Brow2             = WIDGET_BASE(wS3BbuttonBase, /ROW)
  wS3Blabel2           = WIDGET_LABEL(wS3Brow2, VALUE='2. Generate masked supersky flats', YSIZE=30)
  wS3BGenerateSupersky = WIDGET_BUTTON(wS3Brow2, VALUE='Generate Supersky', $
    SENSITIVE = 1, UNAME='S3B_GENERATE_SUPERSKY', EVENT_PRO='S3B_GENERATE_SUPERSKY')
  
;  wS3Brow3             = WIDGET_BASE(wS3BbuttonBase, /ROW)
;  wS3BLabel3           = WIDGET_LABEL(wS3Brow3, VALUE='3. Repair residual supersky artifacts', YSIZE=30)
;  wS3BRepairSupersky   = WIDGET_BUTTON(wS3Brow3, VALUE='Repiar Supersky', $
;    SENSITIVE = 1, UNAME='S3B_REPAIR_SUPERSKY', EVENT_PRO='S3B_REPAIR_SUPERSKY')
  
  wS3Brow3             = WIDGET_BASE(wS3BbuttonBase, /ROW)
  wS3Blabel3           = WIDGET_LABEL(wS3Brow3, VALUE='3. Subtract supersky flats', YSIZE=30)
  wS3BSubtractSupersky = WIDGET_BUTTON(wS3Brow3, VALUE='Subtract Supersky', $
    SENSITIVE = 1, UNAME='S3B_SUBTRACT_SUPERSKY', EVENT_PRO='S3B_SUBTRACT_SUPERSKY')


  ;***STEP 4 TAB***************************************************************
  wS4photometry      = WIDGET_BASE(wPreTabs, TITLE='Photometry', /ROW)
  wS4ButtonBase      = WIDGET_BASE(wS4photometry, /COLUMN, /ALIGN_LEFT, /BASE_ALIGN_LEFT)
  
  wS4row1            = WIDGET_BASE(wS4ButtonBase, /ROW)
  wS4label1          = WIDGET_LABEL(wS4row1, VALUE='1. Compute dither overlap', YSIZE=30)
  wS4ditherOverlap   = WIDGET_BUTTON(wS4row1, VALUE='Find Boundaries', EVENT_PRO='S4_DETERMINE_BOUNDARIES')
  
  wS4row2            = WIDGET_BASE(wS4ButtonBase, /ROW)
  wS4label2          = WIDGET_LABEL(wS4row2, VALUE='2. Select photometry stars', YSIZE=30)
  wS4magRangeSlider  = CW_DUAL_SLIDER(wS4row2, minimum=0E, maximum=14E, value=FLOAT([1,13]), $
    XSIZE = 175, MAX_DIFFERENCE=2, EVENT_PRO='S4_MAGNITUDE_RANGE', UNAME='S4_MAG_RANGE', TITLE='Magnitude Range', SENSITIVE = 0)
  wS4checkboxBase    = WIDGET_BASE(wS4row2, /NONEXCLUSIVE, /ALIGN_CENTER, /BASE_ALIGN_CENTER)
  wS4checkbox        = WIDGET_BUTTON(wS4checkboxBase, VALUE='Slider Inactive', $
    EVENT_PRO='S4_SELECT_PHOTOMETRY_MAGNITUDE_RANGE', UNAME='S4_SELECT_PHOTOMETRY_MAGNITUDE_RANGE')
  
  wS4row3            = WIDGET_BASE(wS4ButtonBase, /ROW)
  wS4label3          = WIDGET_LABEL(wS4row3, VALUE='3. Compute multi-aperture photometry', YSIZE=30)
  wS4PerformPhot     = WIDGET_BUTTON(wS4row3, VALUE='Perform Photometry', EVENT_PRO='S4_PERFORM_PHOTOMETRY')

  wS4row4            = WIDGET_BASE(wS4ButtonBase, /ROW)
  wS4label4          = WIDGET_LABEL(wS4row4, VALUE='4. Manually filter out bad stars', YSIZE=30)
  wS4CheckPhotBase   = WIDGET_BASE(wS4row4, /COLUMN, /ALIGN_CENTER, FRAME = 2)
  wS4CheckPhot       = WIDGET_BUTTON(wS4CheckPhotBase, VALUE='Check Photometry', EVENT_PRO='S4_CHECK_PHOTOMETRY')
  wS4EditPhotBase    = WIDGET_BASE(wS4CheckphotBase, COLUMN = 3, /ALIGN_CENTER)
  wS4AcceptButton    = WIDGET_BUTTON(wS4EditPhotBase, VALUE='Accept', UNAME = 'S4_ACCEPT_IMAGE_PHOT')
  wS4StarLabel       = WIDGET_LABEL(wS4EditPhotBase, VALUE='Star #', UNAME = 'S4_STAR_LABEL', SENSITIVE=0)
  wS4RejectButton    = WIDGET_BUTTON(wS4EditPhotBase, VALUE='Reject', UNAME = 'S4_REJECT_IMAGE_PHOT')
  wS4StarText        = WIDGET_TEXT(wS4EditPhotBase, XSIZE = 3, /EDITABLE, UNAME = 'S4_STAR_NUMBER', SENSITIVE=0)
  wS4EditButton      = WIDGET_BUTTON(wS4EditPhotBase, VALUE='Edit', UNAME = 'S4_EDIT_STAR_LIST')
  wS4DeleteButton    = WIDGET_BUTTON(wS4EditPhotBase, VALUE='Delete', UNAME = 'S4_DELETE_STAR', SENSITIVE=0)
  
  ;****************************************************************************
  ;***************************** POST PROCESSING ******************************
  ;****************************************************************************

  ;***STEP 1 TAB***************************************************************
  ; Create the first tab base, containing a label and two
  ; button groups.
  wPost1              = WIDGET_BASE(wPostTabs, TITLE='Final images', /COLUMN, /ALIGN_LEFT, /BASE_ALIGN_LEFT)
  wPost1Label1        = WIDGET_LABEL(wPost1, VALUE=NEW_LINE()+'1. Combine all PPOL step 11 images', YSIZE=30)
  wCombineImages      = WIDGET_BUTTON(wPost1, VALUE='Start Combining', EVENT_PRO='S1_AVERAGE_STOKES_IMAGES')
  
  wPost1Label2        = WIDGET_LABEL(wPost1, VALUE=NEW_LINE()+'2. Compute astrometry of final, combined images', YSIZE=30)
  wAstrometry         = WIDGET_BUTTON(wPost1, VALUE='Start Astrometry', EVENT_PRO='S1_FINAL_ASTROMETRY')
  wAstrometryBase     = WIDGET_BASE(wPost1, /ROW, UNAME='ASTROMETRY_BASE')
  wAstrometryTextBase = WIDGET_BASE(wAstrometryBase, COLUMN=2)
  wCenterRALabel      = WIDGET_LABEL(wAstrometryTextBase, VALUE='Center RA')
  wCenterRAText       = WIDGET_TEXT(wAstrometryTextBase, XSIZE=20, UNAME='RA_TEXT')
  wPlateScaleLabel    = WIDGET_LABEL(wAstrometryTextBase, VALUE='Pl. Sc. (as/px)')
  wPlateScaleText     = WIDGET_TEXT(wAstrometryTextBase, XSIZE=20, UNAME='PLATE_SCALE')
  wCenterDecLabel     = WIDGET_LABEL(wAstrometryTextBase, VALUE='Center Dec')
  wCenterDecText      = WIDGET_TEXT(wAstrometryTextBase, XSIZE=20, UNAME='DEC_TEXT')
  wRotAngleLabel      = WIDGET_LABEL(wAstrometryTextBase, VALUE='Rot. Angle (deg.)')
  wRotAngleText       = WIDGET_TEXT(wAstrometryTextBase, XSIZE=20, UNAME='ROT_ANGLE')
  
  ;***STEP 2 TAB***************************************************************
  wPost2                = WIDGET_BASE(wPostTabs, TITLE='Calibrate Photometry', /COLUMN, /ALIGN_LEFT, /BASE_ALIGN_LEFT)
  wS2row1               = WIDGET_BASE(wPost2, /ROW)
  wS2label1             = WIDGET_LABEL(wS2row1, VALUE='1. Select photometry stars', YSIZE=30)
  wS2magRangeSlider     = CW_DUAL_SLIDER(wS2row1, minimum=0E, maximum=14E, value=FLOAT([1,13]), $
    XSIZE = 175, MAX_DIFFERENCE=5, EVENT_PRO='S2_MAGNITUDE_RANGE', UNAME='S2_MAG_RANGE', TITLE='Magnitude Range', SENSITIVE = 0)
  wS2sliderCheckboxBase = WIDGET_BASE(wS2row1, /NONEXCLUSIVE, /ALIGN_CENTER, /BASE_ALIGN_CENTER)
  wS2sliderCheckbox     = WIDGET_BUTTON(wS2sliderCheckboxBase, VALUE='Slider Inactive', $
    EVENT_PRO='S2_SELECT_PHOTOMETRY_MAGNITUDE_RANGE', UNAME='S2_SELECT_PHOTOMETRY_MAGNITUDE_RANGE')

  wPost2Label1         = WIDGET_LABEL(wPost2, VALUE=NEW_LINE()+'2. Measure instrumental photometry', YSIZE=30)
  wMeasurePhotometry   = WIDGET_BUTTON(wPost2, VALUE='Measure Photometry', EVENT_PRO='S2_MEASURE_PHOTOMETRY')

  wS2blank             = WIDGET_LABEL(wPost2, VALUE=' ')
  wS2pathCheckBoxBase  = WIDGET_BASE(wPost2, /NONEXCLUSIVE, /ALIGN_LEFT, /BASE_ALIGN_CENTER)
  wS2pathCheckbox      = WIDGET_BUTTON(wS2pathCheckboxBase, VALUE='Use a second NIR band to compute color correction', $
    EVENT_PRO='S2_USE_2BAND_PHOTOMETRY', UNAME='S2_USE_2BAND_PHOTOMETRY')
  
  wS2_2ndBandRow       = WIDGET_BASE(wPost2, /ROW)
  wS2_pathTextBox      = WIDGET_TEXT(ws2_2ndBandRow, XSIZE=50, /EDITABLE, SENSITIVE=0, UNAME='2ND_BAND_PATH')
  wS2_pathBrowseButton = WIDGET_BUTTON(wS2_2ndBandRow, VALUE='Browse...', $
    UNAME='S2_BROWSE_FOR_2ND_BAND', EVENT_PRO='S2_BROWSE_FOR_2ND_BAND', SENSITIVE=0)

  wPost2Label2         = WIDGET_LABEL(wPost2, VALUE=NEW_LINE()+'3. Calibrate photometry to 2MASS', YSIZE=30)
  wCalibratePhotometry = WIDGET_BUTTON(wPost2, VALUE='Calibrate Photometry', EVENT_PRO='S2_CALIBRATE_PHOTOMETRY')

  
  ;***STEP 3 TAB***************************************************************
  wPost3            = WIDGET_BASE(wPostTabs, TITLE='Rotate Stokes I', /COLUMN, /BASE_ALIGN_CENTER)
;  wS3galPAlabel     = WIDGET_LABEL(wPost3, VALUE='Galaxy PA (degrees CCW)')
;  wS3galPAtext      = WIDGET_TEXT(wPost3, XSIZE=20, UNAME='GALAXY_PA')
;  wS3galCenterBase  = WIDGET_BASE(wPost3, COLUMN=2)  
;  wS3galXlabel      = WIDGET_LABEL(wS3galCenterBase, VALUE='Galaxy X Center (pix)')
;  wS3galXtext       = WIDGET_TEXT(wS3galCenterBase, XSIZE=20, UNAME='GALAXY_X_PIXEL')
;  wS3galYlabel      = WIDGET_LABEL(wS3galCenterBase, VALUE='Galaxy Y Center (pix)')
;  wS3galYtext       = WIDGET_TEXT(wS2galCenterBase, XSIZE=20, UNAME='GALAXY_Y_PIXEL')

  wS3blank          = WIDGET_LABEL(wPost3, VALUE=' ')
  wS3checkBoxBase   = WIDGET_BASE(wPost3, /NONEXCLUSIVE)
  wS3checkBox       = WIDGET_BUTTON(wS3checkBoxBase, VALUE='Use Galaxy Model Values', $
    UNAME='S3_USE_MODEL_VALUES', EVENT_PRO='S3_USE_MODEL_VALUES')
  
  wS3rotPAlabel     = WIDGET_LABEL(wPost3, VALUE='Rotation Angle (degrees CCW)')
  wS3rotPAtext      = WIDGET_TEXT(wPost3, XSIZE=20, /EDITABLE, UNAME='ROTATION_ANGLE')
  wS3rotCenterBase  = WIDGET_BASE(wPost3, COLUMN=2)  
  wS3rotXlabel      = WIDGET_LABEL(wS3rotCenterBase, VALUE='Rotation X Center (pix)')
  wS3rotXtext       = WIDGET_TEXT(wS3rotCenterBase, XSIZE=20, /EDITABLE, UNAME='ROTATION_X_PIXEL')
  wS3rotYlabel      = WIDGET_LABEL(wS3rotCenterBase, VALUE='Rotation Y Center (pix)')
  wS3rotYtext       = WIDGET_TEXT(wS3rotCenterBase, XSIZE=20, /EDITABLE, UNAME='ROTATION_Y_PIXEL')
  wS3rotateImages   = WIDGET_BUTTON(wPost3, VALUE='Rotate Image', EVENT_PRO='S3_ROTATE_IMAGE')
  
  ;***STEP 3 TAB***************************************************************
  wPost4            = WIDGET_BASE(wPostTabs, TITLE='Smooth and/or Rebin', /COLUMN)
  wS4ImageLabel     = WIDGET_LABEL(wPost4, VALUE='Select the images on which to operate')
  wS4ImageBase      = WIDGET_BASE(wPost4, /ROW, /EXCLUSIVE, EVENT_PRO='S4_SELECT_INPUT_IMAGES', UNAME='S4_SELECT_INPUT_IMAGES')
  wS4OriginalButton = WIDGET_BUTTON(wS4ImageBase, VALUE='Original images', UNAME='S4_ORIGINAL_IMAGES')
  wS4RotatedButton  = WIDGET_BUTTON(wS4ImageBase, VALUE='Rotated images', UNAME='S4_ROTATED_IMAGES')
  wS4MethodLabel    = WIDGET_LABEL(wPost4, VALUE='Select which processing method to use')
  wS4MethodTabs     = WIDGET_TAB(wPost4, LOCATION=0, EVENT_PRO='S4_SELECT_PROCESSING_METHOD', UNAME='S4_METHOD_TABS')
  WIDGET_CONTROL, wS4MethodTabs, SET_UVALUE=0


  wS4Rebin            = WIDGET_BASE(wS4MethodTabs, TITLE='Integer Pixel Rebin', /COLUMN)
  wS4RebinNumPixBase  = WIDGET_BASE(wS4Rebin, /ROW)
  wS4RebinNumPixLabel = WIDGET_LABEL(wS4RebinNumPixBase, VALUE='Number of pixels to rebin')
  wS4RebinNumPix      = WIDGET_TEXT(wS4RebinNumPixBase, XSIZE=8, /EDITABLE, UNAME='REBIN_PIXELS', EVENT_PRO='S4_SET_REBIN_PIXELS')
  wS4RebinOldPSbase   = WIDGET_BASE(wS4Rebin, /ROW)
  wS4RebinOldPSlabel  = WIDGET_LABEL(wS4RebinOldPSbase, VALUE='Old Plate Scale')
  wS4RebinOldPS       = WIDGET_TEXT(wS4RebinOldPSbase, XSIZE=8, UNAME='OLD_PLATE_SCALE')
  wS4RebinNewPSbase   = WIDGET_BASE(wS4Rebin, /ROW)
  wS4RebinNewPSlabel  = WIDGET_LABEL(wS4RebinNewPSbase, VALUE='New Plate Scale')
  wS4RebinNewPS       = WIDGET_TEXT(wS4RebinNewPSbase, XSIZE=8, UNAME='NEW_PLATE_SCALE')
  
  
  wS4Adaptive         = WIDGET_BASE(wS4MethodTabs, TITLE='Adaptive Mesh', /COLUMN)
  wS4smallestBinBase  = WIDGET_BASE(wS4Adaptive, /ROW)
  wS4smallestBinLabel = WIDGET_LABEL(wS4smallestBinBase, VALUE='Smallest rebinning size')
  wS4smallestBin      = WIDGET_TEXT(wS4smallestBinBase, XSIZE=6, /EDITABLE, $
    UNAME='SMALLEST_MESH_BIN', EVENT_PRO='S4_SET_SMALLEST_MESH_BIN')
  wS4numLevelsBase    = WIDGET_BASE(wS4Adaptive, /ROW)
  wS4numLevelsLabel   = WIDGET_LABEL(wS4numLevelsBase, VALUE='Number of Rebin Levels')
  wS4numLevels        = WIDGET_TEXT(wS4numLevelsBase, XSIZE=6, /EDITABLE, $
    UNAME='NUMBER_REBIN_LEVELS', EVENT_PRO='S4_SET_NUMBER_REBIN_LEVELS')
  wS4SNRcutoffBase    = WIDGET_BASE(wS4Adaptive, /ROW)
  wS4SNRcutoffLabel   = WIDGET_LABEL(wS4SNRcutoffBase, VALUE='Minimum allowable SNR')
  wS4SNRcutoff        = WIDGET_TEXT(wS4SNRcutoffBase, XSIZE=6, /EDITABLE, $
    UNAME='MINIMUM_MESH_SNR', EVENT_PRO='S4_SET_MINIMUM_MESH_SNR')
  
  
  wS4Smooth              = WIDGET_BASE(wS4MethodTabs, TITLE='Gaussian Smooth and Rebin', /COLUMN)
  wS4KernelWidthBase     = WIDGET_BASE(wS4Smooth, /ROW)
  wS4KernelWidthLabel    = WIDGET_LABEL(wS4KernelWidthBase, VALUE='Kernel FWHM (arcsec)')
  wS4KernelWidth         = WIDGET_TEXT(wS4KernelWidthBase, XSIZE=6, /EDITABLE)
  wS4KernelWidthLabelPix = WIDGET_LABEL(wS4KernelWidthBase, VALUE='Kernel FWHM (pixels)')
  wS4KernelWidthPix      = WIDGET_TEXT(wS4KernelWidthBase, XSIZE=6)
  wS4SamplePitchBase     = WIDGET_BASE(wS4Smooth, /ROW)
  wS4SamplePitchLabel    = WIDGET_LABEL(wS4SamplePitchBase, VALUE='Resample pitch (arcsec)')
  wS4SamplePitch         = WIDGET_TEXT(wS4SamplePitchBase, XSIZE=6, /EDITABLE)
  wS4SamplePitchLabelPix = WIDGET_LABEL(wS4SamplePitchBase, VALUE='Resample pitch (pixels)')
  wS4SamplePitchPixl     = WIDGET_TEXT(wS4SamplePitchBase, XSIZE=6)

  wS4StartBase           = WIDGET_BASE(wPost4, /COLUMN)
  wS4StartButon          = WIDGET_BUTTON(wS4StartBase, VALUE='Start Processing', EVENT_PRO='S4_START_PROCESSING')
  
  ;Produce a display window that will be used for ALL analysis steps
  wImageWindow           = WIDGET_DRAW(wAnalysisWidgets, $
    XSIZE=10, YSIZE=10, $
    UNAME='IMAGE_DISPLAY_WINDOW')
  
  ;***********************bottom pannel********************
  wControlBase = WIDGET_BASE(wTLB, /ROW, /BASE_ALIGN_CENTER) 
  
  IF STRUPCASE(!VERSION.OS_FAMILY) EQ 'UNIX' $
    THEN monospaceFont = 'Courier' $
    ELSE IF STRUPCASE(!VERSION.OS_FAMILY) EQ 'WINDOWS' $
    THEN monospaceFont = 'Courier*8'

  wPrintText   = WIDGET_TEXT(wControlBase, XSIZE=90, YSIZE=8, $       ;This text box will display messages to the user...
    /WRAP, /SCROLL, FONT = monospaceFont, UNAME='BASE_TEXT_BOX')      ;...just use the command "PRINT_TEXT2, event, message"

  wSpace             = WIDGET_LABEL(wControlBase, VALUE='   ')
  wControlButtonBase = WIDGET_BASE(wControlBase, /COLUMN)
  wManualButton      = WIDGET_BUTTON(wControlButtonBase, VALUE='Manual')
  wExitButton        = WIDGET_BUTTON(wControlButtonBase, VALUE='Exit', EVENT_PRO='EXIT_BUTTON');, $
;    YSIZE=25, UNITS=0)
  
  ; Realize the widgets, set the user value of the top-level
  ; base, and call XMANAGER to manage everything.
  WIDGET_CONTROL, wTLB, /REALIZE                                      ;Realize the top-level-base (and WHOLE GUI)

  ;Now that the top-level-base has been realized, it is possible to correctly resize the main draw window.
  analysisWidgetGeometry = WIDGET_INFO(wAnalysisWidgets, /GEOMETRY)
  WIDGET_CONTROL, wImageWindow, XSIZE=analysisWidgetGeometry.ysize-9, YSIZE=analysisWidgetGeometry.ysize-9

  ;Issue some commands to appropriately set the dual slider values (and cause them to be drawn)
  WIDGET_CONTROL, wS2contourSlider, set_value=FLOAT([1,3])
  WIDGET_CONTROL, wS3AmagRangeSlider, set_value=FLOAT([1,13])         ;Set the dual sliders to initial values
  WIDGET_CONTROL, wS4magRangeSlider, set_value=FLOAT([1,13])
  WIDGET_CONTROL, wS2magRangeSlider, set_value=FLOAT([8,13])

  ;Set default masking procedure (ignore IRAC image based PA)
  WIDGET_CONTROL, wS2IRAC_PA_checkbox, SET_UVALUE=0                   ;Default ignore IRAC PA
  
  ;Set the default AMR image set to the unrotated images
  WIDGET_CONTROL, wS4OriginalButton, /SET_BUTTON
  WIDGET_CONTROL, wS4ImageBase, SET_UVALUE=0
  
  ;Align the button bases
  AlignColumns, wLoadSummaryBase
  AlignColumns, wS2ButtonBase
  AlignColumns, wS3AbuttonBase
  AlignColumns, wS3BbuttonBase
  AlignColumns, wS4ButtonBase
  AlignColumns, wS4Rebin
  AlignColumns, wS4Adaptive
  AlignColumns, wS4Smooth

  XMANAGER, 'PEGS_POL_GUI', wTLB, /NO_BLOCK; CATCH = 0, NO_BLOCK = 0

  PRINT_TEXT2, {top:wTLB}, 'Welcome to ' + versionString
  PRINT_TEXT2, {top:wTLB}, 'Current program operating out of ' + launchDirectory

END