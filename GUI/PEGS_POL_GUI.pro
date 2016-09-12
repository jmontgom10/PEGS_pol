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
  wS3AButtonBase      = WIDGET_BASE(wS3Arepair, /COLUMN, /ALIGN_LEFT, /BASE_ALIGN_LEFT)
  
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
;  wS3Aspace           = WIDGET_LABEL(wS3Arow3, VALUE='')
  
  wS3Arow4            = WIDGET_BASE(wS3AbuttonBase, /ROW)
  wS3Alabel4          = WIDGET_LABEL(wS3Arow4, VALUE = '3. Check Astrometry')
  wS3AcheckButtons    = WIDGET_BASE(wS3Arow4, /ROW)
  wS3AcheckAstro      = WIDGET_BUTTON(wS3AcheckButtons, VALUE = 'Start', EVENT_PRO='S3A_CHECK_ASTROMETRY')
  wS3AgoodAstro       = WIDGET_BUTTON(wS3AcheckButtons, VALUE = 'Good', UNAME='S3A_CHECK_GOOD')
  wS3AbadAstro        = WIDGET_BUTTON(wS3AcheckButtons, VALUE = 'Bad', UNAME='S3A_CHECK_BAD')
  
  wS3Arow5            = WIDGET_BASE(wS3AbuttonBase, /ROW)
  wS3Alabel5          = WIDGET_LABEL(wS3Arow5, VALUE='Solved astrometry information')
  wS3AinfoBase        = WIDGET_BASE(wS3Arow5, COLUMN=2, UNAME='S3A_ASTROMETRY_BASE')
;  wAstrometryTextBase = WIDGET_BASE(wS3AinfoBase, COLUMN=2)
  wCenterRALabel      = WIDGET_LABEL(wS3AinfoBase, VALUE='RA')
  wCenterRAText       = WIDGET_TEXT(wS3AinfoBase, XSIZE=14, UNAME='S3A_RA_TEXT')
  wPlateScaleLabel    = WIDGET_LABEL(wS3AinfoBase, VALUE='arcsec/pix')
  wPlateScaleText     = WIDGET_TEXT(wS3AinfoBase, XSIZE=14, UNAME='S3A_PLATE_SCALE')
  wCenterDecLabel     = WIDGET_LABEL(wS3AinfoBase, VALUE='Dec')
  wCenterDecText      = WIDGET_TEXT(wS3AinfoBase, XSIZE=14, UNAME='S3A_DEC_TEXT')
  wRotAngleLabel      = WIDGET_LABEL(wS3AinfoBase, VALUE='Rot. (deg.)')
  wRotAngleText       = WIDGET_TEXT(wS3AinfoBase, XSIZE=14, UNAME='S3A_ROT_ANGLE')
  
  
  ;***STEP 3.B TAB*************************************************************
  wS3Brepair           = WIDGET_BASE(wPreTabs, TITLE='Supersky Subtraction', /ROW, /ALIGN_CENTER)
  wS3BButtonBase       = WIDGET_BASE(wS3Brepair, /COLUMN, /ALIGN_LEFT, /BASE_ALIGN_LEFT)
;  wS3B1Label           = WIDGET_LABEL(wS3BbuttonBase, VALUE=NEW_LINE()+'1. (Optional) Protect galaxy from supersky', YSIZE=30)
;  wS3BMaskCheckBox     = CW_BGROUP(wS3BButtonBase, ['Mask Galaxy'], $
;    /NONEXCLUSIVE, UNAME='GALAXY_MASK_CHECKBOX', EVENT_FUNCT='S3B_MASK_CHECK')
;  wS3BMakeMask         = WIDGET_BUTTON(wS3BButtonBase, VALUE='Build Galaxy Mask', $
;    SENSITIVE = 0, UNAME='S3B_BUILD_GALAXY_MASK', EVENT_PRO='S3B_BUILD_GALAXY_MASK')
  wS3B2Label           = WIDGET_LABEL(wS3BbuttonBase, VALUE='1. Flatten images with supersky procedure', YSIZE=30)
  wS3BSuperskyFlatten  = WIDGET_BUTTON(wS3BButtonBase, VALUE='Supersky Subtraction', $
    SENSITIVE = 1, UNAME='S3B_SUBTRACT_SUPERSKY', EVENT_PRO='S3B_SUBTRACT_SUPERSKY')
;  wS3B3Label           = WIDGET_LABEL(wS3BbuttonBase, VALUE=NEW_LINE()+'3. Refine image astrometry', YSIZE=30)
;  wS3BRefineAstrometry = WIDGET_BUTTON(wS3BButtonBase, VALUE='Refine Astrometry', EVENT_PRO='S3B_REFINE_ASTROMETRY')
  wS3B3Label           = WIDGET_LABEL(wS3BbuttonBase, VALUE='Current files in S3 directory', YSIZE=30)
  wS3BSwapFilesText    = WIDGET_TEXT(wS3BButtonBase, XSIZE=30, UNAME='S3B_S3_CURRENT_FILES')
  wS3BSwapFilesButton  = WIDGET_BUTTON(wS3BButtonBase, VALUE='Swap Files', EVENT_PRO='S3B_SWAP_FILES')

  
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
  

;  wS3AmagRangeSlider = CW_DUAL_SLIDER(wS3Arow2, minimum=0E, maximum=14E, value=FLOAT([1,13]), $
;    XSIZE = 175, EVENT_PRO='S3A_MAGNITUDE_RANGE', UNAME='S3A_MAG_RANGE', TITLE='Magnitude Range', SENSITIVE = 0)
;  wS3AcheckboxBase   = WIDGET_BASE(wS3Arow2, /NONEXCLUSIVE, /ALIGN_CENTER, /BASE_ALIGN_CENTER)
;  wS3Acheckbox       = WIDGET_BUTTON(wS3AcheckboxBase, VALUE='Slider Inactive', $
;    EVENT_PRO='S3A_SELECT_ASTROMETRY_MAGNITUDE_RANGE')

  
;  wS41Label          = WIDGET_LABEL(wS4buttonBase, VALUE=NEW_LINE()+'1. Determine dither boundaries', YSIZE=30)
;  wS4DitherBounds    = WIDGET_BUTTON(wS4ButtonBase, VALUE='Find dither boundaries', EVENT_PRO='S4_DETERMINE_BOUNDARIES')
;  wS42Label          = WIDGET_LABEL(wS4buttonBase, VALUE=NEW_LINE()+'1. Select the stars to use for photometry', YSIZE=30)
;  wS4magRangeSlider  = CW_DUAL_SLIDER(wS4ButtonBase, minimum=0E, maximum=14E, value=FLOAT([1,13]), $
;    XSIZE = 175, MAX_DIFFERENCE=2, EVENT_PRO='S4_MAGNITUDE_RANGE', UNAME='S4_MAG_RANGE', TITLE='Magnitude Range', SENSITIVE = 0)
;  wS4SelectPhotStars = WIDGET_BUTTON(wS4buttonBase, VALUE='Select magnitude range', $
;    EVENT_PRO='S4_SELECT_PHOTOMETRY_MAGNITUDE_RANGE')
;  wS4SelectPhotStars = WIDGET_BUTTON(wS4ButtonBase, VALUE='Get Photometry Stars', EVENT_PRO='S4_GET_PHOTOMETRY_STARS')

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
  
  wPost1Label3        = WIDGET_LABEL(wPost1, VALUE=NEW_LINE()+'3. Final photometric scaling', YSIZE=30)
  wPhotometry         = WIDGET_BUTTON(wPost1, VALUE='Start Photometry', EVENT_PRO='S1_FINAL_PHOTOMETRY')
  
  ;***STEP 2 TAB***************************************************************
  wStep2            = WIDGET_BASE(wPostTabs, TITLE='Rotate Stokes Images', /COLUMN, /BASE_ALIGN_CENTER)
  wS2galPAlabel     = WIDGET_LABEL(wStep2, VALUE='Galaxy PA (degrees CCW)')
  wS2galPAtext      = WIDGET_TEXT(wStep2, XSIZE=20, UNAME='GALAXY_PA')
  wS2galCenterBase  = WIDGET_BASE(wStep2, COLUMN=2)  
  wS2galXlabel      = WIDGET_LABEL(wS2galCenterBase, VALUE='Galaxy X Center (pix)')
  wS2galXtext       = WIDGET_TEXT(wS2galCenterBase, XSIZE=20, UNAME='GALAXY_X_PIXEL')
  wS2galYlabel      = WIDGET_LABEL(wS2galCenterBase, VALUE='Galaxy Y Center (pix)')
  wS2galYtext       = WIDGET_TEXT(wS2galCenterBase, XSIZE=20, UNAME='GALAXY_Y_PIXEL')

  wS2blank          = WIDGET_LABEL(wStep2, VALUE=' ')
  wS2cheCkBoxBase   = WIDGET_BASE(wStep2, /NONEXCLUSIVE)
  wS2checkBox       = WIDGET_BUTTON(wS2checkBoxBase, VALUE='Use Model Values.', $
    UNAME='S2_USE_MODEL_VALUES', EVENT_PRO='S2_USE_MODEL_VALUES')
  
  wS2rotPAlabel     = WIDGET_LABEL(wStep2, VALUE='Rotation Angle (degrees CCW)')
  wS2rotPAtext      = WIDGET_TEXT(wStep2, XSIZE=20, /EDITABLE, UNAME='ROTATION_ANGLE')
  wS2rotCenterBase  = WIDGET_BASE(wStep2, COLUMN=2)  
  wS2rotXlabel      = WIDGET_LABEL(wS2rotCenterBase, VALUE='Rotation X Center (pix)')
  wS2rotXtext       = WIDGET_TEXT(wS2rotCenterBase, XSIZE=20, /EDITABLE, UNAME='ROTATION_X_PIXEL')
  wS2rotYlabel      = WIDGET_LABEL(wS2rotCenterBase, VALUE='Rotation Y Center (pix)')
  wS2rotYtext       = WIDGET_TEXT(wS2rotCenterBase, XSIZE=20, /EDITABLE, UNAME='ROTATION_Y_PIXEL')
  wS2rotateImages   = WIDGET_BUTTON(wStep2, VALUE='Rotate Images', EVENT_PRO='S2_ROTATE_IMAGES')
  
  ;***STEP 3 TAB***************************************************************
  wStep3            = WIDGET_BASE(wPostTabs, TITLE='Smooth and/or Rebin', /COLUMN)
  wS3ImageLabel     = WIDGET_LABEL(wStep3, VALUE='Select the images on which to operate')
  wS3ImageBase      = WIDGET_BASE(wStep3, /ROW, /EXCLUSIVE, EVENT_PRO='S3_SELECT_INPUT_IMAGES', UNAME='S3_SELECT_INPUT_IMAGES')
  wS3OriginalButton = WIDGET_BUTTON(wS3ImageBase, VALUE='Original images', UNAME='S3_ORIGINAL_IMAGES')
  wS3RotatedButton  = WIDGET_BUTTON(wS3ImageBase, VALUE='Rotated images', UNAME='S3_ROTATED_IMAGES')
  wS3MethodLabel    = WIDGET_LABEL(wStep3, VALUE='Select which processing method to use')
  wS3MethodTabs     = WIDGET_TAB(wStep3, LOCATION=0, EVENT_PRO='S3_SELECT_PROCESSING_METHOD', UNAME='S3_METHOD_TABS')
  WIDGET_CONTROL, wS3MethodTabs, SET_UVALUE=0


  wS3Rebin            = WIDGET_BASE(wS3MethodTabs, TITLE='Integer Pixel Rebin', /COLUMN)
  wS3RebinNumPixBase  = WIDGET_BASE(wS3Rebin, /ROW)
  wS3RebinNumPixLabel = WIDGET_LABEL(wS3RebinNumPixBase, VALUE='Number of pixels to rebin')
  wS3RebinNumPix      = WIDGET_TEXT(wS3RebinNumPixBase, XSIZE=8, /EDITABLE, UNAME='REBIN_PIXELS', EVENT_PRO='S3_SET_REBIN_PIXELS')
  wS3RebinOldPSbase   = WIDGET_BASE(wS3Rebin, /ROW)
  wS3RebinOldPSlabel  = WIDGET_LABEL(wS3RebinOldPSbase, VALUE='Old Plate Scale')
  wS3RebinOldPS       = WIDGET_TEXT(wS3RebinOldPSbase, XSIZE=8, UNAME='OLD_PLATE_SCALE')
  wS3RebinNewPSbase   = WIDGET_BASE(wS3Rebin, /ROW)
  wS3RebinNewPSlabel  = WIDGET_LABEL(wS3RebinNewPSbase, VALUE='New Plate Scale')
  wS3RebinNewPS       = WIDGET_TEXT(wS3RebinNewPSbase, XSIZE=8, UNAME='NEW_PLATE_SCALE')
  
  
  wS3Adaptive         = WIDGET_BASE(wS3MethodTabs, TITLE='Adaptive Mesh', /COLUMN)
  wS3smallestBinBase  = WIDGET_BASE(wS3Adaptive, /ROW)
  wS3smallestBinLabel = WIDGET_LABEL(wS3smallestBinBase, VALUE='Smallest rebinning size')
  wS3smallestBin      = WIDGET_TEXT(wS3smallestBinBase, XSIZE=6, /EDITABLE, $
    UNAME='SMALLEST_MESH_BIN', EVENT_PRO='S3_SET_SMALLEST_MESH_BIN')
  wS3numLevelsBase    = WIDGET_BASE(wS3Adaptive, /ROW)
  wS3numLevelsLabel   = WIDGET_LABEL(wS3numLevelsBase, VALUE='Number of Rebin Levels')
  wS3numLevels        = WIDGET_TEXT(wS3numLevelsBase, XSIZE=6, /EDITABLE, $
    UNAME='NUMBER_REBIN_LEVELS', EVENT_PRO='S3_SET_NUMBER_REBIN_LEVELS')
  wS3SNRcutoffBase    = WIDGET_BASE(wS3Adaptive, /ROW)
  wS3SNRcutoffLabel   = WIDGET_LABEL(wS3SNRcutoffBase, VALUE='Minimum allowable SNR')
  wS3SNRcutoff        = WIDGET_TEXT(wS3SNRcutoffBase, XSIZE=6, /EDITABLE, $
    UNAME='MINIMUM_MESH_SNR', EVENT_PRO='S3_SET_MINIMUM_MESH_SNR')
  
  
  wS3Smooth              = WIDGET_BASE(wS3MethodTabs, TITLE='Gaussian Smooth and Rebin', /COLUMN)
  wS3KernelWidthBase     = WIDGET_BASE(wS3Smooth, /ROW)
  wS3KernelWidthLabel    = WIDGET_LABEL(wS3KernelWidthBase, VALUE='Kernel FWHM (arcsec)')
  wS3KernelWidth         = WIDGET_TEXT(wS3KernelWidthBase, XSIZE=6, /EDITABLE)
  wS3KernelWidthLabelPix = WIDGET_LABEL(wS3KernelWidthBase, VALUE='Kernel FWHM (pixels)')
  wS3KernelWidthPix      = WIDGET_TEXT(wS3KernelWidthBase, XSIZE=6)
  wS3SamplePitchBase     = WIDGET_BASE(wS3Smooth, /ROW)
  wS3SamplePitchLabel    = WIDGET_LABEL(wS3SamplePitchBase, VALUE='Resample pitch (arcsec)')
  wS3SamplePitch         = WIDGET_TEXT(wS3SamplePitchBase, XSIZE=6, /EDITABLE)
  wS3SamplePitchLabelPix = WIDGET_LABEL(wS3SamplePitchBase, VALUE='Resample pitch (pixels)')
  wS3SamplePitchPixl     = WIDGET_TEXT(wS3SamplePitchBase, XSIZE=6)

  wS3StartBase           = WIDGET_BASE(wStep3, /COLUMN)
  wS3StartButon          = WIDGET_BUTTON(wS3StartBase, VALUE='Start Processing', EVENT_PRO='S3_START_PROCESSING')
  
  ;Produce a display window that will be used for ALL analysis steps
  wImageWindow           = WIDGET_DRAW(wAnalysisWidgets, $
    XSIZE=12, YSIZE=12, $
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

  ;Set default masking procedure (ignore IRAC image based PA)
  WIDGET_CONTROL, wS2IRAC_PA_checkbox, SET_UVALUE=0                   ;Default ignore IRAC PA
  
  ;Set the default AMR image set to the unrotated images
  WIDGET_CONTROL, wS3OriginalButton, /SET_BUTTON
  WIDGET_CONTROL, wS3ImageBase, SET_UVALUE=0
  
  ;Align the button bases
  AlignColumns, wLoadSummaryBase
  AlignColumns, wS2ButtonBase
  AlignColumns, wS3AbuttonBase
  AlignColumns, wS4ButtonBase
  AlignColumns, wS3Rebin
  AlignColumns, wS3Adaptive
  AlignColumns, wS3Smooth

  XMANAGER, 'PEGS_POL_GUI', wTLB, /NO_BLOCK; CATCH = 0, NO_BLOCK = 0

  PRINT_TEXT2, {top:wTLB}, 'Welcome to ' + versionString
  PRINT_TEXT2, {top:wTLB}, 'Current program operating out of ' + launchDirectory

END