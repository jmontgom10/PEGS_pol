PRO OPEN_PPOL_PROJECT, event

  ; Save the current directory and querry for the new project directory
  CD, CURRENT=start_directory
  FINDPRO, 'PEGS_POL_GUI', DIRLIST=PEGS_POL_dir, /NOPRINT
  IF N_ELEMENTS(PEGS_POL_dir) NE 1 THEN STOP ELSE PEGS_POL_dir = PEGS_POL_dir[0]
  analysis_dir = DIALOG_PICKFILE(PATH=start_directory, $
    TITLE='Select PPOL analysis directory', /DIRECTORY)

  ; Change into that directory
  PPOLsummary = analysis_dir + $
    'S1_Image_Groups_and_Meta_Groups' + PATH_SEP() + 'Group_Summary.sav'
  IF FILE_TEST(PPOLsummary) THEN BEGIN                                  ;Check if this is a fully functioning PPOL directory
    PRINT_TEXT2, event, 'Found working analysis directory'
;    CD, analysis_dir
    group_summary = analysis_dir + $                                    ;The path to save PPOL+ group summary
      'S1_Image_Groups_and_Meta_Groups' + PATH_SEP() + 'PEGS_POL_Group_Summary.sav'
    
    IF FILE_TEST(group_summary) THEN BEGIN
      PRINT_TEXT2, event, 'Found previous PEGS POL group summary'
      RESTORE, group_summary
      IF groupStruc.analysis_dir NE analysis_dir THEN $
        UPDATE_PPOL_PATH, analysis_dir
      
      UPDATE_LOAD_TAB, event, groupStruc
      UPDATE_GROUP_SUMMARY, event, groupStruc
    ENDIF ELSE BEGIN
      group_summary = analysis_dir + PATH_SEP() + $
        'S1_Image_Groups_and_Meta_Groups' + PATH_SEP() + 'Group_Summary.sav'
      RESTORE, group_summary
      
      PRINT_TEXT2, event, 'Generating PEGS POL group summary from PPOL summary file'
      
      numGroups    = (*G_PTR).N_GROUPS                                  ;Number of PPOL groups
      groupNames   = (*G_PTR).GROUP_NAMES[0:numGroups-1]                ;Name of each PPOL group
      groupNumbers = (*G_PTR).group_numbers[0:numGroups-1]              ;Number of images in each group
      maxGroup     = MAX(groupNumbers)                                  ;Grab the size of the LARGEST group
      groupFlags   = BYTARR(numGroups)+1B                               ;Initalize array for storing group usage flags
      groupImages  = STRARR(numGroups, maxGroup)                        ;Initalize array for storing image names
      imageFlags   = INTARR(numGroups, maxGroup)                        ;Initalize array for image use flags
      astroFlags   = INTARR(numGroups, maxGroup) + 1                    ;Initalize array for astrometry success flags
      metaData     = REPLICATE({telRA:-99D, telDec:-99D, NIRband:''}, $ ;Initalize array of structures to store meta-data
        numGroups, maxGroup)

      PRINT_TEXT2, event, 'Reading image headers to estimate median pointing.'
      FOR i = 0, numGroups - 1 DO BEGIN
        groupImages[i,0:maxGroup-1] = (*G_PTR).GROUP_IMAGES[i,0:maxGroup-1]
        imageFlags[i,0:maxGroup-1]  = (*G_PTR).GROUP_IMAGE_FLAGS[i,0:maxGroup-1]
        FOR j = 0 , groupNumbers[i] - 1 DO BEGIN
          tempHeader = HEADFITS(groupImages[i,j])
          ;Convert coordinates to decimal degrees for averaging
          tempRA     = 15D*TEN(STRSPLIT(SXPAR(tempHeader, 'TELRA'), ':', /EXTRACT))
          tempDEC    =     TEN(STRSPLIT(SXPAR(tempHeader, 'TELDEC'), ':', /EXTRACT))
          metaData[i,j].telRA   = tempRA                                ;Store the telescope RA pointing
          metaData[i,j].telDec  = tempDec                               ;Store the telescope Dec pointing
          metaData[i,j].NIRband = SXPAR(tempHeader, 'FILTNME2')         ;Store the filterwheel 2 value
        ENDFOR
      ENDFOR
      
      SAVE, G_PTR, DESCRIPTION="PPOL Group Summary File ", FILENAME = group_summary
      PTR_FREE, (G_PTR)                                                 ;Free the heap space

      ;Test for uniq NIR band
      NIRband = metadata[UNIQ(metaData.NIRband)].NIRband
      nullInd = WHERE(NIRband EQ '', numNull, COMPLEMENT=notNullInd)
      IF numNull GT 0 THEN NIRbands = NIRband[notNullInd]
      IF N_ELEMENTS(bands) GT 1 THEN BEGIN
        PRINT_TEXT2, event, 'Found more than one NIR band in this project.'
        PRINT_TEXT2, event, 'Please re-arrange your PPOL data so PPOL+ can operate one one band per project.'
        RETURN
      ENDIF ELSE NIRband = STRTRIM(NIRband[0], 2)
      
      ;Compute MEDIAN pointing
      goodInds = WHERE((metaData.telRA NE -99D) AND (metaData.telRA NE -99D), numGood)
      IF numGood GT 0 THEN BEGIN
        medianRA  = MEDIAN(metaData[goodInds].telRA)
        medianDec = MEDIAN(metaData[goodInds].telDec)
      ENDIF ELSE BEGIN
        PRINT_TEXT2, event, 'No good (RA, Dec) values'
        RETURN
      ENDELSE

      ;Grab all the good stars within 12-arcmin of the median pointing.
      ;Retrieve the 2MASS star info
;      this_mirror = ['CfA', 'UK', 'CDS', 'CA', 'Jp']
      this_mirror = ['UK', 'CDS', 'CA', 'Jp']
      ;
      nloop = 0L
      WHILE nloop LT 4320L DO BEGIN                                       ; 1 day at 20 sec per try
        ;
        vizier_flag = 0
        FOR imirror = 0, N_ELEMENTS(this_mirror)-1 DO BEGIN
          IF(vizier_flag EQ 0) THEN BEGIN
            info = mp_QueryVizier('2MASS-PSC', [medianRA,medianDec], [16,16], $
              MIRROR=this_mirror[imirror], constraint='Qflg==AAA')
            test = size(info, /TYPE)
            IF (test EQ 8) THEN BEGIN
              IF TOTAL(info.DEJ2000 GT 90) EQ 0 THEN vizier_flag = 1
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
        ENDIF ELSE nloop = 4321L                                ;Force the loop to close
      ENDWHILE
      
      IF NIRband EQ 'H' THEN magIndex = 5 $                     ;Define structure tag number for THIS band
        ELSE IF NIRband EQ 'Ks' THEN magIndex = 7
      info       = info[SORT(info.(magIndex))]                  ;Sort the stars by brightness in THIS band

      groupStruc = { $
        PEGS_POL_dir:PEGS_POL_dir, $
        analysis_dir:analysis_dir, $
        objectName:'', $
        medianRA:medianRA, $
        medianDec:medianDec, $
        starInfo:info, $
        astroStarFlags:(1B+BYTARR(N_ELEMENTS(info))), $
        photStarFlags:(1B+BYTARR(N_ELEMENTS(info))), $
        coverageBoundaries:{RA:[0D,0D], Dec:[0D,0D]}, $
        NIRband:NIRband, $
        stepCompletion:'', $
;        numS3failed:-1L, $
        currentS3files:'not supersky flattened', $
        numGroups:numGroups, $
        groupNames:groupNames, $
        groupNumbers:groupNumbers, $
        groupFlags:groupFlags, $
        groupImages:groupImages, $
        imageFlags:imageFlags, $
        astroFlags:astroFlags, $
        finalPlateScale:!VALUES.F_NAN $
        }
      
      
      PRINT_TEXT2, event, 'Testing for failed astrometry images'
      FOR i = 0, groupStruc.numGroups - 1 DO BEGIN
        FOR j = 0, groupStruc.groupNumbers[i] - 1 DO BEGIN
          S3filename = groupStruc.analysis_dir + PATH_SEP() + $         ;Setup the path to the file to be tested
            'S3_Astrometry' + PATH_SEP() + FILE_BASENAME(groupStruc.groupImages[i,j])
          S3fileTest = FILE_TEST(S3filename)                            ;Check if that file even exists (passed PPOL step 3)
          IF S3fileTest THEN BEGIN
            header  = HEADFITS(S3filename)                              ;Check if the existing file has reliable astrometry
            numStar = SXPAR(header, 'PPOLNSTR')
            IF numStar LT 3 THEN groupStruc.astroFlags[i,j] = 0         ;Mark all files that fail these tests
          ENDIF ELSE groupStruc.astroFlags[i,j] = 0
        ENDFOR
      ENDFOR
;      numFailed    = TOTAL(groupStruc.astroFlags EQ 0)                  ;Count the number of failed images
;      groupStruc.numS3failed = LONG(numFailed)                          ;Store the number of failed images
;      UPDATE_GROUP_SUMMARY, event, groupStruc                           ;Save the failed image number to disk
      
      UPDATE_GROUP_SUMMARY, event, groupStruc
      UPDATE_LOAD_TAB, event, groupStruc
    ENDELSE

  ENDIF ELSE BEGIN
    PRINT_TEXT2, event, analysis_dir + ' is not  PPOL directory'
    RETURN
  ENDELSE
END

PRO OPEN_EDIT_FLAGS, event

  WIDGET_CONTROL, event.top, GET_UVALUE=groupStruc
  IF N_ELEMENTS(groupStruc) GT 0 THEN BEGIN
    
    updateInfo = DIALOG_EDIT_ASTRO_FLAGS(groupStruc, GROUP_LEADER = event.top)
    updateType = SIZE(updateInfo, /TYPE)
    
    IF updateType EQ 1 THEN BEGIN
      PRINT_TEXT2, event, 'Canceled group info update.'
    ENDIF ELSE IF updateType EQ 8 THEN BEGIN
      UPDATE_GROUP_SUMMARY, event, updateInfo
      PRINT_TEXT2, event, 'Group info updated with edited values'
    ENDIF
  ENDIF

END

PRO RENAME_OBJECT, event, objectName

  tlb_wid         = WIDGET_INFO(event.top, FIND_BY_UNAME='WID_BASE')        ;Grabthe top-level-base WID
  objectNameWID   = WIDGET_INFO(event.top, FIND_BY_UNAME='LOAD_NAME_TEXT')  ;Grab the name text widget-ID
  WIDGET_CONTROL, tlb_wid, GET_UVALUE = groupStruc                          ;Grab the group structure

  IF N_ELEMENTS(groupStruc) GT 0 THEN BEGIN
    WIDGET_CONTROL, objectNameWID, SET_VALUE = 'Enter Object Name'            ;Alarm the user
    WIDGET_CONTROL, objectNameWID, EDITABLE = 1                               ;Sensitize the text box
    
    renameEvent = WIDGET_EVENT(objectNameWID)
    
    WIDGET_CONTROL, objectNameWID, EDITABLE = 0                               ;Desensitize the text box
    WIDGET_CONTROL, objectNameWID, GET_VALUE = objectName                     ;Access the new name
    
    UPDATE_GROUP_SUMMARY, event, groupStruc, 'objectName', objectName, /SAVE  ;Store the updated data to disk
  ENDIF

END