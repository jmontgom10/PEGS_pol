PRO UNDEFINE, varname
  tempvar = SIZE(TEMPORARY(varname))
END

PRO UPDATE_PPOL_PATH, analysis_dir

  PPOLparts    = STRSPLIT(analysis_dir, PATH_SEP(), /EXTRACT, COUNT = numParts) ;Break apart the submitted path
  PPOL_summary  = STRJOIN(PPOLparts, PATH_SEP()) + PATH_SEP() + $      ;Build the path to the PPOL group summary 
    'S1_Image_Groups_and_Meta_Groups' + PATH_SEP() + 'Group_Summary.sav'
  PEGS_summary = STRJOIN(PPOLparts, PATH_SEP()) + PATH_SEP() + $     ;Build the path to the PPOL+ group summary
    'S1_Image_Groups_and_Meta_Groups' + PATH_SEP() + 'PEGS_POL_Group_Summary.sav'
    
  IF FILE_TEST(PPOL_summary) AND FILE_TEST(PEGS_summary) THEN BEGIN   ;Check if this is a fully functioning PPOL directory
    ;
    ;If the summary fiels were found, then break apart their paths and reconstruct the BDP directory from them
    ;
    BDPdir    = STRJOIN(PPOLparts[0:numParts-2], PATH_SEP()) $    ;Attempt to rebuild the BDP directory based on that information.
      + PATH_SEP() + 'BDP_reduced' + PATH_SEP()
      
    IF ~FILE_TEST(BDPdir, /DIRECTORY) THEN BEGIN
      BDPdir = DIALOG_PICKFILE(/DIRECTORY, TITLE = 'Select BDP data for ' + analysis_dir)
    ENDIF
    
    ;********** FIST FIX THE PPOL GROUP SUMMARIES **********
    RESTORE, PPOL_summary                                              ;Read in the original group summary

    new_group_images = (*G_PTR).group_images                          ;Create an aliased set of image names
    FOR j = 0, (*G_PTR).N_GROUPS - 1 DO BEGIN
      PRINT, 'Fixing paths for group ' + (*G_PTR).group_names[j]
      FOR k = 0, 511 DO BEGIN
        IF STRLEN(new_group_images[j,k]) GT 0 THEN BEGIN
          temp_image   = BDPdir + FILE_BASENAME(new_group_images[j,k])
          new_group_images[j,k] = temp_image
        ENDIF
      ENDFOR
    ENDFOR
    ;
    (*G_PTR).group_images = new_group_images                ;Assign new images to the pointer
    ;
    ;Save the modified group summary
    SAVE, G_PTR, DESCRIPTION="PPOL Group Summary File ", FILENAME = PPOL_summary
    PTR_FREE, (G_PTR)                                         ;Free the heap space
    UNDEFINE, (G_PTR)                                         ;Undefine the G_PTR variable
    
    
    ;****
    ;I need to identify which parts of the BDP paths were held in common with the analysis_dir
    ;and use that to construct the expected NEW BDP directory path.
    ;****
  ENDIF

;********** NOW FIX THE PEGS POL GROUP SUMMARIES **********
  RESTORE, PEGS_summary                                  ;Read in the original group summary
  groupStruc.analysis_dir = STRJOIN(PPOLparts, PATH_SEP()) + PATH_SEP()

  ;Modify the group summary to includ the new PPOL path
  new_group_images        = groupStruc.groupImages         ;Create an aliased set of image names
  FOR j = 0, groupStruc.numGroups - 1 DO BEGIN
    PRINT, 'Fixing paths for group ' + groupStruc.groupNames[j]
    FOR k = 0, N_ELEMENTS(groupStruc.groupImages[j,*]) - 1 DO BEGIN
      IF STRLEN(new_group_images[j,k]) GT 0 THEN BEGIN
        temp_image   = BDPdir + FILE_BASENAME(new_group_images[j,k])
        new_group_images[j,k] = temp_image
      ENDIF
    ENDFOR
  ENDFOR
  groupStruc.groupImages = new_group_images

  UPDATE_GROUP_SUMMARY, groupStruc, /SAVE

PRINT, 'Done!'

END