; This script will edit the PPOL "Group_Summary.sav" following this algorithm
; 
; 1. Check if "Group_Summary.old.sav" exists
; 2. If it does exist, then treat that file as the original
; 3. Save a new "Group_Summary.sav" file using relative path references
;         (e.g. BDP path = PPOL_path + ".." + PATH_SEP() + "BPD_reduced" + PATH_SEP() + filename)
;


PRO UNDEFINE, varname
  tempvar = SIZE(TEMPORARY(varname))
END

searchDirectory = DIALOG_PICKFILE(/DIRECTORY, TITLE = 'Select the PEGS data location')

;********** FIST FIX THE PPOL GROUP SUMMARIES **********
summaryFiles = FILE_SEARCH(searchDirectory, 'Group_Summary.sav', COUNT = numFiles)
summaryFiles = summaryFiles[SORT(summaryFiles)]

FOR i = 0, numFiles - 1 DO BEGIN
  pathArray       = STRSPLIT(summaryFiles[i], PATH_SEP(), /EXTRACT)   ;Break apart the path to the summary file
  summaryInd      = WHERE(pathArray EQ 'Group_Summary.sav')           ;Find the summary file location
  IF pathArray[summaryInd-1] NE 'S1_Image_Groups_and_Meta_Groups' THEN CONTINUE ;Skip files in a goofy location
  PPOL_dir        = STRJOIN(pathArray[0:(summaryInd-2)], PATH_SEP())  ;Rebuild the PPOL directory based on summary file
  BDP_guess       = STRJOIN(pathArray[0:(summaryInd-3)], PATH_SEP()) $;Attempt to rebuild the BDP directory based on that information.
    + PATH_SEP() + 'BDP_reduced'
  IF ~FILE_TEST(BDP_guess, /DIRECTORY) THEN BEGIN
    BDP_guess = DIALOG_PICKFILE(/DIRECTORY, TITLE = 'Select BDP data for ' + PPOL_dir)
  ENDIF
  
  RESTORE, summaryFiles[i]                                  ;Read in the original group summary

  ;Modify the group summary to includ the new PEGS drive location
  new_group_images        = (*G_PTR).group_images         ;Create an aliased set of image names
  FOR j = 0, (*G_PTR).N_GROUPS - 1 DO BEGIN
    PRINT, 'Fixing drive letter for group ' + (*G_PTR).group_names[j]
    FOR k = 0, 511 DO BEGIN
      IF STRLEN(new_group_images[j,k]) GT 0 THEN BEGIN
        temp_image   = STRSPLIT(new_group_images[j,k], PATH_SEP(), /EXTRACT)
        fitsExten    = WHERE(STRMID(temp_image, 3, 4, /REVERSE) EQ 'fits')
        temp_image   = BDP_guess + PATH_SEP() + temp_image[fitsExten]
        new_group_images[j,k] = temp_image
      ENDIF
    ENDFOR
  ENDFOR
  
  (*G_PTR).group_images = new_group_images
  
  ;Save the modified group summary
  SAVE, G_PTR, DESCRIPTION="PPOL Group Summary File ", FILENAME = summaryFiles[i]
  PTR_FREE, (G_PTR)                                         ;Free the heap space
  UNDEFINE, (G_PTR)                                         ;Undefine the G_PTR variable
ENDFOR

;********** NOW FIX THE PEGS POL GROUP SUMMARIES **********
summaryFiles = FILE_SEARCH(searchDirectory, 'PEGS_POL_Group_Summary.sav', COUNT = numFiles)
summaryFiles = summaryFiles[SORT(summaryFiles)]

FOR i = 0, numFiles - 1 DO BEGIN
  pathArray       = STRSPLIT(summaryFiles[i], PATH_SEP(), /EXTRACT)   ;Break apart the path to the summary file
  summaryInd      = WHERE(pathArray EQ 'PEGS_POL_Group_Summary.sav')           ;Find the summary file location
  IF pathArray[summaryInd-1] NE 'S1_Image_Groups_and_Meta_Groups' THEN CONTINUE ;Skip files in a goofy location
  PPOL_dir        = STRJOIN(pathArray[0:(summaryInd-2)], PATH_SEP())  ;Rebuild the PPOL directory based on summary file
  BDP_guess       = STRJOIN(pathArray[0:(summaryInd-3)], PATH_SEP()) $;Attempt to rebuild the BDP directory based on that information.
    + PATH_SEP() + 'BDP_reduced'
  IF ~FILE_TEST(BDP_guess, /DIRECTORY) THEN BEGIN
    BDP_guess = DIALOG_PICKFILE(/DIRECTORY, TITLE = 'Select BDP data for ' + PPOL_dir)
  ENDIF
  
  RESTORE, summaryFiles[i]                                  ;Read in the original group summary
;  temp_dir = STRSPLIT(groupStruc.analysis_dir, PATH_SEP(), /EXTRACT)
;  temp_dir = PEGSdrive + STRJOIN(temp_dir[1:*], PATH_SEP()) + PATH_SEP()
  
  groupStruc.analysis_dir = PPOL_dir + PATH_SEP()
  
  ;Modify the group summary to includ the new PEGS drive location
  new_group_images        = groupStruc.groupImages         ;Create an aliased set of image names
  FOR j = 0, groupStruc.numGroups - 1 DO BEGIN
    PRINT, 'Fixing drive letter for group ' + groupStruc.groupNames[j]
    FOR k = 0, N_ELEMENTS(groupStruc.groupImages[j,*]) - 1 DO BEGIN
      IF STRLEN(new_group_images[j,k]) GT 0 THEN BEGIN
        temp_image   = STRSPLIT(new_group_images[j,k], PATH_SEP(), /EXTRACT)
        fitsExten    = WHERE(STRMID(temp_image, 3, 4, /REVERSE) EQ 'fits')
        temp_image   = BDP_guess + PATH_SEP() + temp_image[fitsExten]
        new_group_images[j,k] = temp_image
      ENDIF
    ENDFOR
  ENDFOR
  groupStruc.groupImages = new_group_images
  UPDATE_GROUP_SUMMARY, groupStruc, /SAVE
ENDFOR

PRINT, 'Done!'

END