PRO UPDATE_GROUP_SUMMARY, event, groupStruc, tag_name, tag_value, SAVE=save

  ;***** FIRST UPDATE THE GROUP STRUCTURE ITSELF *****
  ;Check if the user supplied additional tags to include in the structure
  IF (N_PARAMS() GE 4)  AND $                               ;Check that all the parameters have been provided
    (N_ELEMENTS(tag_name) GT 0) AND $                       ;and that the number of tags
    (N_ELEMENTS(tag_value) GT 0) AND $                      ;matches the number of values
    (N_ELEMENTS(tag_name) EQ N_ELEMENTS(tag_value)) THEN BEGIN

    num_old_tags = N_TAGS(groupStruc)                       ;Count the number of tags originally in the structure
    old_tags     = TAG_NAMES(groupStruc)                    ;List the old tag names
    num_new_tags = N_ELEMENTS(tag_name)                     ;Count the number of tags to be included/updated

    ;***** ONE TAG PROVIDED *****    
    ;Handle the case of a single tag to be updated or appended
    IF (num_new_tags EQ 1) AND (SIZE(tag_name, /N_DIMENSIONS) EQ 0) THEN BEGIN
      tagUpdated = 0B                                       ;Initalize flag to track if the tag gets updated
      updateInd  = WHERE(old_tags EQ STRUPCASE(tag_name), numUpdate);Test if this tag already exists
      IF numUpdate EQ 1 THEN BEGIN                          ;If the tag exists,
        groupStruc.(updateInd) = tag_value                  ;then update it
        tagUpdated             = 1B                         ;and flag the tag as updated
      ENDIF ELSE BEGIN                                      ;If the tag was not updated,
        groupStruc = CREATE_STRUCT(groupStruc, tag_name, tag_value);then append to the structure
      ENDELSE
    
    ;***** MULTIPLE TAGS PROVIDED *****
    ;Handle the case of an array of tags to be updated and/or appended
    ENDIF ELSE IF (num_new_tags GT 1) THEN BEGIN            ;If there is more than one tag,
      tagUpdated = BYTARR(num_new_tags)                     ;Initalize array to track if the tag gets updated
      FOR i = 0, num_new_tags - 1 DO BEGIN                  ;then loop through all the supplied tags.
        updateInd = WHERE(old_tags EQ STRUPCASE(tag_name[i]), numUpdate);Test if this tag exists.
        IF numUpdate EQ 1 THEN BEGIN                        ;If the tag exists,
          groupStruc.(updateInd) = tag_value                ;then update the value
          tagUpdated[i] = 1B                                ;and flag the tag as updated
        ENDIF
      ENDFOR
      
      ;Now that all the tags that CAN be updated, HAVE been updated
      ;proceed to create a NEW structure containing all the old tags
      ;and appending all the new tags
      appendInds = WHERE(~tagUpdated, numAppend)            ;Locate which tags were not updated
      FOR i = 0, numAppend - 1 DO BEGIN                     ;Loop through the tags to be appended
        this_tag   = appendInds[i]                          ;Select each tag to append
        groupStruc = CREATE_STRUCT(groupStruc, tag_name[this_tag], tag_value[this_tag]);append the tag
      ENDFOR
    ENDIF
  ENDIF

  ;***** ONCE THE STRUCTURE IS UPDATED, SAVE IT TO DISK *****
  ;Check if only a groupStruc was supplied (outside of WIDGET mode, there is no "event")
  IF N_PARAMS() LT 2 THEN groupStruc = event ELSE BEGIN
    ;Store the group summary in the UVALUE of the top-level base
    tlb_wid = WIDGET_INFO(event.top, FIND_BY_UNAME='WID_BASE')
    WIDGET_CONTROL, tlb_wid, SET_UVALUE=groupStruc
  ENDELSE
  
  ;Check if the user requested that the groupStruc be saved to disk
  ;and the PPOL summary be updated to match the groupStruc contents.
  IF KEYWORD_SET(save) THEN BEGIN
    ;Save the group structure to disk
    outFile = groupStruc.analysis_dir + $
      'S1_Image_Groups_and_Meta_Groups' + PATH_SEP() + 'PEGS_POL_Group_Summary.sav'
    SAVE, groupStruc, FILENAME = outFile
    
    ;Read in the PPOL group summary
    PPOL_group_summary = groupStruc.analysis_dir + PATH_SEP() + $
      'S1_Image_Groups_and_Meta_Groups' + PATH_SEP() + 'Group_Summary.sav'
    RESTORE, PPOL_group_summary
    
    ;If the PPOL summary was successfully read, then update it and save to disk
    IF N_ELEMENTS(G_PTR) GT 0 THEN BEGIN
      ;Update the PPOL group summary image usage flags
      maxGroup = MAX(groupStruc.groupNumbers)
      FOR i = 0, groupStruc.numGroups - 1 DO BEGIN
        (*G_PTR).GROUP_IMAGE_FLAGS[i,0:maxGroup-1] = groupStruc.imageFlags[i,0:maxGroup-1]
      ENDFOR
      SAVE, G_PTR, FILENAME = PPOL_group_summary
    ENDIF
  ENDIF

END