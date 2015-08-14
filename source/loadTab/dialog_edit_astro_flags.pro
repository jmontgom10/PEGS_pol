PRO DIALOG_EDIT_ASTRO_FLAGS_EVENT, event

  ; This event handler responds to all events. Widget
  ; is always destoyed. The text is recorded if ACCEPT
  ; button is selected or user hits CR in text widget.
  
  WIDGET_CONTROL, event.top, GET_UVALUE=info

  CASE event.ID OF
    info.groupListWID: BEGIN
      WIDGET_CONTROL, info.groupListWID, $                  ;Store the selected group index
        SET_UVALUE=event.index
      WIDGET_CONTROL, info.imageInfoWID, $                  ;Update the table display
        SET_VALUE=REFORM((*info.ptr).imageStruc[event.index,*])
    END
    info.toggleGroupWID: BEGIN
      IF event.select THEN BEGIN                            ;Double check for double clicks
        WIDGET_CONTROL, info.groupListWID, GET_UVALUE=groupIndex
        (*info.ptr).groupInfo[groupIndex].groupFlag = ~(*info.ptr).groupInfo[groupIndex].groupFlag
        
        groupValues = (*info.ptr).groupInfo.groupName + $
          ', USE=' + STRTRIM(FIX((*info.ptr).groupInfo.groupFlag), 2)
        
        WIDGET_CONTROL, info.groupListWID, SET_VALUE = groupValues
        WIDGET_CONTROL, info.groupListWID, SET_LIST_SELECT=groupIndex ;Reselect the group
      ENDIF
    END
    info.resetAllWid: BEGIN
      WIDGET_CONTROL, info.groupListWID, GET_UVALUE=index   ;Grab the selected group index
      (*info.ptr).imageStruc[index,*].imageFlag = 0B        ;Reset all the image flags of this group
      WIDGET_CONTROL, info.imageInfoWID, $                  ;Update the table display
        SET_VALUE=REFORM((*info.ptr).imageStruc[index,*])
    END
    info.imageInfoWID: BEGIN
      IF (event.type EQ 0) AND (event.X EQ 1) THEN BEGIN
        WIDGET_CONTROL, info.groupListWID, GET_UVALUE=index ;Grab the selected group index
        WIDGET_CONTROL, info.imageInfoWID, GET_VALUE=text   ;Retrieve all the text information

        IF (text[event.Y].imageFlag EQ 0) OR $              ;Test for acceptable text value
           (text[event.Y].imageFlag EQ 1) OR $
           (text[event.Y].imageFlag EQ 2) OR $
           (text[event.Y].imageFlag EQ 3) THEN BEGIN
           (*info.ptr).imageStruc[index,event.Y].imageFlag = text[event.Y].imageFlag
        ENDIF ELSE BEGIN                                    ;Otherwise reset flag to zero
          (*info.ptr).imageStruc[index,event.Y].imageFlag = 0B
          WIDGET_CONTROL, info.imageInfoWID, $              ;Restore group image information
            SET_VALUE=REFORM((*info.ptr).imageStruc[index,*])
        ENDELSE
        
      ENDIF ELSE IF (event.X EQ 0) THEN BEGIN               ;If the user tried to edit the image name...
        WIDGET_CONTROL, info.imageInfoWID, $                ;Restore group image information
        SET_VALUE=REFORM((*info.ptr).imageStruc[index,*])
      ENDIF
    END
    info.cancelWID: BEGIN
      WIDGET_CONTROL, event.top, /DESTROY                   ;Destroy the widget returning a "cancel" value
    END
    info.saveWID: BEGIN
      (*info.ptr).cancel = 0                                ;Accept the user supplied changes
      Widget_Control, event.top, /DESTROY                   ;Destroy the widget
    END
  ENDCASE
END

FUNCTION DIALOG_EDIT_ASTRO_FLAGS, groupStruc, GROUP_LEADER = group_leader
  
  numGroups            = groupStruc.numGroups
  numImages            = MAX(groupStruc.groupNumbers)
  imageStruc           = REPLICATE({imageName:' ', imageFlag:0B}, numGroups, numImages)
  imageStruc.imageName = FILE_BASENAME(groupStruc.groupImages, '.fits')
  imageStruc.imageFlag = groupStruc.astroFlags
  
  groupInfo            = REPLICATE({groupName:' ', groupFlag:0B}, numGroups)
  groupInfo.groupName  = groupStruc.groupNames
  groupInfo.groupFlag  = groupStruc.groupFlags
  
  TLB          = WIDGET_BASE(TITLE='Image flag editor', $   ;Create a top-level-base for this dialog
    GROUP_LEADER = group_leader, /COLUMN, /MODAL)

  listBase     = WIDGET_BASE(TLB, /ROW)                     ;Create a row base to show groups/images
  groupValues  = groupInfo.groupName + ', USE=' + STRTRIM(FIX(groupInfo.groupFlag), 2)

  groupListWID = WIDGET_LIST(listBase, VALUE=groupValues)   ;Create a list widget for the group names
  
  imageInfoWID = WIDGET_TABLE(listBase, $                   ;Create a table widget to show image info
    VALUE=REFORM(imageStruc[0,*]), $
    SCR_XSIZE = 180, COLUMN_LABELS=['Image Name', 'Flag'], COLUMN_WIDTH=[120,40], $
    SCR_YSIZE = 450, /ROW_MAJOR, /NO_ROW_HEADERS, /EDITABLE)
  buttonBase     = WIDGET_BASE(TLB, /ROW)                   ;Create a row base to store buttons
  saveWID        = WIDGET_BUTTON(buttonBase, VALUE='Save and Exit')     ;Save changes and exit
  cancelWID      = WIDGET_BUTTON(buttonBase, VALUE='Cancel')            ;Don't save changes and exit
  blankSpace     = WIDGET_LABEL(buttonBase, VALUE='    ')
  toggleGroupWID = WIDGET_BUTTON(buttonBase, VALUE='Toggle Group')      ;Switch group on/off
  resetAllWID    = WIDGET_BUTTON(buttonBase, VALUE='Reset Flags')       ;Reset all image flags to zero
    
  WIDGET_CONTROL, TLB, /REALIZE                             ;Realize the top-level-base
  WIDGET_CONTROL, groupListWID, SET_LIST_SELECT=0           ;Set the default group selection
  WIDGET_CONTROL, groupListWID, SET_UVALUE=0                ;Store the group selection in the UVALUE
  
  ptr  = PTR_NEW({imageStruc:imageStruc, $                  ;Create a pointer to store the flag info
    groupInfo:groupInfo, $
    cancel:1})
  info = {ptr:ptr, $                                        ;Create a structure to store the widget info
    groupListWID:groupListWID, imageInfoWID:imageInfoWID, $
    saveWID:saveWID, cancelWID:cancelWID, $
    toggleGroupWID:toggleGroupWID, resetAllWID:resetAllWID}
  WIDGET_CONTROL, TLB, SET_UVALUE=info, /NO_COPY            ;Store the widget info in the UVALUE
  
  XMANAGER, 'DIALOG_EDIT_ASTRO_FLAGS', TLB                  ;Manage this widget with XMANAGER

  cancel     = (*ptr).cancel

  IF cancel THEN BEGIN
    returnData = -1B
  ENDIF ELSE IF ~cancel THEN BEGIN
    returnData = groupStruc                                 ;Alias the group structure
    imageStruc = (*ptr).imageStruc                          ;Retrieve updated info from the pointer
    groupInfo  = (*ptr).groupInfo                           ;Retrieve updated info from the pointer
    
    returnData.groupFlags = groupInfo.groupFlag             ;Update the groupFlags
    returnData.astroFlags = imageStruc.imageFlag            ;Update the astroFlags
  ENDIF

  PTR_FREE, ptr
  
  RETURN, returnData
END