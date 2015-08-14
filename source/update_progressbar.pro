PRO UPDATE_PROGRESSBAR, widgetID, progressPercentage, PERCENTAGE=percentage, DISPLAY_MESSAGE=display_message, $
  ERASE=erase, TEXT_COLOR = text_color, FILL_COLOR = fill_color

  IF N_ELEMENTS(text_color) EQ 0 THEN text_color = RGB_TO_DECOMPOSED([255,180,0])
  IF N_ELEMENTS(fill_color) EQ 0 THEN fill_color = RGB_TO_DECOMPOSED([0,75,255])

  originalWindowIndex = !D.WINDOW                                   ;Store the current window index to reset later
  width = (widget_info(widgetID, /GEOMETRY)).xsize
  WIDGET_CONTROL, widgetID, GET_VALUE=windowIndex                   ;Retrieve display window WID
  WSET, windowIndex

  IF KEYWORD_SET(erase) THEN BEGIN
  
    ERASE, 0L                                                       ;Clear the progress bar
      
  ENDIF ELSE BEGIN

    DEVICE, GET_DECOMPOSED = originalState
    DEVICE, DECOMPOSED = 1
    ERASE, 0L                                                       ;Clear the plot window first
    POLYFILL, [0,0,progressPercentage,progressPercentage]/100.0, [-1,1,1,-1], /NORMAL, COLOR=fill_color
    IF KEYWORD_SET(percentage) THEN BEGIN
      XYOUTS, 0.5,0.25, STRING(progressPercentage, FORMAT = '(I3," !Uo!N/!Io!N")'), /NORMAL, $
        ALIGNMENT = 0.5, CHARSIZE = 1, CHARTHICK = 2, COLOR = text_color
    ENDIF ELSE IF KEYWORD_SET(DISPLAY_MESSAGE) THEN BEGIN
      XYOUTS, 0.5,0.25, display_message, /NORMAL, $
        ALIGNMENT = 0.5, CHARSIZE = 1, CHARTHICK = 2, COLOR = text_color
    ENDIF
    
  ENDELSE


  DEVICE, DECOMPOSED = originalState
  WSET, originalWindowIndex
END