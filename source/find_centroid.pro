FUNCTION FIND_CENTROID, img, IS_SUBIMG = is_subimg, RANGE = range, WINDOW_ID = window_id, $
  CUTOUT_SIZE = cutout_size
  
  ;Check keywords, image size, and other initial requirements
  IF N_ELEMENTS(cutout_size) EQ 0 THEN cutout_size = 20
;  IF N_ELEMENTS(range) EQ 0 THEN STOP
  sz = SIZE(img, /DIMENSIONS)
  
  SKY, img, skyMode, skyNoise, /SILENT                         ;Estimate sky brightness and noise
  IF ~KEYWORD_SET(is_subimg) THEN BEGIN
    ;Begin by asking the user to estimate the bump (star or bulge) locaiton
    CURSOR, xGuess, yGuess, /DATA, /DOWN                         ;Click on the approximate location
  
    ;Cut out a subarray for a more precise positioning
    xOff   = (xGuess - (cutout_size - 1)) > 0
    xRt    = (xOff  + 2*cutout_size) < (sz[0] - 1)
    yOff   = (yGuess - (cutout_size - 1)) > 0
    yTop   = (yOff + 2*cutout_size)  < (sz[1] - 1)
    subImg = img[xOff:xRt, yOff:yTop]
  ENDIF ELSE BEGIN
    subImg = img
    xOff   = 0
    yOff   = 0
  ENDELSE

  retry  = 0
  done   = 0
  manual = 0
  WHILE ~done DO BEGIN
    TVIM, subImg, RANGE = [skyMode-skyNoise, MAX(subImg)]       ;Show the user a zoom-in of the point of interest

    ;Draw a pseudo button to toggle manual mode
    buttonXY = CONVERT_COORD([0,0,0.2,0.2,0], [0,0.08,0.08,0,0], /NORMAL, /TO_DATA)
    PLOTS, REFORM(buttonXY[0,*]), REFORM(buttonXY[1,*])
    
;    POLYFILL, [0, 0, 0.199, 0.199, 0], [0, 0.0799, 0.0799, 0,0], /NORMAL, COLOR=!P.BACKGROUND
    IF ~manual THEN BEGIN
      XYOUTS, 0.1, 0.03, 'Manual', /NORMAL, ALIGNMENT = 0.5
    ENDIF ELSE BEGIN
      XYOUTS, 0.1, 0.03, 'Centroid', /NORMAL, ALIGNMENT = 0.5
    ENDELSE

    IF retry EQ 0 THEN BEGIN
      XYOUTS, 0.5, 0.06, 'Click on the point of interest', /NORMAL, ALIGNMENT = 0.5
    ENDIF
    
    ;Click on the point of interest in the zoom-in
    IF (retry EQ 1) THEN BEGIN
      XYquery = CONVERT_COORD([Xquery], [Yquery], /NORMAL, /TO_DATA)
      xCen1   = XYquery[0]
      yCen1   = XYquery[1]
    ENDIF ELSE BEGIN
      ;If the user is not retrying, then use the cursor to get the guessed position
      CURSOR, xCen1, yCen1, /DATA, /DOWN
    ENDELSE
    
    IF ~manual THEN BEGIN
      ;Centroid the star using GCTRD
      GCNTRD, subImg, xCen1, yCen1, xCen, yCen, 3.0 ;, /SILENT      ;Compute the centroid about the clicked position
    ENDIF ELSE BEGIN
      xCen = xCen1
      yCen = yCen1
    ENDELSE
    
    OPLOT, [xcen,xcen], [0,40], LINESTYLE = 2, THICK = 2, COLOR = '0000FF'x ;Draw cross-hairs on the centroid position
    OPLOT, [0,40], [ycen,ycen], LINESTYLE = 2, THICK = 2, COLOR = '0000FF'x
    
    POLYFILL, [0.201,0.201,1,1,0.0201], [0,0.09,0.09,0,0], /NORMAL, COLOR=!P.BACKGROUND
    XYOUTS, 0.5, 0.06, 'Left click to recentroid', /NORMAL, ALIGNMENT = 0.5
    XYOUTS, 0.5, 0.03, 'Right click to accept centroid poisition', /NORMAL, ALIGNMENT = 0.5

    ;Ask the user what they want to do    
    CURSOR, Xquery, Yquery, /NORMAL, /DOWN
    
    ;If they clicked in the "manual" button" then toogle the "manual" boolean variable
    IF (Xquery LT 0.2) AND (Yquery LT 0.12) THEN manual = ~manual

    ;If they left clicked, then just retry the centroid (or manual positioning)
    IF (!MOUSE.BUTTON EQ 1) THEN BEGIN
      retry = 1
    ENDIF
    
    ;If they right clicked, then proceed to return these values
    IF (!MOUSE.BUTTON EQ 4) THEN BEGIN
      retry = 0
      done  = 1
    ENDIF
    
  ENDWHILE
  
  xPoint = Xcen + xOff                                               ;Recompute the x-position
  yPoint = Ycen + yOff                                               ;Recompute the y-position
  
  RETURN, [xPoint, yPoint] 

END