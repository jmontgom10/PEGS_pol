PRO S2_DISPLAY_IMAGE, event, message
  ;Retrieve the root directory
  tlb_wid = WIDGET_INFO(event.top, FIND_BY_UNAME='WID_BASE')
  WIDGET_CONTROL, tlb_wid, GET_UVALUE=input_output_dirs
  ;load the intensity image
  intensityFile = FILE_SEARCH(input_output_dirs[1] + "S1_Image_Combine/", $
    "*_I.fits", COUNT = nFiles)                                       ;Search for the intensity image
  IF nFiles NE 1 THEN STOP                                           ;Check for only ONE image file
  img       = READFITS(intensityFile, header, /SILENT)               ;Read in the intensity image
  imgSize   = SIZE(img, /DIMENSIONS)                                  ;Image dimensions
  nx        = imgSize[0]                                              ;Image width
  ny        = imgSize[1]                                              ;Image height
  
  maskBaseWID = WIDGET_INFO(event.top, FIND_BY_UNAME='S2_MASK_BASE')  ;Retrieve the Mask Base ID
  WIDGET_CONTROL, maskBaseWID, GET_UVALUE=maskValues                  ;Retrieve the maskValues structure
  
  ;Display the image in the DRAW widget
  displayWID  = WIDGET_INFO(event.top, $                              ;Grab the widget ID for the DRAW widget
    FIND_BY_UNAME='S2_IMAGE_WINDOW')
  WIDGET_CONTROL, displayWID, GET_VALUE=windowIndex                   ;Grab window Index
  WSET, windowIndex                                                   ;Set active display window
  
  ;***Eventually write my own plotting tool***
  outlineCheckboxWID = WIDGET_INFO(event.top, FIND_BY_UNAME='OUTLINE_CHECKBOX')
  WIDGET_CONTROL, outlineCheckboxWID, GET_VALUE=outlineOnly           ;Retrieve the outline checkbox value
  IF outlineOnly THEN BEGIN                                           ;Show unmasked image
    topLimitImg = SIGRANGE(img, FRACTION=0.995, RANGE=dataRange)    ;Intensity range to display
    TVIM, topLimitImg
  ENDIF ELSE BEGIN
    maskedImg = S2_APPLY_MASK(event, img, maskValues)                 ;Apply the mask to the image
    goodData  = WHERE(maskedImg NE -1E6, count)                       ;Find the unmasked pixels
    IF count GT 0 THEN sigImg = img[goodData] ELSE sigImg = img       ;Store unmasked values
    topLimitImg = SIGRANGE(sigImg, FRACTION=0.99, RANGE=dataRange)    ;Intensity range to display
    TVIM, maskedImg, RANGE=dataRange                                  ;Plot the masked image
  ENDELSE
  ;Draw an outline for the ellipses
  thetaValues = 2*!PI*(FINDGEN(501)/500)                             ;Generate theta values
  xEllipse    = COS(thetaValues)                                      ;Generate ellipse X-values
  yEllipse    = SIN(thetaValues)                                      ;Generate ellipse Y-values
  
  xDisk = xEllipse*maskValues.disk[1]*(60.0/0.58)                     ;Stretch unrotated axes
  yDisk = yEllipse*maskValues.disk[0]*(60.0/0.58)                     ;Stretch unrotated axes
  
  ROT_AXES, xDisk, yDisk, $                                           ;Rotate the ellipse x and y values
    -maskValues.galPA, xDisk1, yDisk1, /DEGREES
    
  PLOTS, xDisk1 + maskValues.galCenter[0], yDisk1+maskValues.galCenter[1]                                               ;Overplot the ellipse edge
  
  bulgeCheckWID = WIDGET_INFO(event.top, $                            ;Retrieve the bulge checkbox ID
    FIND_BY_UNAME='S2_BULGE_CHECK')
  WIDGET_CONTROL, bulgeCheckWID, GET_VALUE=bulgeCheck                 ;Retrieve the bulge checkbox value
  
  IF bulgeCheck AND $                                                 ;Check the bulge checkbox
    (maskValues.bulge[0] NE 0) AND $                                  ;Check for non-zero size
    (maskValues.bulge[1] NE 0) THEN BEGIN
    xBulge = xEllipse*maskValues.bulge[1]*(60.0/0.58)                 ;Stretch unrotated axes
    yBulge = yEllipse*maskValues.bulge[0]*(60.0/0.58)                 ;Stretch unrotated axes
    ROT_AXES, xBulge, yBulge, $                                       ;Rotate the ellipse x and y values
      -maskValues.galPa, xBulge1, yBulge1, /DEGREES
    PLOTS, xBulge1 + maskValues.galCenter[0], yBulge1 + maskValues.galCenter[1]
  ENDIF
  
  ;Finally plot the PA line (red) and galaxy center (green)
  slope       = TAN((maskValues.galPA - 90)*!DTOR)                      ;Slope of the PA line
  intercept   = maskValues.galCenter[1] - slope*maskValues.galCenter[0] ;Intercept of the PA line
  xPAline     = [0, nx]                                                 ;The X extremeties of the image
  yPAline     = slope*xPAline + intercept                               ;The Y values for those extremeties
  outOfbounds = WHERE((yPAline LT 0) OR (yPAline GT ny), count)       ;Search for out of bounds values
  IF count GT 0 THEN BEGIN                                            ;If some aredound, then replace them
    FOR i = 0, count - 1 DO BEGIN                                     ;Loop through the out of bounds values
      index          = outOfBounds[i]                                   ;Select one of the out of bounds values
      yPAline[index] = ([0, ny])[index]                                 ;Replace the out of bounds value
      xPAline[index] = (yPAline[index] - intercept)/slope               ;Compute the corresponding xvalue
    ENDFOR
  ENDIF
  
  PLOTS, xPAline, yPAline, COLOR = cgColor("Red")                     ;Overplot the PA line
  OPLOT, [maskValues.galCenter[0]], [maskValues.galCenter[1]], $      ;Mark the galaxy center
    COLOR = cgColor("Green"), PSYM = 7
  ;*********************
  IF N_ELEMENTS(message) NE 0 THEN $
    XYOUTS, 0.5*nx, 0.95*ny, message, /DATA, ALIGNMENT=0.5
END