PRO S2_USE_MODEL_VALUES, event
  
  IF event.select THEN BEGIN
    WIDGET_CONTROL, event.top, GET_UVALUE = groupStruc
  
    imgPath  = groupStruc.analysis_dir + 'S11B_Combined_Images' + $     ;Construct the combined image path
      PATH_SEP() + groupStruc.NIRband + 'band_I.fits'
    maskPath = groupStruc.analysis_dir + 'S2_Ski_Jump_Fixes' + $        ;Construct the mask path
      PATH_SEP() + 'Masking_files' + PATH_SEP() + 'galMask.fits'
    imgHead  = HEADFITS(imgPath)                                        ;Read in the FITS header for the image file
    maskHead = HEADFITS(maskPath)                                       ;Read in the FITS header for the mask file
    
    RA_cen  = SXPAR(maskHead, 'RA_CEN')                                 ;Extract the galaxy center RA
    DEC_cen = SXPAR(maskHead, 'DEC_CEN')                                ;Extract the galaxy center Dec
    gal_PA  = SXPAR(maskHead, 'P03')                                    ;Extract the galaxy position angle (radians)

    EXTAST, imgHead, astr                                               ;Read in the astrometry for the combined image
    AD2XY, RA_cen, DEC_cen, astr, Xcen, Ycen                            ;Convert galaxy position into pixels
    
    angleWID = WIDGET_INFO(event.top, FIND_BY_UNAME='ROTATION_ANGLE')   ;Get the widget IDs for the rotation information
    xCenWID  = WIDGET_INFO(event.top, FIND_BY_UNAME='ROTATION_X_PIXEL')
    yCenWID  = WIDGET_INFO(event.top, FIND_BY_UNAME='ROTATION_Y_PIXEL')

    WIDGET_CONTROL, angleWID, EDITABLE=0, $                             ;Update the text angle widget
      SET_VALUE=SIG_FIG_STRING(-gal_PA*!RADEG, 4), SET_UVALUE=-gal_PA*!RADEG
    WIDGET_CONTROL, xCenWID, EDITABLE=0, $                              ;Update the text X widget
      SET_VALUE=SIG_FIG_STRING(Xcen, 4), SET_UVALUE=Xcen
    WIDGET_CONTROL, yCenWID, EDITABLE=0, $                              ;Update the text Y widget
      SET_VALUE=SIG_FIG_STRING(Ycen, 4), SET_UVALUE=Ycen

  ENDIF ELSE BEGIN
    angleWID = WIDGET_INFO(event.top, FIND_BY_UNAME='ROTATION_ANGLE')   ;Get the widget IDs for the rotation information
    xCenWID  = WIDGET_INFO(event.top, FIND_BY_UNAME='ROTATION_X_PIXEL')
    yCenWID  = WIDGET_INFO(event.top, FIND_BY_UNAME='ROTATION_Y_PIXEL')
    
    WIDGET_CONTROL, angleWID, EDITABLE=1
    WIDGET_CONTROL, xCenWID, EDITABLE=1
    WIDGET_CONTROL, yCenWID, EDITABLE=1
  ENDELSE
  
  
END

PRO S2_ROTATE_IMAGES, event
  
  angleWID = WIDGET_INFO(event.top, FIND_BY_UNAME='ROTATION_ANGLE')
  xCenWID  = WIDGET_INFO(event.top, FIND_BY_UNAME='ROTATION_X_PIXEL')
  yCenWID  = WIDGET_INFO(event.top, FIND_BY_UNAME='ROTATION_Y_PIXEL')
  
  WIDGET_CONTROL, event.top, GET_UVALUE = groupStruc
  WIDGET_CONTROL, angleWID, GET_UVALUE = rotAngle
  WIDGET_CONTROL, xCenWID, GET_UVALUE = xCen
  WIDGET_CONTROL, yCenWID, GET_UVALUE = yCen
  
  rotAngle = -DOUBLE(rotAngle[0])
  xCen     =  DOUBLE(xCen[0])
  yCen     =  DOUBLE(yCen[0])

  ;Define the paths in which to search for the input images and file masks, etc...
  inputDir   = groupStruc.analysis_dir + 'S11B_Combined_Images' + PATH_SEP()
  filePrefix = groupStruc.NIRband + 'band_'
  
  Iimg  = READFITS(inputDir + filePrefix + 'I.fits', Iheader)         ;Read in the stakes images and headers
  Uimg  = READFITS(inputDir + filePrefix + 'U.fits', Uheader)
  Qimg  = READFITS(inputDir + filePrefix + 'Q.fits', Qheader)
  sUimg = READFITS(inputDir + filePrefix + 'sU.fits', sUheader)
  sQimg = READFITS(inputDir + filePrefix + 'sQ.fits', sQheader)
  
  sz     = SIZE(Iimg, /DIMENSIONS)                                    ;Grab the size of the image
  deltaX = ROUND(0.5*sz[0] - xCen)                                    ;Compute how far to shift the image
  deltaY = ROUND(0.5*sz[1] - yCen)
  xCen   = ROUND(0.5*sz[0])                                           ;Now pivot about the central pixel
  yCen   = ROUND(0.5*sz[1])

  Iimg  = SMART_SHIFT(Iimg, deltaX, deltaY)                           ;Shift the image arrays
  Uimg  = SMART_SHIFT(Uimg, deltaX, deltaY)
  Qimg  = SMART_SHIFT(Qimg, deltaX, deltaY)
  sUimg = SMART_SHIFT(sUimg, deltaX, deltaY)
  sQimg = SMART_SHIFT(sQimg, deltaX, deltaY)

  crpix1 = SXPAR(Iheader, 'CRPIX1')
  SXADDPAR, Iheader, 'CRPIX1', (crpix1+deltaX+1)                      ;Adding one pixel to the shift seems to help matters...
  crpix2 = SXPAR(Iheader, 'CRPIX2')                                   ;Double check if this works on other galaxies...
  SXADDPAR, Iheader, 'CRPIX2', (crpix2+deltaY+1)
  crpix1 = SXPAR(Uheader, 'CRPIX1')
  SXADDPAR, Uheader, 'CRPIX1', (crpix1+deltaX+1)
  crpix2 = SXPAR(Uheader, 'CRPIX2')
  SXADDPAR, Uheader, 'CRPIX2', (crpix2+deltaY+1)
  crpix1 = SXPAR(Qheader, 'CRPIX1')
  SXADDPAR, Qheader, 'CRPIX1', (crpix1+deltaX+1)
  crpix2 = SXPAR(Qheader, 'CRPIX2')
  SXADDPAR, Qheader, 'CRPIX2', (crpix2+deltaY+1)
  crpix1 = SXPAR(sUheader, 'CRPIX1')
  SXADDPAR, sUheader, 'CRPIX1', (crpix1+deltaX+1)
  crpix2 = SXPAR(sUheader, 'CRPIX2')
  SXADDPAR, sUheader, 'CRPIX2', (crpix2+deltaY+1)
  crpix1 = SXPAR(sQheader, 'CRPIX1')
  SXADDPAR, sQheader, 'CRPIX1', (crpix1+deltaX+1)
  crpix2 = SXPAR(sQheader, 'CRPIX2')
  SXADDPAR, sQheader, 'CRPIX2', (crpix2+deltaY+1)


;  ;************* THIS SECTION OF CODE SAMPLES UP, ROTATES, THEN SAMPLES DOWN **************
;  ;Begin by resampling the images to a higher resolution (using nearest neighbor sampling)
;  overSample = 8
;  PRINT_TEXT2, event, 'Oversampling intput images before rotation'
;  HREBIN, Iimg,Iheader, Iimg1, Iheader1, overSample*sz[0], overSample*sz[1], /SAMPLE
;  HREBIN, Uimg,Uheader, Uimg1, Uheader1, overSample*sz[0], overSample*sz[1], /SAMPLE
;  HREBIN, Qimg,Qheader, Qimg1, Qheader1, overSample*sz[0], overSample*sz[1], /SAMPLE  
;  HREBIN, sUimg,sUheader, sUimg1, sUheader1, overSample*sz[0], overSample*sz[1], /SAMPLE
;  HREBIN, sQimg,sQheader, sQimg1, sQheader1, overSample*sz[0], overSample*sz[1], /SAMPLE
;  
;  ;Rotate the images about the pivot point
;  PRINT_TEXT2, event, 'Rotating images and updating headers'
;  HROT, Iimg1, Iheader1, Irot1, IrotHead1, rotAngle, overSample*(xCen+1), overSample*(yCen+1), 0, /PIVOT, MISSING=0
;  HROT, Uimg1, Uheader1, Urot1, UrotHead1, rotAngle, overSample*(xCen+1), overSample*(yCen+1), 0, /PIVOT, MISSING=0
;  HROT, Qimg1, Qheader1, Qrot1, QrotHead1, rotAngle, overSample*(xCen+1), overSample*(yCen+1), 0, /PIVOT, MISSING=0
;  HROT, sUimg1, sUheader1, sUrot1, sUrotHead1, rotAngle, overSample*(xCen+1), overSample*(yCen+1), 0, /PIVOT, MISSING=0
;  HROT, sQimg1, sQheader1, sQrot1, sQrotHead1, rotAngle, overSample*(xCen+1), overSample*(yCen+1), 0, /PIVOT, MISSING=0
;  
;  ;Sample the images back down...
;  PRINT_TEXT2, event, 'Resampling down to original plate scale'
;  HREBIN, Irot1, IrotHead1, Irot, IrotHead, sz[0], sz[1]
;  HREBIN, Urot1, UrotHead1, Urot, UrotHead, sz[0], sz[1]
;  HREBIN, Qrot1, QrotHead1, Qrot, QrotHead, sz[0], sz[1]
;  HREBIN, sUrot1, sUrotHead1, sUrot, sUrotHead, sz[0], sz[1]
;  HREBIN, sQrot1, sQrotHead1, sQrot, sQrotHead, sz[0], sz[1]


  ;************* THIS SECTION OF CODE USES CUBIC INTERPOLATION **************
  ;Rotate the images
  PRINT_TEXT2, event, 'Rotating images using cubic interpolation'
  HROT, Iimg, Iheader, Irot, IrotHead, rotAngle, xCen, yCen, 2, /PIVOT, MISSING=0
  HROT, Uimg, Uheader, Urot, UrotHead, rotAngle, xCen, yCen, 2, /PIVOT, MISSING=0
  HROT, Qimg, Qheader, Qrot, QrotHead, rotAngle, xCen, yCen, 2, /PIVOT, MISSING=0
  HROT, sUimg, sUheader, sUrot, sUrotHead, rotAngle, xCen, yCen, 2, /PIVOT, MISSING=0
  HROT, sQimg, sQheader, sQrot, sQrotHead, rotAngle, xCen, yCen, 2, /PIVOT, MISSING=0


  ROT_AXES, Qrot, Urot, 2*rotAngle, Qrot1, Urot1, /DEGREES            ;Now rotate the actual Q and U values
  
  rotAngleRadian = rotAngle*!DTOR                                     ;Propagate errors for Q and U rotation
  sUrot = SQRT(((SIN(2*rotAngleRadian) + COS(2*rotAngleRadian)*Urot)*sUrot)^2 + $
               ((SIN(2*rotAngleRadian)*Qrot + COS(2*rotAngleRadian))*sQrot)^2)
  sQrot = SQRT(((COS(2*rotAngleRadian) - SIN(2*rotAngleRadian)*Urot)*surot)^2 + $
               ((COS(2*rotAngleRadian)*Qrot + COS(2*rotangleRadian))*sQrot)^2)
  
  WRITEFITS, (inputDir + filePrefix + 'Irot.fits'), Irot, IrotHead
  WRITEFITS, (inputDir + filePrefix + 'Urot.fits'), Urot1, UrotHead
  WRITEFITS, (inputDir + filePrefix + 'Qrot.fits'), Qrot1, QrotHead
  WRITEFITS, (inputDir + filePrefix + 'sUrot.fits'), sUrot, sUrotHead
  WRITEFITS, (inputDir + filePrefix + 'sQrot.fits'), sQrot, sQrotHead
  
  ;**************Finally, recompute a rotated mask**************
  inputDir = groupStruc.analysis_dir + 'S2_Ski_Jump_Fixes' + PATH_SEP() + 'Masking_files' + PATH_SEP()
  maskImg  =READFITS(inputDir + 'galMask.fits', maskHead)
  

  skynoise = SXPAR(groupStruc.displayHeader, 'SIGMA')       ;Use the 2MASS compute sky noise to set 2.5 sigma threshold
  IF skynoise EQ 0 THEN BEGIN
    skyParam = STRMID(groupStruc.NIRband,0,1) + 'SKYSIG'
    skynoise = SXPAR(groupStruc.displayHeader, skyParam)
  ENDIF
  galMask = GENERATE_MASK(event, sz[0], sz[1], xCen, yCen, 0.579, 2.5*skynoise, /ROTATED)
  SXADDPAR, maskHead, 'P03', sxpar(maskHead, 'P03')*!RADEG - rotAngle
  WRITEFITS, (inputDir + 'galMaskrot.fits'), galMask, maskHead
  
  PRINT_TEXT2, event, 'Done writing files to disk'

END