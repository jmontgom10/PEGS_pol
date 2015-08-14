FUNCTION S2_APPLY_MASK, event, img, maskValues
  maskedImg = img                                                     ;Alias the image to be masked
  
  ;Apply border mask
  border  = maskValues.border                                         ;Pull out the size of the mask border
  imgSize = SIZE(maskedImg, /DIMENSIONS)                              ;Image dimensions
  nx      = imgSize[0]                                                ;Image width
  ny      = imgSize[1]                                                ;Image height
  maskedImg[0:border,*] = -1E6
  maskedImg[*,0:border] = -1E6
  maskedImg[nx-border-1:nx-1,*] = -1E6
  maskedImg[*,ny-border-1:ny-1] = -1E6
  
  ;Compute disk pixels
  xx = REBIN(REFORM(FINDGEN(nx), nx, 1), nx, ny, /SAMPLE)  - maskValues.galCenter[0] ;Create a 2-D map of X-values
  yy = REBIN(REFORM(FINDGEN(ny), 1, ny), nx, ny, /SAMPLE)  - maskValues.galCenter[1] ;Create a 2-D map of Y-values
  
  ROT_AXES, xx, yy, maskValues.galPA, xx1, yy1, /DEGREES              ;Rotate the unscaled axes
  semiMinor = xx1*(0.58/60.0)    /  maskValues.disk[1]                ;Scale the x-axis to semi-minor axis units
  semiMajor = yy1*(0.58/60.0)    /  maskValues.disk[0]                ;Scale the y-axis to semi-major axis units
  diskMask  = SQRT(semiMinor^2 + semiMajor^2) LT 1                    ;Identify pixels inside the disk
  
  ;****FLOADING DIVIDE BY 0 IN NEXT THREE LINES***
  ;Compute bulge pixels
  bulgeCheckWID = WIDGET_INFO(event.top, $                            ;Retrieve the bulge checkbox ID
    FIND_BY_UNAME='S2_BULGE_CHECK')
  WIDGET_CONTROL, bulgeCheckWID, GET_VALUE=bulgeCheck                 ;Retrieve the bulge checkbox value
  IF bulgeCheck AND $                                                 ;Check the bulge checkbox
    (maskValues.bulge[0] NE 0) AND $                                    ;Check for non-zero size
    (maskValues.bulge[1] NE 0) THEN BEGIN
    semiMinor = xx1*(0.58/60.0)    /  maskValues.bulge[1]             ;Scale the x-axis to semi-minor axis units
    semiMajor = yy1*(0.58/60.0)    /  maskValues.bulge[0]             ;Scale the y-axis to semi-major axis units
    bulgeMask = SQRT(semiMinor^2 + semiMajor^2) LT 1                  ;Identify pixels inside the bulge
    maskInds  = WHERE((diskMask OR bulgeMask), count)
  ENDIF ELSE maskInds = WHERE(diskMask, count)
  ;***********************************************
  
  ;Apply the disk and bulge mask
  IF count GT 0 THEN maskedImg[maskInds] = -1E6
  RETURN, maskedImg
END