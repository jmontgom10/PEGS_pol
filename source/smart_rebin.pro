FUNCTION SMART_REBIN, inArray, nx, ny, MASK_VALUE=mask_value

  ;NOTE: this function cannot interpolate; it can only compute averages over integer numbers of pixels.
  ;The bennefit of this function over the standard "REBIN" is that you can use a MASK_VALUE
  ;to exclude certain pixels from the rebinned average.

  ;Check that the resizing dimensions make sense and will work
  inSize       = SIZE(inArray, /DIMENSIONS)
  okDimensions = ((inSize[0] MOD nx) EQ 0) AND ((inSize[1] MOD ny) EQ 0)
  xRebin       = inSize[0]/FLOAT(nx)
  yRebin       = inSize[1]/FLOAT(ny)
  IF ~okDimensions OR (xRebin NE yRebin)THEN BEGIN                    ;Throw an error if there is a problem
    MESSAGE, 'Result dimensions must be integer factor of original dimensions', LEVEL=-1
  ENDIF ELSE rebinPix = FIX(xRebin)                                   ;Otherwise set the rebin pixel size
  
  ;Create a map of good pixels to use in the averaging
  IF N_ELEMENTS(mask_value) EQ 0 THEN BEGIN
    goodPixMap = FINITE(inArray)                                      ;Use the default masking value: !VALUES.F_NAN
  ENDIF ELSE BEGIN
    goodPixMap = inArray NE mask_value                                ;Use the supplied masking value
  ENDELSE
  
  ;Check what type of output array needs to be created
  arrayType = SIZE(inArray, /TYPE)
  IF arrayType EQ 2 OR arrayType EQ 4 THEN outArray = FLTARR(nx, ny)  ;Create a floating point output array
  IF arrayType EQ 3 OR arrayType EQ 5 THEN outArray = DBLARR(nx, ny)  ;Create a double precission output array

  FOR i = 0, nx - 1 DO BEGIN
    FOR j = 0, ny - 1 DO BEGIN
      lf            = i*rebinPix
      rt            = (i+1)*rebinPix - 1
      bt            = j*rebinPix
      tp            = (j+1)*rebinPix - 1
      goodInds      = WHERE(goodPixMap[lf:rt,bt:tp], numGood)         ;Determine which values are good
      IF numGood GE (rebinPix^2)/2E THEN BEGIN                        ;Skip locations with less than half the super-pixel filled
        sampleValues  = inArray[lf:rt,bt:tp]                          ;Cut out the values to be averaged
        outArray[i,j] = MEAN(sampleValues[goodInds])                  ;If some are good, then compute an average.
      ENDIF
    ENDFOR
  ENDFOR
  
  RETURN, outArray
END