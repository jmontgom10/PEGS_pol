function jm_median_filtered_mean, array, DIMENSION = dimension
;
;   computes the median filtered mean value of the data in the
;   vec vector
;
;   returns the mean and standard deviation
;
;   DPC     20040101  legacy code
;           20051012  trapped stdev to no smaller than 0.001 to halt sigma collapse
;           20051023  tuned to improve speed
;			      20100805  fixed bug for entry with scalar
;			      20130606  test for all inputs non-finite
;		JDM     20140821  re-write to work on entire stacks of images with IDL-8 array statstic capabilities.
;

;Test if the dimension keyword was set (default to dimension = 0),
;and if it is set, then check that it is a legal value.
if n_elements(dimension) eq 0 then dimension = 0
if dimension GT size(array, /n_dimensions) then begin
  message, "DIMENSION keyword value must be less than the number of dimensions of 'array'", LEVEL = -1
endif

if TOTAL(FINITE(array)) gt 0 then begin
  sz           = SIZE(array, /DIMENSIONS)                ;Grab the dimensionality of the array
  dimensionLen = FLOAT(sz[dimension-1])                  ;Figure out the length of the stacking dimension
  newShape     = sz                                      ;Compute the intermediate shape of the statistics array
  newShape[dimension-1] = 1                              ;Set the compressed dimension equal to 1

  ;To use the same procedure as the original median_filtered_mean,
  ;we should begin be masking at 2.2 sigma and loop UP until we reach 3 sigma.
  mask           = BYTE(0*array)  ;Initalize an empty mask array
  numMaskChanged = 1              ;Counter for the number of elements changed in this iteration
  iterCount      = 0              ;Counter for the number of times through the whwile loop
  finiteMask     = FINITE(array)  ;Mark the finite elements of the array
  numPoints      = TOTAL(finiteMask, dimension) ;Count the number of usable elements along the DIMENSION axis
  scale          = REBIN([2.2], newShape, /SAMPLE);Initalize scale value at 2.2 (sigma)
  FOR iloop = 0, 4 DO BEGIN
    ;Setup the coppied array and mask all the rejected values
    copyArr   = array                                     ;Copy the original array to restore original state
    maskInds  = WHERE(mask, numMask)                      ;Identify the masked indices
    IF numMask GT 0 THEN copyArr[maskInds] = !VALUES.F_NAN;Fill in masked values with "NAN"
    
    medianArr = MEDIAN(copyArr, DIMENSION = dimension)    ;Compute the median image from the stack
    medianArr = REFORM(medianArr, newShape)
    medianArr = TEMPORARY(REBIN(medianArr, sz, /SAMPLE))  ;Expand the median back into a stack

    ;This code computes the population variance (as does median_filtered_mean)
;    stdArr    = 0.001 > SQRT((numPoints-1)/numPoints) * $ ;Compute the standard deviation image from the stack
;      STDDEV(copyArr, DIMENSION = dimension, /NAN)        ;and trap for unreasonably small standard deviations...

    ;This code computes the sample variance (which seems to be generally recommended)
    stdArr    = 0.001 > $ ;Compute the standard deviation image from the stack
      STDDEV(copyArr, DIMENSION = dimension, /NAN)        ;and trap for unreasonably small standard deviations...

    stdArr    = REFORM(stdArr, newShape)
    stdArr    = TEMPORARY(REBIN(stdArr, sz, /SAMPLE))     ;Expand the stdArr back into a stack
    
    ;Store the old mask and compute the new mask
    mask1 = mask                                          ;Store the old mask
    scale = REFORM(scale, newShape)
    mask  = ~finiteMask OR $                              ;Mask the non-finite or outlier elements
      ABS(array - medianArr) GT REBIN(scale, sz, /SAMPLE)*stdArr

    ;Store the array couning the valid data count in each column and recompute from the new mask
    numPoints1 = numPoints                                ;Store the old array of numPoints
    numPoints  = dimensionLen - TOTAL(mask, dimension)    ;Compute the new array of numPoints
    nextScale  = (numPoints NE numPoints1)                ;Determine where the number of valid points has changed
    scale     += 0.2*nextScale                            ;Increment to the next scale value for locations needing it
    IF TOTAL(nextScale) EQ 0 THEN BREAK                   ;If no elements of numPoints have changed, then break
  ENDFOR
  
  ;Perform a final masking before computing the mean
  copyArr   = array
  maskInds  = WHERE(mask, numMask)                      ;Identify the masked indices
  IF numMask GT 0 THEN copyArr[maskInds] = !VALUES.F_NAN;Fill in masked values with "NAN"
  meanArr = MEAN(copyArr, DIMENSION = dimension, /NAN)  ;Compute the mean along the DIMENSION axis
  stdArr  = STDDEV(copyArr, DIMENSION = dimension, /NAN);Compute the stddev along the DIMENSION axis
    
;  ;*********************************************************
;  ;This is an alternative algorithm, which simpy loops until
;  ;the mask stops changign or 12 iterations arp performed...
;  ; ***THERE IS AN ADDITIONAL PROBLEM OF TRACKING WHICH
;  ; ***SCALING TO USE FOR EACH COLUMN...
;  ; ***COPY THE CODE ABOVE TO SOLVE THAT PROBLEM
;  ;*********************************************************
;  while (numMaskChanged gt 0) and (iterCount lt 12) do begin
;    ;Compute the masked mean and the new mask based on that masked mean image
;    meanArr = MEAN(copyArr, DIMENSION = dimension, /NAN)    ;Compute mean image ignoring masked elements
;    meanArr = REBIN(meanArr, sz, /SAMPLE)
;    stdArr  = STDDEV(copyArr, DIMENSION = dimension, /NAN)  ;Compute the standard deviation image from the stack
;    stdArr  = REBIN(stdArr, sz, /SAMPLE)                    ;Expand the stdArr back into a stack
;    mask1   = ABS(array - meanArr) gt 3.0*stdArr            ;Find pixels in the original array more than 3-sigma from the masked mean
;    
;    ;Recopy the original array and mask the elements using the newly computed mask
;    copyArr = array
;    copyArr[WHERE(mask)] = !VALUES.F_NAN
;    
;    ;Determine the number of mask elements which have changed
;    ;Count the number of elements in the new mask which are not equal to the elements in the old mask
;    numMaskChanged = TOTAL(mask NE mask1)
;    PRINT, FORMAT = '(I," mask elements have changed.")', numMaskChanged
;    stop
;    ;Copy the new mask into the old mask variable name
;    mask = mask1
;    iterCount++
;  endwhile

endif else begin
  ;No finite values found, so return NaN
  return, {mean:!VALUES.F_NAN, std:!VALUES.F_NAN}
endelse
;
return, {mean:meanArr, std:stdArr}
end
