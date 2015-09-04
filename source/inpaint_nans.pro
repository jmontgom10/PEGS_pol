FUNCTION INPAINT_NANS, img, SMOOTHING_RESOLUTION=smoothing_resolution, SMOOTHING_KERNEL = smoothing_kernel, COUNT=count

  IF N_ELEMENTS(smoothing_resolution) EQ 0 THEN smoothing_resolution = 10
  IF N_ELEMENTS(smoothing_kernel) NE 0 THEN BEGIN
    kernel = smoothing_kernel
    sz     = SIZE(kernel, /DIMENSIONS)
    IF N_ELEMENTS(sz) EQ 1 THEN BEGIN
      sz = [sz, 1]
      bigKernel = INTERPOLATE(kernel, FINDGEN(2*sz[0]-1)/2, /GRID)
    ENDIF ELSE BEGIN
      bigKernel     = INTERPOLATE(kernel, FINDGEN(2*sz[0]-1)/2, FINDGEN(2*sz[1]-1)/2, /GRID)
    ENDELSE
    numPix = N_ELEMENTS(kernel)
    MAKE_2D, FINDGEN(sz[0]), FINDGEN(sz[1]), xx, yy
    muX  = TOTAL(xx*kernel)/TOTAL(kernel)
    muY  = TOTAL(yy*kernel)/TOTAL(kernel)
    sigX = SQRT(TOTAL(kernel*xx^2)/TOTAL(kernel) - muX^2)
    sigY = SQRT(TOTAL(kernel*yy^2)/TOTAL(kernel) - muY^2)
    smoothing_resolution = SQRT(sigX^2 + sigY^2)
  ENDIF ELSE BEGIN
    ;Make sure kernel width is not too large and build the kernel
    kernelWidth = 0.15*MIN(SIZE(img, /DIMENSIONS)) < 3*smoothing_resolution
    kernel      = GAUSSIAN_FUNCTION([2*smoothing_resolution, 2*smoothing_resolution], $
      WIDTH = 3*smoothing_resolution, /NORMALIZE)
    sz     = SIZE(kernel, /DIMENSIONS)
  ENDELSE
  ;
  ; find all the NAN values and fill them in using a numerical "heat transfer" solution
  ;
  goodPix   = WHERE(FINITE(img), COMPLEMENT = badPix, NCOMPLEMENT = numBad)
  IF numBad GT 0 THEN BEGIN
    img1 = img                                                ;Alias the input image
    med  = MEDIAN(img1)                                       ;Grab the median of the image
    img[badPix] = med                                         ;First pass should fill with median value

    ;Perform a second "first pass" filling in the bad pixels
    img1          = CONVOL(img, bigKernel, /EDGE_TRUNCATE)    ;Use convolution to fill in space
    img1[goodPix] = img[goodPix]                              ;Replace original values in the rest of the image
    
    ;Loop through iterations until converging on a solution
    diff  = TOTAL(ABS(img[badPix] - img1[badPix]))/numBad
    count = 0
    WHILE diff GT 5E-5 DO BEGIN
      img2          = CONVOL(img1,kernel, /EDGE_TRUNCATE)
      img2[goodPix] = img[goodPix]
      diff          = TOTAL(ABS(img2[badPix] - img1[badPix]))/numBad
      img1          = img2
      count++
      IF count GT 10000 THEN diff = 1E-5
    ENDWHILE
;    PRINT, 'Inpainting completed in ', count, ' iterations.'
;    output = img1
  ENDIF

RETURN, img1
END