FUNCTION INPAINT_NANS, img, SMOOTHING_RESOLUTION=smoothing_resolution, COUNT=count

  IF N_ELEMENTS(smoothing_resolution) EQ 0 THEN smoothing_resolution = 10
  ;
  ; find all the NAN values and fill them in using a numerical "heat transfer" solution
  ;
  goodPix   = WHERE(FINITE(img), COMPLEMENT = badPix, NCOMPLEMENT = numBad)
  IF numBad GT 0 THEN BEGIN
    img1 = img                                                ;Alias the input image
    med  = MEDIAN(img1)                                       ;Grab the median of the image
    img[badPix] = med                                         ;First pass should fill with median value

    ;Make sure kernel width is not too large and build the kernel
    kernelWidth = 0.15*MIN(SIZE(img, /DIMENSIONS)) < 3*smoothing_resolution
    kernel      = GAUSSIAN_FUNCTION([2*smoothing_resolution,2*smoothing_resolution], $
      WIDTH = 90, /NORMALIZE)
    
    ;Perform a second "first pass" filling in the bad pixels
    img1          = CONVOL(img, kernel, /EDGE_TRUNCATE)       ;Use convolution to fill in space
    img1[goodPix] = img[goodPix]                              ;Replace original values in the rest of the image
    
    ;Loop through iterations until converging on a solution
    diff  = TOTAL(ABS(img[badPix] - img1[badPix]))/numBad
    count = 0
    WHILE diff GT 5E-5 DO BEGIN
      img2          = SMOOTH(img1,smoothing_resolution, /EDGE_TRUNCATE)
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