FUNCTION TEST_SATURATED, image, x, y, satLimit, searchRad

  numStars  = N_ELEMENTS(x)
  imgSize   = SIZE(image, /DIMENSIONS)
  nx        = imgSize[0]
  ny        = imgSize[1]
  xx        = REBIN(REFORM(FINDGEN(nx), nx, 1), nx, ny, /SAMPLE)
  yy        = REBIN(REFORM(FINDGEN(ny), 1, ny), nx, ny, /SAMPLE)
  saturated = INTARR(numStars)                                        ;Create array for storing unsaturated Stars
  FOR i = 0, numStars - 1 DO BEGIN
    distFromStar = SQRT((xx-x[i])^2 + (yy - y[i])^2)                  ;Compute distances from THIS star
    testPixels   = image[WHERE(distFromStar LT searchRad)]            ;Grab nearby pixels
    IF (MAX(testPixels) GT satLimit) THEN saturated[i] = 1
  ENDFOR
  
  RETURN, saturated

END