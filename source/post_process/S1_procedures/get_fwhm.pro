FUNCTION GET_FWHM, image, xPos, yPos, guessFWHM, satLimit

  x = xPos                                                            ;Alias the xPos vector
  y = yPos                                                            ;Alias the yPos vector

  saturated = TEST_SATURATED(image, xPos, yPos, $                     ;Test for saturated stars
    satLimit, 4*guessFWHM)
  unSatInd = WHERE(~saturated, numStars)                              ;grab the Unsaturated indices

  x = x[unSatInd]                                                     ;Remove saturated stars from the list
  y = y[unSatInd]
  
  crowdedStars = TEST_CROWDED(xPos, yPos, 5*guessFWHM)                ;Find crowded stars
  unCrowdInd   = WHERE(~crowdedStars, numUnCrowd)

  x = x[unCrowdInd]                                                   ;Remove the crowded stars from the list
  y = y[unCrowdInd]

  IF FLOAT(numUnCrowd)/FLOAT(numStars) LT 0.7 THEN BEGIN
    PRINT, "Star field is too crowded." + new_line() $
      + "Return and raise threshold for detection"
    RETURN, -1
  ENDIF
  
  delXY   = 3*guessFWHM                                               ;Define array width and height
  FWHMarr = FLTARR(numUnCrowd)
  badStar = 0
  FOR i = 0, numUnCrowd - 1 DO BEGIN
    thisX    = x[i]                                                   ;Round star locations to nearest pixel
    thisY    = y[i]                                                   ;Round star locations to nearest pixel
    subArray = image[ROUND(thisX-delXY):ROUND(thisX+delXY), $         ;Grab array centered around the star
      thisY-delXY:thisY+delXY]
    satInd   = WHERE(subArray GT satLimit, count)                     ;Test for saturated pixels
    IF count GT 0 THEN CONTINUE                                       ;Skip saturated stars
    result     = GAUSS2DFIT(subArray, A, /TILT)                       ;Fit a gaussian
    roundTest1 = ABS(1-MAX(A[2:3])/MIN(A[2:3]))
    roundTest2 = ABS(1-MIN(A[2:3])/MAX(A[2:3]))
    IF (roundTest1 GT 0.2) OR (roundTest2 GT 0.2) THEN CONTINUE
    xStar  = A[4]                                                     ;Pick off the fitted star x position
    yStar  = A[5]                                                     ;Pick off the fitted star y position
    starFitDist = SQRT((thisX - xStar)^2 + (thisY - yStar)^2)         ;Compute offset of fitted gaussian
    IF starFitDist GT 0.5*guessFWHM THEN $
      FWHMarr[i] = (2*SQRT(2*ALOG(2)))*SQRT(A[2]*A[3]) ELSE $         ;Compute geometric mean of FWHM for this star
      FWHMarr[i] = !VALUES.F_NAN
  ENDFOR
  
  FWHMarr = FWHMarr[WHERE(FINITE(FWHMarr))]
  MEANCLIP, FWHMarr, meanFWHM, sigmaFWHM

  RETURN, [meanFWHM, sigmaFWHM]
END