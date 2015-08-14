FUNCTION SMART_SHIFT, array, deltaX, deltaY

  outArray = SHIFT(array, deltaX, deltaY)
  sz       = SIZE(outArray, /DIMENSIONS)
  
  IF deltaX GT 0 THEN BEGIN
    outArray[0:(deltaX-1),*] = 0
  ENDIF ELSE IF deltaX LT 0 THEN BEGIN
    outArray[(sz[0]+deltaX):*,*] = 0
  ENDIF

  IF deltaY GT 0 THEN BEGIN
    outArray[*,0:(deltaY-1)] = 0
  ENDIF ELSE IF deltaY LT 0 THEN BEGIN
    outArray[*,(sz[1]+deltaY):*] = 0
  ENDIF

  RETURN, outArray
END