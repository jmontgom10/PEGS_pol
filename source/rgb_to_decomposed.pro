FUNCTION RGB_TO_DECOMPOSED, RGBtrip
  szColors             = SIZE(RGBtrip)                                ;Determine the properties of the triplet(s) passed
  tripletDimension     = WHERE(szColors[1:2] EQ 3, countTriplet, $    ;Find out if RGBtrip is nx3 or 3xn
    COMPLEMENT = colorsDimension)
  
  
  
  IF (countTriplet LT 1) OR (szColors[0] GT 2) THEN BEGIN             ;Test for the correct number of dimensions
    PRINT, 'The RGB triplet needs to be either nx3 or 3xn'
    RETURN, -1
  ENDIF

  IF colorsDimension EQ 0 THEN BEGIN                                  ;If it is nx3, then...
    numColors            = SzColors[1]                                ;Select the dimension for the number of colors
    convertingVector     = REBIN(REFORM([1L,256L,256L^2], 1, 3), numColors, 3, /SAMPLE)
    decomposedColorIndex = REFORM(LONG(TOTAL(RGBtrip*convertingVector,2)), numColors, 1)
  ENDIF ELSE IF colorsDimension EQ 1 THEN BEGIN                       ;If it is 3xn, then...
    numColors            = SzColors[0]                                ;Select the dimension for the number of colors
    convertingVector     = REBIN(REFORM([1L,256L,256L^2], 3, 1), 3, numColors, /SAMPLE)
    decomposedColorIndex = REFORM(LONG(TOTAL(RGBtrip*convertingVector,1)), 1, numColors)
   ENDIF
  
  IF N_ELEMENTS(decomposedColorIndex) EQ 1 THEN $                     ;If there is only one color, then use a scalar index
    decomposedColorIndex = decomposedColorIndex[0]
  RETURN, decomposedColorIndex

END