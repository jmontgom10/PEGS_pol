FUNCTION SIG_FIG_STRING, number, numSigFigs

  ;This should be easily vectorizable
  numberLength     = N_ELEMENTS(number)
  numSigFigsLength = N_ELEMENTS(numSigFigs)

  IF numberLength NE numSigFigsLength $
    AND numSigFigsLength NE 1 THEN BEGIN
      MESSAGE, 'number and numSigFigs vectors must be the same length'
      RETURN, !NULL
  ENDIF ELSE IF numSigFigsLength EQ 1 $
    THEN numSigFigs = REPLICATE(numSigFigs, numberLength)

  outString = STRARR(numberLength)
  ;  minSigFigs      = 3                                              ;Force to display at least 3 sig-figs
  valueType       = SIZE(number, /TNAME)
  IF valueType EQ 'FLOAT' $
    OR valueType EQ 'DOUBLE' THEN BEGIN
    
    negativeSigns   = number NE ABS(number)                           ;Determine which values need a negative sign
    valueMagnitudes = ALOG10(ABS(number))                             ;This computes the magnitude of the values
    positiveMag     = valueMagnitudes EQ ABS(valueMagnitudes)         ;Determine if this number is greater than 1
    negativeMag     = ~positiveMag
    smallPosMag     = (CEIL(valueMagnitudes) LT numSigFigs)*positiveMag   ;Determine if the sig-figs span the decimal point
    mediumPosMag    = (CEIL(valueMagnitudes) EQ numSigFigs)*positiveMag   ;Determine if there is trailing decimal point
    largePosMag     = valueMagnitudes GE numSigFigs                   ;Determine if there are trailing numbers
    
    stringLength    = numSigFigs $                                    ;Begin with at least the minimum string length
      + 1*negativeSigns $                                             ;Account for a negative sign
      + (FLOOR(ABS(valueMagnitudes)) + 2)*negativeMag $               ;Account for leading zeros and decimal point
      + 1*smallPosMag $                                               ;Account spanning the decimal point
      + 2*mediumPosMag $                                              ;Account for a trailing decimal point
      + (CEIL(valueMagnitudes) - numSigFigs)*largePosMag              ;Account for trailing numbers
    IF TOTAL(ABS(stringLength) GT 10) GT 1 then stop
    
    decimalLength   = (stringLength - negativeSigns - 2)*negativeMag $;Negative magnitudes only leading zero, decimal, and sign
      + (numSigFigs - CEIL(valueMagnitudes))*smallPosMag $            ;Small positive magnitudes have only a few leading digits
      + 1*mediumPosMag $                                              ;Medium positive magnitudes have one trailing decimals
      + 0*largePosMag                                                 ;Large positive magnitudes have no trailing decimals
      
    valueIsSpecial = WHERE(ABS(number) EQ 1 $
      OR number EQ 0, countIsSpecial)                                 ;Treat the special cases of 0, and +/-1
    IF countIsSpecial GT 0 THEN BEGIN
      stringLength[valueIsSpecial]  = 3 + negativeSigns[valueIsSpecial]
      decimalLength[valueIsSpecial] = 1
    ENDIF
    stringLength    = STRTRIM(stringLength, 2)                        ;Convert string length value to a string
    decimalLength   = STRTRIM(decimalLength, 2)                       ;Convert decimal length value to a string
    
    ;***** need to account for different types of (integer) values, but this should work for now *****
    formatStrings   = '(F' + stringLength + '.' + decimalLength + ')'
    
  ENDIF ELSE IF valueType EQ 'INT' THEN BEGIN
    MESSAGE, 'Integer values not handled yet'
    RETURN, !NULL
  ENDIF
  
  FOR i = 0, numberLength - 1 DO BEGIN
    outString[i] = STRING(number[i], FORMAT = formatStrings[i])
  ENDFOR

  IF numberLength EQ 1 THEN outString = outString[0]
  RETURN, outString
END