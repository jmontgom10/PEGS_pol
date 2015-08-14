FUNCTION GENERATE_MASK, event, naxis1, naxis2, xcen, ycen, plate_scale, maskingThreshold, $
  STARS=stars, ROTATED=rotated

IF KEYWORD_SET(stars) THEN stars=1B ELSE stars=0B           ;Test if the mask should cover stars
IF KEYWORD_SET(rotated) THEN rotated=1B ELSE rotated=0B     ;Test if the mask should be rotated

WIDGET_CONTROL, event.top, GET_UVALUE=groupStruc            ;Grab the group structure from TLB

maskDirectory = groupStruc.analysis_dir + $                 ;Generate the path directory to where the mask is saved
  'S2_Ski_Jump_Fixes' + PATH_SEP() + 'Masking_files' + PATH_SEP()

maskFile   = maskDirectory + 'maskInfo.dat'                 ;Generate the mask file name
;nlines     = FILE_LINES(maskFile)
;maskHeader = STRARR(nlines)
OPENR, lun, maskFile, /GET_LUN                              ;Open the file and assign a logical unit number

line = ''                                                   ;Initalize a string to hold each line
WHILE ~EOF(lun) DO BEGIN
  READF, lun, line                                          ;Read in each line of the file
  IF N_ELEMENTS(maskHeader) EQ 0 $
    THEN maskHeader = [line] $                              ;Store the first line
    ELSE maskHeader = [maskHeader,line]                     ;Store subsequent lines
ENDWHILE

CLOSE, lun                                                  ;Close the mask file
FREE_LUN, lun                                               ;Free the logical unit number

;sz2MASS = SIZE(groupStruc.displayImage, /DIMENSIONS)
MAKE_2D, FINDGEN(naxis1), FINDGEN(naxis2), xx, yy

;Setup some arrays to determine which model should be generated and how many parameters are required
modelNames = ['SERSIC_DISK', 'EXP_RADIAL_SECH2_VERTICAL_DISK', 'BULGE_AND_ONE_SERSIC_DISK', 'BULGE_AND_EXP_RADIAL_SECH2_VERTICAL_DISK']
numParams  = [12,            11,                               15,                          14]

modelName  = SXPAR(maskHeader, 'FITMODEL')                  ;Grab the fitted model name from the header
numParams  = (numParams[WHERE(modelNames EQ modelName)])[0] ;Grab the number of parameters needed for the model
parameters = DBLARR(numParams)                              ;Generate array to store parameter values
FOR i = 0, numParams - 1 DO BEGIN                           ;Loop through parameters and read them into the array
  paramString   = STRING(i+1, FORMAT = '("P",I02)')         ;Generate the string name of that parameter value
  parameters[i] = SXPAR(maskHeader, paramString)            ;Extract the parameters from the header
ENDFOR
IF rotated THEN parameters[2] = 0D                          ;Set the rotation value to zero if necessary

pl_sc_convert = SXPAR(maskHeader,'LEN_UNIT')/plate_scale    ;Compute the conversion to the new plate scale

;Generate a model image and rescale the model parameters to match the Mimir plate scale
CASE modelName OF
  'SERSIC_DISK': BEGIN
    parameters[0:1]            = [xcen, ycen]
    lengthInds                 = [5,9]
    parameters[lengthInds]    *= pl_sc_convert
    modelImage                 = SERSIC_DISK(xx, yy, parameters)
    nearGalParams              = parameters
    nearGalParams[lengthInds] *= 1.8
    nearGal                    = SERSIC_DISK(xx, yy, nearGalParams)
  END
  'EXP_RADIAL_SECH2_VERTICAL_DISK': BEGIN
    parameters[0:1]            = [xcen, ycen]
    lengthInds                 = [4,5,8]
    parameters[lengthInds]    *= pl_sc_convert
    modelImage                 = EXP_RADIAL_SECH2_VERTICAL_DISK(xx, yy, parameters)
    nearGalParams              = parameters
    nearGalParams[lengthInds] *= 1.8
    nearGal                    = EXP_RADIAL_SECH2_VERTICAL_DISK(xx, yy, nearGalParams)
  END
  'BULGE_AND_ONE_SERSIC_DISK': BEGIN
    parameters[0:1]            = [xcen, ycen]
    lengthInds                 = [5,8,12]
    parameters[lengthInds]    *= pl_sc_convert
    modelImage                 = BULGE_AND_ONE_SERSIC_DISK(xx, yy, parameters)
    nearGalParams              = parameters
    nearGalParams[lengthInds] *= 1.8
    nearGal                    = BULGE_AND_ONE_SERSIC_DISK(xx, yy, nearGalParams)
  END
  'BULGE_AND_EXP_RADIAL_SECH2_VERTICAL_DISK': BEGIN
    parameters[0:1]            = [xcen, ycen]
    lengthInds                 = [5,7,8]
    parameters[lengthInds]    *= pl_sc_convert
    modelImage                 = BULGE_AND_EXP_RADIAL_SECH2_VERTICAL_DISK(xx, yy, parameters)
    nearGalParams              = parameters
    nearGalParams[lengthInds] *= 1.8
    nearGal                    = BULGE_AND_EXP_RADIAL_SECH2_VERTICAL_DISK(xx, yy, nearGalParams)
  END
ENDCASE

;Create a bitmask covering all pixels with brightnesses greater than the masking threshold
mask = modelImage GT maskingThreshold                       

IF stars THEN BEGIN
  ;Prepare to mask stars
  EXTAST, groupStruc.displayHeader, Astr
  AD2XY, groupStruc.starInfo.RAJ2000, groupStruc.starInfo.DEJ2000, Astr, xStars, yStars
  AD2XY, SXPAR(maskHeader, 'RA_ROT'), SXPAR(maskHeader, 'DEC_ROT'), Astr, Xgal, Ygal
  nearGal = nearGal    GT maskingThreshold                  ;Make a mask that is 1.2X bigger... (for locating nearby stars)
  xStars  = Xcen + (xStars - Xgal)*pl_sc_convert            ;Compute the x and y locations in the new plate scale
  yStars  = Ycen + (yStars - Ygal)*pl_sc_convert
  starWid = parameters[numParams - 3]
  
  IF groupStruc.NIRband EQ 'H' THEN magIndex = 5 ELSE $
    IF groupStruc.NIRband EQ 'Ks' THEN magIndex = 7
  
  brightStar   = groupStruc.starInfo.(magIndex) LE 15E      ;Find the bright stars
  closeStars   = nearGal[xStars, yStars]                    ;Find the stars in those near galaxy pixels
  maskStars    = WHERE(closeStars OR brightStar, numMasked) ;Find all the stars to mask
  magMaskStars = groupStruc.starInfo[maskStars].(magIndex)
  xMaskStars   = xStars[maskStars]                          ;Pick out the x and y locations of the stars to be masked
  yMaskStars   = yStars[maskStars]
  
  FOR i = 0, numMasked - 1 DO BEGIN
    IF (xMaskStars[i] GT 10) AND (xMaskStars[i] LT naxis1 - 10) AND $
      (yMaskStars[i] GT 10) AND (yMaskStars[i] LT naxis2 - 10) THEN BEGIN
      ;Compute a radius to mask bassed on the brightness of the star
      maskRad  = 10E^(0.80E*MIN(magMaskStars)/magMaskStars[i])*starWid*pl_sc_convert
      starMask = SQRT((xx - xMaskStars[i])^2 + $            ;Find pixels near this star
        (yy - yMaskStars[i])^2) LT maskRad
      mask = mask OR starMask                               ;Add those pixels to the mask
    ENDIF
  ENDFOR
ENDIF

RETURN, mask

END