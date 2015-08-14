PRO S4_DETERMINE_BOUNDARIES, event

  tlb_wid = WIDGET_INFO(event.top, FIND_BY_UNAME='WID_BASE')          ;Retrieve the TLB widget ID
  WIDGET_CONTROL, tlb_wid, GET_UVALUE=groupStruc                      ;Retrieve the group summary structure
  displayWID = WIDGET_INFO(event.top, FIND_BY_UNAME='IMAGE_DISPLAY_WINDOW')
  WIDGET_CONTROL, displayWID, GET_VALUE=windowIndex                   ;Retrieve display window WID
  WSET, windowIndex                                                   ;Set plot window WID
  
  S3files = groupStruc.analysis_dir + 'S3_Astrometry' $               ;File paths
    + PATH_SEP() + '*.fits'
  S3files = FILE_SEARCH(S3files, COUNT=nfiles)
  hwp = FLTARR(nfiles)                                                ;Initalize array to store HWP angles
  FOR j = 0, nfiles - 1 DO BEGIN
    header = HEADFITS(S3files[j])                                     ;Loop through ALL images in group
    hwp[j] = SXPAR(header, "MB_HWP")                                  ;Save THIS image HWP rotation angle
  ENDFOR
  sortArr     = SORT(hwp)                                             ;Sort the HWP angles
  S3files     = S3files[sortArr]                                      ;Sort the files to match the HWP angles
  hwp         = hwp[sortArr]
  hwp_uniq    = hwp[UNIQ(hwp)]                                        ;Grab the uniq HWP angles
  n_hwp       = N_ELEMENTS(hwp_uniq)                                  ;Count the number of uniq HWP angles
  file_exists = FILE_TEST(S3files)                                    ;Test which files exist
  
  numImages = BYTARR(n_hwp)                                           ;A vector to keep track dither number per HWP
  FOR j = 0, n_hwp - 1 DO BEGIN                                       ;Loop through all the uniq HWP angles
    HWPind        = WHERE(hwp EQ hwp_uniq[j] AND file_exists, numHWP) ;Find all the images at this HWP angle
    numImages[j] = numHWP                                             ;Store the number of dithers at this HWP angle
  ENDFOR
  HWP_select = MIN(WHERE(numImages EQ MAX(numImages)))                ;Select the first HWP angle with the MOST dither positions
  HWP_inds   = WHERE(hwp EQ hwp[HWP_select], numImages)               ;Grab the indices for the selected HWP angle images
  
  FOR j = 0, numImages - 1 DO BEGIN                                   ;Loop throug all the images with the selected HWP
    tmpHeader = HEADFITS(S3files[HWP_inds[j]])                        ;Read in this file's header
    EXTAST, tmpHeader, astr, noparams                                 ;Extract astrometry from header
    IF (N_ELEMENTS(all_astr) EQ 0) THEN BEGIN                         ;Store astrometry (test for first case)
      all_astr = astr                                                 ;Store all the astrometry
    ENDIF ELSE BEGIN
      all_astr = [all_astr, astr]
    ENDELSE
  ENDFOR
  
  PRINT_TEXT2, event, STRING(hwp[HWP_select], FORMAT='("Using HWP=",F5.1," to determine dither coverage")')
  
  deltaRA   = 1024*0.579D/3600D                                        ;Image width in degrees
  deltaDec  = 1026*0.579D/3600D                                        ;Image height in degrees
  limitsRA  = REBIN(TRANSPOSE(all_astr.crval[0]), 2, numImages, /SAMPLE) + $
    (0.5*REBIN([+deltaRA, -deltaRA], 2, numImages, /SAMPLE))/COS(REBIN(TRANSPOSE(all_astr.crval[1]), 2, numImages, /SAMPLE)*!DTOR)
  limitsDec = REBIN(TRANSPOSE(all_astr.crval[1]), 2, numImages, /SAMPLE) + $
    (0.5*REBIN([-deltaDec,+deltaDec], 2, numImages, /SAMPLE))

  limitsRA  = [MIN(limitsRA[0,*]), MAX(limitsRA[1,*])]
  limitsDec = [MAX(limitsDec[0,*]), MIN(limitsDec[1,*])]

;  FOR j = 1, N_ELEMENTS(all_astr) - 1 DO BEGIN                        ;Loop through all selected images and compute dither coverage
;    
;    limitRA1  = all_astr[j].crval[0] + (0.5*[-deltaRA, +deltaRA])*COS(all_astr[j].crval[1]*!DTOR)
;    limitDec1 = all_astr[j].crval[1] + (0.5*[-deltaDec,+deltaDec])
;    
;    IF (limitRA1[0] GT limitRA[0]) THEN BEGIN
;      limitRA[0] = limitRA1[0]
;    ENDIF
;    IF (limitRA1[1] LT limitRA[1]) THEN BEGIN
;      limitRA[1] = limitRA1[1]
;    ENDIF
;    IF (limitDec1[0] GT limitDec[0]) THEN BEGIN
;      limitDec[0] = limitDec1[0]
;    ENDIF
;    IF (limitDec1[1] LT limitDec[1]) THEN BEGIN
;      limitDec[1] = limitDec1[1]
;    ENDIF
;    IF ABS(limitRA[0] - 148.91413) LT 0.0001D THEN STOP
;    IF limitRA[0] GT limitRA[1] THEN STOP
;  ENDFOR
;    STOP  
  EXTAST,  groupStruc.displayHeader, displayAstr                      ;Grab the astrometry from the 2MASS image
  AD2XY, limitsRA, limitsDec, displayAstr, limitsX, limitsY           ;Compute the coverage limits in pixel space
  AD2XY, all_astr.crval[0], all_astr.crval[1], $                      ;Compute position of each dither pointing (in pixel space)
    displayAstr, pointX, pointY
  lfLimit = MIN(limitsX)
  rtLimit = MAX(limitsX)
  btLimit = MIN(limitsY)
  tpLimit = MAX(limitsY)
  
  groupStruc.coverageBoundaries = {RA:limitsRA, Dec:limitsDec}        ;Store the limits...
  UPDATE_GROUP_SUMMARY, event, groupStruc, $                          ;and update the group structure
    'coverageBoundaries', {RA:limitsRA, Dec:limitsDec}, /SAVE
  
  WIDGET_CONTROL, displayWID, GET_VALUE=windowIndex                   ;Retrieve display window WID
  WSET, windowIndex                                                   ;Set plot window WID
  OPLOT, pointX, pointY, PSYM=4, COLOR=RGB_TO_DECOMPOSED([0,255,255]) ;Overplot the dither pointings
  PLOTS, [lfLimit,lfLimit,rtLimit,rtLimit,lfLimit], $                 ;Draw the boundary of the dither coverage
    [btLimit,tpLimit,tpLimit,btLimit,btLimit]

END


PRO S4_MAGNITUDE_RANGE, event;, stash
  
  displayWID = WIDGET_INFO(event.top,FIND_BY_UNAME='IMAGE_DISPLAY_WINDOW');Grab the display window WID
  tlb_wid    = WIDGET_INFO(event.top,FIND_BY_UNAME='WID_BASE')        ;Grab the top-level-base WID
  wS4magRangeSlider = WIDGET_INFO(event.top, FIND_BY_UNAME='S4_MAG_RANGE')
  wS4magSliderCheck = WIDGET_INFO(event.top, FIND_BY_UNAME='S4_SELECT_PHOTOMETRY_MAGNITUDE_RANGE')
  stateStorageWID   = WIDGET_INFO(wS4magRangeSlider, /CHILD)
  WIDGET_CONTROL, tlb_wid, GET_UVALUE=groupStruc                      ;Grab the group summary structure
  WIDGET_CONTROL, displayWID, GET_VALUE = displayWindowIndex          ;Grab the display window index
  WIDGET_CONTROL, stateStorageWID, GET_UVALUE=state, /NO_COPY         ;Grab the widget "state" variable
  WIDGET_CONTROL, wS4magSliderCheck, GET_UVALUE=starsNearMask

  ;APPLY CHANGES TO SELECTED STARS
  IF groupStruc.NIRband EQ 'H' THEN magIndex = 5 $
    ELSE IF groupStruc.NIRband EQ 'Ks' THEN magIndex = 7

  groupStruc.photStarFlags = groupStruc.starInfo.(magIndex) GE state.value[0] $
    AND groupStruc.starInfo.(magIndex) LE state.value[1] $
    AND groupStruc.starInfo.RAJ2000 GT MIN(groupStruc.coverageBoundaries.RA) $
    AND groupStruc.starInfo.RAJ2000 LT MAX(groupStruc.coverageBoundaries.RA) $
    AND groupStruc.starInfo.DEJ2000 GT MIN(groupStruc.coverageBoundaries.Dec) $
    AND groupStruc.starInfo.DEJ2000 LT MAX(groupStruc.coverageBoundaries.Dec)

  groupStruc.photStarFlags = (groupStruc.photStarFlags AND ~starsNearMask)
  
  ;  SKY, groupStruc.displayImage, skyMode, skyNoise, /SILENT
  WSET, displayWindowIndex                                            ;Set that display window as active
  ;  TVIM, groupStruc.displayImage, RANGE = skyMode + [-1, +100]*skyNoise
  
  useStarInds = WHERE(groupStruc.photStarFlags, numUse, $             ;Find the stars in the selected magnitude range
    COMPLEMENT = noUseStarInds, NCOMPLEMENT = numNoUse)
    
  EXTAST, groupStruc.displayHeader, astr
  AD2XY, groupStruc.starInfo.RAJ2000, groupStruc.starInfo.DEJ2000, $
    astr, xStars, yStars
  
  IF numUse GT 0 THEN BEGIN
;    greenColorInd = RGB_TO_DECOMPOSED([0,255,0])
    OPLOT, (xStars[useStarInds]-1), (yStars[useStarInds]-1), $        ;Show the user where the selected stars are
      PSYM = 6, COLOR = RGB_TO_DECOMPOSED([0,255,0])
  ENDIF
  IF numNoUse GT 0 THEN BEGIN
;    redColorInd = RGB_TO_DECOMPOSED([255,0,0])
    OPLOT, (xStars[noUseStarInds]-1), (yStars[noUseStarInds]-1), $    ;Show the user where the unselected stars are
      PSYM = 6, COLOR = RGB_TO_DECOMPOSED([255,0,0])
  ENDIF
  
  ;Restore "state" variable
  WIDGET_CONTROL, stateStorageWID, SET_UVALUE=state, /NO_COPY         ;Grab the widget "state" variable
  
END


PRO S4_SELECT_PHOTOMETRY_MAGNITUDE_RANGE, event

  WIDGET_CONTROL, event.ID, GET_UVALUE=starsNearMask
  WIDGET_CONTROL, event.top, GET_UVALUE=groupStruc                    ;Retrieve the group summary structure
  wS4magRangeSlider    = WIDGET_INFO(event.top, FIND_BY_UNAME='S4_MAG_RANGE')
  wS4magRangeSensitive = WIDGET_INFO(wS4magRangeSlider, /SENSITIVE)   ;Determine if the slider widget is sensitive or not
  
  ;If the dual slider is desensitized, then sensitize it and change button value
  IF wS4magRangeSensitive EQ 0 THEN BEGIN
    WIDGET_CONTROL, wS4magRangeSlider, SENSITIVE = 1
    WIDGET_CONTROL, event.ID, SET_VALUE='Slider Active'

    IF N_ELEMENTS(starsNearMask) EQ 0 THEN BEGIN                          ;If there is no stored mask, build it and store it
      maskFile  = groupStruc.analysis_dir + 'S2_Ski_Jump_Fixes' + PATH_SEP() + 'Masking_files' + PATH_SEP() + 'mask2MASS.fits'
      mask2MASS = READFITS(maskFile)                                      ;Load in the 2MASS mask

      ;Test for stars in (or near) the galaxy mask
      EXTAST, groupStruc.displayHeader, astr                              ;Extract the image astrometry
      AD2XY, groupStruc.starInfo.RAJ2000, groupStruc.starInfo.DEJ2000,$   ;Convert star positions to pixel coordinates
        astr, xStars, yStars
      numStars      = N_ELEMENTS(xStars)                                  ;Count the number of stars
      starsNearMask = BYTARR(numStars)                                    ;Initalize an array for the tracking stars near mask
      sz2MASS       = SIZE(groupStruc.displayImage, /DIMENSIONS)          ;Get the size of the displayImage
      MAKE_2D, FINDGEN(sz2MASS[0]), FINDGEN(sz2MASS[1]), xx2MASS, yy2MASS ;Make a 2D array for all image pixel locations
      FOR i = 0, numStars - 1 DO BEGIN                                    ;Loop through each of the preliminary star selections
        distFromStar     = SQRT((xx2MASS - xStars[i])^2E + $              ;Compute the distance from each star
                                (yy2MASS - yStars[i])^2E)
        starsNearMask[i] = TOTAL((distFromStar LT 5) AND mask2MASS) GE 1 ;Check if the star is within 10 pixels of the mask
      ENDFOR
      WIDGET_CONTROL, event.ID, SET_UVALUE = starsNearMask
    ENDIF

  ENDIF
  
  ;IF the dual slider is sensitized, then desensiize it and change the button value
  IF wS4magRangeSensitive EQ 1 THEN BEGIN
    WIDGET_CONTROL, wS4magRangeSlider, SENSITIVE = 0
    WIDGET_CONTROL, event.ID, SET_VALUE='Slider Inactive'
    
    stateStorageWID = WIDGET_INFO(wS4magRangeSlider, /CHILD)
    WIDGET_CONTROL, stateStorageWID, GET_UVALUE=state, /NO_COPY       ;Grab the dual-slider "state" variable
    
    
    ;; Value label(s)
    ;--JDM 20150118-- figure out how many significant figures are needed for the display string
    minSigFigs      = 3                                               ;Force to display at least 3 sig-figs
    negativeSigns   = state.value NE ABS(state.value)                 ;Determine which values need a negative sign
    valueMagnitudes = ALOG10(ABS(state.value))                        ;This computes the magnitude of the values
    positiveMag     = valueMagnitudes EQ ABS(valueMagnitudes)         ;Determine if this number is greater than 1
    negativeMag     = ~positiveMag
    smallPosMag     = (CEIL(valueMagnitudes) LT minSigFigs)*positiveMag   ;Determine if the sig-figs span the decimal point
    mediumPosMag    = (CEIL(valueMagnitudes) EQ minSigFigs)*positiveMag   ;Determine if there is trailing decimal point
    largePosMag     = valueMagnitudes GE minSigFigs                       ;Determine if there are trailing numbers
    
    stringLength    = minSigFigs $                                    ;Begin with at least the minimum string length
      + 1*negativeSigns $                                             ;Account for a negative sign
      + (FLOOR(ABS(valueMagnitudes)) + 2)*negativeMag $               ;Account for leading zeros and decimal point
      + 1*smallPosMag $                                               ;Account spanning the decimal point
      + 2*mediumPosMag $                                              ;Account for a trailing decimal point
      + (CEIL(valueMagnitudes) - minSigFigs)*largePosMag              ;Account for trailing numbers
    IF TOTAL(ABS(stringLength) GT 10) GT 1 then stop
    
    decimalLength   = (stringLength - negativeSigns - 2)*negativeMag $;Negative magnitudes only leading zero, decimal, and sign
      + (minSigFigs - CEIL(valueMagnitudes))*smallPosMag $            ;Small positive magnitudes have only a few leading digits
      + 1*mediumPosMag $                                              ;Medium positive magnitudes have one trailing decimals
      + 0*largePosMag                                                 ;Large positive magnitudes have no trailing decimals
      
    valueIsSpecial = WHERE(ABS(state.value) EQ 1 $
      OR state.value EQ 0, countIsSpecial)                            ;Treat the special cases of 0, and +/-1
    IF countIsSpecial GT 0 THEN BEGIN
      stringLength[valueIsSpecial]  = 3 + negativeSigns[valueIsSpecial]
      decimalLength[valueIsSpecial] = 1
    ENDIF
    stringLength    = STRTRIM(stringLength, 2)                        ;Convert string length value to a string
    decimalLength   = STRTRIM(decimalLength, 2)                       ;Convert decimal length value to a string
    
    ;***** need to account for different types of (integer) values, but this should work for now *****
    formatStrings   = '(F' + stringLength + '.' + decimalLength + ')'
    
    
    formatString = '("Photometry will be performed using stars between ",' $
      + formatStrings[0] + '," and ",' $
      + formatStrings[1] + '," magnitudes.")'
    PRINT_TEXT2, event, $
      STRING(state.value, FORMAT=formatString)
      
    ;Flag the stars to use
    IF groupStruc.NIRband EQ 'H' THEN magIndex = 5 $                    ;Setup the structure tag for this band
    ELSE IF groupStruc.NIRband EQ 'Ks' THEN magIndex = 7
    groupStruc.photStarFlags = groupStruc.starInfo.(magIndex) GE state.value[0] $
      AND groupStruc.starInfo.(magIndex) LE state.value[1] $
      AND groupStruc.starInfo.RAJ2000 GT MIN(groupStruc.coverageBoundaries.RA) $
      AND groupStruc.starInfo.RAJ2000 LT MAX(groupStruc.coverageBoundaries.RA) $
      AND groupStruc.starInfo.DEJ2000 GT MIN(groupStruc.coverageBoundaries.Dec) $
      AND groupStruc.starInfo.DEJ2000 LT MAX(groupStruc.coverageBoundaries.Dec)

    UPDATE_GROUP_SUMMARY, event, groupStruc, /SAVE                    ;Store the updated data to disk
    WIDGET_CONTROL, stateStorageWID, SET_UVALUE=state, /NO_COPY       ;Restore the dual-slider "state" variable
    
  ENDIF
  
END



;PRO S4_GET_PHOTOMETRY_STARS, event
;  tlb_wid = WIDGET_INFO(event.top, FIND_BY_UNAME='WID_BASE')          ;Retrieve the TLB widget ID
;  WIDGET_CONTROL, tlb_wid, GET_UVALUE=groupStruc                      ;Retrieve the group summary structure
;  min_wid = WIDGET_INFO(event.top, FIND_BY_UNAME='S4_minMag')        ;Retrieve the min mag widget ID
;  WIDGET_CONTROL, min_wid, GET_UVALUE=minMag                          ;Retrieve the min mag value
;  max_wid = WIDGET_INFO(event.top, FIND_BY_UNAME='S4_maxMag')        ;Retrieve the max mag widget ID
;  WIDGET_CONTROL, max_wid, GET_UVALUE=maxMag                          ;Retrieve the max mag value
;  displayWID = WIDGET_INFO(event.top, FIND_BY_UNAME='IMAGE_DISPLAY_WINDOW')
;     
;  testDir = 'S4_Restricted_photometry'
;  IF ~FILE_TEST(testDir) THEN FILE_MKDIR, testDir
;  testDir = testDir + PATH_SEP() + 'photometryStars'
;  IF ~FILE_TEST(testDir) THEN FILE_MKDIR, testDir
;  
;  maskFile   = 'S3B_Supersky_subtraction' + PATH_SEP() + 'Masking_files' + PATH_SEP() + 'galMask.fits'
;  galMask    = READFITS(maskFile, maskHeader)
;  maskSz     = SIZE(galMask, /DIMENSIONS)
;  RA_cen     = SXPAR(maskHeader, 'RA_CEN')
;  Dec_cen    = SXPAR(maskHeader, 'DEC_CEN')
;  coaddPath  = 'S4_Restricted_photometry' + PATH_SEP() + 'coadds'
;  coaddFiles = FILE_SEARCH(coaddPath, '*quickCoadd.fits', COUNT = numCoadds)
;  IF groupStruc.numGroups NE numCoadds THEN STOP
;  
;  FOR i = 0, numCoadds - 1 DO BEGIN
;    coaddImg    = READFITS(coaddFiles[i])
;    coaddHeader = HEADFITS(coaddFiles[i])
;    EXTAST, coaddHeader, astr, noparams
;    nx         = astr.naxis[0]                              ;Grab coadd image x-axis size
;    ny         = astr.naxis[1]                              ;Grab coadd image y-axis size
;    padGalMask = galMask                                    ;Alias the galaxy mask for manipulation
;    IF nx GT maskSz[0] THEN BEGIN
;      padX         = (nx - maskSz[0])/2                     ;Compute the padding to add on each side
;      padArr       = FLTARR(padX, maskSz[1])                ;Create the padding array
;      padGalMask   = [padArr, padGalMask, padArr]           ;Pad the mask
;    ENDIF
;    IF ny GT maskSz[1] THEN BEGIN
;      padY         = (ny - maskSz[1])/2                     ;Compute the padding to add on each side
;      padArr       = FLTARR(nx, padY)                       ;Create the padding array
;      padGalMask   = [[padArr], [padGalMask], [padArr]]     ;Pad the mask
;    ENDIF
;  
;    AD2XY, RA_cen, Dec_cen, astr, xc, yc
;    
;    ra_off  = ROUND(xc - astr.crpix[0])                     ;Find the x shift for the mask
;    dec_off = ROUND(yc - astr.crpix[0])                     ;Find the y shift for the mask
;    print, "mask shift = ",ra_off, dec_off," in RA, Dec directions"
;    ;
;    ; cycle mask by this amount
;    ;
;    IF ra_off LT 0 THEN $
;      this_mask = [padGalMask[abs(ra_off):*,*], fltarr(abs(ra_off), ny)] $
;    ELSE IF ra_off GT 0 THEN $
;      this_mask = [fltarr(ra_off, ny), padGalMask[0:(nx-1-ra_off),*]] ELSE $
;      this_mask = padGalMask
;    IF dec_off LT 0 THEN $
;      this_mask = [[this_mask[*,abs(dec_off):*]], [fltarr(nx, abs(dec_off))]] $
;    ELSE IF dec_off GT 0 THEN $
;      this_mask = [[fltarr(nx, dec_off)], [this_mask[*,0:(ny-1-dec_off)]]] ELSE $
;      this_mask = this_mask
;    
;    lfTrim = SXPAR(coaddHeader, 'LFTRIM')                    ;Extract the dither overlap region
;    rtTrim = SXPAR(coaddHeader, 'RTTRIM')
;    btTrim = SXPAR(coaddHeader, 'BTTRIM')
;    tpTrim = SXPAR(coaddHeader, 'TPTRIM')
;    
;    xx = REBIN(REFORM(INDGEN(nx), nx, 1), nx, ny, /SAMPLE)
;    yy = REBIN(REFORM(INDGEN(ny), 1, ny), nx, ny, /SAMPLE)
;    border_mask = 1 - (xx GT lfTrim AND xx LT rtTrim $      ;Create a mask for the border trim
;      AND yy GT btTrim AND yy LT tpTrim)
;    padGalMask  = this_mask                                 ;Store the SHIFTED "padded galaxy mask"
;    this_mask   = this_mask OR border_mask                  ;Supplement mask with the border trim
;    
;    ;Retrieve the 2MASS star info
;    this_mirror = ['CfA', 'UK', 'CDS', 'CA', 'Jp']
;    
;    ;
;    nloop = 0L
;    WHILE nloop LT 4320L DO BEGIN                           ; 1 day at 20 sec per try
;      ;
;      vizier_flag = 0
;      FOR imirror = 0, N_ELEMENTS(this_mirror)-1 DO BEGIN
;        IF(vizier_flag EQ 0) THEN BEGIN
;          info = mp_QueryVizier('2MASS-PSC', astr.crval, 10, $
;          MIRROR=this_mirror[imirror], constraint='Qflg==AAA')
;          test = size(info)
;          IF test[0] NE 0 THEN vizier_flag = 1
;        ENDIF
;      ENDFOR
;      
;      IF(vizier_flag EQ 0) THEN BEGIN
;        info = 99   ;No Vizier servers available
;        ;
;        ; if no Vizier servers, wait 20s and retry
;        ;
;        PRINT, 'No Vizier Servers at ',SYSTIME(),' waiting 20s and retrying'
;        WAIT, 20
;        nloop++
;      ENDIF ELSE nloop = 4321L                              ;Force the loop to close
;    ENDWHILE
;    
;    info       = info[SORT(info.Hmag)]                      ;Sort the stars by brightness
;    
;;    sz = SIZE(galMask, /DIMENSIONS)                        ;Create 2D pixel number arrays
;;    nx = sz[0]
;;    ny = sz[1]
;;    xx = REBIN(REFORM(FINDGEN(nx), nx, 1), nx, ny, /SAMPLE)
;;    yy = REBIN(REFORM(FINDGEN(ny), 1, ny), nx, ny, /SAMPLE)
;
;    AD2XY, info.RAJ2000, info.DEJ2000, astr, xStar, yStar   ;Convert star positions to pixel coordinates
;    goodStars  = WHERE(info.Hmag GT minMag $                ;Only the stars in the right magnitude range shall pass
;      AND info.Hmag LT maxMag $
;      AND ~this_mask[ROUND(xStar), ROUND(yStar)], numGood)  ;Also apply the galaxy and border masking
;    IF numGood GT 0 THEN BEGIN
;      info  = info[goodStars]
;      xStar = xStar[goodStars]
;      yStar = yStar[goodStars]
;    ENDIF ELSE STOP
;    
;    ;    EXTAST, coaddHeader, coaddAstr                     ;Extract the coadd astrometry Header
;    nearGalaxy = INTARR(numGood)
;    FOR j = 0, numGood - 1 DO BEGIN
;      distance   = SQRT((xx - xStar[j])^2 + $               ;Compute pixel distances from the star locations
;        (yy - yStar[j])^2)
;      nearPixels = WHERE(distance LT 15, numNear)           ;Grab the pixels nearest to the star
;      ;Test if the star center is near the galaxy mask
;      IF numNear GT 1 THEN nearGalaxy[j]  = (TOTAL(padGalMask[nearPixels]) GT 1)
;    ENDFOR
;    
;    goodStars = WHERE(~nearGalaxy, numGood)                 ;Finally omit stars near to the galaxy mask
;    IF numGood GT 0 THEN photoStars = info[goodStars] ELSE STOP 
;
;  
;    AD2XY, photoStars.RAJ2000, photoStars.DEJ2000, astr, xPhoto, yPhoto
;    SKY, coaddImg, skymode, skynoise, /SILENT
;    TVIM, coaddImg, RANGE = skymode + [-3,+10]*skyNoise
;    PLOTS, [lfTrim, lfTrim, rtTrim, rtTrim, lfTrim], $
;           [btTrim, tpTrim, tpTrim, btTrim, btTrim]
;    PLOTSYM, 0
;    
;    deltaColor = 255/(numGood - 1)
;    HSV, 100, 100, 100, 100, 0, 300E/360E, colors
;;    starNums  = INDGEN(numGood)
;;    colorInds = (starNums)*deltaColor
;
;;    ;           red                         green                      blue
;;    colors    = colors[colorInds,0] + 256L*(colors[colorInds,1] + 256L*colors[colorInds,2])
;;    
;    FOR j = 0, numGood - 1 DO BEGIN
;      ;       red                            green                         blue
;      color = colors[j*deltaColor,0] + 256L*(colors[j*deltaColor,1] + 256L*colors[j*deltaColor,2])
;      OPLOT, [xPhoto[j]], [yPhoto[j]], PSYM = 8, COLOR = color
;      XYOUTS, [xPhoto[j]], [yPhoto[j]] - 60, /DATA, ALIGNMENT = 0.5, $
;        STRING(photoStars[j].Hmag, FORMAT='("H=",F4.1)'), COLOR = color
;      XYOUTS, [xPhoto[j]], [yPhoto[j]] + 20, /DATA, ALIGNMENT = 0.5, $
;        STRING(j+1, FORMAT='("#",F4.1)'), COLOR = color
;    ENDFOR
;
;    
;    ;Insert decision protocol (using GUI buttons)
;
;    photPath = 'S4_Restricted_photometry' + PATH_SEP() + 'photometryStars'
;    SAVE, photoStars, FILENAME = photPath + PATH_SEP() + groupStruc.groupNames[i] + '_photoStars.sav'
;  ENDFOR
;  PRINT, 'Done downloading photometry star data'
;END


PRO S4_PERFORM_PHOTOMETRY, event

  tlb_wid = WIDGET_INFO(event.top, FIND_BY_UNAME='WID_BASE')          ;Retrieve the TLB widget ID
  WIDGET_CONTROL, tlb_wid, GET_UVALUE=groupStruc                      ;Retrieve the group summary structure
  groupProgBarWID = WIDGET_INFO(event.top, FIND_BY_UNAME='GROUP_PROGRESS_BAR')
  imageProgBarWID = WIDGET_INFO(event.top, FIND_BY_UNAME='IMAGE_PROGRESS_BAR')

  S4dir = groupStruc.analysis_dir + 'S4_Restricted_photometry'
  IF ~FILE_TEST(S4dir, /DIRECTORY) THEN FILE_MKDIR, S4dir

  ;The following code sets up the basic properties for the G_FIND procedure to be used for ALL images
  ;fwhm         = 3.0                                          ; a 3 pixel FWHM is probably close enough for jazz
  ;roundlim     = [-1.0,0.85]                                  ; roundness limit for G_FIND procedure
  ;sharplim     = [0.1,0.85]                                   ; sharpness limit for G_FIND procedure
  nsigma       = 8.0                                          ; set the detection threshold in terms of sky noise
  sat_limit    = 1.4e4                                        ; set the basic saturation limit
  badpix       = [-1.0e4,sat_limit]                           ; pixels outside this range are bad
  bright_limit = sat_limit                                    ; set the star photometry saturation threshold

  ;The following code sets up the basic photometry parameters which will be used for ALL images
  ;(****stolen from S4_PSF_FIT.PRO and G_CREATE_GSP_GRID.PRO****)
  ref_ap     = [1.2, 1.7]                                     ; times ap_rad times FWHM
  ref_ap    *= 1.6                                            ; fudge factor produces better star results
  ;
  ;-------
  numFiles  = TOTAL(groupStruc.groupNumbers)                  ;Count up the total number of files that **SHOULD** be present
  photStars = WHERE(groupStruc.photStarFlags, numPhot)        ;Locate the photometry stars...
  IF groupStruc.NIRband EQ 'H' THEN magIndex = 5 $
    ELSE IF groupStruc.NIRband EQ 'Ks' THEN magIndex = 7
  starMags   = ((groupStruc.starInfo[photStars]).(magIndex))
;  ap_rad     = 2.80                                           ; times FWHM
  ap_rad     = 10E^(MIN(starMags)/60E)                        ; times FWHM
  clean_rad  = 3.6                                            ; times ap_rad times FWHM
  ap_vec     = [0.40, 0.475, 0.55, 0.625, 0.7, 0.775, $       ; times FWHM times ap_rad
    0.85, 0.925, 1.00, 1.050, 1.1, 1.150]
  n_ap       = N_ELEMENTS(ap_vec)
  rad_scale  = 3.00                                           ; times fwhm - sets PSF size
  rad_scale *= 1.25                                           ; 20120603 - DPC - fudge factor for better PSF modeling
  ronois     = 3.1                                            ; single image readout noise in ADU (S4_PSF_FIT.PRO)
  phpadu     = 8.21                                           ; photons per AUD (G_CREATE_PSF_GRID.PRO)
  sigma2FWHM = (2*SQRT(2*ALOG(2)))                            ; converstion from sigma to FWHM for a gaussian

;  completedCounter    = 0                                             ;Counter to keep track of how many images have been completed
  UPDATE_PROGRESSBAR, groupProgBarWID, /ERASE
;  UPDATE_PROGRESSBAR, imageProgBarWID, 0E, /PERCENTAGE

  numGoodGroups = TOTAL(groupStruc.groupFlags)
  groupNcount    = 0
  ;Loop thorugh each group
  FOR i = 0, groupStruc.numGroups - 1 DO BEGIN
    IF groupStruc.groupFlags[i] EQ 0 THEN CONTINUE
    groupNcount++
    groupProgString = STRING(groupNcount, numGoodGroups, FORMAT='("Group ",I2," of ",I2)')
    UPDATE_PROGRESSBAR, groupProgBarWID, 100*FLOAT(i+1)/numGoodGroups, DISPLAY_MESSAGE=groupProgString
    WAIT, 0.05
    nextPercentagePoint = 1B                                            ;Variable to track what the next progress update will be
    UPDATE_PROGRESSBAR, imageProgBarWID, 0E, /PERCENTAGE

    ;Loop through all the files in this group
    FOR j = 0, groupStruc.groupNumbers[i] - 1 DO BEGIN
      
      BDPfile = groupStruc.groupImages[i,j]                           ;Alias the BDP file paths
      S3file  = groupStruc.analysis_dir + 'S3_Astrometry' + $         ;Store the astrometry files
        PATH_SEP() + FILE_BASENAME(BDPfile)
      step3Test = FILE_TEST(S3file)                                   ;Test if a step 3 image is present
      astroFlag = (groupStruc.astroFlags[i,j] EQ 1)
      IF step3Test AND astroFlag THEN BEGIN
        photoRAs1  = groupStruc.starInfo[photStars].RAJ2000           ;Reset the RA and Dec list
        photoDecs1 = groupStruc.starInfo[photStars].DEJ2000
        imgBDP     = READFITS(BDPfile, /SILENT)                       ;Read in the BDP image
        img        = READFITS(S3file, imgHeader, /SILENT)             ;Read in the step 3 image
        sz         = SIZE(img, /DIMENSIONS)                           ;Grab the image dimensions
        EXTAST, imgHeader, astr, noparams                             ;Extract image astometry from header
        AD2XY, photoRAs1, photoDecs1, astr, photoX, photoY            ;Convert 2MASS (RA, Dec) into (x,y) positions
        xPhot = photoX & yPhot = photoY                               ;Alias the star positions to be refined
        
        ;***NAME SCHEME***
        ;xPhot and yPhot   = star position as deteced in the image
        ;photoX and photoY = star position as estimaed by (RA, Dec) and astrometry
        
        G_SKY, img, skymode, skysig, mfm_mean, mfm_stdev, $             ;Quickly generate sky statistics
          LLQ=llq, /SILENT
        nStars    = N_ELEMENTS(photoX)                                  ;Count the number of stars to find
        FWHMs     = FLTARR(nStars)                                      ;Array to store FWHM of each star
        keepStars = BYTARR(nStars)
        FOR k = 0, nStars - 1 DO BEGIN                                  ;Loop through each star and find its gcentrd value
          removeStars = -1                                              ;Reset the removeStars variable
          xOff     = ROUND(photoX[k] - 20) > 0
          xRt      = (xOff  + 40) < (sz[0] - 1)
          yOff     = ROUND(photoY[k] - 20) > 0
          yTop     = (yOff + 40)  < (sz[1] - 1)
          subArBDP = imgBDP[xOff:xRt, yOff:yTop]                        ;Cut out a subarray for testing for saturation
          subArray = img[xOff:xRt, yOff:yTop]                           ;Cut out a subarray for gaussian fitting
          result   = GAUSS2DFIT(subArray, A, /TILT)                     ;Gaussian fit the star
          inArray  = (A[4] GT 5) AND (A[4] LT 34) $                     ;If the fit is located in the center of the array
            AND (A[5] GT 5) AND (A[5] LT 34)
          okShape  = (A[2] GT 0.8) AND (A[2] LT 5) $                    ;and if its gaussian width is reasonable (not a hot pixel)
            AND (A[3] GT 0.8) AND (A[3] LT 5)

          IF inArray AND okShape THEN BEGIN
            FWHMs[k] = SQRT(A[2]*A[3])*sigma2FWHM                       ;Compute the FWHM for this star
            GCNTRD, subArray, A[4], A[5], xcen, ycen,  FWHMs[k]         ;Centroid this star (using estimated FWHM)
            xPhot[k] = xOff + xcen                                      ;Update the star x-position
            yPhot[k] = yOff + ycen                                      ;Update the star y-position
            
;            deltaPos = SQRT((xPhot[k] - A[4])^2 + (yPhot[k] - A[5])^2)
            deltaPos = SQRT((xcen - A[4])^2 + (ycen - A[5])^2)          ;Check if the centroid position is far from gaussian fit...
            IF deltaPos LE 1.1E THEN keepStars[k] = 1B
;            IF deltaPos LE 1 THEN keepStars[k] = 1B
;            IF deltaPos GT 1 THEN BEGIN                                 ;Test for large shifts
;              PRINT, 'Could not fit star #', k
;              PRINT, 'Adding to the "removeStars" list'
;              IF TOTAL(removeStars) EQ -1 THEN removeStars = [k] $
;              ELSE removeStars = [removeStars, k]                       ;Remove star from list for "problems fitting"
;            ENDIF
          
;          ENDIF ELSE BEGIN                                              ;If GAUS2DFIT failed then skip star
;            PRINT, 'Could not fit star #', k
;            PRINT, 'Adding to the "removeStars" list'
;            IF TOTAL(removeStars) EQ -1 THEN removeStars = [k] $
;            ELSE removeStars = [removeStars, k]                         ;Remove star from list for "problems fitting"                                  ;Remove star from list for "problems fitting"
          ENDIF
        ENDFOR
        
        mean_FWHM   = (MEDIAN_FILTERED_MEAN(FWHMs))[0]                  ;Compute the mean FWHM
        fit_scale   = 1.75                                              ;Times fwhm - sets fitting zone
        fit_rad     = fit_scale*mean_FWHM                               ;Fit the PSF within this radius
        rad_PSF     = CEIL(rad_scale*mean_FWHM)                         ;Modedl the PSF out to this radius
        rcrit       = SQRT(2)*MAX(ap_vec)*mean_FWHM*ap_rad              ;Star matching critical radius
        IF N_ELEMENTS(xPhot) GT 1 THEN BEGIN                            ;If there are multiple stars, check for neighboring stars
          GROUP, Event, xPhot, yPhot, 2*rcrit, group_vector             ;Group stars within sqrt(2)*max(apr) of each other
          ;DO NOT ATTEMPT PHOTOMETRY OF STARS WITH NEIGHBORS
          groups = group_vector[UNIQ(group_vector, SORT(group_vector))]   ;Parse out the group IDs
          FOR k = 0, N_ELEMENTS(groups) - 1 DO BEGIN                      ;Loop through all the stars
            groupInds = WHERE(group_vector EQ groups[k], groupCount)      ;Select the stars in this group
            IF groupCount GT 1 THEN keepStars[k] = 0B ;BEGIN                                 ;Save the star indices to remove
;              IF TOTAL(removeStars) EQ -1 THEN removeStars = groupInds $
;              ELSE removeStars = [removeStars, groupInds]
;            ENDIF
          ENDFOR

;          IF TOTAL(removeStars) NE -1 THEN BEGIN                          ;Test if there were any group stars to remove
;            REMOVE, removeStars, xPhot, yPhot, $                          ;Remove stars that were part of groups
;              photoRAs1, photoDecs1, group_vector
;          ENDIF
        ENDIF ELSE group_vector = [0]

        
        ;Now that ALL possible tests for good stars have been performed,
        ;cull the list to keep only the good stars, and continue
        keepInds = WHERE(keepStars, numKeep)
        IF numKeep EQ 0 THEN CONTINUE
        xPhot        = xPhot[keepInds]
        yPhot        = yPhot[keepInds]
;        photoRAs1    = photoRAs1[keepInds]
;        photoDecs1   = photoDecs1[keepInds]
        group_vector = group_vector[keepInds]

        SRCOR, xPhot, yPhot, photoX, photoY, $                          ;Match 2MASS entries with image star positions
          2*rcrit, ind1, ind2, OPTION = 1
        xPhot = xPhot[ind1] & yPhot = yPhot[ind1]                       ;Save the matched stars
        photoRAs1 = photoRAs1[ind2] & photoDecs1 = photoDecs1[ind2]     ;Save the matched stars
        nStars = N_ELEMENTS(ind1)                                       ;Recount the photometry stars
        
;        XY2AD, xPhot, yPhot, astr, photoRAs1, photoDecs1                ;Re-compute the star (RA, Dec) using updated astr

        ;Aperture photometry is performed for the pre-selected stars
        apr    = ap_vec * ap_rad * mean_FWHM                            ;Compute the apertures in terms of mean FWHM
        skyrad = ref_ap * ap_rad * mean_FWHM                            ;Compute sky annulus in terms of mean FWHM
        G_APER,img,xPhot,yPhot,mag,err,skyvalues,skyerr,phpadu,apr, $   ;Perform aperture photometry
          skyrad,badpix,bright_limit, /NO_BAD_PIX_FIX, /SILENT
        badStars = WHERE(REFORM(mag[0,*]) LT 0 OR mag GT 18, numBad, $  ;Test for unreasonable magnitude measurements
          COMPLEMENT = goodStars, NCOMPLEMENT = numGood)
        IF (numBad GT 0) AND (numGood GT 0) THEN BEGIN                  ;Only keep the good star information
          nStars       = numGood
          xPhot        = xPhot[goodStars]
          yPhot        = yPhot[goodStars]
          XY2AD, xPhot, yPhot, astr, photoRAs1, photoDecs1              ;Convert measured (x,y) to (RA,Dec)
;          photoRAs1    = photoRAs1[goodStars]
;          photoDecs1   = photoDecs1[goodStars]
          group_vector = group_vector[goodStars]
          G_APER,img,xPhot,yPhot,mag,err,skyvalues,skyerr,phpadu,apr, $ ;Re-compute aperture photometry
            skyrad,badpix,bright_limit, /NO_BAD_PIX_FIX, /SILENT
        ENDIF ELSE IF ~(numGood GT 0) THEN BREAK
        
        skymean = (MEDIAN_FILTERED_MEAN(skyvalues))[0]                  ;Compute a mean sky value in PPOL fashion
        skysig  = mfm_stdev
        
        ;Now that the acceptable stars have been selected
        ;and initial photometry has been done,
        ;a PSF model for this image is generated
        ;********************************************************
        ;******** EVENTUALLY LIMIT THIS TO PSF STAR LIST ********
        ;********************************************************
        n_total_PSF_stars = (N_ELEMENTS(xPhot) < 5)                     ;Limit to the five BRIGHTEST stars
        idpsf             = INDGEN(n_total_PSF_stars)                   ;Set the indices for PSF stars
        psfname = ''                                                    ;Null string prevents saving PSF file
        
        ;*****************************************************************************
        ;**********I NEED TO BE ABLE TO CATCH ERRORS FROM THIS PROCEDURE!*************
        ;*****************************************************************************
        ;IF i EQ 4 AND j EQ 47 THEN BREAK
        JM_GETPSF,img,xPhot,yPhot,mag,skyvalues,ronois,phpadu, $        ;Fit a PSF model
          gauss, psf, mean_fwhm, idpsf, rad_PSF, fit_rad, $
          x_w_psf, y_w_psf, PSF_mag, sum_resid, resid, psfname;, $
        ;NO_RESID = 1, DEBUG=1
        
        ;********* FIND A WAY TO *ONLY* KEEP THE PSF STARS THAT WORKED *****
        ;**** code goes here
        
        
        ;For more crowded fields, neighboring stars should be PSF subtracted
        ;and their photometry re-calculated...
        ;*******************************************************
        ;******* PHOTOMETRY RECALCULATION CODE GOES HERE *******
        ;*******************************************************
        ;      IF FILE_BASENAME(S3file) EQ '20130527.396_LDFC.fits' THEN STOP
        
        
        ng = 1                                                          ;Only one zone for PSF model
        FXADDPAR, imgHeader, 'HISTORY', "PPOL-Plus Step 4"
        SXADDPAR, imgHeader, 'PSF_NG', ng, "Number of X, Y grid zones in psf build"
        SXADDPAR, imgHeader, 'PSF_SIG', nsigma, "Detection threshold in sigma of sky"
        SXADDPAR, imgHeader, 'PSF_SAT', sat_limit, "Saturation Limit for allowed stars"
        SXADDPAR, imgHeader, 'PSF_FWHM', mean_FWHM,"FWHM in pixels"
        SXADDPAR, imgHeader, 'PSF_FRAD', fit_rad, "PSF fitting radius, in pixels"
        SXADDPAR, imgHeader, 'PSF_RSCL', rad_scale, "PSF model size, in FWHM units"
        ;
        SXADDPAR, imgHeader, 'PSFMAG', PSF_mag,"nominal PSF magnitude generated internally"
        ;
        SXADDPAR, imgHeader, 'NPSF', n_total_PSF_stars
        ;      nPSF = n_total_PSF_stars
        FOR inPSF = 0, n_total_PSF_stars - 1 DO BEGIN
          PSFnum = STRING((inPSF+1), format='("PSF_",I03)')
          SXADDPAR, imgHeader, PSFnum+"X",xPhot[idpsf[inPSF]],"X location of this PSF star"
          SXADDPAR, imgHeader, PSFnum+"Y",yPhot[idpsf[inPSF]],"Y location of this PSF star"
          SXADDPAR, imgHeader, PSFnum+"M",mag[idpsf[inPSF]],"Magnitude of this PSF star"
        ENDFOR
        ;
        ; add aperture phot info
        ;
        SXADDPAR, imgHeader, 'PP4_FWHM', mean_FWHM, "FWHM used in aperture phot, in pixels"
        SXADDPAR, imgHeader, 'PP4_ARAD', ap_rad, "Maximum aperture radius in FWHM units"
        SXADDPAR, imgHeader, 'PP4_REF0', ref_ap[0],"Reference aperture inner radius in FWHM units"
        SXADDPAR, imgHeader, 'PP4_REF1', ref_ap[1],"Reference aperture outer radius in FWHM units"
        
        ;***************** THIS IMPLIES THAT THE PSF WAS CELANED *****************************
        SXADDPAR, imgHeader, 'PP4_CLEN', clean_rad, "Radius to PSF clean to, in FWHM * ARAD units"
        ;***************** SHOULD SOMEDAY INCLUDE CLEAN PSF CODE *****************************
        
        SXADDPAR, imgHeader, 'PP4_NAP', n_ap,"Number of apertures used in Ap Phot"
        FOR iap = 0, n_ap-1 DO BEGIN
          name = 'PP4_AP'+STRING(iap,format='(I02)')
          SXADDPAR, imgHeader, name, ap_vec[iap], "Aperture "+STRING(iap+1,format='(I2)')+" in FWHM * ARAD units"
        ENDFOR
        ;
        ; and sky background and sigma info
        ;
        SXADDPAR, imgHeader, 'PP4_SKY', skymean, 'Mean sky value'
        SXADDPAR, imgHeader, 'PP4_SSKY', skysig, 'Sky standard deviation'
        SXADDPAR, imgHeader, 'PP4_LLQ', llq, 'LLQ bad flag (1=bad; 0=good)'
        ;      SXADDHIST, "PPOL-Plus Step 4", imgHeader
        
        ; write modified header, then phot info
        ;
        output_path = groupStruc.analysis_dir + 'S4_Restricted_photometry' + PATH_SEP() + $
          FILE_BASENAME(S3file, '.fits') + '_phot.dat'         ;Define the output filename
        OPENW, lun, output_path, /GET_LUN                               ;Open the file logical unit
        n_header = N_ELEMENTS(imgHeader)
        FOR ih = 0, n_header - 1 DO BEGIN                               ;Write the header to file
          PRINTF, lun, format='(";;",A)', imgHeader[ih]
        ENDFOR
        part1 = ";; Star    X       Y      RA         Dec     Sky   Group   "
        part2 =  "Mag[0]  Mag[1]  Mag[2]  Mag[3]  Mag[4]  Mag[5]  Mag[6]  Mag[7]  Mag[8]  Mag[9]  Mag[10] Mag[11]  "
        part3 = "sMag[0] sMag[1] sMag[2] sMag[3] sMag[4] sMag[5] sMag[6] sMag[7] sMag[8] sMag[9]sMag[10]sMag[11]"
        all_parts = part1 + part2 + part3
        PRINTF, lun, format='(a)', all_parts                            ;Write photometry column headers
        FOR is = 0, nStars - 1 DO BEGIN                                 ;Loop through each star and find its gcentrd value
          if(photoDecs1[is] gt 0.0) then dsign = '+' else dsign = '-'   ;Format the sign of the declination
          d = abs(photoDecs1[is])
          dten = FLOOR(d/10)
          d = d - 10 * dten
          PRINTF, lun, FORMAT='(I6,1x,F7.2,1x,f7.2,1x,f10.6,1x,a1,i1.1,f8.6,1x,f8.2,1x,i4,1x,12(f7.4,1x),12(f7.4,1x))',$
            is,xPhot[is],yPhot[is],photoRAs1[is],dsign,dten,d,$
            skyvalues[is],group_vector[is],mag[*,is],err[*,is]
        ENDFOR
        FREE_LUN, lun
      ENDIF
      
;      completedCounter++                                                ;Increment the completed counter
;      progressPercentage = 100*FLOAT(completedCounter)/FLOAT(numFiles)  ;Compute progress
      progressPercentage = 100E*FLOAT(j+1)/FLOAT(groupStruc.groupNumbers[i])
      IF progressPercentage GT nextPercentagePoint THEN BEGIN           ;Determine if the bar needs to be updated
        UPDATE_PROGRESSBAR, imageProgBarWID, progressPercentage, $      ;Update the progress bar if necessary
          /PERCENTAGE
          WAIT, 0.05
          nextPercentagePoint++                                         ;Increment next update
      ENDIF
    ENDFOR
  ENDFOR
  
  UPDATE_PROGRESSBAR, imageProgBarWID, 100E, /PERCENTAGE              ;Update the progress bar if necessary  
  PRINT_TEXT2, event, 'Finished all photometry'
END

PRO S4_CHECK_PHOTOMETRY, event
  
  DEVICE, DECOMPOSED = 1                                              ;Prepare to use 24-bit color
  HSV, 100, 100, 100, 100, 0, 302.4E/360E, colors                     ;Compute RGB colors
  ;        red                 green              blue
  colors = colors[*,0] + 256L*(colors[*,1] + 256L*colors[*,2])        ;Compute undecomposed, 24-bit color indices

  tlb_wid     = WIDGET_INFO(event.top, FIND_BY_UNAME='WID_BASE')      ;Retrieve the TLB widget ID
  WIDGET_CONTROL, tlb_wid, GET_UVALUE=groupStruc                      ;Retrieve the group summary structure
  groupProgBarWID = WIDGET_INFO(event.top, FIND_BY_UNAME='GROUP_PROGRESS_BAR')
  imageProgBarWID = WIDGET_INFO(event.top, FIND_BY_UNAME='IMAGE_PROGRESS_BAR')
  accept_wid  = WIDGET_INFO(event.top, $                              ;Retrieve the accept button widget ID
    FIND_BY_UNAME='S4_ACCEPT_IMAGE_PHOT')
  reject_wid  = WIDGET_INFO(event.top, $                              ;Retrieve the reject button widget ID
    FIND_BY_UNAME='S4_REJECT_IMAGE_PHOT')
  edit_wid    = WIDGET_INFO(event.top, $                              ;Retrieve the edit button widget ID
    FIND_BY_UNAME='S4_EDIT_STAR_LIST')
  sLabel_wid  = WIDGET_INFO(event.top, $                              ;Retrieve the star label widget ID
    FIND_BY_UNAME='S4_STAR_LABEL')
  sText_wid   = WIDGET_INFO(event.top, $                              ;Retrieve the star field widget ID
    FIND_BY_UNAME='S4_STAR_NUMBER')
  sDelete_wid = WIDGET_INFO(event.top, $                              ;Retrieve the delete button widget ID
    FIND_BY_UNAME='S4_DELETE_STAR')
    
  displayWID = WIDGET_INFO(event.top, FIND_BY_UNAME='IMAGE_DISPLAY_WINDOW');Retrieve display window WID
  WIDGET_CONTROL, displayWID, GET_VALUE=windowIndex                   ;Retrieve display window number
  WSET, windowIndex                                                   ;Set plot window WID


  numGoodGroups = TOTAL(groupStruc.groupFlags)
  groupNcount    = 0
  ;Loop thorugh each group
  FOR i = 0, groupStruc.numGroups - 1 DO BEGIN
    IF groupStruc.groupFlags[i] EQ 0 THEN CONTINUE
    groupNcount++
    groupProgString = STRING(groupNcount, numGoodGroups, FORMAT='("Group ",I2," of ",I2)')
    UPDATE_PROGRESSBAR, groupProgBarWID, 100*FLOAT(i+1)/numGoodGroups, DISPLAY_MESSAGE=groupProgString
    nextPercentagePoint = 1B                                            ;Variable to track what the next progress update will be
    UPDATE_PROGRESSBAR, imageProgBarWID, 0E, /PERCENTAGE
    WAIT, 0.05
    ;Loop through all the files in this group
    FOR j = 0, groupStruc.groupNumbers[i] - 1 DO BEGIN
      IF groupStruc.astroFlags[i,j] NE 1 THEN CONTINUE                ;Skip files that are flagged for disuse
      S4file = groupStruc.analysis_dir + 'S4_Restricted_photometry' + $   ;Set the S4 file name
        PATH_SEP() + FILE_BASENAME(groupStruc.groupImages[i,j], '.fits') + '_phot.dat'
      done = 0                                                        ;Reset the completion flag
      WHILE (done EQ 0) OR (done EQ 2) DO BEGIN
        READCOL, S4file, COMMENT = ';', /SILENT, $                    ;Read in all photometry files
          FORMAT = 'X,X,X,X,X,X,X,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F', $
          mag0,  mag1,  mag2,  mag3,  mag4,  mag5,  mag6,  mag7,  mag8,  mag9,  mag10,  mag11, $
          smag0, smag1, smag2, smag3, smag4, smag5, smag6, smag7, smag8, smag9, smag10, smag11
          
        ;Transpose their formatting to make looping through stars easier
        mags  = TRANSPOSE([[mag0],[mag1],[mag2],[mag3],[mag4],[mag5],[mag6],[mag7],[mag8],[mag9],[mag10],[mag11]])
        sMags = TRANSPOSE([[smag0],[smag1],[smag2],[smag3],[smag4],[smag5],[smag6],[smag7],[smag8],[smag9],[smag10],[smag11]])
        
        numStars     = N_ELEMENTS(mags[0,*])                            ;Count the number of stars
        normal_Mags  = mags - REBIN(mags[0,*], 12, numStars)            ;Normalize magnitudes to innermost aperture
        normal_sMags = SQRT(sMags^2 + REBIN(sMags[0,*], 12, numStars)^2);Adjust uncertainties in normalized mags
        deltaColor = 255/(numStars - 1)                                 ;Compute color stepping interval
        
        ;Statistically analyze if the photometry in this file need to be examined
        numAp      = N_ELEMENTS(mags[*,0])                              ;Count the number of apertures
        badStarAps = FLTARR(numAp, numStars)                            ;Initalize array for storing bad magnitude measurements
        wMean_Mags = FLTARR(numAp)                                      ;Initalize array for storing weight means
        wSig_Mags  = FLTARR(numAp)                                      ;Initalize array for storing uncertainties in the mean
        numBad     = 0                                                  ;Initalize a counter for the number of potentially bad stars
        FOR k = 1, numAp - 1 DO BEGIN                                   ;Loop through each APERTURE
          this_Mags    = REFORM(normal_Mags[k,*])                       ;Alias the relevant magnitudes...
          this_sMags   = REFORM(normal_sMags[k,*])                      ;...and their uncertainties
          this_weight  = (1D/this_sMags^2)                              ;Convert to statistical weights
          this_weight /= TOTAL(this_weight)                             ;Normalize the weights
          ;
          ;May not nead the median_filtered_mean
          ;
          ;        mfm_normMags   = MEDIAN_FILTERED_MEAN(REFORM(normal_Mags[k,*]));Compute median filtered mean deltaMag at THIS aperture
          wMean_Mag       = TOTAL(this_weight*this_Mags)                ;Compute weighted mean deltaMag at THIS aperture
          wMean_Mags[k]   = wMean_Mag                                   ;Store this weighted mean
          wSig_Mag        = 1D/SQRT(TOTAL(1D/this_sMags^2))             ;Compute the uncertainty in the weighted mean
          wSig_Mags[k]    = wSig_Mag                                    ;Store this unceirtainty in the weighted mean
          magDeviat       = ABS(this_Mags - wMean_Mag)                  ;Compute deviations from the mean value
          sigDeviat       = SQRT(wSig_Mag^2 + this_sMags^2)             ;Compute uncertainty in the mag deviation
          badStarAps[k,*] = (magDeviat/sigDeviat) GT 3E                 ;Mark measurements more than 3-sigma from mean
          ;        print, (magDeviat/sigDeviat)
        ENDFOR
        PLOT, [0, 12], [MIN(normal_Mags), MAX(normal_Mags)], /NODATA, $
          XSTYLE=1, XRANGE = [0,12], XTITLE = 'Aperture No.', $
          YSTYLE=1, YRANGE = [MIN(normal_Mags)-0.05E, MAX(normal_Mags)], YTITLE = 'Delta Mag'
        PLOTS, [0,12], [0,0]
        botLeftN = CONVERT_COORD(0.2E, (MIN(normal_Mags)-0.025E), /DATA, /TO_NORMAL)
        botLeftN = botLeftN[0:1]
        botLeftD = CONVERT_COORD(0.2E, (MIN(normal_Mags)-0.025E), /DATA, /TO_DEVICE)
        botLeftD = botLeftD[0]
        deltaY   = CONVERT_COORD([botLeftD,botLeftD], [0,!D.Y_CH_SIZE], /DEVICE, /TO_NORMAL)
        deltaY   = 1.2*(MAX(deltaY[1,*]) - MIN(deltaY[1,*]))
        FOR k = 0, numStars - 1 DO BEGIN                                ;Loop through each STAR
          OPLOTERROR, (mags[*,k] - mags[0,k]), smags[*,k], PSYM=-4, $   ;Plot the data and errors
            COLOR = colors[k*deltaColor], ERRCOLOR = colors[k*deltaColor]

          XYOUTS, botLeftN[0], (botLeftN[1] + deltaY*k), /NORMAL, $
            STRING(FORMAT='("#",I2,": mag(0) = ",F4.1)', k+1,mags[0,k]), COLOR = colors[k*deltaColor]
;          XYOUTS, 11.5, (mags[11,k] - mags[0,k])+0.005, /DATA, $
;            STRING(FORMAT='("#",I2," mag(0)")', k+1), COLOR = colors[k*deltaColor]
;          XYOUTS, 11.5, (mags[11,k] - mags[0,k])-0.005, /DATA, $
;            STRING(FORMAT='(F5.1)', mags[0,k]), COLOR = colors[k*deltaColor]
        ENDFOR
        OPLOT, wMean_Mags
        OPLOTERROR, wMean_Mags, wSig_Mags, PSYM=-4, THICK=2
        
        numBadAps   = TOTAL(badStarAps, 1)                              ;Count the number of bad apertures per star
        badStarInds = WHERE(numBadAps GT 3, numBadStars)                ;Find any stars with more than two bad apertures
        IF (numBadStars GT 0) OR (done EQ 2) THEN BEGIN                 ;If data need to be examined, enter this code
          IF numBadStars GT 0 THEN BEGIN
            FOR k = 0, numBadStars - 1 DO BEGIN                         ;If there are bad stars, then highlight the bad data
              thisBadStar = badStarInds[k]
              thisBadAps  = WHERE(badStarAps[*,thisBadStar])
              OPLOT, thisBadAps, (mags[thisBadAps,thisBadStar] - mags[0,thisBadStar]), $
                THICK = 2, SYMSIZE = 1.2, PSYM=6, COLOR = 255L
            ENDFOR            
          ENDIF

          decisionEvent = WIDGET_EVENT([accept_wid, reject_wid, edit_wid])  ;Wait for the user to decide what to do
          CASE decisionEvent.ID OF
            accept_wid: BEGIN                                           ;If the user accepts photometry, then do nothing
;              PRINT, 'Photometry accepted'
              done = 1                                                  ;Set the completion flag
            END
            reject_wid: BEGIN                                           ;If the user rejects photometry
              REJECT_FILE, event, groupStruc, i, j                      ;Delete the file and set image usage flag to 0
              PRINT_TEXT2, event, 'Photometry file deleted'
              UPDATE_GROUP_SUMMARY, event, groupStruc, /SAVE            ;Save the updates to disk
              done = 1                                                  ;Set the completion flag
            END
            edit_wid: BEGIN
              WIDGET_CONTROL, sLabel_wid, SENSITIVE = 1                 ;Sensitize the necessary widgets
              WIDGET_CONTROL, sText_wid, SENSITIVE = 1
              WIDGET_CONTROL, sDelete_wid, SENSITIVE = 1
              deleteEvent = WIDGET_EVENT(sDelete_wid)                   ;Wait for the delete button to be pushed
              IF deleteEvent.ID EQ sDelete_wid THEN BEGIN
                WIDGET_CONTROL, sText_wid, GET_VALUE=starNumbers        ;Grab the specified star numbers
                starNumbers = FIX(STRSPLIT(starNumbers, ' ,;', /EXTRACT));Parse which star(s) ought to be deleted
                DELETE_STAR_PHOTOMETRY, S4file, starNumbers    ;Delete these stars from the photometry file
              ENDIF
              WIDGET_CONTROL, sLabel_wid, SENSITIVE = 0                 ;Desensitize the necessary widgets
              WIDGET_CONTROL, sText_wid, SENSITIVE = 0
              WIDGET_CONTROL, sDelete_wid, SENSITIVE = 0
              done = 2                                                  ;Set the completion flag to '2' (edited file)
            END
          ENDCASE
        ENDIF ELSE BEGIN
          done = 1                                                      ;This image has no problems
          WAIT, 0.15                                                    ;Flash the image to the user and then proceed
        ENDELSE
      ENDWHILE
      
      ;Now that the photometry has been handles for THIS image, update the progress bar
      progressPercentage = 100E*FLOAT(j+1)/FLOAT(groupStruc.groupNumbers[i])
      IF progressPercentage GT nextPercentagePoint THEN BEGIN         ;Determine if the bar needs to be updated
        UPDATE_PROGRESSBAR, imageProgBarWID, progressPercentage, $    ;Update the progress bar if necessary
          /PERCENTAGE
        WAIT, 0.05
        nextPercentagePoint++                                         ;Increment next update
      ENDIF
    ENDFOR
    UPDATE_PROGRESSBAR, imageProgBarWID, 100E, /PERCENTAGE            ;Update the progress bar to 100%
  ENDFOR
  groupStruc.imageFlags = groupStruc.astroFlags EQ 1
  UPDATE_GROUP_SUMMARY, event, groupStruc, /SAVE
  
  PRINT_TEXT2, event, 'Finished checking all image photometry'
END