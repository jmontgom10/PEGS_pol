;FUNCTION S2_MASK_CHECK, event
;
;  buildMaskWID = WIDGET_INFO(event.top, FIND_BY_UNAME='S3B_BUILD_GALAXY_MASK')
;  WIDGET_CONTROL, buildMaskWID, SENSITIVE=event.select
;  
;  RETURN, event.select
;END

PRO S2_IRAC_PA, event

  IRAC_PA_WID = WIDGET_INFO(event.top, FIND_BY_UNAME='S2_IRAC_PA')
  WIDGET_CONTROL, IRAC_PA_WID, SET_UVALUE=event.select

END

FUNCTION NEAREST_POINT_ON_LINE, intSlope, xPoint, yPoint
  ; Given the intercept and slope of a line and an arbitrary point,
  ; Returns the point on the line NEAREST to the arbitrary point.
  mPerp = -1.0/intSlope[1]
  bPerp = yPoint - mPerp*xPoint
  xNear = (bPerp - intSlope[0])/(intSlope[1] - mPerp)
  yNear = intSlope[1]*xNear + intSlope[0]
  RETURN, [xNear, yNear]
END

;PRO S2_CONTOUR_RANGE, event;, img
;  yminWid = WIDGET_INFO(event.top, FIND_BY_UNAME='S2_CONTOUR_RANGE')
;  stop
;  WIDGET_EVENT()
;  RETURN, LEVELS
;  WIDGET_CONTROL, event.ID, GET_UVALUE
;END

PRO S2_GET_IRAC_PA, event

  displayWID        = WIDGET_INFO(event.top, FIND_BY_UNAME = 'IMAGE_DISPLAY_WINDOW')
  IRAC_PA_WID       = WIDGET_INFO(event.top, FIND_BY_UNAME = 'S2_IRAC_PA')
  wS2contourSlider  = WIDGET_INFO(event.top, FIND_BY_UNAME = 'S2_CONTOUR_RANGE')
  contourStateWID   = WIDGET_INFO(wS2contourSlider, /CHILD)
  wS2redrawButton   = WIDGET_INFO(event.top, FIND_BY_UNAME='S2_REDRAW_CONTOURS')
  wS2continueButton = WIDGET_INFO(event.top, FIND_BY_UNAME='S2_CONTINUE')
;  WIDGET_CONTROL, stateStorageWID, GET_UVALUE=state
  WIDGET_CONTROL, event.top, GET_UVALUE=groupStruc          ;Retrieve group structure
  WIDGET_CONTROL, displayWID, GET_VALUE=windowIndex         ;Retrieve display window WID
  WSET, windowIndex                                         ;Set plot window WID
  
  ;Setup the colors to be used
  redInd   = RGB_TO_DECOMPOSED([255,0,0])
  greenInd = RGB_TO_DECOMPOSED([0,255,0])
  
  
  ;Read in the IRAC file
  IRACfile = DIALOG_PICKFILE(TITLE='Select 3.6 micron IRAC image', $  ;Get the IRAC 3.6 micron image
    PATH=groupStruc.analysis_dir, FILTER='*.fits', /MUST_EXIST)
  IRACimg  = READFITS(IRACfile, IRACheader)                           ;Read in the IRAC image
  units    = SXPAR(IRACheader, 'BUNIT')
  
  sz = SIZE(IRACimg, /DIMENSIONS)
  nx = sz[0]
  ny = sz[1]
    
  SKY, IRACimg, skymode, skynoise, /SILENT
  imageRange = skymode + [0,+500]*skynoise
  TVIM, ALOG(IRACimg), RANGE = [-5,+5]
  XYOUTS, 0.5, 0.95, 'Click on the galaxy center', /NORMAL, ALIGNMENT=0.5
   
  XYcen = FIND_CENTROID(IRACimg)

  
;  ;Click on the galaxy center and HEXTRACT an 8 arcmin region
;  CURSOR, Xcen, Ycen, /DATA, /DOWN                        ;Estimage the galaxy center
;  centerImg = IRACimg[(Xcen-50):(Xcen+50), (Ycen-50):(Ycen+50)]
;  
;  done = 0
;  WHILE ~done DO BEGIN
;    TVIM, ALOG(centerImg);, RANGE=ALOG([1,20])      ;Display the central region
;    ;      stop
;    CURSOR, Xcen1, Ycen1, /DATA, /DOWN                    ;Better estimate the galaxy center
;    GCNTRD, centerImg, Xcen1, Ycen1, Xcen1, Ycen1, 3.0    ;Centroid the flux in that region
;    ;    IF (Xcen NE -1) AND (Ycen NE -1) THEN done = 1  ;Double check that the centroid was successful
;    
;    OPLOT, [Xcen1, Xcen1], [0, 200], $                    ;Mark the estimated galaxy center
;      LINESTYLE = 2, THICK = 2
;    OPLOT, [0, 200], [Ycen1, Ycen1], $
;      LINESTYLE = 2, THICK = 2
;      
;    XYOUTS, 0.5, 0.045, 'Right click to approve center fit', /NORMAL, ALIGNMENT=0.5
;    XYOUTS, 0.5, 0.0125, 'Left click to try again', /NORMAL, ALIGNMENT=0.5
;    
;    CURSOR, junk1, junk2, /DOWN
;    IF !MOUSE.button EQ 4 $                               ;Query user if they're done
;      THEN done = 1                                       ;Either exit or try again
;  ENDWHILE
;  Xgal = Xcen + (Xcen1-50)                               ;Shift the centroided value back to image coordinates
;  Ygal = Ycen + (Ycen1-50)
  
  Xgal = XYcen[0]
  Ygal = XYcen[1]
  
  GETROT, IRACheader, rotang, cdelt                       ;Grab the equatorial rotation of the image
  pl_sc = SQRT(ABS(cdelt[0]*cdelt[1]))*3600E              ;Compute the plate scale
  
  ;Compute an 8 arcmin square region centered on the galaxy
  x1    = ROUND(Xgal) - ROUND(300E/pl_sc) & x2 = ROUND(Xgal) + ROUND(300E/pl_sc)    ;Begin with an 8 arcmin region
  y1    = ROUND(Ygal) - ROUND(300E/pl_sc) & y2 = ROUND(Ygal) + ROUND(300E/pl_sc)
  
  ;Test if the boundaries of the cropped image are outside the starting image
  IF (x1 LT 0) OR (y1 LT 0) OR $
    (x2 GT (nx-1)) OR (y2 GT (ny-1)) THEN BEGIN
    deltaPix = MIN(ROUND([Xgal, Ygal, (nx - Xgal - 1), (ny - Ygal - 1)]))
    
    x1 = ROUND(Xgal) - deltaPix & x2 = ROUND(Xgal) + deltaPix
    y1 = ROUND(Ygal) - deltaPix & y2 = ROUND(Ygal) + deltaPix
    xGalCen = Xgal - x1
    yGalCen = Ygal - y1
  ENDIF ELSE BEGIN
    xGalcen = Xgal - x1
    yGalCen = Ygal - y1
  ENDELSE
  
  ;Making sure that dimensions are odd.
  if (x2-x1) mod 2 eq 1 then begin
    IF x2 EQ (nx-1) THEN x2-- ELSE x2++
  endif
  if (y2-y1) mod 2 eq 1 then begin
    IF y2 EQ (ny-1) THEN y2-- ELSE y2++
  endif
  
  ;      xGalCen = CEIL(0.5*(x2-x1)) & yGalCen = CEIL(0.5*(y2-y1))         ;Recompute the galaxy center for the cropped image
  ;
  HEXTRACT, IRACimg, IRACHeader, IRACcropImg, IRACcropHeader, x1, x2, y1, y2
  EXTAST, IRACcropHeader, cropAstr
  nx = cropAstr.naxis[0]
  ny = cropAstr.naxis[1]
  
  nanInds = WHERE(~FINITE(IRACcropImg), numNans)                      ;Find the NANs
  IF numNans GT 0 $                                                   ;Fill in NANs with model background
    THEN IRACcropImg[nanInds] = skymode + skynoise*randomn(seed,numnans)
  
  
;  levels = S2_GET_CONTOUR_LEVELS(event, IRACcropImg)
  PRINT_TEXT2, event, 'Activating contour level slider'
  WIDGET_CONTROL, wS2contourSlider, SENSITIVE=1
  WIDGET_CONTROL, wS2redrawButton, SENSITIVE=1
  WIDGET_CONTROL, wS2continueButton, SENSITIVE=1
  
  numLevels = 3                                             ;Insist on three levels (change later?)
  
  ;Draw the default contour levels
  WIDGET_CONTROL, contourStateWID, GET_UVALUE=state
  ymin   = MIN(state.value)
  ymax   = MAX(state.value)
  levels = ((FINDGEN(numlevels)/(numLevels-1E))^2E)*(ymax-ymin) + ymin
  IRACsmoothImg = GAUSS_SMOOTH(IRACcropImg, 4/pl_sc)
  TVIM, ALOG(IRACcropImg), RANGE = [-4,+5]
  PRINT_TEXT2, event, 'Drawing contours with levels ' + STRJOIN(SIG_FIG_STRING(levels, 3), ', ') + ' ' + units
  CONTOUR, IRACsmoothImg, LEVELS = levels, /OVERPLOT

  ;Enter redraw loop until contours have been approved
  contourDone = 0
  WHILE ~contourDone DO BEGIN
    buttonPush = WIDGET_EVENT([wS2contourSlider, wS2redrawButton, wS2continueButton])
    IF buttonPush.ID EQ wS2redrawButton THEN BEGIN
      WIDGET_CONTROL, contourStateWID, GET_UVALUE=state
      ymin   = MIN(state.value)
      ymax   = MAX(state.value)
      levels = ((FINDGEN(numlevels)/(numLevels-1E))^2E)*(ymax-ymin) + ymin
      WSET, windowIndex
      TVIM, ALOG(IRACcropImg), RANGE = [-4,+5]
      PRINT_TEXT2, event, 'Drawing contours with levels ' + STRJOIN(SIG_FIG_STRING(levels, 3), ', ') + ' ' + units
      CONTOUR, IRACsmoothImg, LEVELS = levels, /OVERPLOT
    ENDIF
    IF buttonPush.ID EQ wS2continueButton THEN contourDone   = 1
  ENDWHILE
  WSET, windowIndex
  WIDGET_CONTROL, wS2contourSlider, SENSITIVE=0
  WIDGET_CONTROL, wS2redrawButton, SENSITIVE=0
  WIDGET_CONTROL, wS2continueButton, SENSITIVE=0
  PRINT_TEXT2, event, 'Using contour levels ' + STRJOIN(SIG_FIG_STRING(levels, 3), ', ') + ' Jy/Sr'

  
  ;Click on stars to be removed
  ;left click will subtract a star
  ;right click will proceed without further subtraction
  done = 0
  WHILE ~done DO BEGIN
;    POLYFILL, [0,0,1,1,0],[0.92,1.0,1.0,0.92,0.92], /NORMAL, COLOR=!P.background
    XYOUTS, 0.5, 0.045, 'Left click on stars to subtract', /NORMAL, ALIGNMENT=0.5
    XYOUTS, 0.5, 0.0125, 'Right click to proceed without further subtraction', /NORMAL, ALIGNMENT=0.5
    
    CURSOR, Xstar, Ystar, /DATA, /DOWN                ;Estimate star position
    IF (!MOUSE.button NE 4) THEN BEGIN
      lfIn = (ROUND(Xstar) - 50) > 0
      rtIn = (lfIn + 100) < (nx - 1)
      lfIn = rtIn - 100
      btIn = (ROUND(Ystar) - 50) > 0
      tpIn = (btIn + 100) < (ny - 1)
      btIn = tpIn - 100
      starImg = IRACcropImg[lfIn:rtIn, btIn:tpIn]
      
      doneStar = 0
      WHILE ~doneStar DO BEGIN
        TVIM, ALOG(starImg);, RANGE=ALOG([1,20])            ;Display the central region
        ;***CLICK CAREFULLY!***
        CURSOR, Xcen1, Ycen1, /DATA, /DOWN                  ;Click on the star center
        GCNTRD, starImg, Xcen1, Ycen1, Xcen, Ycen, 6.0
        
        IF (Xcen GT 0) AND (Ycen GT 0) THEN BEGIN
          OPLOT, [Xcen, Xcen], [0, 101], $                  ;Mark the estimated star center
            LINESTYLE = 2, THICK = 2
          OPLOT, [0, 101], [Ycen, Ycen], $
            LINESTYLE = 2, THICK = 2
                    
          XYOUTS, 0.5, 0.045, 'Right click to approve center fit', /NORMAL, ALIGNMENT=0.5
          XYOUTS, 0.5, 0.0125, 'Left click to try again', /NORMAL, ALIGNMENT=0.5

          CURSOR, junk1, junk2, /DOWN
          IF !MOUSE.button EQ 4 THEN BEGIN                            ;Query user if they're done
            starSubImg = starImg[(Xcen-5):(Xcen+5),(Ycen-5):(Ycen+5)] ;Grab a small array on star
            gaussFit   = GAUSS2DFIT(starSubImg, A, /TILT)             ;Fit a gaussian to the star
            MAKE_2D, FINDGEN(101), FINDGEN(101), xx, yy               ;Generate pixel positions for subarray
            starDist   = SQRT((xx-Xcen)^2 + (yy-Ycen)^2)              ;Compute distance from star centroid
            mask       = starDist LT 6.5                              ;Make a mask to cover the star
            starNans   = starImg
            starNans[WHERE(mask)] = !VALUES.F_NAN
            starImg    = INPAINT_NANS(starNans, SMOOTHING_RESOLUTION = 2)
            ;
            ;Finish by replacing the original portion of the image with the inpainted subarray
            ;
            IRACcropImg[lfIn:rtIn, btIn:tpIn] = starImg
            doneStar = 1
          ENDIF
        ENDIF
      ENDWHILE
      IRACsmoothImg = GAUSS_SMOOTH(IRACcropImg, 4/pl_sc)          ;Recompute smoothed image SANS stars
      TVIM, ALOG(IRACcropImg), RANGE = [-5,+5]
      CONTOUR, IRACsmoothImg, LEVELS = levels, /OVERPLOT
    ENDIF ELSE done = 1                                           ;If the right button was clicked, then exit loop
  ENDWHILE
  
  ;Click on the isophote extremes to estimate PA
  axisStrs = ['Click on the major axis extremes','Click on the minor axis extremes']
;  levels = (FINDGEN(3)+1)                                 ;Contour bightness levels
  labels = SIG_FIG_STRING(levels,2) + ' ' + units         ;Contour labels
  logRange     = ALOG(skyMode + [1, 1E3]*skyNoise)        ;Establish a log-scaled image display range
  ellipseAxes  = FLTARR(2)                                ;Create an array for storing ellipse axes length
  done         = 0                                        ;Test for if the user is done marking ellipses
  WHILE ~done DO BEGIN
    TVIM, ALOG(IRACcropImg), RANGE=logRange               ;Show the image
    OPLOT, [xGalCen], [yGalCen], PSYM=4, COLOR=redInd     ;Overplot the galaxy center
    CONTOUR, IRACsmoothImg, $                             ;Overplot the smoothed contours
      /OVERPLOT, LEVELS = levels, C_ANNOTATION = labels
      
    FOR i = 0, 1 DO BEGIN                                 ;Loop through the major and minor axes
      POLYFILL, [0,0,1,1,0],[0.92,1.0,1.0,0.92,0.92], /NORMAL, COLOR=!P.background
      XYOUTS, 0.5, 0.95, axisStrs[i], /NORMAL, ALIGNMENT=0.5
      CURSOR, Xend1, Yend1, /DATA, /DOWN                  ;Estimate one end of the axis
      OPLOT, [Xend1], [Yend1], PSYM=4, COLOR=redInd
      CURSOR, Xend2, Yend2, /DATA, /DOWN                  ;Estimate the other end of the axis
      OPLOT, [Xend2], [Yend2], PSYM=4, COLOR=redInd
      IF (i EQ 0) THEN BEGIN                              ;Computre major axis slope (PA) and length
        majorIntSlope = LINFIT([Xend1, xGalCen, Xend2], [Yend1, yGalCen, Yend2])
        PA            = (ATAN(majorIntSlope[1])*!RADEG + 2*360 - 90) MOD 180
        xyNear1       = NEAREST_POINT_ON_LINE(majorIntSlope, Xend1, Yend1)
        xyNear2       = NEAREST_POINT_ON_LINE(majorIntSlope, Xend2, Yend2)
        
        OPLOT, [xyNear1[0], xyNear2[0]], [xyNear1[1], xyNear2[1]], COLOR=greenInd
        ellipseAxes[i] = 0.5*SQRT((xyNear1[0] - xyNear2[0])^2 + $
          (xyNear1[1] - xyNear2[1])^2)*(pl_sc/0.579)
          
      ENDIF ELSE IF (i EQ 1) THEN BEGIN                   ;Compute minor axis length
        minorIntSlope = [(yGalCen + (1.0/majorIntSlope[1])*xGalCen), (-1.0/majorIntSlope[1])]
        xyNear1       = NEAREST_POINT_ON_LINE(minorIntSlope, Xend1, Yend1)
        xyNear2       = NEAREST_POINT_ON_LINE(minorIntSlope, Xend2, Yend2)
        
        OPLOT, [xyNear1[0], xyNear2[0]], [xyNear1[1], xyNear2[1]], COLOR=greenInd
        ellipseAxes[i] = 0.5*SQRT((xyNear1[0] - xyNear2[0])^2 + $
          (xyNear1[1] - xyNear2[1])^2)*(pl_sc/0.579)
          
        XYOUTS, 0.5, 0.045, 'Left click to try again', /NORMAL, ALIGNMENT=0.5
        XYOUTS, 0.5, 0.0125, 'Right click accept axes', /NORMAL, ALIGNMENT=0.5
        PRINT_TEXT2, event, 'PA estimate = ' + SIG_FIG_STRING(PA, 6) + ' deg.'
        ;          XYOUTS, 0.5, 0.85, 'PA = ' + SIG_FIG_STRING(PA, 3) + ' deg.', /NORMAL, ALIGNMENT = 0.5, CHARSIZE = 2, CHARTHICK = 2
        
        CURSOR, junk1, junk2, /DOWN                       ;After the minor axis, ask the user whay they want to do
        IF !MOUSE.button EQ 4 THEN done = 1               ;Query user if they're done
      ENDIF
    ENDFOR
  ENDWHILE
  
  done      = 0
  iterCount = 1
  WHILE ~done DO BEGIN
    ;Rotate the image so that major-axis = y-axis
    PRINT_TEXT2, event, 'Beginning PA routine: iteration ' + STRTRIM(iterCount,2)
    HROT, IRACcropImg, IRACcropHeader, IRACrotImg, IRACrotHeader, $
      PA, xGalCen, yGalCen, 0, /PIVOT, MISSING = !VALUES.F_NAN
      
    nanInds = WHERE(~FINITE(IRACcropImg), numNans)                  ;Find the NANs
    IF numNans GT 0 $                                               ;Fill in NANs with model background
      THEN IRACcropImg[nanInds] = skymode + skynoise*randomn(seed,numnans)
      
    ;Smooth-image and find maximum and minimum of isophote contours
    PRINT_TEXT2, event, 'Smoothing rotated Image
    smoothRotImg = GAUSS_SMOOTH(IRACrotImg, 4/pl_sc)
    PRINT_TEXT2, event, 'Contouring the smoothed, rotated image'
    CONTOUR, smoothRotImg, LEVELS=levels, PATH_XY=path_xy, PATH_INFO=path_info, /PATH_DATA_COORDS
    
    galaxyInds = LONARR(numLevels)                                  ;Initalize an array to store the indices of galaxy paths
    FOR i = 0, numLevels - 1 DO BEGIN                               ;Loop through each level
      levelInds      = WHERE(path_info.level EQ i, numPaths)        ;Select the paths for THIS level
      longestOfLevel = (REVERSE(SORT(path_info.N)))[0]              ;Find the longest path for THIS level
      galaxyInds[i]  = levelInds[longestOfLevel]                    ;Store the corresponding path index
    ENDFOR
    galaxyContours = path_info[galaxyInds]                          ;Select the LONGEST three contours

    axisXs = FLTARR(2, numLevels)                                   ;Initalize an array for the contour X extreema
    axisYs = FLTARR(2, numLevels)                                   ;Initalize an array for the contour Y extreema
    FOR i = 0, numLevels - 1 DO BEGIN                               ;Loop through each contour
      numberInPath = [INDGEN(galaxyContours[i].N), 0]                    ;Generate a set of indices for the PATH_XY variable
      contX = REFORM(path_xy[0, galaxyContours[i].OFFSET + numberInPath ]);Select the X values for THIS contour
      contY = REFORM(path_xy[1, galaxyContours[i].OFFSET + numberInPath ]);Select the Y values for THIS contour
      
      topInd   = (WHERE(contY EQ MAX(contY)))[0]                    ;Select the index with the HIGHEST Y value
      botInd   = (WHERE(contY EQ MIN(contY)))[0]                    ;Select the index with the LOWEST Y value
      deltaInd = ROUND(0.05*galaxyContours[i].N)                         ;Determine 5% of the path perimeter
      
      IF (topInd+deltaInd) GT galaxyContours[i].N THEN BEGIN             ;Select the path maximum +/- 5%
        numOffEdge = (topInd+deltaInd) - galaxyContours[i].N
        topX       = contX[(topInd-deltaInd):(galaxyContours[i].N)]
        topX       = [topX, contX[0:numOffEdge]]
        topY       = contY[(topInd-deltaInd):(galaxyContours[i].N)]
        topY       = [topY, contY[0:numOffEdge]]
      ENDIF ELSE IF (topInd-deltaInd) LT 0 THEN BEGIN
        numOffEdge = ABS(topInd-deltaInd)
        topX       = contX[(galaxyContours[i].N-numOffEdge):galaxyContours[i].N]
        topX       = [topX, contX[0:(2*deltaInd - N_ELEMENTS(topX))]]
        topY       = contY[(galaxyContours[i].N-numOffEdge):galaxyContours[i].N]
        topY       = [topY, contY[0:(2*deltaInd - N_ELEMENTS(topY))]]
      ENDIF ELSE BEGIN
        topX = contX[(topInd-deltaInd):(topInd+deltaInd)]
        topY = contY[(topInd-deltaInd):(topInd+deltaInd)]
      ENDELSE
      
      IF (botInd+deltaInd) GT galaxyContours[i].N THEN BEGIN             ;Select the path minimum +/- 5%
        numOffEdge = (botInd+deltaInd) - galaxyContours[i].N
        botX       = contX[(botInd-deltaInd):(galaxyContours[i].N)]
        botX       = [botX, contX[0:numOffEdge]]
        botY       = contY[(botInd-deltaInd):(galaxyContours[i].N)]
        botY       = [botY, contY[0:numOffEdge]]
      ENDIF ELSE IF (botInd-deltaInd) LT 0 THEN BEGIN
        numOffEdge = ABS(botInd-deltaInd)
        botX       = contX[(galaxyContours[i].N-numOffEdge):galaxyContours[i].N]
        botX       = [botX, contX[0:(2*deltaInd - N_ELEMENTS(botX))]]
        botY       = contY[(galaxyContours[i].N-numOffEdge):galaxyContours[i].N]
        botY       = [botY, contY[0:(2*deltaInd - N_ELEMENTS(botY))]]
      ENDIF ELSE BEGIN
        botX = contX[(botInd-deltaInd):(botInd+deltaInd)]
        botY = contY[(botInd-deltaInd):(botInd+deltaInd)]
      ENDELSE
      
      dydxTop  = DERIV(topX, topY)                                  ;Compute the derivative about the path maximum
      topMax   = (WHERE(ABS(dydxTop) EQ MIN(ABS(dydxTop))))[0]      ;Select where the derivative passes through zero
      axisXs[0,i] = topX[topMax]                                    ;Select the X value for the path maximum
      axisYs[0,i] = topY[topMax]                                    ;Select the Y alue for the path maximum
      
      dydxBot  = DERIV(botX, botY)                                  ;Compute the derivative about the path minimum
      botMin   = (WHERE(ABS(dydxBot) EQ MIN(ABS(dydxBot))))[0]      ;Select where the derivative passes through zero
      axisXs[1,i] = botX[botMin]                                    ;Select the X value for the path minimum
      axisYs[1,i] = botY[botMin]                                    ;Select the Y value for the path minimum
    ENDFOR
    
    OPLOT, [REFORM(axisXs[0,*]), REFORM(axisXs[1,*])], $
      [REFORM(axisYs[0,*]), REFORM(axisYs[1,*])], PSYM = 4, COLOR = redInd
      
    ;Save the RA and Dec of the isophote locations
    ;      EXTAST, IRACrotHeader, rotAstr
    ;      XY2AD, axisXs, axisYs, rotAstr, axisRAs, axisDecs
    
    ;Compute the (x, y) coordinates of isophote extremes in the original image coordinates
    TVIM, ALOG(IRACcropImg), RANGE = logRange
    ;      AD2XY, axisRAs, axisDecs, cropAstr, axisXs, axisYs
    XYXY, IRACrotHeader, IRACcropHeader, axisXs, axisYs, axisXs, axisYs
    
    CONTOUR, IRACsmoothImg, $                                       ;Overplot the smoothed contours
      /OVERPLOT, LEVELS = levels, C_ANNOTATION = labels;, COLOR = RGB_TO_DECOMPOSED([0,255,255])
      
    ;Fit a line through the isophote extremes
    axisXs1       = [REFORM(axisXs[0,*]), REFORM(axisXs[1,*])]      ;Unwrap the contour extreema
    axisYs1       = [REFORM(axisYs[0,*]), REFORM(axisYs[1,*])]
    majorIntSlope = LINFIT(axisXs1, axisYs1)                        ;Fit a line through the contour extreema
    PA_old        = PA                                              ;Save old PA value
    PA            = (ATAN(majorIntSlope[1])*!RADEG + 2*360 - 90) MOD 180  ;Compute new PA value
    leftInd       = WHERE(axisXs1 EQ MIN(axisXs1))                  ;Select the leftmost contour extreemum
    rightInd      = WHERE(axisXs1 EQ MAX(axisXs1))                  ;Select the rightmost contour etreemum
    xyLeft        = NEAREST_POINT_ON_LINE(majorIntSlope, axisXs1[leftInd], axisYs1[leftInd])
    xyRight       = NEAREST_POINT_ON_LINE(majorIntSlope, axisXs1[rightInd], axisYs1[rightInd])
    OPLOT, [xyLeft[0], xyRight[0]], [xyLeft[1], xyRight[1]], COLOR=greenInd
    OPLOT, axisXs1, axisYs1, PSYM = 4, COLOR = redInd
    ;      XYOUTS, 0.5, 0.85, 'PA = ' + SIG_FIG_STRING(PA, 3) + ' deg.', /NORMAL, ALIGNMENT = 0.5, CHARSIZE = 2, CHARTHICK = 2
    PRINT_TEXT2, event, 'new PA estimate = ' + SIG_FIG_STRING(PA, 6) + ' deg.'
    done = (ABS(PA_old - PA) LT 0.05)                               ;Test if 0.1 degree tolerance has been reached
    
    ;Find the point on the line closest to the bright galaxy center (SHOULD THIS BE FORCED AS GAL CENTER?)
    ;
    ;Not sure how to handle consistent 'galaxy center' value
    
    iterCount++
  ENDWHILE
  IRAC_PA = PA
  PRINT_TEXT2, event, 'Final PA = ' + SIG_FIG_STRING(PA, 6) + ' deg.'
  
  UPDATE_GROUP_SUMMARY, event, groupStruc, 'GAL_PA', PA, /SAVE
END

PRO S2_BUILD_GALAXY_MASK, event

  displayWID  = WIDGET_INFO(event.top, FIND_BY_UNAME='IMAGE_DISPLAY_WINDOW')
  IRAC_PA_WID = WIDGET_INFO(event.top, FIND_BY_UNAME = 'S2_IRAC_PA')
  WIDGET_CONTROL, event.top, GET_UVALUE=groupStruc          ;Retrieve group structure
  WIDGET_CONTROL, displayWID, GET_VALUE=windowIndex         ;Retrieve display window WID
  WSET, windowIndex                                         ;Set plot window WID
  
  ;Setup the colors to be used
  redInd   = RGB_TO_DECOMPOSED([255,0,0])
  greenInd = RGB_TO_DECOMPOSED([0,255,0])
  
  ;Get the IRAC based PA if the checkbox is marked
  WIDGET_CONTROL, IRAC_PA_WID, GET_UVALUE=useIRAC_PA
  IF useIRAC_PA THEN BEGIN
    PA      = groupStruc.GAL_PA
    IRAC_PA = PA
  ENDIF
  
  ;********************NOW BEGIN FITTING MODELS********************
  EXTAST, groupStruc.displayHeader, astr                    ;Extract the 2MASS astrometry
  nx    = astr.naxis[0]                                     ;Grab the image x size
  ny    = astr.naxis[1]                                     ;Grab the image y size
  pl_sc = SQRT(ABS(astr.cd[0,0])*ABS(astr.cd[1,1]))         ;Compute the image plate scale
  
  ;  SKY, groupStruc.displayImage, skyMode, skyNoise, /SILENT  ;Compute the sky base level and noise
  skymode  = 0E                                             ;2MASS tiles have been set to 0 background
  skynoise = SXPAR(groupStruc.displayHeader, 'SIGMA')       ;2MASS sky noise was more reliably computed
  IF skynoise EQ 0 THEN BEGIN
    skynoiseStr = STRMID(groupStruc.NIRband, 0, 1) + 'SKYSIG'
    skynoise = SXPAR(groupStruc.displayHeader, skynoiseStr)
  ENDIF
  imageRange =skyMode + [-3, 100]*skyNoise                  ;Establish an image display range
  TVIM, groupStruc.displayImage, RANGE=imageRange           ;Plot the image
  
  ;BUILD A 2MASS PSF IMAGE (NOTICE HOW THE SUGGESTED FORM (BELOW) IS IDENTICAL TO A CIRCULAR SERSIC PROFILE
  ;f(dr) = f0 * exp {-dr/alpha)^(1/beta)]
  
  starInfo = groupStruc.starInfo
  AD2XY, starInfo.RAJ2000, starInfo.DEJ2000, astr, xStars, yStars

  IF groupStruc.NIRband EQ 'H' THEN magIndex = 5 ELSE $
    IF groupStruc.NIRband EQ 'Ks' THEN magIndex = 7

  magStarInds = WHERE((xStars GT 40) AND (xStars LT (astr.naxis[0] - 41)) $
                  AND (yStars GT 40) AND (yStars LT (astr.naxis[1] - 41)), numStars)
  minMag      = MIN(starInfo[magStarInds].(magIndex))
  maxMag      = minMag + 2
  starInds    = WHERE((starInfo.(magIndex) LT maxMag) $
                  AND (xStars GT 40) AND (xStars LT (astr.naxis[0] - 41)) $
                  AND (yStars GT 40) AND (yStars LT (astr.naxis[1] - 41)), numStars)

  
  IF numStars EQ 0 THEN STOP
  AD2XY, starInfo[starInds].RAJ2000, starInfo[starInds].DEJ2000, astr, xStars, yStars
  OPLOT, [xStars], [yStars], PSYM = 6, COLOR = greenInd
  XYOUTS, 0.5, 0.95, 'Click which marked stars to use for building the PSF', /NORMAL, ALIGNMENT=0.5
  XYOUTS, 0.5, 0.045, 'Left click to select stars', /NORMAL, ALIGNMENT=0.5
  XYOUTS, 0.5, 0.0125, 'Right click to continue', /NORMAL, ALIGNMENT=0.5
  selectedStars = LONARR(numStars)
  done          = 0
  WHILE ~done DO BEGIN
    CURSOR, xClick, yClick, /DATA, /DOWN                    ;Estimate a star position
    
    IF !MOUSE.button EQ 4 $                                 ;Query user if they're done
      THEN done = 1 ELSE BEGIN
      distance = SQRT((xStars - xClick)^2 + (yStars - yClick)^2);Compute distance between stars and click
      thisStar = WHERE(distance EQ MIN(distance))
      selectedStars[thisStar] = ~selectedStars[thisStar]    ;Mark the star as selected
      IF selectedStars[thisStar] EQ 1 $
        THEN OPLOT, xStars[thisStar], yStars[thisStar], PSYM=4, COLOR=redInd $
        ELSE OPLOT, xStars[thisStar], yStars[thisStar], PSYM=4, COLOR=0L

      ;************
      ;ADD CODE TO REPLOT WITH MARKERS ONLY ON SELECTED STARS
      ;************
    ENDELSE
    IF TOTAL(selectedStars) EQ 0 THEN done = 0              ;Catch an accidental exit with no stars selected
  ENDWHILE
  
  selectedStars = WHERE(selectedStars, numSelected)
  xStars        = xStars[selectedStars]
  yStars        = yStars[selectedStars]
  sz            = SIZE(groupStruc.displayImage, /DIMENSIONS)
  
  MAKE_2D, FINDGEN(21), FINDGEN(21), xx, yy
  xx        -= 10
  yy        -= 10
  stackedPSF = FLTARR(21, 21)
  
  parinfo = replicate({value:0.D, fixed:0,limited:[0,0],limits:[0.D,0.D],mpminstep:0.D},4)
  parinfo[*].limited(*) = 1                                 ;Turn on limits
  
  FOR i = 0, numSelected - 1 DO BEGIN
    xOff     = (xStars[i] - 9) > 0
    xRt      = (xOff  + 20) < (sz[0] - 1)
    yOff     = (yStars[i] - 9) > 0
    yTop     = (yOff + 20)  < (sz[1] - 1)
    subArray = groupStruc.displayImage[xOff:xRt, yOff:yTop]
    GCNTRD, subArray, (xStars[i]-xOff), (yStars[i]-yOff), xcen, ycen, 4.0
    shiftedPSF = SHIFT(subArray, (10-xcen), (10-ycen))
    
    ;Sersic profile parameters
    ;
    ; p(0) is the central surface brightness
    ; p(1) is the PSF scale length
    ; p(2) is the (1/n) factor in the Sersic profile
    ; p(3) is the background
    ;
    
    parinfo[*].limits[0] = [0,                 0.1D, 1D-2, -3*skynoise]   ;lower limits
    parinfo[*].limits[1] = [2*MAX(shiftedPSF), 10D,  1D1,  +3*skynoise]   ;upper limits
    
    ;Set initial guesses for image parameters
    parinfo(*).value = [MAX(shiftedPSF), 3.0, 1.0, 0.0]
    ;    params = parinfo.value
    
    ;    errors = SQRT(skynoise^2 + shiftedPSF^2)
    weights = FLTARR(21,21) + 1
    params  = MPFIT2DFUN('TWO_MASS_SERSIC_PSF', xx, yy, shiftedPSF,$
      weights=weights, parinfo=parinfo, perror=perror, bestnorm=bestnorm, /QUIET)
      
    bgLevel = 0.25*(MEAN(shiftedPSF[0:1,0:1]) + $
      MEAN(shiftedPSF[0:1,19:20]) + $
      MEAN(shiftedPSF[19:20,19:20]) + $
      MEAN(shiftedPSF[19:20,0:1]))
    IF i EQ 0 THEN BEGIN
      params1 = params
      stackedPSF += (shiftedPSF - bgLevel)
    ENDIF ELSE BEGIN
      scaleFactor = params1[0] / params[0]
      stackedPSF += scaleFactor*(shiftedPSF - bgLevel)
    ENDELSE
  ENDFOR
  bgLevel     = 0.25*(MEAN(stackedPSF[0:1,0:1]) + $
    MEAN(stackedPSF[0:1,19:20]) + $
    MEAN(stackedPSF[19:20,19:20]) + $
    MEAN(stackedPSF[19:20,0:1]))
  stackedPSF -= bgLevel
  stackedPSF /= numSelected
  
  parinfo[3].fixed     = 1
  parinfo[*].limits[0] = [0,                 0.1D, 1D-2, -3*skynoise]   ;lower limits
  parinfo[*].limits[1] = [2*MAX(stackedPSF), 10D,  1D1,  +3*skynoise]   ;upper limits
  
  ;Set initial guesses for image parameters
  parinfo(*).value = [MAX(stackedPSF), 3D, 1D, 0D]
  weights   = FLTARR(21,21) + 1
  PSFparams = MPFIT2DFUN('TWO_MASS_SERSIC_PSF', xx, yy, stackedPSF,$
    weights=weights, parinfo=parinfo, perror=perror, bestnorm=bestnorm, /QUIET)
    
  PSFparamString = STRING(FORMAT='("With h = ",F4.2,", n = ",F4.2)', PSFparams[1:2])
  PRINT_TEXT2, event, 'Stellar PSF generated using the Sersic functional form'
  PRINT_TEXT2, event, 'f(r) = f0 * EXP(-(r/h)^(1/n))'
  PRINT_TEXT2, event, PSFparamString
  
  POLYFILL, [0,0,1,1,0],[0.92,1.0,1.0,0.92,0.92], /NORMAL, COLOR=!P.background
  XYOUTS, 0.5, 0.95, 'Click on the galaxy center', /NORMAL, ALIGNMENT=0.5
  CURSOR, Xcen, Ycen, /DATA, /DOWN                          ;Estimage the galaxy center
  centerImg = groupStruc.displayImage[(Xcen-30):(Xcen+30), (Ycen-30):(Ycen+30)]
    
  done = 0
  WHILE ~done DO BEGIN
    TVIM, centerImg, RANGE=5*imageRange                     ;Display the central region
    CURSOR, Xcen1, Ycen1, /DATA, /DOWN                      ;Better estimate the galaxy center
    GCNTRD, centerImg, Xcen1, Ycen1, Xcen1, Ycen1, 3.0      ;Centroid the flux in that region
;    IF (Xcen NE -1) AND (Ycen NE -1) THEN done = 1         ;Double check that the centroid was successful
    
    OPLOT, [Xcen1, Xcen1], [0, 60], $                       ;Mark the estimated galaxy center
      LINESTYLE = 2, THICK = 2
    OPLOT, [0, 60], [Ycen1, Ycen1], $
      LINESTYLE = 2, THICK = 2
    
    XYOUTS, 0.5, 0.045, 'Right click to approve center fit', /NORMAL, ALIGNMENT=0.5
    XYOUTS, 0.5, 0.0125, 'Left click to try again', /NORMAL, ALIGNMENT=0.5
    
    CURSOR, junk1, junk2, /DOWN
    IF !MOUSE.button EQ 4 $                                 ;Query user if they're done
      THEN done = 1                                         ;Either exit or try again
  ENDWHILE
  Xgal = Xcen + (Xcen1-30)                                  ;Shift the centroided value back to image coordinates
  Ygal = Ycen + (Ycen1-30)
  XY2AD, Xgal, Ygal, astr, RA_cen, Dec_cen                  ;Convert the galaxy center to RA and Dec coordinates
  GETROT, groupStruc.displayHeader, rotang, cdelt           ;Grab the equatorial rotation of the image
  pl_sc       = SQRT(ABS(cdelt[0]*cdelt[1]))*3600E          ;Compute the plate scale
  
  ;Compute an 8 arcmin square region centered on the galaxy
  x1 = ROUND(Xgal) - ROUND(300E/pl_sc) & x2 = ROUND(Xgal) + ROUND(300E/pl_sc)    ;Begin with an 8 arcmin region
  y1 = ROUND(Ygal) - ROUND(300E/pl_sc) & y2 = ROUND(Ygal) + ROUND(300E/pl_sc)
  
  ;Test if the boundaries of the cropped image are outside the starting image
  IF (x1 LT 0) OR (y1 LT 0) OR $
    (x2 GT (nx-1)) OR (y2 GT (ny-1)) THEN BEGIN
    deltaPix = MIN(ROUND([Xgal, Ygal, (nx - Xgal - 1), (ny - Ygal - 1)]))
    
    x1 = ROUND(Xgal) - deltaPix & x2 = ROUND(Xgal) + deltaPix
    y1 = ROUND(Ygal) - deltaPix & y2 = ROUND(Ygal) + deltaPix
    xGalCen = Xgal - x1
    yGalCen = Ygal - y1
  ENDIF ELSE BEGIN
    xGalcen = Xgal - x1
    yGalCen = Ygal - y1
  ENDELSE
  
  ;Making sure that dimensions are odd.
  if (x2-x1) mod 2 eq 1 then begin
    IF x2 EQ (nx-1) THEN x2-- ELSE x2++
  endif
  if (y2-y1) mod 2 eq 1 then begin
    IF y2 EQ (ny-1) THEN y2-- ELSE y2++
  endif
  
  ;  xGalCen = CEIL(0.5*(x2-x1)) & yGalCen = CEIL(0.5*(y2-y1))         ;Recompute the galaxy center for the cropped image
  ;
  HEXTRACT,groupStruc.displayImage,groupStruc.displayHeader, image, head, x1,x2,y1,y2
  
  EXTAST, head, cropAstr
  starInfo = groupStruc.starInfo
  AD2XY, starInfo.RAJ2000, starInfo.DEJ2000, cropAstr, xStars, yStars
  
  deleteStars = WHERE((xStars GT 15) AND (xStars LT (cropAstr.naxis[0]-16)) $
    AND (yStars GT 15) AND (yStars LT (cropAstr.naxis[1]-16)), numDelete)
  xStars      = xStars[deleteStars]
  yStars      = yStars[deleteStars]
  
  MAKE_2D, FINDGEN(31), FINDGEN(31), xx, yy                          ;Generate 2D (x,y) maps
  xx        -= 15                                                    ;Shift maps so that origin is at center pixel
  yy        -= 15
  IF numDelete GT 0 THEN BEGIN                                       ;Loop through stars near galaxy and 'delete' them from image
    FOR i = 0, numDelete - 1 DO BEGIN
    
      xOff     = (xStars[i] - 14) > 0
      xRt      = (xOff  + 30) < (cropAstr.naxis[0] - 1)
      yOff     = (yStars[i] - 14) > 0
      yTop     = (yOff + 30)  < (cropAstr.naxis[1] - 1)
      subArray = image[xOff:xRt, yOff:yTop]
      GCNTRD, subArray, (xStars[i]-xOff), (yStars[i]-yOff), xcen, ycen, 4.0
      shiftedStarImg = SHIFT(subArray, (15-xcen), (15-ycen))
      
      bgLevel     = 0.25*(MEAN(shiftedStarImg[0:2,0:2]) + $
        MEAN(shiftedStarImg[0:2,28:30]) + $
        MEAN(shiftedStarImg[28:30,28:30]) + $
        MEAN(shiftedStarImg[28:30,0:2]))
        
      parinfo[1:3].fixed   = REPLICATE(1,3)
      parinfo[*].limits[0] = [0,                     0.1D, 1D-2, bgLevel-3*skynoise]   ;lower limits
      parinfo[*].limits[1] = [2*MAX(shiftedStarImg), 10D,  1D1,  bgLevel+3*skynoise]   ;upper limits
      
      ;Set initial guesses for image parameters
      parinfo[*].value = [MAX(shiftedStarImg), PSFparams[1:2], bgLevel]
      weights          = FLTARR(31,31) + 1
      thisStarParams   = MPFIT2DFUN('TWO_MASS_SERSIC_PSF', xx, yy, shiftedStarImg,$
        weights=weights, parinfo=parinfo, perror=perror, bestnorm=bestnorm, /QUIET)
        
      thisStarParams = [thisStarParams[0:2], 0D]
      thisStarImage  = TWO_MASS_SERSIC_PSF(xx, yy, thisStarParams)
      image[xOff:xRt, yOff:yTop] -= SHIFT(thisStarImage, (xcen-15), (ycen-15))
    ENDFOR
  ENDIF
  
  
  
  ;Now get the ellipse properties
  logRange     = ALOG(skyMode + [1, 1E3]*skyNoise)          ;Establish a log-scaled image display range
  ellipseAxes  = FLTARR(2)                                  ;Create an array for storing ellipse axes length
  ellipseCount = 0                                          ;Index for counting the number of ellipses in the mask
  done         = 0                                          ;Test for if the user is done marking ellipses
  WHILE ~done DO BEGIN
    IF ellipseCount GT 0 $                                  ;Add extra ellipse axes values if more than one ellipse
      THEN ellipseAxes = [[ellipseAxes], [FLTARR(2)]]
    TVIM, ALOG(image), RANGE=logRange                       ;Show the image
    OPLOT, [xGalCen], [yGalCen], PSYM=4, COLOR=redInd       ;Overplot the galaxy center
    levels = [3,6,9]*skyNoise                               ;Contour bightness levels
    labels = [" 3 sig", " 6 sig", " 9 sig" ]                ;Contour labels
    CONTOUR, GAUSS_SMOOTH(image,6), $                       ;Overplot the smoothed contours
      LEVELS = levels, C_ANNOTATION = labels, /OVERPLOT
      
    axisStrs = ['Click on the disk major axis extremes','Click on the disk minor axis extremes', $
      'Click on the bulge major axis extremes', 'Click on the bulge minor axis extremes']
    FOR i = 0, 1 DO BEGIN                                   ;Loop through the major and minor axes
      POLYFILL, [0,0,1,1,0],[0.92,1.0,1.0,0.92,0.92], /NORMAL, COLOR=!P.background
      strIndex = ellipseCount*2 + i
      XYOUTS, 0.5, 0.95, axisStrs[strIndex], /NORMAL, ALIGNMENT=0.5
      CURSOR, Xend1, Yend1, /DATA, /DOWN                    ;Estimate one end of the axis
      OPLOT, [Xend1], [Yend1], PSYM=4, COLOR=redInd
      CURSOR, Xend2, Yend2, /DATA, /DOWN                    ;Estimate the other end of the axis
      OPLOT, [Xend2], [Yend2], PSYM=4, COLOR=redInd
      IF (i EQ 0) THEN BEGIN                                ;Computre major axis slope (PA) and length
        majorIntSlope = LINFIT([Xend1, xGalCen, Xend2], [Yend1, yGalCen, Yend2])
        PA1           = (ATAN(majorIntSlope[1])*!RADEG + 2*360 - 90) MOD 180
        xyNear1       = NEAREST_POINT_ON_LINE(majorIntSlope, Xend1, Yend1)
        xyNear2       = NEAREST_POINT_ON_LINE(majorIntSlope, Xend2, Yend2)
        
        OPLOT, [xyNear1[0], xyNear2[0]], [xyNear1[1], xyNear2[1]], COLOR=greenInd
        ellipseAxes[i, ellipseCount] = 0.5*SQRT((xyNear1[0] - xyNear2[0])^2 + $
          (xyNear1[1] - xyNear2[1])^2)*(pl_sc/0.579)
        IF ellipseCount EQ 0 THEN BEGIN
          PA = PA1                                          ;Store AT LEAST the first ellipse PA
        ENDIF ELSE BEGIN
          IF ABS(PA - PA1) GT 45 $                          ;If the new PA is significantly different
            THEN PA = [PA, ((PA+90) MOD 180)]               ;Then force it to be perpendicular
        ENDELSE
      ENDIF ELSE IF (i EQ 1) THEN BEGIN                     ;Compute minor axis length
        minorIntSlope = [(yGalCen + (1.0/majorIntSlope[1])*xGalCen), (-1.0/majorIntSlope[1])]
        xyNear1       = NEAREST_POINT_ON_LINE(minorIntSlope, Xend1, Yend1)
        xyNear2       = NEAREST_POINT_ON_LINE(minorIntSlope, Xend2, Yend2)
        
        OPLOT, [xyNear1[0], xyNear2[0]], [xyNear1[1], xyNear2[1]], COLOR=greenInd
        ellipseAxes[i, ellipseCount] = 0.5*SQRT((xyNear1[0] - xyNear2[0])^2 + $
          (xyNear1[1] - xyNear2[1])^2)*(pl_sc/0.579)
          
        XYOUTS, 0.5, 0.045, 'Left click for another ellipse', /NORMAL, ALIGNMENT=0.5
        XYOUTS, 0.5, 0.0125, 'Right click to continue with disk fitting', /NORMAL, ALIGNMENT=0.5
        CURSOR, junk1, junk2, /DOWN                         ;After the minor axis, ask the user whay they want to do
        IF !MOUSE.button EQ 4 $                             ;Query user if they're done
          THEN done = 1 ELSE ellipseCount++                 ;Either exit or increment the number of ellipses
      ENDIF
    ENDFOR
  ENDWHILE
  
  PRINT_TEXT2, event, 'Fitting galaxy models'
  IF useIRAC_PA THEN BEGIN                                  ;Force the PA determined by the IRAC 3.6 micron image
    parameterStructure = FIT_GALAXY_MODELS(event, image, xGalCen, yGalCen, ellipseAxes, IRAC_PA, skynoise, PSFparams, /FIX_PA)
  ENDIF ELSE BEGIN                                          ;Determine the PA by the best fitting model to H-band image
    parameterStructure = FIT_GALAXY_MODELS(event, image, xGalCen, yGalCen, ellipseAxes, PA1, skynoise, PSFparams)
  ENDELSE
  
  PRINT_TEXT2, event, NEW_LINE() + 'The best fitting model is ' + parameterStructure.bestFit
  galPA = parameterStructure.parameters[2]*!RADEG - 90D
  UPDATE_GROUP_SUMMARY, event, groupStruc, 'GAL_PA', galPA, /SAVE
  
  ;*****************************************************************************************
  ;   Effectively everything after this can be skipped now that the mask info is simply written to a file
  ;   and masks are generated 'on the fly'
  
  ;Compute Mimir and 2MASS image pixel locations
  nxMimir = 1024                                            ;Width of a standard Mimir image
  nyMimir = 1026                                            ;Height of a standard Mimir image
  center  = [512, 513]                                      ;This is where the galaxy mask was centered
  pl_sc_convert = pl_sc/0.579                               ;Conversion factor between plate scales
  MAKE_2D, FINDGEN(nxMimir), FINDGEN(nyMimir), xxMimir, yyMimir
  MAKE_2D, FINDGEN(cropAstr.naxis[0]), FINDGEN(cropAstr.naxis[1]), xx, yy

  ;Compute the pixel locations for the 2MASS-LGA image
  sz2MASS = SIZE(groupStruc.displayImage, /DIMENSIONS)
  MAKE_2D, FINDGEN(sz2MASS[0]), FINDGEN(sz2MASS[1]), xx2MASS, yy2MASS
  
  ;Generate a model image and rescale the model parameters to match the Mimir plate scale
  CASE parameterStructure.bestFit OF
    'SERSIC_DISK': BEGIN
      modelImage  = SERSIC_DISK(xx, yy, parameterStructure.parameters)
      parameters2MASS = parameterStructure.parameters
      parameters2MASS[0:1] += [x1,y1]
      model2MASS  = SERSIC_DISK(xx2MASS, yy2MASS, parameters2MASS)
      MimirParams              = parameterStructure.parameters
      MimirParams[0:1]         = center
      lengthInds               = [5,9]
      MimirParams[lengthInds] *= pl_sc_convert
      MimirModel  = SERSIC_DISK(xxMimir, yyMimir, MimirParams)
      nearGalParams              = MimirParams
      nearGalParams[lengthInds] *= 1.8
      nearGal     = SERSIC_DISK(xxMimir, yyMimir, nearGalParams)
    END
    'EXP_RADIAL_SECH2_VERTICAL_DISK': BEGIN
      modelImage  = EXP_RADIAL_SECH2_VERTICAL_DISK(xx, yy, parameterStructure.parameters)
      parameters2MASS = parameterStructure.parameters
      parameters2MASS[0:1] += [x1,y1]
      model2MASS  = EXP_RADIAL_SECH2_VERTICAL_DISK(xx2MASS, yy2MASS, parameters2MASS)
      MimirParams              = parameterStructure.parameters
      MimirParams[0:1]         = center
      lengthInds               = [4,5,8]
      MimirParams[lengthInds] *= pl_sc_convert
      MimirModel  = EXP_RADIAL_SECH2_VERTICAL_DISK(xxMimir, yyMimir, MimirParams)
      nearGalParams              = MimirParams
      nearGalParams[lengthInds] *= 1.8
      nearGal     = EXP_RADIAL_SECH2_VERTICAL_DISK(xxMimir, yyMimir, nearGalParams)
    END
    'BULGE_AND_ONE_SERSIC_DISK': BEGIN
      modelImage = BULGE_AND_ONE_SERSIC_DISK(xx, yy, parameterStructure.parameters)
      parameters2MASS = parameterStructure.parameters
      parameters2MASS[0:1] += [x1,y1]
      model2MASS  = BULGE_AND_ONE_SERSIC_DISK(xx2MASS, yy2MASS, parameters2MASS)
      MimirParams              = parameterStructure.parameters
      MimirParams[0:1]         = center
      lengthInds               = [5,8,12]
      MimirParams[lengthInds] *= pl_sc_convert
      MimirModel  = BULGE_AND_ONE_SERSIC_DISK(xxMimir, yyMimir, MimirParams)
      nearGalParams              = MimirParams
      nearGalParams[lengthInds] *= 1.8
      nearGal     = BULGE_AND_ONE_SERSIC_DISK(xxMimir, yyMimir, nearGalParams)
    END
    'BULGE_AND_EXP_RADIAL_SECH2_VERTICAL_DISK': BEGIN
      modelImage = BULGE_AND_EXP_RADIAL_SECH2_VERTICAL_DISK(xx, yy, parameterStructure.parameters)
      parameters2MASS = parameterStructure.parameters
      parameters2MASS[0:1] += [x1,y1]
      model2MASS  = BULGE_AND_EXP_RADIAL_SECH2_VERTICAL_DISK(xx2MASS, yy2MASS, parameters2MASS)
      MimirParams              = parameterStructure.parameters
      MimirParams[0:1]         = center
      lengthInds               = [5,7,8]
      MimirParams[lengthInds] *= pl_sc_convert
      MimirModel  = BULGE_AND_EXP_RADIAL_SECH2_VERTICAL_DISK(xxMimir, yyMimir, MimirParams)
      nearGalParams              = MimirParams
      nearGalParams[lengthInds] *= 1.8
      nearGal     = BULGE_AND_EXP_RADIAL_SECH2_VERTICAL_DISK(xxMimir, yyMimir, nearGalParams)
    END
  ENDCASE

  CONTOUR, modelImage, /OVERPLOT, LEVELS = skynoise*EXP((FINDGEN(5) + 1)), COLOR=redInd
  xLine = [0, cropAstr.naxis[0]]
  yLine = TAN(parameterStructure.parameters[2])*(xLine - parameterStructure.parameters[0]) + parameterStructure.parameters[1]
  OPLOT, xLine, yLine, COLOR = RGB_TO_DECOMPOSED([255,255,0]), THICK = 2
  
  POLYFILL, [0,0,1,1,0],[0.92,1.0,1.0,0.92,0.92], /NORMAL, COLOR=!P.background
  POLYFILL, [0,0,1,1,0], [0,0.075,0.075,0,0], /NORMAL, COLOR=!P.background
  XYOUTS, 0.5, 0.035, 'Click to continue...', /NORMAL, ALIGNMENT=0.5
  CURSOR, junk1, junk2, /DOWN                               ;After the minor axis, ask the user whay they want to do
  
  ;Prepare to mask stars
  AD2XY, starInfo.RAJ2000, starInfo.DEJ2000, astr, xStars, yStars
  mask2MASS   = model2MASS GT 2.5*skynoise                  ;Make a mask for the 2MASS image
  galMask     = mimirModel GT 2.5*skyNoise                  ;Make a mask using the fitted model
  nearGal     = nearGal    GT 2.5*skyNoise                  ;Make a mask that is 1.2X bigger... (for locating nearby stars)
  xMimirStars = center[0] + (xStars - Xgal)*pl_sc_convert   ;Compute the x and y Mimir locations for this star.
  yMimirStars = center[1] + (yStars - Ygal)*pl_sc_convert
  starWid     = PSFparams[1]
  
  brightStar   = groupStruc.starInfo.(magIndex) LE 15E      ;Find the bright stars
  closeStars   = nearGal[xMimirStars, yMimirStars]          ;Find the stars in those near galaxy pixels
  maskStars    = WHERE(closeStars OR brightStar, numMasked) ;Find all the stars to mask
  magMaskStars = starInfo[maskStars].(magIndex)
  xMaskStars   = xMimirStars[maskStars]                     ;Pick out the x and y locations of the stars to be masked
  yMaskStars   = yMimirStars[maskStars]
  
  mask = galMask                                            ;Alias the original galaxy mask
  FOR i = 0, numMasked - 1 DO BEGIN
    IF (xMaskStars[i] GT 10) AND (xMaskStars[i] LT nxMimir - 10) AND $
      (yMaskStars[i] GT 10) AND (yMaskStars[i] LT nyMimir - 10) THEN BEGIN
      ;Compute a radius to mask bassed on the brightness of the star
      maskRad  = 10E^(0.80E*MIN(magMaskStars)/magMaskStars[i])*starWid*pl_sc_convert
      starMask = SQRT((xxMimir - xMaskStars[i])^2 + $       ;Find pixels near this star
        (yyMimir - yMaskStars[i])^2) LT maskRad
      mask = mask OR starMask                               ;Add those pixels to the mask
    ENDIF
  ENDFOR
  
  rebinImage  = CONGRID(image, nxMimir, nyMimir)                      ;Rebin the 2MASS image to match the MimirMask
  TVIM, mask                                                ;Display the mask
  CONTOUR, GAUSS_SMOOTH(rebinImage,6), $                    ;Overplot the smoothed contours
    LEVELS = levels, C_ANNOTATION = labels, /OVERPLOT, COLOR=redInd
  XYOUTS, 0.5, 0.045, 'This is your mask!', /NORMAL, ALIGNMENT=0.5
  XYOUTS, 0.5, 0.0125, 'Click to save file', /NORMAL, ALIGNMENT=0.5
  CURSOR, junk1, junk2, /DOWN
  
  XY2AD, parameterStructure.parameters[0], parameterStructure.parameters[1], cropAstr, $
    RA_mask, Dec_mask
  maskHeader = ['Model Fitting Information']
  SXADDPAR, maskHeader, 'RA_MASK',  RA_mask,  'Center of this galaxy mask'
  SXADDPAR, maskHeader, 'DEC_MASK', Dec_mask, 'Center of this galaxy mask'
  SXADDPAR, maskHeader, 'RA_ROT', RA_cen, 'Center for image rotation'
  SXADDPAR, maskHeader, 'DEC_ROT', Dec_cen, 'Center for image rotation'
  SXADDPAR, maskHeader, 'IRAC_PA', useIRAC_PA, 'Was the IRAC image used to fix galaxy PA?'
  SXADDPAR, maskHeader, 'LEN_UNIT', pl_sc, 'Model length units (arcsec/pix)'
  SXADDPAR, maskHeader, 'FITMODEL', parameterStructure.bestFit, 'The best fit galaxy model'
  
  ;Add the model parameters to the mask header
  FOR i = 0, N_ELEMENTS(parameterStructure.parameters) - 1 DO BEGIN
    SXADDPAR, maskHeader, STRING((i+1), FORMAT='("P",I02)'), parameterStructure.parameters[i]
  ENDFOR
  
  S2_path  = groupStruc.analysis_dir + 'S2_Ski_Jump_Fixes'
  IF ~FILE_TEST(S2_path) THEN FILE_MKDIR, S2_path                     ;Make S3B directory if necessary
  maskPath  = S2_path + PATH_SEP() + 'Masking_files'
  IF ~FILE_TEST(maskPath, /DIRECTORY) THEN FILE_MKDIR, maskPath       ;Make masking subdirectory if necessary
  
  ;Write the mask files to disk
  WRITEFITS, maskPath + PATH_SEP() + 'galMask.fits', galMask
  WRITEFITS, maskPath + PATH_SEP() + 'starMask.fits', mask
  WRITEFITS, maskPath + PATH_SEP() + 'mask2MASS.fits', mask2MASS
  WRITEHEAD, maskPath + PATH_SEP() + 'maskInfo.dat', maskHeader

;  ;Save all the mask information to a text file
;  OPENW, lun, maskPath + PATH_SEP() + 'maskInfo.dat', /GET_LUN        ;Open a file to write masking info
;  FOR j = 0, N_ELEMENTS(maskheader) - 1 DO BEGIN                      ;Loop through the header
;    PRINTF, lun, maskHeader[j]                                        ;Print each line to file
;  ENDFOR
;  CLOSE, lun                                                          ;Close the logical unit number
;  FREE_LUN, lun                                                       ;Free the logical unit number for future use
END




PRO S2_PEGS_SKI_JUMP_REPAIR, event
  
  displayWID          = WIDGET_INFO(event.top, FIND_BY_UNAME='IMAGE_DISPLAY_WINDOW')
  groupProgressBarWID = WIDGET_INFO(event.top, FIND_BY_UNAME='GROUP_PROGRESS_BAR')
  imageProgressBarWID = WIDGET_INFO(event.top, FIND_BY_UNAME='IMAGE_PROGRESS_BAR')
  WIDGET_CONTROL, event.top, GET_UVALUE=groupStruc          ;Retrieve group structure
  WIDGET_CONTROL, displayWID, GET_VALUE=windowIndex         ;Retrieve display window WID
  WSET, windowIndex                                         ;Set plot window WID

  UPDATE_PROGRESSBAR, groupProgressBarWID, /ERASE           ;Clean up the progress bars
  UPDATE_PROGRESSBAR, imageProgressBarWID, /ERASE

  S2dir        = groupStruc.analysis_dir + 'S2_Ski_Jump_Fixes' + PATH_SEP()
  galMaskPath  = S2dir + 'Masking_files' + PATH_SEP() + 'galMask.fits'
  galMask      = READFITS(galMaskPath, maskHead)
  starMaskPath = S2dir + 'Masking_files' + PATH_SEP() + 'starMask.fits'
  starMask     = READFITS(starMaskPath)
  mask         = galMask or starMask
  skynoise     = SXPAR(groupStruc.displayHeader, 'SIGMA')
  
  CASE groupStruc.NIRband OF
    'H':  testCriterion = 6.0
    'Ks': testCriterion = 12.0
  ENDCASE

  FOR i = 0, groupStruc.numGroups - 1 DO BEGIN              ;Loop through each of the groups
    goodFiles = WHERE(groupStruc.groupImages[i,*] NE '', numGood)
    ;
    progressString = 'group ' + STRTRIM(i+1,2) + $          ;Create the message to display in the progress bar
      ' of ' + STRTRIM(groupStruc.numGroups,2)
    UPDATE_PROGRESSBAR, groupProgressBarWID, $              ;Update the progress bar to show the latest progress
      100*FLOAT(i+1)/groupStruc.numGroups, DISPLAY_MESSAGE=progressString
    WAIT, 0.05
    FOR j = 0, numGood - 1 DO BEGIN
      image_path = groupStruc.groupImages[i,goodFiles[j]]   ;Grab the 'image_path' for this particular image
      S2_PEGS_SKI_JUMP_DETECTOR, event, image_path, $       ;Use the PPOL ski jump detector with a 6.0-sigma detection threshold
        ski_jump_code, CRITERION=testCriterion
      ;
      ; ski_jump_code -
      ;   00 = top, bottom OK
      ;   10, 01 = top or bottom bad
      ;   11 = both bad
      ;
      ; if test showed no correction needed, set let to green and jump past fix
      ;
;      if(ski_jump_code eq 0) then ski_jump_flag = 0 else ski_jump_flag = 1
      ;
      ski_jump_str = STRING(ski_jump_code, FORMAT = '(I02)')
      IF ski_jump_str NE STRING(0, FORMAT = '(I02)') THEN BEGIN
        fileTest = FILE_TEST(groupStruc.analysis_dir + $    ;Test if this file has already been corrected
          PATH_SEP() + 'S2_Ski_Jump_Fixes' + PATH_SEP() + FILE_BASENAME(image_path))
        IF fileTest THEN CONTINUE                           ;Skip files than have already been corrected
        img      = READFITS(image_path, BDPhead)            ;Read in the ski-jump image
        filename = STRSPLIT(image_path, PATH_SEP(), /EXTRACT) ;Construct the new file name for the flattened image
        filename = (REVERSE(filename))[0]                   ;Grab the actual file name
        
        SKY, img, skyMode, imgNoise, /SILENT
        TVIM, img, RANGE = skyMode + [-3,+20]*imgNoise, $   ;Display the ski-jump image
          TITLE=filename
        XYOUTS, 0.5, 0.045, 'Click on the galaxy center', /NORMAL, ALIGNMENT=0.5
        CURSOR, Xcen, Ycen, /DATA, /DOWN                    ;Estimage the galaxy center
        centerImg = img[(Xcen-30):(Xcen+30), (Ycen-30):(Ycen+30)]

        clickCounter = 0
        done         = 0
        WHILE done EQ 0 DO BEGIN
          SKY, centerImg, skyMode, imgNoise, /SILENT
          TVIM, centerImg, RANGE = skyMode + [-3,+20]*imgNoise;Display the central region
          XYOUTS, 0.5, 0.95, 'Click on the galaxy center', /NORMAL, ALIGNMENT=0.5
          CURSOR, Xcen1, Ycen1, /DATA, /DOWN                ;Better estimate the galaxy center
          GCNTRD, centerImg, Xcen1, Ycen1, Xcen1, Ycen1, 3.0;Centroid the flux in that region
;          IF (Xcen NE -1) AND (Ycen NE -1) THEN done = 1   ;Double check that the centroid was successful
          OPLOT, [Xcen1, Xcen1], [0, 60], $                 ;Mark the estimated galaxy center
            LINESTYLE = 2, THICK = 2
          OPLOT, [0, 60], [Ycen1, Ycen1], $
            LINESTYLE = 2, THICK = 2
          
          XYOUTS, 0.5, 0.045, 'Right click to approve center location', /NORMAL, ALIGNMENT=0.5
          XYOUTS, 0.5, 0.0125, 'Left click to try again', /NORMAL, ALIGNMENT=0.5
          
          CURSOR, junk1, junk2, /DOWN
          IF !MOUSE.button EQ 4 $                           ;Query user if they're done
            THEN done = 1 $                                ;Either exit or try again
            ELSE clickCounter++
          IF clickCounter GE 5 THEN done = 2
        ENDWHILE
        
        IF done EQ 2 THEN BEGIN
          TVIM, centerImg, RANGE = skyMode + [-3,+50]*skyNoise;Display the central region
          XYOUTS, 0.5, 0.95, 'FINAL ATTEMPT... JUST CLICK ON CENTER', /NORMAL, ALIGNMENT=0.5
          CURSOR, Xcen1, Ycen1, /DATA, /DOWN              ;Better estimate the galaxy center
          OPLOT, [Xcen1, Xcen1], [0, 60], $               ;Mark the estimated galaxy center
            LINESTYLE = 2, THICK = 2
          OPLOT, [0, 60], [Ycen1, Ycen1], $
            LINESTYLE = 2, THICK = 2
            
          XYOUTS, 0.5, 0.0125, 'Click to proceed', /NORMAL, ALIGNMENT=0.5
          
          CURSOR, junk1, junk2, /DOWN
        ENDIF
        
        Xgal = Xcen + (Xcen1-30)                            ;Shift the centroided value back to image coordinates
        Ygal = Ycen + (Ycen1-30)
        
        deltaX = Xgal - 512                                 ;Compute the X and Y shifts for the mask
        deltaY = Ygal - 513
        shiftedMask = GENERATE_MASK(event, 1024, 1026, Xgal, Ygal, 0.579, 2.5*skynoise)
        ;
        ;Model the ski-jump and subtract model from the image
        ;
        model       = pegs_ski_jump_fit(img, ski_jump_code, MASK = shiftedMask, BAD_PIXEL_MAP = bad_pixel_map)
        resid_image = img - model
        ;
        ; determine mean sky from  UNMASKED PIXELS in the center of original image
        ;
        sz = SIZE(img, /DIMENSIONS)
        image_center = img[fix(5*sz[0]/16):fix(11*sz[0]/16),fix(5*sz[1]/16):fix(11*sz[1]/16)]
        mask_center  = (shiftedMask OR bad_pixel_map)[fix(5*sz[0]/16):fix(11*sz[0]/16),fix(5*sz[1]/16):fix(11*sz[1]/16)]
        goodInds     = where(mask_center EQ 0, numNotMasked)
        IF numnotMasked GT 0 THEN BEGIN
          vec          = REFORM(image_center[goodInds])
          image_mean   = (jm_median_filtered_mean(vec)).mean
          resid_image += image_mean
        ENDIF ELSE STOP
        ;
        ;Fill in the bad pixels with the bad_pix_value
        ;
        bad_pix_value = -1E6
        maskInds      = WHERE(bad_pixel_map, numMasked)
        IF numMasked GT 0 THEN BEGIN
          resid_image[maskInds] = bad_pix_value
        ENDIF
        ;
        FXADDPAR, BDPhead, "HISTORY", "Subtracted Model Ski Jump using PEGS_pol, Step 2"
        ;
        ;Display the repaired image to the user?
        ;
        filePath = S2dir + filename                         ;Tag on the S2 directory in front
        WRITEFITS, filePath, resid_image, BDPhead           ;Write the file to disk
        SKY, resid_image, skyMode, skyNoise
        TVIM, resid_image, RANGE = skyMode + [-3,+20]*skyNoise, $ ;Display the repaired image
          TITLE=filename
      ENDIF
      UPDATE_PROGRESSBAR, imageProgressBarWID, $            ;Update the progress bar to show the latest progress
        100*FLOAT(j+1)/numGood, /PERCENTAGE
        WAIT, 0.05
    ENDFOR
  ENDFOR
  
  PRINT_TEXT2, event, 'Done with the ski-jump removal procedure'
  
  
END