;This wrapper program will start by computing the APPROXIMATE offset between two images using their header astrometry,
;then it will cut out an array of PSFs from the constituent images and compute subpixel offsets using "CORREL_OPTIMIZE"
;from the Astrolib.

FUNCTION GET_IMAGE_OFFSETS, imgArray, headArray, sources

  ;Test if image array is proper shape
  imgArrSz = SIZE(imgArray, /DIMENSIONS)
  IF N_ELEMENTS(imgArrSz) EQ 3 THEN BEGIN
    nx = imgArrSz[0] & ny = imgArrSz[1]
    numFiles = imgArrSz[2]
  ENDIF ELSE BEGIN
    MESSAGE, 'The imgArray value must be 3 dimensions.'
    RETURN, -99
  ENDELSE

  ;Test if the userl supplied a list of positions
  IF N_ELEMENTS(sources) EQ 0 THEN BEGIN
    MESSAGE, 'Must supply a list of source positions to use for aligning images'
    RETURN, -99
  ENDIF
  
  ;Compute median star position
  RA0  = MEDIAN(sources.RA)
  Dec0 = MEDIAN(sources.Dec)
  
  ;Loop through each image and compute which stars appear well within the image frame
  ;Simultaneously compute approximate image offsets
  numStars  = N_ELEMENTS(sources.RA)
  keepStars = BYTARR(numStars) + 1
  dx        = FLTARR(numFiles)
  dy        = FLTARR(numFiles)
  FOR iFile = 0, numFiles - 1 DO BEGIN
    ;Extract the header
    EXTAST, headArray[*, iFile], astr

    ;Compute positions of the stars
    AD2XY, sources.RA, sources.Dec, astr, xTest, yTest
    
    ;Test which stars are in this image
    inThisImg = (xTest GT 50) AND (xTest LT nx - 51) $
      AND (yTest GT 50) AND (yTest LT ny - 51)
    
    ;Concatenate new tests with previous tests
    keepStars = keepStars AND inThisImg
    
    ;*** Compute approximate offset
    AD2XY, RA0, Dec0, astr, x1, y1
    dx[iFile] = x1
    dy[iFile] = y1
  ENDFOR
  
  ;Cull the list to eliminate unsuitable stars
  keepInds = WHERE(keepStars, numStars)
  IF numStars GT 0 THEN BEGIN
    sources = { $
      RA:sources.RA[keepInds], $
      Dec:sources.Dec[keepInds]}
  ENDIF ELSE BEGIN
    MESSAGE, 'No suitable stars were found for alignment.'
    RETURN, -99
  ENDELSE

  ;Compute offset WRT up to the median pointing
  dx -= MEDIAN(dx, /EVEN)
  dy -= MEDIAN(dy, /EVEN)
  
;  ;Test switching dx, dy signs
;  dx *= -1
;  dy *= -1
  
  ;Find the image closest to the median pointing
  dr     = SQRT(dx^2 + dy^2)
  minInd = MIN(WHERE(dr EQ MIN(dr), numMin))
  IF numMin GT 0 THEN BEGIN
    ;Center about median pointing
    dx -= dx[minInd]
    dy -= dy[minInd]
  ENDIF
  
  ;Figure out which gridding pattern is possible
  numPerSide  = 2 + INDGEN(4)
  sqNumbers   = (numPerSide)^2
  possibleInd = MAX(WHERE(sqNumbers LE numStars, numPossible))
  IF numPossible GT 0 THEN BEGIN
    numStars   = sqNumbers[MAX(possibleInd)]
    numPerSide = numPerSide[possibleInd]
    sources    = {$
      RA:sources.RA[0:numStars-1],$
      Dec:sources.Dec[0:numStars-1]}
  ENDIF ELSE BEGIN
    MESSAGE, 'You must have at least 4 usable stars for alignment'
    RETURN, -99
  ENDELSE
  
  ;Start by computing the star positions of the FIRST image
  EXTAST, headArray[*, minInd], astr0
  AD2XY, sources.RA, sources.Dec, astr0, x0, y0
  
  ;Loop through each image array and create star_tile array
  starTileDXDY  = 28
  nx1 = numPerSide*starTileDXDY + 1
  ny1 = nx1 + 1
  nz1 = numFiles
  starTileArray = FLTARR(nx1, ny1, nz1)
  FOR iFile = 0, numFiles - 1 DO BEGIN
    ;Compute the star position for this image
    lf = ROUND(x0 + dx[iFile] - starTileDxDy/2)
    rt = lf + starTileDxDy - 1
    bt = ROUND(y0 + dy[iFile] - starTileDxDy/2)
    tp = bt + starTileDxDy - 1

    ;Loop through each star and extract its tile
    iStar = 0
    FOR xTile = 0, numPerSide - 1 DO BEGIN
      FOR yTile = 0, numPerSide - 1 DO BEGIN
        ;Grab the cut indices for this star
        lf_ct = lf[iStar]
        rt_ct = rt[iStar]
        bt_ct = bt[iStar]
        tp_ct = tp[iStar]

        ;Compute the paste indices for this star
        lf_pt = xTile*starTileDxDy
        rt_pt = lf_pt + starTileDxDy - 1
        bt_pt = yTile*starTileDxDy
        tp_pt = bt_pt + starTileDxDy - 1
        
        ;Cut and normalize this tile
        starTile  = imgArray[lf_ct:rt_ct, bt_ct:tp_ct, iFile]
        starTile -= MIN(starTile)
        starTile /= MAX(starTile)
        
        ;Check for possible NaNs from dividing by zero
        nanInds = WHERE(~FINITE(starTile), numNans)
        IF numNans GT 0 THEN starTile[nanInds] = 0.0
        
        ;Paste the tile in its place
        starTileArray[lf_pt:rt_pt, bt_pt:tp_pt, iFile] = starTile
        
        ;Increment the star counter
        iStar++
      ENDFOR
    ENDFOR
  ENDFOR

  ;Define kernels for the Sobel convolution
  Sy = [[ 1, 2, 1], $
        [ 0, 0, 0], $
        [-1,-2,-1]]

  Sx = [[-1, 0, 1], $
        [-2, 0, 2], $
        [-1, 0, 1]]
  
  ;Define a 3x3 (x,y) grid
  MAKE_2D, FINDGEN(3), FINDGEN(3), xx, yy
  

  ;Now that the star tile aray has been computed,
  ;let's get offsets relative to the minInd image
  image_offsets = FLTARR(2, numFiles)
  FOR iFile = 0, numFiles - 1 DO BEGIN
    ;Skip the "self" image
    IF iFile EQ minInd THEN CONTINUE

    ; Now experiment with the convolution procedure
    corr_image = CONVOL_FFT(starTileArray[*,*,minInd], starTileArray[*,*,iFile], /CORRELATE)
    
    ;Define kernels for the Sobel convolution
    Sy = [[ 1, 2, 1], $
      [ 0, 0, 0], $
      [-1,-2,-1]]
      
    Sx = [[-1, 0, 1], $
      [-2, 0, 2], $
      [-1, 0, 1]]
      
    ;Compute the gradient in the x and y directions
    Gx = CONVOL(corr_image, Sx)
    Gy = CONVOL(corr_image, Sy)
    
    ;Grab the location of the maximum correlation
    maxInd = WHERE(corr_image EQ MAX(corr_image), numMax)
    IF numMax NE 1 THEN MESSAGE, 'Could not find a unique maximum in correlation image'
    
    max_xy = ARRAY_INDICES(corr_image, maxInd)
    
    ;Cut out a 3x3 slice centered on the max index
    lf_pk    = max_xy[0] - 1
    rt_pk    = lf_pk + 2
    bt_pk    = max_xy[1] - 1
    tp_pk    = bt_pk + 2
    Gx1      = Gx[lf_pk:rt_pk, bt_pk:tp_pk]
    Gy1      = Gy[lf_pk:rt_pk, bt_pk:tp_pk]
    
    ;Fit a perfectly flat plane to the Gx subregion
    resultX = SFIT(Gx1, 1, KX = polyX, /MAX_DEGREE)
    
    ;Grab some values from that fit to compute the plane equation
    zzX00 = resultX[0,0]
    zzX02 = resultX[0,2]
    zzX20 = resultX[2,0]
    
    ;Compute numerical derivatives to get plane equation
    Xdzdx = 0.5D*(zzX20 - zzX00)
    Xdzdy = 0.5D*(zzX02 - zzX00)
    
    ;Line for plane intersepcting the x-y-plane
    ; Xine = -(zzX00/Xdzdy) + (-Xdzdx/Xdzdy)*xVals
    xLineIntercept = -zzX00/Xdzdy
    xLineSlope     = -Xdzdx/Xdzdy
    
    ;Fit a perfectly flate plane to the Gy subregion
    resultY = SFIT(Gy1, 1, KX = polyY, /MAX_DEGREE)
    
    ;Grab some values from that fit to compute the plane equation
    zzY00 = resultY[0,0]
    zzY02 = resultY[0,2]
    zzY20 = resultY[2,0]
    
    ;Compute numerical derivatives to get plane equation
    Ydzdx = 0.5D*(zzY20 - zzY00)
    Ydzdy = 0.5D*(zzY02 - zzY00)
    
    ;Line for plane intersepcting the x-y-plane
    ; Yine = -(zzY00/Ydzdy) + (-Ydzdx/Ydzdy)*xVals
    yLineIntercept = -zzY00/Ydzdy
    yLineSlope     = -Ydzdx/Ydzdy
    
    ;Now we can love for the point where they intersect!!!
    xIntersect = (xLineIntercept - yLineIntercept)/(yLineSlope - xLineSlope)
    yIntersect = xLineIntercept + xLineSlope*xIntersect
    
    ;Compute integer pixel offsets
    ;Measure the shape of the correlation image
    szCorr = SIZE(corr_image, /DIMENSIONS)
    
    ;If szCorr[i] is odd, then add 0.5, else add 1.0
    indexCorrection = 1D - 0.5D*[szCorr mod 2]
    
    ;Compute actual integer offsets
    dxdyInt  = (max_xy - 0.5*szCorr + indexCorrection)
    
    ;Compute subpixel position refinements
    dxdyFrac = ([xIntersect, yIntersect] - 1D)
    dxdyTot  = dxdyInt + dxdyFrac

    ;Store the final image offset
    image_offsets[*, iFile] = [dxdyTot[0] - dx[iFile], dxdyTot[1] - dy[iFile]]
  ENDFOR
  
  RETURN, image_offsets

END