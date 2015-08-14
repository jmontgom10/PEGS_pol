FUNCTION STAR_MATCHER, starsMimir, stars2MASS

  ;******I SHOULD ADD A COMPONENT WHERE THE MATCHER SOLVES FOR A PLATE SCALE FIRST******
  ;******THIS WOULD REQUIRE INPUTTING stars2MASS as an array of RAs and Decs.**********

  num2MASS          = N_ELEMENTS(stars2MASS)
  num2MASStriangles = num2MASS*(num2MASS - 1)*(num2MASS - 2)/6        ;Count all possible triangles in 2MASS

  dist2MASS = FLTARR(num2MASS, num2MASS)                              ;Create array to star inter-star separations
  FOR i = 0, num2MASS - 1 DO BEGIN                                    ;Loop through ALL the provided stars
    dist2MASS[*,i] =  SQRT((stars2MASS.x - stars2MASS[i].x)^2 $       ;Compute inter-star separations
      + (stars2MASS.y - stars2MASS[i].y)^2)
  ENDFOR

  triangles2MASS  = FLTARR(3, num2MASStriangles)
  vertices2MASS   = FLTARR(3, num2MASStriangles)
  triangle2Mindex = 0
  FOR i = 0, num2MASS - 1 DO BEGIN
    FOR j = (i + 1), num2MASS - 1 DO BEGIN
      FOR k = (j + 1), num2MASS - 1 DO BEGIN
        ;Each element in this triangle side-length array must be made
        ;from the vertices stored in the OTHER two elements of the vertice array below
        ;E.g. if vertices = [i,j,k], then sides = [jk, ik, ij]
        tempDistances = [dist2MASS[j,k],dist2MASS[i,k],dist2MASS[i,j]];Temporarily store the side lengths
        sortArray     = SORT(tempDistances)                          ;Array for sorting triangles 
        triangles2MASS[*,triangle2Mindex] = tempDistances[sortArray]  ;Place sorted distances in triangle array
        vertices2MASS[*,triangle2Mindex]  = ([i,j,k])[sortArray]
        triangle2Mindex++                                             ;Increment the triangle index
      ENDFOR
    ENDFOR
  ENDFOR

  xTri2MASS = REFORM(triangles2MASS[1,*]/triangles2MASS[2,*])         ;Normalize medium length by longest
  yTri2MASS = REFORM(triangles2MASS[0,*]/triangles2MASS[2,*])         ;Normalize shortest length by longest
  avgSep    = SQRT(0.25/num2MASStriangles)                            ;Average 2MASS surface density in triangle space
  
  avg2MSize = AVG(triangles2MASS, 0)                                  ;Average side-length for EACH triangle

  numMimir          = N_ELEMENTS(starsMimir)
  numMimirTriangles = numMimir*(numMimir - 1)*(numMimir - 2)/6        ;Count all possible triangles in Mimir
  
  distMimir = FLTARR(numMimir, numMimir)                              ;Create array to star inter-star separations
  FOR i = 0, numMimir - 1 DO BEGIN                                    ;Loop through ALL the provided stars
    distMimir[*,i] =  SQRT((starsMimir.x - starsMimir[i].x)^2 $     ;Compute inter-star separations
      + (starsMimir.y - starsMimir[i].y)^2)
  ENDFOR
  
  trianglesMimir  = FLTARR(3, numMimirTriangles)
  verticesMimir   = FLTARR(3, numMimirTriangles)
  triangleMiIndex = 0
  
  FOR i = 0, numMimir - 1 DO BEGIN
    FOR j = (i + 1), numMimir - 1 DO BEGIN
      FOR k = (j + 1), numMimir - 1 DO BEGIN
        ;Each element in this triangle side-length array must be made
        ;from the vertices stored in the OTHER two elements of the vertice array below
        ;E.g. if vertices = [i,j,k], then sides = [jk, ik, ij]
        tempDistances = [distMimir[j,k],distMimir[i,k],distMimir[i,j]];Temporarily store the side lengths
        sortArray     = SORT(tempDistances)                           ;Array for sorting triangles
        trianglesMimir[*,triangleMiIndex] = tempDistances[sortArray]  ;Place sorted DISTANCES in triangle array
        verticesMimir[*,triangleMiIndex] = ([i,j,k])[sortArray]       ;Place sorted VERTICES in triangle array
        triangleMiIndex++
      ENDFOR
    ENDFOR
  ENDFOR

  xTriMimir = REFORM(trianglesMimir[1,*]/trianglesMimir[2,*])         ;Normalize medium length by longest
  yTriMimir = REFORM(trianglesMimir[0,*]/trianglesMimir[2,*])         ;Normalize shortest length by longest

;  PLOT, [0,1], [0,1], /XSTYLE, /YSTYLE, /NODATA
;  OPLOT, xTriMimir, yTriMimir, PSYM=1, COLOR=cgColor('Green')

  FOR i = 0, numMimirTriangles - 1 DO BEGIN
    Mimir2MASStriSep = SQRT((xTri2MASS - xTriMimir[i])^2 $            ;Compute separation in triangle space
      + (yTri2MASS - yTriMimir[i])^2)
    separSort        = SORT(Mimir2MASStriSep)
    Mimir2MASStriSep = Mimir2MASStriSep[separSort]                    ;Sort the separation array
    done    = 0                                                       ;Variable for if the search for a  match is done
    j       = 0                                                       ;This will index through possible 2MASS triangles
    matched = 0                                                       ;Variable for if the search was successful
    WHILE ~done DO BEGIN
      ;(1) Test if the triangles are about the same size
      this2MASStri  = separSort[j]                                    ;Index for the current triangle to test
      avgMimirSize  = MEAN(trianglesMimir[*,i])                       ;Grab the average size of THIS 2MASS triangle
      sizeRatioTest = ABS(avgMimirSize/avg2MSize[this2MASStri] - 1) $ ;Test if the triangles are about the same size
        LT 5E-3                                                       ;Allow a 0.5% difference in size

      ;(2) Test if the star relative brightnesses agree
      brightSortMimir  = SORT(starsMimir[verticesMimir[*,i]].flux)
      brightSort2MASS  = SORT(stars2MASS[vertices2MASS[*,this2MASStri]].flux)
      relativeFluxTest = TOTAL(brightSortMimir EQ brightSort2MASS) EQ 3
      fluxRatios       = starsMimir[verticesMimir[brightSortMimir,i]].flux $
                       / stars2MASS[vertices2mass[brightSort2MASS,this2MASStri]].flux
      ratioPercentErr  = (MAX(fluxRatios) - MIN(fluxRatios))/MEDIAN(fluxRatios)
      fluxRatioTest    = ratioPercentErr LT 0.20                      ;Allow a 20% difference in relative fluxes

      ;(3) if it passes BOTH of these tests AND is close in triangle space,
      ;then call it a match
      IF sizeRatioTest AND $
         relativeFluxTest AND fluxRatioTest AND $
        (Mimir2MASStriSep[j] LT 0.5*avgSep) THEN BEGIN
          matched = 1
          done    = 1
          IF N_ELEMENTS(Mimir2MASStriMap) EQ 0 THEN BEGIN
            Mimir2MASStriMap = [[verticesMimir[*,i]],[vertices2MASS[*,this2MASStri]]]
          ENDIF ELSE BEGIN
            tempMap       = [[verticesMimir[*,i]],[vertices2MASS[*,this2MASStri]]]
            Mimir2MASStriMap = [[[Mimir2MASStriMap]], [[[tempMap]]]]
          ENDELSE
      ENDIF
      IF Mimir2MASStriSep[j] GE 0.5*avgSep THEN done = 1              ;If the separation is too larg, then give up
      j++                                                             ;Increment to the next 2MASS triangle
    ENDWHILE
  ENDFOR

  mapMimir     = Mimir2MASStriMap[*,0,*]                              ;Create a temporary listing of Mimir verticies
  map2MASS     = Mimir2MASStriMap[*,1,*]                              ;Create a temporary listing of 2MASS verticies
  FOR i = 0, numMimir - 1 DO BEGIN                                    ;Loop through ALL the Mimir stars
    MimirMapIndex = WHERE(mapMimir EQ i, starIsInTriangle)            ;Look for THIS star in the Mimir triangles
    IF starIsInTriangle GT 0 THEN BEGIN                               ;If that star has been triangle matched then...
      match2Mstars = map2MASS[MimirMapIndex]                          ;Grab the matched 2MASS vertices
;      print, match2Mstars

      uniqMatches  = UNIQ(match2Mstars, SORT(match2Mstars))           ;Grab the UNIQ match ***2MASS ELEMENT INDICES***
      numUniq      = N_ELEMENTS(uniqMatches)                          ;Count the number of uniq matches found
      IF numUniq EQ 1 THEN BEGIN                                      ;If only one match found, then great!
        IF N_ELEMENTS(MimirMap2MASS) EQ 0 THEN BEGIN
          MimirMap2MASS = [[i],[match2Mstars[uniqMatches]]]                   ;Store that match in an array
        ENDIF ELSE BEGIN
          MimirMap2MASS = [MimirMap2MASS, [[i],[match2Mstars[uniqMatches]]]]  ;Append the matches...
        ENDELSE
      ENDIF ELSE BEGIN                                                    ;If more than one match was found, then...
        uniqCounts = INTARR(numUniq)                                      ;Create an array for the number of each match
        FOR j = 0, N_ELEMENTS(uniqMatches) - 1 DO BEGIN                   ;Loop through the possible matches
          uniqCount     = TOTAL(match2Mstars EQ uniqMatches[j])           ;Find the number of matches
          uniqCounts[j] = uniqCount
        ENDFOR
        percentBestMatch = FLOAT(MAX(uniqCounts))/FLOAT(countMatch)       ;How common is the MOST common match?
        IF percentBestMatch GT 0.95 THEN BEGIN                            ;If the best match is exceedingly common
          uniqMatches  = uniqMatches[WHERE(uniqCount EQ MAX(uniqCount))]  ;Grab the MOST common match...
          match2Mindex = match2Mstars[uniqMatches]                        ;...and its associated 2M entry index
          IF N_ELEMENTS(MimirMap2MASS) EQ 0 THEN BEGIN
            MimirMap2MASS = [[i],[match2Mindex]]                          ;Store that match in an array
          ENDIF ELSE BEGIN
            MimirMap2MASS = [MimirMap2MASS, [[i],[match2Mindex]]]         ;Append the matches...
          ENDELSE
        ENDIF                                                             ;Otherwise skip this potential match
      ENDELSE
    ENDIF
  ENDFOR


  ;as a final step, the VERTICES of the triangles must be compared.
;  FOR i = 0, N_ELEMENTS(MimirMap2MASS)/2 - 1 DO BEGIN
;    verticesMimir[*,MimirMap2MASS[i]]
;  ENDFOR
  
  ;********JUST TO BE SURE, I SHOULD DO THIS VISUALLY,*****
  ;********TO MAKE SURE THE SAME LINES ARE BEING DRAWN*****
  
  ;Return a list of indices mapping identical Mimir stars to 2MASS stars.
  
  
  
  ;Need a totally new algorithm...
  ;DO THE TRIANGLES!!!
;  distances = FLTARR(numMimir, numMimir)                              ;Create array to star inter-star separations
;  FOR i = 0, numStars - 1 DO BEGIN                                    ;Loop through ALL the provided stars
;    distances[*,i] =  SQRT((x - x[i])^2 + (y - y[i])^2)               ;Compute inter-star separations
;  ENDFOR
;  FOR i = 0, numTriangles - 1 DO BEGIN
;    trianglesStruc = {}
;  ENDFOR
  
  
  
  
  
  
  
  
  
  
;  ;    exposureTime = SXPAR(header, 'IMGETIME')                         ;This should be taken out of the header
;;  exposureTime = 10.0
;;  findMags     = zeroPointMag - 2.5*ALOG10(findInfo.starFlux/exposureTime) ;Convert estimated fluxes to magnitudes
;
;  ;Match the found stars to the 2MASS-PSC and store in a file
;  
;  ;********FOR NOT MAGNITUDES DON'T MATTER....*****
;  ;********WE WILL QueryVizier AGAIN FOR PHOTOMETRY****
;  ;  band        = STRTRIM(SXPAR(header, "BAND"), 2)                  ;What waveband is this image?
;  ;  bandNumber  = WHERE(band EQ ["J", "H", "Ks"], count)             ;Convert that band to a number
;  ;  IF count EQ 0 THEN STOP                                          ;Test that a NIR band was identified
;  ;  CASE bandNumber OF                                               ;Store the magnitudes of this image
;  ;    1: magnitude = info.jmag
;  ;    2: magnitude = info.hmag
;  ;    3: magnitude = info.kmag
;  ;  ENDCASE
;  ;searchRad    = 3.0/60                                               ;matching tolerance of 3 arcsec
;
;;  matchedStars = 0                                                     ;Counter for the number of 2MASS matched stars
;;  OPENW, lun, filename, /GET_LUN                                      ;Open the matched star catalog file
;;  PRINTF, lun, ";            STAR        X        Y" $                ;Print the catalog header
;;    + "    2M_DESIGNATION    RAJ2000    DEJ2000   "
;
;  ;Loop through the sparse FIND stars and find best matching 2MASS STARS
;  ;
;  ;NEED TO DO THE FOLLOWING
;  ;1. Check if the 2MASS star has already been matched to something
;  ;     a. If it has, then check if this star is a BETTER match (smaller distance)
;  ;
;  ;2. Once al the BEST matches have been found, check the following
;  ;     a. offsets should be within a tolerated deviation of MODE (MMM)
;  ;     b. directions should be within a tolerated deviation of MODE (MMM)
;  ;     c. (iterate BACK through if necessary)
;  num2MASS  = N_ELEMENTS(starInfo)                                    ;Count the number of 2MASS entries
;  matchFlag = INTARR(num2MASS)                                        ;An array for keeping track of matched stars
;  numFind   = N_ELEMENTS(findInfo.starFlux)                           ;Count the number of FIND entries
;  matchInds = LONARR(numFind)                                         ;An array for assigning each 2MASS entry to
;  FOR i = 0, numFind - 1 DO BEGIN                                     ;Loop through FIND star entries
;    deltaX    = x2MASS - findInfo.xPos[i]                             ;Compute X-offsets of 2MASS entries
;    deltaY    = y2MASS - findInfo.yPos[i]                             ;Compute Y-offsets of 2MASS entries
;    distances = SQRT(deltaX^2 + deltaY^2)                             ;Compute net offsets of 2MASS entries
;    minDist   = WHERE(distances EQ MIN(distances))                    ;Find the BEST match from the available stars
;
;    IF matchFlag[minDist] EQ 1 THEN BEGIN
;      
;    ENDIF ELSE BEGIN
;      matchInds[i]       = minDist                                    ;Store the index of the corresponding 2MASS entry
;      matchFlag[minDist] = 1                                          ;Mark that 2MASS entry as already matched
;    ENDELSE
;    
;;**THIS IS LIKELY not A USEFUL DIAGNOSTIC**
;;    distPA    = ATAN(deltaY,deltaX)*!RADEG
;;******************************************
;    stop
;;    toleranceTest = MIN(distances) LT 5.0                             ;Test if CLOSEST match is LT 3.0 arcsec
;;    confusionTest = MIN(distances[WHERE(distances GT MIN(distances))]) $;Test if 2ND CLOSEST match is GT 10.0 arcesc
;;      GT 10.0
;    IF toleranceTest AND confusionTest THEN BEGIN
;      findInd = WHERE(distances EQ MIN(distances))                    ;Store the index of the closest match
;      IF N_ELEMENTS(matchStruc) EQ 0 THEN BEGIN                       ;Create the structure
;        matchStruc = {xPos: (findInfo.xPos[findInd])[0], $
;          yPos: (findInfo.yPos[findInd])[0], $
;          RAJ2000: starInfo[i].RAJ2000, $
;          DEJ2000: starInfo[i].DEJ2000}
;      ENDIF ELSE BEGIN                                                ;Append more values
;        matchStruc = [matchStruc, $
;          {xPos: (findInfo.xPos[findInd])[0], $
;          yPos: (findInfo.yPos[findInd])[0], $
;          RAJ2000: starInfo[i].RAJ2000, $
;          DEJ2000: starInfo[i].DEJ2000}]
;      ENDELSE
;;      string1 = STRING((matchedStars + 1), xCen, YCen, $              ;Format the image position
;;        FORMAT = '(I17,2F9.3)')
;;      string2 = STRING(starInfo[i].RAJ2000, starInfo[i].DEJ2000, $    ;Format the 2MASS position
;;        FORMAT = '(2F11.6, F7.3)')
;;      printString = string1 + "  " + starInfo[i]._2MASS + string2     ;Join the entire string
;;      IF N_ELEMENTS(magnitude) GT 1 THEN STOP                        ;Check for duplicate entries
;;      PRINTF, lun, printString
;;      matchedStars++                                                  ;Increment the number of stars matched
;
;    ENDIF
;  ENDFOR
;  FREE_LUN, lun
;  PRINT, FORMAT = '("Matched ",I4," high quality stars in the 2MASS database")', matchedStars
  RETURN, MimirMap2MASS
END