PRO MAP_SUPER_PIXEL, polMaps, j, k, rebinPix, rebinLevel, mesh_tree, $
finalPolMaps, finalPolValues, QUADRANT_NAME=quadrant_name
  
  ;This procedure will recursively search the mesh_tree
  ;and populate finalPolMaps and finalPolValues accordingly
 
  ;The "QUADRANT_NAME" keyword will be a CONCATENATED string of quadrant values
  ;Each of these values will be used to parse the exact region of the final pol_map to fill
  
  ;Test if the mesh_tree recommends dividing the super-pixel

  IF N_TAGS(mesh_tree) EQ 0 THEN BEGIN                                ;If it is not a structure, then fill in the map!

    ;Here is where we parse the quadrant_name string
    
    IF N_ELEMENTS(quadrant_name) EQ 0 THEN BEGIN
      lf = j*rebinPix[0]                                              ;Determine which parts of the final map
      rt = lf + rebinPix[0] - 1                                       ;need to be filled in
      bt = k*rebinPix[0]
      tp = bt + rebinPix[0] - 1
      
      xFrom = j                                                       ;Using the super-pixel (x,y) coordinates
      yFrom = k
    ENDIF ELSE BEGIN
      lf    = j*rebinPix[0]                                           ;Start with the left side of the super-pixel
      bt    = k*rebinPix[0]                                           ;Start with the bottom of the super-pixel
      xFrom = j                                                       ;Start with the super-pixel x index
      yFrom = k                                                       ;Start with the super-pixel y index
      FOR iQuad = 0, rebinLevel - 1 DO BEGIN                          ;Loop through each rebinning level
        this_quad = STRMID(quadrant_name, 2*iQuad, 2)                 ;Grab the current quadrant in the loop
        yQuad     = STRMID(this_quad,0,1)                             ;Extract the x and y parts of this quadrant
        xQuad     = STRMID(this_quad,1,1)
        xFrom    *= 2                                                 ;Each level down has double the number of pixels
        yFrom    *= 2
        IF yQuad EQ 'U' THEN BEGIN
          bt    += rebinPix[iQuad+1]                                  ;Shift the finalPolMaps y index
          yFrom += 1                                                  ;Shift the reference map y index
        ENDIF
        IF xQuad EQ 'R' THEN BEGIN
          lf    += rebinPix[iQuad+1]                                  ;Shift the finalPolMaps x index
          xFrom += 1                                                  ;Shift the reference map x index
        ENDIF
      ENDFOR

      rt = lf + rebinPix[rebinLevel] - 1
      tp = bt + rebinPix[rebinLevel] - 1
    ENDELSE

    finalPolMaps[lf:rt,bt:tp,*] = REBIN((polMaps.(rebinLevel))[xFrom,yFrom,*], $ ;Copy over the map data
      rebinPix[rebinLevel], rebinPix[rebinLevel], 7, /SAMPLE)
    
    IF N_ELEMENTS(finalPolValues) EQ 0 $                                         ;Copy over the point data
      THEN finalPolValues = REFORM((polMaps.(rebinLevel))[xFrom,yFrom,*]) $
      ELSE finalPolValues = [[finalPolValues], [REFORM((polMaps.(rebinLevel))[xFrom,yFrom,*])]]
    
  ENDIF ELSE IF N_TAGS(mesh_tree) EQ 4 THEN BEGIN                   ;If this is not the terminal pixel, then loop through sub-pixels
    quadrant_names = TAG_NAMES(mesh_tree)                           ;Save the quadrant names to pass into recursion

    IF N_ELEMENTS(quadrant_name) EQ 0 $
      THEN quadrant_names = TAG_NAMES(mesh_tree) $
      ELSE quadrant_names = quadrant_name + quadrant_names
    FOR i = 0, 3 DO BEGIN
      MAP_SUPER_PIXEL, polMaps, j, k, rebinPix, (rebinLevel+1), mesh_tree.(i), $
        finalPolMaps, finalPolValues, QUADRANT_NAME=quadrant_names[i]
    ENDFOR

  ENDIF
  
  

END