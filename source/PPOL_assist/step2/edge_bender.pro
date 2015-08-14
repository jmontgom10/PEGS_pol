PRO EDGE_BENDER, img, mask, flattenedImg
  
  
  topSkyBox = (img*(~mask))[25:1001,515:765]                   ;Grab the values of the
  botSkyBox = (img*(~mask))[25:1001,260:510]
  
  topSkyValues = topSkyBox[WHERE(topSkybox NE 0)]
  botSkyValues = botSkyBox[WHERE(topSkybox NE 0)]
  
  topSkyMean = (MEDIAN_FILTERED_MEAN(topSkyValues))[0]
  botSkyMean = (MEDIAN_FILTERED_MEAN(botSkyValues))[0]
  
  flattenedImg = img
  
  ;Flatten the top quadrants
  FOR i = 513, 1013 DO BEGIN
    imgRow    = img[200:800,i]
    rowPixels = WHERE(mask[200:800,i] EQ 0, numPix)
    IF numPix GT 0 THEN BEGIN
      rowValues          = imgRow[rowPixels]
      rowMean            = (MEDIAN_FILTERED_MEAN(rowValues))[0]
      flattenedImg[*,i] -= (rowMean - topSkyMean)
    ENDIF
  ENDFOR

  ;Flatten the bottom quadrants
  FOR i = 9, 511 DO BEGIN
    imgRow  = img[200:800,i]
    rowPixels = WHERE(mask[200:800,i] EQ 0, numPix)
    IF numPix GT 0 THEN BEGIN
      rowValues          = imgRow[rowPixels]
      rowMean            = (MEDIAN_FILTERED_MEAN(rowValues))[0]
      flattenedImg[*,i] -= (rowMean - topSkyMean)
    ENDIF
  ENDFOR
  
  flattenedImg[*,0:9]    = -1E6                             ;Mark the top rows as bad
  flattenedImg[*,1014:*] = -1E6                             ;Mark the bottom rows as bad
END