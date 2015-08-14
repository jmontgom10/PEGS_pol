PRO DELETE_STAR_PHOTOMETRY, filename, deleteStars

  ;Begin by making a copy of the original
  copyName = STRSPLIT(filename, '.', /EXTRACT)
  copyName = copyName[0] + '.' + copyName[1] + '_orig.' + copyName[2]
  IF ~FILE_TEST(copyName) THEN FILE_COPY, filename, copyName

  ;Read in the photometry header
  OPENR, lun, filename, /GET_LUN
  fullHead = ''
  line     = ''
  nskip    = 0
  WHILE ~EOF(lun) DO BEGIN
    readf, lun, line, format='(a)'
    test = STRPOS(line, ';;')
    IF(test NE -1) THEN BEGIN
      fullHead = [fullHead, line]
      nskip++
    ENDIF
  ENDWHILE
  FREE_LUN, lun
  fullHead = fullHead[1:*]
  
  ;READCOL, filename, id, x, y, ra, dec, SKIPLINE=nskip, $           ;Read in the photometry data
  ;  FORMAT='L,D,D,D,D,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X', /SILENT
  ;  
    READCOL, filename, SKIPLINE=nskip, $                      ;Read in all photometry files
    FORMAT = 'L,D,D,D,D,F,L,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F', $
      id, x, y, ra, dec, sky, group, $
      mag0,  mag1,  mag2,  mag3,  mag4,  mag5,  mag6,  mag7,  mag8,  mag9,  mag10,  mag11, $
      smag0, smag1, smag2, smag3, smag4, smag5, smag6, smag7, smag8, smag9, smag10, smag11
  
  OPENW, lun, filename, /GET_LUN
  FOR i = 0, N_ELEMENTS(fullHead) - 1 DO BEGIN                ;Print out the header
    PRINTF, lun, fullHead[i]
  ENDFOR
  FOR i = 0, N_ELEMENTS(id) - 1 DO BEGIN                      ;Count the number of stars read in
    IF TOTAL(i EQ (deleteStars-1)) GT 0 THEN CONTINUE         ;Skip stars marked for deletion
    
    IF(dec[i] GT 0.0) THEN dsign = '+' else dsign = '-'   ;Format the sign of the declination
    d    = abs(dec[i])
    dten = FLOOR(d/10)
    d    = d - 10 * dten
    
    PRINTF, lun, FORMAT='(I6,1x,F7.2,1x,f7.2,1x,f10.6,1x,a1,i1.1,f8.6,1x,f8.2,1x,i4,1x,12(f7.4,1x),12(f7.4,1x))',$
      i, x[i], y[i], ra[i], dsign, dten, d, sky[i], group[i], $
      mag0[i], mag1[i], mag2[i], mag3[i], mag4[i], mag5[i], mag6[i], mag7[i], mag8[i], mag9[i], mag10[i], mag11[i],$
      smag0[i],smag1[i],smag2[i],smag3[i],smag4[i],smag5[i],smag6[i],smag7[i],smag8[i],smag9[i],smag10[i],smag11[i]
  ENDFOR
  FREE_LUN, lun

END