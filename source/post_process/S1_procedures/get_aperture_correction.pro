FUNCTION GET_APERTURE_CORRECTION, kingParams, selectedApr

  nApr           = N_ELEMENTS(selectedApr)
  aprCorrections = FLTARR(nApr)

  FOR i = 0, nApr - 1 DO BEGIN
    integratedKing    = INTEGRATED_KING_MODEL(kingParams, [0, selectedApr[i]])
    aprCorrections[i] = 2.5*ALOG10(integratedKing)    

  ;**** CODE TO CHECK FOR REASONABILITY ****
;  xTest      = 10*(FINDGEN(501)/500)
;  kingModel  = king_model(kingParams, xTest)
;  integrand  = kingModel*2*!pi*xTest
;  window, 0
;  plot, xTest, 2*!PI*kingModel;, YRANGE = [0, 0.1]
;  oplot, xTest, integrand
;  polyfill, [0, xTest[where(xTest LE selectedApr[i])], selectedApr[i]], $
;    [0,integrand[where(xTest LE selectedApr[i])],0], $
;    /LINE_FILL, ORIENTATION=45.0
;  XYOUTS, 0.5, 0.8, /NORMAL, CHARSIZE = 1.5, $
;    STRING(100*integratedKing, FORMAT='(F4.1, "% of flux inside aperture")')
;  XYOUTS, 0.5, 0.7, /NORMAL, CHARSIZE = 1.5, $
;    STRING(aprCorrections[i], FORMAT='("Aperture correction of ", F6.3, " mags")')
;  legendStr = STRING(2,F='(I1)') + cgSymbol("pi") + cgSymbol("times") + "King Model"
;  AL_LEGEND, [legendStr], LINESTYLE=[0], POSITION = [0.48, 0.6], /NORMAL, $
;    box = 0, CHARSIZE = 1.5, LINSIZE = 0.5
;  stop
  ;******************************************

  ENDFOR
  
  RETURN, aprCorrections

END