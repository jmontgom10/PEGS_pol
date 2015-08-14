FUNCTION GET_OPTIMUM_APERTURES, image, xStars, yStars, PSF_FWHM, skyradii

  phpadu    = 8.21                                                    ;This value can be found on Mimir website
  ronois    = 17.8                                                    ;(elec) This value is from Mimir website
;  ronois   = 3.1                                                     ;(ADU) This value is from GPIPS code "S4_PSF_fit"
;  badpix    = [-300L, 6000L]
  badpix    = [-300L, 12000L]

;  WINDOW, 0
  nStars      = N_ELEMENTS(xStars)
  optimumAprs = FLTARR(nSTars)
  FOR i = 0, nStars - 1 DO BEGIN
    spanRad  = 3.0                                          ;Number of FWHM the aperture should span
    deltaRad = spanRad*PSF_FWHM/11.0                        ;Starting aperture increments (spans 3 FWHM)
    midRad   = 2.0*PSF_FWHM + 5.5*deltaRad                  ;Starting midrange aperture
    redoAPER = 0
    done     = 0
    loops    = 1
    WHILE ~done DO BEGIN
      SNRapr = (midRad + deltaRad*(FINDGEN(12)-5.5))        ;Generate a list of apertures for high SNR  
      
      
      ;***** APER is misbehaving
      APER, image, xStars[i], yStars[i], $
        flux, errap, sky, skyerr, phpadu, SNRapr, skyradii, badpix, /SILENT, /FLUX

;      G_APER, image, xStars[i], yStars[i], $
;        flux, errap, sky, skyerr, phpadu, SNRapr, skyradii, badpix, MAX(badPix), /SILENT, /FLUX

      SNRs = flux/errap

;      SNRs = flux[*,i]/errap[*,i]
;      PLOT, SNRapr, SNRs, PSYM = -4, YRANGE=[MIN(SNRs), MAX(SNRs)]

      goodRad = WHERE(flux NE 0, numGood)
      
      
      IF (numGood EQ 0) THEN BEGIN                          ;If none of these are good,
        minRad = MAX(SNRapr)                                ;then try next largest full set of apertures
        redoAPER = 1
        stop
      ENDIF
      
      IF (numGood GT 0) AND (numGood LT 12) THEN BEGIN      ;Find the minimum good radius,
        midRad = MIN(goodRad) + 5.5*deltaRad                ;make sure all radii are greater than minimum good aperture
        redoAPER = 1
        stop
      ENDIF
      
      IF numGood EQ 12 THEN redoAPER = 0

;        SNRapr = SNRapr[MIN(goodRads)] + deltaR*FINDGEN(12)
;        APER, image, xStars[i], yStars[i], $
;          flux, errap, sky, skyerr, phpadu, SNRapr, skyradii, badpix, /SILENT, /FLUX
;        SNRs = flux/errap
;      ENDIF
;      deltaR = 1.5                                                    ;Start iterations with a 0.5 pixel apr separation
      
      bestApr = WHERE(SNRs EQ MAX(SNRs), numMax)                      ;Locate the aperture with best SNR
      IF numMax EQ 0 THEN BEGIN
        redoAPER = 1
        STOP
      ENDIF
      
      IF bestApr LT 2 THEN BEGIN                                      ;If best aperture is small, then try smaller set of apertures
        midRad = (SNRapr[bestApr])[0]
        redoAPER = 1
        STOP
        ;
        ;
        ;
        ;
        ;**** GETTING STUCK AT i = 3!
        ;
        ;
;        SNRapr = SNRapr[MIN(goodRads)] + deltaR*FINDGEN(12)
      ENDIF
      
      IF bestApr GT 9 THEN BEGIN                                      ;If best aperture is large, then try a larger set of apertures
        midRad = (SNRapr[bestApr])[0]
        redoAPER = 1
        STOP
      ENDIF
      
;      stop
      IF redoAPER THEN CONTINUE                                       ;If there is a need to redo the aperture photometry, restart
      
;      stop
      IF (bestApr GT 2) AND (bestApr LT 9) AND (numMax EQ 1) THEN BEGIN
;        bestApr = (SNRapr[WHERE(SNRs EQ MAX(SNRs))])[0]
        dydx  = DERIV(SNRapr, SNRs)                                   ;Estimate 1st derivative
        dydx2 = DERIV(SNRapr, dydx)                                   ;Estimate 2nd derivative
        IF dydx2[bestApr] LT 0 THEN BEGIN
          IF deltaRad LT 0.25 THEN BEGIN
            optimumAprs[i] = (SNRapr[bestApr])[0]
            done = 1
          ENDIF
          deltaRad *= 0.5                                               ;Decrease the radius
;          SNRapr    = (SNRapr[bestApr])[0] + deltaRad*(FINDGEN(12)/11.0 - 0.5)
        ENDIF ELSE STOP
      ENDIF ELSE IF numMax EQ 0 THEN deltaRad *= 1.5 $                  ;If none of the apertures worked, then make them bigger
        ELSE BEGIN
        IF loops GT 6 THEN BEGIN                                      ;If not converging
          dydx  = DERIV(SNRapr, SNRs)                                 ;Estimate 1st derivative
          dydx2 = DERIV(SNRapr, dydx)                                 ;Estimate 2nd derivative
          lineCoeffs = LINFIT(SNRapr, dydx)                           ;try to center where derivative passes through zero
          bestApr    = -lineCoeffs[0]/lineCoeffs[1]
          SNRapr     = bestApr + deltaRad*(FINDGEN(12)/11.0 - 0.5)
;          PLOT, SNRapr, dydx, PSYM = 4
;          OPLOT, SNRapr, (lineCoeffs[0] + SNRapr*lineCoeffs[1])
;          OPLOT, [bestApr], [lineCoeffs[0] + bestApr*lineCoeffs[1]], PSYM=7, COLOR=cgColor('red')
;          stop
        ENDIF ELSE SNRapr  = (SNRapr[bestApr])[0] + deltaRad*(FINDGEN(12)/11.0 - 0.5)
      ENDELSE
      loops++
      IF loops GT 10 THEN BEGIN
        optimumAprs[i] = !VALUES.F_NAN
        done = 1
      ENDIF

    ENDWHILE
  ENDFOR
  
  stop
  RETURN, optimumAprs

END