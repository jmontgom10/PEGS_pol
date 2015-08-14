FUNCTION FIT_GALAXY_MODELS, event, image, x0, y0, ellipseAxes, PAguess, skynoise, PSFparams, $
  FIX_PA=FIX_PA

  ON_ERROR, 2
  IF N_ELEMENTS(FIX_PA) EQ 0 THEN FIX_PA = 0
  IF (N_ELEMENTS(FIX_PA) NE 0) THEN BEGIN
    IF ~((FIX_PA EQ 1) XOR (FIX_PA EQ 0)) THEN MESSAGE, 'FIX_PA does not accept values', LEVEL=-1
  ENDIF
    
  xn = n_elements(image[*,0])
  yn = n_elements(image[0,*])
  xr = dindgen(xn)
  yc = dindgen(yn)
  x  = xr#(yc*0+1)
  y  = (xr*0+1)#yc
  re = (((x-x0)*cos((PAguess-90D)*!DTOR)+(y-y0)*sin((PAguess-90D)*!DTOR))^2.0 $
      + ((x-x0)*sin((PAguess-90D)*!DTOR)-(y-y0)*cos((PAguess-90D)*!DTOR))^2.0 / $
        (ellipseAxes[1]/ellipseAxes[0])^2)^0.5
  ;
  ; Specify ranges for distance of the center of the galaxy from center of the image
  if (xn gt 50) then begin
    xrange=15
  endif else begin
    xrange=0.6*xn
  endelse
  ;
  if (yn gt 50) then begin
    yrange=15
  endif else begin
    yrange=0.6*yn
  endelse

  smoothImg = GAUSS_SMOOTH(image,6)
  noiseImg  = (smoothImg LT 2*skynoise)
  useImg    = (smoothImg LT 5E1*skynoise) AND $
              (re LT 1.15*ellipseAxes[0]) AND (x GT 7 AND x LT (xn - 8)) AND (y GT 7 AND y LT (yn - 8))
  gain      = 8E                                            ;gain       (electrons/ADU)
  readNoise = 40E                                           ;Read noise (electrons)
  nCombine  = 6E                                            ;Standard 2MASS tile stacking
  sigmaImg  = SQRT(nCombine*ABS(image)*gain*(~noiseImg) + $
                   nCombine*(skynoise*gain)^2)/gain
;  sigmaImg  = SQRT(ABS(image*gain)*(~noiseImg) + $
;                       (skynoise*gain)^2)/gain
  weight    = useImg/(sigmaImg^(2E))
;  weight    = useImg*sigmaImg


  ;*** FIT SERSIC_DISK ***
  ; p(0) is the center pixel in the x direction
  ; p(1) is the center pixel in the y direction
  ; p(2) is the sky "position angle" (rotation angle) of the major axis (radians CCW from x-axis)
  ; p(3) is the eccentricity (1.0 = straight line, 0 = cirele)
  ; p(4) is the central surface brightness
  ; p(5) is the disk scale length
  ; p(6) is the (1/n) factor in the Sersic profile
  ; p(7) is the background
  ;
  
  parinfo = replicate({value:0.D, fixed:0,limited:[0,0],limits:[0.D,0.D],mpminstep:0.D},12)
  parinfo[0:7].limited = 1  ;Turn on limits
  parinfo[8:*].fixed   = 1  ;Fix the PSF values
  IF KEYWORD_SET(FIX_PA) THEN parinfo[2].fixed = 1          ;Fix the PA value
  ;
  ;Set the lower limits for the fitting procedure
  parinfo[0:7].limits[0] = [x0-xrange, $                    ;p(0) is the center pixel in the x direction
                            y0-yrange, $                    ;p(1) is the center pixel in the y direction
                            -!dpi+0.0001, $                 ;p(2) is the sky "position angle" of the major axis
                            0E, $                           ;p(3) is the eccentricity (1.0 = straight line, 0 = cirele)
                            0.1*max(image), $               ;p(4) is the central surface brightness
                            1E-3*ellipseAxes[0], $          ;p(5) is the disk scale length
                            1E-4, $                         ;p(6) is the (1/n) factor in the Sersic profile
                            -3*skynoise]                    ;p(7) is the background
  ;
  ;Set the upper limits for the fitting procedure
  parinfo[0:7].limits[1] = [x0+xrange, $                    ;p(0) is the center pixel in the x direction
                            y0+yrange, $                    ;p(1) is the center pixel in the y direction
                            !dpi, $                         ;p(2) is the sky "position angle" of the major axis
                            0.9999, $                       ;p(3) is the eccentricity (1.0 = straight line, 0 = cirele)
                            25E*max(image), $               ;p(4) is the central surface brightness
                            max(re), $                      ;p(5) is the disk scale length
                            10E, $                          ;p(6) is the (1/n) factor in the Sersic profile
                            +3*skynoise]                    ;p(7) is the background
  ;
  ;Set initial guesses for image parameters
  parinfo[*].value = [x0, $                                 ;p(0) is the center pixel in the x direction
                      y0, $                                 ;p(1) is the center pixel in the y direction
                      (PAguess-90D)*!DTOR, $                ;p(2) is the sky "position angle" of the major axis
                      SQRT(1 - (ellipseAxes[1]/ellipseAxes[0])^2), $ ;p(3) is the eccentricity (1.0 = straight line, 0 = cirele)
                      MAX(image), $                         ;p(4) is the central surface brightness
                      ellipseAxes[0]/12E, $                  ;p(5) is the disk scale length
                      1E, $                                 ;p(6) is the (1/n) factor in the Sersic profile
                      1E, $                                 ;p(7) is the background
                      [1E3,PSFparams[1:2],0]]               ;Append stellar PSF parameters
  p1 = parinfo.value
  
  ; Use the function ellipse to find the image pameters "p" to the Sersic profile
  PRINT_TEXT2, event, NEW_LINE() + 'Now fitting the SERSIC_DISK model'
  p1 = MPFIT2DFUN('SERSIC_DISK',x,y,image,$
    weights=weight,parinfo=parinfo,perror=perror1,bestnorm=bestnorm, /QUIET)

  arr       = SERSIC_DISK(x,y,p1)
  chi_nu1   = TOTAL(((image-arr)*weight)^2)/(TOTAL(weight GT 0) - TOTAL(~parinfo.fixed))
  chi_nuStr = SIG_FIG_STRING(chi_nu1, 3)
  PRINT_TEXT2, event, 'SERSIC_DISK model fit with reduced chi^2 = ' + chi_nuStr

  ;*** FIT EXP_RADIAL_SECH2_VERTICAL_DISK ***
  ; p(0) is the center pixel in the x direction
  ; p(1) is the center pixel in the y direction
  ; p(2) is the sky "position angle" (rotation angle) of the major axis (radians CCW from x-axis)
  ; p(3) is the central surface brightness
  ; p(4) is the disk radial scale length
  ; p(5) is the disk vertical scale length
  ; p(6) is the background
  ;

  parinfo = replicate({value:0.D, fixed:0,limited:[0,0],limits:[0.D,0.D],mpminstep:0.D},11)
  parinfo[0:6].limited = 1  ;Turn on limits
  parinfo[7:*].fixed   = 1  ;Fix the PSF values
  IF KEYWORD_SET(FIX_PA) THEN parinfo[2].fixed = 1          ;Fix the PA value
  ;
  parinfo[0:6].limits[0] = [x0-xrange, y0-yrange, -!dpi+0.0001, 0.1*max(image), $
    
                            1E-3*ellipseAxes[0], 1E-3*ellipseAxes[1], -3*skynoise] ;lower limits
                            
  parinfo[0:6].limits[1] = [x0+xrange, y0+yrange, +!dpi,        25.*max(image), $
    
                            max(re),               max(re),           +3*skynoise] ;upper limits
  ;
  ;Set initial guesses for image parameters
  parinfo[*].value = [x0, y0, (PAguess-90D)*!DTOR, max(image), $
    ellipseAxes[0]/12E, ellipseAxes[1]/12E, 1E, [1E3,PSFparams[1:2],0]]
  p2 = parinfo.value

  ; Use the function ellipse to find the image pameters "p" to the Sersic profile
  PRINT_TEXT2, event, NEW_LINE() + 'Now fitting the EXP_RADIAL_SECH2_VERTICAL_DISK model'
  p2 = MPFIT2DFUN('EXP_RADIAL_SECH2_VERTICAL_DISK',x,y,image,$
    weights=weight,parinfo=parinfo,perror=perror2,bestnorm=bestnorm, /QUIET)

  arr       = EXP_RADIAL_SECH2_VERTICAL_DISK(x,y,p2)
  chi_nu2   = TOTAL(((image-arr)*weight)^2)/(TOTAL(weight GT 0) - TOTAL(~parinfo.fixed))
  chi_nuStr = SIG_FIG_STRING(chi_nu2, 3)
  PRINT_TEXT2, event, 'EXP_RADIAL_SECH2_VERTICAL_DISK model fit with reduced chi^2 = ' + chi_nuStr

  
  ;*** FIT BULGE_AND_ONE_SERSIC_DISK ***
  ; p(0) is the center pixel in the x direction
  ; p(1) is the center pixel in the y direction
  ; p(2) is the sky "position angle" (rotation angle) of the major axis (radians CCW from x-axis)
  ; p(3) is the eccentricity of the Bulge (1.0 = straight line, 0 = cirele)
  ; p(4) is the central surface brightness of the bulge
  ; p(5) is the bulge scale length
  ; p(6) is the eccentricity of the Sersic disk (1.0 = straight line, 0 = cirele)
  ; p(7) is the central surface brightness of the disk
  ; p(8) is the disk scale length
  ; p(9) is the (1/n) factor in the Sersic profile for the disk
  ; p(10) is the background
  ;  
  
  parinfo               = replicate({value:0.D, fixed:0,limited:[0,0],limits:[0.D,0.D],mpminstep:0.D},15)
  parinfo[0:10].limited = 1  ;Turn on limits
  parinfo[11:*].fixed   = 1  ;Fix the PSF values
  IF KEYWORD_SET(FIX_PA) THEN parinfo[2].fixed = 1          ;Fix the PA value
  ;
  ;Set the lower limits for the fitting procedure
  parinfo[0:10].limits[0] = [x0-xrange, $                   ;p(0) is the center pixel in the x direction
                             y0-yrange, $                   ;p(1) is the center pixel in the y direction
                             -!dpi+0.0001, $                ;p(2) is the sky "position angle"  of the major axis
                             0E, $                          ;p(3) is the eccentricity of the Bulge (1.0 = straight line, 0 = cirele)
                             0.1*max(image), $              ;p(4) is the central surface brightness of the bulge
                             1E-3*ellipseAxes[1], $         ;p(5) is the bulge scale length
                             1E-4, $                        ;p(6) is the eccentricity of the Sersic disk
                             1E-3*max(image), $             ;p(7) is the central surface brightness of the disk
                             1E-3*ellipseAxes[0], $         ;p(8) is the disk scale length
                             1E-4, $                        ;p(9) is the (1/n) factor in the Sersic profile for the disk
                             -3*skynoise]                   ;p(10) is the background
  ;
  ;Set the upper limits for the fitting procedure                         
  parinfo[0:10].limits[1] = [x0+xrange, $                   ;p(0) is the center pixel in the x direction
                             y0+yrange, $                   ;p(1) is the center pixel in the y direction
                             +!dpi, $                       ;p(2) is the sky "position angle"  of the major axis
                             0.5, $                         ;p(3) is the eccentricity of the Bulge (1.0 = straight line, 0 = cirele)
                             25E*max(image), $              ;p(4) is the central surface brightness of the bulge
                             max(re), $                     ;p(5) is the bulge scale length
                             0.9999, $                      ;p(6) is the eccentricity of the Sersic disk
                             25E*max(image), $              ;p(7) is the central surface brightness of the disk
                             max(re), $                     ;p(8) is the disk scale length
                             10E, $                         ;p(9) is the (1/n) factor in the Sersic profile for the disk
                             +3*skynoise]                   ;p(10) is the background
  ;
  ;Set initial guesses for image parameters
  parinfo[*].value = [x0, $                                 ;p(0) is the center pixel in the x direction
                      y0, $                                 ;p(1) is the center pixel in the y direction
                      (PAguess-90D)*!DTOR, $                ;p(2) is the sky "position angle"  of the major axis
                      1E-4, $                               ;p(3) is the eccentricity of the Bulge (1.0 = straight line, 0 = cirele)
                      max(image), $                         ;p(4) is the central surface brightness of the bulge
                      ellipseAxes[1]/12E, $                  ;p(5) is the bulge scale length
                      SQRT(1 - (ellipseAxes[1]/ellipseAxes[0])^2), $ ;p(6) is the eccentricity of the Sersic disk
                      0.5*max(image), $                     ;p(7) is the central surface brightness of the disk
                      ellipseAxes[0]/12E, $                  ;p(8) is the disk scale length
                      0.5, $                                ;p(9) is the (1/n) factor in the Sersic profile for the disk
                      1E, $                                 ;p(10) is the background
                     [1E3,PSFparams[1:2],0]]                ;Append TWO_MASS_SERSIC_PSF parameters
  p3 = parinfo.value
  ; Use the function ellipse to find the image pameters "p" to the Sersic profile
  PRINT_TEXT2, event, NEW_LINE() + 'Now fitting the BULGE_AND_ONE_SERSIC_DISK model'
  p3 = MPFIT2DFUN('BULGE_AND_ONE_SERSIC_DISK',x,y,image,$
    weights=weight,parinfo=parinfo,perror=perror3,bestnorm=bestnorm, /QUIET)
  
  arr       = BULGE_AND_ONE_SERSIC_DISK(x,y,p3)
  chi_nu3   = TOTAL(((image-arr)*weight)^2)/(TOTAL(weight GT 0) - TOTAL(~parinfo.fixed))
  chi_nuStr = SIG_FIG_STRING(chi_nu3, 3)
  PRINT_TEXT2, event, 'BULGE_AND_ONE_SERSIC_DISK model fit with reduced chi^2 = ' + chi_nuStr


  ;*** FIT BULGE_AND_EXP_RADIAL_SECH2_VERTICAL_DISK ***
  ; p(0) is the center pixel in the x direction
  ; p(1) is the center pixel in the y direction
  ; p(2) is the sky "position angle" (rotation angle) of the major axis (radians CCW from x-axis)
  ; p(3) is the eccentricity of the Bulge (1.0 = straight line, 0 = cirele)
  ; p(4) is the central surface brightness of the bulge
  ; p(5) is the bulge scale length
  ; p(6) is the central surface brightness of the disk
  ; p(7) is the disk radial scale length
  ; p(8) is the disk vertical scale height
  ; p(9) is the background
  ;
  
  parinfo               = replicate({value:0.D, fixed:0,limited:[0,0],limits:[0.D,0.D],mpminstep:0.D},14)
  parinfo[0:9].limited  = 1  ;Turn on limits
  parinfo[10:*].fixed   = 1  ;Fix the PSF values
  IF KEYWORD_SET(FIX_PA) THEN parinfo[2].fixed = 1          ;Fix the PA value
  ;
  ;Set the lower limits for the fitting procedure
  parinfo[0:9].limits[0] = [x0-xrange, $                    ;p(0) is the center pixel in the x direction
                            y0-yrange, $                    ;p(1) is the center pixel in the y direction
                            -!dpi+0.0001, $                 ;p(2) is the sky "position angle" of the major axis
                            0E, $                           ;p(3) is the eccentricity of the Bulge (1.0 = straight line, 0 = cirele)
                            0.1*max(image), $               ;p(4) is the central surface brightness of the bulge
                            1E-3*ellipseAxes[1], $          ;p(5) is the bulge scale length
                            1E-3*max(image), $              ;p(6) is the central surface brightness of the disk
                            1E-3*ellipseAxes[0], $          ;p(7) is the disk radial scale length
                            1E-3*ellipseAxes[1], $          ;p(8) is the disk vertical scale height
                            -3*skynoise]                    ;p(9) is the background
  ;
  ;Set the upper limits for the fitting procedure
  parinfo[0:9].limits[1] = [x0+xrange, $                    ;p(0) is the center pixel in the x direction
                            y0+yrange, $                    ;p(1) is the center pixel in the y direction
                            +!dpi, $                        ;p(2) is the sky "position angle" of the major axis
                            0.5, $                          ;p(3) is the eccentricity of the Bulge (1.0 = straight line, 0 = cirele)
                            25.*max(image), $               ;p(4) is the central surface brightness of the bulge
                            max(re), $                      ;p(5) is the bulge scale length
                            25.*max(image), $               ;p(6) is the central surface brightness of the disk
                            max(re), $                      ;p(7) is the disk radial scale length
                            max(re), $                      ;p(8) is the disk vertical scale height
                            +3*skynoise]                    ;p(9) is the background
  ;
  ;Set initial guesses for image parameters
  parinfo[*].value = [x0, $                                 ;p(0) is the center pixel in the x direction
                      y0, $                                 ;p(1) is the center pixel in the y direction
                      (PAguess-90D)*!DTOR, $                ;p(2) is the sky "position angle" of the major axis
                      1E-4, $                               ;p(3) is the eccentricity of the Bulge (1.0 = straight line, 0 = cirele)
                      max(image), $                         ;p(4) is the central surface brightness of the bulge
                      ellipseAxes[1]/12E, $                  ;p(5) is the bulge scale length
                      0.5*max(image), $                     ;p(6) is the central surface brightness of the disk
                      ellipseAxes[0]/12E, $                  ;p(7) is the disk radial scale length
                      ellipseAxes[1]/12E, $                  ;p(8) is the disk vertical scale height
                      1E, $                                 ;p(9) is the background
                      [1E3,PSFparams[1:2],0]]               ;Append the TWO_MASS_SERSIC_PSF parameters
  p4 = parinfo.value

  ; Use the function ellipse to find the image pameters "p" to the Sersic profile
  PRINT_TEXT2, event, NEW_LINE() + 'Now fitting the BULGE_AND_EXP_RADIAL_SECH2_VERTICAL_DISK model'
  p4 = MPFIT2DFUN('BULGE_AND_EXP_RADIAL_SECH2_VERTICAL_DISK',x,y,image,$
    weights=weight,parinfo=parinfo,perror=perror4,bestnorm=bestnorm, /QUIET)
  
  arr       = BULGE_AND_EXP_RADIAL_SECH2_VERTICAL_DISK(x,y,p4)
  chi_nu4   = TOTAL(((image-arr)*weight)^2)/(TOTAL(weight GT 0) - TOTAL(~parinfo.fixed))
  chi_nuStr = SIG_FIG_STRING(chi_nu4, 3)
  PRINT_TEXT2, event, 'BULGE_AND_EXP_RADIAL_SECH2_VERTICAL_DISK model fit with reduced chi^2 = ' + chi_nuStr

;  ;*** FIT BULGE_AND_TWO_SERSIC_DISKS ***
;  ; p(0) is the center pixel in the x direction
;  ; p(1) is the center pixel in the y direction
;  ; p(2) is the sky "position angle" (rotation angle) of the major axis (radians CCW from x-axis)
;  ; p(3) is the eccentricity of the Bulge (1.0 = straight line, 0 = cirele)
;  ; p(4) is the central surface brightness of the bulge
;  ; p(5) is the bulge scale length
;  ; p(6) is the eccentricity of the thin Sersic disk (1.0 = straight line, 0 = cirele)
;  ; p(7) is the central surface brightness of the thin disk
;  ; p(8) is the thin disk scale length
;  ; p(9) is the (1/n) factor in the thin Sersic profile for the disk
;  ; p(10) is the eccentricity of the thick Sersic disk (1.0 = straight line, 0 = cirele)
;  ; p(11) is the central surface brightness of the thick disk
;  ; p(12) is the thick disk scale length
;  ; p(13) is the (1/n) factor in the thick Sersic profile for the disk
;  ; p(14) is the background
;  ;
;  
;  parinfo               = replicate({value:0.D, fixed:0,limited:[0,0],limits:[0.D,0.D],mpminstep:0.D},19)
;  parinfo[0:14].limited = 1  ;Turn on limits
;  parinfo[15:*].fixed   = 1  ;Fix the PSF values
;  ;
;  ;Set the lower limits for the fitting procedure
;  parinfo[0:14].limits[0] = [x0-xrange, $                   ;p(0) is the center pixel in the x direction
;                             y0-yrange, $                   ;p(1) is the center pixel in the y direction
;                             -!dpi+0.0001, $                ;p(2) is the sky "position angle" of the major axis (CCW from x-axis)
;                             0E, $                          ;p(3) is the eccentricity of the Bulge (1.0 = straight line, 0 = cirele)
;                             0.1*max(image), $              ;p(4) is the central surface brightness of the bulge
;                             1E-3*ellipseAxes[1], $         ;p(5) is the bulge scale length
;                             1E-4, $                        ;p(6) is the eccentricity of the thin Sersic disk
;                             1E-3*max(image), $             ;p(7) is the central surface brightness of the thin disk
;                             1E-3*ellipseAxes[0], $         ;p(8) is the thin disk scale length
;                             1E-4, $                        ;p(9) is the (1/n) factor in the thin Sersic profile for the disk
;                             1E-4, $                        ;p(10) is the eccentricity of the thick Sersic disk
;                             1E-3*max(image), $             ;p(11) is the central surface brightness of the thick disk
;                             1E-3*ellipseAxes[0], $         ;p(12) is the thick disk scale length
;                             1E-4, $                        ;p(13) is the (1/n) factor in the thick Sersic profile for the disk
;                             -3*skynoise]                   ;p(14) is the background
;  ;
;  ;Set the upper limits for the fitting procedure
;  parinfo[0:14].limits[1] = [x0+xrange, $                   ;p(0) is the center pixel in the x direction
;                             y0+yrange, $                   ;p(1) is the center pixel in the y direction
;                             +!dpi, $                       ;p(2) is the sky "position angle" of the major axis (CCW from x-axis)
;                             0.5, $                         ;p(3) is the eccentricity of the Bulge (1.0 = straight line, 0 = cirele)
;                             25.*max(image), $              ;p(4) is the central surface brightness of the bulge
;                             max(re), $                     ;p(5) is the bulge scale length
;                             0.9999, $                      ;p(6) is the eccentricity of the thin Sersic disk
;                             25.*max(image), $              ;p(7) is the central surface brightness of the thin disk
;                             max(re), $                     ;p(8) is the thin disk scale length
;                             10E, $                         ;p(9) is the (1/n) factor in the thin Sersic profile for the disk
;                             0.9999, $                      ;p(10) is the eccentricity of the thick Sersic disk
;                             25E*max(image), $              ;p(11) is the central surface brightness of the thick disk
;                             max(re), $                     ;p(12) is the thick disk scale length
;                             10E, $                         ;p(13) is the (1/n) factor in the thick Sersic profile for the disk
;                             +3*skynoise]                   ;p(14) is the background
;  ;
;  ;Set initial guesses for model parameters
;  parinfo[*].value = [x0, $                                 ;p(0) is the center pixel in the x direction
;                      y0, $                                 ;p(1) is the center pixel in the y direction
;                      (PAguess-90D)*!DTOR, $                ;p(2) is the sky "position angle" of the major axis (CCW from x-axis)
;                      1E-4, $                               ;p(3) is the eccentricity of the Bulge (1.0 = straight line, 0 = cirele)
;                      max(image), $                         ;p(4) is the central surface brightness of the bulge
;                      ellipseAxes[1]/6E, $                  ;p(5) is the bulge scale length
;                      SQRT(1 - (ellipseAxes[1]/ellipseAxes[0])^2), $;p(6) is the eccentricity of the thin Sersic disk
;                      0.5*max(image), $                     ;p(7) is the central surface brightness of the thin disk
;                      ellipseAxes[0]/10E, $                 ;p(8) is the thin disk scale length
;                      0.5, $                                ;p(9) is the (1/n) factor in the thin Sersic profile for the disk
;                      0.75*SQRT(1 - (ellipseAxes[1]/ellipseAxes[0])^2), $;p(10) is the eccentricity of the thick Sersic disk
;                      0.25*max(image), $                    ;p(11) is the central surface brightness of the thick disk
;                      ellipseAxes[0]/3E, $                  ;p(12) is the thick disk scale length
;                      0.5, $                                ;p(13) is the (1/n) factor in the thick Sersic profile for the disk
;                      1E, $                                 ;p(14) is the background
;                      [1E3,PSFparams[1:2],0]]               ;append the TWO_MASS_SERSIC_PSF parameters
;  ;
;  p5 = parinfo.value
;  ; Use the function ellipse to find the image pameters "p" to the Sersic profile
;  PRINT_TEXT2, event, NEW_LINE() + 'Now fitting the BULGE_AND_TWO_SERSIC_DISKS model'
;  p5 = MPFIT2DFUN('BULGE_AND_TWO_SERSIC_DISKS',x,y,image,$
;    weights=weight,parinfo=parinfo,perror=perror5,bestnorm=bestnorm, /QUIET)
;  
;  arr       = BULGE_AND_TWO_SERSIC_DISKS(x,y,p5)
;  chi_nu5   = TOTAL(((image-arr)*weight)^2)/(TOTAL(weight GT 0) - TOTAL(~parinfo.fixed))
;  chi_nuStr = SIG_FIG_STRING(chi_nu5, 3)
;  PRINT_TEXT2, event, 'BULGE_AND_TWO_SERSIC_DISKS model fit with reduced chi^2 = ' + chi_nuStr

  
  ;Find the lowest reduced chi^2 value
  chi_nu  = [chi_nu1, chi_nu2, chi_nu3, chi_nu4];, chi_nu5]
  bestFit = WHERE(chi_nu EQ MIN(chi_nu))
  
  modelNames         = ['SERSIC_DISK', $
                        'EXP_RADIAL_SECH2_VERTICAL_DISK', $
                        'BULGE_AND_ONE_SERSIC_DISK', $
                        'BULGE_AND_EXP_RADIAL_SECH2_VERTICAL_DISK']
  numParams          = [8, 7, 11, 10, 14] + 4
  parameterStructure = {bestFit:(modelNames[bestfit])[0], $
                        parameters:DBLARR(numParams[bestFit]), $
                        parameterErrors:DBLARR(numParams[bestFit])}
  
  CASE bestFit OF
    0: BEGIN                                                ;SERSIC_DISK
      parameterStructure.parameters      = p1;[0:7]
      parameterStructure.parameterErrors = perror1;[0:7]
    END
    1: BEGIN                                                ;EXP_RADIAL_SECH2_VERTICAL_DISK
      parameterStructure.parameters      = p2;[0:6]
      parameterStructure.parameterErrors = perror2;[0:6]
    END
    2: BEGIN                                                ;BULGE_AND_ONE_SERSIC_DISK
      parameterStructure.parameters      = p3;[0:10]
      parameterStructure.parameterErrors = perror3;[0:10]
    END
    3: BEGIN                                                ;BULGE_AND_EXP_RADIAL_SECH2_VERTICAL_DISK
      parameterStructure.parameters      = p4;[0:9]
      parameterStructure.parameterErrors = perror4;[0:9]
    END
;    4: BEGIN                                                ;BULGE_AND_TWO_SERSIC_DISKS
;      parameterStructure.parameters      = p5[0:14]
;      parameterStructure.parameterErrors = perror5[0:14]
;    END
  ENDCASE

  RETURN, parameterStructure
  
END