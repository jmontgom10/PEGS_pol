FUNCTION  SERSIC_DISK, x,y,p

  ;This function returns an image of an exponential model galaxy
  ;with a disk length scale and a scale height and vertical disk scale height.
  ;The model galaxy has its major axis oriented along the x-axis
  ;and its minor axis oriented along the y axis.
  
  ;Sersic profile parameters
  ; p(0) is the center pixel in the x direction
  ; p(1) is the center pixel in the y direction
  ; p(2) is the sky "position angle" (rotation angle) of the major axis (radians CCW from x-axis)
  ; p(3) is the eccentricity of the Sersic disk (1.0 = straight line, 0 = circle)
  ; p(4) is the central surface brightness
  ; p(5) is the disk scale length
  ; p(6) is the (1/n) factor in the Sersic profile
  ; p(7) is the background
  ; p(8) is the peak of the PSF
  ; p(9) is the scale length of the PSF
  ; p(10) is the sersic index
  ; p(11) is the background level of the PSF
  ;
  
  
  ;First, compute the rotated, eccentricity normalized radial distances from the galaxy center
  r   = (((x-p[0])*cos(p[2])+(y-p[1])*sin(p[2]))^2.0 $
    + ((x-p[0])*sin(p[2])-(y-p[1])*cos(p[2]))^2.0 / (1.-p[3]^2.0))^0.5
    
  ;Now compute the model galaxy brightness at each point in the image
  modelImage = p[4] * exp(-(r/p[5])^p[6]) + p[7]
   
  IF N_ELEMENTS(p) LT 12 THEN BEGIN
    ;Generate a simple Gaussian PSF
    xn2 = 7
    yn2 = 7
    xr2 = dindgen(xn2)
    yc2 = dindgen(yn2)
    x2  = (xr2#(yc2*0+1))                                 ;x positions of all pixels in PSF image
    y2  = ((xr2*0+1)#yc2)                                 ;y positions of all pixels in PSF image
    rc2 = double(sqrt((x2-3.)^2.+(y2-3.)^2.))             ;distance from center pixel of PSF image
    b   = double(exp(-(rc2)^2./(1.61^2.*2.)))             ;PSF image
  ENDIF ELSE BEGIN
    xn2 = 15
    yn2 = 15
    xr2 = dindgen(xn2)
    yc2 = dindgen(yn2)
    x2  = (xr2#(yc2*0+1)) - 7                             ;x positions of all pixels in PSF image
    y2  = ((xr2*0+1)#yc2) - 7                             ;y positions of all pixels in PSF image
    b   = TWO_MASS_SERSIC_PSF(x2, y2, p[8:*])
  ENDELSE
  convolImage = CONVOL(modelImage,b,total(b), /EDGE_TRUNCATE);Convolve the model galaxy with a model PSF to simulate seeing
  
  RETURN, convolImage
  
END


FUNCTION  EXP_RADIAL_SECH2_VERTICAL_DISK, x,y,p

  ;This function returns an image of an exponential model galaxy
  ;with a disk length scale and a scale height and vertical disk scale height.
  ;The model galaxy has its major axis oriented along the x-axis
  ;and its minor axis oriented along the y axis.
  ;This function only plots one QUADRANT of the galaxy with the galaxy center at (x, y) = (0, 0)
  ;
  ;The model parameters are stored in the argument "p" and have the following meaning
  ; p(0) is the center pixel in the x direction
  ; p(1) is the center pixel in the y direction
  ; p(2) is the sky "position angle" (rotation angle) of the major axis (radians CCW from x-axis)
  ; p(3) is the central surface brightness
  ; p(4) is the disk radial scale length
  ; p(5) is the disk vertical scale length
  ; p(6) is the background
  ;
  
  
  ;First, compute the galaxy-frame coordinates
  ROT_AXES, (x-p[0]), (y-p[1]), p[2], x1, y1
  
  ;Now compute the model galaxy brightness at each point in the image
  modelImage  = p[3]*EXP(-ABS(x1)/p[4])
  modelImage *= (1/COSH(y1/p[5]))^2E                        ;This is the hyperbolic secant function (squared)
  modelImage += p[6]
  
  ;Generate a simple Gaussian PSF
  IF N_ELEMENTS(p) LT 11 THEN BEGIN
    xn2 = 7
    yn2 = 7
    xr2 = dindgen(xn2)
    yc2 = dindgen(yn2)
    x2  = (xr2#(yc2*0+1))                                   ;x positions of all pixels in PSF image
    y2  = ((xr2*0+1)#yc2)                                   ;y positions of all pixels in PSF image
    rc2 = double(sqrt((x2-3.)^2.+(y2-3.)^2.))               ;distance from center pixel of PSF image
    b   = double(exp(-(rc2)^2./(1.61^2.*2.)))               ;PSF image
  ENDIF ELSE BEGIN
    xn2 = 15
    yn2 = 15
    xr2 = dindgen(xn2)
    yc2 = dindgen(yn2)
    x2  = (xr2#(yc2*0+1)) - 7                               ;x positions of all pixels in PSF image
    y2  = ((xr2*0+1)#yc2) - 7                               ;y positions of all pixels in PSF image
    b   = TWO_MASS_SERSIC_PSF(x2, y2, p[7:*])
  ENDELSE
  convolImage = convol(modelImage,b,total(b),/EDGE_TRUNCATE);convolve the model galaxy with the PSF to model seeing
  
  RETURN, convolImage
  
END

;; NOTE THE FOLLOWING!!!
; From "STEEP VERTICAL STELLAR DISTRIBUTION" by BANERJEE & JOG (2007)
;
;If luminosity is distributed as
;L(R,z) = L0 *  (sech(n*z/(2*Z_e)))^(2/n) * exp(-R/h)
;
;Then surface brightness is distributed as
;I(R,z) = I(0, 0) * (R/h) * K1(R/h) * (sech(nz/2ze))^(2/n),
;where I(0,0) = 2*L0*h and K1 is the modified Bessel function.
;
;
;Therefore, there should be a model of surface brightness
;using a "Modified Bessel function" along the major axis
;and a "(Sech^2(n*z/(2*Z_e)))^(2/n)" function along the minor axis
;


FUNCTION BULGE_AND_ONE_SERSIC_DISK, x,y,p

  ;The program fits a de Vaucouleurs profile (r^(1/4)) to the bulge and a Sersic profile to the disk.
  ;The parameters used are:
  ;
  ; r_e  : The bulge scale length.
  ; r_s  : The disk scale length.
  ; D/B  : The disk-to-bulge ratio.
  ; P/B  : The ratio of the luminosity of the point source to the bulge luminosity.
  ;        As stated earlier, for the sample, P/B was always fixed at zero.
  ; Sig_e: The central magnitude.
  ; e_b  : The bulge ellipticity.
  ; e_d  : The disk ellipticity.
  ;
  ;The bulge and disk ellipticities are invisible parameters and the D/B ratio appears as the relative luminosity of the disk and the bulge.
  ;
  ;One can specify the range within which a specific parameter is to vary and the step size.
  ;Alternately, one can hold a parameter fixed at a particular value if that value is known.
  ;If, for instance, one knows that a galaxy does not have a disk component, D/B for that galaxy can be fixed at zero.
  ;
  ; disk-to-bulge ratio
  ; bulge-to-total light ratio
  ; D/B = 0.28*(r_s/r_e)^2 * (Sig_s/Sig_e)
  ; B/T = 1/(D/B + 1)
  ;
  
  ;The model parameters are stored in the argument "p" and have the following meaning
  ; p(0) is the center pixel in the x direction
  ; p(1) is the center pixel in the y direction
  ; p(2) is the sky "position angle" (rotation angle) of the major axis (radians CCW from x-axis)
  ; p(3) is the eccentricity of the Bulge (1.0 = straight line, 0 = circle)
  ; p(4) is the central surface brightness of the bulge
  ; p(5) is the bulge scale length
  ; p(6) is the eccentricity of the Sersic disk (1.0 = straight line, 0 = circle)
  ; p(7) is the central surface brightness of the disk
  ; p(8) is the disk scale length
  ; p(9) is the (1/n) factor in the Sersic profile for the disk
  ; p(10) is the background
  ;
  
  
  ;Bulge: Compute the rotated, bulge eccentricity normalized radial distances from the galaxy center
  rBulge     = (((x-p[0])*cos(p[2])+(y-p[1])*sin(p[2]))^2.0 $
    + ((x-p[0])*sin(p[2])-(y-p[1])*cos(p[2]))^2.0 / (1.-p[3]^2.0))^0.5
    
  ;Create a de Vaucouleurs bulge
  modelBulge =  p[4]*EXP(-(rBulge/p[5])^0.25D)
  
  ;Disk: Compute the rotated, disk eccentricity normalized radial distances from the galaxy center
  rDisk      = (((x-p[0])*cos(p[2])+(y-p[1])*sin(p[2]))^2.0 $
    + ((x-p[0])*sin(p[2])-(y-p[1])*cos(p[2]))^2.0 / (1.-p[6]^2.0))^0.5
    
  ;Create a Sersic disk
  modelDisk = p[7]*EXP(-(rDisk/p[8])^p[9])
  
  ;The final model image is simply the superposition of the two components
  modelImage = modelBulge + modelDisk + p[10]
  
  IF N_ELEMENTS(p) LT 15 THEN BEGIN
    xn2 = 7
    yn2 = 7
    xr2 = dindgen(xn2)
    yc2 = dindgen(yn2)
    x2  = (xr2#(yc2*0+1))                                   ;x positions of all pixels in PSF image
    y2  = ((xr2*0+1)#yc2)                                   ;y positions of all pixels in PSF image
    rc2 = double(sqrt((x2-3.)^2.+(y2-3.)^2.))               ;distance from center pixel of PSF image
    b   = double(exp(-(rc2)^2./(1.61^2.*2.)))               ;PSF image
  ENDIF ELSE BEGIN
    xn2 = 15
    yn2 = 15
    xr2 = dindgen(xn2)
    yc2 = dindgen(yn2)
    x2  = (xr2#(yc2*0+1)) - 7                               ;x positions of all pixels in PSF image
    y2  = ((xr2*0+1)#yc2) - 7                               ;y positions of all pixels in PSF image
    b   = TWO_MASS_SERSIC_PSF(x2, y2, p[11:*])
  ENDELSE
  convolImage = convol(modelImage,b,total(b), /EDGE_TRUNCATE);convolve the model galaxy with the PSF to model seeing

  RETURN, convolImage
  
END


FUNCTION BULGE_AND_EXP_RADIAL_SECH2_VERTICAL_DISK, x,y,p

  ;The model parameters are stored in the argument "p" and have the following meaning
  ; p(0) is the center pixel in the x direction
  ; p(1) is the center pixel in the y direction
  ; p(2) is the sky "position angle" (rotation angle) of the major axis (radians CCW from x-axis)
  ; p(3) is the eccentricity of the Bulge (1.0 = straight line, 0 = circle)
  ; p(4) is the central surface brightness of the bulge
  ; p(5) is the bulge scale length
  ; p(6) is the central surface brightness of the disk
  ; p(7) is the disk radial scale length
  ; p(8) is the disk vertical scale height
  ; p(9) is the background
  ;
  
  ;Bulge: Compute the rotated, bulge eccentricity normalized radial distances from the galaxy center
  rBulge     = (((x-p[0])*cos(p[2])+(y-p[1])*sin(p[2]))^2.0 $
              + ((x-p[0])*sin(p[2])-(y-p[1])*cos(p[2]))^2.0 / (1.-p[3]^2.0))^0.5
    
  ;Create a de Vaucouleurs bulge
  modelBulge =  p[4]*EXP(-(rBulge/p[5])^0.25)
  
  ;First, compute the galaxy-frame coordinates
  ROT_AXES, (x-p[0]), (y-p[1]), p[2], x1, y1
  
  ;Now compute the model galaxy brightness at each point in the image
  modelDisk  = p[6]*EXP(-ABS(x1)/p[7])*(1/COSH(y1/p[8]))^2E
  
  modelImage = modelBulge + modelDisk + p[9]
  
  IF N_ELEMENTS(p) LT 14 THEN BEGIN
    xn2 = 7
    yn2 = 7
    xr2 = dindgen(xn2)
    yc2 = dindgen(yn2)
    x2  = (xr2#(yc2*0+1))                                   ;x positions of all pixels in PSF image
    y2  = ((xr2*0+1)#yc2)                                   ;y positions of all pixels in PSF image
    rc2 = double(sqrt((x2-3.)^2.+(y2-3.)^2.))               ;distance from center pixel of PSF image
    b   = double(exp(-(rc2)^2./(1.61^2.*2.)))               ;PSF image
  ENDIF ELSE BEGIN
    xn2 = 15
    yn2 = 15
    xr2 = dindgen(xn2)
    yc2 = dindgen(yn2)
    x2  = (xr2#(yc2*0+1)) - 7                   ;x positions of all pixels in PSF image
    y2  = ((xr2*0+1)#yc2) - 7                   ;y positions of all pixels in PSF image
    b   = TWO_MASS_SERSIC_PSF(x2, y2, p[10:*])
  ENDELSE
  
  convolImage = convol(modelImage,b,total(b), /EDGE_TRUNCATE);convolve the model galaxy with the PSF to model seeing
  RETURN, convolImage
  
END


FUNCTION BULGE_AND_TWO_SERSIC_DISKS, x,y,p

  ;The program fits a de Vaucouleurs profile (r^(1/4)) to the bulge and a Sersic profile to the disk.
  ;The parameters used are:
  ;
  ; r_e  : The bulge scale length.
  ; r_s  : The disk scale length.
  ; D/B  : The disk-to-bulge ratio.
  ; P/B  : The ratio of the luminosity of the point source to the bulge luminosity. As stated earlier, for the sample, P/B was always fixed at zero.
  ; Sig_e: The central magnitude.
  ; e_b  : The bulge ellipticity.
  ; e_d  : The disk ellipticity.
  ;
  ;The bulge and disk ellipticities are invisible parameters and the D/B ratio appears as the relative luminosity of the disk and the bulge.
  ;
  ;One can specify the range within which a specific parameter is to vary and the step size. Alternately, one can hold a parameter fixed at a particular value if that value is known.
  ;If, for instance, one knows that a galaxy does not have a disk component, D/B for that galaxy can be fixed at zero.
  ;
  ; disk-to-bulge ratio
  ; bulge-to-total light ratio
  ; D/B = 0.28*(r_s/r_e)^2 * (Sig_s/Sig_e)
  ; B/T = 1/(D/B + 1)
  ;
  
  ;The model parameters are stored in the argument "p" and have the following meaning
  ; p(0) is the center pixel in the x direction
  ; p(1) is the center pixel in the y direction
  ; p(2) is the sky "position angle" (rotation angle) of the major axis (radians CCW from x-axis)
  ; p(3) is the eccentricity of the Bulge (1.0 = straight line, 0 = circle)
  ; p(4) is the central surface brightness of the bulge
  ; p(5) is the bulge scale length
  ; p(6) is the eccentricity of the thin Sersic disk (1.0 = straight line, 0 = circle)
  ; p(7) is the central surface brightness of the thin disk
  ; p(8) is the thin disk scale length
  ; p(9) is the (1/n) factor in the thin Sersic profile for the disk
  ; p(10) is the eccentricity of the thick Sersic disk (1.0 = straight line, 0 = circle)
  ; p(11) is the central surface brightness of the thick disk
  ; p(12) is the thick disk scale length
  ; p(13) is the (1/n) factor in the thick Sersic profile for the disk
  ; p(14) is the background
  ;
  
  
  ;Bulge: Compute the rotated, bulge eccentricity normalized radial distances from the galaxy center
  rBulge     = (((x-p[0])*cos(p[2])+(y-p[1])*sin(p[2]))^2.0 $
    + ((x-p[0])*sin(p[2])-(y-p[1])*cos(p[2]))^2.0 / (1.-p[3]^2.0))^0.5
    
  ;Create a de Vaucouleurs bulge
  modelBulge =  p[4]*EXP(-(rBulge/p[5])^0.25D)
  
  ;Disk1: Compute the rotated, disk eccentricity normalized radial distances from the galaxy center
  rDisk1     = (((x-p[0])*cos(p[2])+(y-p[1])*sin(p[2]))^2.0 $
    + ((x-p[0])*sin(p[2])-(y-p[1])*cos(p[2]))^2.0 / (1.-p[6]^2.0))^0.5
    
  ;Create a thin Sersic disk
  modelDisk1 = p[7]*EXP(-(rDisk1/p[8])^p[9])
  
  ;Disk2: Compute the rotate, disk eccentricity normalized radias distances from the galaxy center
  rDisk2     = (((x-p[0])*cos(p[2])+(y-p[1])*sin(p[2]))^2.0 $
    + ((x-p[0])*sin(p[2])-(y-p[1])*cos(p[2]))^2.0 / (1.-p[10]^2.0))^0.5
    
  ;Create a thin Sersic disk
  modelDisk2 = p[11]*EXP(-(rDisk2/p[12])^p[13])
  
  ;The final model image is simply the superposition of the two components
  modelImage = modelBulge + modelDisk1 + modelDisk2 + p[14]

  IF N_ELEMENTS(p) LT 15 THEN BEGIN
    xn2 = 7
    yn2 = 7
    xr2 = dindgen(xn2)
    yc2 = dindgen(yn2)
    x2  = (xr2#(yc2*0+1))                                   ;x positions of all pixels in PSF image
    y2  = ((xr2*0+1)#yc2)                                   ;y positions of all pixels in PSF image
    rc2 = double(sqrt((x2-3.)^2.+(y2-3.)^2.))               ;distance from center pixel of PSF image
    b   = double(exp(-(rc2)^2./(1.61^2.*2.)))               ;PSF image
  ENDIF ELSE BEGIN
    xn2 = 15
    yn2 = 15
    xr2 = dindgen(xn2)
    yc2 = dindgen(yn2)
    x2  = (xr2#(yc2*0+1)) - 7                               ;x positions of all pixels in PSF image
    y2  = ((xr2*0+1)#yc2) - 7                               ;y positions of all pixels in PSF image
    b   = TWO_MASS_SERSIC_PSF(x2, y2, p[15:*])
  ENDELSE
  convolImage = convol(modelImage,b,total(b), /EDGE_TRUNCATE);convolve the model galaxy with the PSF to model seeing
  RETURN, convolImage
  
END