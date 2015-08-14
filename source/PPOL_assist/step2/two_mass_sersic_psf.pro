FUNCTION  TWO_MASS_SERSIC_PSF, xx, yy, p

  ;This function returns an image of an exponential model galaxy
  ;with a disk length scale and a scale height and vertical disk scale height.
  ;The model galaxy has its major axis oriented along the x-axis
  ;and its minor axis oriented along the y axis.
  
  ;Sersic profile parameters
  ;
  ; p(0) is the central surface brightness
  ; p(1) is the PSF scale length
  ; p(2) is the (1/n) factor in the Sersic profile
  ; p(3) is the background
  ;
  
  r = SQRT(xx^2 + yy^2)
  
  ;Now compute the model galaxy brightness at each point in the image
  modelImage = p[0] * EXP(-(r/p[1])^p[2]) + p[3]
  RETURN, modelImage
  
END