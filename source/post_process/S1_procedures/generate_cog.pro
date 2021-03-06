FUNCTION KING_MODEL, kingParams, r

  ;This program computes the value of the king model at the requested radius, "r",
  ;using the specified parameters "kingParams"

  ;Disambiguate the King mmodel parameters
  Ri = kingParams[0]                                                  ;Gaussian standard-deviation width
  A  = kingParams[1]                                                  ;Moffat denominator exponent
  B  = kingParams[2]                                                  ;Weighting for the Moffat component
  C  = kingParams[3]                                                  ;Weighting for the Gaussian (and exponential component)
  D  = kingParams[4]                                                  ;Scale radius for the exponential component
  
  gaussParams = [1.0/(2*!PI*Ri^2), 0, Ri, 0]                          ;Define the Gaussian component of the King model
  G = GAUSSIAN(r,  gaussParams)                                       ;Compute the Gaussian component of the King model
  M = (A-1.0)/!PI * (1.0 + r^2)^(-A)                                  ;Compute the Moffat component of the King model
  H = EXP(-r/(D*Ri)^2)/(2.0*!PI*D*Ri)                                 ;Compute the exponential component of the King model
  kingProfile = B*M + (1.0-B)*(C*G + (1.0-C)*H)                       ;Weight and sum each component of the King model
  
  RETURN, kingProfile
END


FUNCTION INTEGRATED_KING_MODEL, kingParams, rLimits

  ;This program computes the definite integral of the King model between the 

  ;Disambiguate the King mmodel parameters
  Ri = kingParams[0]                                                  ;Gaussian standard-deviation width
  A  = kingParams[1]                                                  ;Moffat denominator exponent
  B  = kingParams[2]                                                  ;Weighting for the Moffat component
  C  = kingParams[3]                                                  ;Weighting for the Gaussian (and exponential component)
  D  = kingParams[4]                                                  ;Scale radius for the exponential component
  
  ;Because the king model is a linear combination of several independent terms,
  ;the area under its curve can be computed as the sum of the integral of each term.
  
  intMoffat = (1 + rLimits[0]^2)^(-A) - (1 + rLimits[1]^2)^(-A)         ;Area under the Moffat component
  intGauss  = EXP(-rLimits[0]^2/(2*Ri^2)) - EXP(-rLimits[1]^2/(2*Ri^2)) ;Area under the Gaussian component
  intExpon  = EXP(-rLimits[0]/(D*Ri))*(rLimits[0]/D*Ri + 1) - $         ;Area under the exponential component
              EXP(-rLimits[1]/(D*Ri))*(rLimits[1]/D*Ri + 1)
  
  integratedKing = B*intMoffat + (1.0-B)*(C*intGauss + (1.0-C)*intExpon);Total area under the King model curve

  RETURN, integratedKing
END

FUNCTION COG_RESIDUALS, p, r0=r0, radii=radii, delMags=delMags, ERR=err

  ;This program computes the residual values between
  ;the King model curve-of-growth (COG)
  ;and the observed incremental magnitude difference (delMags)

  Nradii  = N_ELEMENTS(radii)                                         ;Count the number of radii used for the COG
  kingCOG = FLTARR(Nradii)                                            ;Create an array with one element for each radius
  FOR i = 0, Nradii - 1 DO BEGIN
    numerLimits = [0,radii[i]]                                        ;Store the annular radius limits for the numerator
    IF i EQ 0 THEN $
      denomLimits = [0,r0] ELSE $                                     ;Store the annular radius limits for the denominator
      denomLimits = [0,radii[i-1]]
    numerator   = INTEGRATED_KING_MODEL(p, numerLimits)               ;Compute the numerator value for the integrated flux
    denominator = INTEGRATED_KING_MODEL(p, denomLimits)               ;Compute the denominator value for the integrated flux
    kingCOG[i]  = -2.5*ALOG10(numerator/denominator)                  ;Convert the integrated King model flux into magnitudes
  ENDFOR
  COGresiduals = (delMags - kingCOG)/err                              ;Report the residuals as uncertainty normalized differences

  RETURN, COGresiduals
END

FUNCTION COG_APER, image, xStars, yStars, apr, skyradii, badpix
  ;This program simply retrieves the MEDIAN-FILTERED-MEAN of the aperture
  ;photometry for the specified values in 'apr'. These filtered values
  ;are returned to be used in the 'GENERATE_COG' program for actually
  ;fitting the curve-of-growth.
  ;DEPENDS ON: APER, MEDIAN_FILTERED_MEAN
  ;
  phpadu   = 8.21                                                     ;This value can be found on Mimir website
  ronois   = 17.8                                                     ;(elec) This value is from Mimir website
  ;  ronois   = 3.1                                                     ;(ADU) This value is from GPIPS code "S4_PSF_fit"
  ;  badpix   = [-300L, 6000L]
  
  APER, image, xStars, yStars, $
    mags, errap, sky, skyerr, phpadu, apr, skyradii, badpix, /SILENT
    
  ;Compute the magnitude differences and the uncertainty in that value
  Napr         = N_ELEMENTS(apr)
  deltaMags    = mags[1:*,*] - mags[0:(Napr - 2),*]
  sigDeltaMags = SQRT(errap[1:*,*]^2 + errap[0:(Napr - 2),*]^2)
  
  ;Compute the median-filtered-mean for each aperture
  Nstars  = N_ELEMENTS(xStars)
  medians = FLTARR(Napr - 1)
  errs    = FLTARR(Napr - 1)
  FOR i = 0, Napr - 2 DO BEGIN
    result = MEDIAN_FILTERED_MEAN(REFORM(deltaMags[i,*]))
    medians[i] = result[0]
    errs[i]    = result[1]
  ENDFOR
  
  ;Identify outliers in the deltaMag data
  mfmVals = REBIN(medians, Napr - 1, Nstars, /SAMPLE)
  mfmSigs = REBIN(errs, Napr - 1, Nstars, /SAMPLE)
  badInds = WHERE(ABS(deltaMags - mfmVals)/mfmSigs GT 3.0, numBad)
  IF numBad GT 0 THEN deltaMags[badInds] = !VALUES.F_NAN
  
  FOR i = 0, Napr - 2 DO BEGIN
    goodInds   = WHERE(FINITE(deltaMags[i,*]), numGood)
    IF numGood GT 0 THEN BEGIN
      medians[i] = MEDIAN(deltaMags[i,goodInds])
      errs[i]    = STDDEV(deltaMags[i,goodInds])
    ENDIF ELSE STOP
  ENDFOR
  
;  ;Compute a weighted mean value for each aperture (ignoring NaN values)
;  wts              = 1.0/(100*sigDeltaMags)^2
;  sigMeanDeltaMags = SQRT(TOTAL(FINITE(deltaMags), 2))/(TOTAL(wts, 2))
;  meanDeltaMags    = sigMeanDeltaMags*TOTAL(wts*deltaMags, 2, /NAN)
  
  ;Build the structure to be returned
  COGphotometry = {$
    deltaMags:medians, $
    sigDeltaMags:errs}

  RETURN, COGphotometry
END

FUNCTION GENERATE_COG, image, xStars, yStars, apr, skyradii, badpix
  ;This program uses a least squares minimization method (via the mpfit.pro package)
  ;to determine the best fitting King model parameters
  ;based on the stars in the image.
  ;
  ;DEPENDS ON: APER, MEANCLIP, MPFIT, 
  ;            COG_RESIDUALS, INTEGRATED_KING_MODEL, KING_MODEL

  COGphotometry = COG_APER(image, xStars, yStars, apr, skyradii, badpix)
 
  ;Starting parameters taken from Stetson (1990)
  start_params = [1.5, 1.2, 0.150, 0.5, 0.9]

  ;Initalize a parameter structure
  numPars      = N_ELEMENTS(start_params)
  parinfo      = REPLICATE({value:0, limited:[0,0], limits:[0.0,0.0], mpside:0, fixed:0}, numPars)

  ;Create a structure to store COG residual function arguments
  functargs    = {$
    r0:apr[0],$
    radii:apr[1:*], $
    delMags:COGphotometry.deltaMags,$
    ERR:COGphotometry.sigDeltaMags}

  ;Define which parameters are limited and what those limits are
  parinfo[0].limited = [1,1]
  parinfo[0].limits  = [0.5, 5.0]
  parinfo[1].limited = [1,0]
  parinfo[1].limits  = [1.0,10.0]
  parinfo[2].limited = [1,0]
  parinfo[2].limits  = [0.0,10.0]
  parinfo[3].limited = [1,0]
  parinfo[3].limits  = [0.0,1.0]
  parinfo[4].limited = [1,0]
  parinfo[4].limits  = [1E-3,10.0]
  
  ;Compute chi-squared minimization parameter values
  kingParams  = MPFIT('COG_RESIDUALS', start_params, FUNCTARGS=functargs, $
    PARINF=parinfo, STATUS=status, ERRMSG=errmsg, /QUIET)

  RETURN, kingParams

END