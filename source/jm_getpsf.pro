pro jm_getpsf,image,xc,yc,apmag,sky,ronois,phpadu, gauss,psf, fwhm, idpsf,psfrad, $
  fitrad, x_w_psf, y_w_psf, psfmag, sum_resid, resid, $
  psfname, NO_RESID = no_resid, DEBUG=debug
  ;
  ; NO_RESID if set turns off residuals - gaussian PSF only
  ;+
  ; NAME:
  ; GETPSF
  ; PURPOSE:
  ; To generate a point-spread function (PSF) from observed stars.
  ; EXPLANATION:
  ; The PSF is represented as a 2-dimensional Gaussian
  ; (integrated over each pixel) and a lookup table of residuals.
  ; The lookup table and Gaussian parameters are output in a FITS
  ; image file.   The PSF FITS file created by GETPSF can be
  ; read with the procedure RDPSF.      Adapted from the 1986 STSDAS
  ; version of DAOPHOT
  ;
  ; CALLING SEQUENCE:
  ; GETPSF, image, xc, yc, apmag, sky, [ronois, phpadu, gauss, psf,
  ;     idpsf, psfrad, fitrad, psfname, /DEBUG ]
  ;
  ; INPUTS:
  ; IMAGE  - input image array
  ; XC     - input vector of x coordinates (from FIND), these should be
  ;   IDL (first pixel is (0,0)) convention.
  ; YC     - input vector of y coordinates (from FIND)
  ; APMAG  - vector of magnitudes (from APER), used for initial estimate
  ;   of gaussian intensity.  If APMAG is multidimensional, (more
  ;   than 1 aperture was used in APER) then the first aperture
  ;   is used.
  ; SKY    - vector of sky values (from APER)
  ;
  ; OPTIONAL INPUTS:
  ; The user will be prompted for the following parameters if not supplied.
  ;
  ; RONOIS - readout noise per pixel, (in electrons, or equivalent photons)
  ; PHPADU - photons per analog digital unit, used to scale the data
  ;   numbers in IMAGE into photon units
  ; IDPSF  - subscripts of the list of stars created by
  ;   APER which will be used to define the PSF.   Stars whose
  ;   centroid does not fall within PSFRAD of the edge of the frame,
  ;   or for which a Gaussian fit requires more than 25 iterations,
  ;   will be ignored when creating the final PSF.
  ; PSFRAD - the scalar radius, in pixels, of the circular area within
  ;   which the PSF will be defined.   This should be slightly larger
  ;   than the radius of the brightest star that one will be
  ;   interested in.
  ; FITRAD - the scalar radius, in pixels of the circular area used in the
  ;   least-square star fits.  Stetson suggest that FITRAD should
  ;   approximately equal to the FWHM, slightly less for crowded
  ;   fields.  (FITRAD must be smaller than PSFRAD.)
  ; PSFNAME- Name of the FITS file that will contain the table of residuals,
  ;   and the best-fit Gaussian parameters.    This file is
  ;   subsequently required for use by NSTAR.
  ;
  ; OPTIONAL OUTPUTS:
  ; GAUSS  - 5 element vector giving parameters of gaussian fit to the
  ;   first PSF star
  ;   GAUSS(0) - height of the gaussian (above sky)
  ;   GAUSS(1) - the offset (in pixels) of the best fitting gaussian
  ;     and the original X centroid
  ;   GAUSS(2) - similiar offset from the Y centroid
  ;   GAUSS(3) - Gaussian sigma in X
  ;   GAUSS(4) - Gaussian sigma in Y
  ; PSF    - 2-d array of PSF residuals after a Gaussian fit.
  ;
  ; PROCEDURE:
  ; GETPSF fits a Gaussian profile to the core of the first PSF star
  ; and generates a look-up table of the residuals of the
  ; actual image data from the Gaussian fit.  If desired, it will then
  ; fit this PSF to another star (using PKFIT) to determine its precise
  ; centroid, scale the same Gaussian to the new star's core, and add the
  ; differences between the actual data and the scaled Gaussian to the
  ; table of residuals.   (In other words, the Gaussian fit is performed
  ; only on the first star.)
  ;
  ; OPTIONAL KEYWORD INPUT:
  ; DEBUG - if this keyword is set and non-zero, then the result of each
  ;   fitting iteration will be displayed.
  ;
  ; PROCEDURES CALLED
  ; DAOERF, MAKE_2D, MKHDR, RINTER(), PKFIT, STRNUMBER(), STRN(), WRITEFITS
  ;
  ; REVISON HISTORY:
  ; Adapted from the 1986 version of DAOPHOT in STSDAS
  ; IDL Version 2  W Landsman           November 1988
  ; Use DEBUG keyword instead of !DEBUG  W. Landsman       May 1996
  ; Converted to IDL V5.0   W. Landsman   September 1997
  ;
  ; added flux-weighted x, y position of psf location  DPC  20080627
  ;-
  On_error,2                      ;Return to caller
  ;On_error, 0
  
  common rinter,c1,c2,c3,init  ;Save time in RINTER
  init = 0                        ;Initialize the common blocks
  ;
  if(N_ELEMENTS(debug) gt 0) then this_debug = debug else this_debug = 0
  if(N_ELEMENTS(no_resid) gt 0) then begin
    ;   print, 'this_GETPSF - entered with no_resid = ',no_resid
    zero_resid = ( 1 < (no_resid > 0))
  endif else zero_resid = 0.0
  ;
  ; print, 'this_GETPSF - zero_resid = ',zero_resid
  ;
  if(this_debug gt 0) then print, "this_GETPSF: zero_resid = ",zero_resid
  ; stop
  
  if(this_debug gt 0) then begin
    print, "this_GETPSF: xc = ",xc[idpsf]
    print, "this_GETPSF: yc = ",yc[idpsf]
    print, "this_GETPSF: apmag = ",apmag[idpsf]
    print, "this_GETPSF: sky = ",sky[idpsf]
  endif
  ;
  ;  test for bad input data
  ;
  ind_bad = WHERE(~FINITE(xc) or ~FINITE(yc) or ~FINITE(apmag) or ~FINITE(sky) $
    or ~FINITE(image), bad_count)
  if(bad_count gt 0) then begin
    print, 'ERROR: bad input to this_GETPSF'
    print, 'xc = ',xc
    print, 'yc = ',yc
    print, 'apmag = ',apmag
    print, 'sky = ',sky
    stop
  endif
  
  npar = N_params()
  
  if npar LT 5 then begin    ;Enough parameters passed?
    print,'Syntax -  GETPSF, image, x, y, mags, sky, '
    print,'       [ronois, phpadu, gauss, psf, idpsf, psfrad, fitrad, ' + $
      'psfname, /DEBUG]'
    return
  endif
  ;
  fit_sky = 0          ; dpc : 1 if sky to be recomputed, 0 else
  ;
  ;  constants for flux weighted psf x, y location
  ;
  x_w_psf = 0.0d0
  y_w_psf = 0.0d0
  wt_psf = 0.0d0
  sum_resid = 0.0d0
  ;
  s = size(image)        ;Get number of rows and columns in image
  ncol = s[1]
  nrow = s[2]
  nstar = N_elements(xc)         ;Total # of stars identified in image
  
  if N_elements(idpsf) LT 1 then begin ;Array of PSF id's defined?
    idpsf = intarr(25)
    i = 0
    id = ''
    print,"GETPSF: Enter index of stars to be used for PSF, one index per line"
    RD_ID:
    print,'Enter a stellar ID ( [RETURN] when finished) '
    read,id
    if id EQ '' then begin             ;Did User hit the [RETURN] key
      if i EQ 0 then return    ;No stellar ID's supplied
      idpsf = idpsf[0:i-1]
      goto, GOT_ID
    endif else result = strnumber(id,val)
    
    if not result then print,string(7b),'INVALID INPUT:' else $
      if (val GE nstar) or (val LT 0) then $
      print,string(7b),'INVALID ID NUMBER' else begin
      idpsf[i] = fix(val)
      i = i+1
    endelse
    goto,RD_ID
  endif
  
  GOT_ID:
  
  if N_elements(psfrad) NE 1 then read, $
    'Enter radius (in pixels) of circular area defining the PSF: ',psfrad
  if N_elements(fitrad) NE 1 then read, $
    'Enter radius (in pixels) to be used for Gaussian fitting: ',fitrad
  if fitrad GE psfrad then $
    message,'ERROR - Fitting radius must be smaller than radius defining PSF'
    
  if N_elements(ronois) NE 1 then read, $
    'Enter readout noise per pixel: ',ronois
  if N_elements(phpadu) NE 1 then read, $
    'Enter photons per analog digital unit: ',phpadu
    
  numpsf = N_elements(idpsf)      ;# of stars used to create the PSF
  
  smag = size(apmag)     ;Is APMAG multidimensional?
  if N_elements(apmag) NE smag[1] then mag = apmag[0,*] else mag = apmag[*]
  
  n = 2*fix(psfrad+0.5)+1  ;(Odd) width of box that contains PSF circle
  npsf = 2*n+7             ;Lookup table has half pixel interpolation
  ; 3 was 7 - dpc 20090507 - returned to 7 20090723, as RINTER doesn't
  ;   interpolate for outer two pixels of PSF !
  nbox = n+7         ;(Even) Width of subarray to be extracted from image
  
  nhalf = nbox/2
  
  if (this_debug gt 0) then begin
    print,'this_GETPSF: Fitting radius - ',string(float(fitrad),'(F5.1)')
    print,'        PSF Radius     - ',string(float(psfrad),'(F5.1)')
    print,'        Stellar IDs: ',idpsf   & print,' '
  endif
  
  boxgen = findgen(nbox)
  make_2d, boxgen, boxgen, xgen, ygen
  
  ;               Find the first PSF star in the star list.
  nstrps = -1  ;Counter for number of stars used to create PSF
  GETSTAR:
  nstrps = nstrps + 1
  if nstrps GE numpsf then $
    message,'ERROR - No valid PSF stars were supplied'
    
  istar = idpsf[nstrps]       ;ID number of first PSF star
  ixcen = fix(xc[istar])
  iycen = fix(yc[istar])
  ;
  ; print, "getstar with star number = ",istar
  
  ;  Now a subarray F will be read in from the big image, given by
  ;  IXCEN-NBOX/2+1 <= x <= IXCEN+NBOX/2, IYCEN-NBOX/2+1 <= y <= IYCEN+NBOX/2.
  ;  (NBOX is an even number.)  In the subarray, the coordinates of the centroid
  ;  of the star will lie between NBOX/2 and NBOX/2+1 in each coordinate.
  
  lx = ixcen-nhalf+1
  ux = ixcen + nhalf  ;Upper & lower bounds in X
  ly = iycen-nhalf+1
  uy = iycen + nhalf
  if ((lx LT 0)   or (ly LT 0) or $     ;Star too close to edge?
    (ux GE ncol) or (uy GE nrow)) then begin
    print,'this_GETPSF: Star ',strn(istar),' too near edge of frame at (x,y) = ',xc[istar],yc[istar]
    goto, GETSTAR
  endif
  
  f = image[lx:ux,ly:uy] - sky[istar]  ;Read in subarray, subtract off sky
  
  ; An integrated Gaussian function will be fit to the central part of the
  ; stellar profile.  Initially, a 5x5 box centered on the centroid of the
  ; star is used, but if the sigma in one coordinate drops to less than
  ; 1 pixel, then the box width of 3 will be used in that coordinate.
  ; If the sigma increases to over 3 pixels, then a box width of 7 will be
  ; used in that coordinate
  
  x = xc[istar] - lx    ;X coordinate of stellar centroid in subarray F
  y = yc[istar] - ly    ;Y coordinate of stellar centroid in subarray F
  ix = fix(x+0.5)       ;Index of pixel containing centroid
  iy = fix(y+0.5)
  ;                     ;Begin least squares
  H = max(f)            ;Initial guess for peak intensity
  sigx = fwhm / 2.354
  sigy = fwhm / 2.354
  dxcen=0.
  dycen=0.
  ;
  niter = 0                    ;Beginning of big iteration loop
  v = fltarr(5)
  c = fltarr(5,5)
  ;                            Print the current star
  fmt1 = "(/17X, 'STAR', 5X, 'X', 8X, 'Y', 5X, 'MAG  1', 5X, 'SKY')"
  fmt2 = "(15X, I5, 2F9.2, 12F9.3)"
  if keyword_set(DEBUG) then begin
    print,format=fmt1
    print,format=fmt2,istar, xc[istar], yc[istar], mag[istar], sky[istar]
  endif
  
  if (this_debug gt 0) then print,'this_GETPSF: Gaussian Fit Iteration'
  ;
  ;  DPC 20090307
  ;
  nx = 3
  old_nx = 3
  ny = 3
  old_ny = 3
  ;-----------------
  
  REPEAT BEGIN        ;Begin the iterative loop
  
    niter = niter + 1
    if niter GT 50 then begin   ;No convergence after 50 iterations?
      message,'No convergence after 50 iterations for star ' + strn(istar),/INF
      goto, GETSTAR
    endif
    
    if sigx LE 0.035 then nx = 2  $  ;A default box width - each incremented by 1 DPC 20090729
    else if sigx GT 3 then nx = 4  $
    else                   nx = 3
    
    if sigy LE 0.035 then ny = 2  $
    else if sigy GT 3 then ny = 4  $
    else                   ny = 3
    
    ; check for changed nx or ny
    ;
    if(nx ne old_nx) then begin
      print,"this_GETPSF: niter = ",niter," nx changed from ",old_nx," to ",nx,"  sig_x = ",sigx
      old_nx = nx
    endif
    if(ny ne old_ny) then begin
      print,"this_GETPSF: niter = ",niter," ny changed from ",old_ny," to ",ny,"  sig_y = ",sigy
      old_ny = ny
    endif
    ;
    a = [H, x+dxcen,y+dycen,sigx,sigy]
    ;
    xin = (findgen(2*nx+1)-nx) + ix
    yin = (findgen(2*ny+1)-ny) + iy
    make_2d, xin, yin
    DAOERF, xin, yin, a, g, t
    
    ;  The T's are the first derivatives of the model profile with respect
    ;  to the five fitting parameters H, DXCEN, DYCEN, SIGX, and SIGY.
    ;  Note that the center of the best-fitting Gaussian profile is
    ;  expressed as an offset from the centroid of the star.  In the case of
    ;  a general, asymmetric stellar profile, the center of symmetry of the
    ;  best-fitting Gaussian profile will not necessarily coincide with the
    ;  centroid determined by any arbitrary centroiding algorithm.
    
    dh = f[ ix-nx:ix+nx, iy-ny:iy+ny] - g ;Subtract best fit Gaussian from subarray
    for kk = 0,4 do begin
      tk = t[*,kk]
      v[kk] = total( dh * tk )
      for ll = 0,4 do c[kk,ll] = total( tk * t[*,ll] )
    endfor
    
    c = invert(c,status) ;IDL version assumes INVERT is successful
    
    if status EQ 1 then begin
      message,'Singular matrix encountered fitting star ' + strn(istar),/INF
      goto, GETSTAR
    endif
    
    z = c#v         ;Multiply by vector of residuals
    
    h = h + z[0]/(1.0+4.0*abs(z[0]/h)) ;Correct the fitting parameters
    dxcen = dxcen+z[1]/(1.0+3.0*abs(z[1]))
    dycen = dycen+z[2]/(1.0+3.0*abs(z[2]))
    sigx = sigx+z[3]/(1.0+4.0*abs(z[3]/sigx))
    sigy = sigy+z[4]/(1.0+4.0*abs(z[4]/sigy))
    ;
    ;  test for sigx or sigy going under 0.35 pixels - DPC 20090307 (0.5->0.35 20090401)
    ;
    if(sigx lt 0.35) then begin
      print, "this_GETPSF: Warning - sigx = ",sigx," after adding star ",istar+1," at X,Y = ",xc[istar],yc[istar]
    endif
    if(sigy lt 0.35) then begin
      print, "this_GETPSF: Warning - sigy = ",sigy," after adding star ",istar+1," at X,Y = ",xc[istar],yc[istar]
    endif
    ;
    if KEYWORD_SET(debug) then print,niter,h,dxcen,dycen,sigx,sigy
    
  endrep until $         ;Test for convergence
    (abs(z[0]/h)+abs(z[3]/sigx)+abs(z[4]/sigy) LT 0.001)
    
  ;  Now that the solution has converged, we can generate an
  ;  array containing the differences between the actual stellar profile
  ;  and the best-fitting Gaussian analytic profile.
  
  a = [H, x+dxcen, y+dycen, sigx,sigy]  ;Parameters for Gaussian fit
  DAOERF,xgen,ygen,a,g                  ;Compute Gaussian
  f = f - g                             ;Residuals (Real profile - Gaussian)
  ;
  ;  DPC - 20090522 - finding that sometimes (rarely), very bad pixel values
  ;    survive and get into the residuals. So, lets "clean" the residuals
  ;    before accumulating them...
  ;
  zz = median_filtered_mean(f)
  bad_f = WHERE(( abs(f-zz[0])/zz[1] gt 90.0) or ~FINITE(f),n_bad_f) ; 90 sigma deviations
  if(n_bad_f gt 0) then begin
    f[bad_f] = zz[0]
  endif
  ;
  
  psfmag = mag[istar]        ; this establishes the PSFmag of the model - DPC
  xpsf1 = xc[istar]
  ypsf1 = yc[istar]
  ;
  x_w_psf = xpsf1
  y_w_psf = ypsf1
  wt_psf = 1.0
  
  ; The look-up table is obtained by interpolation within the array of
  ; fitting residuals.  We need to interpolate because we want the look-up
  ; table to be centered accurately on the centroid of the star, which of
  ; course is at some fractional-pixel position in the original data.
  
  ncen = (npsf-1)/2.
  psfgen = (findgen(npsf) - ncen)/2.         ;Index function for PSF array
  YY = psfgen + Y
  XX = psfgen + X
  make_2d,xx,yy
  psf = RINTER(F, XX, YY)            ;Interpolate residuals onto current star
  gauss = [h,dxcen,dycen,sigx,sigy]
  goodstar = nstrps                   ;Index of first good star
  ;
  ; if(fit_sky eq 1) then begin
  ;  ispsf = size(psf)
  ;  nx_new_sky = ispsf[1]
  ;  ny_new_sky = ispsf[2]
  ;  new_sky = [REFORM(psf[0,*]),REFORM(psf[1:*]),REFORM(psf[*,0]),REFORM(psf[*,1]),$
  ;     REFORM(psf[nx_new_sky-2,*]),REFORM(psf[nx_new_sky-1:*]),$
  ;     REFORM(psf[*,ny_new_sky-2]),REFORM(psf[*,ny_new_sky-1])]
  ;  zz = median_filtered_mean(new_sky)
  ;  psf = psf - zz[0]
  ; endif
  ;
  psf_vec = [0.]
  npsfvec = 0L
  xmeanx = TOTAL(XX)/N_ELEMENTS(XX)
  ymeany = TOTAL(YY)/N_ELEMENTS(YY)
  ;help, XX
  ;print, "number of elements in XX = ",N_ELEMENTS(XX)
  ;print, "XX = ",XX
  for ixdc = 0, npsf-1 do begin
    for iydc = 0, npsf-1 do begin
      r = sqrt((XX[ixdc,iydc]-xmeanx)^2 + (YY[ixdc,iydc]-ymeany)^2)/2.0
      if(r le psfrad) then begin
        if(r gt fitrad) then begin
          psf[ixdc,iydc] = psf[ixdc,iydc] * float(1.0 - zero_resid)
        endif else begin
          if(npsfvec eq 0) then begin
            psf_vec = [psf[ixdc,iydc]]
          endif else begin
            psf_vec = [psf_vec, psf[ixdc,iydc]]
          endelse
          npsfvec++
        endelse
      endif else begin
        psf[ixdc,iydc] = psf[ixdc,iydc] * float(1.0 - zero_resid)
      endelse
    endfor
  endfor
  ;
  sum_resid = TOTAL(psf_vec) / 4.0   ; reduced to account for proper flux accounting while
  ; num_resid = N_ELEMENTS(psf_vec)   ; interpolating onto model pixels
  ; resid_unc = STDDEV(psf_vec) * sqrt(npsfvec)
  ; if (this_debug gt 0 or sum_resid lt 0.0) then begin
  ;   print, "for star ",istar+1," residual psf sum, uncert. = ",sum_resid,resid_unc
  ;   print, "... that, for a total of ",num_resid," elements in psf_vec within psfrad of center"
  ; endif
  
  ; For each additional star, determine the precise  coordinates of the
  ; centroid and the relative brightness of the star
  ; by least-squares fitting to the current version of the point-spread
  ; function.  Then subtract off the appropriately scaled integral under
  ; the analytic Gaussian function  and add the departures of the actual
  ; data from the analytic Gaussian function to the look-up table.
  
  GETMORE:            ;Loop for additional PSF stars begins here
  nstrps = nstrps+1
  if nstrps GE numpsf then goto,WRITEOUT ;Have all the stars been done?
  
  istar = idpsf[nstrps]
  ;
  ; print, "adding star number ",istar
  ;
  if (this_debug gt 0) then print, "For star ",nstrps," which is star list number = ",istar+1,"...
  ;
  ixcen = fix(xc[istar])
  iycen = fix(yc[istar])
  scale = 10.^(-0.4*(mag[istar]-psfmag))
  
  ; Fit the current version of the point-spread function to the data for
  ; this star.
  
  lx = ixcen-nhalf+1
  ux =ixcen + nhalf
  ly = iycen-nhalf+1
  uy =iycen + nhalf
  ;
  if ( (lx LT 0) or (ly LT 0) or $             ;Star too close to edge?
    (ux GE ncol) or (uy GE nrow)) then begin
    print,'this_GETPSF: Star ',strn(istar),' too near edge of frame.'
    goto,GETMORE
  endif
  
  if keyword_set (DEBUG) then begin
    print,format=fmt1
    print,format=fmt2, istar, xc[istar], yc[istar], mag[istar], sky[istar]
  endif
  
  f = image[lx:ux,ly:uy]
  x = xc[istar]-lx
  y = yc[istar]-ly
  
  ;
  if (this_debug gt 0) then print, "Scale factor before PSF fit = ",scale
  ;
  zz = median_filtered_mean(f)
  if(abs(zz[0]-sky[istar])/zz[1] gt 3.0) then this_sky = zz[0] else this_sky = sky[istar]
  ;
  ;  call the modified version of pkfit
  ;
  ; print, "Calling g_pkfit with error state - ",!ERROR_STATE
  ;
  g_pkfit, f, scale, x, y, this_sky, fitrad, ronois, phpadu, $
    gauss, psf, errmag, chi, sharp, niter;, DEBUG = debug
  ; print, "After calling g_pkfit error state - ",!ERROR_STATE
  ;
  ;
  ;  if no fit, skip this star
  ;
  if(errmag eq -1.0) then begin
    print, 'this_GETPSF: could not fit gaussian for PSF star number ',istar+1,' at (X,Y) = ',xc[istar],$
      yc[istar],' mag = ',apmag[istar], ' sky = ',sky[istar]
    print, '             best fit gaussian values = ',gauss,' skipping this star'
    goto, GETMORE
  endif
  ;
  if niter ge 50 then begin  ;Convergence in less than 50 iterations?
    print,'this_GETPSF: No convergence after 50 iterations for star',istar+1
    goto, GETMORE
  endif
  ;
  if (this_debug gt 0) then print, "Scale factor after PSF fit = ",scale
  ;
  if(~FINITE(scale)) then stop
  ;
  if (this_debug gt 0) then print, "Sky value after PSF fit = ",sky[istar]
  ;
  a = [gauss[0], x+dxcen,y+dycen,sigx,sigy]  ;Parameters of successful fit
  daoerf,xgen,ygen,a,e           ; the gaussian representing this new star
  ;
  if (this_debug gt 0) then print, "Total F before removing gaussian and sky = ",TOTAL(f)
  f = f - scale*e
  if (this_debug gt 0) then print, "Total F after removing scaled gaussian = ",TOTAL(f)
  f = f - this_sky;      sky[istar]             ;Compute array of residuals
  if (this_debug gt 0) then print, "Total F after removing sky = ",TOTAL(f)
  ;
  zz = median_filtered_mean(f)
  bad_f = WHERE(( abs(f-zz[0])/zz[1] gt 90.0) or ~FINITE(f),n_bad_f) ; 9 sigma deviations
  if(n_bad_f gt 0) then begin
    f[bad_f] = zz[0]
  endif
  ;
  ;  recompute sky as mean of outer band of pixels
  ;
  ; if(fit_sky eq 1) then begin
  ;  isf = size(f)
  ;  nx_new_sky = isf[1]
  ;  ny_new_sky = isf[2]
  ;  new_sky = [REFORM(f[0,*]),REFORM(f[1:*]),REFORM(f[*,0]),REFORM(f[*,1]),$
  ;     REFORM(f[nx_new_sky-2,*]),REFORM(f[nx_new_sky-1:*]),$
  ;     REFORM(f[*,ny_new_sky-2]),REFORM(f[*,ny_new_sky-1])]
  ;  zz = median_filtered_mean(new_sky)
  ;  f = f - zz[0]
  ;  if (debug gt 0) then print, "Total F after removing new sky = ",TOTAL(f)
  ; endif
  
  ; Values of the array of residuals are now interpolated to an NPSF by
  ; NPSF (NPSF is an odd number) array centered on the centroid of the
  ; star, and added to the existing look-up table of corrections to the
  ; analytic profile
  
  xx = psfgen + x
  yy = psfgen + y
  make_2d,xx,yy
  ff = RINTER(f,xx,yy)
  if (this_debug gt 0) then print, "Total FF after interpolation = ",TOTAL(ff)
  ;
  ; print, "about to add ff to psf"
  psf = psf + ff
  ; print, "after adding ff to psf"
  
  ; Now correct both the height of the analytic Gaussian, and the value
  ; of the aperture-magnitude of the point-spread function for the
  ; inclusion of the additional star.
  ;
  if (this_debug gt 0) then print, "PSFmag before updating = ",psfmag
  ;
  psfmag = -2.5d0*alog10((1.d0+scale)*10.0d0^(-0.4d0*psfmag))
  ;
  if (this_debug gt 0) then print, "PSFmag after updating = ",psfmag
  ;
  gauss[0] = gauss[0]*(1.+scale)
  goodstar = [ goodstar, nstrps]
  ;
  ;  and update the flux weighted psf x,y position
  ;
  x_w_psf = x_w_psf + xc[istar] * scale
  y_w_psf = y_w_psf + yc[istar] * scale
  wt_psf = wt_psf + scale
  ;
  ;  sum of residuals
  ;
  psf_vec = [0.]
  npsfvec = 0L
  xmeanx = TOTAL(XX)/N_ELEMENTS(XX)
  ymeany = TOTAL(YY)/N_ELEMENTS(YY)
  for ixdc = 0, npsf-1 do begin
    for iydc = 0, npsf-1 do begin
      r = sqrt((XX[ixdc,iydc]-xmeanx)^2 + (YY[ixdc,iydc]-ymeany)^2)/2.
      if(r le psfrad) then begin
        if(r gt fitrad) then begin
          psf[ixdc,iydc] = psf[ixdc,iydc] * float(1.0 - zero_resid)
        endif else begin
          if(npsfvec eq 0) then begin
            psf_vec = [psf[ixdc,iydc]]
          endif else begin
            psf_vec = [psf_vec, psf[ixdc,iydc]]
          endelse
          npsfvec++
        endelse
      endif else begin
        psf[ixdc,iydc] = psf[ixdc,iydc] * float(1.0 - zero_resid)
      endelse
    endfor
  endfor
  ;
  ;
  sum_resid = TOTAL(psf_vec) / 4.0   ; reduced by 4 to account for proper flux accounting while
  ;                     ; interpolating onto model pixels
  ; resid_unc = STDDEV(psf_vec) * sqrt(npsfvec)
  ; if (this_debug gt 0) then begin
  ;   print, "for star ",istar," residual psf sum, uncert. = ",sum_resid,resid_unc
  ;   print, "... that, for a total of ",num_resid," elements in psf_vec within psfrad of center"
  ; endif
  ;
  ;
  goto, GETMORE
  
  WRITEOUT:
  
  ; Create FITS file containing the PSF created.
  
  if ( N_elements(psfname) EQ  0 ) then begin
    psfname=''
    read,'Enter name of FITS file to contain final PSF ([RETURN] to exit): ',psfname
  endif
  
  if ( psfname EQ  '' ) then begin
    print, "no psf name supplied:
    return
  endif
  
  mkhdr, hdr, psf         ;Create a minimal FITS header
  ;
  ; print, "Created FITS header for PSF"  ; 20100524
  ;
  sxaddpar, hdr, 'PHPADU', phpadu, 'Photons per Analog Digital Unit'
  sxaddpar, hdr, 'RONOIS', ronois, 'Readout Noise'
  sxaddpar, hdr, 'PSFRAD', psfrad, 'Radius where PSF is defined (pixels)'
  sxaddpar, hdr, 'FITRAD', fitrad, 'Fitting Radius'
  ;
  ;  if psfmag is -NaN, blow up here for debug
  ;
  if(NOT(FINITE(psfmag))) then begin
    print,"PSFMAG is not finite, blowing up.."
    a = 0.
    b = 1.
    c = b / a
  endif
  ;
  sxaddpar, hdr, 'PSFMAG', psfmag, 'PSF Magnitude'
  ;
  if (this_debug) then print,"THIS_GETPSF: Leaving with PSFmag = ",psfmag
  ;
  sxaddpar, hdr, 'GAUSS1', gauss[0], 'Gaussian Scale Factor'
  sxaddpar, hdr, 'GAUSS2', gauss[1], 'Gaussian X Position'
  sxaddpar, hdr, 'GAUSS3', gauss[2], 'Gaussian Y Position'
  sxaddpar, hdr, 'GAUSS4', gauss[3], 'Gaussian Sigma: X Direction'
  sxaddpar, hdr, 'GAUSS5', gauss[4], 'Gaussian Sigma: Y Direction'
  ;
  sxaddpar, hdr, 'SUMRESID', sum_resid, 'Flux sum of resid array inside FITRAD'
  ;
  x_w_psf = x_w_psf / wt_psf
  y_w_psf = y_w_psf / wt_psf
  sxaddpar, hdr, 'X_W_PSF', x_w_psf, 'Flux-Weighted X position'
  sxaddpar, hdr, 'Y_W_PSF', y_w_psf, 'Flux-Weighted Y position'
  
  ngood = N_elements(goodstar)
  sxaddhist,'GETPSF: '+ systime() + ' ' + strn(ngood) +  $
    ' Stars Used to Create PSF',hdr
    
  sxaddhist,'GETPSF: ID - '+ string(idpsf[goodstar[0:12<ngood-1]], $
    format='(13i5)'),hdr
    
  if ngood gt 13 then $
    sxaddhist,'GETPSF: ID - '+ string(idpsf[goodstar[13:*]], $
    format='(13i5)'),hdr
    
  sxaddhist,'PSF Coordinates:'+ $
    string(xpsf1, format='(F8.2)') + $
    string(ypsf1, format='(F8.2)'), hdr
  ;
  ; clean the PSF to 8.5 mag using new Clean_PSF code - 20100306 - DPC
  ;
  clean_PSF, hdr, psf, DEBUG=debug
  ;
  ; writefits,psfname,psf,hdr
  ;
  ; save for return to calling program
  ;
  resid = psf
  ;
  ;print, "this_getpsf: PSFmag on exit = ",psfmag
  ;g_mag = 25.0 - 2.5d0 * alog10(gauss[0] * 2.0d0 * !DPI * gauss[3] * gauss[4])
  ;r_mag = 25.0 - 2.5d0 * alog10(sum_resid)
  ;print, "this_getpsf: Gaussian mag, resid_mag = ",g_mag, r_mag
  ;
  ; return
  ;
  ;print, !ERROR_STATE
  ;
end
