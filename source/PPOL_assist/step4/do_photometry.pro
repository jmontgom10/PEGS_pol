;This script reads in a list of pre-approved photometry stars,
;performs aperture photometry using the G_APER procedure
;and writes a STEP 4 style header.

groupDir = DIALOG_PICKFILE(TITLE='Select the galaxy+project folder', /DIRECTORY)
testDir  = FILE_TEST(groupDir, /DIRECTORY)
IF ~testDir THEN STOP ELSE CD, groupDir

;Read in the PPOL directory for this project
ppolDir  = ''
OPENR, lun, 'ppolDir.dat', /GET_LUN
READF, lun, ppolDir
FREE_LUN, lun
S3directory = ppolDir + 'S3_Astrometry' + PATH_SEP()
S4directory = ppolDir + 'S4_First_PSF_Fits' + PATH_SEP()

;Read in all of the BDP file names
groupFiles    = FILE_SEARCH('groups' + PATH_SEP() + '*.dat', COUNT=numGroups)

;The following code sets up the basic properties for the G_FIND procedure to be used for ALL images
;fwhm         = 3.0                                          ; a 3 pixel FWHM is probably close enough for jazz
;roundlim     = [-1.0,0.85]                                  ; roundness limit for G_FIND procedure
;sharplim     = [0.1,0.85]                                   ; sharpness limit for G_FIND procedure
nsigma       = 8.0                                          ; set the detection threshold in terms of sky noise
sat_limit    = 1.4e4                                        ; set the basic saturation limit
badpix       = [-1.0e4,sat_limit]                           ; pixels outside this range are bad
bright_limit = sat_limit                                    ; set the star photometry saturation threshold

;The following code sets up the basic photometry parameters which will be used for ALL images
;(****stolen from S4_PSF_FIT.PRO and G_CREATE_GSP_GRID.PRO****)
ref_ap    = [1.2, 1.7]                                      ; times ap_rad times FWHM
ref_ap   *= 1.6                                             ; fudge factor produces better star results
;
;-------
ap_rad    = 2.80                                            ; times FWHM
clean_rad = 3.6                                             ; times ap_rad times FWHM
ap_vec    = [0.40, 0.475, 0.55, 0.625, 0.7, 0.775, $        ; times FWHM times ap_rad
             0.85, 0.925, 1.00, 1.050, 1.1, 1.150]
n_ap      = N_ELEMENTS(ap_vec)
rad_scale  = 3.00                                           ; times fwhm - sets PSF size
rad_scale *= 1.25                                           ; 20120603 - DPC - fudge factor for better PSF modeling
ronois     = 3.1                                            ; single image readout noise in ADU (S4_PSF_FIT.PRO)
phpadu     = 8.21                                           ; photons per AUD (G_CREATE_PSF_GRID.PRO)
sigma2FWHM = (2*SQRT(2*ALOG(2)))                            ; converstion from sigma to FWHM for a gaussian

;Loop through each group and read in the file lists for each group
FOR i = 0, numGroups - 1 DO BEGIN
  groupBDPfile  = ''                                                  ;Initalize strings for reading file contents
  groupBDPfiles = ''
  groupS3files  = ''
  OPENR, lun, groupFiles[i], /GET_LUN
  WHILE ~EOF(lun) DO BEGIN
    READF, lun, groupBDPfile
    S3test = FILE_TEST(S3directory + FILE_BASENAME(groupBDPfile))     ;Test if there step 3
    IF S3test THEN BEGIN
      groupBDPfiles = [groupBDPfiles, groupBDPfile]                   ;Concatenate filenames
      groupS3files  = [groupS3files, S3directory + FILE_BASENAME(groupBDPfile)]
    ENDIF
  ENDWHILE
  FREE_LUN, lun
  groupBDPfiles = groupBDPfiles[1:*]                                  ;Trim the leading null entry
  groupS3files  = groupS3files[1:*]                                   ;Trim the leading null entry
  
  ;Read in the PSF star list for this group
  PSFstarsFile = 'S4_Photometry_Files' + PATH_SEP() + $               ;Setup the star list filename
    STRING((i+1), '("PSFstars",I02,".dat")')
  READCOL, PSFstarsFile, PSF_ID, PSF_RAs, PSF_Decs, $                 ;Read in the columns of data
    FORMAT = 'I,D,D'
  
  ;Read in the photometry star list for this group
  photoStarsFile = 'S4_Photometry_Files' + PATH_SEP() + $             ;Setup the star list filename
    STRING((i+1), '("photometryStars",I02,".dat")')
  READCOL, photoStarsFile, photo_ID, photoRAs, photoDecs, $           ;Read in the columns of data
    FORMAT = 'I,D,D'
  
  ;Loop through all the files in this group
  FOR j = 0, N_ELEMENTS(groupS3files) - 1 DO BEGIN
    step3Test = FILE_TEST(groupS3files[j])                            ;Test if a step 3 image is present
    IF step3Test THEN BEGIN
      photoRAs1 = photoRAs & photoDecs1 = photoDecs                   ;Reset the RA and Dec list
      imgBDP = READFITS(groupBDPfiles[j])                             ;Read in the BDP image
      img    = READFITS(groupS3files[j], imgHeader)                   ;Read in the step 3 image
      sz     = SIZE(img, /DIMENSIONS)                                 ;Grab the image dimensions
      EXTAST, imgHeader, astr, noparams                               ;Extract image astometry from header
      AD2XY, photoRAs1, photoDecs1, astr, photoX, photoY              ;Convert 2MASS (RA, Dec) into (x,y) positions
      xPhot = photoX & yPhot = photoY                                 ;Alias the star positions to be refined
      
      ;***NAME SCHEME***
      ;xPhot and yPhot   = star position as deteced in the image
      ;photoX and photoY = star position as estimaed by (RA, Dec) and astrometry
      
      G_SKY, img, skymode, skysig, mfm_mean, mfm_stdev, $             ;Quickly generate sky statistics
        LLQ=llq, /SILENT
;      SKY, img, skymode, skynoise                                    ;Quickly generate sky statistics
;      hmin   = skymode + nsigma*skysig                                ;Set an n-sigma detection limit
      nStars = N_ELEMENTS(photoX)                                     ;Count the number of stars to find
      FWHMs  = FLTARR(nStars)                                         ;Array to store FWHM of each star
      FOR k = 0, nStars - 1 DO BEGIN                                  ;Loop through each star and find its gcentrd value
        removeStars = -1                                              ;Reset the removeStars variable
        xOff     = (photoX[k] - 20) > 0
        xRt      = (xOff  + 40) < (sz[0] - 1)
        yOff     = (photoY[k] - 20) > 0
        yTop     = (yOff + 40)  < (sz[1] - 1)
        subArBDP = imgBDP[xOff:xRt, yOff:yTop]                        ;Cut out a subarray for testing for saturation
        subArray = img[xOff:xRt, yOff:yTop]                           ;Cut out a subarray for gaussian fitting
        result   = GAUSS2DFIT(subArray, A, /TILT)                     ;Gaussian fit the star
        IF A[4] GT 5 AND A[4] LT 35 $
          AND A[5] GT 5 AND A[5] LT 35 THEN BEGIN
          FWHMs[k] = SQRT(A[2]*A[3])*sigma2FWHM                       ;Compute the FWHM for this star
          GCNTRD, subArray, A[4], A[5], xcen, ycen,  FWHMs[k]         ;Centroid this star (using estimated FWHM)
          xPhot[k] = xOff + xcen                                      ;Update the star x-position
          yPhot[k] = yOff + ycen                                      ;Update the star y-position
          
          deltaPos = SQRT((xPhot[k] - photoX[k])^2 + (yPhot[k] - photoY[k])^2)
          IF deltaPos GT 7 THEN BEGIN                                 ;Test for large shifts
            PRINT, 'Could not fit star #', k
            PRINT, 'Adding to the "removeStars" list'
            IF TOTAL(removeStars) EQ -1 THEN removeStars = [k] $
              ELSE removeStars = [removeStars, k]                     ;Remove star from list for "problems fitting"
          ENDIF
;          tvim, subArray - subArBDP
;          oplot, REPLICATE(xcen,2), [0,40]
;          oplot, [0,40], REPLICATE(ycen, 2)
;          stop
        ENDIF ELSE BEGIN                                              ;If GAUS2DFIT failed then skip star
          PRINT, 'Could not fit star #', k
          PRINT, 'Adding to the "removeStars" list'
          IF TOTAL(removeStars) EQ -1 THEN removeStars = [k] $
            ELSE removeStars = [removeStars, k]                       ;Remove star from list for "problems fitting"                                  ;Remove star from list for "problems fitting"
        ENDELSE
      ENDFOR
      mean_FWHM   = (MEDIAN_FILTERED_MEAN(FWHMs))[0]                  ;Compute the mean FWHM
      fit_scale   = 1.75                                              ;Times fwhm - sets fitting zone
      fit_rad     = fit_scale*mean_FWHM                               ;Fit the PSF within this radius
      rad_PSF     = CEIL(rad_scale*mean_FWHM)                         ;Modedl the PSF out to this radius
      rcrit       = SQRT(2)*MAX(ap_vec)*mean_FWHM*ap_rad              ;Star matching critical radius
      GROUP, Event, xPhot, yPhot, 2*rcrit, group_vector               ;Group stars within sqrt(2)*max(apr) of each other
                                                                      ;DO NOT ATTEMPT PHOTOMETRY OF STARS WITH NEIGHBORS
      groups = group_vector[UNIQ(group_vector, SORT(group_vector))]   ;Parse out the group IDs
      FOR k = 0, N_ELEMENTS(groups) - 1 DO BEGIN                      ;Loop through all the stars
        groupInds = WHERE(group_vector EQ groups[k], groupCount)      ;Select the stars in this group
        IF groupCount GT 1 THEN BEGIN                                 ;Save the star indices to remove
          IF TOTAL(removeStars) EQ -1 THEN removeStars = groupInds $
            ELSE removeStars = [removeStars, groupInds]
        ENDIF
      ENDFOR
      IF TOTAL(removeStars) NE -1 THEN BEGIN                          ;Test if there were any group stars to remove 
        REMOVE, removeStars, xPhot, yPhot, $                          ;Remove stars that were part of groups
          photoRAs1, photoDecs1, group_vector
      ENDIF
      SRCOR, xPhot, yPhot, photoX, photoY, $                          ;Match 2MASS entries with image star positions
        2*rcrit, ind1, ind2, OPTION = 1
      xPhot = xPhot[ind1] & yPhot = yPhot[ind1]                       ;Save the matched stars
      photoRAs1 = photoRAs1[ind2] & photoDecs1 = photoDecs1[ind2]     ;Save the matched stars
      nStars = N_ELEMENTS(ind1)                                       ;Recount the photometry stars


;SINCE THERE ARE SO FEW GOOD PHOTOMETRY STARS, IT IS NOT A GOOD IDEA TO REDO ASTROMETRY on them...


;      IF nStars LT 8 THEN BEGIN
;        WINDOW, xs = 800, ys = 800
;        TVIM, img, RANGE = (skymode + [-3, +20]*skysig)
;        OPLOT, photoX, photoY, PSYM = 6, SYMSIZE=2, COLOR=255L, THICK =2
;        OPLOT, xPhot, yPhot, PSYM = 4, SYMSIZE=2, COLOR=150L, THICK = 2
;        STOP
;      ENDIF
      
      ;******* ADD SOME FINAL LINES OF CODE TO RECOMPUTE THE ASTROMETRY
      ;******* USING THE FINAL MATCHED PHOTOMETRY STAR POSITIONS
;      IF nStars GE 6 THEN BEGIN
;        astr = JM_SOLVE_ASTRO(photoRAs1, photoDecs1, xPhot, yPhot, $
;          NAXIS1 = sz[0], NAXIS2 = sz[1])
;        crpix = [511, 512]
;        XY2AD, crpix[0], crpix[1], astr, crval1, crval2
;        astr.crpix = (crpix + 1)                                      ;FITS convention is offset 1 pixel from IDL
;        astr.crval = [crval1, crval2]                                 ;Store the updated reference pixel values
;        PUTAST, imgHeader, astr, EQUINOX = 2000                       ;Update the header with the new astrometry
;      ENDIF ELSE IF nStars GE 3 THEN BEGIN
;        numTri = numGood*(numGood-1)*(numGood-2)/6
;        big_cd = DBLARR(2,2,numTri)
;        triCnt = 0                                                    ;Initalize a counter for looping through triangles
;        FOR iStar = 0, numGood - 1 DO BEGIN                           ;Loop through all possible triangles
;          FOR jStar = iStar+1, numGood - 1 DO BEGIN
;            FOR kStar = jStar+1, numGood - 1 DO BEGIN
;              these_stars = [iStar,jStar,kStar]                       ;Grab the indices of the stars in this triangle
;              STARAST, photoRAs1[these_stars], photoDecs1[these_stars], $ ;Sove astrometry using this triangle of stars
;                xPhot[these_stars], yPhot[these_stars], $
;                this_cd, PROJECTION = 'TAN'
;              big_cd[*,*,triCnt] = this_cd                            ;Store the CD matrix
;              triCnt++                                                ;Increment the triangle counter
;            ENDFOR
;          ENDFOR
;        ENDFOR
;        cd_matrix = DBLARR(2,2)                                       ;Initalize an array for mean CD matrix
;        FOR iMat = 0, 1 DO BEGIN
;          FOR jMat = 0, 1 DO BEGIN
;            cd_matrix[iMat,jMat] = $                                  ;Compute the mean CD matrix
;              (MEDIAN_FILTERED_MEAN(REFORM(big_cd[iMat,jMat, *])))[0]
;          ENDFOR
;        ENDFOR
;        crpix      = [511, 512]                                       ;Define the center pixels
;        centerDist = SQRT((xPhot - crpix[0])^2 + $                    ;Compute star distances from the image center
;          (yPhot - crpix[1])^2)
;        centerStar = WHERE(centerDist EQ MIN(centerDist))             ;Grab the star closest to the image center
;        deltaX     = crpix[0] - xPhot[centerStar]                     ;Compute pixel x-offset from center
;        deltaY     = crpix[1] - yPhot[centerStar]                     ;Compute pixel y-offset from center
;        deltaAD    = REFORM(cd_matrix##[[deltaX],[deltaY]])           ;Compute (RA, Dec) offsets from center
;        deltaAD[0] = deltaAD[0]*COS(photoDecs1[centerStar]*!DTOR)     ;Correct RA offset for distortion 
;        crval      = [photoRAs1[centerStar],photoDecs1[centerStar]]   ;Re-compute the center value
;        MAKE_ASTR, astr, CD = cd_matrix, CRPIX = [xPhot[centerStar], yPhot[centerStar]], $ ;Create final astrometry structure
;          CRVAL = [photoRAs1[centerStar],photoDecs1[centerStar]], CTYPE = ['RA---TAN','DEC--TAN']
;        XY2AD, 511, 512, astr, crval1, crval2                         ;Recenter astrometry structure
;        MAKE_ASTR, astr, CD = cd_matrix, CRPIX = (crpix+1), $         ;Create final astrometry structure
;          CRVAL = [crval1, crval2], CTYPE = ['RA---TAN','DEC--TAN']
;        PUTAST, imgHeader, astr, EQUINOX = 2000                       ;Store astrometry in header
;      ENDIF ELSE STOP
      
      
      ;Do a quick test of the astrometry to make sure it makes sense
;      GETROT, astr, rot_angle, cdelt
;      dx = ABS(cdelt[0]*3600D)
;      dy = ABS(cdelt[1]*3600D)
;      IF (ABS(rot_angle) GT 0.34) $                                    ;Test if the rotation angle
;        OR (dx - 0.5786D)/0.5786D GT 0.005 $                          ;and the plate scales
;        OR (dy - 0.5786D)/0.5786D GT 0.005 THEN BEGIN                 ;make sense
;        loadct, 0
;        tvim, img, range = skymode + [-3,+20]*skysig
;        loadct, 13
;        OPLOT, xPhot, yPhot, PSYM = 7, THICK = 2, SYMSIZE = 2, color = 500
;        OPLOT, photoX, photoY, PSYM = 1, color = 100
;        PRINT, FORMAT='("Rotation angle is ",F6.4," degrees east of north")', rot_angle
;        PRINT, FORMAT='("Plate scale is (dx, dy) = (",F6.4,",",F6.4,") arcsec/pixel")', dx, dy
;        STOP
;      ENDIF

;      WRITEFITS, groupS3files[j], img, imgHeader                      ;Re-save the S3 image with updated astrometry header


      XY2AD, xPhot, yPhot, astr, photoRAs1, photoDecs1                ;Re-compute the star (RA, Dec) using updated astr

;      loadct, 0
;      tvim, img, range = skymode + [-3,+20]*skysig
;      loadct, 13
;      OPLOT, xPhot, yPhot, PSYM = 7, THICK = 2, SYMSIZE = 2, color = 500
;      OPLOT, photoX, photoY, PSYM = 1, color = 100
;      stop

      
      ;Aperture photometry is performed for the pre-selected stars
      apr    = ap_vec * ap_rad * mean_FWHM                            ;Compute the apertures in terms of mean FWHM
      skyrad = ref_ap * ap_rad * mean_FWHM                            ;Compute sky annulus in terms of mean FWHM
      G_APER,img,xPhot,yPhot,mag,err,skyvalues,skyerr,phpadu,apr, $   ;Perform aperture photometry
        skyrad,badpix,bright_limit, /NO_BAD_PIX_FIX, /SILENT
      badStars = WHERE(REFORM(mag[0,*]) LT 0 OR mag GT 18, numBad, $  ;Test for unreasonable magnitude measurements
        COMPLEMENT = goodStars, NCOMPLEMENT = numGood)
      IF numBad GT 0 AND numGood GT 0 THEN BEGIN                      ;Only keep the good star information
        nStars       = numGood
        xPhot        = xPhot[goodStars]
        yPhot        = yPhot[goodStars]
        photoRAs1    = photoRAs1[goodStars]
        photoDecs1   = photoDecs1[goodStars]
        group_vector = group_vector[goodStars]
        G_APER,img,xPhot,yPhot,mag,err,skyvalues,skyerr,phpadu,apr, $ ;Re-compute aperture photometry
          skyrad,badpix,bright_limit, /NO_BAD_PIX_FIX, /SILENT
      ENDIF ELSE IF ~(numGood GT 0) THEN STOP

      skymean = (MEDIAN_FILTERED_MEAN(skyvalues))[0]                  ;Compute a mean sky value in PPOL fashion
      skysig  = mfm_stdev

      ;Now that the acceptable stars have been selected
      ;and initial photometry has been done, 
      ;a PSF model for this image is generated
      ;********************************************************
      ;******** EVENTUALLY LIMIT THIS TO PSF STAR LIST ********
      ;********************************************************
      n_total_PSF_stars = (N_ELEMENTS(xPhot) < 5)                     ;Limit to the five BRIGHTEST stars
      idpsf             = INDGEN(n_total_PSF_stars)                   ;Set the indices for PSF stars
      psfname = ''                                                    ;Null string prevents saving PSF file
      JM_GETPSF,img,xPhot,yPhot,mag,skyvalues,ronois,phpadu, $        ;Fit a PSF model
        gauss, psf, mean_fwhm, idpsf, rad_PSF, fit_rad, $
        x_w_psf, y_w_psf, PSF_mag, sum_resid, resid, psfname;, $
        ;NO_RESID = 1, DEBUG=1
        
      ;********* FIND A WAY TO *ONLY* KEEP THE PSF STARS THAT WORKED *****
      ;**** code goes here
      
      
      ;For more crowded fields, neighboring stars should be PSF subtracted
      ;and their photometry re-calculated...
      ;*******************************************************
      ;******* PHOTOMETRY RECALCULATION CODE GOES HERE *******
      ;*******************************************************
;      IF FILE_BASENAME(groupS3files[j]) EQ '20130527.396_LDFC.fits' THEN STOP


      ng = 1                                                          ;Only one zone for PSF model
      FXADDPAR, imgHeader, 'HISTORY', "PPOL-Plus Step 4"
      SXADDPAR, imgHeader, 'PSF_NG', ng, "Number of X, Y grid zones in psf build"
      SXADDPAR, imgHeader, 'PSF_SIG', nsigma, "Detection threshold in sigma of sky"
      SXADDPAR, imgHeader, 'PSF_SAT', sat_limit, "Saturation Limit for allowed stars"
      SXADDPAR, imgHeader, 'PSF_FWHM', mean_FWHM,"FWHM in pixels"
      SXADDPAR, imgHeader, 'PSF_FRAD', fit_rad, "PSF fitting radius, in pixels"
      SXADDPAR, imgHeader, 'PSF_RSCL', rad_scale, "PSF model size, in FWHM units"
      ;
      SXADDPAR, imgHeader, 'PSFMAG', PSF_mag,"nominal PSF magnitude generated internally"
      ;
      SXADDPAR, imgHeader, 'NPSF', n_total_PSF_stars
;      nPSF = n_total_PSF_stars
      FOR inPSF = 0, n_total_PSF_stars - 1 DO BEGIN
        PSFnum = STRING((inPSF+1), format='("PSF_",I03)')
        SXADDPAR, imgHeader, PSFnum+"X",xPhot[idpsf[inPSF]],"X location of this PSF star"
        SXADDPAR, imgHeader, PSFnum+"Y",yPhot[idpsf[inPSF]],"Y location of this PSF star"
        SXADDPAR, imgHeader, PSFnum+"M",mag[idpsf[inPSF]],"Magnitude of this PSF star"
      ENDFOR
      ;
      ; add aperture phot info
      ;
      SXADDPAR, imgHeader, 'PP4_FWHM', mean_FWHM, "FWHM used in aperture phot, in pixels"
      SXADDPAR, imgHeader, 'PP4_ARAD', ap_rad, "Maximum aperture radius in FWHM units"
      SXADDPAR, imgHeader, 'PP4_REF0', ref_ap[0],"Reference aperture inner radius in FWHM units"
      SXADDPAR, imgHeader, 'PP4_REF1', ref_ap[1],"Reference aperture outer radius in FWHM units"
     
      ;***************** THIS IMPLIES THAT THE PSF WAS CELANED *****************************
      SXADDPAR, imgHeader, 'PP4_CLEN', clean_rad, "Radius to PSF clean to, in FWHM * ARAD units"
      ;***************** SHOULD SOMEDAY INCLUDE CLEAN PSF CODE *****************************
     
      SXADDPAR, imgHeader, 'PP4_NAP', n_ap,"Number of apertures used in Ap Phot"
      FOR iap = 0, n_ap-1 DO BEGIN
        name = 'PP4_AP'+STRING(iap,format='(I02)')
        SXADDPAR, imgHeader, name, ap_vec[iap], "Aperture "+STRING(iap+1,format='(I2)')+" in FWHM * ARAD units"
      ENDFOR
      ;
      ; and sky background and sigma info
      ;
      SXADDPAR, imgHeader, 'PP4_SKY', skymean, 'Mean sky value'
      SXADDPAR, imgHeader, 'PP4_SSKY', skysig, 'Sky standard deviation'
      SXADDPAR, imgHeader, 'PP4_LLQ', llq, 'LLQ bad flag (1=bad; 0=good)'
;      SXADDHIST, "PPOL-Plus Step 4", imgHeader

      ; write modified header, then phot info
      ;
      output_path = 'S4_photometry_rewrites' + PATH_SEP() + $
        FILE_BASENAME(groups3files[j], '.fits') + '_phot.dat'         ;Define the output filename
      OPENW, lun, output_path, /GET_LUN                               ;Open the file logical unit
      n_header = N_ELEMENTS(imgHeader)
      FOR ih = 0, n_header - 1 DO BEGIN                               ;Write the header to file
        PRINTF, lun, format='(";;",A)', imgHeader[ih]
      ENDFOR
      part1 = ";; Star    X       Y      RA         Dec     Sky   Group   "
      part2 =  "Mag[0]  Mag[1]  Mag[2]  Mag[3]  Mag[4]  Mag[5]  Mag[6]  Mag[7]  Mag[8]  Mag[9]  Mag[10] Mag[11]  "
      part3 = "sMag[0] sMag[1] sMag[2] sMag[3] sMag[4] sMag[5] sMag[6] sMag[7] sMag[8] sMag[9]sMag[10]sMag[11]"
      all_parts = part1 + part2 + part3
      PRINTF, lun, format='(a)', all_parts                            ;Write photometry column headers
      FOR is = 0, nStars - 1 DO BEGIN                                 ;Loop through each star and find its gcentrd value
        if(photoDecs1[is] gt 0.0) then dsign = '+' else dsign = '-'   ;Format the sign of the declination
        d = abs(photoDecs1[is])
        dten = FLOOR(d/10)
        d = d - 10 * dten
        PRINTF, lun, FORMAT='(I6,1x,F7.2,1x,f7.2,1x,f10.6,1x,a1,i1.1,f8.6,1x,f8.2,1x,i4,1x,12(f7.4,1x),12(f7.4,1x))',$
          is,xPhot[is],yPhot[is],photoRAs1[is],dsign,dten,d,$
          skyvalues[is],group_vector[is],mag[*,is],err[*,is]
      ENDFOR
      FREE_LUN, lun
    ENDIF
  ENDFOR
  PRINT, 'Done with group', (i+1)
ENDFOR

PRINT, 'Completed inital photometry for all gorups!'

END