;This script will read in images and STEP4 *phot.dat files
;Stars in the corresponding *phot.dat file will be overplotted on the galaxy image for examination
;

groupDir = DIALOG_PICKFILE(TITLE='Select the galaxy+project folder', /DIRECTORY)
testDir  = FILE_TEST(groupDir, /DIRECTORY)
IF ~testDir THEN STOP ELSE CD, groupDir

testDir   = FILE_TEST('S4_photometry_rewrites', /DIRECTORY)
IF ~testDir THEN FILE_MKDIR, 'S4_photometry_rewrites'

galMask = READFITS('galMask.fits', galMaskHeader)
maskSZ  = SIZE(galMask, /DIMENSIONS)
nx      = maskSZ[0]
ny      = maskSZ[1]
RA_cen  = SXPAR(galMaskHeader, 'RA_CEN')
Dec_cen = SXPAR(galmaskHeader, 'Dec_CEN')

;Read in the PPOL directory for this project
ppolDir  = ''
OPENR, lun, 'ppolDir.dat', /GET_LUN
READF, lun, ppolDir
FREE_LUN, lun
S3directory = ppolDir + 'S3_Astrometry' + PATH_SEP()
S4directory = ppolDir + 'S4_First_PSF_Fits' + PATH_SEP()

mask    = READFITS('fullMask.fits', maskHeader)
RA_cen  = SXPAR(maskHeader, 'RA_CEN')
Dec_cen = SXPAR(maskHeader, 'Dec_CEN')

;Read in all of the BDP file names
groupFiles    = FILE_SEARCH('groups' + PATH_SEP() + '*.dat', COUNT=numGroups)
groupBDPfile  = ''
groupBDPfiles = ''
;WINDOW, 0, XS=800, YS=800
FOR i = 0, numGroups - 1 DO BEGIN
  groupBDPfile  = ''
  groupBDPfiles = ''
  groupS3files  = ''
  groupS4files  = ''
  OPENR, lun, groupFiles[i], /GET_LUN
  WHILE ~EOF(lun) DO BEGIN
    READF, lun, groupBDPfile
    S3test = FILE_TEST(S3directory + FILE_BASENAME(groupBDPfile))
    S4test = FILE_TEST(S4directory + FILE_BASENAME(groupBDPfile, '.fits') + '_phot.dat')
    IF S3test AND S4test THEN BEGIN
      groupBDPfiles = [groupBDPfiles, groupBDPfile]
      groupS3files  = [groupS3files, S3directory + FILE_BASENAME(groupBDPfile)]
      groupS4files  = [groupS4files, S4directory + FILE_BASENAME(groupBDPfile, '.fits') + '_phot.dat']
    ENDIF
  ENDWHILE
  FREE_LUN, lun
  groupBDPfiles = groupBDPfiles[1:*]
  groupS3files  = groupS3files[1:*]
  groupS4files  = groupS4files[1:*]
  
  FOR j = 0, N_ELEMENTS(groupBDPfiles) - 1 DO BEGIN
    imgHeader = HEADFITS(groupS3files[j])
;    img       = READFITS(groupS3files[j])
;    SKY, img, skyMode, skyNoise
;    logRange = ALOG(skyMode + [-1, 1E1]*skyNoise)
    ;
    ; determine shift of mask
    ;
    ; extract astrometry from header
    ;
    extast, imgHeader, astrom
    ;
    ; get x, y location of galaxy center in pixels
    ;
    ad2xy, RA_cen, Dec_cen, astrom, xc, yc
    ;
    ra_off = ROUND(xc - 512)
    dec_off = ROUND(yc - 513)
    print, "mask shift = ",ra_off, dec_off," in RA, Dec directions"
    ;
    ; cycle mask by this amount
    ;
    IF ra_off LT 0 THEN $
      this_mask = [galMask[abs(ra_off):*,*], fltarr(abs(ra_off), ny)] $
    ELSE IF ra_off GT 0 THEN $
      this_mask = [fltarr(ra_off, ny), galMask[0:(nx-1-ra_off),*]] ELSE $
      this_mask = galMask
    IF dec_off LT 0 THEN $
      this_mask = [[this_mask[*,abs(dec_off):*]], [fltarr(nx, abs(dec_off))]] $
    ELSE IF dec_off GT 0 THEN $
      this_mask = [[fltarr(nx, dec_off)], [this_mask[*,0:(ny-1-dec_off)]]] ELSE $
      this_mask = this_mask
      
    ;Read in the photometry header
    OPENR, lun, groupS4files[j], /GET_LUN
    fullHead = ''
    line     = ''
    nskip    = 0
    WHILE ~EOF(lun) do begin
      readf, lun, line, format='(a)'
      test = STRPOS(line, ';;')
      if(test ne -1) then begin
        fullHead = [fullHead, line]
        nskip++
      endif
    ENDWHILE
    FREE_LUN, lun
    fullHead = fullHead[1:*]
    
    READCOL, groupS4files[j], id, x, y, ra, dec, SKIPLINE=nskip, $
      FORMAT='L,D,D,D,D,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X', /SILENT
    goodInd = WHERE(~this_mask[ROUND(x), ROUND(y)], numGood)

;    TVIM, ALOG(img), RANGE=logRange
;    OPLOT, x[goodInd], y[goodInd], PSYM = 6, COLOR=255L*255L    
;    stop
    filename = 'S4_photometry_rewrites' + PATH_SEP() + FILE_BASENAME(groupS4files[j])
    OPENW, lun, filename, /GET_LUN
    FOR k = 0, N_ELEMENTS(fullHead) - 1 DO BEGIN
      PRINTF, lun, fullHead[k]
    ENDFOR
    FOR k = 0, numGood - 1 DO BEGIN
      gi = goodInd[k]
      PRINTF, lun, FORMAT='(I6,1x,2(F7.2,1x),2(F10.6,1x),1x,F6.2,1x,I3.1,3x,12(F7.4,1x),12(F7.4,1x))',$
        k, x[gi], y[gi], ra[gi], dec[gi], sky[gi], grp[gi], $
        mag0[gi], mag1[gi], mag2[gi], mag3[gi], mag4[gi], mag5[gi], mag6[gi], mag7[gi], mag8[gi], mag9[gi], mag10[gi], mag11[gi],$
        smag0[gi],smag1[gi],smag2[gi],smag3[gi],smag4[gi],smag5[gi],smag6[gi],smag7[gi],smag8[gi],smag9[gi],smag10[gi],smag11[gi]
    ENDFOR
    FREE_LUN, lun
  ENDFOR
  PRINT, 'Completed group', (i+1)
ENDFOR

END