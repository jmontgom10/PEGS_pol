;This script will read in the .FITS image for each field in a group,
;read in the step 4 photometry file,
;loop through the photometry header data,
;pull out the PSF star postions,
;and overplot the PSF stars in the .FITS image
;

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
groupBDPfile  = ''
groupBDPfiles = ''
WINDOW, 0, XS=800, YS=800
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
    ;Read in the photometry header
    OPENR, lun, groupS4files[j], /GET_LUN
    fullHead = ''
    line     = ''
    WHILE ~EOF(lun) do begin
      readf, lun, line, format='(a)'
      test = STRPOS(line, ';;')
      if(test ne -1) then begin
        fullHead = [fullHead, STRMID(line,2,STRLEN(line))]
      endif
    ENDWHILE
    FREE_LUN, lun
    fullHead = fullHead[1:*]
    
    PSFx = FLTARR(1)
    PSFy = FLTARR(1)
    PSFstarExists = 1
    starNumber    = 1
    WHILE PSFstarExists DO BEGIN
      PSFx = [PSFx, SXPAR(fullHead, STRING(starNumber, FORMAT='("PSF_",I03,"X")'))]
      PSFy = [PSFy, SXPAR(fullHead, STRING(starNumber, FORMAT='("PSF_",I03,"Y")'))]
      IF (PSFx[starNumber] EQ 0) AND (PSFy[starNumber] EQ 0) $
        THEN PSFstarExists = 0 ELSE starNumber++
    ENDWHILE
    PSFx = PSFx[1:*]
    PSFy = PSFy[1:*]

    ;Read in the image for final display
    img = READFITS(groupS3files[j])
    SKY, img, skyMode, skyNoise
    logRange = ALOG(skyMode + [-1, 1E1]*skyNoise)
    TVIM, ALOG(img), RANGE=logRange
    OPLOT, PSFx, PSFy, PSYM = 6, COLOR=255L*255L
    stop
  ENDFOR
ENDFOR

END