;This script will read in the photometry files produced by the "do_photometry" routine
;and display the results to the user. If the magnitues and/or the sky noise profiles
;behave strangely, then one should be a little suspect of the results.
;


groupDir = DIALOG_PICKFILE(TITLE='Select the galaxy+project folder', /DIRECTORY)
testDir  = FILE_TEST(groupDir, /DIRECTORY)
IF ~testDir THEN STOP ELSE CD, groupDir

;Read in the PPOL directory for this project
ppolDir  = ''
OPENR, lun, 'ppolDir.dat', /GET_LUN
READF, lun, ppolDir
FREE_LUN, lun
S4directory = 'S4_photometry_rewrites' + PATH_SEP()

;Read in all of the BDP file names
groupFiles    = FILE_SEARCH('groups' + PATH_SEP() + '*.dat', COUNT=numGroups)

;window, 0, XSIZE = 800, YSIZE = 800
window, 1, XSIZE = 800, YSIZE = 600
;Loop through each group and read in the file lists for each group
FOR i = 1, numGroups - 1 DO BEGIN
  groupBDPfile  = ''                                                  ;Initalize strings for reading file contents
  groupBDPfiles = ''
  groupS4files  = ''
  OPENR, lun, groupFiles[i], /GET_LUN
  WHILE ~EOF(lun) DO BEGIN
    READF, lun, groupBDPfile
    S4file = S4directory + FILE_BASENAME(groupBDPfile, '.fits') + '_phot.dat'
    S4test = FILE_TEST(S4file)                                        ;Test if the step 4 file is present
    IF S4test THEN BEGIN
      groupBDPfiles = [groupBDPfiles, groupBDPfile]
      groupS4files = [groupS4files, S4file]                           ;Concatenate filenames
    ENDIF
  ENDWHILE
  FREE_LUN, lun
  groupS4files = groupS4files[1:*]                                  ;Trim the leading null entry
  groupBDPfiles = groupBDPfiles[1:*]
  
;  window, 0
;  window, 1
;  FOR j = 0, N_ELEMENTS(groupBDPfiles) - 1 DO BEGIN
;    wset, 0
;    img = READFITS(groupBDPfiles[j])
;    sky, img, skymode, skynoise
;    TVIM, img, RANGE = skymode  + [-3,+20]*skynoise
;    stop
;  ENDFOR
;  STOP

  FOR j = 0, N_ELEMENTS(groupS4files) - 1 DO BEGIN
    wset, 1
    READCOL, groupS4files[j], COMMENT = ';', $
      FORMAT = 'X,X,X,X,X,X,X,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F', $
      mag0,  mag1,  mag2,  mag3,  mag4,  mag5,  mag6,  mag7,  mag8,  mag9,  mag10,  mag11, $
      smag0, smag1, smag2, smag3, smag4, smag5, smag6, smag7, smag8, smag9, smag10, smag11
     
    mags  = TRANSPOSE([[mag0],[mag1],[mag2],[mag3],[mag4],[mag5],[mag6],[mag7],[mag8],[mag9],[mag10],[mag11]])
    sMags = TRANSPOSE([[smag0],[smag1],[smag2],[smag3],[smag4],[smag5],[smag6],[smag7],[smag8],[smag9],[smag10],[smag11]])
    
    numStars   = N_ELEMENTS(mags[0,*])
    normalMags = mags - REBIN(mags[0,*], 12, numStars)
    deltaColor = 255/(numStars)
    PLOT, [0, 14], [MIN(normalMags), MAX(normalMags)], /NODATA, YSTYLE=1, $
      YTITLE = 'Delta Mag', XTITLE = 'Aperture No.'
;    PLOT, [0, 14], [MIN(sMags), MAX(sMags)], /NODATA, YSTYLE=1, $
;      YTITLE = 'sigma Mag', XTITLE = 'Aperture No.'
    PLOTS, [0,12], [0,0]
    LOADCT, 13
    FOR k = 0, numStars - 1 DO BEGIN
;      plotMags = FLTARR(12)
;      FOR, l = 0, 12 DO BEGIN
;        plotMags[l] 
;      ENDFOR
      OPLOTERROR, (mags[*,k] - mags[0,k]), smags[*,k], PSYM=-4, $
        COLOR = (k+1)*deltaColor, ERRCOLOR = (k+1)*deltaColor
      XYOUTS, 11.5, (mags[11,k] - mags[0,k]), $
        STRING(FORMAT='("#",I2," mag(0) =",F5.1)', k+1, mags[0,k]), COLOR = (k+1)*deltaColor
;      OPLOT, smags[*,k], PSYM=-4, COLOR = (k+1)*deltaColor
;      XYOUTS, 11.5, smags[11,k], $
;        STRING(FORMAT='("#",I2," mag(0) =",F5.1)', k+1, mags[0,k]), COLOR =(k+1)*deltaColor
    ENDFOR
    LOADCT, 0
    STOP
      
  ENDFOR
  STOP
ENDFOR

END