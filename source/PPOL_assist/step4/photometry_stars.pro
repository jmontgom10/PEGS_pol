;This script loops through the group coadds,
;retrieves the 2MASS catalog entries, and checks if they are
;(1) Inside a shifted galaxy mask from the supersky steps
;(2) Outside the overlapping dither region
;(3) Brighter than 13.5 magnitues (this value may be adjusted depending on results
;Stars which pass these tests and are saved as "photometry stars"
;Stars which pass this test and are within two magnitudes of the brightest unsaturated stars
;are saved as PSF fitting stars (this may not be used depending on if the photometry stars need neighbors removed.
;

groupDir = DIALOG_PICKFILE(TITLE='Select the galaxy+project folder', /DIRECTORY)
testDir  = FILE_TEST(groupDir, /DIRECTORY)
IF ~testDir THEN STOP ELSE CD, groupDir


galMask   = READFITS('galMask.fits', maskHeader)
maskSz    = SIZE(galMask, /DIMENSIONS)
RA_cen    = SXPAR(maskHeader, 'RA_CEN')
Dec_cen   = SXPAR(maskHeader, 'Dec_CEN')

;CD, 'groups'
coaddFiles = FILE_SEARCH('S4_Photometry_Files', 'quickCoadd*.fits', COUNT = numCoadds)
;CD, '..'

FOR i = 0, numCoadds - 1 DO BEGIN
  coaddImg    = READFITS(coaddFiles[i])
  coaddHeader = HEADFITS(coaddFiles[i])
  EXTAST, coaddHeader, astr, noparams
  nx         = astr.naxis[0]                                ;Grab coadd image x-axis size
  ny         = astr.naxis[1]                                ;Grab coadd image y-axis size
  padGalMask = galMask                                      ;Alias the galaxy mask for manipulation
  IF nx GT maskSz[0] THEN BEGIN
    padX         = (nx - maskSz[0])/2            ;Compute the padding to add on each side
    padArr       = FLTARR(padX, maskSz[1])                  ;Create the padding array
    padGalMask   = [padArr, padGalMask, padArr]             ;Pad the mask
  ENDIF
  IF ny GT maskSz[1] THEN BEGIN
    padY         = (ny - maskSz[1])/2            ;Compute the padding to add on each side
    padArr       = FLTARR(nx, padY)              ;Create the padding array
    padGalMask   = [[padArr], [padGalMask], [padArr]]       ;Pad the mask
  ENDIF

  AD2XY, RA_cen, Dec_cen, astr, xc, yc
  
  ra_off  = ROUND(xc - astr.crpix[0])                       ;Find the x shift for the mask
  dec_off = ROUND(yc - astr.crpix[0])                       ;Find the y shift for the mask
  print, "mask shift = ",ra_off, dec_off," in RA, Dec directions"
  ;
  ; cycle mask by this amount
  ;
  IF ra_off LT 0 THEN $
    this_mask = [padGalMask[abs(ra_off):*,*], fltarr(abs(ra_off), ny)] $
  ELSE IF ra_off GT 0 THEN $
    this_mask = [fltarr(ra_off, ny), padGalMask[0:(nx-1-ra_off),*]] ELSE $
    this_mask = padGalMask
  IF dec_off LT 0 THEN $
    this_mask = [[this_mask[*,abs(dec_off):*]], [fltarr(nx, abs(dec_off))]] $
  ELSE IF dec_off GT 0 THEN $
    this_mask = [[fltarr(nx, dec_off)], [this_mask[*,0:(ny-1-dec_off)]]] ELSE $
    this_mask = this_mask
  
  lfTrim = SXPAR(coaddHeader, 'LFTRIM')                      ;Extract the dither overlap region
  rtTrim = SXPAR(coaddHeader, 'RTTRIM')
  btTrim = SXPAR(coaddHeader, 'BTTRIM')
  tpTrim = SXPAR(coaddHeader, 'TPTRIM')
  
  xx = REBIN(REFORM(INDGEN(nx), nx, 1), nx, ny, /SAMPLE)
  yy = REBIN(REFORM(INDGEN(ny), 1, ny), nx, ny, /SAMPLE)
  border_mask = 1 - (xx GT lfTrim AND xx LT rtTrim $        ;Create a mask for the border trim
    AND yy GT btTrim AND yy LT tpTrim)
  this_mask   = this_mask OR border_mask                    ;Supplement mask with the border trim
  
  ;Retrieve the 2MASS star info
  this_mirror = ['CfA', 'UK', 'CDS', 'CA', 'Jp']
  
  ;
  nloop = 0L
  WHILE nloop LT 4320L DO BEGIN                             ; 1 day at 20 sec per try
    ;
    vizier_flag = 0
    FOR imirror = 0, N_ELEMENTS(this_mirror)-1 DO BEGIN
      IF(vizier_flag EQ 0) THEN BEGIN
        info = mp_QueryVizier('2MASS-PSC', astr.crval, 10, $
          MIRROR=this_mirror[imirror], constraint='Qflg==AAA')
        test = size(info)
        IF test[0] NE 0 THEN vizier_flag = 1
      ENDIF
    ENDFOR
    
    IF(vizier_flag EQ 0) THEN BEGIN
      info = 99   ;No Vizier servers available
      ;
      ; if no Vizier servers, wait 20s and retry
      ;
      PRINT, 'No Vizier Servers at ',SYSTIME(),' waiting 20s and retrying'
      WAIT, 20
      nloop++
    ENDIF ELSE nloop = 4321L                                ;Force the loop to close
  ENDWHILE
  
  AD2XY, info.RAJ2000, info.DEJ2000, astr, xStars, yStars   ;Compute x and y positions
  goodStars = WHERE(info.Hmag LT 13.0 AND info.Hmag GT 9.2 AND $
    (1 - this_mask)[ROUND(xStars), ROUND(yStars)], numGood)
  IF numGood GT 0 THEN info = info[goodStars] ELSE STOP     ;Toss out dim or masked stars
  info = info[SORT(info.Hmag)]                              ;Sort by brightness
;  AD2XY, info.RAJ2000, info.DEJ2000, astr, xStars, yStars   ;Recompute x and y positions

;  sky, coaddImg, skymode, skynoise
;  TVIM, (1 - this_mask)*coaddImg, RANGE = skymode + [-3, +10]*skynoise
;  OPLOT, xStars, yStars, PSYM=4, COLOR=255L*255L
;  STOP
  
  filename = 'S4_Photometry_Files' + PATH_SEP() + $         ;Generate the photometry filename
    STRING(FORMAT='("photometryStars",I02,".dat")', (i+1))
  OPENW, lun, filename, /GET_LUN
  FOR j = 0, numGood - 1 DO BEGIN
    PRINTF, lun, STRING(FORMAT='(I02,4X,2(D9.5,4X))', $     ;Print the RA and Dec values to file
      j, info[j].RAJ2000, info[j].DEJ2000)
  ENDFOR
  FREE_LUN, lun
  
  delMag = 3
  maxMag = info[0].Hmag + delMag
  PSFind = WHERE(info.Hmag LT maxMag, numPSF)               ;Select viable PSF stars
  
  filename = 'S4_Photometry_Files' + PATH_SEP() + $         ;Generate the photometry filename
    STRING(FORMAT='("PSFstars",I02,".dat")', (i+1))
  OPENW, lun, filename, /GET_LUN
  FOR j = 0, numPSF - 1 DO BEGIN
    PRINTF, lun, STRING(FORMAT='(I02,4X,2(D9.5,4X))', $     ;Print the RA and Dec values to file
      j, info[j].RAJ2000, info[j].DEJ2000)
  ENDFOR
  FREE_LUN, lun

  PRINT, 'Completed group', (i+1)
ENDFOR
PRINT, 'Done with all groups!'
END