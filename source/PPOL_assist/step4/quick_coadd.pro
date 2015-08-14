PRO QUICK_COADD, groupS3files, out_img, out_header

;  nDithers   = 6                                            ;Number of dithers in the Mimir script
;  coaddInds  = INTARR(nDithers)                             ;Array to store indices of the images to use in coadd
;  coaddCount = 0                                            ;Count the counted coadd images

  
  hwp = fltarr(nfiles)
  for i = 0, nfiles-1 do begin
    header = headfits(astrometryFiles[i])
    hwp[i] = SXPAR(header, "MB_HWP")
  endfor
  ;
  ; how many of each kind?
  ;
  ind = SORT(hwp)
  hwp_uniq = hwp(ind[uniq(hwp[ind])])
  n_hwp = N_ELEMENTS(hwp_uniq)
  print, "Found ",n_hwp," different HWP angles: ",hwp_uniq
  
  FOR j = 0, N_ELEMENTS(groupS3files) - 1 DO BEGIN
    IF FILE_TEST(groupS3files[j]) EQ 0 THEN CONTINUE
    IF coaddCount EQ 0 THEN BEGIN
      coaddInds[coaddCount] = j
      tmpHeader             = HEADFITS(groupS3files[j])
      centerRA              = SXPAR(tmpHeader,'CRVAL1')
      centerDec             = SXPAR(tmpHeader,'CRVAL2')
      coaddCount++
    ENDIF ELSE BEGIN
      tmpHeader  = HEADFITS(groupS3files[j])
      centerRA1  = SXPAR(tmpHeader,'CRVAL1')
      centerDec1 = SXPAR(tmpHeader,'CRVAL2')
      ditherDist = SQRT(((centerRA - centerRA1)*COS(centerDec1*!DTOR))^2 + $
        (centerDec - centerDec1)^2)*3600D
      IF ditherDist GT 10 THEN BEGIN
        coaddInds[coaddCount] = j
        centerRA              = centerRA1
        centerDec             = centerDec1
        coaddCount++
      ENDIF
    ENDELSE
  ENDFOR
  
  coaddFiles = groupS3files[coaddInds]
  PRINT, 'Reading in astrometry for all files...'
  FOR j = 0, nDithers - 1 DO BEGIN
    tmpHeader = HEADFITS(coaddfiles[j])                     ;Read in this file's header
    EXTAST, tmpHeader, astr, noparams                       ;Extract astrometry from header
    IF (N_ELEMENTS(all_astr) EQ 0) THEN BEGIN               ;Store astrometry (test for first case)
      all_astr = astr
    ENDIF ELSE BEGIN
      all_astr = [all_astr, astr]
    ENDELSE
  ENDFOR
  
  PRINT, "Finding the center image..."
  
  ;Find the deviations from the median pointing **** SHOULD USE MAX - MIN POINTING ****
  del_RA  = all_astr.crval[0] - MEDIAN(all_astr.crval[0])
  del_DEC = all_astr.crval[1] - MEDIAN(all_astr.crval[1])
  del_th  = SQRT((del_RA*COS(del_DEC))^2 + del_DEC^2)
  
  
  ;Select the minimum deviation and use that as the "center image"
  center_img_ind = (WHERE(del_th EQ MIN(del_th)))[0]
  
  ;Find the x_offsets and y_offsets from the center image
  AD2XY, all_astr.crval[0], all_astr.crval[1], all_astr[center_img_ind], mapX, mapY
  x_offsets = ROUND(mapX - all_astr[center_img_ind].crpix[0])
  y_offsets = ROUND(mapY - all_astr[center_img_ind].crpix[1])
  ctr_shift = [y_offsets[center_img_ind], y_offsets[center_img_ind]]
  x_offsets = x_offsets - ctr_shift[0]                                ;Correct for .FITS vs. IDL pixel counting
  y_offsets = y_offsets - ctr_shift[1]

  ;Find the extreema of the offsets
  xmin = MIN(x_offsets)
  xmax = MAX(x_offsets)
  ymin = MIN(y_offsets)
  ymax = MAX(y_offsets)
  
  ;********I NEED TO RETHINK THIS... WOULD WOULD WORK EVERY TIME?**********
  ;Calculate the extra array space needed to store the input image
  dx = xmax - xmin
  dy = ymax - ymin
  
  ;Increase the image size by the necessary ammount
  new_naxis1 = MAX(all_astr.naxis[0]) + dx + 1                        ;Compute the necessary coadd image size
  new_naxis2 = MAX(all_astr.naxis[1]) + dy + 1
  new_crpix1 = new_naxis1/2                                           ;Compute the new center pixel
  new_crpix2 = new_naxis2/2

  ;Calculate where the new x_center and y_center are in RA and DEC,
  XY2AD, (xmin + new_crpix1 + ctr_shift[0]), $                        ;Location of new image center
    (ymin + new_crpix2 + ctr_shift[0]), $
    all_astr[center_img_ind], ra_cen, dec_cen                         ;Output of the new image center CRVALs
  finalAstr       = all_astr[center_img_ind]                          ;Store the final astrometry
  finalAstr.crpix = [new_crpix1, new_crpix2]                          ;Update the final astrometry
  finalAstr.crval = [ra_cen, dec_cen]

  ;Find the x_offsets and y_offsets from the center image of the new image
  AD2XY, all_astr.crval[0], all_astr.crval[1], finalAstr, mapX, mapY
  x_offsets = ROUND(mapX - finalAstr.crpix[0])
  y_offsets = ROUND(mapY - finalAstr.crpix[1])
  x_offsets = x_offsets - ctr_shift[0]                                ;Correct for .FITS vs. IDL pixel counting
  y_offsets = y_offsets - ctr_shift[1]
  
  ;Create an image into which the combined data will be fed
  stack_img   = FLTARR(new_naxis1, new_naxis2, nDithers)              ;store the images in massive arrays
  stack_count = INTARR(new_naxis1, new_naxis2)
  
  PRINT, "Stacking input images into 3D array at "
  
  ;Stack the images into a 3D array
  lfTrim = 0                                                          ;Initalize variables to keep track of
  rtTrim = new_naxis1                                                 ;the boundaries of the area
  btTrim = 0                                                          ;common to ALL images
  tpTrim = new_naxis2
  FOR j = 0, nDithers - 1 DO BEGIN
    ;Read in the input image
    in_img  = READFITS(coaddFiles[j], tmp_header, /SILENT)
    
    ;Find the boundaries to which the input image maps
    lf_in = new_crpix1 + x_offsets[j] - 0.5*all_astr[j].naxis[0] + 1
    rt_in = lf_in + all_astr[j].naxis[0]
    bt_in = new_crpix2 + y_offsets[j] - 0.5*all_astr[j].naxis[1] + 1
    tp_in = bt_in + all_astr[j].naxis[1]
    
    ;Test if these further restrict the overlapping region of the dither
    IF lf_in GT lfTrim THEN lfTrim = lf_in
    IF rt_in LT rtTrim THEN rtTrim = rt_in
    IF bt_in GT btTrim THEN btTrim = bt_in
    IF tp_in LT tpTrim THEN tpTrim = tp_in

    ;Fill in one layer of the stack
    stack_img[lf_in:rt_in - 1, bt_in:tp_in - 1, j] = in_img
    stack_count[lf_in:rt_in - 1, bt_in:tp_in - 1] += 1
  ENDFOR
  lfTrim += 8 & rtTrim -= 8 & btTrim += 24 & tpTrim -= 24             ;Buffer the edge of the images to allow photometry
  
  emptyPix = WHERE(stack_count EQ 0, numEmpty)                        ;Locate the empty pixels
  IF numEmpty GT 0 THEN   stack_count[emptyPix] = 1                   ;Fill in empty pixels to avoid arithmetic error
  out_img = TOTAL(stack_img, 3)/stack_count                           ;Compute the average image

  IF (new_naxis1 MOD 2) EQ 1 THEN BEGIN                               ;Ensure that the image is an even number of pixels
    out_img     = [out_img, FLTARR(1,new_naxis2)]
    new_naxis1 +=1
  ENDIF
  IF (new_naxis2 MOD 2) EQ 1 THEN BEGIN
    out_img     = [[out_img], [FLTARR(new_naxis1)]]
    new_naxis2 +=1
  ENDIF

  
  MKHDR, out_header, out_img
  PUTAST, out_header, finalAstr
  SXADDPAR, out_header, 'LFTRIM', lfTrim, 'Left edge of the accepted overlapping region'
  SXADDPAR, out_header, 'RTTRIM', rtTrim, 'Right edge of the accepted overlapping region'
  SXADDPAR, out_header, 'BTTRIM', btTrim, 'Bottom edge of the accepted overlapping region'
  SXADDPAR, out_header, 'TPTRIM', tpTrim, 'Top edge of the accepted overlapping region'
  
;  filename = 'S4_Restricted_photometry' + PATH_SEP() + $            ;Store the output filename
;    'coadds' + PATH_SEP() + groupName + '_quickCoadd.fits'
;  WRITEFITS, filename, out_img, out_header
END