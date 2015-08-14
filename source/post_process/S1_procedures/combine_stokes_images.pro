PRO COMBINE_STOKES_IMAGES, info_structure
  ; Begin by simplifying references to the input/output directories, etc...
  tlb_wid = WIDGET_INFO(event.top, FIND_BY_UNAME='WID_BASE')
  WIDGET_CONTROL, tlb_wid, GET_UVALUE=groupStruc
  inDir   = groupStruc.analysis_dir + PATH_SEP() + 'S11_Full_Field_Polarimetry' + PATH_SEP()
  outDir  = groupStruc.analysis_dir + PATH_SEP() + 'S11B_Full_Field_Polarimetry' + PATH_SEP()
  stokes  = info_structure.stokes_param
  objName = info_structure.object_name


  PRINT, "Gathering file paths..."
  
  IF stokes EQ "Normalized U" OR stokes EQ "Normalized Q" THEN BEGIN
    valueName    = "*_" + STRMID(stokes, 0,1, /REVERSE_OFFSET) + "_cor.fits" ;stokes value name
    sigmaName    = "*s" + STRMID(stokes, 0,1, /REVERSE_OFFSET) + "_cor.fits" ;stokes uncertainty name
    valueFiles   = FILE_SEARCH(inDir, valueName, COUNT = nValFiles)   ;List of stokes value files
    sigmaFiles   = FILE_SEARCH(inDir, sigmaName, COUNT = nSigFiles)   ;List of stokes uncertainty files
    sigma        = 1                                                  ;flag for inverse variance weighting
  ENDIF ELSE BEGIN
    valueName  = "*Intensity.fits"
    valueFiles = FILE_SEARCH(inDir, valueName, COUNT = nValFiles)
  ENDELSE

  PRINT, "Generating combined header"
  CREATE_COMBINED_HEADER, valueFiles, info_structure, all_astr, combined_header

  ; Grab the astrometry from the newly created header
  EXTAST, combined_header, new_astr,  noparams
  new_naxis1 = new_astr.naxis[0]                                      ;store the new image size
  new_naxis2 = new_astr.naxis[1]
  new_crpix1 = new_astr.crpix[0]                                      ;store the new reference value
  new_crpix2 = new_astr.crpix[1]
  
  ;Create an image into which the combined data will be fed
  n_files   = N_ELEMENTS(valueFiles)                                  ;Count the files
  stack_img = FLTARR(new_naxis1, new_naxis2, n_files)                 ;store the images in massive arrays
  stack_sig = FLTARR(new_naxis1, new_naxis2, n_files)                 ;store the uncertainties in massive arrays

  ;Find the x_offsets and y_offsets from the center image
  AD2XY, all_astr.crval[0], all_astr.crval[1], new_astr, mapX, mapY
  x_offsets = ROUND(mapX - new_astr.crpix[0])
  y_offsets = ROUND(mapY - new_astr.crpix[1])
  
  ;******WILL I NEED TO WORRY ABOUT CTR_SHIFT VALUE?******
  ;Correct for any shifting errors(***???***)
  ;ctr_shift = [y_offsets[center_image], y_offsets[center_image]]
  ;x_offsets = x_offsets - ctr_shift[0]            ;Correct for weird AD2XY screw-up
  ;y_offsets = y_offsets - ctr_shift[1]
  
  PRINT, "Stacking input images into 3D array at " + STRMID(SYSTIME(), 11, 8) + "... Please be patient."
  WAIT, 0.25

  ;Stack the images into a 3D array
  FOR i = 0, n_files - 1 DO BEGIN
    ;Read in the input image
    in_img  = READFITS(valueFiles[i], tmp_header, /SILENT)
    
    ;Find the boundaries to which the input image maps
    lf_in = ROUND(new_crpix1 + x_offsets[i] - 0.5*all_astr[i].naxis[0])
    rt_in = ROUND(new_crpix1 + x_offsets[i] + 0.5*all_astr[i].naxis[0])
    bt_in = ROUND(new_crpix2 + y_offsets[i] - 0.5*all_astr[i].naxis[1])
    tp_in = ROUND(new_crpix2 + y_offsets[i] + 0.5*all_astr[i].naxis[1])
    ;   PRINT, SIZE(stack_img, /dim)
    ;   print, lf_in, rt_in, bt_in, tp_in
    ;Fill in one layer of the stack
    stack_img[lf_in:rt_in - 1, bt_in:tp_in - 1, i] = in_img
    IF KEYWORD_SET(sigma) THEN BEGIN
      sig_im  = READFITS(sigmaFiles[i], /SILENT)
      stack_sig[lf_in:rt_in - 1, bt_in:tp_in - 1, i] = sig_im
    ENDIF
  ENDFOR
  
  PRINT, "Beginning the averaging procedure at " + STRMID(SYSTIME(), 11, 8) + "... Please be patient."
  WAIT, 0.25
  
  out_img = FLTARR(new_naxis1, new_naxis2)
  out_sig = FLTARR(new_naxis1, new_naxis2)
  FOR j = 0, new_naxis1 - 1 DO BEGIN
    FOR k = 0, new_naxis2 - 1 DO BEGIN
      IF KEYWORD_SET(sigma) THEN BEGIN
        ;Preliminary filter out +/- 1E6 values and zero values
        good_data = WHERE(ABS(stack_img[j,k,*]) NE 1E6 $
          AND stack_img[j,k,*] NE 0 $
          AND ABS(stack_sig[j,k,*]) NE 1E6 $
          AND stack_sig[j,k,*] NE 0 , count)
        IF count GT 1 THEN BEGIN
          ;Filter out > 3-sigma deviation values
          merit_values = MEDIAN_FILTERED_MEAN(REFORM(stack_img[j,k,good_data]))
          good_data    = good_data[WHERE((ABS(stack_img[j,k,good_data] - merit_values[0]) LT 3*merit_values[1]), count)]
          IF count GT 0 THEN BEGIN
            ;Finally weight an average and create a normalization map
            out_pix = TOTAL(stack_img[j,k,good_data]/(stack_sig[j,k,good_data])^2)
            sig_pix = TOTAL(1/(stack_sig[j,k,good_data])^2)
            ;Test if this is an acceptable value to inclue in the final data
            IF FINITE(out_pix) AND (out_pix NE 0) AND FINITE(sig_pix) THEN BEGIN
              out_img[j,k] = TOTAL(stack_img[j,k,good_data]/(stack_sig[j,k,good_data])^2)
              out_sig[j,k] = TOTAL(1/(stack_sig[j,k,good_data])^2)
            ENDIF
          ENDIF
        ENDIF
      ENDIF ELSE BEGIN
        ;Preliminary filter out +/- 1E6 values and zero values
        good_data = WHERE((ABS(stack_img[j,k,*]) NE 1E6) AND (stack_img[j,k,*] NE 0), count)
        IF count GT 1 THEN BEGIN
          ;Average the good data
          zz           = MEDIAN_FILTERED_MEAN(REFORM(stack_img[j,k,good_data]))
          out_img[j,k] = zz[0]
          out_sig[j,k] = zz[1]
        ENDIF
      ENDELSE
      IF ~FINITE(out_img[j,k]) OR ~FINITE(out_sig[j,k]) THEN STOP
    ENDFOR
  ENDFOR
  
  PRINT, "Finished the averaging procedure at " + STRMID(SYSTIME(), 11, 8)
  
  ;Fill in empty data data in the sigma map and output images
  IF KEYWORD_SET(sigma) THEN BEGIN
    zero_in = ARRAY_INDICES(out_sig, WHERE(out_sig EQ 0))
    out_sig[zero_in[0,*],zero_in[1,*]] = -1E6
    out_img = out_img/out_sig
    out_img[zero_in[0,*],zero_in[1,*]] = -1E6
  ENDIF
  
  
  ;Write the final output images
  band         = STRTRIM(SXPAR(combined_header, 'BAND'), 2)
  stokes       = STRTRIM(SXPAR(combined_header, 'STOKES'), 2)
  subDir       = "S1_Image_Combine" + PATH_SEP()
  outValuePath = outDir + subDir + band + "_"  + stokes + ".fits"     ;path for writing weighted average
  outSigmaPath = outDir + subDir + band + "_s" + stokes + ".fits"     ;path for writing weighted uncertainty

  WRITEFITS, outValuePath, out_img, combined_header
  IF KEYWORD_SET(sigma) THEN WRITEFITS, outSigmaPath, SQRT(1/out_sig), combined_header
   
END