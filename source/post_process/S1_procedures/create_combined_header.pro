FUNCTION GET_UNIQ_PARAMETERS, headers, check_parameter
  n_headers = N_ELEMENTS(TAG_NAMES(headers))
  ; Create an array holding ALL the values of THIS parameter from ALL the headers
  data_type = SIZE(SXPAR(headers.(0), check_parameter), /TYPE)
  CASE data_type OF                           ;Create the correct type of temporary storage
    1: temp_par_values = BYTARR(n_headers)
    2: temp_par_values = INTARR(n_headers)
    3: temp_par_values = LONARR(n_headers)
    4: temp_par_values = FLTARR(n_headers)
    5: temp_par_values = DBLARR(n_headers)
    7: temp_par_values = STRARR(n_headers)
  ENDCASE
  ; Loop through and populate the temporary parameter storage with values from headers
  FOR i = 0, n_headers - 1 DO BEGIN
    temp_par_values[i] = SXPAR(headers.(i), check_parameter)
  ENDFOR
  
  ; Test for multiple values for THIS parameter
  uniq_par_values = temp_par_values[UNIQ(temp_par_values, SORT(temp_par_values))]
  IF N_ELEMENTS(uniq_par_values) GT 1 THEN BEGIN
    PRINT, "More than one parameter value detected", uniq_par_values
    STOP
  ENDIF
  RETURN, uniq_par_values
END

FUNCTION GET_AVERAGE_DATE, headers
  nHeads  = N_TAGS(headers)
  dateArr = DBLARR(nHeads)
  FOR i = 0, nHeads - 1 DO BEGIN
    ;Handle DATE-OBS vs. DATEOBS
    tmpDate = SXPAR(headers.(i), 'DATE-OBS')
    IF tmpDate EQ 0 THEN tmpDate = SXPAR(headers.(i), 'DATEOBS')
    dateArr[i] = DATE_CONV(tmpDate)
  ENDFOR
  averageDate = DATE_CONV(MEAN(dateArr), 'FITS')
  RETURN, averageDate
END

FUNCTION GET_TOTAL_EXPTIME, headers
  totalExpTime = 0L                                         ;Initalize expTime counter
  FOR i = 0, N_TAGS(headers) - 1 DO BEGIN
    imagesInGroup = SXPAR(headers.(i), 'MPP_DADD')
    imageExpTime  = SXPAR(headers.(i), 'EXPTIME')
    totalExpTime += imagesInGroup*imageExpTime
  ENDFOR
  RETURN, totalExpTime
END

PRO CREATE_COMBINED_HEADER, filePaths, inDir, object_name, all_astr, combined_header

  ; Simultaneously read in all the headers (to create merged header) and all astrometry
  n_files   = N_ELEMENTS(filePaths)         ;Count the files and define a center pixel in the input images
  FOR i = 0, n_files - 1 DO BEGIN           ;Read in all of the astrometry from all of the images
    tmp_header = HEADFITS(filePaths[i])     ;Rip out the header for each file
    name_number = STRTRIM(i, 2)             ;Grab the loop index number
    IF STRLEN(name_number) LE FLOOR(ALOG10(n_files)) THEN BEGIN
      ; Prepend the correct number of zeros and store the header in structure
      FOR j = 0, (STRLEN(name_number) - ALOG10(n_files)) DO name_number = STRTRIM(0, 2) + name_number
    ENDIF
    IF N_ELEMENTS(headers) EQ 0 THEN headers =  CREATE_STRUCT('header'+name_number, tmp_header) $
    ELSE headers = CREATE_STRUCT(headers, 'header'+name_number, tmp_header)
  ENDFOR

  n_headers = N_ELEMENTS(TAG_NAMES(headers))
  PRINT, "Reading in astrometry..."
  FOR i = 0, n_headers - 1 DO BEGIN
    EXTAST, headers.(i), astr, noparams      ;Store astrometry (test for first case)
    IF N_ELEMENTS(all_astr) EQ 0 $
      THEN all_astr = astr $
      ELSE all_astr = [all_astr, astr]
  ENDFOR

  PRINT, "Finding the center image..."
  
  ;Find the deviations from the center pointing
;  cen_RA  = MIN(all_astr.crval[0]) + 0.5*(MAX(all_astr.crval[0]) - MIN(all_astr.crval[0]))
;  cen_DEC = MIN(all_astr.crval[1]) + 0.5*(MAX(all_astr.crval[1]) - MIN(all_astr.crval[1]))
;  del_RA  = all_astr.crval[0] - cen_RA
;  del_DEC = all_astr.crval[1] - cen_DEC
  del_RA  = all_astr.crval[0] - MEDIAN(all_astr.crval[0], /EVEN)
  del_DEC = all_astr.crval[1] - MEDIAN(all_astr.crval[1], /EVEN)
  del_th  = SQRT((del_RA*COS(all_astr.crval[0]*!DTOR))^2 + del_DEC^2)
  
  ;Select the minimum deviation and use that as the "center image"
  center_image = (WHERE(del_th EQ MIN(del_th)))[0]
  center_astr  = all_astr[center_image]
  
  ;Find the x_offsets and y_offsets from the center image
  ;Correct for any shifting errors
  AD2XY, all_astr.crval[0], all_astr.crval[1], center_astr, mapX, mapY
;  x_offsets = ROUND(mapX - center_astr.crpix[0])
;  y_offsets = ROUND(mapY - center_astr.crpix[1])
;  ctr_shift = [y_offsets[center_image], y_offsets[center_image]]  
  
  deltaX_right = mapX + (all_astr.naxis[0] - all_astr.crpix[0])
  deltaX_left  = mapX - (all_astr.crpix[0])
  deltaY_top   = mapY + (all_astr.naxis[1] - all_astr.crpix[1])
  deltaY_bot   = mapY - (all_astr.crpix[0])
;  deltaX_left  = x_offsets + 

;  x_offsets = x_offsets - ctr_shift[0]                                ;Correct .FITS vs. IDL convention
;  y_offsets = y_offsets - ctr_shift[1]
  
  ;Find the extreema of the offsets
;  xmin = MIN(x_offsets)
;  xmax = MAX(x_offsets)
;  ymin = MIN(y_offsets)
;  ymax = MAX(y_offsets)
  
  xMaxInd = (WHERE(deltaX_right EQ MAX(deltaX_right)))[0]
  xMinInd = (WHERE(deltaX_left EQ MIN(deltaX_left)))[0]
  yMaxInd = (WHERE(deltaY_top EQ MAX(deltaY_top)))[0]
  yMinInd = (WHERE(deltaY_bot EQ MIN(deltaY_bot)))[0]
  
  ;********I NEED TO RETHINK THIS... WOULD THIS WORK EVERY TIME?**********
  ;Calculate the extra array space needed to store the input image
;  dx = xmax - xmin
;  dy = ymax - ymin
  
  ;Increase the image size by the necessary ammount
;  new_naxis1 = MAX(all_astr.naxis[0]) + dx
;  new_naxis2 = MAX(all_astr.naxis[1]) + dy
  new_naxis1 = CEIL(deltaX_right[xMaxInd]) - FLOOR(deltaX_left[xMinInd])
  new_naxis2 = CEIL(deltaY_top[yMaxInd])   - FLOOR(deltaY_bot[yMinInd])

  PRINT, "Creating header template"
  ;Create a brand new header to fill with the correct information
  MKHDR, combined_header, 4, [new_naxis1, new_naxis2]

  PRINT, "Populating with basic astrometry"
  ;Locate the center of the new image
  new_x_center = new_naxis1/2
  new_y_center = new_naxis2/2
  
  ;Locate the pixel in the new image to which the reference pixel in the center image maps
;  new_crpix1  = center_astr.crpix[0] - xmin - ctr_shift[0]
;  new_crpix2  = center_astr.crpix[1] - ymin - ctr_shift[1]
  new_crpix1  = center_astr.crpix[0] - FLOOR(deltaX_left[xMinInd])
  new_crpix2  = center_astr.crpix[1] - FLOOR(deltaY_bot[yMinInd])
  center_astr.crpix = [new_crpix1, new_crpix2]
  
  ;Calculate where the new x_center and y_center are in RA and DEC,
  
  ;*********THIS LINE IS WRONG!******
  ;*********I NEED TO COMPUTE THE CENTER VALUE, BUT I CANNOT USE THE PREVIOUS ASTROMETRY.
  ;*********FIRST I NEED TO UPDATE SOME OF THE ASTROMETRY, THEN I CAN USE IT TO RECOMPUTE THE CENTER VALUE.
  XY2AD, new_x_center, new_y_center, center_astr, ra_cen, dec_cen
  
;  MAKE_ASTR, combined_astr, CRPIX = [(new_x_center - ctr_shift[0]), (new_y_center - ctr_shift[1])], $
;    CRVAL = [ra_cen, dec_cen];, CD = center_astr.CD

;*****THIS NEEDS UPDATING TO INCLUDE AVERAGE OBSERVATION DATE*****
  PUTAST, combined_header, $
    center_astr.CD, $
;    [(new_x_center - ctr_shift[0]), (new_y_center - ctr_shift[1])], $
    [(new_x_center), (new_y_center)], $
    [ra_cen, dec_cen], $
    center_astr.CTYPE
    
;  combined_header = headers.(center_image)
;  SXADDPAR, combined_header, 'CRPIX1', (new_x_center - ctr_shift[0])
;  SXADDPAR, combined_header, 'CRPIX2', (new_y_center - ctr_shift[1])
;  SXADDPAR, combined_header, 'CRVAL1', ra_cen
;  SXADDPAR, combined_header, 'CRVAL2', dec_cen
;  SXADDPAR, combined_header, 'NAXIS1', new_naxis1
;  SXADDPAR, combined_header, 'NAXIS2', new_naxis2

  PRINT, "Inserting other useful header parameters"
  ; Here ar the new parameters to add to the header
  parameters_to_add = ['BZERO',    'BSCALE',   'BUNIT', $
                       'OBSERVER', 'OBSAFFIL', 'OBSERVAT', $
                       'ALTITUDE', 'LATITUDE', 'LONGITUD', $
                       'TELESCOP', 'INSTRUME', 'DATE-OBS', $
                       'OBJNAME' , 'DIRNAME', $
                       'EXPTIME', 'BAND',     'STOKES']
  ; Here are the comments to associate with those parameters
  comments_to_add   = ['zero point', $
                       'data scaled by', $
                       'pixel units (ADU, electrons)', $
                       'observer(s)', $
                       'observer(s) affiliation', $
                       'observatory', $
                       'altitude in meters', $
                       'latitude, degrees', $
                       'east longitude, degrees', $
                       'telescope name', $
                       'instrument name', $
                       'average UT date(yyy-mm-dd) of observations', $
                       'object name', $
                       'directory for source images', $
                       'total exposure time (sec)', $
                       'filter name', $
                       'stokes parameter of this image']

  ; Now loop through the parameters and address each one specifically
  FOR i = 0, N_ELEMENTS(parameters_to_add) - 1 DO BEGIN
    ; Test if this parameter needs secial handling
    CASE parameters_to_add[i] OF
      'OBSERVER': BEGIN
        SXADDPAR, combined_header, 'OBSERVER', 'still needs fixing', comments_to_add[i]
      END
      'DATE-OBS': BEGIN
        averageDate = GET_AVERAGE_DATE(headers)
        ;For some reason ASTROLIB omits the hyphen in the 'DATE-OBS' parameter
        SXADDPAR, combined_header, 'DATEOBS', averageDate, comments_to_add[i]
      END
      'DIRNAME': BEGIN
        SXADDPAR, combined_header, 'DIRNAME', inDir, comments_to_add[i]
      END
      'OBJNAME': BEGIN
        SXADDPAR, combined_header, 'OBJNAME', object_name, comments_to_add[i]
      END
      'EXPTIME': BEGIN
        totalExpTime = GET_TOTAL_EXPTIME(headers)
        SXADDPAR, combined_header, 'EXPTIME', totalExpTime, comments_to_add[i]
      END
      'BAND': BEGIN
        uniq_par_values = GET_UNIQ_PARAMETERS(headers, 'FILTNME2')
        SXADDPAR, combined_header, 'BAND', uniq_par_values, comments_to_add[i]
      END
      'STOKES': BEGIN
        uniq_par_values = GET_UNIQ_PARAMETERS(headers, 'S11IMG')
        SXADDPAR, combined_header, 'STOKES', uniq_par_values, comments_to_add[i]
      END
      ELSE: BEGIN
        uniq_par_values = GET_UNIQ_PARAMETERS(headers, parameters_to_add[i])
        SXADDPAR, combined_header, parameters_to_add[i], uniq_par_values
      END
      
    ENDCASE
  ENDFOR
END