FUNCTION PEGS_SKI_JUMP_FIT, image_input, ski_jump_code, model, MASK=mask, BAD_PIXEL_MAP = bad_pixel_map
;
;   fits ski jump for an entire image, one column
;   at a time, returns residuals as an image
;
;   DPC 20050613     original code
;       20060129     modified to work with subframing
;		20081204	 modified to work in PPOL package
;						can return model image
;		20090129	 changed "swath" to "picket fence" to improve
;						bright star rejection
;		20091209	 added bad pixel detection and sidelining, to account for new col clock detection and tuned fit parameters
;		20100108	 improved fit for second order ski jump
;		20100109	 added ski_jump_code keyword (00, 10, 11, 02, 20, 22)
;		20111211	 added short_fit_flag keyword for fitting very short (in terms of rows) ski jumps - for extended objects
;		JDM 20150831   removed "short_fit_flag" and included a MASK image to build a model using NON-MASKED pixels
;		               (allows user to make extended objects, e.g. galaxies)
;
masked_image = image_input
bad_pixel_value = -1.0e6
bad_pixel_map  = BYTE(0*masked_image)
bad_pixel_map1 = image_input EQ bad_pixel_value
model        = 0.0*masked_image
;
;	normal or abnormal?
;
normal_flag = 1									; is normal
if(ski_jump_code gt 11) then normal_flag = 0	; is abnormal
;
isize  = size(masked_image)
im_col = isize[1]
im_row = isize[2]
;
cwidth = 13         ; half width of column swath
pitch  = 3          ; keep only every 4 columns to try to step over stars
;
maskedInd = WHERE(mask EQ 1, numMasked)
badInd    = WHERE(image_input EQ bad_pixel_value, numBad)
IF numMasked GT 0 THEN masked_image[maskedInd] = !VALUES.F_NAN
IF numBad GT 0 THEN masked_image[badInd] = !VALUES.F_NAN
;
cwidth = cwidth * pitch			; widen by pitch
;
center_y = (im_row - 2) / 2
;
if(im_col eq 1024 and im_row eq 1026) then begin
  ;
  if(normal_flag eq 1) then begin
  	;
  	;	the normal set
  	;
  	; full frame; establish 4 zones and boundaries
    ;
    nzones = 4
    fit_region = intarr(2,nzones)
    fit_order = intarr(nzones)
    apply_region = intarr(2,nzones)     ; [start:stop, zone number]
  	;
    fit_region[0,0] = 0
    fit_region[1,0] = 12		; was 15, then 10
    apply_region[0,0] = 0
    apply_region[1,0] = 8		; was 10
    fit_order[0] = 5
    ;
    fit_region[0,1] = 8			; was 10, then 6
    fit_region[1,1] = 60		; was 75, then 80
    apply_region[0,1] = 9		; was 11
    apply_region[1,1] = 45		; was 60
    fit_order[1] = 7			; was 6, then 8
    ;
    fit_region[0,2] = 30		; was 50
    fit_region[1,2] = 350
    apply_region[0,2] = 46
    apply_region[1,2] = 300
    fit_order[2] = 6			; was 5
    ;
    fit_region[0,3] = 250
    fit_region[1,3] = 511
    apply_region[0,3] = 301
    apply_region[1,3] = 511
    fit_order[3] = 4
  endif else begin
		;
		print, 'fitting abnormal jump'
		;
		;	the abnormal set
		;
		nzones = 6
    fit_region = intarr(2,nzones)
    fit_order = intarr(nzones)
    apply_region = intarr(2,nzones)     ; [start:stop,
    ;
    reg = 0
		fit_region[0,reg] = 5
    fit_region[1,reg] = 12
    apply_region[0,reg] = 5
    apply_region[1,reg] = 9
    fit_order[reg] = 5
    ;
    reg++
    fit_region[0,reg] = 8			; was 10, then 6
    fit_region[1,reg] = 60		; was 75, then 80
    apply_region[0,reg] = 10		; was 11
    apply_region[1,reg] = 45		; was 60
    fit_order[reg] = 7			; was 6, then 8
    ;
    reg++
    fit_region[0,reg] = 35		; was 50
    fit_region[1,reg] = 90
    apply_region[0,reg] = 46
    apply_region[1,reg] = 75
    fit_order[reg] = 6			; was 5
    ;
    reg++
    fit_region[0,reg] = 60		; was 50
    fit_region[1,reg] = 160
    apply_region[0,reg] = 76
    apply_region[1,reg] = 95
    fit_order[reg] = 6			; was 5
    ;
    reg++
    fit_region[0,reg] = 70		; was 50
    fit_region[1,reg] = 350
    apply_region[0,reg] = 96
    apply_region[1,reg] = 300
    fit_order[reg] = 7			; was 5
    ;
    reg++
    fit_region[0,reg] = 250
    fit_region[1,reg] = 511
    apply_region[0,reg] = 301
    apply_region[1,reg] = 511
    fit_order[reg] = 4
  endelse
endif else begin
	MESSAGE, "ERROR - didn't find full frame image", LEVEL = -1
endelse
;
fixed = model        ; clear result array
xmodel = findgen(center_y)
ymodel = fltarr(center_y)
yresid = ymodel
;
;   big loop on the columns in the image
;
column_vec = fltarr(im_row)
result = column_vec * 0.0
;
for icol = 0, im_col-1 do begin
	;
	;	clear buffers
	;
	ymodel = fltarr(center_y)
	yresid = ymodel
	badPix = bytarr(center_y)
	;
	;	column zone to extract
	; c1    = left  index of the extracted column
	; c2    = right index of the extracted column
	; pitch = sampling rate between left and right columns (set to 3)
	;
	c1 = 0 > (icol-cwidth)
	c2 = (im_col-1) < (icol+cwidth)
  ;
  ;	determine the mean column cut by doing mfm at each row
  ;
  iswath          = masked_image[c1:c2:pitch,*]             ;Grab the sub-sampled section of this column
  column_vec      = (jm_median_filtered_mean(iswath, $      ;Compute the MFM of the sub-sampled column
    DIMENSION = 1)).mean
  ;
  ;Now store grab the masked and unmasked (1-suffix) data for this column
  cswath           = masked_image[icol,0:im_row-1]          ;Grab this specific column (icol)
  data_column_vec  = REFORM(cswath)                         ;Reform that data into a row vector
  ;
  ;Split the MFM column in two halves and store them in two vectors
  cut_L  = column_vec[0:center_y-1]                          ;skipping top two rows
  cut_U  = dpc_reverse(column_vec[center_y:im_row-3])        ;swap the upper half order to look like lower half
  ;
  ;Split this column (icol) in two halves and store them in two vectors.
  ;These are the unsmoothed data
  ;The L1 and U1 vectors will contain the UNMASKED data
  data_cut_L  = data_column_vec[0:center_y-1]
  data_cut_U  = dpc_reverse(data_column_vec[center_y:im_row-3])
  ;
  ;
  ; fit each of the four (or six for abnormal jumps) zones - as per discovery of shape of ski jump
  ;---------------------------------------------------------------
  ; for subframing, and especially after we tuned the detector in 2005/11,
  ; probably good enough to fit 0-10 with 4th order, 11-center with 2nd order
  ;---------------------------------------------------------------
  ;
  ; The following loop solves the upper half of the column (cut_U and data_cut_U) first,
  ; then it solves the lower half (cut_L, data_cut_L).
  for iquad = 0, 1 do begin
    ;
    case iquad of
        0: begin
        	y       = cut_U
        	data_y  = data_cut_U
           end
        1: begin
        	y       = cut_L
        	data_y  = data_cut_L
           end
    endcase
    ;
    ;	fit to smoothed (mfm) column behavior
    ;	The following loop passes through each "zone" (4-normal, 6-abnormal)
    ;	which is a HORIZONTAL section of the image.
    ; The "fit_region" for each zone extends a bit beyond the boundaries of the zone
    ; in order to produce smooth transitions at the boundaries.
    for izone = 0, nzones-1 do begin
      ;For each zone, grab the data and assign "x-values" to their indices
      ;
      ;Key for the following variables:
      ;xfit      = the indices of the column values used for polynomial fitting
      ;            (these overlap in successive zones)
      ;y_sub     = the subsection of the smoothed (mfm) column values used for polynomial fitting
      ;order     = the order of the polynomial to be fit to this data
      ;low_fit   = the lowest index of the column values to be replaced in this zone
      ;           (these DO NOT overlap in successive zones)
      ;high_fit  = the highest index of the column values to be replaced in this zone
      ;           (these DO NOT overlap in successive zones)
      ;applyInds = these are the indicies of the
      ;
      xfit      = fit_region[0,izone] + findgen(fit_region[1,izone] - fit_region[0,izone] + 1)
      y_sub     = y[fit_region[0,izone]:fit_region[1,izone]]
      order     = fit_order[izone]
     	low_fit   = apply_region[0,izone]
     	high_fit  = apply_region[1,izone]
     	applyInds = findgen(high_fit - low_fit) + low_fit
     	;
     	;	test that the y values aren't all deleted or masked
     	;
     	ind_good = WHERE(finite(y_sub), ngood)
     	if(ngood gt 0) then begin
     	  ;
        ;	loop 2 times to reject pixels more than 7 sigma out of mean of region
        ;
        for iloop = 0, 1 do begin
        	yind = WHERE(finite(y_sub), nok)
        	if(nok gt 0) then begin
        		zz      = jm_median_filtered_mean(y_sub[yind])
        		bad_ind = WHERE(abs(y_sub - zz.mean)/zz.std gt 7.0,n_bad)
        		if(n_bad gt 0) then begin
        			y_sub[bad_ind] = !VALUES.F_NAN
        		endif
        	endif
        endfor
        ;
        ; loop 3 times to reject pixels more than 5 sigma out of mean of fit
        ;
        coeffs_sub = dblarr(order+1)		; clear to zero
        for iloop = 0, 2 do begin
          ;
          ;Check for finite values in this column subsection
          yind = WHERE(FINITE(y_sub), tcount)
          ;
          ;Check if there are enough surviving values to fully determine the polynomial
          if (tcount gt order) then begin
            ;
            ;Fit the surviving datapoints with a polynomial
            coeffs_sub = POLY_FIT(xfit[yind], y_sub[yind], order, /DOUBLE, YERROR=sigma, $
            		YFIT=yfit, STATUS=status)
            ;
            ;Check if the poly_fit succeeded
            if(status eq 0 or status eq 2) then begin
              ;Since only overdetermined columns were POLY_FIT,
              ;we can and should test for outliers from the fit.
            	ybad = where(abs(y_sub[yind] - yfit)/sigma gt 5., count)
            	if(count gt 0) then begin
            	  y_sub[yind[ybad]] = !VALUES.F_NAN
            	endif
            endif
          endif
        endfor
        IF ((status EQ 0) OR (status EQ 2)) AND (tcount GT order) THEN BEGIN
          ;
          ; If the last polynomial fit succeeded
          ; and enough data survived the outlier rejection,
          ; then build the model from coefficients.
          ;
          ymodel[low_fit:high_fit] = 0.0                      ;Clear the model data
          FOR ipower = 0, order DO BEGIN                      ;Loop through each power in the order
            ;Accumulate the values into the model vector
            ymodel[low_fit:high_fit] += coeffs_sub[ipower] * (xmodel[low_fit:high_fit]^ipower)
          ENDFOR
        ENDIF ELSE BEGIN
          ymodel[low_fit:high_fit] *= !VALUES.F_NAN
        ENDELSE
        ;
        ; Now that all the possible bad pixels are marked,
        ; let's fill in the badPix vector with the marked values.
        ; Use the OR logical opperator to make sure that pixels
        ; marked as "bad" in ANY fitted region are marked.
        ;
        fitInds  = WHERE((xfit GE low_fit) AND (xfit LE high_fit))
        badPix[low_fit:high_fit] =  ~FINITE(y_sub[fitInds])
      endif else begin
       	badPix[low_fit:high_fit] = 1B
    	endelse
    endfor
    ;
    ;   pack into result array
    ;
    case iquad of
        0: begin
          ymodel                                = dpc_reverse(ymodel)
          badPix                                = dpc_reverse(badPix)
          bad_pixel_map[icol,center_y:im_row-3] = badPix[0:center_y-1]
          model[icol,center_y:im_row-3]         = ymodel[0:center_y-1]
        end
        1: begin
          bad_pixel_map[icol,0:center_y-1] = badPix[0:center_y-1]
        	model[icol,0:center_y-1]         = ymodel[0:center_y-1]
        end
    endcase
  endfor
endfor
;
; Now that the full model is built, there may be some NAN values needing to be fixed.
;
model[maskedInd] = !VALUES.F_NAN
nx      = 31
sigX    = 10
kernel  = EXP(-0.5*((findgen(nx)-nx/2)/sigX)^2)
kernel /= TOTAL(kernel)
PRINT, 'Inpainting masked region'
model  = inpaint_nans(model, smoothing_kernel = kernel)
bad_pixel_map[maskedInd] = 0B
bad_pixel_map            = (bad_pixel_map OR bad_pixel_map1)
;
;	mask top and bottom rows, as per normal or abnormal
;
if(normal_flag) then begin
	model[*,im_row-10:im_row-1] = bad_pixel_value
	model[*,0:8] = bad_pixel_value
  bad_pixel_map[*,im_row-10:im_row-1] = 1B
  bad_pixel_map[*,0:8] = 1B
endif else begin
	model[*,im_row-12:im_row-1] = bad_pixel_value
	model[*,0:9] = bad_pixel_value
  bad_pixel_map[*,im_row-12:im_row-1] = 1B
  bad_pixel_map[*,0:9] = 1B
endelse
;
; Everything is done... return the model!
;
RETURN, model
end
