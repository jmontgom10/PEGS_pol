pro S2_PEGS_SKI_JUMP_DETECTOR, event, image_path, ski_jump_code, CRITERION=criterion
;
;	tests for the presence of ski jumps at top and bottom of
;	Mimir images
;
;	DPC 20100108 - moved test regions closer to edges to pick up second order ski jumps
;	DPC 20100109 - added test for "normal" and "abnormal" ski jumps
;       20111211 - trimmed T, TV tests to not get confused by large negative mfm values
;
;	criterion
;
if(N_ELEMENTS(CRITERION) eq 0) then criterion = 5.0
;
;print, "Criterion = ",criterion
;
image = readfits(image_path, header, /SILENT)
;
image_size = SIZE(image)
nc = image_size[1]
nr = image_size[2]
;
;	select first quartile of image columns
;
ncx = ROUND(nc/4)
;
;	and column strip to examine
;
ncx1 = 0 > (ncx-10)
ncx2 = nc < (ncx+10)
;
dncx = abs(ncx2 - ncx1) + 1
;
;	center reference region
;
nry = ROUND(nr/2)
nr_ref_1 = 0 > (nry-11)
nr_ref_2 = nr < (nry+10)
;
dnr_ref = abs(nr_ref_2 - nr_ref_1) + 1
dnr_ref_2 = dnr_ref / 2
;
;	bottom ski jump test region
;
;nr_bot_1 = 5
;nr_bot_2 = 25
nr_vbot_1 = 0
nr_vbot_2 = 4
;
nr_bot_1 = 5
nr_bot_2 = 15
;
dnr_bot = abs(nr_bot_2 - nr_bot_1) + 1
dnr_vbot = abs(nr_vbot_2 - nr_vbot_1) + 1
;
;	top ski jump test region, skipping top two lines
;
;nr_top_2 = nr-7
;nr_top_1 = nr-27
;
nr_vtop_1 = nr-6
nr_vtop_2 = nr-2
;
nr_top_2 = nr-7
nr_top_1 = nr-17
;
dnr_top = abs(nr_top_2 - nr_top_1) + 1
dnr_vtop = abs(nr_vtop_2 - nr_vtop_1) + 1
;
;	compute median, rms for center reference region, on
;	and even-odd row basis to account for O/E readout
;
even = FLOOR(nr_ref_1/2)*2
odd = even + 1
;
center_even = fltarr(dncx,dnr_ref_2)
center_odd = fltarr(dncx,dnr_ref_2)
;
for irow = 0, dnr_ref_2-1 do begin
	jrow = even + 2*irow
	center_even[*,irow] = image[ncx1:ncx2,jrow]
endfor
for irow = 0, dnr_ref_2-1 do begin
	jrow = odd + 2*irow
	center_odd[*,irow] = image[ncx1:ncx2,jrow]
endfor
;
center_e = REFORM(center_even[*,*], dncx*dnr_ref_2 )
center_o = REFORM(center_odd[*,*], dncx*dnr_ref_2)
;help, center
ref_e = median_filtered_mean(center_e)
ref_o = median_filtered_mean(center_o)
;print, "center mean, rms = ",ref
;
;	for top, bottom, do MFM along rows first
;
bot_vec = fltarr(dnr_bot)
top_vec = fltarr(dnr_top)
;
for nr = 0, dnr_bot-1 do begin
	nrr = nr + nr_bot_1
	zz = median_filtered_mean(REFORM(image[ncx1:ncx2,nrr:nrr]))
	bot_vec[nr] = zz[0]
endfor
for nr = 0, dnr_top-1 do begin
	nrr = nr + nr_top_1
	zz = median_filtered_mean(REFORM(image[ncx1:ncx2,nrr:nrr]))
	top_vec[nr] = zz[0]
endfor
;
;	same for very top and very bottom rows
;
vbot_vec = fltarr(dnr_vbot)
vtop_vec = fltarr(dnr_vtop)
;
for nr = 0, dnr_vbot-1 do begin
	nrr = nr + nr_vbot_1
	zz = median_filtered_mean(REFORM(image[ncx1:ncx2,nrr:nrr]))
	vbot_vec[nr] = zz[0]
endfor
for nr = 0, dnr_vtop-1 do begin
	nrr = nr + nr_vtop_1
	zz = median_filtered_mean(REFORM(image[ncx1:ncx2,nrr:nrr]))
	vtop_vec[nr] = zz[0]
endfor
;
;	compute average top, bottom
;
indb = WHERE(bot_vec ne -1.0e6 and bot_vec gt -5000.0,nbot)
if(nbot gt 0) then begin
	bottom = MOMENT(bot_vec[indb])
	bottom = bottom[0]
endif else bottom = -1.0e6
indt = WHERE(top_vec ne -1.0e6 and top_vec gt -5000.0,ntop)
if(ntop gt 0) then begin
	top = MOMENT(top_vec[indt])
	top = top[0]
endif else top = -1.0e6
;
;	same for very top, very bottom
;
indvb = WHERE(vbot_vec ne -1.0e6 and vbot_vec gt -5000.0,nvbot)
if(nvbot gt 0) then begin
	if(nvbot eq 1) then begin
		vbottom = vbot_vec[indvb[0]]
	endif else begin
		vbottom = MOMENT(vbot_vec[indvb])
		vbottom = vbottom[0]
	endelse
endif else vbottom = -1.0e6
indvt = WHERE(vtop_vec ne -1.0e6 and vtop_vec gt -5000.0,nvtop)
if(nvtop gt 0) then begin
	if(nvtop eq 1) then begin
		vtop = vtop_vec[indvt[0]]
	endif else begin
		vtop = MOMENT(vtop_vec[indvt])
		vtop = vtop[0]
	endelse
endif else vtop = -1.0e6
;
ski_jump = intarr(2)
;
t1 = (bottom - ref_e[0])/ref_e[1]
t2 = (bottom - ref_o[0])/ref_o[1]
if(bottom eq -1.0e6) then begin	; can't test if bottom deleted
	t1 = 0.
	t2 = 0.
endif
t3 = (top - ref_e[0])/ref_o[1]
t4 = (top - ref_o[0])/ref_o[1]
if(top eq -1.0e6) then begin
	t3 = 0.
	t4 = 0.
endif
;
tv1 = (ref_e[0] - vbottom) / (ref_e[1] * 5.0)
tv2 = (ref_o[0] - vbottom) / (ref_o[1] * 5.0)
if(vbottom eq -1.0e6) then begin
	tv1 = 0.
	tv2 = 0.
endif
tv3 = (ref_e[0] - vtop) / (ref_e[1] * 5.0)
tv4 = (ref_o[0] - vtop) / (ref_o[1] * 5.0)
if(vtop eq -1.0e6) then begin
	tv3 = 0.
	tv4 = 0.
endif
;
;	"normal" ski jump has negative TV tests and significant T tests
;	"abnormal" ski jump has positive TV tests and significant T tests
;
if(abs(t1) ge criterion and abs(t2) ge criterion) then ski_jump[0] = 1 else ski_jump[0] = 0
if(abs(t3) ge criterion and abs(t4) ge criterion) then ski_jump[1] = 1 else ski_jump[1] = 0
;
;	test for TVs - but increase criterion by 5x to reduce spurious fixes
;
if(tv1 ge criterion and tv2 ge criterion) then ski_jump[0] = 2
if(tv3 ge criterion and tv4 ge criterion) then ski_jump[1] = 2
;
t_str = ''
if(ski_jump[0] eq 2 or ski_jump[1] eq 2) then t_str = 'abnormal jump'
;
print_text2, Event, "For Image "+FILE_BASENAME(image_path)+" Ski jump T sigmas = "+STRING(format='(4(F7.1,1x))',t1,t2,t3,t4)+' '+t_str
if(t_str ne '') then print_text2, Event, '   and TV sigmas = '+STRING(format='(4(F7.1,1x))',tv1,tv2,tv3,tv4)
;
ski_jump_code = ski_jump[0] + 10 * ski_jump[1]
;
;print," Image ",FILE_BASENAME(image_path)," has ski jump detection code = ",ski_jump_code
;
end