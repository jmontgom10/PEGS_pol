pro print_text2, Event, str
;
;   prints the string str to the base text window
;
;	and, if open, into the log file
;
ON_ERROR, 2
;
;
text_ID = WIDGET_INFO( Event.top, FIND_BY_UNAME="BASE_TEXT_BOX")
;
;	how many lines are present?
;
WIDGET_CONTROL, text_ID, GET_VALUE=window_text
nl = N_ELEMENTS(window_text)
;
;	if str too long, cut into two lines
;
line_len = 110
len = STRLEN(str)
if(len gt line_len) then begin
  slash = STRPOS(str,PATH_SEP(),line_len-15)
  if(slash eq -1) then slash = 90
  slash++
  line_1 = STRMID(str,0,slash)
  line_2 = STRMID(str,slash,len-slash)
  WIDGET_CONTROL, text_ID, SET_VALUE=line_1, /APPEND
  WIDGET_CONTROL, text_ID, SET_VALUE='          '+line_2, /APPEND
  WIDGET_CONTROL, text_ID, SET_TEXT_TOP_LINE=nl-1
endif else begin
  WIDGET_CONTROL, text_ID, SET_VALUE=str, /APPEND
  WIDGET_CONTROL, text_ID, SET_TEXT_TOP_LINE=nl-2
endelse
;
;if( (*s_ptr).lu_log ne 0) then begin
;	;
;	;	for some reason logging often turns off on GPIPSE
;	;
;	CATCH, log_error
;	;
;	if(log_error ne 0) then begin
;		print, 'ERROR - failed to write to logfile: ',(*s_ptr).lu_path
;		print, '      - continuing with no logging to unit:',(*s_ptr).lu_log
;		;
;		CATCH, /CANCEL
;		goto, no_log
;	endif
;	;
;	printf, (*s_ptr).lu_log, str
;	;
;	no_log:
;endif
;
end