PRO WRITEHEAD, filename, header
  ; A basic procedure for writing a '*.head' file.

  OPENW, lun, filename, /GET_LUN, WIDTH = 250
  FOR i = 0, N_ELEMENTS(header) - 1 DO BEGIN
    PRINTF, lun, header[i]
  ENDFOR
  FREE_LUN, lun
  CLOSE, lun

END