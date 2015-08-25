FUNCTION READHEAD, filename

  OPENR, lun, filename, /GET_LUN
  line  = ''
  array = ''
  WHILE ~EOF(lun) DO BEGIN
    READF, lun, line
    array = [array, line]
  ENDWHILE
  array = array[1:*]
  
  FREE_LUN, lun
  CLOSE, lun
  
  RETURN, array

END