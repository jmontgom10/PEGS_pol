;This function will shift an array an arbitrary amount and conserve flux between pixels
;

FUNCTION FSHIFT, img, dx, dy

  dxI = ROUND(dx)
  dyI = ROUND(dy)
  dxF = dx - dxI
  dyF = dy - dyI
  
  ; The x-shift is non-integer...
  ; Compute the two integer shiftings needed
  dxRt = FIX(CEIL(dx))
  dxLf = dxRt - 1
  
  ; Produce the shifted arrays
  arrRt = SHIFT(img, dxRt, 0)
  arrLf = SHIFT(img, dxLf, 0)
  
  ; Compute the fractional contributions of each array
  fracRt = ABS(dx - dxLf)
  fracLf = ABS(dx - dxRt)
  
  ; Compute the shifted array
  shiftArr = fracRt*arrRt + fracLf*arrLf
  
  ; The y-shift is non-integer...
  ; Compute the two integer shiftings needed
  dyTop = FIX(CEIL(dy))
  dyBot = dyTop - 1
  
  ; Produce the shifted arrays
  arrTop = SHIFT(shiftArr, 0, dyTop)
  arrBot = SHIFT(shiftArr, 0, dyBot)
  
  ; Compute the fractional contributions of each array
  fracTop = ABS(dy - dyBot)
  fracBot = ABS(dy - dyTop)
  
  ; Compute the shifted array
  shiftArr = fracTop*arrTop + fracBot*arrBot
  
  RETURN, shiftArr
END