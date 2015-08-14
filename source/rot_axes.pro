;****THIS PROCEDURE APPLIES A ROTATION MATRIX TO AN ARRAY OF X AND Y VALUES****
;
; xx    - original x values
; yy    - original y values
; theta - rotation angle
; xx1   - rotated x values
; yy1   - rotated y values
; 
; Keywords:
; DEGREES - tells the program the 'theta' is in degrees (not radians)
;
PRO ROT_AXES, xx, yy, theta, xx1, yy1, DEGREES = degrees
  IF KEYWORD_SET(degrees) THEN theta1 = theta*!DTOR ELSE theta1 = theta
  xx1 = +xx*COS(theta1) + yy*SIN(theta1)
  yy1 = -xx*SIN(theta1) + yy*COS(theta1)
END