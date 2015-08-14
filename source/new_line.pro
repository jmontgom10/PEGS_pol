FUNCTION NEW_LINE
  ; Cross platform newline string
  IF (!D.NAME eq 'WIN') THEN newline = STRING([13B, 10B]) ELSE newline = STRING(10B)
RETURN, newline
END