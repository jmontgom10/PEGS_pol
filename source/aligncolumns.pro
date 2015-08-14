PRO AlignColumns, ColumnBase

;  Code taken from Mark ***something*** "Align Widgets" online example
;  
;Feel free to modify the code for more general cases. Note that:
;This example only looks one level deep for child widgets. You might consider recursion.
;It doesn’t account for widget heights, which may also be important to you.
;It doesn’t take into account other geometry items such as spacing and padding.
;You may also see different results if the utility is called postrealization
;rather than prerealization.
;(That is, WIDGET_CONTROL, /REALIZE is called before AlignColumns rather than after.)
;Consider a mechanism to indicate to the utility routine that a widget should occupy more than one column,
;such as the “status” label in the bottom-most row.
;  
;  compile_opt idl2

  ; The outer loop iterates over the rows in the column base.
  columnmaxwidths = []
  child = widget_info(ColumnBase, /CHILD)
  while (widget_info(child, /VALID_ID)) do begin
    ; The inner loop iterates over the widgets in each row.
    ; The number of widgets in this base is interpreted
    ; as the number of columns.
    columncount = 0L
    grandchild = widget_info(child, /CHILD)
    while (widget_info(grandchild, /VALID_ID)) do begin
      ; What is the screen width in pixels of this widget?
      width = (widget_info(grandchild, /GEOMETRY)).scr_xsize
      ; If this is a new column, record its width, otherwise
      ; take the maximum of this width and the previous
      ; recorded maximum width.
      if (n_elements(columnmaxwidths) lt columncount + 1) then begin
        columnmaxwidths = [columnmaxwidths, width]
      endif else begin
        columnmaxwidths[columncount] >= width
      endelse
      columncount++
      grandchild = widget_info(grandchild, /SIBLING)
    endwhile
    child = widget_info(child, /SIBLING)
  endwhile
  ; Pass 2: Set the screen width of each widget equal
  ; to the maximum width found per column
  child = widget_info(ColumnBase, /CHILD)
  while (widget_info(child, /VALID_ID)) do begin
    columncount = 0L
    grandchild = widget_info(child, /CHILD)
    while (widget_info(grandchild, /VALID_ID)) do begin
      widget_control, grandchild, SCR_XSIZE = columnmaxwidths[columncount]
      columncount++
      grandchild = widget_info(grandchild, /SIBLING)
    endwhile
    child = widget_info(child, /SIBLING)
  endwhile
end