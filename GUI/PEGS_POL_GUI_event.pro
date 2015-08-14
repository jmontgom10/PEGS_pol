PRO PEGS_POL_GUI_event, event
  ;PRINT, "Event generated at the top level"
END

PRO EXIT_BUTTON, event
  WIDGET_CONTROL, event.TOP, /DESTROY
END