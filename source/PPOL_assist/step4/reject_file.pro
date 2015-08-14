PRO REJECT_FILE, event, groupStruc, groupNum, imageNum

  groupStruc.astroFlags[groupNum,imageNum]  = 3             ;Set the image usage flag to zero
  deleteFile = groupStruc.analysis_dir + $                  ;Determine which photometry file should be deleted
    'S4_Restricted_photometry' + PATH_SEP() + $
    FILE_BASENAME(groupStruc.groupImages[groupNum,imageNum], '.fits') + '_phot.dat'
  FILE_DELETE, deleteFile                                   ;Delete the photometry file
  UPDATE_GROUP_SUMMARY, event, groupStruc
  
END