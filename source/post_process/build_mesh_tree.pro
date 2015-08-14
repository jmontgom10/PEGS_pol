FUNCTION BUILD_MESH_TREE, SNR_core, min_SNR, numRebinLevels, rebinLevel

  FORWARD_FUNCTION BUILD_MESH_TREE                                    ;Make sure IDL knows that this is a recursive function
  
  IF rebinLevel EQ (numRebinLevels - 1) THEN RETURN, 1B               ;Mark the terminal rebin level
  
;    IF rebinLevel EQ (numRebinLevels - 1) THEN BEGIN                 ;Catch the ultimate-terminal case...
;      testSNR = SNR_core.(0) GT min_SNR
;      IF TOTAL(testSNR) EQ 4 THEN RETURN, {UL:1B, UR:1B, LL:1B, LR:1B} ;Return the final binning level sucess
;      ;A failure to pass this test will have been detected one level up
;    ENDIF

    ;Test if this binning met the SNR requirement
    IF SNR_core.(0) GE min_SNR THEN BEGIN
    
    ;Test if ALL the sub-pixes also pass the SNR test
    testSNR = SNR_core.(1) GT min_SNR
    
    ;If not all four pixels passed, then this is the terminal node in the tree
    IF TOTAL(testSNR) LT 4 THEN RETURN, 1B                            ;Return a terminal value in this case
    
    ;Cut out the SNR cores related to each of the four sub-pixels
    SNR_coreUL = CREATE_STRUCT('level' + STRING(rebinLevel+1, FORMAT='(I1)'), $
      (SNR_core.(1))[0,1])
    SNR_coreUR = CREATE_STRUCT('level' + STRING(rebinLevel+1, FORMAT='(I1)'), $
      (SNR_core.(1))[1,1])
    SNR_coreLL = CREATE_STRUCT('level' + STRING(rebinLevel+1, FORMAT='(I1)'), $
      (SNR_core.(1))[0,0])
    SNR_coreLR = CREATE_STRUCT('level' + STRING(rebinLevel+1, FORMAT='(I1)'), $
      (SNR_core.(1))[1,0])
    scale = 1
    FOR i = 2, (numRebinLevels - rebinLevel - 1) DO BEGIN
      tp         = 2*scale
      rt         = 2*scale
      
      SNR_coreUL = CREATE_STRUCT(SNR_coreUL, 'level' + STRING(rebinLevel+i,FORMAT='(I1)'), $
        (SNR_core.(i))[0:(rt-1),tp:((2*tp)-1)])
      SNR_coreUR = CREATE_STRUCT(SNR_coreUR, 'level' + STRING(rebinLevel+i,FORMAT='(I1)'), $
        (SNR_core.(i))[rt:((2*rt)-1),tp:(2*tp)-1])
      SNR_coreLL = CREATE_STRUCT(SNR_coreLL, 'level' + STRING(rebinLevel+i,FORMAT='(I1)'), $
        (SNR_core.(i))[0:(rt-1),0:(tp-1)])
      SNR_coreLR = CREATE_STRUCT(SNR_coreLR, 'level' + STRING(rebinLevel+i,FORMAT='(I1)'), $
        (SNR_core.(i))[rt:((2*rt)-1),0:(tp-1)])
        
      scale *= 2                                                    ;Double the scaling factor for the next level down
    ENDFOR

    ;Further test the sub-pixels, returning their results to the user
    RETURN, {UL: BUILD_MESH_TREE(SNR_coreUL, min_SNR, numRebinLevels, rebinLevel + 1), $
      UR: BUILD_MESH_TREE(SNR_coreUR, min_SNR, numRebinLevels, rebinLevel + 1), $
      LL: BUILD_MESH_TREE(SNR_coreLL, min_SNR, numRebinLevels, rebinLevel + 1), $
      LR: BUILD_MESH_TREE(SNR_coreLR, min_SNR, numRebinLevels, rebinLevel + 1)  $
    }
  ENDIF ELSE RETURN, 1B

END

