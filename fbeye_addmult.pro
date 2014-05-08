pro fbeye_addmult,f0,fevent,fstartpos,fstoppos,tpeak,tstart,tstop,trise,tdecay,lpeak,ed,cplx_flg,mltpk_flg,mltpk_num,tmltpk,lmltpk,multpos,filename=filename
;FBEYE_PATH+'tmp/'+lightcurve+'.out'
compile_opt defint32, strictarr, strictarrsubs
compile_opt HIDDEN


; find where this sub-peak overlaps a flare event
  overlap = where(f0 ge fstartpos and f0 le fstoppos)
  if overlap[0] eq -1 then begin
     FBEYE_MSG,'Not an existing flare event.'
     return
  endif

; add a sub-peak entry
  fevent = [fevent,fevent[overlap[0]]] ; has same flare event ID
  fstartpos=[fstartpos,f0] ; no start position (look at primary flare event)
  fstoppos =[fstoppos,f0]  ; also no stop position
  tpeak=[tpeak,0]
  tstart=[tstart,0]
  tstop=[tstop,0]
  trise=[trise,0]
  tdecay=[tdecay,0]
  lpeak=[lpeak,0]
  ed=[ed,0]

  cplx_flg=[cplx_flg,2]
; activate complex flag for this flare event
  cplx_flg[where(fevent eq fevent[overlap[0]])] = 2

  mltpk_flg=[mltpk_flg,1] ; set mult-peak flag
  mltpk_num=[mltpk_num,max(mltpk_num)+1] ; add mult-peak event ID
  multpos = [multpos,f0] ; the sub-peak position

  tmltpk=[tmltpk,0] ; the sub-peak time 
  lmltpk=[lmltpk,0] ; the sub-peak luminosity

; save new structure
save,fevent,fstartpos,fstoppos,tpeak,tstart,tstop,trise,tdecay,lpeak,ed,cplx_flg,mltpk_flg,mltpk_num,tmltpk,lmltpk,multpos,filename=filename

return
end
