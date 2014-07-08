pro fbeye_delmult,f0,fevent,fstartpos,fstoppos,tpeak,tstart,tstop,trise,tdecay,lpeak,ed,cplx_flg,mltpk_flg,mltpk_num,tmltpk,lmltpk,multpos,filename=filename

compile_opt defint32, strictarr, strictarrsubs
compile_opt HIDDEN


; find this sub-peak
  overlap = where(f0 ge fstartpos and f0 le fstoppos)
  if overlap[0] eq -1 then begin
     FBEYE_MSG,'Not an existing flare event.'
     print,'Not an existing flare event.'
     return
  endif 


;  if total(mltpk_flg[where(fevent eq fevent[overlap[0]])] gt 0) eq 0 then begin
  if total(multpos ge fstartpos[overlap[0]] and multpos le fstoppos[overlap[0]]) eq 0 then begin
     FBEYE_MSG,'No sub-peak'
     print,'No sub-peak'
     return
  endif
  
;  overmult = where(fevent eq fevent[overlap[0]] and $ ; same event ID

  overmult = where(multpos ge fstartpos[overlap[0]] and $ ; within flare
                   multpos le fstoppos[overlap[0]] and $
                   abs(f0-multpos) eq min(abs(f0-multpos))) ; closest


  fbeye_remove,overmult,fevent,fstartpos,fstoppos,tpeak
  fbeye_remove,overmult,lpeak,ed,cplx_flg,multpos
  fbeye_remove,overmult,mltpk_flg,mltpk_num,tmltpk,lmltpk
  fbeye_remove,overmult,tstart,tstop,trise,tdecay ; << NEW

; save new structure
save,fevent,fstartpos,fstoppos,tpeak,tstart,tstop,trise,tdecay,lpeak,ed,cplx_flg,mltpk_flg,mltpk_num,tmltpk,lmltpk,multpos,filename=filename

return
end
