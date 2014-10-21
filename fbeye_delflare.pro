pro fbeye_delflare,f0,fevent,fstartpos,fstoppos,tpeak,tstart,tstop,trise,tdecay,lpeak,ed,cplx_flg,mltpk_flg,mltpk_num,tmltpk,lmltpk,multpos,s2n,filename=filename
compile_opt defint32, strictarr, strictarrsubs
compile_opt HIDDEN

; check to see this flare is unique (not within any fstart & fstop)
  overlap = where(f0 ge fstartpos and f0 le fstoppos)
  if overlap[0] eq -1 then begin
     FBEYE_MSG,'Not a flare event.'
     print,'Not a flare event.'
     return
  endif

  rflare = where(fevent eq fevent[overlap[0]])


; remove the event, and any sub-peaks w/ same fevent id
  fbeye_remove,rflare,fevent,fstartpos,fstoppos,tpeak
  fbeye_remove,rflare,lpeak,ed,cplx_flg,multpos
  fbeye_remove,rflare,mltpk_flg,mltpk_num,tmltpk,lmltpk
  fbeye_remove,rflare,s2n ; << NEW
  fbeye_remove,rflare,tstart,tstop,trise,tdecay 

; save new structure
save,fevent,fstartpos,fstoppos,tpeak,tstart,tstop,trise,tdecay,lpeak,ed,cplx_flg,mltpk_flg,mltpk_num,tmltpk,lmltpk,multpos,s2n,filename=filename


return
end
