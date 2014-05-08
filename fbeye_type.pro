pro fbeye_type,f0,tmpflg,fevent,fstartpos,fstoppos,tpeak,tstart,tstop,trise,tdecay,lpeak,ed,cplx_flg,mltpk_flg,mltpk_num,tmltpk,lmltpk,multpos,filename=filename

compile_opt defint32, strictarr, strictarrsubs
compile_opt HIDDEN

; find where this overlaps a flare event
  overlap = where(f0 ge fstartpos and f0 le fstoppos)
  if overlap[0] eq -1 then begin
     Print,'> Not an existing flare event.'
     return
  endif

;assign the type flag to this flare event
  cplx_flg[where(fevent eq fevent[overlap[0]])] = tmpflg

  flgs = ['Unspecified','Classical','Complex','Unusual','Maybe']

  print,'> Flag Assigned: ',flgs[tmpflg]

save,fevent,fstartpos,fstoppos,tpeak,tstart,tstop,trise,tdecay,lpeak,ed,cplx_flg,mltpk_flg,mltpk_num,tmltpk,lmltpk,multpos,filename=filename
return
end
