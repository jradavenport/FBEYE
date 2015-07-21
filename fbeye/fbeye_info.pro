pro fbeye_info,f0,fevent,fstartpos,fstoppos,tpeak,tstart,tstop,trise,tdecay,lpeak,ed,cplx_flg,mltpk_flg,mltpk_num,tmltpk,lmltpk,multpos

  print,''

; find the flare event
  overlap = where(f0 ge fstartpos and f0 le fstoppos)
  if overlap[0] eq -1 then begin
     print,'Not an existing Flare Event'
     return
  endif

  nevent = where(fevent eq fevent[overlap[0]])

  print,'Flare Event ID  : ',fevent[overlap[0]],f='(A,I)'
  print,' F-start index  : ',fstartpos[overlap[0]],f='(A,I)'
  print,'  F-stop index  : ',fstoppos[overlap[0]],f='(A,I)'
  for n=0L,n_elements(nevent)-1 do begin
     if mltpk_flg[nevent[n]] eq 0 then print,'    Flare index :',nevent[n],f='(A,I)'
     if mltpk_flg[nevent[n]] eq 1 then print,' Sub-Peak index :',nevent[n],f='(A,I)'
  endfor
  print,'      Flare ED  : ',ed[overlap[0]],f='(A,G11.4)'
  print,'    Flag Type   : ',cplx_flg[overlap[0]],f='(A,F)'

  flgs = ['Unspecified','Classical','Complex','Unusual','Maybe']
  print,'Flag Definition :  ',flgs[cplx_flg[overlap[0]]]


return
end


