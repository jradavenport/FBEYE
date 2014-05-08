pro fbeye_recalc,time,flux,filename

compile_opt defint32, strictarr, strictarrsubs
compile_opt HIDDEN

restore,filename

print,'> Recalculating properties for ',strtrim(string(n_elements(where(fevent gt 0))),2),' flare events...'

;fevent,fstartpos,fstoppos,tpeak,tstart,tstop,trise,tdecay,lpeak,ed,cplx_flg,mltpk_flg,mltpk_num,tmltpk,lmltpk,multpos

tpeak=0
tstart=0
tstop=0
trise=0
tdecay=0
lpeak=0
ed=0

x = where(fevent gt 0)
FOR n=0L,n_elements(x)-1 DO BEGIN
   ; fevent = same
   ; fstartpos = same
   ; fstoppos = same
   tmpstat = FBEYE_FLARESTAT(time,flux,fstartpos[x[n[0]]],fstoppos[x[n[0]]])
   
   tpeak=[tpeak,tmpstat[1]]  
   tstart=[tstart,time[fstartpos[x[n[0]]]]]
   tstop=[tstop,time[fstoppos[x[n[0]]]]]  
   trise=[trise,tmpstat[1]-time[fstartpos[x[n[0]]]]]
   tdecay=[tdecay,time[fstoppos[x[n[0]]]]-tmpstat[1]]
   lpeak=[lpeak,tmpstat[2]] 
   ed=[ed,tmpstat[0]]   
ENDFOR
fbeye_remove,0,tpeak,tstart,tstop,trise,tdecay
remove,0,lpeak,ed

if n_elements(cplx_flg) eq 0 then cplx_flg=intarr(n_elements(fevent))
if n_elements(mltpk_flg) eq 0 then mltpk_flg=intarr(n_elements(fevent))
if n_elements(mltpk_num) eq 0 then mltpk_num=intarr(n_elements(fevent))
if n_elements(tmltpk) eq 0 then tmltpk=intarr(n_elements(fevent))
if n_elements(lmltpk) eq 0 then lmltpk=intarr(n_elements(fevent))
if n_elements(multpos) eq 0 then multpos=intarr(n_elements(fevent))

if total(fevent eq 0) gt 0 then begin
   fbeye_remove,where(fevent eq 0),cplx_flg,mltpk_flg,mltpk_num
   fbeye_remove,where(fevent eq 0),tmltpk,lmltpk,multpos
   fbeye_remove,where(fevent eq 0),fevent,fstartpos,fstoppos   
endif

save,fevent,fstartpos,fstoppos,tpeak,tstart,tstop,trise,tdecay,lpeak,ed,cplx_flg,mltpk_flg,mltpk_num,tmltpk,lmltpk,multpos,filename=filename
return
end
