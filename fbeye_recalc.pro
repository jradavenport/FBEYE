pro fbeye_recalc,time,flux,fsmooth,outfilename

compile_opt defint32, strictarr, strictarrsubs
compile_opt HIDDEN

print,'> Using previous flare t_start and t_stop from '+outfilename
restore,outfilename

print,'> Recalculating properties for ',strtrim(string(n_elements(where(fevent gt 0))),2),' flare events...'

;fevent,fstartpos,fstoppos,tpeak,tstart,tstop,trise,tdecay,lpeak,ed,cplx_flg,mltpk_flg,mltpk_num,tmltpk,lmltpk,multpos

tpeak=0
;tstart=0
;tstop=0
fstartpos = 0
fstoppos = 0
trise=0
tdecay=0
lpeak=0
ed=0

x = where(fevent gt 0)

FOR n=0L,n_elements(x)-1 DO BEGIN
   ; fevent = same
   ; fstartpos = same
   ; fstoppos = same

   ; tstart = same
   ; tstop = same
   ;; tstart=[tstart,time[fstartpos[x[n[0]]]]]
   ;; tstop=[tstop,time[fstoppos[x[n[0]]]]]  

   fstarttmp = where(abs(time-tstart[x[n[0]]]) eq min(abs(time-tstart[x[n[0]]])))
   fstoptmp = where(abs(time-tstop[x[n[0]]]) eq min(abs(time-tstop[x[n[0]]])))

   if fstarttmp[0] eq fstoptmp[0] then fstoptmp = fstoptmp+1
   
   fstartpos = [fstartpos, fstarttmp]
   fstoppos = [fstoppos, fstoptmp]

   ;; tmpstat = FBEYE_FLARESTAT(time,flux,fstartpos[x[n[0]]],fstoppos[x[n[0]]])
   tmpstat = FBEYE_FLARESTAT(time,flux, fsmooth, fstarttmp, fstoptmp)   

   tpeak=[tpeak, tmpstat[1]]  
   trise=[trise, tmpstat[1]-time[fstarttmp]]
   tdecay=[tdecay, time[fstoptmp]-tmpstat[1]]
   lpeak=[lpeak, tmpstat[2]] 
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

; dont save tlastviewed or dtlast
save,fevent,fstartpos,fstoppos,tpeak,tstart,tstop,trise,tdecay,lpeak,ed,cplx_flg,mltpk_flg,mltpk_num,tmltpk,lmltpk,multpos,filename=outfilename

return
end
