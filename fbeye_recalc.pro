pro fbeye_recalc,time,flux,fsmooth,outfilename

compile_opt defint32, strictarr, strictarrsubs
compile_opt HIDDEN

print,'> Using previous flare t_start and t_stop from '+outfilename
restore,outfilename

print,'> Recalculating properties for ', $
      strtrim(string(n_elements(where(fevent gt 0))),2),' flare events...'


nflare = n_elements(tstart)
tpeak = fltarr(nflare)
trise = fltarr(nflare)
tdecay = fltarr(nflare)
lpeak = fltarr(nflare)
ed = fltarr(nflare)
fstartpos = fltarr(nflare)
fstoppos = fltarr(nflare)
 

FOR n=0L,nflare-1 DO BEGIN
   if fevent[n] eq 0 then continue

   fstarttmp = where(abs(time-tstart[n[0]]) eq $
                     min(abs(time-tstart[n[0]])))
   fstoptmp = where(abs(time-tstop[n[0]]) eq $
                    min(abs(time-tstop[n[0]])))

   if fstarttmp[0] eq fstoptmp[0] then fstoptmp = fstoptmp+1
   
   fstartpos[n] = fstarttmp
   fstoppos[n] = fstoptmp

   tmpstat = FBEYE_FLARESTAT(time,flux, fsmooth, fstarttmp, fstoptmp)   
   
   ed[n] = tmpstat[0]
   tpeak[n] = tmpstat[1]
   trise[n] = tmpstat[1]-time[fstarttmp]
   tdecay[n] = time[fstoptmp]-tmpstat[1]
   lpeak[n] = tmpstat[2]
ENDFOR


; dont save tlastviewed or dtlast
save,fevent,fstartpos,fstoppos,$
     tpeak,tstart,tstop,trise,tdecay,lpeak,ed,$
     cplx_flg,mltpk_flg,mltpk_num,tmltpk,lmltpk,multpos,filename=outfilename

return
end
