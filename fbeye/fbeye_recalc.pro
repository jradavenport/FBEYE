pro fbeye_recalc,time,flux,fsmooth,outfilename

compile_opt defint32, strictarr, strictarrsubs
compile_opt HIDDEN

print,'> Using previous flare t_start and t_stop from '+outfilename
restore, outfilename

print,'> Recalculating properties for ', $
      strtrim(string(total(fevent gt 0),f='(I)'),2),' valid flare events...'

nflare = n_elements(tstart)

tpeak = fltarr(nflare)
trise = fltarr(nflare)
tdecay = fltarr(nflare)
lpeak = fltarr(nflare)
ed = fltarr(nflare)
fstartpos = fltarr(nflare)
fstoppos = fltarr(nflare)
s2n = fltarr(nflare)
quies = fltarr(nflare)

FOR n=0L,nflare-1 DO BEGIN
   if fevent[n] le 0 then continue

   fstarttmp = where(abs(time-tstart[n[0]]) eq $
                     min(abs(time-tstart[n[0]])))
   fstoptmp = where(abs(time-tstop[n[0]]) eq $
                    min(abs(time-tstop[n[0]])))

   if fstarttmp[0] eq fstoptmp[0] then $
      fstoptmp = fstoptmp+1

   fstartpos[n] = fstarttmp
   fstoppos[n] = fstoptmp

   tmpstat = FBEYE_FLARESTAT(time,flux, fsmooth, fstarttmp, fstoptmp, $
                             tstart,tstop)

   ed[n] = tmpstat[0]
   tpeak[n] = tmpstat[1]
   trise[n] = tmpstat[1]-time[fstarttmp]
   tdecay[n] = time[fstoptmp]-tmpstat[1]
   lpeak[n] = tmpstat[2]
   s2n[n] = tmpstat[3]
   quies[n] = tmpstat[4]

ENDFOR

cksum = 0
; dont save tlastviewed or dtlast
save,fevent,fstartpos,fstoppos,$
     tpeak,tstart,tstop,trise,tdecay,lpeak,ed,$
     cplx_flg,mltpk_flg,mltpk_num,tmltpk,lmltpk,multpos,s2n,quies,cksum,$
     filename=outfilename


return
end
