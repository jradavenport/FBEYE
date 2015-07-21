pro fbeye_flarefit,lightcurve
; A post-processing script to go through the lightcurve and fit each
; primary flare with the 8-param function.

set_plot,'X'
device, retain = 2
device, true_color = 2
device, decomposed = 0


readcol,lightcurve,/silent,f='(D,D,D)',time,flux,error


; flatten the LC
   flux = flux - softserve(time,flux) + median(flux)
   print,'> Using SOFTSERVE smoothing perscription.'

;   auto-find the FBEYE path
FBEYE_PATH = file_search(strsplit(!path,path_sep(/search),/extract),'fbeye.pro')
FBEYE_PATH = strmid(fbeye_path,0,strpos(fbeye_path,'fbeye.pro'))
FBEYE_PATH = FBEYE_PATH[0]

; restore the flare file
restore,FBEYE_PATH+'tmp/'+lightcurve+'.out'

x = where(ed gt 0)
ed1 = ed[x]
for n=0L,n_elements(x)-1 do ed1[n]=tsum(time*86400.,fluxclean,fstartpos[x[n[0]]],fstoppos[x[n[0]]])

chi = fltarr(n_elements(x))

; run the 8-param fit
for n=0L,n_elements(x)-1 do begin

; a bigger range...  n++
   rng = where(time ge tstart[x[n]]-(tstop[x[n]]-tstart[x[n]])*1.5 and $
               time le tstop[x[n]]+(tstop[x[n]]-tstart[x[n]])*2.5 )

   a = time[rng] 
   b = fluxclean[rng]

   if n_elements(b) lt 10 then continue

; new 8-parameter fit
   ptmp = [tstart[x[n]],tpeak[x[n]],max(fluxclean[rng0]),(tstop[x[n]]-tpeak[x[n]])*0.15+tpeak[x[n]],0.8,tstop[x[n]],10., median(fluxclean[rng0[0]+[-11,-2]])]

   pout= mpfitfun('flarefit8',a,b,err[rng]/median(flux)/5.,ptmp,/nan,/quiet)

   coeff[n,*] = pout

   chi[n] = total((flarefit8(a,pout)-b)^2./abs(flarefit8(a,pout)))


endfor


return
end
