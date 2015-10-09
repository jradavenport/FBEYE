pro fbeye_apimport, lightcurve_in

compile_opt defint32, strictarr, strictarrsubs
compile_opt HIDDEN

print,'>> running FBEYE_APIMPORT'


; if the appaloosa lightcurve has a .lc.gz extension, uncompress and
; convert to .dat for FBEYE
if strpos(lightcurve_in, '.lc.gz') ne -1 then begin
   print,'>> Converting .lc.gz file to .dat'
   spawn,'gunzip -k ' + lightcurve_in

   readcol, strmid(lightcurve_in, 0, strpos(lightcurve_in, '.gz')), $
            time, flux, error,f='(X, F, F, F)'
   
   forprint, textout=strmid(lightcurve_in, 0, strpos(lightcurve_in, '.lc.gz')) + '.dat',$
             time, flux, error, /silent, f='(D, D, D)',/nocomment
   
   lightcurve = strmid(lightcurve_in, 0, strpos(lightcurve_in, '.lc.gz')) + '.dat'
endif else begin
   lightcurve = lightcurve_in
endelse


; read the columnated text file from "appaloosa"
print,'>> Reading .flare file, converting to .out format'
readcol, strmid(lightcurve, 0, strpos(lightcurve, '.dat')) + '.flare', $
         f='(D)', /silent, $
         tstart, tstop, tpeak, lpeak, $
         FWHM, duration, t_peak_aflare1, t_FWHM_aflare1, amplitude_aflare1, $
         flare_chisq, KS_d_model, KS_p_model, KS_d_cont, KS_p_cont, ed


trise = tpeak - tstart
tdecay = tstop - tpeak

fevent = findgen(n_elements(tstart)) + 1
fstartpos = fltarr(n_elements(tstart)) - 99.
fstoppos = fltarr(n_elements(tstart)) - 99.

cplx_flg = fltarr(n_elements(tstart)) * 0. + 1.

mltpk_flg = fltarr(n_elements(tstart))
mltpk_num = fltarr(n_elements(tstart))
tmltpk = fltarr(n_elements(tstart)) - 99.
lmltpk = fltarr(n_elements(tstart)) - 99.
multpos = fltarr(n_elements(tstart)) - 99.
s2n = fltarr(n_elements(tstart)) - 99.
quies = fltarr(n_elements(tstart)) - 99.

tlastviewed = -9d9
dtlast = -9d9


outfilename = lightcurve + ".out"

; make a save file
save,fevent,fstartpos,fstoppos,$
     tpeak,tstart,tstop,trise,tdecay,lpeak,ed,$
     cplx_flg,mltpk_flg,mltpk_num,tmltpk,lmltpk,multpos,$
     s2n,quies,tlastviewed,dtlast,$
     filename=outfilename

print, '>> Generating file '+outfilename

lightcurve_in = lightcurve
return
end
