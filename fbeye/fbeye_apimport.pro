pro fbeye_apimport, lightcurve

compile_opt defint32, strictarr, strictarrsubs
compile_opt HIDDEN

print,'>> running FBEYE_APIMPORT'

; read the columnated text file from "appaloosa"

readcol, strmid(lightcurve, 0, strpos(lightcurve, '.dat')) + '.flare', $
         f='(F)', /silent, $
         tstart, tstop, tpeak, lpeak, $
         FWHM, duration, t_peak_aflare1, t_FWHM_aflare1, amplitude_aflare1, $
         flare_chisq, KS_d_model, KS_p_model, KS_d_cont, KS_p_cont, ed


trise = tpeak - tstart
tdecay = tstop - tpeak

fevent = findgen(n_elements(tstart))
fstartpos = fltarr(n_elements(tstart)) - 99.
fstoppos = fltarr(n_elements(tstart)) - 99.

cplx_flg = fltarr(n_elements(tstart)) * 0.

mltpk_flg = fltarr(n_elements(tstart)) - 99.
mltpk_num = fltarr(n_elements(tstart)) - 99.
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

print, '> Generating file '+outfilename
return
end
