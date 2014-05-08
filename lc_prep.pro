pro lc_prep,filename,flat=flat,raw=raw,smooth=smooth,outfile=outfile
; clean and prep a kepler_lc for FBeye use
; a standalone script, not incorporated in the FBeye suite

t = mrdfits(filename,1,/silent)

time = t.time
flux = real(t.pdcsap_flux,-99)
 err = real(t.pdcsap_flux_err,-99)

if keyword_set(raw) then begin
   flux = t.sap_flux
    err = t.sap_flux_err
endif

if keyword_set(flat) then begin
   mm = smooth(flux,5000,/edge_truncate,/nan)
   flux2 = flux - mm + mean(mm,/nan)
   flux = flux2
endif


if total(t.sap_quality gt 0) gt 0 then $
   fbeye_remove,where(t.sap_quality gt 0),time,flux,err
if total(err lt -1) gt 0 then $
   fbeye_remove,where(err lt -1),time,flux,err

if keyword_set(smooth) then flux = smooth(flux,smooth,/edge_truncate,/nan)

if not keyword_set(outfile) then outfile = filename

openw,1,outfile+'.dat'
for i=0L,n_elements(time)-1 do $
   printf,1,time[i],flux[i],err[i],f='(D25.15,D20.5,D20.5)'

close,1

plot,time,flux,psym=3,/xstyle,/ystyle


return
end
