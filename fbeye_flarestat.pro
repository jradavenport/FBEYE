function fbeye_flarestat,time,flux,flux_sm,f0,f1
; calculate the important stats to populate the other fields
; - rise time
; - decay time
; - peak
; - equiv. duration

compile_opt defint32, strictarr, strictarrsubs
compile_opt HIDDEN

;yflux = (flux - mean([flux[f0],flux[f1]]))/median(flux)
yflux = (flux_sm-median(flux_sm))/median(flux_sm)

; (use time in seconds, assume given in days)
; calcualte area under flare, and above surrounding LC
if f0 eq f1 then begin
   print,'warning: flare starts/stops at same data point'
   print,'         ...adding 1 data point.'
   f1 = f1+1
   ed1 = tsum(time[f0:f1]*86400., yflux[f0:f1])
endif

if f0 ne f1 then $
   ed1 = tsum(time[f0:f1]*86400., yflux[f0:f1])

; use linear approximation for slope beneath flare from start/stop points
;; ed0 = tsum([time[f0]*(24.*60.*60.),time[f1]]*(24.*60.*60.),[yflux[f0],yflux[f1]])

lpeak = max(flux[f0:f1],peak_ind,/nan)
tpeak = time[f0+peak_ind]

outstat = [ed1, tpeak, lpeak]

return,outstat
end
