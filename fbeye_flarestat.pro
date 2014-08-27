function fbeye_flarestat,time,flux,flux_sm,f0,f1
; calculate the important stats to populate the other fields
; - peak (time & flux)
; - equiv. duration

compile_opt defint32, strictarr, strictarrsubs
compile_opt HIDDEN

;-- fractional flux units
;; yflux = (flux_sm - median(flux_sm)) / median(flux_sm) 
; yflux = (flux - flux_sm) / median(flux_sm) 

; (use time in seconds, assume normally given in days)
; calcualte area under flare, and above surrounding LC
if f0 eq f1 then begin
   print,'warning: flare starts/stops at same data point'
   print,'         ...adding 1 data point.'
   f1 = f1+1
endif

;-- use linear approximation for slope beneath flare from start/stop points
;; ed = tsum([time[f0]*(24.*60.*60.),time[f1]]*(24.*60.*60.),[yflux[f0],yflux[f1]])

;-- just integrate fractional flux units (more simple, but not great)
;; ed = tsum(time[f0:f1]*86400d0, yflux[f0:f1])


dur = (time[f1[0]] - time[f0[0]])[0]
c1 = where(time ge time[f0[0]]-dur*0.75 and time lt time[f0[0]]-dur*0.1)
if c1[0] eq -1 then c1 = where(abs(time-time[f0[0]]) lt .001)

c2 = where(time gt time[f1[0]]+dur*0.25 and time le time[f1[0]]+dur)
if c2[0] eq -1 then c2 = where(abs(time-time[f1[0]]) lt .001)

slope = (median(flux[c1])-median(flux[c2])) / (median(time[c1])-median(time[c2]))
inter = median(flux[c1])-slope*median(time[c1])
fit =[inter,slope]

flux_n = (flux - poly(time, fit)) / median(flux[[c1,c2]])
ed = TSUM(time[f0:f1]*864000d0, flux_n[f0:f1])

;; plot,time,flux,xrange=[time[f0[0]]-dur,time[f1[0]]+dur],/xsty,/ysty
;; oplot,psym=6,symsize=4,thick=5, [median(time[c1]),median(time[c2])],[median(flux[c1]),median(flux[c2])]
;; oplot,time,poly(time,fit)
;; print,'ED ',ed
;; stop

;-- find peak between start/stop index
lpeak = max(flux[f0:f1], peak_ind, /nan)
tpeak = time[f0 + peak_ind]

;-- return the answers
outstat = [ed, tpeak, lpeak]
return,outstat
end
