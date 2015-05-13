function fbeye_flarestat,time,flux,flux_sm,f0,f1, tstart,tstop
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
   print,'         ...adding 1 data point to end.'
   f1 = f1+1
endif

;-- use linear approximation for slope beneath flare from start/stop points
dur = (time[f1[0]] - time[f0[0]])[0]
;c1 = where(time ge time[f0[0]]-dur*0.75 and time lt time[f0[0]]-dur*0.1)
c1 = where(time ge time[f0[0]]-dur*1.5 and time lt time[f0[0]])
if c1[0] eq -1 then c1 = where(abs(time-time[f0[0]]) lt .001)
;-- remove overlap of neighbor flares
xc1 = where(tstart lt time[f0[0]] and tstop ge min(time[c1]))
if xc1[0] ne -1 then begin 
   tr = [-1] ; to-remove index
   ; then for each overlapping flare...
   for k=0l,n_elements(xc1)-1 do begin
      tr = [tr, where(time[c1] ge tstart[xc1[k]] and $
                      time[c1] le tstop[xc1[k]])]
   endfor
   tr = tr[1:*]
   tr = tr[sort(tr)]
   tr = tr[uniq(tr)]
   if n_elements(tr) eq n_elements(c1) and $
      n_elements(tr) gt 2 then $
      tr = tr[0:(n_elements(tr)-3)]
   if n_elements(tr) lt n_elements(c2) then $
      fbeye_remove, tr, c1
endif


; define the continuum region after the flare
c2 = where(time gt time[f1[0]] and time le time[f1[0]]+dur*1.5)
if c2[0] eq -1 then c2 = where(abs(time-time[f1[0]]) lt .001)
; do any flares start between this flare's start and the end of
; the second continuum region?
xc2 = where(tstart gt time[f0[0]] and tstart le max(time[c2]) )
if xc2[0] ne -1 then begin
   tr = [-1] ; to-remove index
   ; then for each overlapping flare...
   for k=0l,n_elements(xc2)-1 do begin
      tr = [tr, where(time[c2] ge tstart[xc2[k]] and $
                      time[c2] le tstop[xc2[k]]) ]
   endfor
   tr = tr[1:*]
   tr = tr[sort(tr)]
   tr = tr[uniq(tr)]
   ; if to-remove is same size as continuum region
   ;  AND larger than 2 points...
   if n_elements(tr) eq n_elements(c2) and $ 
      n_elements(tr) gt 2 then $
         tr = tr[2:*]
   if n_elements(tr) lt n_elements(c2) then $
      fbeye_remove, tr, c2
endif




mf = median(flux[[c1,c2]])
mt = median(time[[c1,c2]])

;-- this is the brute-force (robust) linear fit, using medians
slope = (median(flux[c1]-mf)-median(flux[c2]-mf)) / $
        (median(time[c1]-mt)-median(time[c2]-mt))
inter = median(flux[c1]-mf)-slope*median(time[c1]-mt)
fit = [inter,slope]

;-- now use a 2nd order polynomial,
;   recenter to aid fit minimizer
fit2 = POLY_FIT( time[[c1,c2]]-mt, $
                 flux[[c1,c2]]-mf, 2, status=status,/double)
;-- if fit is OK, then use 2nd order (better for starspot subtraction)
if status[0] eq 0 then fit = fit2


flux_n = (flux - (poly(time-mt, fit)+mf)) / mf
ed = TSUM(time[f0:f1]*86400d0, flux_n[f0:f1])


;-- compute the s2n
std = stddev( flux_n[[c1,c2]] )
noise = std * dur * 86400d0
; this Signal to Noise ratio becomes poisson for large energy,
; or small stddev (local noise level)
s2n = abs(ed / sqrt(ed + noise))

;--- vis for sanity checking
;; loadct,39,/silent
;; print,ed,std,s2n,status
;; plot,time,flux,xrange=[time[min(c1)],time[max(c2)]],/xsty,/ysty
;; oplot,time[f0:f1],flux[f0:f1],color=150
;; oplot,time[c1],flux[c1],color=250,psym=1
;; oplot,time[c2],flux[c2],color=250,psym=1
;; oplot,time,poly(time-mt,fit)+mf,color=90
;; stop
;---


;-- find peak between start/stop index
lpeak = max(flux_n[f0:f1], peak_ind, /nan)
tpeak = time[f0 + peak_ind]

; add new parameter to save
quies = mf

;-- return the answers
outstat = [ed, tpeak, lpeak, s2n, quies]
return,outstat
end
