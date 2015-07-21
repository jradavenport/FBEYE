function fbeye_autotype,time,flux,f0,f1

compile_opt defint32, strictarr, strictarrsubs
compile_opt HIDDEN

;---- auto choose a type, based on simple info in the flare:
; default = Classical (1)
; a "significant" secondary peak = Complex (2)
; cannot auto-find Unusual(3)
; any flare with duration < 10 datum = Maybe (4)
if abs(f1-f0) le 10 then return,4

thetype = 1 ; now default to CLASSICAL

; no short duration complex... too messy
if (f1-f0) lt 20 then return,1

;-- look for secondary peak(s)
max = max(flux[f0:f1],b)
min = min(flux[f0:f1])

; pad w/ a 0 incase flare at END of lightcurve
dfdt = ([flux[1:*] - flux,0])[f0:f1]

pscale = 0.3

x = where((flux[f0:f1]-min)/(max-min) gt pscale)
if n_elements(x) gt 2 then begin ; find other high points
   
; find secondary peak AFTER max, and with PSCALE*max amplitude
   if b lt n_elements(dfdt)-4 then begin
      y = where(dfdt[(b+3):*]/(max-min) gt pscale)
   endif else begin
      y = -1
   endelse
;;   y = where(x gt b+3 and dfdt[x] gt pscale*(max-min))
;;   y = where(x gt b+3 and ((flux[f0:f1]-min)/(max-min))[x] gt pscale and sgn[x]-sgn[x-1] lt 1)
   if y[0] ne -1 then thetype=2

; find secondary peak BEFORE max, and with PSCALE*max amplitude
   ;z = where(x lt b-3 and dfdt[x] gt pscale*dfdt[b])
   z = -1
   if b gt 5 then z = where(dfdt[0:(b-1)]/(max-min) gt pscale)
   if z[0] ne -1 then thetype=2
endif


;if time[f0] gt 540.159+.9 then stop

return,thetype
end
