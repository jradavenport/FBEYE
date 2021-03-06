function fbeye_pick, time, flux, error, fstart, fstop, fdur, kk=kk,corr=corr,$
                     fsmooth=fsmooth
; a sorta fast auto flare finder
; FBEYE_PICK just returns the indicies that stand out as possible
; flares epochs when using the SOFTSERVE smoothing routine 

  compile_opt defint32, strictarr, strictarrsubs
  compile_opt HIDDEN

  if not keyword_set(kk) then kk = 1.
  
  if not keyword_set(fsmooth) then $
     fsmooth = softserve(time,flux,kk)
  
  yya = abs(flux-fsmooth)
  yyb =(fsmooth)

  myy = median(yya) ; median absolute residual


;-- here's the arbitrary selection criteria, chosen by trial/error
;   selects all points above the smoothed threshold
  pick = where(yya gt 2.5d0*myy and $
               flux gt yyb and $
               flux gt 2.5d0*error+yyb)

  fl_d = pick[1:*]-pick
; make flares have to be separated by more than 1 epoch
  fstart = pick[where([3,fl_d] gt 2)]
  fstop = pick[where([fl_d,3] gt 2)]


  fdur = (fstop-fstart) + 1      ; index duration

  IF total(fdur lt 2) eq n_elements(fdur) then begin
     fstart = -1
     fstop = -1
     fdur = -1
     print,'NO FLARES FOUND'
     return,-1
  ENDIF

  if total(fdur lt 2) gt 0 then $
     fbeye_remove,where(fdur lt 2),fstart,fstop,fdur

; Optional correction for start and stop times.
; Determined empirically, calibrated for GJ 1243
; with 1-min Kepler data.
  if keyword_set(corr) then begin
     tstart = time[fstart]
     tstop = time[fstop]
     dur = tstop-tstart

; the coefficients I found by hand, w/ polynomial fitting
; using a sample of flares from month 1,2
; with both manual and auto flare finding at work
     fit_start = [-1.60022, 0.634893, 0.116893]
     fit_stop = [0.580246, 1.82442, 0.311467]

; the coeff's seemed to be over-correcting, having a hard time
; separating flares. thus i divided the corrections by 2. This is a
; conservative correction, and should be enough to allow better
; fitting for flare shapes
     tstart = tstart - 10.^poly(alog10(dur),fit_start)/2.0
     tstop = tstop + 10.^poly(alog10(dur),fit_stop)/2.0


     ;; for n=0L,n_elements(tstart)-1 do fstart[n] = where(abs(time-tstart[n]) eq min(abs(time-tstart[n])))
     ;; for n=0L,n_elements(tstop)-1 do fstop[n] = where(abs(time-tstop[n]) eq min(abs(time-tstop[n])))
     newpick = [-1]
     for n=0L,n_elements(tstart)-1 do newpick = [newpick,where(time ge tstart[n] and time le tstop[n])]
     newpick = newpick[1:*]
     fl_d = newpick[1:*]-newpick
     fstart = newpick[where([3,fl_d] gt 2)]
     fstop = newpick[where([fl_d,3] gt 2)]
  endif

  fdur = fstop-fstart
  return,pick
end
