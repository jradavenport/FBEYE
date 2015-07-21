;+
; NAME: FBEYE_PCENT
;
; PURPOSE: compute the percentile values in an array. Searaches over
;   the range [0, 1]
;
; CALLING SEQUENCE:
;   fbeye_pcent(data, lower, upper)
;
; INPUTS:
;   data: the 1-d array of values to search over
;
;   lower, upper: the percentiles to find values at. Returns the
;                 closest match, or NaN if beyond the range of the
;                 data. Values must be between 0 and 1.
;
; OUTPUTS:
;   A 2 element array
;
; NOTES:
;   This could (should?) be re-done to be similar to the numpy
;   percentile function, that can take an arbitrary list of limits
;
;   This could possibly be sped up (optional /fast mode?) using the
;   HISTOGRAM function, at the cost of precision.
;
;-
function fbeye_pcent, flux, lower, upper
  

  
  ; set options for the compiler
  compile_opt defint32, strictarr, strictarrsubs
  ; suppress some outputs
  compile_opt HIDDEN

  On_error,2
  if N_params() lt 2 then begin
     print, 'Error: Must supply array to search and at least one limit'
     return, [!VALUES.F_NAN, !VALUES.F_NAN]
  endif

  ; if only 1 limit is supplied, assume it is the upper limit
  ; and provide the minimum for lower limit
  if N_params() lt 3 then begin
     upper = lower
     lower = 0.
  endif
  
  ;-- possibly faster way to do this, at expense of precision:
  ; use the IDL histogram to compute the approximate percentile
  ; values of [lower, upper] for an array (flux)


  ss = sort(flux)
  sflux = flux[ss]

  ; the cumulative distribution, with correction to span from [0,1]
  cumul = total(sflux - sflux[0],/cumulative) / total(sflux - sflux[0])

  up_x = where(cumul ge upper)
  lo_x = where(cumul le lower)

  if up_x[0] eq -1 then begin
     print, 'Warning: Upper limit beyond range of data!'
     upper_flux = !VALUES.F_NAN
  endif else $
     upper_flux = sflux[up_x[0]]
  
  
  if lo_x[0] eq -1 then begin
     print, 'Warning: Lower limit beyond range of data!'
     lower_flux = !VALUES.F_NAN
  endif else $
     lower_flux = sflux[max(lo_x)]
  
  
  return, [lower_flux, upper_flux]
end
