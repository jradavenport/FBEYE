pro fbeye_apimport, lightcurve_in

compile_opt defint32, strictarr, strictarrsubs
compile_opt HIDDEN

print,'>> running FBEYE_APIMPORT'


; if the appaloosa lightcurve has a .lc.gz extension, uncompress and
; convert to .dat for FBEYE
if strpos(lightcurve_in, '.lc.gz') ne -1 then begin
   print,'>> Converting .lc.gz file to .dat'
   spawn,'gunzip -k ' + lightcurve_in

   ;-- for PDC flux
   ;; readcol, strmid(lightcurve_in, 0, strpos(lightcurve_in, '.gz')), $
   ;;          time, flux, error,f='(X, F, F, F)'

   ;-- for SAP flux
   readcol, strmid(lightcurve_in, 0, strpos(lightcurve_in, '.gz')), $
            time, flux, error,f='(X,D,X,X,X,X,F,F)', /silent

    lightcurve = strmid(lightcurve_in, 0, strpos(lightcurve_in, '.lc.gz')) + '.dat'

    forprint, textout=lightcurve,$
        time, flux, error, /silent, f='(D20.10, D20.10, D20.10)',/nocomment

    flare_file = strmid(lightcurve, 0, strpos(lightcurve, '.dat')) + '.flare'
endif

; if the appaloosa lightcurve has a .fits extension, open and extract columns,
; convert to .dat for FBEYE
if strpos(lightcurve_in, '.fits') ne -1 then begin
    lc = mrdfits(lightcurve_in, 1, /silent)
    lightcurve = strmid(lightcurve_in, 0, strpos(lightcurve_in, '.fits')) + '.dat'

    time = lc.time
    flux = real(lc.sap_flux, -99)
    error = real(lc.sap_flux_err, -99)

    if (where(flux eq -99))[0] ne -1 then $
        remove, where(flux eq -99), time, flux, error

    forprint, textout=lightcurve, time, flux, error, /silent, $
        f='(D20.10, D20.10, D20.10)',/nocomment

    flare_file = lightcurve_in + '.flare'

endif

if strpos(lightcurve_in, '.fits') eq -1 and strpos(lightcurve_in, '.lc.gz') eq -1 then begin
    print,'FBEYE_APIMPORT ERROR: Only .fits and .lc.gz file formats supported at this time.'
    return
endif

if FILE_TEST(flare_file) eq 1 then begin
    ; read the columnated text file from "appaloosa"
    print,'>> Reading .flare file, converting to .out format.'
    readcol, flare_file, f='(D)', /silent, $
            tstart_0, tstop_0, tpeak_0
            ;; ignore other columns, not needed! ##
            ;  tstart, tstop, tpeak, lpeak, $
            ;  FWHM, duration, t_peak_aflare1, t_FWHM_aflare1, amplitude_aflare1, $
            ;  flare_chisq, KS_d_model, KS_p_model, KS_d_cont, KS_p_cont, ed

    ; create flare event ID's. Events with ID=0 are ignored, so put an extra
    ; event w/ ID=0 up front so arrays are always full
    fevent = findgen(n_elements(tstart_0) + 1)
    tstart = [-1, tstart_0]
    tstop = [-1, tstop_0]
    tpeak = [-1, tpeak_0]
endif else begin
    print,'>> No .flare file found, creating a blank .out file.'

    tstart = [-1]
    tstop = [-1]
    tpeak = [-1]

    ; make a flare event to be ignored
    fevent = [0]
endelse

trise = tpeak - tstart
tdecay = tstop - tpeak

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
cksum = 0

; make a save file
save,fevent,fstartpos,fstoppos,$
     tpeak,tstart,tstop,trise,tdecay,lpeak,ed,$
     cplx_flg,mltpk_flg,mltpk_num,tmltpk,lmltpk,multpos,$
     s2n,quies,tlastviewed,dtlast,cksum,$
     filename=outfilename

print, '>> Generating file '+outfilename

print, '>> FBEYE_APIMPORT is complete. To use the results, do the following:'
print, '   IDL> fbeye, "' + lightcurve + '" '

lightcurve_in = lightcurve

return
end
