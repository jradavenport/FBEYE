pro fbeye,lightcurve,debug=debug,recalculate=recalculate,$
          keyboard=keyboard,auto=auto,noauto=noauto
 
;-----------------
; Flares By EYE
; Created by:
;    J. R. A. Davenport
; With helpful input by many students/collaborators



print,'> Welcome to FBeye...'
print,' Note: Setting /auto will disable interactive mode.'
print,'       Setting /noauto will disable all auto-finding.'
print,'       Setting /recalculate will use prev start/stop times '
print,'         and recompute the Equiv. Durations, etc.'
print,'  '

device, retain = 2
device, true_color = 24
device, decomposed = 0
; set options for the compiler
compile_opt defint32, strictarr, strictarrsubs



if keyword_set(keyboard) then keyboard = 1 else keyboard = -1

;   auto-find the FBEYE path
FBEYE_PATH = file_search(strsplit(!path,path_sep(/search),/extract),'fbeye.pro')
FBEYE_PATH = strmid(fbeye_path,0,strpos(fbeye_path,'fbeye.pro'))
FBEYE_PATH = FBEYE_PATH[0]
;== this may not be the most efficient way to do this... oh well

;== open file w/ the keyboard shortcuts, which user can edit
openr,lun,FBEYE_PATH+'fbeye.keys',/get_lun
keynames = ''
line=''
WHILE NOT EOF(LUN) DO BEGIN
   READF,lun,line,f='(A1)'
   keynames = [keynames,line]
ENDWHILE
keynames=keynames[1:*]


if not keyword_set(debug) then begin
; set options for the compiler
   compile_opt defint32, strictarr, strictarrsubs
; suppress some outputs
   compile_opt HIDDEN
endif

; set display properties to behave correctly
set_plot,'X'
device, retain = 2
device, true_color = 2
device, decomposed = 0

VERSION = 'v1.1.6'
print,"  You are currently running FBEYE "+VERSION


if keyword_set(debug) then print,'Debug: 1'
if not keyword_set(lightcurve) then begin
   lightcurve='a'
   read,lightcurve,prompt='> Please enter the lightcurve filename: '
   if strlen(lightcurve) lt 2 then return
   lightcurve = strtrim(lightcurve,2) ; remove lead/trail spaces
endif

if keyword_set(debug) then print,'Debug: 2'
if FILE_TEST(lightcurve) eq 0 then begin
   print,'Error: File not found ['+lightcurve+']'
   return
endif
print,''
print,'> Reading the Lightcurve file: '+lightcurve

;----- required to have an error column
readcol,lightcurve,/silent,f='(D,D,D)',time,flux,error


tlastviewed = -9d9 ; a time value that shouldn't exist
dtlast = -9d9 ; a time bin that shouldn't exist

;1) 
if keyword_set(debug) then print,'Debug: 3'
doover = 'y'
; LOOK FOR THE OUTPUT ALREADY EXISTING...
already_done = FILE_TEST(lightcurve+'.out')
already_own = FILE_TEST(lightcurve+'.out',GET_MODE=out_per,/USER)
if keyword_set(debug) then print, 'already_done = ',already_done
if keyword_set(debug) then print, 'out_per=',out_per


if already_done eq 1 then $
   print,'> ['+lightcurve+'] output files already exist.'

if not keyword_set(auto) then begin
   if already_done eq 1 then $
      read,doover,prompt='> Use existing results (y/n)? [default: y]  '
   if doover eq 'n' then $
      already_done=0
endif

if already_done eq 1 and keyword_set(auto) then $
   doover = 'y'
   

; temp file 
if already_done eq 1 then begin
   restore,lightcurve+'.out'
endif


; IF NOT, RUN ; SAVE THIS OUTPUT
if already_done eq 0 then begin
   print,''
   finds={fstartpos:0d0,fstoppos:0d0,pstartpos:0d0,pstoppos:0d0}

; now convert the output in a useful format for FBeye
   fstartpos = finds.fstartpos
   fstoppos = finds.fstoppos
   pstartpos = finds.pstartpos
   pstoppos = finds.pstoppos

; now send these vectors into a jrad flare-characterizing program
;---- to be written.... will fill-out lots of these #'s

;the vectors (quantities) we ultimately want are:
; flare event ID
;      this includes all sub-peaks w/i same flare!!!
   fevent = indgen(n_elements(fstartpos))
; tstart = fstartpos
; tstop = fstoppos
; tpeak
   tpeak = fix((pstartpos+pstoppos)/2.) ; this will do for now
; delta L peak
   Lpeak = fltarr(n_elements(fevent)) ; just pick max real quick for now
;; for i=0L,n_elements(Lpeak)-1 do Lpeak[i]=max(flux[(fstartpos[i]):(fstoppos[i])])
; ED
   ED = fltarr(n_elements(fevent)) 
; complex flag - can be enabled, auto-enabled if mltpk_flg != 0
   cplx_flg = fltarr(n_elements(fevent)) 
; mult peak flag
   mltpk_flg = fltarr(n_elements(fevent)) 
; # of secondary peaks
   mltpk_num = fltarr(n_elements(fevent)) 
; t mult peak
   tmltpk = fltarr(n_elements(fevent)) 
; delta L mult peak
   Lmltpk = fltarr(n_elements(fevent)) 
; where the mult-peak is marked
   multpos = fltarr(n_elements(fevent)) 

tstart= fltarr(n_elements(fevent)) 
tstop= fltarr(n_elements(fevent)) 
trise= fltarr(n_elements(fevent)) 
tdecay= fltarr(n_elements(fevent)) 
s2n = fltarr(n_elements(fevent)) 

   save,fevent,fstartpos,fstoppos,tpeak,tstart,tstop,trise,tdecay,lpeak,ed,cplx_flg,mltpk_flg,mltpk_num,tmltpk,lmltpk,multpos,s2n,filename=lightcurve+'.out'

;   spawn,'chmod 777 '+lightcurve+'.out'
;   if FILE_TEST(lightcurve) eq 0 then
   FILE_CHMOD,lightcurve+'.out', '777'o
endif

if keyword_set(debug) then print,'Debug: 4'
;== read in paramters file
;; readcol,FBEYE_PATH+'fbeye.par',skipline=1,f='(a)',$
;;         tunit,/silent
; removed file fbeye.par, which just contained these lines:
;; tunit
;; days

tunit = 'days'

print,'> Generating smooth lightcurve'
print,' ... this can take a few moments.'
fsmooth = softserve(time,flux)
flux_sm = flux - fsmooth + median(flux)



; reprocess stuff if requested
; this a good way to update data from prior version of FBeye
; assumes only the time,flux,start/stop index
; recomputes all the durations/energies/etc
; preserves flags, etc, if set
if keyword_set(recalculate) then begin
   yn = 'n'
   print,'Flare recalculation requested. This cannot be undone!'
   if not keyword_set(auto) then begin
      read,yn,prompt='>  Are you sure? (y/n) [default: n] '
      if yn eq 'y' then print,'>  OKAY! '
      if yn ne 'y' then print,'> Canceled'
   endif
   if keyword_set(auto) then $
      yn = 'y'
   
   if yn eq 'y' then FBEYE_RECALC,time,flux,fsmooth,lightcurve+'.out'

   print,'> '
   goto,theend
;   RETURN ;<<<< done with program
endif



;-- force errors >= 0
if total(error le 0) gt 0 then $
   error[where(error le 0)] = 1.0

; run the simple jrad auto-find stuff
; it will save indicies that we'll want for later (pick)
if not keyword_set(noauto) then begin
   print,'> Auto flare finding...'
   pick = FBEYE_PICK(time, flux, error, $ ; returns start/stop auto-find indx
                     pflarestart, pflarestop,/corr,fsmooth=fsmooth) 
   
   if already_done eq 0 then begin
      IF pflarestart[0] ne -1 then begin
         FOR n=0L,n_elements(pflarestart)-1L DO BEGIN
            FBEYE_ADDFLARE,time,flux,flux_sm,pflarestart[n],pflarestop[n],$
                           fevent,fstartpos,fstoppos,tpeak,tstart,tstop,trise,tdecay,$
                           lpeak,ed,cplx_flg,mltpk_flg,mltpk_num,tmltpk,lmltpk,$
                           multpos,s2n,filename=lightcurve+'.out'
         ENDFOR
      ENDIF
   endif ; /already_done

   print,'>'
   if keyword_set(auto) then goto,theend

endif ; /noauto

if keyword_set(noauto) then begin
   print,'> Skipping Auto-find...'
   pick = -1
   pflarestart = -1
   pflarestop = -1
endif

;=========
;set up the graphics window, and plot settings
loadct,39,/silent
!p.charsize=1.2
set_plot,'X'
!p.font = -1
window,0,xsize=1200,ysize=700,title='FBeye '+VERSION

;=========


time0=min(time,/nan)
maxtime=max(time,/nan)
lock=0
ylock = 0 ; lock to fix the yzoom
if dtlast gt 0 then dt = dtlast else begin
   if median(time[1:*]-time) ge 0.01 then dt = 1.5 ; days
   if median(time[1:*]-time) lt 0.01 then dt = 0.5
endelse

if time[1]-time[0] gt dt then dt = (time[1]-time[0]) * 10.
t=0 ; start time to view
;-- update if looked at previously
if tlastviewed gt 0 then t = tlastviewed
yzm = [1.,1]
smlock = 0
fluxsv = flux

task = 0 ;  not complete================================================
while task eq 0 do begin
if keyword_set(debug) then print,'Debug: 5'
device,cursor_standard = 22
; cursor images are available here:
;http://tronche.com/gui/x/xlib/appendix/b/
ERASE

;==== plot raw LC  in time bin =====
yu = where(time-time0 gt t and time-time0 lt t+dt)
if yu[0] ne -1 and ylock ne 1 then begin
   yrng = minmax(flux[yu],/nan) * yzm
   yrng = FBEYE_REAL(yrng,mean(flux,/nan)-stddev(flux,/nan))
endif

if keyword_set(debug) then print,'Debug: yrange=',yrng


plot,time-time0,flux,/xstyle,/ystyle,xrange=[t,t+dt],$
     xtitle='Time ('+tunit[0]+')',$
     position=posgen(1,1,1,xsp=-.85),ytickn=replicate(' ',8),$
     psym=10,yrange=[ yrng ],/nodata;,ytitle='Flux'

if keyword_set(error) then begin
   loadct,0,/silent
   ploterror,time-time0,flux,error,/xstyle,/ystyle,xrange=[t,t+dt],xtitle='Time ('+tunit[0]+')', position=posgen(1,1,1,xsp=-.85),ytickn=replicate(' ',8),psym=10,yrange=[ yrng ],/noerase,/nohat,errcolor=65;,ytitle='Flux'
   loadct,39,/silent
endif


;==== plot start/stop times in time bin =====
;  only do lines within the plot range
xx0 = where(fstartpos gt 0)

if xx0[0] ne -1 then begin
   ac = where((time[fstartpos[xx0]]-time0 ge t and time[fstartpos[xx0]]-time0 le t+dt and mltpk_num[xx0] eq 0) or $
              (time[fstoppos[xx0]]-time0 ge t and time[fstoppos[xx0]]-time0 le t+dt and mltpk_num[xx0] eq 0))

   if ac[0] ne -1 then for n=0L,n_elements(ac)-1 do begin
      fclr = 20
      if cplx_flg[xx0[ac[n]]] eq 1 then fclr=50
      loadct,0,/silent
      if cplx_flg[xx0[ac[n]]] eq 2 then begin
         cubehelix,/silent,rot=1,start=2
         fclr=65
      endif
      if cplx_flg[xx0[ac[n]]] eq 3 then begin
         cubehelix,/silent,rot=1,start=3
         fclr=65
      endif
      if cplx_flg[xx0[ac[n]]] eq 4 then begin
         cubehelix,/silent,rot=1,start=1
         fclr=65
      endif


      if time[(fstoppos[xx0])[ac[n]]]-time0 le t+dt and $
         time[(fstartpos[xx0])[ac[n]]]-time0 ge t then $
            polyfill,[time[(fstartpos[xx0])[ac[n]]],time[(fstartpos[xx0])[ac[n]]],$
                      time[(fstoppos[xx0])[ac[n]]],time[(fstoppos[xx0])[ac[n]]]]-time0,$
                     [yrng[0],yrng[1],yrng[1],yrng[0]],color=fclr
      if time[(fstartpos[xx0])[ac[n]]]-time0 lt t then $
         polyfill,[t+time0,t+time0,$
                   time[(fstoppos[xx0])[ac[n]]],time[(fstoppos[xx0])[ac[n]]]]-time0,$
                  [yrng[0],yrng[1],yrng[1],yrng[0]],color=fclr
      if time[(fstoppos[xx0])[ac[n]]]-time0 gt t+dt then $
         polyfill,[time[(fstartpos[xx0])[ac[n]]],time[(fstartpos[xx0])[ac[n]]],$
                   t+dt+time0,t+dt+time0]-time0,$
                  [yrng[0],yrng[1],yrng[1],yrng[0]],color=fclr
      loadct,39,/silent
      oplot,[time[(fstartpos[xx0])[ac[n]]],time[(fstartpos[xx0])[ac[n]]]]-time0,[yrng],color=60,linestyle=2,thick=.8

      oplot,[time[(fstoppos[xx0])[ac[n]]],time[(fstoppos[xx0])[ac[n]]]]-time0,[yrng],color=250,linestyle=2,thick=.8
   endfor 
endif

;=== oplot sub-peaks ===
xx = where(mltpk_flg eq 1)
if xx[0] ne -1 then for n=0L,n_elements(xx)-1 do $
   oplot,[time[multpos[xx[n]]],time[multpos[xx[n]]]]-time0,[yrng],color=114,linestyle=2,thick=.8


;==== oplot LC again to put on top =====
plot,time-time0,flux,/xstyle,/ystyle,xrange=[t,t+dt],xtitle='Time ('+tunit[0]+')', position=posgen(1,1,1,xsp=-.85),psym=10,yrange=[ yrng ],/noerase,title='FILE: '+lightcurve;,ytickn=replicate(' ',8),ytitle='Flux'

xyouts,/norm,0.24,0.94,'Flux'

; oplot jrad auto-suggested points
; these will always update with latest version of the auto-finder, no
; matter if you re-use previous results or not.
if pick[0] ne -1 then $
   oplot,time[pick]-time0,flux[pick],psym=4,color=170,symsize=0.5,thick=1.5


;==== plot a std dev bar on the side =====
;; if yu[0] ne -1 then begin
;;    fmi = median(flux[yu],/even)
;;    fsi = stddev(flux[yu],/nan)
;;    oploterror,[t+dt-dt/20.],[fmi],[fsi],color=222,errcolor=222,thick=2
;; endif

FBEYE_STDDISP,version,keynames          ; generate the normal GUI
FBEYE_MSG,string(total(fstartpos gt 0),f='(I05)')+'  flares       '+$
          string(total(mltpk_flg gt 0),f='(I05)')+' sub-peaks     '+$
          string(maxtime-time0,f='(F06.1)')+' '+tunit[0]+' of data '

if ylock eq 1 then xyouts,.05,.7,'Y Zoom Lock ('+keynames[7]+')',/normal,color=212
if smlock eq 1 then xyouts,.08,.77,'Smooth ('+keynames[3]+')',/normal,color=90

if lock eq 0 then btn = 0
while btn le 0 do begin
if keyword_set(debug) then print,'Debug: 6'
   clkx = 1
   clky = 1

 if (keyboard) ne 1 then begin
    CURSOR,clkx,clky,/down,/normal 
   if clkx gt 0.2 then btn = 0 & lock=0

   if clkx lt 0.2 then begin
; quit?
      if clkx gt .04 and clkx lt .14 and clky gt .02 and clky lt .08 then btn=99
; +flare
      if clkx gt 0 and clkx lt .09 and clky gt .89 and clky lt .93 then btn=20
      if clkx gt .1 and clkx lt .19 and clky gt .89 and clky lt .93 then btn=21

; time
      if clkx gt 0 and clkx lt .09 and clky gt .49 and clky lt .53 then btn =41
      if clkx gt .1 and clkx lt .19 and clky gt .49 and clky lt .53 then btn =40

; dt
      if clkx gt 0 and clkx lt .09 and clky gt .6-.01 and clky lt .6+.03 then btn =51
      if clkx gt .1 and clkx lt .19 and clky gt .6-.01 and clky lt .6+.03 then btn =50

; y zoom
      if clkx gt 0 and clkx lt .09 and clky gt .65-.01 and clky lt .65+.03 then btn =60
      if clkx gt .1 and clkx lt .19 and clky gt .65-.01 and clky lt .65+.03 then btn =61
      if clkx gt .05 and clkx lt .125 and clky gt .69 and clky lt .71 then btn=62

; multipeak
      if clkx gt 0 and clkx lt .09 and clky gt .84 and clky lt .88 then btn =70
      if clkx gt .1 and clkx lt .19 and clky gt .84 and clky lt .88 then btn =71

; reset
      if clky gt .97 and clkx lt .04 then btn=98

; info
      if clkx gt .145 and clkx lt .185 and clky gt .77-.03 and clky lt .77+.03 $
      then btn=411

; type
      if clkx gt 0 and clkx lt .05 and clky gt .77-.03 and clky lt .77+.03 $
      then btn=30

; smooth
      if clkx gt .07 and clkx lt .13 and clky gt .74 and clky lt .8 $
      then btn = 66

      if clkx gt .15 and clkx lt .2 and clky gt .94 and clky lt .96 then btn = 777

   endif
endif
 if keyboard eq 1 then begin
    kclk = ''
    xyouts,.15,.95,charsize=.8,'(K)EYBOARD',color=250,/norm
    print,kclk,'enter keyboard input'
    kclk = get_kbrd()
;   read,kclk,prompt='>'

    ; table of conditional inputs
; f = add flare
; d = delete flare
; s = smooth
; t = fwd time
; r = bck time
; i = info
; q = quit

    ;; if strlowcase(kclk) eq 'q' then btn = 99  ; quit
    ;; if strlowcase(kclk) eq 'f' then btn = 20  ; add flare
    ;; if strlowcase(kclk) eq 'd' then btn = 21  ; delete flare
    ;; if strlowcase(kclk) eq 't' then btn = 40  ; forward time
    ;; if strlowcase(kclk) eq 'r' then btn = 41  ; backward time
    ;; if strlowcase(kclk) eq 'i' then btn = 411 ; info
    ;; if strlowcase(kclk) eq 'y' then btn = 30  ; type
    ;; if strlowcase(kclk) eq 's' then btn = 66  ; smooth
    ;; if strlowcase(kclk) eq 'w' then btn = 51  ; time zoom in
    ;; if strlowcase(kclk) eq 'e' then btn = 50  ; time zoom out
    ;; if strlowcase(kclk) eq 'g' then btn = 61  ; y zoom in
    ;; if strlowcase(kclk) eq 'b' then btn = 60  ; y zoom out
    ;; if strlowcase(kclk) eq 'l' then btn = 62  ; y lock
    ;; if strlowcase(kclk) eq '~' then btn = 98  ; reset

    if strlowcase(kclk) eq keynames[12] then btn = 99  ; quit
    if strlowcase(kclk) eq keynames[0] then btn = 20  ; add flare
    if strlowcase(kclk) eq keynames[1] then btn = 21  ; delete flare
    if strlowcase(kclk) eq keynames[11] then btn = 40  ; forward time
    if strlowcase(kclk) eq keynames[10] then btn = 41  ; backward time
    if strlowcase(kclk) eq keynames[4] then btn = 411 ; info
    if strlowcase(kclk) eq keynames[2] then btn = 30  ; type
    if strlowcase(kclk) eq keynames[3] then btn = 66  ; smooth
    if strlowcase(kclk) eq keynames[8] then btn = 51  ; time zoom in
    if strlowcase(kclk) eq keynames[9] then btn = 50  ; time zoom out
    if strlowcase(kclk) eq keynames[5] then btn = 61  ; y zoom in
    if strlowcase(kclk) eq keynames[6] then btn = 60  ; y zoom out
    if strlowcase(kclk) eq keynames[7] then btn = 62  ; y lock
    if strlowcase(kclk) eq keynames[13] then btn = 98  ; reset

    if strlowcase(kclk) eq 'k' then begin     ; disable keyboard

       btn = 999
       keyboard = -1
    endif
 endif

endwhile
if keyword_set(debug) then print,'Debug: btn=',btn

;FBEYE_MSG,string(btn)

if btn eq 777 then begin $
   keyboard = 1
;print key names?
endif

if btn eq 99 then begin
   print,'> Quit Selected. Have a nice day.'
   tlastviewed = t
   dtlast = dt
;   FBEYE_MSG,'Quit Selected. Have a nice day.'
   task = 1 ;this is how to quit
   save,fevent,fstartpos,fstoppos,tpeak,tstart,tstop,trise,tdecay,lpeak,ed,cplx_flg,mltpk_flg,mltpk_num,tmltpk,lmltpk,multpos,tlastviewed,dtlast,s2n,filename=lightcurve+'.out'
   ;; save,fevent,fstartpos,fstoppos,tpeak,tstart,tstop,trise,tdecay,lpeak,ed,cplx_flg,mltpk_flg,mltpk_num,tmltpk,lmltpk,multpos,tlastviewed,dtlast,filename=lightcurve+'.sav'

   ;; spawn,'chmod 777 '+lightcurve+'.out'
   ;; spawn,'chmod 777 '+lightcurve+'.sav'
   already_own = FILE_TEST(lightcurve+'.out',GET_MODE=out_per,/USER)
   if already_own eq 1 then $
      FILE_CHMOD,lightcurve+'.out', '777'o
   ;; FILE_CHMOD,lightcurve+'.sav', '777'

endif

; reset display
if btn eq 98 then yzm=[1.,1.] ; reset y-zoom
if btn eq 98 then begin ; re-ingest data from scratch
   ;if keyword_set(error) then $
      readcol,lightcurve,/silent,f='(D,D,D)',time,flux,error
   ;if not keyword_set(error) then $
   ;   readcol,lightcurve,/silent,f='(D,D)',time,flux
endif
if btn eq 98 then continue ; back to top of loop


; smooth
if btn eq 66 then begin
   if smlock eq 0 then begin
      flux = flux_sm;flux - softserve(time,flux) + median(flux)
      tmp = 1
      print,'> Using SOFTSERVE smoothing perscription...'
      
   endif
   if smlock eq 1 then begin
      flux = fluxsv
      tmp = 0
   endif
   smlock = tmp
endif

; control time
if btn eq 40 then begin 
   t=t+dt/2.
;   yzm=[1.,1.]
endif
if btn eq 41 then begin
   t=t-dt/2.
;   yzm=[1.,1.]
endif 
if t lt 0 then  t=0.
if t ge max(time-min(time,/nan),/nan) then t = t-dt/2.

;control dt
if btn eq 50 then begin
   dt = dt*2.
;   yzm=[1.,1.]
endif

if btn eq 51 then begin
   dt = dt/2.
;   yzm=[1.,1.]
endif

;control y zoom
if btn eq 60 then begin
   if ylock eq 0 then yzm = yzm*[1,((yrng[1]-yrng[0])*0.75+yrng[0])/yrng[1]]
   if ylock eq 1 then ylock=0
endif
if btn eq 61 then begin
   if ylock eq 0 then yzm = yzm*[1,((yrng[1]-yrng[0])*1.25+yrng[0])/yrng[1]]
   if ylock eq 1 then ylock=0
endif
if btn eq 62 then begin
   if ylock eq 0 then tmp=1
   if ylock eq 1 then tmp=0
   ylock=tmp
endif

; ADD NEW FLARE ----
if btn eq 20 then begin
   if lock eq 1 then xyouts,.01,.9,'+ Flare ('+keynames[0]+')',/normal,color=212
   device,cursor_standard=106;30
   FBEYE_MSG,'+ FLARE selected    CLICK flare start',90
   CURSOR,f0,tmp,/down,/data

   locktest = CONVERT_COORD(f0,tmp,/data,/to_normal)
; allow user to lock +flare on
   if lock eq 0 and locktest[0] gt 0 and locktest[0] lt .09 and locktest[1] gt .89 and locktest[1] lt .93 then begin
      lock=1
      continue
   endif
   if lock eq 1 and f0 lt t then lock = 0
   if lock eq 0 and f0 lt t then continue


   if f0 gt t+dt then continue
   ind0 = where(abs(time-min(time,/nan) -f0) eq min(abs(time-min(time,/nan) -f0),/nan))
   ind0 = ind0[0]

   if keyword_set(debug) then print,'f0=',f0
   oplot,[f0,f0],[-1d6,1d9],color=170,linestyle=1
   FBEYE_MSG,'+ FLARE selected    CLICK flare stop',250
   CURSOR,f1,tmp,/down,/data
   if f1 eq f0 then continue
   if f1 lt f0 then continue
   if f1 gt t+dt then continue

   if keyword_set(debug) then print,'f1=',f1
   
   oplot,[f1,f1],[-1d6,1d9],color=170,linestyle=1
   ind1 = where(abs(time-min(time,/nan) -f1) eq min(abs(time-min(time,/nan) -f1),/nan))
   ind1 = ind1[0]

;error trap for both start/stop being different, but within same datum
; (important for long-cadence)
   if ind0 eq ind1 then begin
      if ind1 lt n_elements(time)-1 then ind1 = ind1+1 else $
         if ind0 gt 0 then ind0 = ind0-1
      print,'fix'
   endif


;now add to flare library for this star, so gets plotted each time
;  & save the flare library for this star
   FBEYE_ADDFLARE,time,flux,flux_sm,ind0,ind1,fevent,fstartpos,fstoppos,tpeak,tstart,tstop,trise,tdecay,lpeak,ed,cplx_flg,mltpk_flg,mltpk_num,tmltpk,lmltpk,multpos,s2n,filename=lightcurve+'.out',noauto=noauto

; refresh flare library - not a polite way to code this...
   restore,lightcurve+'.out'
endif

; DELETE FLARE ----
if btn eq 21 then begin
   FBEYE_MSG,'- FLARE selected    CLICK within event',250
   device,cursor_standard = 88;36
   CURSOR,f0,tmp,/down,/data
   if f0 lt t then continue
   if f0 gt t+dt then continue
   ind = where(abs(time-min(time,/nan) -f0) eq min(abs(time-min(time,/nan) -f0),/nan))   
   FBEYE_DELFLARE,ind[0],fevent,fstartpos,fstoppos,tpeak,tstart,tstop,trise,tdecay,lpeak,ed,cplx_flg,mltpk_flg,mltpk_num,tmltpk,lmltpk,multpos,s2n,filename=lightcurve+'.out'
endif

; + MULT PEAK -----
if btn eq 70 then begin
   if lock eq 1 then xyouts,.01,.85,'+ Sub-Peak',/normal,color=212
   device,cursor_standard=30
   FBEYE_MSG,'+ SUB PEAK          CLICK flare start',90
   CURSOR,f0,tmp,/down,/data
   locktest = CONVERT_COORD(f0,tmp,/data,/to_normal)

;  allow user to lock +flare on
   if lock eq 0 and locktest[0] gt 0 and locktest[0] lt .09 and locktest[1] gt .84 and locktest[1] lt .88 then begin
      lock=1
      continue
   endif
   if lock eq 1 and f0 lt t then lock = 0
   if lock eq 0 and f0 lt t then continue

   if f0 gt t+dt then continue

   ind0 = where(abs(time-min(time,/nan) -f0) eq min(abs(time-min(time,/nan) -f0),/nan))

;now add to flare library for this star, so gets plotted each time
;  & save the flare library for this star
   FBEYE_ADDMULT,ind0[0],fevent,fstartpos,fstoppos,tpeak,tstart,tstop,trise,tdecay,lpeak,ed,cplx_flg,mltpk_flg,mltpk_num,tmltpk,lmltpk,multpos,s2n,filename=lightcurve+'.out'
; refresh flare library - not a polite way to code this
   restore,lightcurve+'.out'
endif

; - MULT PEAK -----
if btn eq 71 then begin
   FBEYE_MSG,'- SUB PEAK          CLICK near sub-peak',250
   device,cursor_standard = 36
   CURSOR,f0,tmp,/down,/data
   if f0 lt t then continue
   if f0 gt t+dt then continue
   ind = where(abs(time-min(time,/nan) -f0) eq min(abs(time-min(time,/nan) -f0),/nan))
   FBEYE_DELMULT,ind[0],fevent,fstartpos,fstoppos,tpeak,tstart,tstop,trise,tdecay,lpeak,ed,cplx_flg,mltpk_flg,mltpk_num,tmltpk,lmltpk,multpos,s2n,filename=lightcurve+'.out'
endif

;--- flare event info ----
if btn eq 411 then begin
   FBEYE_MSG,' flare event info'
   device,cursor_standard = 24
   CURSOR,f0,tmp,/down,/data
   if f0 lt t then continue
   if f0 gt t+dt then continue
   ind = where(abs(time-min(time,/nan) -f0) eq min(abs(time-min(time,/nan) -f0),/nan))
   FBEYE_INFO,ind[0],fevent,fstartpos,fstoppos,tpeak,tstart,tstop,trise,tdecay,lpeak,ed,cplx_flg,mltpk_flg,mltpk_num,tmltpk,lmltpk,multpos
endif

;--- TYPE -------
if btn eq 30 then begin 
; we'll use the cplx_flg to hold the "type"
   FBEYE_MSG,'Choose Type'
   xyouts,.007,.77,'Type ('+keynames[2]+')',/normal,color=90
;   polyfill,/normal,color=10,[.065,.2,.2,.065,.065],[.71,.71,.82,.82,.71]
   loadct,0,/silent
   polyfill,/normal,color=50,[.065,.2,.2,.065,.065],[.8,.8,.83,.83,.8]
   cubehelix,/silent,rot=1,start=2
   polyfill,/normal,color=65,[.065,.2,.2,.065,.065],[.77,.77,.8,.8,.77]
   cubehelix,/silent,rot=1,start=3
   polyfill,/normal,color=65,[.065,.2,.2,.065,.065],[.74,.74,.77,.77,.74]
   cubehelix,/silent,rot=1,start=1
   polyfill,/normal,color=65,[.065,.2,.2,.065,.065],[.71,.71,.74,.74,.71]
   loadct,39,/silent
   xyouts,.08,.81,'Classical (1)',/normal,charsize=1.2
   xyouts,.08,.78,'Complex (2)',/normal,charsize=1.2
   xyouts,.08,.75,'Unusual (3)',/normal,charsize=1.2
   xyouts,.08,.72,'Maybe (4)',/normal,charsize=1.2

   if lock eq 0 then begin
      tmpflg = -1
      
      if keyboard ne 1 then begin
         CURSOR,f0,tmp0,/down,/normal
      endif
      
      if keyboard eq 1 then begin
         kclk = ''
         kclk = get_kbrd()
         f0 = 0.08
         if strlowcase(kclk) eq '1' then tmp0 = 0.81
         if strlowcase(kclk) eq '2' then tmp0 = 0.78
         if strlowcase(kclk) eq '3' then tmp0 = 0.75
         if strlowcase(kclk) eq '4' then tmp0 = 0.72
         if strlowcase(kclk) ne '1' and strlowcase(kclk) ne '2' and strlowcase(kclk) ne '3' and strlowcase(kclk) ne '4' then begin
            f0 = 0
            tmp0 = 0
         endif 
         
      endif

      if f0 lt .075 or f0 gt .14 or tmp0 gt .83 or tmp0 lt .71 then continue
      if tmp0 gt .8 and tmp0 lt .83 then begin
         xyouts,.08,.81,'Classical (1)',/normal,charsize=1.2,color=145
         tmpflg = 1
      endif
      if tmp0 gt .77 and tmp0 lt .8 then begin
         xyouts,.08,.78,'Complex (2)',/normal,charsize=1.2,color=145
         tmpflg = 2
      endif
      if tmp0 gt .74 and tmp0 lt .77 then begin
         xyouts,.08,.75,'Unusual (3)',/normal,charsize=1.2,color=145
         tmpflg = 3
      endif
      if tmp0 gt .71 and tmp0 lt .74 then begin
         xyouts,.08,.72,'Maybe (4)',/normal,charsize=1.2,color=145
         tmpflg = 4
      endif
      
   endif
   if lock eq 1 then begin
      if tmpflg eq 1 then xyouts,.08,.81,'Classical (1)',/normal,charsize=1.2,color=213
      if tmpflg eq 2 then xyouts,.08,.78,'Complex (2)',/normal,charsize=1.2,color=213
      if tmpflg eq 3 then xyouts,.08,.75,'Unusual (3)',/normal,charsize=1.2,color=213
      if tmpflg eq 4 then xyouts,.08,.72,'Maybe (4)',/normal,charsize=1.2,color=213
   endif
   
   FBEYE_MSG,'Choose Flare'
   CURSOR,f1,tmp1,/down,/data
   locktest = CONVERT_COORD(f1,tmp1,/data,/to_normal)
   if lock eq 0 and locktest[0] gt .075 and locktest[0] lt .14 and $
      abs(locktest[1]-tmp0) lt .03 then begin
      lock=1
      continue
   endif
   if lock eq 1 and f1 lt t then lock=0
   
   if f1 lt t then continue
   if f1 gt t+dt then continue
   ind = where(abs(time-min(time,/nan) -f1) eq min(abs(time-min(time,/nan) -f1),/nan))
   
   FBEYE_TYPE,ind[0],tmpflg,fevent,fstartpos,fstoppos,tpeak,tstart,tstop,trise,tdecay,lpeak,ed,cplx_flg,mltpk_flg,mltpk_num,tmltpk,lmltpk,multpos,s2n,filename=lightcurve+'.out'
endif

;
;_____ END OF INTERACTIVE _______
endwhile 

wdelete,!D.WINDOW

; if the user has flares selected, then save to an output file
; as a change of pace: save as a text file in the working directory


theend: print,'> Writing output (.fbeye) file, please wait.'
restore,lightcurve+'.out' ; read output file in one more time


xx0 = where(fstartpos gt 0)
close,/all


openw,4,lightcurve+'.fbeye'
                                ; print a header line
printf,4,'#'
printf,4,'# This output file was generated by FBEYE version '+VERSION
printf,4,'# Created on '+systime()
printf,4,"# The column names are: "
printf,4,'# Event_ID, Start_INDX, Stop_INDX, t_peak, t_start, t_stop, t_rise, t_decay, Flux_peak, Equiv_Dur, S/N, CPLX_flg, MLTPK_flg, MLTPK_num, t_MLTPK, L_mltpk, MLTPK_INDX'
printf,4,'# where INDX is the index in the light curve file '+lightcurve
printf,4,'# MLTPK is multiple peak stuff, probably ignore'
printf,4,'# CPLX_flg is the complex flag:'
printf,4,'#    1=classical, 2=complex, 3=weird, 4=maybe'
printf,4,'# '

if xx0[0] gt -1 then begin
   for n=1L,n_elements(fevent)-1 do $
      printf,4,fevent[n],fstartpos[n],fstoppos[n],$
             tpeak[n],tstart[n],tstop[n],$
             trise[n],tdecay[n],lpeak[n],ed[n],s2n[n],$
             cplx_flg[n],mltpk_flg[n],mltpk_num[n],$
             tmltpk[n],lmltpk[n],multpos[n],$
             f='(i,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,"  ;")'
   close,4
endif
   ;print,'> OUTPUT being saved to local text file'
;;    spawn,'chmod 777 '+lightcurve+'.fbeye'
FILE_CHMOD,lightcurve+'.fbeye', '777'o


return
end

