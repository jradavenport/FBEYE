pro fbeye,lightcurve,no_auto=no_auto,debug = debug,error=error,recalculate=recalculate
print,'> Welcome to FBeye...'
print,'  The available options are '
print,'  /no_auto,  /error,  /recalculate'
;-----------------
; Flares By EYE
;    J. R. A. Davenport
;

;== experimental:
;   auto-find the FBEYE path
FBEYE_PATH = file_search(strsplit(!path,path_sep(/search),/extract),'fbeye.pro')
FBEYE_PATH = strmid(fbeye_path,0,strpos(fbeye_path,'fbeye.pro'))
FBEYE_PATH = FBEYE_PATH[0]
;== this may not be the most efficient way to do this...

;;FBEYE_PATH = '/Users/james/idl/jrad/fbeye_master/'



if not keyword_set(debug) then begin
; set options for the compiler
   compile_opt defint32, strictarr, strictarrsubs
; suppress some outputs
   compile_opt HIDDEN
endif

set_plot,'X'
device, retain = 2
device, true_color = 2
device, decomposed = 0


if keyword_set(debug) then print,'Debug: 1'
if not keyword_set(lightcurve) then begin
   lightcurve='a'
   read,lightcurve,prompt='> Please enter the lightcurve filename: '
   if strlen(lightcurve) lt 2 then return
endif

if keyword_set(debug) then print,'Debug: 2'
   if FILE_TEST(lightcurve) eq 0 then begin
      print,'Error: File not found ['+lightcurve+']'
      return
   endif
   print,''
   print,'> Reading the Lightcurve file: '+lightcurve
   if keyword_set(error) then readcol,lightcurve,/silent,f='(D,D,D)',time,flux,error
   if not keyword_set(error) then begin
      readcol,lightcurve,/silent,f='(D,D)',time,flux
   endif



;1) 
if keyword_set(debug) then print,'Debug: 3'
doover = 'y'
; LOOK FOR THE OUTPUT ALREADY EXISTING...
already_done = FILE_TEST(FBEYE_PATH+'tmp/'+lightcurve+'.out')
if already_done eq 1 then print,'> ['+lightcurve+'] output files already exist.'
if already_done eq 1 then read,doover,prompt='> Use existing results (y/n)? [default: y]  '
if doover eq 'n' then already_done=0

; temp file should be in FBEYE_PATH/tmp
if already_done eq 1 then begin
   restore,FBEYE_PATH+'tmp/'+lightcurve+'.out'
   if FILE_TEST(FBEYE_PATH+'tmp/'+lightcurve+'.auto') then restore,FBEYE_PATH+'tmp/'+lightcurve+'.auto' else no_auto=1
endif


; IF NOT, RUN ; SAVE THIS OUTPUT
if already_done eq 0 then begin
   print,''
   if not keyword_set(no_auto) then begin
      autodetect_flares,time,flux,finds,mined,duration
      print,'> Running Hilton Auto-Detect Flares.'
      print,'> This may take some time!'
      print,''
      save,filename=FBEYE_PATH+'tmp/'+lightcurve+'.auto',finds
   endif
   if keyword_set(no_auto) then finds={fstartpos:0d0,fstoppos:0d0,pstartpos:0d0,pstoppos:0d0}

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


   save,fevent,fstartpos,fstoppos,tpeak,tstart,tstop,trise,tdecay,lpeak,ed,cplx_flg,mltpk_flg,mltpk_num,tmltpk,lmltpk,multpos,filename=FBEYE_PATH+'tmp/'+lightcurve+'.out'
endif

if keyword_set(debug) then print,'Debug: 4'
;== read in paramters file
readcol,FBEYE_PATH+'fbeye.par',skipline=1,f='(a)',$
        tunit,/silent

print,''
print,'> NOTE: Output files are being stored in:'
print,FBEYE_PATH+'tmp/'
print,''


; reprocess stuff if requested
; this a good way to update data from prior version of FBeye
; assumes only the time,flux,start/stop index
; recomputes all the durations/energies/etc
; preserves flags, etc, if set
if keyword_set(recalculate) then begin
   yn = 'n'
   read,yn,prompt='> Flare recalculation requested. Are you sure? (y/n) [default: n] '
   if yn eq 'y' then FBEYE_RECALC,time,flux,FBEYE_PATH+'tmp/'+lightcurve+'.out'
   if yn ne 'y' then print,'> Canceled'
   if yn eq 'y' then print,'  DONE! '
   print,'> Have a nice day.'
   RETURN ;<<<< done with program
endif



VERSION = 'v0.9b'
;=========
;set up the graphics window, and plot settings
loadct,39,/silent
!p.charsize=1.2
set_plot,'X'
window,0,xsize=1000,ysize=600,title='FBeye '+VERSION

;=========


time0=min(time,/nan)
maxtime=max(time,/nan)
lock=0
ylock = 0 ; lock to fix the yzoom
dt = 0.5 ; days
if time[1]-time[0] gt dt then dt = (time[1]-time[0]) * 10.
t=0 ; 0 to 1
yzm = [1.,1]

task = 0 ; means not complete================================================
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
     position=posgen(1,1,1,xsp=-.85),ytitle='Flux',ytickn=replicate(' ',8),$
     psym=10,yrange=[ yrng ],/nodata


;==== plot start/stop times in time bin =====
;  only do lines wihtin the plot range
xx = where(fstartpos gt 0)
if xx[0] ne -1 then begin
   ac = where((time[fstartpos[xx]]-time0 ge t and time[fstartpos[xx]]-time0 le t+dt and mltpk_num eq 0) or (time[fstoppos[xx]]-time0 ge t and time[fstoppos[xx]]-time0 le t+dt and mltpk_num eq 0))
   if ac[0] ne -1 then for n=0L,n_elements(ac)-1 do begin
      loadct,0,/silent
      if time[(fstoppos[xx])[ac[n]]]-time0 le t+dt and $
         time[(fstartpos[xx])[ac[n]]]-time0 ge t then $
            polyfill,[time[(fstartpos[xx])[ac[n]]],time[(fstartpos[xx])[ac[n]]],$
                      time[(fstoppos[xx])[ac[n]]],time[(fstoppos[xx])[ac[n]]]]-time0,$
                     [yrng[0],yrng[1],yrng[1],yrng[0]],color=45
      if time[(fstartpos[xx])[ac[n]]]-time0 lt t then $
         polyfill,[t+time0,t+time0,$
                   time[(fstoppos[xx])[ac[n]]],time[(fstoppos[xx])[ac[n]]]]-time0,$
                  [yrng[0],yrng[1],yrng[1],yrng[0]],color=45
      if time[(fstoppos[xx])[ac[n]]]-time0 gt t+dt then $
         polyfill,[time[(fstartpos[xx])[ac[n]]],time[(fstartpos[xx])[ac[n]]],$
                   t+dt+time0,t+dt+time0]-time0,$
                  [yrng[0],yrng[1],yrng[1],yrng[0]],color=45
      loadct,39,/silent
      oplot,[time[(fstartpos[xx])[ac[n]]],time[(fstartpos[xx])[ac[n]]]]-time0,[yrng],color=60,linestyle=2,thick=.8

      oplot,[time[(fstoppos[xx])[ac[n]]],time[(fstoppos[xx])[ac[n]]]]-time0,[yrng],color=250,linestyle=2,thick=.8
   endfor 
endif
;=== oplot sub-peaks ===
xx = where(mltpk_flg eq 1)
if xx[0] ne -1 then for n=0L,n_elements(xx)-1 do $
   oplot,[time[multpos[xx[n]]],time[multpos[xx[n]]]]-time0,[yrng],color=114,linestyle=2,thick=.8

if keyword_set(error) then begin
   loadct,0,/silent
   ploterror,time-time0,flux,error,/xstyle,/ystyle,xrange=[t,t+dt],xtitle='Time ('+tunit[0]+')', position=posgen(1,1,1,xsp=-.85),ytitle='Flux',ytickn=replicate(' ',8),psym=10,yrange=[ yrng ],/noerase,/nohat,errcolor=50
   loadct,39,/silent
endif

;==== oplot LC again =====
;if not keyword_set(error) then 
plot,time-time0,flux,/xstyle,/ystyle,xrange=[t,t+dt],xtitle='Time ('+tunit[0]+')', position=posgen(1,1,1,xsp=-.85),ytitle='Flux',ytickn=replicate(' ',8),psym=10,yrange=[ yrng ],/noerase,title='FILE: '+lightcurve



FBEYE_STDDISP,version           ; generate the normal GUI
FBEYE_MSG,string(total(fstartpos gt 0),f='(I05)')+'  flares       '+$
          string(total(mltpk_flg gt 0),f='(I05)')+' sub-peaks     '+$
          string(maxtime-time0,f='(F06.1)')+' '+tunit[0]+' of data '

if ylock eq 1 then xyouts,.05,.7,'Y Zoom Lock',/normal,color=212


if lock eq 0 then btn = 0
while btn le 0 do begin
if keyword_set(debug) then print,'Debug: 6'
   clkx = 1
   clky = 1

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

   endif
endwhile
if keyword_set(debug) then print,'Debug: btn=',btn

;FBEYE_MSG,string(btn)


if btn eq 99 then begin
   print,'> Quit Selected. Have a nice day.'
;   FBEYE_MSG,'Quit Selected. Have a nice day.'
   task = 1 ;this is how to quit
   save,fevent,fstartpos,fstoppos,tpeak,tstart,tstop,trise,tdecay,lpeak,ed,cplx_flg,mltpk_flg,mltpk_num,tmltpk,lmltpk,multpos,filename=FBEYE_PATH+'tmp/'+lightcurve+'.out'
endif

; reset display
if btn eq 98 then yzm=[1.,1.]
if btn eq 98 then continue

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
   if lock eq 1 then xyouts,.01,.9,'+ Flare',/normal,color=212
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

   oplot,[f0,f0],[-1d6,1d9],color=170,linestyle=1
   FBEYE_MSG,'+ FLARE selected    CLICK flare stop',250
   CURSOR,f1,tmp,/down,/data
   if f1 lt f0 then continue
   if f1 gt t+dt then continue

   oplot,[f1,f1],[-1d6,1d9],color=170,linestyle=1
   ind1 = where(abs(time-min(time,/nan) -f1) eq min(abs(time-min(time,/nan) -f1),/nan))

;now add to flare library for this star, so gets plotted each time
;  & save the flare library for this star
   FBEYE_ADDFLARE,time,flux,ind0[0],ind1[0],fevent,fstartpos,fstoppos,tpeak,tstart,tstop,trise,tdecay,lpeak,ed,cplx_flg,mltpk_flg,mltpk_num,tmltpk,lmltpk,multpos,filename=FBEYE_PATH+'tmp/'+lightcurve+'.out'

; refresh flare library - not a polite way to code this
   restore,FBEYE_PATH+'tmp/'+lightcurve+'.out'
endif

; DELETE FLARE ----
if btn eq 21 then begin
   FBEYE_MSG,'- FLARE selected    CLICK within event',250
   device,cursor_standard = 88;36
   CURSOR,f0,tmp,/down,/data
   if f0 lt t then continue
   if f0 gt t+dt then continue
   ind = where(abs(time-min(time,/nan) -f0) eq min(abs(time-min(time,/nan) -f0),/nan))   
   FBEYE_DELFLARE,ind[0],fevent,fstartpos,fstoppos,tpeak,tstart,tstop,trise,tdecay,lpeak,ed,cplx_flg,mltpk_flg,mltpk_num,tmltpk,lmltpk,multpos,filename=FBEYE_PATH+'tmp/'+lightcurve+'.out'
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
   FBEYE_ADDMULT,ind0[0],fevent,fstartpos,fstoppos,tpeak,tstart,tstop,trise,tdecay,lpeak,ed,cplx_flg,mltpk_flg,mltpk_num,tmltpk,lmltpk,multpos,filename=FBEYE_PATH+'tmp/'+lightcurve+'.out'
; refresh flare library - not a polite way to code this
   restore,FBEYE_PATH+'tmp/'+lightcurve+'.out'
endif

; - MULT PEAK -----
if btn eq 71 then begin
   FBEYE_MSG,'- SUB PEAK          CLICK near sub-peak',250
   device,cursor_standard = 36
   CURSOR,f0,tmp,/down,/data
   if f0 lt t then continue
   if f0 gt t+dt then continue
   ind = where(abs(time-min(time,/nan) -f0) eq min(abs(time-min(time,/nan) -f0),/nan))
   FBEYE_DELMULT,ind[0],fevent,fstartpos,fstoppos,tpeak,tstart,tstop,trise,tdecay,lpeak,ed,cplx_flg,mltpk_flg,mltpk_num,tmltpk,lmltpk,multpos,filename=FBEYE_PATH+'tmp/'+lightcurve+'.out'
endif

;--- flare event info ----
if btn eq 411 then begin
   FBEYE_MSG,' flare event info'
   device,cursor_standard = 24
   CURSOR,f0,tmp,/down,/data
   if f0 lt t then continue
   if f0 gt t+dt then continue
   ind = where(abs(time-min(time,/nan) -f0) eq min(abs(time-min(time,/nan) -f0),/nan))
   FBEYE_INFO,ind[0],fevent,fstartpos,fstoppos,tpeak,tstart,tstop,trise,tdecay,lpeak,ed,cplx_flg,mltpk_flg,mltpk_num,tmltpk,lmltpk,multpos;,filename=FBEYE_PATH+'tmp/'+lightcurve+'.out'
endif

;--- TYPE -------
if btn eq 30 then begin 
; we'll use the cplx_flg to hold the "type"
   FBEYE_MSG,'Choose Type'
   xyouts,.01,.77,'Type',/normal,color=90
   polyfill,/normal,color=0,[.065,.2,.2,.065,.065],[.71,.71,.82,.82,.71]
   xyouts,.08,.81,'Classical',/normal,charsize=1.2
   xyouts,.08,.78,'Complex',/normal,charsize=1.2
   xyouts,.08,.75,'Unusual',/normal,charsize=1.2
   xyouts,.08,.72,'Maybe',/normal,charsize=1.2

if lock eq 0 then begin
   tmpflg = -1

   CURSOR,f0,tmp0,/down,/normal
   if f0 lt .075 or f0 gt .14 or tmp0 gt .83 or tmp0 lt .71 then continue
   if tmp0 gt .8 and tmp0 lt .83 then begin
      xyouts,.08,.81,'Classical',/normal,charsize=1.2,color=145
      tmpflg = 1
   endif
   if tmp0 gt .77 and tmp0 lt .8 then begin
      xyouts,.08,.78,'Complex',/normal,charsize=1.2,color=145
      tmpflg = 2
   endif
   if tmp0 gt .74 and tmp0 lt .77 then begin
      xyouts,.08,.75,'Unusual',/normal,charsize=1.2,color=145
      tmpflg = 3
   endif
   if tmp0 gt .71 and tmp0 lt .74 then begin
      xyouts,.08,.72,'Maybe',/normal,charsize=1.2,color=145
      tmpflg = 4
   endif
endif 
   if lock eq 1 then begin
      if tmpflg eq 1 then xyouts,.08,.81,'Classical',/normal,charsize=1.2,color=213
      if tmpflg eq 2 then xyouts,.08,.78,'Complex',/normal,charsize=1.2,color=213
      if tmpflg eq 3 then xyouts,.08,.75,'Unusual',/normal,charsize=1.2,color=213
      if tmpflg eq 4 then xyouts,.08,.72,'Maybe',/normal,charsize=1.2,color=213
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

   FBEYE_TYPE,ind[0],tmpflg,fevent,fstartpos,fstoppos,tpeak,tstart,tstop,trise,tdecay,lpeak,ed,cplx_flg,mltpk_flg,mltpk_num,tmltpk,lmltpk,multpos,filename=FBEYE_PATH+'tmp/'+lightcurve+'.out'
endif

;
;_____ END OF INTERACTIVE _______
endwhile 

wdelete,!D.WINDOW

return
end

