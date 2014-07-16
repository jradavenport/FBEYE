pro FBEYE_ADDFLARE,time,flux,flux_sm,f0,f1,fevent,fstartpos,fstoppos,tpeak,tstart,tstop,trise,tdecay,lpeak,ed,cplx_flg,mltpk_flg,mltpk_num,tmltpk,lmltpk,multpos,filename=filename,noauto=noauto

compile_opt defint32, strictarr, strictarrsubs
compile_opt HIDDEN

; check to see this flare is unique (not within any fstart & fstop)
  overlap = where(f0 ge fstartpos and f1 le fstoppos)
  if overlap[0] ne -1 then begin
     FBEYE_MSG,'Not a new flare event.'
     print,'Not a new flare event.'
     return
  endif


; pass to routine that calculates stats on flares
; returns single array with these elements:
; [ed, tpeak, peak]
tmpstat = FBEYE_FLARESTAT(time,flux,flux_sm,f0,f1)

; automatically (simply) pick a flare type
newtype = 1
if not keyword_set(noauto) then $
   newtype = FBEYE_AUTOTYPE(time,flux_sm,f0,f1) 

cplx_flg=[cplx_flg,newtype]            ; flag for flare type

; add a new entry
fevent = [fevent,max(fevent)+1]        ; event id
fstartpos=[fstartpos,f0]               ; start index
fstoppos =[fstoppos, f1]               ; stop index
tpeak=[tpeak,tmpstat[1]]               ; time of peak
tstart=[tstart,time[f0[0]]]            ; << NEW
tstop=[tstop,time[f1[0]]]              ; << NEW tstart,tstop,trise,tdecay,
trise=[trise,tmpstat[1]-time[f0[0]]]   ; << NEW
tdecay=[tdecay,time[f1[0]]-tmpstat[1]] ; << NEW
lpeak=[lpeak,tmpstat[2]]               ; max luminosity (peak)
ed=[ed,tmpstat[0]]                     ; equivalent duration
;cplx_flg=[cplx_flg,0]                  ; flag for flare type

mltpk_flg=[mltpk_flg,0]                ; flag for multi-peak
mltpk_num=[mltpk_num,0]                ; number of mult-peak
tmltpk=[tmltpk,0]                      ; time of mult-peak
lmltpk=[lmltpk,0]                      ; max luminosity of mult-peak
multpos=[multpos,0]                    ; index of mult-peak


; save new structure
save,fevent,fstartpos,fstoppos,tpeak,tstart,tstop,trise,tdecay,lpeak,ed,cplx_flg,mltpk_flg,mltpk_num,tmltpk,lmltpk,multpos,filename=filename


return
end
