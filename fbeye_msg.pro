pro fbeye_msg,msg,color
;loadct,39,/silent
if not keyword_set(color) then color=255
 
; clear the MSG window
polyfill,/normal,[.02,.18,.18,.02,.02],[.1,.1,.4,.4,.1],color=0
arrow,[.02,.18,.18,.02],[.1,.1,.4,.4],[.18,.18,.02,.02],[.1,.4,.4,.1],/normal,hthick=0,hsize=0


; step thru the msg in 20 character steps
if strlen(msg) le 20 then $
   xyouts,charsize=1.2,.025,.38,/normal,'!5'+msg,font=0,color=color

if strlen(msg) gt 20 then begin
   for n=0L,fix(strlen(msg)/20.) do begin
      xyouts,charsize=1.2, .025, .38-n*.04,/normal,'!5'+strmid(msg,n*20,20),font=0,color=color
   endfor
endif

return
end
