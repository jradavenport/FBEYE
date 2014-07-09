pro fbeye_stddisp,version,keynames
compile_opt defint32, strictarr, strictarrsubs
compile_opt HIDDEN
loadct,0,/silent
xyouts,charsize=1.5,0.88,.96,/normal,'!18FBeye!3 '+version ; disp version
arrow,.2,0,.2,1,hthick=0,/normal,hsize=0 ; frame boarderxyouts,/normal,.05,.45,'Msg:',charsize=1

;keyboard button
loadct,0,/silent
xyouts,.15,.95,charsize=.8,'(K)EYBOARD',color=100,/norm

xyouts,.01,.9,'+ Flare ('+keynames[0]+')',/normal
xyouts,.1,.9,'- Flare ('+keynames[1]+')',/normal
arrow,[0,.09,.09,0],[.89,.89,.93,.93],[.09,.09,0,0],[.89,.93,.93,.89],/normal,hthick=0,hsize=0,color=60
arrow,[.1,.19,.19,.1],[.89,.89,.93,.93],[.19,.19,.1,.1],[.89,.93,.93,.89],/normal,hthick=0,hsize=0,color=60

xyouts,.01,.85,'+ Sub-Peak',/normal
xyouts,.1,.85,'- Sub-Peak',/normal
arrow,[0,.09,.09,0],[.84,.84,.88,.88],[.09,.09,0,0],[.84,.88,.88,.84],/normal,hthick=0,hsize=0,color=60
arrow,[.1,.19,.19,.1],[.85-.01,.85-.01,.85+.03,.85+.03],[.19,.19,.1,.1],[.85-.01,.85+.03,.85+.03,.85-.01],/normal,hthick=0,hsize=0,color=60


arrow,[0,.05,.05,0],[.77-.03,.77-.03,.77+.03,.77+.03],[.05,.05,0,0],[.77-.03,.77+.03,.77+.03,.77-.03],/normal,hthick=0,hsize=0,color=60
xyouts,.007,.77,'Type ('+keynames[2]+')',/normal


arrow,[.07,.14,.14,.07],[.77-.03,.77-.03,.77+.03,.77+.03],[.14,.14,.07,.07],[.77-.03,.77+.03,.77+.03,.77-.03],/normal,hthick=0,hsize=0,color=60
xyouts,.08,.77,'Smooth ('+keynames[3]+')',/normal


arrow,/normal,hthick=0,hsize=0,color=60,[.145,.185,.185,.145],[.77-.03,.77-.03,.77+.03,.77+.03],[.185,.185,.145,.145],[.77-.03,.77+.03,.77+.03,.77-.03]
xyouts,.154,.77,'!18i!3 ('+keynames[4]+')',/normal,charsize=1.4


;; xyouts,.01,.65,'+ Y Zoom',/normal
;; xyouts,.1,.65,'- Y Zoom',/normal
arrow,[0,.09,.09,0],[.65-.01,.65-.01,.65+.03,.65+.03],[.09,.09,0,0],[.65-.01,.65+.03,.65+.03,.65-.01],/normal,hthick=0,hsize=0,color=60
arrow,[.1,.19,.19,.1],[.65-.01,.65-.01,.65+.03,.65+.03],[.19,.19,.1,.1],[.65-.01,.65+.03,.65+.03,.65-.01],/normal,hthick=0,hsize=0,color=60
xyouts,.01,.65,'+ Y ('+keynames[5]+')',/normal
xyouts,.1,.65,'- Y ('+keynames[6]+')',/normal

xyouts,.05,.7,'Y Zoom Lock ('+keynames[7]+')',/normal
;;plot,/normal,cos(FINDGEN(17) * (!PI*2/16.))+.01,sin(FINDGEN(17) * (!PI*2/16.))+.7,/noerase

;; xyouts,.01,.6,'+ dt Zoom',/normal
;; xyouts,.1,.6,'- dt Zoom',/normal
arrow,[0,.09,.09,0],[.6-.01,.6-.01,.6+.03,.6+.03],[.09,.09,0,0],[.6-.01,.6+.03,.6+.03,.6-.01],/normal,hthick=0,hsize=0,color=60
arrow,[.1,.19,.19,.1],[.6-.01,.6-.01,.6+.03,.6+.03],[.19,.19,.1,.1],[.6-.01,.6+.03,.6+.03,.6-.01],/normal,hthick=0,hsize=0,color=60
xyouts,.01,.6,'+ dt ('+keynames[8]+')',/normal
xyouts,.1,.6,'- dt ('+keynames[9]+')',/normal

arrow,[0,.09,.09,0],[.5-.01,.5-.01,.5+.03,.5+.03],[.09,.09,0,0],[.5-.01,.5+.03,.5+.03,.5-.01],/normal,hthick=0,hsize=0,color=60
arrow,[.1,.19,.19,.1],[.5-.01,.5-.01,.5+.03,.5+.03],[.19,.19,.1,.1],[.5-.01,.5+.03,.5+.03,.5-.01],/normal,hthick=0,hsize=0,color=60
xyouts,.01,.5,'- Time ('+keynames[10]+')',/normal
xyouts, .1,.5,'+ Time ('+keynames[11]+')',/normal


;msg box
xyouts,/normal,.05,.42,'Message:',charsize=1
arrow,[.02,.18,.18,.02],[.1,.1,.4,.4],[.18,.18,.02,.02],[.1,.4,.4,.1],/normal,hthick=0,hsize=0

loadct,39,/silent
; quit button w/ box
xyouts,.05,.05,'Save+Quit ('+keynames[12]+')',/normal
arrow,[.04,.14,.14,.04],[.02,.02,.08,.08],[.14,.14,.04,.04],[.02,.08,.08,.02],/normal,hthick=0,hsize=0,color=250

; reset disp button
arrow,/normal,hthick=0,hsize=0,[0,.054,.054,0],[.97,.97,.99,.99],[.054,.054,0,0],[.97,.99,.99,.97]
xyouts,/normal,.005,.975,'reset ('+keynames[13]+')',charsize=1,color=165


return
end
