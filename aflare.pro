function aflare,t,p
;
;  This is the Analytic Flare Model from the flare-morphology paper
;  Please reference Davenport (2014) http://arxiv.org/abs/1411.3723
;
;  jrad@astro.washington.edu
;
;
;; p = [tpeak, fwhm (units of time), amplitude (units of flux)] x N
;;    preserve variable order for backwards compatibility w/ modelnflare 
Nflare = floor(n_elements(p)/3.0)

;--- 2 pieces:
; 1) 4th order polynomial from t=[-1:0]
;fr = [1.00,1.8905,-0.8035,-3.414,-1.720]
fr = [1.00000, 1.94053, -0.175084, -2.24588, -1.12498]


; 2) double exponential from t=[0:*]
;fd = [0.555896,-1.83774,0.440413,-0.466723]
fd = [0.689008, -1.60053, 0.302963, -0.278318]

flare = fltarr(n_elements(t))
for i=0l,Nflare-1 do begin
   outm=((fr[0]+$                                             ; 0th order
          fr[1]*((t-p[0+i*3])/p[1+i*3])+$                     ; 1st order
          fr[2]*((t-p[0+i*3])/p[1+i*3])^2.+$                  ; 2nd order
          fr[3]*((t-p[0+i*3])/p[1+i*3])^3.+$                  ; 3rd order
          fr[4]*((t-p[0+i*3])/p[1+i*3])^4. )*$                ; 4th order
         (t le p[0+i*3] and (t-p[0+i*3])/p[1+i*3] gt -1.) + $ ; rise  mask
         ( fd[0]*exp( ((t-p[0+i*3])/p[1+i*3])*fd[1] ) + $     ; first exp
           fd[2]*exp( ((t-p[0+i*3])/p[1+i*3])*fd[3] ) ) * $   ; second exp
         (t gt p[0+i*3])) * $                                 ; decay  mask
        P[2+i*3]                                              ; amplitude
   
   flare = flare + outm
endfor
return,flare
end
