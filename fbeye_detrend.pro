pro fbeye_detrend,time,flux,time0,t,dt,yzm,path,file

; if the detrend has not been run before, then save the orig LC
if not FILE_TEST(path+file+'.lc_raw') then $
   save,filename=path+file+'.lc_raw'


yrng = minmax(flux[where(time-time0 gt t and time-time0 lt t+dt)],/nan) * yzm
yrng = real(yrng,mean(flux,/nan)-stddev(flux,/nan))
plot,time-time0,flux,/xstyle,/ystyle,xrange=[t,t+dt],xtitle='Time',$
     position=posgen(1,1,1,xsp=-.85),ytitle='Flux',ytickn=replicate(' ',8),$
     psym=10,yrange=[ yrng ],/nodata


; now provide some simple smoothing options and write output




return
end
