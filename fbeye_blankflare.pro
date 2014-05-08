pro fbeye_blankflare,file,path
      fstartpos = -99
      fstoppos = -99
      pstartpos = -99
      pstoppos = -99
      fevent = -99
      tpeak=-99
      lpeak=-99
      ed=-99
      cplx_flg=-99
      mltpk_flg=-99
      mltpk_num=-99
      tmltpk=-99
      lmltpk=-99
      multpos=-99
      tstart=-99
      tstop=-99
      trise=-99
      tdecay-99
      save,fevent,fstartpos,fstoppos,tpeak,tstart,tstop,trise,tdecay,lpeak,ed,cplx_flg,mltpk_flg,mltpk_num,tmltpk,lmltpk,multpos,filename=PATH+'tmp/'+file+'.out'

finds={fstartpos:0d0,fstoppos:0d0,pstartpos:0d0,pstoppos:0d0}
save,filename=PATH+'tmp/'+file+'.auto',finds

return
end

