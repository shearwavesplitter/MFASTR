	program ass
	implicit none
	integer nwbeg,nwend,ierr,lu
	real wbeg_best,wend_best
	character*50 event,ext1,ext2
	integer nmin,max_no_clusters
	real dtlag_max,dfast_max,tlag_scale,fast_scale,dt_beg,dt_end
	real t_off_beg,t_off_end
	logical OPT_verbose,OPT_outfiles

	lu=20

	print*,'------------------------------------------------------'
	print*,'ENTER EVENT TO RUN ANALYSIS ON:'
	read (*,'(a)'),event
	print*,'=====> processing file: ',event
	
c  ** read initialisation paramters from ass.ini **
	call zass_ini('ass.ini',lu,ext1,ext2,nwbeg,nwend,
     >dt_beg,dt_end,dtlag_max,dfast_max,t_off_beg,t_off_end,
     >tlag_scale,fast_scale,max_no_clusters,nmin,
     >OPT_verbose,OPT_outfiles)

c  ** do the automated shear-wave splitting **    	
	call zass(event,ext1,ext2,lu,
     >nwbeg,nwend,dt_beg,dt_end,t_off_beg,t_off_end,
     >dtlag_max,dfast_max,tlag_scale,fast_scale,max_no_clusters,nmin,
     >OPT_verbose,OPT_outfiles,
     >wbeg_best,wend_best,ierr)

	end
