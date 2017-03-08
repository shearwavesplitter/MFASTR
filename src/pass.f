c-----------------------------------------------------------------------
	program pass
c-----------------------------------------------------------------------
c
c	parallel version of ass.f
c
c-----------------------------------------------------------------------
c	n.teanby	12-5-03	original code (modified from old pass c. 8/02)
c-----------------------------------------------------------------------
	implicit none
	include "mpif.h"
	integer nevents,npevents,irank,irank_req,nprocs,mpierr,lu
	parameter (npevents=500)
	character*50 event(npevents)

	integer nwbeg,nwend,ierr
	real wbeg_best,wend_best
	character*50 ext1,ext2
	integer nmin,max_no_clusters,bstep,estep
	logical OPT_verbose,OPT_outfiles
	real dtlag_max,dfast_max,tlag_scale,fast_scale,dt_beg,dt_end	
	real t_off_beg,t_off_end
	integer i,ievent
	integer tag_ievent,tag_req,status

	nevents=0
	tag_ievent=1
	tag_req   =2

c  ** find processor number **
	call MPI_Init(mpierr)
	call MPI_Comm_rank(MPI_COMM_WORLD,irank,mpierr)
	lu = 20 + irank
	call MPI_Comm_size(MPI_COMM_WORLD,nprocs,mpierr)
	print*,'processor: ',irank,' active out of (',nprocs,')'

c  ** read initialisation paramters from ass.ini **
	call zass_ini('ass.ini',lu,ext1,ext2,nwbeg,nwend,
     >dt_beg,dt_end,dtlag_max,dfast_max,t_off_beg,t_off_end,
     >tlag_scale,fast_scale,max_no_clusters,nmin,
     >OPT_verbose,OPT_outfiles)

c  ** each processor reads in the event list **
	open(lu,file='event.list',status='old')
	do i=1,npevents
	   read(lu,*,end=1) event(i)
	   nevents = nevents + 1
	enddo
1	continue
	close(lu)

c  ** process the events **
	if (irank.ne.0) then
	   do i=1,nevents+1
c	   ** send a requenst for an event number to proc0 **
		call MPI_SSEND(irank,1,MPI_INTEGER,0,
     >		tag_req,MPI_COMM_WORLD,mpierr)
c	   ** receive an event number from proc0 **
		call MPI_RECV(ievent,1,MPI_INTEGER,0,
     >		tag_ievent,MPI_COMM_WORLD,status,mpierr)
		if (ievent.eq.0) then
		   goto 999
		else
c	      ** process event **
	      call zass(event(ievent),ext1,ext2,lu,nwbeg,nwend,
     >	   dt_beg,dt_end,t_off_beg,t_off_end,
     >	   dtlag_max,dfast_max,tlag_scale,fast_scale,
     >	   max_no_clusters,nmin,
     >	   OPT_verbose,OPT_outfiles,
     >	   wbeg_best,wend_best,ierr)
	      endif
	   enddo
	else
	   do i=1,nevents+nprocs-1
c	   ** set ievent to 0 if no more events left **
		if (i.gt.nevents+nprocs-1) then
		   goto 999 !for some reason the loop doesnt stop itself!!
		else if (i.gt.nevents) then
		   ievent=0
		else
		   ievent=i
		endif
c	   ** receive a request for an event number **
		call MPI_RECV(irank_req,1,MPI_INTEGER,MPI_ANY_SOURCE,
     >		tag_req,MPI_COMM_WORLD,status,mpierr)
c	   ** proc0 sends an event number to processor irank_req **
		call MPI_SSEND(ievent,1,MPI_INTEGER,irank_req,
     >		tag_ievent,MPI_COMM_WORLD,mpierr)
	   enddo
	endif
999	continue
	print*,'processor: ',irank,' FINISHED'
	
	call MPI_Finalize(mpierr)
	end
