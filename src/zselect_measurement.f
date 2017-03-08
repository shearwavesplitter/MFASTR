c-----------------------------------------------------------------------
	subroutine zselect_measurement(dx0,dy0,n,
     >wbeg,wend,delta,spick,xscale,yscale,cluster,k,kbest,
     >ibest)
c-----------------------------------------------------------------------
c
c	subroutine to find the best measurement from
c	within the best cluster.
c
c    in:
c	dx0/dy0(npc)	real	standard deviation of tlag and fast measurements
c	n			int	number of data points
c	delta		real		sampling interval (s) read from sac header
c	spick		real		s-wave pick read from sac header
c	x/yscale		real	scale/standardisation factors for x0/y0 data
c	cluster(npc,npc)	int	assignment of datapoints. eg cluster(3,17) is the
c					number of the cluster that the 3rd datapoint 
c					is in for 17 clusters
c	k			int	optimum number of clusters = max(k1,k2)
c	kbest			int	best cluster
c    out:
c	ibest			int	index of best measurement within best cluster
c    other:
c	dx/dy(npc)		real	scaled data standard deviation
c-----------------------------------------------------------------------
c	n.teanby	15-8-02	original code
c-----------------------------------------------------------------------
	implicit none
	integer n,npc,k,kbest,ibest
	include "SIZE_npc.h"
	real dx0(npc),dy0(npc),dx(npc),dy(npc)
	real xscale,yscale,wbeg(npc),wend(npc),delta,spick
	integer cluster(npc,npc)
	logical first_pass
	integer i
	real err,err_min
	
	ibest=0
	
	if (kbest.ne.0) then
c  	** scale the data **
	   call zmultiply(dx0,n,npc,1./xscale,dx)
	   call zmultiply(dy0,n,npc,1./yscale,dy)
c  	** find best splitting measurement from the best cluster **
	   first_pass=.true.
	   do i=1,n
	      if (cluster(i,k).eq.kbest) then
		   err=sqrt(dy(i)**2 + dx(i)**2)/((wend(i)-spick)/delta)
c		   err=sqrt(dy(i)**2 + dx(i)**2)
		   if (first_pass) then
		      first_pass = .false.
		      ibest      = i
		      err_min    = err
		   else if (err.le.err_min) then
		      ibest   = i
		      err_min = err
		   endif
	      endif
	   enddo
	endif
	
	return
	end
