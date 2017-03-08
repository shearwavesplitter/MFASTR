c-----------------------------------------------------------------------
	subroutine zpackresults(wbeg,wend,tlag,dtlag,fast,dfast,n,
     >npc,dtlag_max,dfast_max)
c-----------------------------------------------------------------------
c	
c	remove results which have errors over dtlag_max and dfast_max and
c	compress the arrays. results which only fail one of the criteria
c	are still accepted.
c
c    in/out:(modified on return)
c	wbeg(npc)	real		beginning of ith window
c	wend(npc)	real		end of ith window
c	fast(npc)	real		fast direction for ith window
c	dfast(npc)	real		s.d. of fast direction for ith window
c	tlag(npc)	real		tlag direction for ith window
c	dtlag(npc)	real		s.d. of tlag direction for ith window
c	n		int		number of data
c    in:
c	npc		int		array dimension
c	dtlag_max	real		max allowable error in lag time
c	dfast_max	real		max allowable error in fast direction
c
c-----------------------------------------------------------------------
c	n.teanby	20-8-02	original code
c-----------------------------------------------------------------------
	
	implicit none
	integer n,npc
	real wbeg(npc),wend(npc),fast(npc),dfast(npc),tlag(npc)
	real dtlag(npc),dtlag_max,dfast_max
	integer i,j
	
c  ** remove bad results and compress arrays **
	j=0
	do i=1,n
	   if ((dtlag(i).le.dtlag_max).or.(dfast(i).le.dfast_max)) then
	      j=j+1
		wbeg(j)  = wbeg(i)
		wend(j)  = wend(i)
		tlag(j)  = tlag(i)
		dtlag(j) = dtlag(i)
		fast(j)  = fast(i)
		dfast(j) = dfast(i)
	   endif
	enddo
	n=j
	
c  ** clean up end of array **
	do i=j+1,npc
	   wbeg(i)  = 0.
	   wend(i)  = 0.
	   tlag(i)  = 0.
	   dtlag(i) = 0.
	   fast(i)  = 0.
	   dfast(i) = 0.
	enddo
	
	return
	end
