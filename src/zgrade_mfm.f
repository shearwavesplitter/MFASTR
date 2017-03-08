c-----------------------
       subroutine zgrade_mfm(dt,phi,k,kbest,nmin,nc,var_overall,tdifmax,
     1   phidifmax, grade)
c----------------------
c
c  subroutine to grade the clusters based on whether the other clusters have
c    similar good fits but are giving different values--basically, is it possibly
c    a cycle-skipped event or a null?
c
c   MKS 30 May 2008
c
c   MKS 5 March 2010 fixed a bug that was allowing C events to be turned
c    into B events.
c  MKS 28 March 2013 fixing a problem that phidif was not properly accounting
c    for the fact that there is a modulo 180 degrees in it.
c
c  	in:
c	  dt/phi	real	tlag and fast meas.
c         k		int	number of clusters
c	  kbest	int	best cluster from zselect_cluster
c	  nmin	int	Minimum number in cluster to accept in zselect_cluster
c	  nc	int	number of data points in ith cluster
c	  var_overall	real	overall variance of ith cluster
c         tdifmax	real	maximum time difference to allow 
c                               probably best to set to a fraction of the max
c				time allowed to be measured
c	  phidifmax	real	maximum phi difference to allow
c
c	out:
c	  grade	char*5	Grade of measurement
c
	implicit none
        character*5 grade
	integer j,npc,kbest,k,nmin
	include "SIZE_npc.h"
	real tdifmax,phidifmax,tdif,phidif,varcheck
	real dt(npc),phi(npc),var_overall(npc)
	integer nc(npc),nccheck

c    look at all clusters with more than half as many points as the first cluster and variance up to
c    five times as high as the best cluster to see if it could be cycle-skipped or a null
c  All results come out as -90 to +90
        grade="ACl"
        varcheck=5*var_overall(kbest)
        nccheck=nc(kbest)/2
        print*, 'varcheck, nccheck are ',varcheck,nccheck
        print*, ' kbest,npc are  ', kbest,npc
        do j=1,k
          if (j .eq. kbest) goto 10
 	  tdif=abs(dt(kbest)-dt(j))
          phidif = abs(phi(kbest) - phi(j))
c  mks 28 march 2013 adding revised phidif in lines below
          if ((180-phidif) .lt. phidifmax) then
              phidif = 180-phidif
          endif
          if (nc(j) .ge. nccheck) then 
           if (var_overall(j) .lt.  varcheck) then
c             print *,"j,kbest,dt,phi",j,kbest,dt(kbest),phi(kbest)
c	     print *, "tdif, phidif,phibest,phi",tdif,phidif,phi(kbest),phi(j)
c	     phidif=mod(phidif,180)
c
c mks 28 march 2013 modifying below to include best method above
c             if ((tdif .gt. tdifmax) .or. ((phidif .gt. phidifmax) .and. 
c     1            (phidif .lt. (180-phidifmax)))) then 
             if ((tdif .gt. tdifmax) .or. (phidif .gt. phidifmax)) then  
		grade="DCl"
                print *,"grade,",grade,tdif,phidif,phi(kbest),phi(j)
 		go to 20
             else
		if((tdif .gt. tdifmax/2) .or. (phidif .gt. phidifmax/2)) then
		grade="CCl"
                print *,"grade,",grade,tdif,phidif,phi(kbest),phi(j)
	        endif
             endif
           endif
         else
c
c  Check a higher threshold for an A event--don't let any events over the minimum  number come in
         if (nc(j) .ge. nmin) then
           if (var_overall(j) .lt.  varcheck) then
c             print *,"j,kbest,dt,phi",j,kbest,dt(kbest),phi(kbest)
c	     print *, "tdiff, phidif, phibest phi",tdif,phidif,phi(kbest),phi(j)
c  mks 28 marchc 2013 commenting line below
c	     phidif=mod(phidif,180.0)
        	if((tdif .gt. tdifmax/2) .or. (phidif .gt. phidifmax/2)) then
		  if (grade .ne. "CCl") grade="BCl"
                  print *,"grade",grade,tdif,phidif,phi(kbest),phi(j)
	        endif
             endif
           endif
         endif
  10    continue
 	enddo
  20 	continue
	return
	end
