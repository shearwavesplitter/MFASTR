c-----------------------------------------------------------------------
	subroutine zselect_cluster_mfm(dx0,dy0,vxc0,vyc0,n,
     >xscale,yscale,cluster,nmin,k,
     >kbest,nc,var_overall)
c-----------------------------------------------------------------------
c
c	subroutine to find the best cluster.
c
c  MKS 22 May 2008 modifying to allow information about each cluster to
c    be passed to the code to write into a new file for later analysis
c    if people want to work with the cluster statistics
c  Also trying to give a better estimate of the cluster from this
c
c  MKS 15 June 2011 modifying to allow very small variance points to be counted.
c
c  also fixing a problem with calculating variance and testing whether
c    to allow the number of measurements in a cluster to count more.
c
c	best cluster has:	nmin or greater points; and the lowest overall variance
c
c    in:
c	dx0/dy0(npc)	real	standard deviation of tlag and fast measurements
c	vxc0/vyc0(npc)	real	within cluster variance
c	n			int	number of data points
c	x/yscale		real	scale/standardisation factors for x0/y0 data
c	cluster(npc,npc)	int	assignment of datapoints. eg cluster(3,17) is the
c					number of the cluster that the 3rd datapoint 
c					is in for 17 clusters
c	nmin			int	min no. data points for an acceptable cluster
c	k			int	number of clusters
c    out:
c	kbest			int	best cluster
c	nc(npc)		int	number of data points in ith cluster
c	var_overall(npc)	real	overall variance = max(var_cluster,var_data)
c    other:
c	dx/dy(npc)		real	scaled data standard deviation
c	vxc/vyc(npc,npc)	real	scaled cluster variance of ith cluster when k=j
c	var_data(npc)	real	average variance of data within ith cluster
c	var_cluster(npc)	real	within cluster variance (=vxc+vyc)
c-----------------------------------------------------------------------
c	n.teanby	15-8-02	original code
c-----------------------------------------------------------------------
	implicit none
	integer n,npc,j,k,kbest
	include "SIZE_npc.h"
	real dx0(npc),dy0(npc),vxc0(npc),vyc0(npc)
	real dx(npc),dy(npc),vxc(npc,npc),vyc(npc,npc)
	real xscale,yscale
	integer nc(npc),cluster(npc,npc)
	real var_overall(npc),var_data(npc),var_cluster(npc),var_min
	integer nmin
	logical first_pass
        character grade(5)

	kbest=0
	
c  ** scale the errors on the data **
	call zmultiply(dx0,n,npc,1./xscale,dx)
	call zmultiply(dy0,n,npc,1./yscale,dy)

c  ** calc composite variance of data in clusters **
	call zcluster_variance(dx,dy,n,npc,cluster,k,var_data)

c  ** calc variance of points in cluster about the cluster mean **
	do j=1,k
c  mks 15 jun 2011 dividing by two to get mean over both variables
           print *," cluster variances are ",j,vxc(j,k),vyc(j,k)
c	   var_cluster(j) = (vxc(j,k)**2 + vyc(j,k)**2)/2
	   var_cluster(j) = (vxc(j,k)**2 + vyc(j,k)**2)
	enddo

c  ** calc number of data, nc, in each of the k clusters **
	call zcluster_number(cluster,n,npc,k,nc)

c  ** set variance to be maximum of var_data, and var_cluster **
	do j=1,k
           print *,j,var_cluster(j),var_data(j)
	   var_overall(j) = max(var_cluster(j),var_data(j))
	   print*,'cluster no.=',j,' error = ',var_overall(j),' n=',nc(j)
	enddo

c  ** find best cluster (lowest error and n>=nmin) **
c  Trying to count more numerous clusters more
	first_pass=.true.
	do j=1,k
	   if(nc(j).ge.nmin) then
		print*,'cluster ',j,' passes n>',nmin
		if (first_pass) then
	         first_pass = .false.
		   var_min    = var_overall(j)
		   kbest      = j
	      else if(var_overall(j).lt.var_min) then
		   var_min    = var_overall(j)
		   kbest   = j
	      endif
	   endif
	enddo
c   moving code to a new program
cc  ** mks 30 May 2008 adding a grading section to see now much better it is than other clusters 
cc    look at all clusters with more than half as many points as the first cluster and variance up to
cc    twice as high as the best cluster to see if it could be cycle-skipped or a null 
c        grade="ACl"
c        do j=1,k
c          if ((nc(j) .ge. nc(kbest)/2) .and. (var_overall(j) .lt. 2*var_overall(kbest) then
c             if (x0 

	return
	end

c-----------------------------------------------------------------------
	subroutine zcluster_variance(dx,dy,n,npc,cluster,k,var_data)
c-----------------------------------------------------------------------
c
c	variance =        1
c	           --------------
c	           SUM( 1/var_i )
c
c	using this definition of the variance (instead of the average
c	variance) means that the overall variance is dominated by the 
c	smaller individual variances.
c
c   changing this now when the variances are very small to use the real average
c    variance.
c
c  mks 15 June 2011  Also, it seems that he had forgotten to divide by
c    the number of points to calculate the variance--maybe falsely making
c    small clusters look better.
c
c-----------------------------------------------------------------------
c	n.teanby	15-8-02	original code
c-----------------------------------------------------------------------
	implicit none
	integer n,npc
	real dx(npc),dy(npc),var_data(npc),TINY
	parameter (TINY=1.e-20)
	integer k,cluster(npc,npc)
	real xsum,ysum
	integer nc,i,j

c  ** find variance of data in jth cluster **
	do j=1,k
	   xsum=0.
	   ysum=0.
	   nc=0
	   do i=1,n
	      if (cluster(i,k).eq.j) then
c		** don't include zero variance points **
		   if ((dx(i).gt.TINY).and.(dy(i).gt.TINY)) then
		      xsum = xsum + 1./dx(i)**2
		      ysum = ysum + 1./dy(i)**2
		      nc   = nc+1
c  MKS adding the else statement 15 June 2011
                   else 
                     print *," Note!  have zero variance points at j ",j
                     goto 20
		   endif
		endif
	   enddo
c	   var_data(j) = 1./xsum + 1./ysum
c  mks 15 June 2011 changing to divide by the number of meas. in the cluster
	   var_data(j) = (1./xsum + 1./ysum)/(2*nc)

	enddo
        goto 30
 20     continue
        print *, " recalculating averages"
c  ** find variance of data in jth cluster **
c  Do straight variance if have very low variance points
	do j=1,k
	   xsum=0.
	   ysum=0.
	   nc=0
           print *, "j is ",j
	   do i=1,n
	      if (cluster(i,k).eq.j) then
		      xsum = xsum + dx(i)**2
		      ysum = ysum + dy(i)**2
		      nc   = nc+1
		endif
	   enddo
	   var_data(j) = (xsum + ysum)/(nc*2)
           print *, "var_data", j, xsum,ysum,var_data(j)
	enddo
 30     continue
        return
	end

c-----------------------------------------------------------------------
	subroutine zcluster_number(cluster,n,npc,k,nc)
c-----------------------------------------------------------------------
c
c	count the number of data points in each cluster
c
c-----------------------------------------------------------------------
c	n.teanby	15-8-02	original code
c-----------------------------------------------------------------------
	implicit none
	integer n,npc,k
	integer cluster(npc,npc),nc(npc)
	integer i,j

c  ** calc number of data in jth cluster **
	do j=1,k
	   nc(j)=0
	   do i=1,n
	      if (cluster(i,k).eq.j) then
		   nc(j) = nc(j) + 1
		endif
	   enddo
	enddo
	return
	end

