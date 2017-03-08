c-----------------------------------------------------------------------
	subroutine zc_74calhar(x,y,n,xc,yc,xmin,ymin,
     >cluster,max_no_clusters,c,kopt)
c-----------------------------------------------------------------------
c
c	calc the clustering criteria of Calinski and Harabasz 1974
c
c	       trace(B)/(k-1)
c	c(k) = --------------
c	       trace(W)/(n-k)
c
c	W = within-cluster covariance matrix
c	B = between-cluster covariance matrix
c	k = number of clusters
c	n = number of datapoints
c
c	maximise c(k) to find the optimum number of clusters
c
c	this rule is for continuous data and we have discrete data so
c	the minimum errors are set to xmin/ymin, corresponding to the grid
c	spacing. this also avoids division by zero issues
c	
c	variables
c	---------
c    in:
c	x/y(npc)		real	data that has been clustered
c	n			real	number of data points
c	xc/yc(npc,npc)	real	cluster centres (=mean of datapoints in cluster)
c					first index is cluster number (1-k)
c					second index is number of clusters (=k=1-n)
c	x/ymin		real	grid spacing of x/y data
c	cluster(npc,npc)	int	assignment of datapoints. eg cluster(3,17) is the
c					number of the cluster that the 3rd datapoint 
c					is in for 17 clusters
c	max_no_cluster	int	max number of clusters
c    out:
c	c(npc)		real	clustering criteria to determine optimum 
c					number of clusters
c	kopt			int	optimum number of clusters
c
c-----------------------------------------------------------------------
c	n.teanby	28-08-02	original code
c-----------------------------------------------------------------------
	implicit none
	integer n,npc,kopt,max_no_clusters
	include "SIZE_npc.h"
	real x(npc),y(npc),xc(npc,npc),yc(npc,npc),c(npc)
	integer cluster(npc,npc)
	real xbar,ybar,traceb,tracew,c_max,xmin,ymin
	integer i,j,k,nc

c  ** calc x/ybar **
	xbar=0.
	ybar=0.	
	do i=1,n
	   xbar=xbar+x(i)
	   ybar=ybar+y(i)
	enddo
	xbar=xbar/real(n)
	ybar=ybar/real(n)

c  ** calc c for each number of clusters **
	c(1)=0.
	do k=2,n
	   tracew=0.
	   traceb=0.
	   do j=1,k
		nc=0
		do i=1,n
		   if (cluster(i,k).eq.j) then
			tracew=tracew+(max(xmin,x(i)-xc(j,k)))**2 
     >				+ (max(ymin,y(i)-yc(j,k)))**2
			nc=nc+1
		   endif
		enddo
		traceb=traceb+nc*((xc(j,k)-xbar)**2+(yc(j,k)-ybar)**2)
	   enddo
	   c(k)=traceb*real(n-k)/(real(k-1)*tracew)
c	   print*,'74calhar',k,traceb,tracew,c(k)
	enddo
	
c  ** find optimum number of clusters **
	c_max=c(1)
	kopt=1
	do k=2,max_no_clusters
	   if (c(k).gt.c_max) then
	      c_max=c(k)
		kopt = k
	   endif
	enddo
	
	return
	end
