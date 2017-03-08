c-----------------------------------------------------------------------
	subroutine zc_73dudhar(x,y,n,xc,yc,xmin,ymin,
     >cluster,max_no_clusters,c,kopt)
c-----------------------------------------------------------------------
c
c	calc the clustering criteria of Duda and Hart 1973
c
c	consider 2 groups which are combined to form a single group in the
c	next iteration.
c
c	je2 is the sum of square errors within the two clusters
c	je1 is the sum of square errors within the combined cluster
c
c	this rule is for continuous data and we have discrete data so
c	the minimum errors are set to xmin/ymin, corresponding to the grid
c	spacing. this also avoids division by zero issues
c
c	calc je2/je1
c	if je2/je1 > c_critical then stop the clustering here
c
c	Milligan and Cooper 1985 found c_critical = 3.20 gave good results
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
	real je1,je2,c_critical,xmin,ymin
	integer i,j,k,cluster1,cluster2,cluster12,nc
	logical first_pass

	kopt=0
	c(n)=0.
	c_critical=3.20
	
c  ** for all numbers of clusters **
	do k=1,n-1
c	** for each cluster **
	   do j=1,k
c	   ** for each data point **
c	   ** search each data point to find a single cluster which is two seperate clusters when the number of clusters is increased by one **
		first_pass=.true.
		do i=1,n
		   if (cluster(i,k).eq.j) then
		      if (first_pass) then
		         first_pass=.false.
			   cluster1 = cluster(i,k+1)
			   cluster12= j
		      else if (cluster(i,k+1).ne.cluster1) then
		         cluster2=cluster(i,k+1)
			   goto 99
		      endif		      
		   endif
		enddo
	   enddo
99	   continue
c  ** calc Je1 **
c  ** cluster12 = number of the combined cluster when there are k clusters **
	   je1=0.
	   nc=0
	   do i=1,n
	      if (cluster(i,k).eq.cluster12) then
	   	   je1 = je1 + max(xmin,x(i)-xc(cluster12,k))**2 + 
     >			   max(ymin,y(i)-yc(cluster12,k))**2
		   nc=nc+1
		endif
	   enddo
c  ** calc Je2 **
c  ** cluster1 and cluster2 = number of the seperate clusters when there
c	are k+1 clusters **
	   je2=0.
	   do i=1,n
	      if (cluster(i,k+1).eq.cluster1) then
	   	   je2 = je2 + max(xmin,x(i)-xc(cluster1,k+1))**2 + 
     >			   max(ymin,y(i)-yc(cluster1,k+1))**2
		endif
	      if (cluster(i,k+1).eq.cluster2) then
	   	   je2 = je2 + max(xmin,x(i)-xc(cluster2,k+1))**2 + 
     >			   max(ymin,y(i)-yc(cluster2,k+1))**2
		endif
	   enddo
c	** this is the special case for 2 parameters **
	   c(k)=(0.681690113 - je2/je1)*sqrt(real(nc)/0.18943053)
	enddo
	
c  ** search for the optimum number of clusters **
c  ** if c(k) exceeds c_critical then the cluster should be subdivided,
c	giving kopt = k + 1 as the optimum number of clusters **
	do k=1,max_no_clusters
	   if (c(k).gt.c_critical) then
	      kopt = max(kopt,k+1)
	   endif
	enddo
			      
	return
	end
