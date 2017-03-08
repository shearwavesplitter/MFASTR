c-----------------------------------------------------------------------
	subroutine zagglomerative_cluster(x,y,n,xc,yc,vxc,vyc,cluster)
c-----------------------------------------------------------------------
c
c	subroutine to perform agglomerative/hierarchical clustering on a
c	2D data set (x,y). Initially we have n clusters, at each step the 
c	nearest two clusters are combined to give one less cluster. This
c	is continued until there is only one cluster, composed of the entire
c	dataset.
c
c	algorithm
c	---------
c	do for no_cluster k = n - 1
c	-calc all inter-cluster distances (euclidian distance)
c	-find clusters A and B with minimum distance
c	-reassign points in cluster B to cluster A
c	-renumber clusters from 1-k
c	-calc mean and variance with each cluster
c	-calc the cluster criteria (maximise to find no. clusters)
c	repeat
c
c	variables
c    in:
c	x/y(npc)		real	x and y data to cluster
c	n			int	number of points
c    out:
c	xc/yc(npc,npc)	real	cluster centres (=mean of datapoints in cluster)
c					first index is cluster number (1-k)
c					second index is number of clusters (=k=1-n)
c	vxc/vyc(npc,npc)	real	within cluster variance
c	cluster(npc,npc)	int	assignment of datapoints. eg cluster(3,17) is the
c					number of the cluster that the 3rd datapoint 
c					is in for 17 clusters
c
c    other:
c	k			int	number of clusters
c	d(npc,npc)		real	distance between i and jth cluster
c	
c	NB. matricies with two indicies are upper triangular. first index is
c	cluster number (1-k), second index is number of clusters (=k=1-n)
c
c-----------------------------------------------------------------------
c	n.teanby	20-08-02	original code
c-----------------------------------------------------------------------
	implicit none
	integer n,npc
	include "SIZE_npc.h"
	real x(npc),y(npc)
	integer cluster(npc,npc),nc(npc)
	real xc(npc,npc),yc(npc,npc),vxc(npc,npc),vyc(npc,npc)
	real d(npc,npc),dmin
	integer i,k,imin,jmin

c  ** initially there are n clusters **
	k=n
c  ** assign the k clusters **
	do i=1,k
	   cluster(i,k)=i
	   xc(i,k)=x(i)
	   yc(i,k)=y(i)
	enddo

c  ** reduce the number of clusters from n to 1 **
c  ** by grouping the nearest neigbours **	
	do k=n-1,1,-1
c     ** calc the distance matrix between the k+1 clusters **
	   call zdiss_euclid(xc,yc,k+1,k+1,npc,d)
c	** find minimum distance **
	   call zdiss_min(d,k+1,npc,imin,jmin,dmin)
c     ** reasign the datapoints in cluster imin to cluster jmin **
	   do i=1,n
		if (cluster(i,k+1).eq.imin) then
		   cluster(i,k)=jmin
		else
		   cluster(i,k)=cluster(i,k+1)
		endif
	   enddo
c     ** renumber clusters from 1-k (i.e. remove gaps in the cluster nos) **
	   call zclust_renumber(cluster,n,npc,k)
c     ** find the average cluster positions **
	   call zcluster_loc(x,y,cluster,k,n,npc,xc,yc,vxc,vyc,nc)
	enddo
	
	return
	end

c-----------------------------------------------------------------------
	subroutine zclust_renumber(cluster,n,np,k)
c-----------------------------------------------------------------------
c
c	renumber an array of k +ve integers so that they number from 1-k
c
c	uses a collapsing algorithm where the excess differences between 
c	elements are incrementally removed
c
c	cluster(np,np)	int	cluster numbers
c	n			int	number of data
c	np			int	array dimension
c	k			int	number of distinct clusters
c	
c-----------------------------------------------------------------------
c	n.teanby	9-8-02	original code
c-----------------------------------------------------------------------
	implicit none
	integer n,np,jump,imax
	integer cluster(np,np)
	integer i,j,k

c  ** find maximum cluster number **
	imax=cluster(1,k)
	do i=1,n
	   if (cluster(i,k).gt.imax) then
	      imax=cluster(i,k)
	   endif
	enddo
		
c  ** renumber the clusters from 1 to k **
c  ** for every cluster **
	do i=1,k
	   jump=imax
c	** for all the data points **
	   do j=1,n
	      if (cluster(j,k).eq.i) then
c		** if cluster number exists move to next cluster number **
		   goto 1
		else
c		** else find the difference between cluster no. i and the nearest
c		   old cluster number **
	         if (cluster(j,k).gt.i) then
		      jump=min0(jump,cluster(j,k)-i)
		   endif
		endif
	   enddo
c	** shift all clusters numbered over i by -jump **
	   do j=1,n
	      if (cluster(j,k).gt.i) then
		   cluster(j,k)=cluster(j,k)-jump
		endif
	   enddo
1	   continue
	enddo
	return
	end

c-----------------------------------------------------------------------
	subroutine zdiss_euclid(x,y,k,n,np,d)
c-----------------------------------------------------------------------
c
c	calculate the Euclidian distances (d=sqrt(dx**2+dy**2)) between a
c	set of n points defined by the kth column of x and y
c
c	x/y(np,np)		int	x/y coordinates of points
c	k			int	column to use in calculation
c	n			int	number of data
c	np			int	array dimension
c	d(np,np)		int	distance between i and j th point
c
c	NB. d is only defined for elements i=2,n and j=1,i-1 (ie lower 
c	triangular matrix). diagonal is undefined as it reperesents the 
c	distance between the same point (=0)
c	
c-----------------------------------------------------------------------
c	n.teanby	9-8-02	original code
c-----------------------------------------------------------------------
	implicit none
	integer n,np,k
	real x(np,np),y(np,np),d(np,np)
	integer i,j
	
	do i=2,n
	   do j=1,i-1
	      d(i,j)=sqrt((x(i,k)-x(j,k))**2+(y(i,k)-y(j,k))**2)
	   enddo
	enddo
	return
	end


c-----------------------------------------------------------------------
	subroutine zdiss_min(d,n,np,imin,jmin,dmin)
c-----------------------------------------------------------------------
c
c	search the dissimalarity matrix d for the lowest value
c
c	d(np,np)		real	dissimalarity matrix
c	n			int	number of data
c	np			int	array dimension
c	imin			int	i location of minimum dissimalarity
c	jmin			int	j location of minimum dissimalarity
c	dmin			real	minimum dissimalarity =d(imin,jmin)
c
c	NB. d is only defined for elements i=2,n and j=1,i-1 (ie lower 
c	triangular matrix)
c	jmin < imin (because j < i)
c-----------------------------------------------------------------------
c	n.teanby	9-8-02	original code
c-----------------------------------------------------------------------
	implicit none
	integer n,np,imin,jmin
	real d(np,np),dmin
	integer i,j

c  ** set initial trial minimum **
	dmin=d(2,1)
	imin=2
	jmin=1
c  ** find minimum distance **
	do i=2,n
	   do j=1,i-1
	      if (d(i,j).lt.dmin) then
		   dmin=d(i,j)
		   imin=i
		   jmin=j
	   	endif
	   enddo
	enddo
	
	return
	end

c-----------------------------------------------------------------------
	subroutine zcluster_loc(x,y,cluster,k,n,np,xc,yc,vxc,vyc,nc)
c-----------------------------------------------------------------------
c
c	find the average cluster positions, variances, and number in each
c	cluster
c
c	x/y(np)		real	x/y possitions of data points
c	cluster(np,np)	int	k th column gives cluster number of i th data point
c	k			int	number of cluster (column to use in calculation)
c	n			int	number of data
c	np			int	array dimension
c	xc/yc(np,np)	real	mean location of i th cluster when there 
c					are a total ofk clusters
c	vxc/vyc(np,np)	real	variance in location of i th cluster
c	nc(np)		int	number of data in i th cluster
c
c	NB. (v)x/yc is only defined for elements i=2,n and j=1,i-1 (ie lower 
c	triangular matrix)
c-----------------------------------------------------------------------
c	n.teanby	9-8-02	original code
c-----------------------------------------------------------------------
	implicit none
	integer n,np,k
	real x(np),y(np),xc(np,np),yc(np,np),vxc(np,np),vyc(np,np)
	integer cluster(np,np),nc(np)
	real xsum,ysum
	integer i,j

c  ** calc the mean positions **
	do j=1,k
	   nc(j)=0
	   xsum=0.
	   ysum=0.
	   do i=1,n
		if (cluster(i,k).eq.j) then
	   	   xsum = xsum + x(i)
	   	   ysum = ysum + y(i)
	   	   nc(j) = nc(j) + 1
	   	endif
	   enddo
	   xc(j,k)=xsum/real(nc(j))
	   yc(j,k)=ysum/real(nc(j))
	enddo

c  ** calc the within cluster variance for each cluster **
	do j=1,k
	   nc(j)=0
	   xsum=0.
	   ysum=0.
	   do i=1,n
		if (cluster(i,k).eq.j) then
		   xsum = xsum + (x(i)-xc(j,k))**2
		   ysum = ysum + (y(i)-yc(j,k))**2
	   	   nc(j) = nc(j) + 1
	   	endif
	   enddo
	   vxc(j,k)=xsum/real(nc(j))
	   vyc(j,k)=ysum/real(nc(j))
	enddo
	return
	end


