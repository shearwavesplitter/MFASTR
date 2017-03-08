c-----------------------------------------------------------------------
	subroutine zcluster(x0,dx0,y0,dy0,n,
     >xscale,yscale,xmin0,ymin0,max_no_clusters,
     >xc0,yc0,vxc0,vyc0,cluster,k)
c-----------------------------------------------------------------------
c
c	subroutine to perform cluster analysis on a dataset comprised of
c	tlag and fast direction from the different trial windows. Cluster
c	the data and find the optimum number of clusters.
c
c    in:
c	x0/y0(npc)	 	real	tlag and fast values
c	dx0/dy0(npc)	real	standard deviation of tlag and fast measurements
c	n			int	number of data points
c	x/yscale		real	scale/standardisation factors for x0/y0 data
c	x/ymin0		real	grid spacing of x/y data
c	max_no_cluster	int	max number of clusters
c    out:
c	k			int	optimum number of clusters = max(k1,k2)
c	xc0/yc0(npc)	real	cluster means
c	vxc0/vyc0(npc)	real	within cluster variance
c	cluster(npc,npc)	int	assignment of datapoints. eg cluster(3,17) is the
c    other:
c	x/y/dx/dy(npc)	real	scaled data and standard deviation
c	xc/yc(npc,npc)	real	scaled cluster means of ith cluster when k=j
c					 [indecies are (i,j)]
c	vxc/vyc(npc,npc)	real	scaled cluster variance of ith cluster when k=j
c					number of the cluster that the 3rd datapoint 
c					is in for 17 clusters
c	c(npc)		real	clustering criteria to determine optimum 
c					number of clusters
c	k1			int	optimnum cluster no. from Calinski and Harabasz
c					1974 method
c	k2			int	optimnum cluster no. from Duda and Hart
c					1973 method
c-----------------------------------------------------------------------
c	n.teanby	15-8-02	original code
c-----------------------------------------------------------------------
	implicit none
	integer n,npc,j,k,k1,k2
	include "SIZE_npc.h"
	real x0(npc),y0(npc),dx0(npc),dy0(npc)
	real xc(npc,npc),yc(npc,npc),vxc(npc,npc),vyc(npc,npc)
	real xc0(npc),yc0(npc),vxc0(npc),vyc0(npc)
	real x(npc),y(npc),dx(npc),dy(npc)
	real xscale,yscale,xmin,ymin,xmin0,ymin0,c(npc)
	integer cluster(npc,npc)
	integer max_no_clusters

c  ** scale the data **
	call zmultiply(x0,n,npc,1./xscale,x)
	call zmultiply(y0,n,npc,1./yscale,y)
	call zmultiply(dx0,n,npc,1./xscale,dx)
	call zmultiply(dy0,n,npc,1./yscale,dy)
	xmin=xmin0/xscale
	ymin=ymin0/yscale

c  ** calc the cluster hierachy of the scaled data **
	call zagglomerative_cluster(x,y,n,xc,yc,vxc,vyc,cluster,c)

c  ** calc clustering criteria **
c  ** calc k using Calinski and Harabasz (1974) method **
	call zc_74calhar(x,y,n,xc,yc,xmin,ymin,
     >cluster,max_no_clusters,c,k1)
ccc	print*,'calhar'
ccc	do j=1,n
ccc	  print*,j,c(j)
ccc	enddo

c  ** calc k using Duda and Hart (1973) method **
	call zc_73dudhar(x,y,n,xc,yc,xmin,ymin,
     >cluster,max_no_clusters,c,k2)
ccc	print*,'dudhar'
ccc	do j=1,n
ccc	  print*,j,c(j)
ccc	enddo
	
c  ** set number of clusters to max of k1 and k2 **
	k=max(k1,k2)

c  ** unscale the cluster possitions and variances **
	do j=1,k
	   xc0(j) = xscale * xc(j,k)
	   yc0(j) = yscale * yc(j,k)
	   vxc0(j) = xscale**2 * vxc(j,k)
	   vyc0(j) = yscale**2 * vyc(j,k)
	enddo
	
	return
	end
