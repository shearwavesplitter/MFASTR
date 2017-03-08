	program test
	implicit none
	integer n,np,m,i,j
	parameter (np=50000)
	real x0(np),y0(np),xc(np),yc(np),vxc(np),vyc(np),xscale,yscale
	real bic(np),bicmin
	integer mmin,mmax
	
	n=0
	xscale=40.
	yscale=180.
	mmax=250
	
	open(20,file='test.clust',status='old')
	do 1 i=1,np
	   read(20,*,end=11) x0(i),y0(i)
	   n=n+1
1	continue
11	continue
	close(20)

	do 2 m=2,mmax
	   call zkmeans_cluster(x0,y0,n,np,xscale,yscale,m,
     >xc,yc,vxc,vyc,bic(m))
	   print*,'m=',m,' bic= ',bic(m)
2	continue

	mmin=2
	bicmin=bic(2)
	do 20 j=3,mmax
	   if (bic(j).lt.bicmin) then
	     bicmin=bic(j)
	     mmin=j
	   endif
20	continue
	print*,'bicmin=',bicmin,' m=',mmin

	call zkmeans_cluster(x0,y0,n,np,xscale,yscale,mmin,
     >xc,yc,vxc,vyc,bic(mmin))
	open(20,file='test.clustxy')
	do 3 i=1,mmin
	      write(20,*) xc(i),yc(i),sqrt(vxc(i)),sqrt(vyc(i))
3	continue
	close(20)
	end

c-----------------------------------------------------------------------
	subroutine zkmeans_cluster(x0,y0,n,np,xscale,yscale,m,
     >xc,yc,vxc,vyc,bic)
c-----------------------------------------------------------------------
c
c	subroutine to find m clusters in a set of n pixels defined by x0 and y0. The x0 and y0 data are scaled by x/yscale. This is necessary because x0 and y0 may have very different ranges. For example if x is 0-0.0001 and y is 0-100 then the clustering will be dominated by the y axis. The algorithm is:
c	-set m trail cluster locations, equally spaced along the data 
c	range diagonal
c	-asign each data point to it's nearest cluster
c	-calc the location of the cluster from the mean position of the 
c	data points in that cluster
c	-re-asign the datapoints to the updated cluster locations
c	-continue until the asignment of points remains unchanged number
c	 of itereations exceeds MAXITER 
c    in:
c	x0(np)	real		x data
c	y0(np)	real		y data
c	n		int		number of pixels
c	np		int		array dimension
c	xscale	real		scale x data by this factor
c	yscale	real		scale y data by this factor
c	m		int		number of clusters
c    out:
c	xc/yc(np)	real		cluster positions (means)
c	vxc/vyc(np)	real		variance in x and y directions
c
c	Algorithm from:
c	Richards and Jia. Remote sensing digital image analysis, 1999, 
c	springer (99RJ)
c
c-----------------------------------------------------------------------
c	N. Teanby	8-8-02	Original code
c-----------------------------------------------------------------------
	implicit none
	integer n,np,m,np_local,iter,maxiter
	real TINY
	parameter (np_local=50000,maxiter=100,TINY=1.e-100)
	real x0(np),y0(np),xc(np),yc(np),vxc(np),vyc(np),xscale,yscale,bic
	real x(np_local),y(np_local)
	real xmin,xmax,ymin,ymax,distmin,xsum,ysum,dist,term1,term2
	integer clusters(np_local),clustersOLD(np_local)
	integer i,j,nclust(np_local),nc
	logical clusters_stable

c  ** check np_local is big enough **
	if (n.gt.np_local) pause 'ERROR: zmigrating-means: n.gt.np_local'

c  ** scale the x and y values **
	do 11 i=1,n
	   x(i)=x0(i)/xscale
	   y(i)=y0(i)/yscale
11	continue

c  ** set initial cluster locations **
c  ** equally space along the diagonal from minimum x and y to the
c	maximum x and y (99RJ p227) **
c  ** find min and max of x and y **
	xmin=x(1)
	xmax=x(1)
	ymin=y(1)
	ymax=y(1)
	do 1 i=1,n
	   xmin=min(xmin,x(i))
	   xmax=max(xmax,x(i))
	   ymin=min(ymin,y(i))
	   ymax=max(ymax,y(i))
1	continue
c  ** set the initial cluster positions **
	do 2 j=1,m
	   xc(j) = xmin + real(j-1)*(xmax-xmin)/real(m-1)
	   yc(j) = ymin + real(j-1)*(ymax-ymin)/real(m-1)
2	continue

	do 9 iter=1,maxiter
c	** calculate the distance between the ith datapoint and the jth cluster **
c	** assign each datapoint to the cluster which is closest **
	   do 10 i=1,n
	      distmin=sqrt((xmax-x(i))**2+(ymax-y(i))**2)
		do 20 j=1,m 
		   dist=sqrt((x(i)-xc(j))**2+(y(i)-yc(j))**2)
		   if (dist.lt.distmin) then
		      distmin=dist
		      clusters(i)=j
		   endif
20	      continue
10	   continue
c	** if allocations are unchanged then exit the loop **
	   clusters_stable=.true.
	   do 25 i=1,n
	     if (iter.gt.1) then
	        if(clusters(i).ne.clustersOLD(i)) then
	           clusters_stable=.false.
	        endif
	     else
	        clusters_stable=.false.
	     endif
25	   continue
	   if (clusters_stable) then
		goto 99
	   endif
c	** recalculate the cluster centres **
	   do 30 j=1,m
	      nclust(j)=0
	      xsum=0.
	      ysum=0.
	      do 40 i=1,n
	         if (clusters(i).eq.j) then
		      xsum = xsum + x(i)
		      ysum = ysum + y(i)
		      nclust(j) = nclust(j) + 1
		   endif
40	      continue
	      if (nclust(j).gt.0) then
		   xc(j)=xsum/real(nclust(j))
	         yc(j)=ysum/real(nclust(j))
		else
		   xc(j)=0.
		   yc(j)=0.
		endif
30	   continue
c	** copy cluster allocations **
	   do 50 i=1,n
	      clustersOLD(i)=clusters(i)
50	   continue
9	continue
	print*,'ERROR: zmigrating-means:'
	pause 'maximum nuber of itererations reached, clusters not stable'
99	continue
	
c  ** remove the effect of scaling on the cluster centres **
	do 100 j=1,m
	   xc(j)=xc(j)*xscale
	   yc(j)=yc(j)*yscale
100	continue

c  ** calculate the variance of each cluster in the x and y directions **
	do 150 j=1,m
	   nclust(j)=0
	   xsum=0.
	   ysum=0.
	   do 300 i=1,n
	   	if (clusters(i).eq.j) then
	   	   xsum = xsum+ (x0(i)-xc(j))**2
	   	   ysum = ysum+ (y0(i)-yc(j))**2
		   nclust(j)=nclust(j)+1
	   	endif
300	   continue
	   if (nclust(j).ne.0) then
	      vxc(j)=xsum/real(nclust(j))
	      vyc(j)=ysum/real(nclust(j))
	   else
	      vxc(j)=0.
	      vyc(j)=0.
	   endif
150	continue

c  ** calc Baysian Information Criterion **
	nc=0
	do 500 j=1,m	
	   if (nclust(j).ne.0) then
		nc=nc+1
	   endif
500	continue
	bic=0.
	do 400 j=1,m	
	   if (nclust(j).ne.0) then
		term1=(vxc(j)/xscale + vyc(j)/yscale)/real(nclust(j))
		term2=nc*log(real(nclust(j)))/real(nclust(j))
		if (term1.gt.TINY) then
		   bic = bic + log(term1) + term2
		endif
	   endif
400	continue

	return
	end
