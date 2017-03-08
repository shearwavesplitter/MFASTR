c-----------------------------------------------------------------------
      SUBROUTINE zmedfit(x,y,np,ndata,a,b,abdev)
c-----------------------------------------------------------------------
c
c	robust (L1 norm) fit to x-y data
c	
c-----------------------------------------------------------------------
c  modifications:
c	1986-92 Numerical Recipes Software	Original code (medfit.f)
c	11-07-01	N. Teanby			real -> double precision
c							np allocatable by calling program
c-----------------------------------------------------------------------

      INTEGER ndata,NMAX,ndatat,np
      PARAMETER (NMAX=1000000)
      double precision a,abdev,b,x(np),y(np),arr(NMAX),xt(NMAX),
     *yt(NMAX),aa,abdevt
      COMMON /arrays/ xt,yt,arr,aa,abdevt,ndatat
CU    USES rofunc
      INTEGER j
      double precision b1,b2,bb,chisq,del,f,f1,f2,sigb,sx,sxx,sxy,sy
	double precision rofunc
      if (NMAX.lt.np) then
	   print*,'WARNING:zmedfit'
	   print*,'WARNING:  NMAX.lt.np, fitting not done properly'
	   stop
	endif
	sx=0.
      sy=0.
      sxy=0.
      sxx=0.
      do 11 j=1,ndata
        xt(j)=x(j)
        yt(j)=y(j)
        sx=sx+x(j)
        sy=sy+y(j)
        sxy=sxy+x(j)*y(j)
        sxx=sxx+x(j)**2
11    continue
      ndatat=ndata
      del=ndata*sxx-sx**2
      aa=(sxx*sy-sx*sxy)/del
      bb=(ndata*sxy-sx*sy)/del
      chisq=0.
      do 12 j=1,ndata
        chisq=chisq+(y(j)-(aa+bb*x(j)))**2
12    continue
      sigb=sqrt(chisq/del)
      b1=bb
      f1=rofunc(b1)
      b2=bb+sign(3.*sigb,f1)
      f2=rofunc(b2)
1     if(f1*f2.gt.0.)then
        bb=2.*b2-b1
        b1=b2
        f1=f2
        b2=bb
        f2=rofunc(b2)
        goto 1
      endif
      sigb=0.01*sigb
2     if(abs(b2-b1).gt.sigb)then
        bb=0.5*(b1+b2)
        if(bb.eq.b1.or.bb.eq.b2)goto 3
        f=rofunc(bb)
        if(f*f1.ge.0.)then
          f1=f
          b1=bb
        else
          f2=f
          b2=bb
        endif
        goto 2
      endif
3     a=aa
      b=bb
      abdev=abdevt/ndata
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software *%&&,1{.


      FUNCTION rofunc(b)
      INTEGER NMAX
      double precision rofunc,b,EPS
      PARAMETER (NMAX=1000000,EPS=1.e-7)
CU    USES select
      INTEGER j,ndata
      double precision aa,abdev,d,sum,arr(NMAX),x(NMAX),y(NMAX),select
      COMMON /arrays/ x,y,arr,aa,abdev,ndata
      do 11 j=1,ndata
        arr(j)=y(j)-b*x(j)
11    continue
      if (mod(ndata,2).eq.0) then
        j=ndata/2
        aa=0.5*(select(j,ndata,arr)+select(j+1,ndata,arr))
      else
        aa=select((ndata+1)/2,ndata,arr)
      endif
      sum=0.
      abdev=0.
      do 12 j=1,ndata
        d=y(j)-(b*x(j)+aa)
        abdev=abdev+abs(d)
        if (y(j).ne.0.) d=d/abs(y(j))
        if (abs(d).gt.EPS) sum=sum+x(j)*sign(1.0d0,d)
c        if (abs(d).gt.EPS) sum=sum+x(j)*sign(1.0,d)
12    continue
      rofunc=sum
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software *%&&,1{.

      FUNCTION select(k,n,arr)
      INTEGER k,n
      double precision select,arr(n)
      INTEGER i,ir,j,l,mid
      double precision a,temp
      l=1
      ir=n
1     if(ir-l.le.1)then
        if(ir-l.eq.1)then
          if(arr(ir).lt.arr(l))then
            temp=arr(l)
            arr(l)=arr(ir)
            arr(ir)=temp
          endif
        endif
        select=arr(k)
        return
      else
        mid=(l+ir)/2
        temp=arr(mid)
        arr(mid)=arr(l+1)
        arr(l+1)=temp
        if(arr(l+1).gt.arr(ir))then
          temp=arr(l+1)
          arr(l+1)=arr(ir)
          arr(ir)=temp
        endif
        if(arr(l).gt.arr(ir))then
          temp=arr(l)
          arr(l)=arr(ir)
          arr(ir)=temp
        endif
        if(arr(l+1).gt.arr(l))then
          temp=arr(l+1)
          arr(l+1)=arr(l)
          arr(l)=temp
        endif
        i=l+1
        j=ir
        a=arr(l)
3       continue
          i=i+1
        if(arr(i).lt.a)goto 3
4       continue
          j=j-1
        if(arr(j).gt.a)goto 4
        if(j.lt.i)goto 5
        temp=arr(i)
        arr(i)=arr(j)
        arr(j)=temp
        goto 3
5       arr(l)=arr(j)
        arr(j)=a
        if(j.ge.k)ir=j-1
        if(j.le.k)l=i
      endif
      goto 1
      END
C  (C) Copr. 1986-92 Numerical Recipes Software *%&&,1{.

