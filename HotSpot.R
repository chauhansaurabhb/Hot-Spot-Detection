base=c(1:250000)
b=matrix(base,nrow=500,ncol=500,byrow=TRUE)
c=matrix(base,nrow=500,ncol=500,byrow=TRUE)
c[1,1]=500
c[1,2]=1000
c[1,3]=300
count=0


  b1=svd(b)
  c1=svd(c)
  u1=matrix(c1$u[,1]-b1$u[,1],nrow = 500,ncol = 1)
  c1$v=t(c1$v)
  b1$v=t(b1$v)
  v1=matrix(c1$v[1,]-b1$v[1,],nrow = 1,ncol = 500)
  zu1=(u1-mean(u1))/sd(u1)
  zv1=(v1-mean(v1))/sd(v1)
  pu1=pnorm(-abs(zu1))
  pv1=pnorm(-abs(zv1))
  pu1
  for(i in 1:500)
  {
    if(pu1[i,1]<.04)
    {
      for(j in i:i)
      {
        for(k in 1:500)
        {
          if(b[j,k]!=c[j,k])
          {
            count=count+1
            c[j,k]=b[j,k]
          }
        }
      }
    }
  }

count

