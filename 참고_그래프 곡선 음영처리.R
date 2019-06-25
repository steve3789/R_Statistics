library(ggplot2)

xv<-seq(0,4,0.01)
yv<-dnorm(xv,2,0.5) 
plot(xv,yv,type="l") 
polygon(c(xv[xv<=1.5],1.5),c(yv[xv<=1.5],yv[xv==0]),col="grey") 

x<-seq(0.0,0.1699,0.0001)   
ytop<-dnorm(0.12,0.08,0.02)
MyDF<-data.frame(x=x,y=dnorm(x,0.08,0.02))
p<-qplot(x=MyDF$x,y=MyDF$y,geom="line") 
p+geom_segment(aes(x=0.12,y=0,xend=0.12,yend=ytop))



#First subst the data and add the coordinates to make it shade to y = 0
shade <- rbind(c(0.12,0), subset(MyDF, x > 0.12), c(MyDF[nrow(MyDF), "X"], 0))

#Then use this new data.frame with geom_polygon
p + geom_segment(aes(x=0.12,y=0,xend=0.12,yend=ytop)) +
  geom_polygon(data = shade, aes(x, y))