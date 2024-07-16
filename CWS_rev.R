## Daniel Ballesteros Chávez
## Now the profile curve of a constant widh curve
## Compute roots of unity
library(rgl)

myunitroots  <- function(k,n){
    k  <- k%%n
    return(c(cos(2*pi*k/n),sin(2*pi*k/n)))}

NN <- 3
t1 <- seq(5*pi/6 ,7*pi/6  ,by=0.03)
t2 <- seq(3*pi/2 ,11*pi/6 ,by=0.03)
t3 <- seq(pi/6   ,3*pi/6  ,by=0.03)
V1  <- myunitroots(0,NN)
V2  <- myunitroots(1,NN)
V3  <- myunitroots(2,NN)
RR  <- sqrt(sum(( V1 - V2)^2))
G   <- c(
  ( V1[1] + RR*cos(t1)),
  ( V2[1] + RR*cos(t2)),
  ( V3[1] + RR*cos(t3)))
H  <- c(
  (V1[2] + RR*sin(t1)),
  (V2[2] + RR*sin(t2)),
  (V3[2] + RR*sin(t3)))
## Rotate pi/2
g <- -H
h  <- G
plot(g,h,type="l",xlim=c(-5,5),ylim = c(-5,5))
alpha <- seq(0,2*pi,by=0.05)
LL <- lapply(alpha,function(x){
    XXt <- cos(x)*g
    YYt  <-  sin(x)*g
    ZZt <- h
    data.frame(cbind(XXt,YYt,ZZt))})
MM <- do.call(rbind,LL)
plot3d(MM$XXt, MM$YYt, MM$ZZt, xlab = "X", ylab = "Y", zlab = "Z", col="cyan", main="A surface of constant width")
aspect3d("iso")
htmlwidgets::saveWidget(
rglwidget(width = 400, height = 400, reuse = FALSE),
"CWS_Rev.html")
