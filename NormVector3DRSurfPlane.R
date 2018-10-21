# Compute the Normal to the 2D PC plane
NormVec_dmean<-function(Matrix,x,y,z){
  normVec = c(Matrix$rotation[2,1]*Matrix$rotation[3,2]-
              Matrix$rotation[3,1]*Matrix$rotation[2,2],
            Matrix$rotation[3,1]*Matrix$rotation[1,2]-
              Matrix$rotation[1,1]*Matrix$rotation[3,2],
            Matrix$rotation[1,1]*Matrix$rotation[2,2]-
              Matrix$rotation[2,1]*Matrix$rotation[1,2]
)
# Compute the 3D point of gravitational balance (Plane has to go through it)
dMean <- apply( cbind(x,y,z), 2, mean)
d <- as.numeric((-1)*normVec %*% dMean)  # force the plane to go through the mean
list("NormVec"=normVec,"dmean"=dMean,"d"=d)
}

# Create regression plane for bivariate linear model
###   Define 3d features

BivariatePlane<-function(data,x,y,z){
fit<-lm(z~x+y,data) 
coef.lm.fit<-coef(fit)

x.seq = seq(min(x), max(x),length.out = 100)
y.seq = seq(min(y), max(y),length.out = 100)
z.seq <- function(x,y) coef.lm.fit[1]+coef.lm.fit[2]*x+coef.lm.fit[3]*y

# define the values of z = z(x.seq, y.seq), as a Matrix of 
# dimension c(dim(x.seq), dim(y.seq))
z <- t(outer(x.seq, y.seq, z.seq))
residuals<-fit$residuals  
  
list("x.seq"=x.seq,"y.seq"=y.seq,z=z,residuals=residuals)
}


