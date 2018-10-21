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

Results<-NormVec_dmean(Matrix=pca1,x=MyData$x,y=MyData$y,z=MyData$z)
