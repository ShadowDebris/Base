library(mgcv)
library(nlme)
install.packages("rgl")
library(rgl)
if(interactive() && require(rgl) && require(mgcv)){
  scatter3d(prestige ~ income + education, data=Duncan)
  Sys.sleep(5) # wait 5 seconds
  scatter3d(prestige ~ income + education | type, data=Duncan)
  Sys.sleep(5)
  scatter3d(prestige ~ income + education | type, surface=FALSE, 
            ellipsoid=TRUE, revolutions=3, data=Duncan)
  scatter3d(prestige ~ income + education, fit=c("linear", "additive"),
            data=Prestige)
  Sys.sleep(5)
  scatter3d(prestige ~ income + education | type, 
            radius=(1 + women)^(1/3), data=Prestige)
}
## Not run: 
# drag right mouse button to identify points, click right button in open area to exit
scatter3d(prestige ~ income + education, data=Duncan, id.method="identify")
scatter3d(prestige ~ income + education | type, data=Duncan, id.method="identify")

## End(Not run)
