require(circular)
require(lidR)
require(viridis)
require(rgl)
library(fpc)

source("R/2021_OHM_FUN.R")

files.trunk<-list.files("data", pattern="trunk.csv", full.names = TRUE, recursive=TRUE)
files.branch<-list.files("data", pattern="branch.csv", full.names = TRUE, recursive=TRUE)
tree.ID<-list.dirs("data", full.names = FALSE)

files<-data.frame(treeID=tree.ID[-1],
                  trunk=files.trunk,
           branch=files.branch)

ohm.files<-read.csv.ohm.input(files[1,])

trunk<-data.frame(ohm.files[[1]])
branch<-data.frame(ohm.files[[2]])

OHM.output<-OHM.tree(trunk = trunk,
                     branch = branch,
                     interval = 0.1, buff = 0.1, outlier_pct = 20,
                     vox.res=0.1, vol.correction=2.5)

plot(OHM.output$all.circle.fits$r, OHM.output$all.circle.fits$z, col="grey", main='Trunk Radius Fits',
     ylab='Height (m)', xlab= "Radius (m)")
lines(OHM.output$filtered.circle.fits$r, OHM.output$filtered.circle.fits$z, col="forestgreen")

plot(OHM.output$trunk.volume$z, cumsum(OHM.output$trunk.volume$vol), col="white", xlab='Height (m)', ylab= "Volume (m^3)", 
     main="Cumulative Tree \n Component Volume")
lines(OHM.output$trunk.volume$z, cumsum(OHM.output$trunk.volume$vol), col="brown")
lines(OHM.output$branch.volume$z, cumsum(OHM.output$branch.volume$vol.log), col="forestgreen")

print(paste("Total tree biomass estimate is",
            OHM.output$biomass[3]
))
print(paste("Trunk tree biomass estimate is",
            OHM.output$biomass[1]
))
print(paste("Branch tree biomass estimate is",
            OHM.output$biomass[2]
))


