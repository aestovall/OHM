

# Modeling Conifer Biomass from Terrestrial Laser Scanning Data with OHM
Algorithms described in "Can terrestrial laser scanning improve or replace destructive calibration data for species-specific allometric biomass equations?" (Stovall et al. Submitted) for estimating tree-level component biomass from terrestrial laser scanning (TLS) data. 

## Getting Started

### Prepare Tree-level TLS Data
First we must separate isolated tree TLS data into trunk and branch point clouds. We reccomend [CloudCompare](https://www.danielgm.net/cc/]) to open point clouds and isolate the trunk and branch tree components. 

Start with a view above or below the tree and use the scissor tool to clip the interior trunk points. Continue to clip as much of the branch points from the trunk point cloud as possible. When complete, save the branch and trunk point clouds as separate files (.csv, .asc, or similar). These files will be used with the OHM algorithm to estimate tree-level biomass.

### Setup
```{r echo=T, results='hide'}
require(circular)
require(lidR)
require(viridis)
require(rgl)
library(fpc)

source("R/2021_OHM_FUN.R")

```

### Read in Tree-level TLS Data

```{r}
#Where is the TLS data?
files.trunk<-list.files("data", pattern="trunk.csv", full.names = TRUE, recursive=TRUE)
files.branch<-list.files("data", pattern="branch.csv", full.names = TRUE, recursive=TRUE)
tree.ID<-list.dirs("data", full.names = FALSE)

#Making a dataframe of files for the tree components
files<-data.frame(treeID=tree.ID[-1],
                  trunk=files.trunk,
           branch=files.branch)

#Read in TLS data
ohm.files<-read.csv.ohm.input(files[1,])

#We can split the files into trunk and biomass elements
trunk <- ohm.files[[1]]
branch <- ohm.files[[2]]
```

### Estimate Tree Volume and Biomass with OHM

```{r echo=T, results='hide'}
OHM.output<-OHM.tree(trunk = trunk,
                     branch = branch,
                     sg=0.4,
                     interval = 0.1, buff = 0.1, outlier_pct = 20,
                     vox.res=0.1, vol.correction=1)
```


### Check the output
We can look at the OHM algorithm output in several ways. First the trunk radius fits:
```{r}
#Look at the trunk radius fits
plot(OHM.output$all.circle.fits$r, OHM.output$all.circle.fits$z, col="grey", main='Trunk Radius Fits',
     ylab='Height (m)', xlab= "Radius (m)")
lines(OHM.output$filtered.circle.fits$r, OHM.output$filtered.circle.fits$z, col="forestgreen")
```
![](https://github.com/aestovall/OHM/readme/trunk_radius.png)


We can also look at the cumulative biomass of individual trees with height:
```{r}
plot(OHM.output$trunk.volume$z, cumsum(OHM.output$trunk.volume$vol), col="white", xlab='Height (m)', ylab= "Volume (m^3)", 
     main="Cumulative Tree \n Component Volume")
lines(OHM.output$trunk.volume$z, cumsum(OHM.output$trunk.volume$vol), col="brown")
lines(OHM.output$branch.volume$z, cumsum(OHM.output$branch.volume$vol.log), col="forestgreen")
```
![](https://github.com/aestovall/OHM/readme/tree_volume.png)

Finally, what is the component biomass of the modeled tree? 
```{r}
print(paste("Total tree biomass estimate is",
            OHM.output$biomass[3]
))
print(paste("Trunk tree biomass estimate is",
            OHM.output$biomass[1]
))
print(paste("Branch tree biomass estimate is",
            OHM.output$biomass[2]
))
```
##[1] "Total tree biomass estimate is 1012.92835037034"
##[1] "Trunk tree biomass estimate is 504.203603733976"
##[1] "Branch tree biomass estimate is 508.724746636365"

### References
Stovall, A.E.L., Vorster, A.G., Anderson, R.S., Evangelista, P.H., Shugart, H.H., 2017. Non-destructive aboveground biomass estimation of coniferous trees using terrestrial LiDAR. Remote Sensing of Environment 200, 31–42. https://doi.org/10.1016/j.rse.2017.08.013


