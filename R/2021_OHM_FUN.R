circle_stats<-function(circle_fit){
  return(data.frame(se=sd(circle_fit$coefficients[1]-circle_fit$radius)/sqrt(length(circle_fit$radius)),
                    se_pct=(sd(circle_fit$coefficients[1]-circle_fit$radius)/sqrt(length(circle_fit$radius)))/as.numeric(circle_fit$coefficients[1])*100,
                    RMSE=sqrt(mean((circle_fit$coefficients[1]-circle_fit$radius)^2)),
                    RMSE_pct=sqrt(mean((circle_fit$coefficients[1]-circle_fit$radius)^2))/as.numeric(circle_fit$coefficients[1])*100))
}

OHM.outlier.removal<-function(z_heights.all, outlier_pct=20, model="lm"){
  z_heights.all<-z_heights.all[z_heights.all$r<0.50,]
  z_heights.all<-z_heights.all[!is.na(z_heights.all$r),]
  z_heights.all$n<-1
  
  z_heights.all<-z_heights.all[z_heights.all$r<=(mean(z_heights.all$r)+2*sd(z_heights.all$r))&
                                 z_heights.all$r>=(mean(z_heights.all$r)+-2*sd(z_heights.all$r)),]
  
  z_heights.sum<-data.frame(aggregate(r~z,FUN=function(x) quantile(x,0.5),z_heights.all),
                            sd=aggregate(r~z,FUN=sd,z_heights.all)[,2])
  
  if(model == "poly") {m<-lm(r~poly(z,2),weights=1/sd,
                             na.exclude(z_heights.sum[z_heights.sum$sd>0,]))}
  
  if(model == "lm") {m<-lm(r~z,weights=1/sd,
                           na.exclude(z_heights.sum[z_heights.sum$sd>0,]))}
  
  plot(z_heights.all$z, z_heights.all$r)
  lines(z_heights.sum$z, predict(m, newdata=z_heights.sum), col="red")
  
  z_heights.all<-z_heights.all[abs(predict(m, newdata=z_heights.all)-z_heights.all$r)/predict(m, newdata=z_heights.all)*100<=outlier_pct,]
  points(z_heights.all$z, z_heights.all$r, col="blue")
  
  return(z_heights.all)
}

OHM.circle <- function(z_list,trunk, buff){
  
  return(do.call(rbind,
                 lapply(z_list, function(x){
    
                   trunk.sub<-NULL
                   
    trunk.sub<-trunk[trunk$Z>(x-buff)&
                     trunk$Z<(x+buff), ]
    
    if(nrow(trunk.sub)<3|is.null(trunk.sub)) return(data.frame(r=NA,a=NA,b=NA,z=x)) else{
      
      # plot(trunk.sub[,c(1,2)], asp=1, cex=0.8)
      # plot3d(trunk.sub[,1:3], asp=1, cex=0.2)
      c.m<-NULL
      try(c.m<-circular::lsfit.circle(trunk.sub$X,trunk.sub$Y), silent = TRUE)
      
      if(!is.null(c.m)){
        # plot(c.m)
        
        
        
        # c.m.p<-raster::buffer(SpatialPoints(coordinates(data.frame(t(c.m$coefficients[2:3])))),
        #                       width=c.m$coefficients[1]*2)
        
        # circ.coord<-data.frame(x=c.m$coefficients[1]*cos(seq(0,2*pi,by=2*pi/50)),
        #            y=c.m$coefficients[1]*sin(seq(0,2*pi,by=2*pi/50)))
        #
        # polygon(circ.coord[,1:2]+t(c.m$coefficients[2:3]))
        # plot(trunk.sub$X,trunk.sub$Y)
        options(warn=-1)
        trunk.sub<-LAS(data.frame(trunk.sub))
        options(warn=0)
        trunk.sub.clip<-lidR::clip_circle(trunk.sub,
                                         xcenter=c.m$coefficients[2],
                                         ycenter=c.m$coefficients[3],
                                         radius=c.m$coefficients[1])
        
        # c.m<-circular::lsfit.circle(trunk.sub.clip$X,trunk.sub.clip$Y)
        # plot(c.m)
        
        trunk.sub.clip<-trunk.sub.clip@data
        
        circ<-NULL
        circ<-do.call(rbind,lapply(seq(x-buff, x+buff, by=0.01), function(xx){
          trunk.sub.sub<-trunk.sub.clip[trunk.sub.clip$Z>(xx-0.01)&
                                        trunk.sub.clip$Z<(xx+0.01),]
          if(nrow(trunk.sub.sub)<3) return(NULL) else{
            c.m<-circular::lsfit.circle(trunk.sub.sub$X,trunk.sub.sub$Y)
            return(coef(c.m))
          }}))
        
        if(is.null(circ)&!is.null(c.m)) return(data.frame(t(coef(c.m)),z=x)) else{
          if(is.null(circ)&is.null(c.m)) return(data.frame(r=NA,a=NA,b=NA,z=x)) else{
            return(data.frame(circ,z=x))
          }
        }
      }
    }
  })
  ))
}

vol.frustrum <- function(R_1, R_2, h){
  return( 1/3*pi*h*(R_1^2+R_1*R_2+R_2^2) )
}

OHM.vol<-function(z_heights.all){
  z_heights.all$n<-1
  
  z_heights.all.summary<-aggregate(r~z, FUN=function(x) quantile(x, 0.50), z_heights.all)
  z_heights.all.summary$sd<-aggregate(r~z, FUN=sd, na.exclude(z_heights.all))[,2]
  z_heights.all.summary$n<-aggregate(n~z, FUN=sum, na.exclude(z_heights.all))[,2]
  z_heights.all.summary$se<-z_heights.all.summary$sd/sqrt(z_heights.all.summary$n)
  
  z_heights.all.3d<-aggregate(.~z, FUN=function(x) quantile(x, 0.50), z_heights.all[-length(z_heights.all)])
  
  lines(z_heights.all.summary$z, z_heights.all.summary$r, col="red")
  
  # rgl::open3d()
  # for(j in 1:(nrow(z_heights.all.3d)-1)){
  #   rgl::shade3d(
  #     rgl::cylinder3d(center = z_heights.all.3d[j:(j+1),c(3,4,1)],
  #                radius = z_heights.all.3d[j:(j+1),2],
  #                sides = 200),
  #     col = "brown")
  # 
  # }
  # writePLY(gsub(".csv", "_OHM.ply",files[i]))
  # rgl::rgl.quit()
  
  vol<-data.frame(z=z_heights.all.3d$z[1:(nrow(z_heights.all.3d)-1)])
  vol$vol<-(-99.9)
  for(ii in 1:(nrow(z_heights.all.3d)-1)){
    vol[ii,2]<-data.frame(vol=(c(vol.frustrum(R_1= z_heights.all.3d$r[ii],
                                              R_2 = z_heights.all.3d$r[ii+1],
                                              h=z_heights.all.3d$z[ii+1]-z_heights.all.3d$z[ii]))))
    
  }
  
  plot(vol)
  return(vol)
}


branch.angle.dist<-function(x){
  
  r.adj<-length(unique(branch.sub$angle.bin))
  
  branch.sub.angle<-branch.sub[branch.sub$angle.bin==x,]
  
  branch.sub.angle$vol<-1
  branch.ag<-aggregate(vol~I(floor(dist/vox.res)*vox.res), FUN="sum",branch.sub.angle)
  colnames(branch.ag)[1]<-"dist"
  
  
  branch.ag$vox.size<-as.numeric(trunk.sub$r/r.adj*log(max(branch.ag$dist+1)-branch.ag$dist)/log(max(branch.ag$dist+1)))
  branch.ag$vox.size[branch.ag$vox.size<0.005]<-0.005
  
  branch.ag.log<-branch.ag
  
  branch.ag$vox.size<-as.numeric((-(trunk.sub$r/r.adj)/max(branch.ag$dist))*branch.ag$dist+(trunk.sub$r/r.adj))
  branch.ag$vox.size[branch.ag$vox.size<0.005]<-0.005
  
  branch.ag.lm<-branch.ag
  
  df.angle<-data.frame(angle=x,branch.ag.log[,-c(2)],branch.ag.lm$vox.size)
  colnames(df.angle)[3:4]<-c("vox.size.log","vox.size.lm")
  
  return(df.angle)
}


OHM.trunk<-function(trunk, branch,interval = 0.1, buff = 0.1, outlier_pct = 20){
  print(paste("Processing trunk point cloud"))
 
  trunk.min<-min(trunk$Z)
  if(min(trunk$Z)>2){
    trunk$Z<-trunk$Z-trunk.min
    branch$Z<-branch$Z-trunk.min
  } 
  
  z_list<-seq(0, max(trunk$Z), by = interval)
  
  z_heights.all.out<-data.frame(r=NA,
                            a=NA,
                            b=NA,
                            z=NA)
  
  z_heights.all.out<-OHM.circle(z_list,trunk, buff=buff)
  z_heights.all.out<-OHM.outlier.removal(z_heights.all.out, outlier_pct=outlier_pct, model="lm")
  
  return(z_heights.all.out)
}

OHM.circle.3D<-function(x) aggregate(.~z, FUN=function(x) quantile(x, 0.50), x[-length(x)])

OHM.branch<-function(branch, OHM.circle.3D.out, vox.res=0.1, vol.correction=1){
  branch.vox<-VoxR::vox(branch, res=vox.res)
  colnames(branch.vox)<-c("X","Y","Z", "nbpts")
  
  branch.vol<-do.call(rbind,
                      lapply(
                        unique(branch.vox$Z)[order(unique(branch.vox$Z))], function(x){
                          
                          branch.sub<-branch.vox[branch.vox$Z==x,]
                          trunk.sub<-OHM.circle.3D.out[OHM.circle.3D.out$z==round(x,1),]
                          
                          if(nrow(branch.sub) != 0& nrow(trunk.sub) !=0) {
                            
                            if(nrow(trunk.sub)==0){
                              trunk.sub=data.frame(z=x,
                                                   r=predict(lm(r~z,OHM.circle.3D.out), newdata=data.frame(z=x)),
                                                   a=min(branch.sub$X)+(max(branch.sub$X)-min(branch.sub$X))/2,
                                                   b=min(branch.sub$Y)+(max(branch.sub$Y)-min(branch.sub$Y))/2)
                              if(trunk.sub$r<0.025) trunk.sub$r<-0.025
                              
                            }
                            
                            branch.sub$X.center<-branch.sub$X-trunk.sub$a
                            branch.sub$Y.center<-branch.sub$Y-trunk.sub$b
                            
                            branch.sub$angle<-atan2(branch.sub$Y.center,branch.sub$X.center)*(180/pi)
                            branch.sub$angle.bin<-round(branch.sub$angle, -1)
                            branch.sub$dist<-sqrt(branch.sub$X.center^2+branch.sub$Y.center^2)
                            
                            # branch.angle.dist.out<-lapply(unique(branch.sub$angle.bin), function(x) branch.angle.dist(x))
                            # branch.angle.dist.out<-do.call(rbind, branch.angle.dist.out)  
                            
                            branch.sub$vol<-1
                            branch.ag<-aggregate(vol~I(floor(dist/vox.res)*vox.res), FUN="sum",branch.sub)
                            colnames(branch.ag)[1]<-"dist"
                            
                            branch.ag$vox.size<-2*trunk.sub$r*log(max(branch.ag$dist+1)-branch.ag$dist)/log(max(branch.ag$dist+1))
                            branch.ag$vox.size[branch.ag$vox.size<0.001]<-0.001
                            branch.ag.log<-branch.ag
                            
                            branch.ag$vox.size<-(-2*trunk.sub$r/max(branch.ag$dist))*branch.ag$dist+2*trunk.sub$r
                            branch.ag$vox.size[branch.ag$vox.size<0.001]<-0.001
                            
                            return(data.frame(z=x, 
                                              vol.log=sum(branch.ag.log$vol/sum(branch.ag.log$vol)*
                                                            ((vox.res*pi*branch.ag.log$vox.size^2))),
                                              vol.log.cube=sum(branch.ag.log$vol/sum(branch.ag.log$vol)*
                                                                 ((branch.ag.log$vox.size^3)))
                                              # vol.lm=sum(branch.ag.lm$vol/sum(branch.ag.lm$vol)*
                                              #              ((vox.res*pi*branch.ag.lm$vox.size^2))),
                                              # vol.lm.cube=sum(branch.ag.lm$vol/sum(branch.ag.lm$vol)*
                                              #                   ((branch.ag.lm$vox.size^3))),
                                              # vol.angle.log=sum(((vox.res*pi*branch.angle.dist.out$vox.size.log^2)))
                                              # vol.angle.lm=sum(((vox.res*pi*branch.angle.dist$vox.size.lm^2)))
                                              
                            ))
                          }
                          
                        }
                      )
  )
  
  return(branch.vol)
  
}

read.csv.ohm.input<-function(files){
  tree<-NULL
  try(tree<-read.csv(files$trunk, header = TRUE, stringsAsFactors = FALSE)[1:3],silent=TRUE)
  if(is.null(tree)) try(tree<-read.csv(files$trunk, header = TRUE, sep=" ", stringsAsFactors = FALSE)[1:3],silent=TRUE)
  if(is.null(tree)) warning("Incorrect input format! Must be xyz data.")
  colnames(tree)<-c("X", "Y", "Z")
  
  ID<-files$treeID
  
  branch<-NULL
  try(branch<-read.csv(files$branch, header = TRUE, stringsAsFactors = FALSE)[1:3],silent=TRUE)
  if(is.null(branch)) try(branch<-read.csv(files$branch, header = TRUE, sep=" ", stringsAsFactors = FALSE)[1:3],silent=TRUE)
  if(is.null(branch)) warning("Incorrect input format! Must be xyz data.")
  colnames(branch)<-c("X", "Y", "Z")
  
  return(list(tree,
              branch))
  
}

# OHM.tree<-function(trunk,
#                    branch,
#                    sg=0.4,
#                    interval = 0.1, buff = 0.1, outlier_pct = 20,
#                    vox.res=0.1, vol.correction=1){
#   
#   z_heights.all<-OHM.trunk(trunk, interval, buff, outlier_pct)
#   
#   vol<-OHM.vol(z_heights.all)
#   
#   OHM.circle.3D.out<-OHM.circle.3D(z_heights.all)
#   
#   OHM.branch.out<-OHM.branch(branch, OHM.circle.3D.out, vox.res, vol.correction)
#   
#   return(list(all.circle.fits = z_heights.all,
#               filtered.circle.fits=OHM.circle.3D.out,
#               trunk.volume = vol,
#               branch.volume=OHM.branch.out,
#               biomass = data.frame(trunk=sum(OHM.output$trunk.volume$vol*sg)*1000,
#                                    branch=sum((OHM.output$branch.volume$vol.log*sg/vol.correction))*1000,
#                                    total=sum(OHM.output$trunk.volume$vol*0.45)*1000+
#                                      sum((OHM.output$branch.volume$vol.log*0.45/vol.correction))*1000)
#               ))
#   
# }


OHM.tree<-function(trunk,
                   branch,
                   sg=0.4,
                   interval = 0.1, buff = 0.1, outlier_pct = 20,
                   vox.res=0.1, vol.correction=1){
  
  print(paste("Processing trunk point cloud"))
  
  trunk.min<-min(trunk$Z)
  if(min(trunk$Z)>2){
    trunk$Z<-trunk$Z-trunk.min
    branch$Z<-branch$Z-trunk.min
  } 
  
  z_list<-seq(0, max(trunk$Z), by = interval)
  
  z_heights.all.out<-data.frame(r=NA,
                                a=NA,
                                b=NA,
                                z=NA)
  
  z_heights.all.out<-OHM.circle(z_list,trunk, buff=buff)
  z_heights.all<-OHM.outlier.removal(z_heights.all.out, outlier_pct=outlier_pct, model="lm")
  
  vol<-OHM.vol(z_heights.all)
  
  OHM.circle.3D.out<-OHM.circle.3D(z_heights.all)
  
  OHM.branch.out<-OHM.branch(branch, OHM.circle.3D.out, vox.res, vol.correction)
  OHM.branch.out[is.nan(OHM.branch.out[,2]),2]<-0
  OHM.branch.out[is.nan(OHM.branch.out[,3]),3]<-0
  
  return(list(all.circle.fits = z_heights.all,
              filtered.circle.fits=OHM.circle.3D.out,
              trunk.volume = vol,
              branch.volume=OHM.branch.out,
              biomass = data.frame(trunk=sum(vol$vol*sg)*1000,
                                   branch=sum((OHM.branch.out$vol.log*sg/vol.correction))*1000,
                                   total=sum(vol$vol*sg)*1000+
                                     sum((OHM.branch.out$vol.log*sg/vol.correction))*1000)
  ))
  
}


