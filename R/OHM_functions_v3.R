require(VoxR)
require(circular)
require(sp)
require(spatstat)

buffer <- function (tree_vox, d = 20, voxel = 0.02) {
  z_list <- z_list_range(tree_vox)
  datalist = list()
  
  for (i in 1:length(z_list)) {
    if (d>0) {
      slice <- tree_vox[tree_vox[,3] >= z_list[i]-(d*voxel) & tree_vox[,3] <= z_list[i]+(d*voxel),1:3]
      if (nrow(slice)==0) next
      slice$z <- z_list[i]
      datalist[[i]] <- slice[!duplicated(slice[,1:3]),]
    }
    # if (d>0) datalist[[i]] <- tree_vox[tree_vox[,3] >= z_list[i]-(d*voxel*2) & 
    #                                      tree_vox[,3] <= z_list[i],]
  }
  
  slab<-do.call(rbind, datalist)
  if (d==0) return(tree_vox) else return(slab)
}

#get list of potential z values
z_list_range<- function (tree_vox, voxel = 0.02){
  return(seq(min(tree_vox[,3]), max(tree_vox[,3]), by = voxel))
}

initiate <- function (tree_vox, slice_n_min = 10, area = TRUE, voxel = 0.02){
  
  z_list <- z_list_range(tree_vox)
  
  for (i in 1:length(z_list)) {
    
    slice <- tree_vox[tree_vox[,3] == z_list[i],1:3] # subset a single slice the thickness of a voxel
    
    if(length(slice$z) == 0) next 
    else if(length(slice$z) < slice_n_min) {
      if (area == FALSE) return(cbind(slice, 0)) else {
        
        return(data.frame(x = mean(slice$x),
                          y = mean(slice$y), 
                          z = mean(slice$z), 
                          A = nrow(slice)*voxel^2, 
                          V = nrow(slice)*voxel^3)) 
      }
      
    } else if(length(slice$z) > slice_n_min) break # stop when slice has greater than n_min voxels.
  }
}

circ_err <- function (slice, area = TRUE, r = TRUE) {
  #Fit a circle to the slice with ls means
  circle<-circular::lsfit.circle(slice$x,slice$y)
  if (r == FALSE) return(sqrt(mean((circle$radius-circle$coefficients[1])^2))) 
  else return(circle$coefficients[1])
}

circ <- function (slice, area = TRUE, r_max = 0.1, voxel = 0.02) {
  
  #Fit a circle to the slice with ls means
  circle<-circular::lsfit.circle(slice$x,slice$y)
  c <- circle$coefficients
  
  if (!is.null(circle)) {
    
    if (c[1]<=r_max) {
      
      if (area == TRUE){
        
        return (data.frame(x = c[2], 
                           y = c[3], 
                           z = mean(slice$z), 
                           A = pi*c[1]^2, 
                           V = (pi*c[1]^2)*voxel))  
      }
      
      if (area == FALSE) {
        r <- c[1]
        x_center <- c[2]
        y_center <- c[3]
        
        pts = seq(0, 2 * pi, length.out = 1000)
        xy = cbind(x_center + r * sin(pts), y_center + r * cos(pts))
        colnames(xy) <- c("x", "y")
        xy <- as.data.frame(xy)
        conv<-convexhull.xy(xy$x, xy$y)
        mask <- as.mask(conv, eps=voxel)
        fill <- raster.xy(mask, drop = TRUE)
        xyz_fill <- cbind(fill$x, fill$y, mean(slice$z))
        xyz_fill_circ <- cbind(xyz_fill, 1)
        
        return(xyz_fill_circ) 
      }
      
    } else return(data.frame(x = mean(slice$x),
                             y = mean(slice$y), 
                             z = mean(slice$z), 
                             A = nrow(slice)*voxel^2, 
                             V = nrow(slice)*voxel^3))
  } else if (is.null(circle)) {
    
    if (area == FALSE) return(slice) else if (area == TRUE) return(data.frame(x = mean(slice$x),
                                                                              y = mean(slice$y), 
                                                                              z = mean(slice$z), 
                                                                              A = nrow(slice)*voxel^2, 
                                                                              V = nrow(slice)*voxel^3))
    
  }
}

convex <- function (slice, voxel = 0.02, area = TRUE){
  
  # remove duplicate points so the chull function doesn't error out
  treenodup <- lapply(slice,"[",which(!duplicated(as.matrix(as.data.frame(slice)))))
  conv <-convexhull.xy(treenodup)
  
  if (!is.null(conv)) {
    
    if (area == TRUE) {
      return(data.frame(x = mean(conv$xrange),
                        y = mean(conv$yrange), 
                        z = mean(slice$z), 
                        A = area(conv), 
                        V = area(conv)*voxel))
    }
    
    if (area == FALSE) {
      mask <- as.mask(conv, eps=voxel)
      fill <- raster.xy(mask, drop = TRUE)
      xyz_fill <- cbind(fill$x, fill$y, slice$z[1])
      xyz_fill_conv <- cbind(xyz_fill, 2)
      return(xyz_fill_conv)
    }
    
  } else if (is.null(conv)) return(cbind(slice, 0))
  
}

convex_peel <- function (slice, voxel = 0.02, area = TRUE, peel_min = 100, d = 20){
  
  peel_pct <- -0.0163*d^2+1.8021*d+10.997
  
  # remove duplicate points so the chull function doesn't error out
  treenodup <- lapply(slice,"[",which(!duplicated(as.matrix(as.data.frame(slice)))))
  conv <-convexhull.xy(treenodup)
  xy_bdry <- cbind(conv[[4]][[1]][[1]], conv[[4]][[1]][[2]])
  if (is.null(xy_bdry)) next
  c_xy_bdry<- SpatialPoints(xy_bdry)
  c_slice<- SpatialPoints(slice[1:2])
  
  pct <- 100
  
  list_pts <- zerodist2(c_xy_bdry, c_slice)
  peeled <- c_slice[-list_pts]
  peeled <- as.data.frame(peeled)
  
  if (nrow(peeled) > peel_min) {
    
    conv <-convexhull.xy(peeled)
    A <- area(conv)
    xy_bdry <- cbind(conv[[4]][[1]][[1]], conv[[4]][[1]][[2]]) 
    
    for (p in 1:50) {
      #Percent of Original trunk points remining
      pct[p] <- nrow(peeled)/(nrow(slice))*100
      #percent threshold
      if (pct[p] > peel_pct) {    
        #make spatial coordinates and subtract outer points from original points
        c_xy_bdry<- SpatialPoints(xy_bdry)
        peeled<- SpatialPoints(peeled)
        list_pts <- zerodist2(c_xy_bdry, peeled)
        peeled <- peeled[-list_pts]
        peeled <- as.data.frame(peeled)
        conv <-convexhull.xy(peeled)
        A[p] <- area(conv)  
        xy_bdry <- cbind(conv[[4]][[1]][[1]], conv[[4]][[1]][[2]])
      }
    }
  }
  
  if (!is.null(conv)) {
    
    if (area == TRUE) {
      return(data.frame(x = mean(conv$xrange),
                        y = mean(conv$yrange), 
                        z = mean(slice$z), 
                        A = area(conv), 
                        V = area(conv)*voxel))
    }
    
    if (area == FALSE) {
      mask <- as.mask(conv, eps=voxel)
      fill <- raster.xy(mask, drop = TRUE)
      xyz_fill <- cbind(fill$x, fill$y, slice$z[1])
      xyz_fill_conv <- cbind(xyz_fill, 2)
      return(xyz_fill_conv)
    }
    
  } else if (is.null(conv)){
    if (area == FALSE) return(cbind(slice, 0))
    if (area == TRUE){
      return(data.frame(x = mean(slice$x),
                        y = mean(slice$y), 
                        z = mean(slice$z), 
                        A = nrow(slice)*voxel^2, 
                        V = nrow(slice)*voxel^3)) 
    }
    
  }
  
}

OHM<- function (tree, slice_n_min = 10, voxel = 0.02, buffer = TRUE, d = 20, area = TRUE, r = TRUE, r_max = 0.01, method = "peel", peel_pct = 20, peel_min = 10) {
  
  #Voxelize
  tree_vox <- vox(tree, voxel)
  colnames(tree_vox) <- c("x", "y","z", "n")
  z_list<-z_list_range(tree_vox,voxel) # range of z values to iterate over
  
  if (buffer == TRUE) tree_vox_buffer<-buffer(tree_vox, d, voxel) else tree_vox_buffer <- tree_vox
  
  if (area == TRUE) {
    vol_h <- matrix(0.0000,nrow = length(z_list), ncol = 5)
    vol_h <- as.data.frame(vol_h)
    # vol_h[1,] <- c(initiate(tree_vox_buffer, slice_n_min, area))
    
    for(i in 1:length(z_list)){
      slice <- subset(tree_vox_buffer, z >= z_list[i] & z <= z_list[i])
      if (nrow(slice)<slice_n_min) next 
      else if(circ_err(slice, area, r) < r_max | method == "circle") vol_h[i,] <- circ(slice, area) 
      else if (circ_err(slice, area, r)>=r_max | method == "peel") vol_h[i,] <- convex_peel(slice, voxel, area, peel_min, d) 
      else if(method == "convex") vol_h[i,] <- convex(slice, voxel, area)
      progress(i/(length(z_list))*100)
    }
    colnames(vol_h) <- c("x","y","z","A","V")
    return(vol_h)
  }
  else if (area == FALSE) {
    datalist = list()
    for(i in 1:length(z_list)){
      slice <- subset(tree_vox_buffer, z >= z_list[i] & z <= z_list[i])
      if (nrow(slice)<slice_n_min) next 
      else if(circ_err(slice) < r_max | method == "circle") datalist[[i]] <- circ(slice, area = FALSE)[,1:3]
      else if (circ_err(slice) >= r_max | method == "peel") datalist[[i]]  <- convex_peel(slice, voxel, area = FALSE, peel_min, d)[,1:3]
      else if(method == "convex") datalist[[i]]  <- convex(slice, voxel, area = FALSE)[,1:3]
      progress(i/(length(z_list))*100)
    }
    slab<-do.call(rbind, datalist)
    return(slab)
  }
  
}


maxznorm <- function (raw_z) max_h<-max(raw_z)-min(raw_z)

tcorrect <- function (ohm_tree, tree_vox, max_h, voxel = 0.02) {

  l_pred<-floor(max_h/voxel)-nrow(ohm_tree)
  
  ohm_tree$z<-ohm_tree$z-min(ohm_tree$z)
  #create approximation of trunk diameeter
  trunk_taper_m<-data.frame(z = ohm_tree$z, d = ohm_tree$V, z2 = ohm_tree$z^2)
  
  tot_z_range<-seq(0,max_h,by=voxel)
  
  trunk_taper_bin<-aggregate(d~z,data = trunk_taper_m, FUN = "sum")
  tree_vox$z<-round(tree_vox$z-min(tree_vox$z),2)
  w <- aggregate(n~z, FUN = "sum", data = tree_vox)
  
  # d_h<-trunk_taper_m
  
  f.prime <- (diff(as.numeric(trunk_taper_m$d))/diff(seq(1,length(trunk_taper_m$d),1)))
  d_h<-data.frame(z = trunk_taper_m$z[f.prime<quantile(f.prime,.70, na.rm = TRUE) & f.prime>quantile(f.prime,.30, na.rm = TRUE)],
                  d = trunk_taper_m$d[f.prime<quantile(f.prime,.70, na.rm = TRUE) & f.prime>quantile(f.prime,.30, na.rm = TRUE)])
  
  
  d_h$z2<- d_h$z^2
  
  
  d_h$n<-1
  d_h$n <- w$n[match(as.character(round(d_h$z, 2)) , as.character(round(w$z,2)))]

  taper_est <- lm(d ~ z + z2, weights = n, data = d_h)
  
    
  # plot(d_h$z,d_h$d, pch=16, ylab = "diameter", cex.lab = 1.3, col = "red" )
  # abline(taper_est, col="blue")
  predictedtaper <- predict(taper_est,trunk_taper_m)
  
  #create taper threshold
  max_predictedtaper <- predictedtaper+predictedtaper*.05
  min_predictedtaper <- max_predictedtaper*0.50
  
  # lines(trunk_taper_m$z, predictedtaper, col = "blue", lwd = 3)
  # lines(trunk_taper_m$z, max_predictedtaper, col = "blue", lwd = 3)
  # lines(trunk_taper_m$z, min_predictedtaper, col = "blue", lwd = 3)
  
  #Constrain trunk with min and max taper
  # ii=NULL
  # d_cor <- matrix(1,nrow = length(d_h$z), ncol = 2)
  
  trunk_taper_m$d_cor<-trunk_taper_m$d
  trunk_taper_m$d_cor[trunk_taper_m$d > max_predictedtaper] <- max_predictedtaper[trunk_taper_m$d > max_predictedtaper]
  trunk_taper_m$d_cor[trunk_taper_m$d < min_predictedtaper] <- min_predictedtaper[trunk_taper_m$d < min_predictedtaper]
 
  # lines(trunk_taper_m$z,trunk_taper_m$d_cor, col = "green", lwd = 3)
 

  add_top<-data.frame(z = seq(max(ohm_tree$z),max_h,by = voxel))
  add_top$z2<-add_top$z^2
  add_top$d <- add_top$d_cor <- predict(taper_est,add_top)
  
  ohm_tree_corrected<-rbind.data.frame(add_top,trunk_taper_m)
  ohm_tree_corrected<-ohm_tree_corrected[,-2]
  colnames(ohm_tree_corrected)<-c("z","Vcor","V")
  # plot(d_cor)
  return(ohm_tree_corrected)
}







 
  