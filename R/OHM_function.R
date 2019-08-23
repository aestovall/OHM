
require(VoxR)
require(circular)
require(sp)
require(spatstat)
require(svMisc)
require(rgl)


#Biomass Calculation#####
source("R/OHM_functions_v4.R")
# source("cylinder3D.R")
getwd()

# slice_n_min = 10 
# voxel = 0.02 
# d = 20
# area = TRUE
# r_max = 0.01
# method = "peel"
# peel_pct = 20
# peel_min = 10

# list_f<-list.dirs()
# 
# sg = 0.6 #tropical tree species?
# f = list_f[2]
# for (f in list_f[2]) {
#   # f<-list_f[4]
#   setwd(f)
#   setwd("ZEB_clean")
#   getwd()
#   list = list.files(pattern="*.txt")
#   df = list[4]
#   dir.create("modeled")
#   for (df in list){
#     
#     tree<-NULL
#     tree <- read.csv(df, header = TRUE, sep = "\ ", stringsAsFactors = FALSE)[1:3]
#     # 
#     # tree_vox<-vox(tree,0.02)
#     # colnames(tree_vox)<-c("x","y","z","n")
#     # # tree_vox$z<-tree_vox$z-min(tree_vox$z)
#     # tree_vox_b<-buffer(tree_vox,d=5)
#     
#     # plot(aggregate(n~z,FUN = "sum", data = buffer(tree_vox,d=20),.1))
#     # lines(aggregate(n~z,FUN = "sum", data = vox(tree,.1)))
#     
#     ohm_tree<-OHM(tree, 
#                   slice_n_min = 10,
#                   buff = TRUE,
#                   voxel = 0.02, 
#                   d = 30, #default is 20, but has errors sometimes.
#                   area = TRUE, 
#                   r_max = 0.1, 
#                   method = "peel", 
#                   peel_pct = 20, #default is 20% 
#                   peel_min = 10)
#     
#     write.table(ohm_tree,paste("modeled/",gsub('.txt','',df),"_OHM.txt", sep = ""),
#               row.names = FALSE, col.names = FALSE,
#               sep = "\ ")
#     
#     # raw_z<-c(read.csv("trunk.csv", header = FALSE, skip = 1, sep = "\ ")[,3],
#     #          read.csv("branch/branch.csv", header = FALSE, skip = 1, sep = "\ ")[,3])
#     # 
#     # max_h<-max(tree$Z)
#     # 
#     # ohm_final<-tcorrect(ohm_tree, tree_vox, max_h, voxel = 0.02)
#     # 
#     # plot(ohm_tree$V, col = NULL)
#     # lines(ohm_tree$V, col = "black")
#     # 
#     # plot(ohm_final$V, col = NULL)
#     # lines(ohm_final$V, col = "black")
#     # 
#     # 
#     # ds[ds[,"TreeID"] == gsub("./","", f),"bole_tls"] <- sum(ohm_final$Vcor)*sg*1000
#     
#   }
#   
#   setwd("..")
# }

#run it in parallel
library(foreach)
library(doParallel)
source("R/OHM_functions_v4.R")

list = list.files("input", pattern="*.txt", full.names = TRUE)
dir.create("output/modeled")

#setup parallel backend to use many processors
cores=detectCores()
cl <- makeCluster(cores[1]-2) #not to overload your computer
registerDoParallel(cl)

df<-list[1]

foreach(df=list, .errorhandling=c('pass')) %dopar% {
  
  require(VoxR)
  require(circular)
  require(sp)
  require(spatstat)
  require(svMisc)
  require(rgl)
  source("R/OHM_functions_v4.R")
  
  tree<-NULL
  tree <- read.csv(df, header = TRUE, sep = "\ ", stringsAsFactors = FALSE)[1:3]
  
  ohm_tree<-NULL
  ohm_tree<-OHM(tree,
                slice_n_min = 10,
                buff = TRUE,
                voxel = 0.02,
                d = 10, #default is 20.
                area = TRUE,
                r_max = 0.1,
                method = "peel",
                peel_pct = 20, #default is 20%
                peel_min = 10,
                correct = TRUE)
  
  # plot(ohm_tree$z,ohm_tree$V)
  # lines(ohm_tree$z,ohm_tree$Vcor)
  
  if (!is.null(ohm_tree)) write.table(ohm_tree,paste("output/modeled/",gsub("input/","",
                                                                            gsub('.txt','',df)),"_OHM.txt", sep = ""),
                                      row.names = FALSE, col.names = FALSE,
                                      sep = "\ ")
}

#stop cluster
stopCluster(cl)

# 
# list<-list[!(list %in% gsub("_OHM","",list.files("modeled")))]
# 
# cores=detectCores()
# cl <- makeCluster(cores[1]-2) #not to overload your computer
# registerDoParallel(cl)
# 
# foreach(df=list, .errorhandling=c('pass')) %dopar% {
#   
#   require(VoxR)
#   require(circular)
#   require(sp)
#   require(spatstat)
#   require(svMisc)
#   require(rgl)
#   source("../../OHM_functions_v4.R")
#   
#   tree<-NULL
#   tree <- read.csv(df, header = TRUE, sep = "\ ", stringsAsFactors = FALSE)[1:3]
#   
#   
#   for (i in seq(5,40, by = 5)) {
#     ohm_tree<-NULL
#     ohm_tree<-OHM(tree,
#                   slice_n_min = 10,
#                   buff = TRUE,
#                   voxel = 0.02,
#                   d = i, #default is 20.
#                   area = TRUE,
#                   r_max = 0.1,
#                   method = "peel",
#                   peel_pct = 20, #default is 20%
#                   peel_min = 10,
#                   correct = TRUE)
#     
#     if(!is.null(ohm_tree)) break
#     
#   }
#   
#   
#   
#   # plot(ohm_tree$z,ohm_tree$V)
#   # lines(ohm_tree$z,ohm_tree$Vcor)
#   
#   if (!is.null(ohm_tree)) write.table(ohm_tree,paste("modeled/",gsub('.txt','',df),"_OHM.txt", sep = ""),
#                                       row.names = FALSE, col.names = FALSE,
#                                       sep = "\ ")
# }
# 
# #stop cluster
# stopCluster(cl)








m_list<-list.files("modeled")
tree_ls<-list()

# for (i in 1:length(m_list)) tree_ls[[i]]<-data.frame(id = gsub('_OHM.txt','',m_list[i]),
#                                                      V = sum(read.table(paste("modeled/",m_list[i],sep = ""), sep = "\ ")[,4]))


for (i in 1:length(m_list)) {
  
  tree<-read.table(paste("modeled/",m_list[i],sep = ""), sep = "\ ")
  tree<-tree[tree$V3>=0,]
  tree_ls[[i]]<-data.frame(id = gsub('_OHM.txt','',m_list[i]),
                           V = sum(tree$V5)
  )
  
}

zeb_ohm<-do.call(rbind,tree_ls)
colnames(zeb_ohm)<-c("ZEB","Z_VOL")
zeb_ohm$ZEB <- gsub("stem_","",zeb_ohm$ZEB)




#run it in parallel
library(foreach)
library(doParallel)

sg = 0.6 #tropical tree species?
f = list_f[2]

setwd("../..")
setwd("tam6_clean")
getwd()
list = list.files(pattern="*.txt")
df = list[4]
dir.create("modeled")
#setup parallel backend to use many processors
cores=detectCores()
cl <- makeCluster(cores[1]-2) #not to overload your computer
registerDoParallel(cl)


foreach(df=list) %dopar% {
  
  require(VoxR)
  require(circular)
  require(sp)
  require(spatstat)
  require(svMisc)
  require(rgl)
  
  source("../OHM_functions_v4.R")
  
  tree<-NULL
  tree <- read.csv(df, header = FALSE, sep = "\ ", stringsAsFactors = FALSE)[1:3]
  
  ohm_tree<-OHM(tree,
                slice_n_min = 10,
                buff = TRUE,
                voxel = 0.02,
                d = 5, #default is 20, but has errors sometimes.
                area = TRUE,
                r_max = 80,
                method = "circle",
                peel_pct = 20, #default is 20%
                peel_min = 10,
                correct = TRUE)
  
  # ohm_tree<-OHM(tree,
  #               slice_n_min = 10,
  #               buff = TRUE,
  #               voxel = 0.02,
  #               d = 5, #default is 20.
  #               area = TRUE,
  #               r_max = 0.1,
  #               method = "peel",
  #               peel_pct = 20, #default is 20%
  #               peel_min = 10,
  #               correct = TRUE)
  
  # tree_ls[[(1:length(list))[df==list]]]<-data.frame(id = gsub('.txt','',df),
  #                                                   vol = sum(ohm_tree$Vcor))
  
  write.table(ohm_tree,paste("modeled/",gsub('.txt','',df),"_OHM.txt", sep = ""),
              row.names = FALSE, col.names = FALSE,
              sep = "\ ")
}

#stop cluster
stopCluster(cl)

m_list<-list.files("modeled", pattern = ".txt")
tree_ls<-list()

for (i in 1:length(m_list)) {
  
  tree<-read.table(paste("modeled/",m_list[i],sep = ""), sep = "\ ")
  tree<-tree[tree$V3>=0,]
  tree_ls[[i]]<-data.frame(id = gsub('_OHM.txt','',m_list[i]),
                           V = sum(tree$V5)
  )
  
}


reigl_ohm<-do.call(rbind,tree_ls)
colnames(reigl_ohm)<-c("TLS","R_VOL")
reigl_ohm$TLS <- gsub("stem_","",reigl_ohm$TLS)



link_vol<-merge.data.frame(reigl_ohm,link,by = "TLS")
link_vol_all<-merge.data.frame(zeb_ohm,link_vol,by = "ZEB")

# write.csv(link_vol_all,"../zeb_riegl_comp.csv", row.names = FALSE)

link_vol_all[abs(link_vol_all$Z_VOL-link_vol_all$R_VOL)<1,]

setwd("..")

library(ggplot2)
ggplot(link_vol_all, aes(y = Z_VOL, x = R_VOL)) + 
  geom_abline(slope = 1, intercept = 0, color = "grey", linetype = 2) + 
  stat_smooth(se = FALSE, method = "lm", color = "blue") +
  geom_point(shape = 1) + 
  theme_bw() +
  # ylim(0,10) + xlim(0,10) +
  # coord_equal() + 
  scale_y_log10(limits = c(0.1,10)) + scale_x_log10(limits = c(0.1,10)) + 
  labs(x=bquote('RIEGL Volume (m'^3*')'), y=bquote('ZEB Volume (m'^3*')')) +
  theme(axis.title.x = element_text(vjust=-0.5)) +
  theme(axis.title.y = element_text(vjust=1.5)) +
  theme(plot.title = element_text(vjust=2.5)) +
  theme(legend.position="none")

summary(lm(Z_VOL~R_VOL,data = link_vol_all))
summary(lm(Z_VOL~R_VOL,data = link_vol_all[abs(link_vol_all$Z_VOL-link_vol_all$R_VOL)<1.8,]))

ggsave(paste("ZEB_RIEGL1", ".pdf", sep=""), width = 3.75, height = 3.75, units = "in")

  


ggplot(link_vol_all[abs(link_vol_all$Z_VOL-link_vol_all$R_VOL)<1,], aes(y = Z_VOL, x = R_VOL)) + geom_point() + stat_smooth(method = "lm", color = "black") + theme_bw() + geom_abline(slope = 1,intercept = 0, color = "grey") + 
  # ylim(0,4) + xlim(0,4) +
  coord_equal()

summary(lm(Z_VOL~R_VOL, link_vol_all))
summary(lm(Z_VOL~R_VOL, link_vol_all[abs(link_vol_all$Z_VOL-link_vol_all$R_VOL)<1,]))

sqrt(mean((link_vol_all$Z_VOL-link_vol_all$R_VOL)^2))/mean(link_vol_all$R_VOL)

mean((link_vol_all$Z_VOL-link_vol_all$R_VOL))/mean(link_vol_all$R_VOL)


m_list<-list.files("modeled", pattern = ".txt")
tree_ls<-list()

for (i in 1:length(m_list)) tree_ls[[i]]<-data.frame(id = gsub('_OHM.txt','',m_list[i]),
                                                     read.table(paste("modeled/",m_list[i],sep = ""), sep = "\ "))


t_vol<-do.call(rbind,tree_ls)
colnames(t_vol)<-c("id","x","y","z","V","Vcor")

library(ggplot2)
ggplot(t_vol, aes(y = Vcor, x = z, group= factor(id), color = factor(id))) + geom_line(alpha = 0.5) + theme_bw() + coord_flip() + 
  # ylim(0,0.05) + 
  xlim(0,5) + 
  theme(legend.position = "none")  

plot(cumsum(t_vol$Vcor))



comp<-merge.data.frame(reigl_ohm,zeb_ohm,by = "id")
comp<-comp[comp$V.x>0,]
comp<-comp[comp$V.y>0,]

plot(comp$V.x,comp$V.y, asp = 1)
abline(0,1, col = "blue")



setwd("..")
getwd()

sg = 0.53 # douglas fir
setwd("DougFir")
list_f = list.dirs(path = ".", recursive = FALSE)


for (f in list_f) {
  
  setwd(f)
  getwd()
  list = list.files(pattern="*.csv")
  df = list[1]
  
  for (df in list){
    
    tree<-NULL
    tree <- read.csv(df, header = TRUE, sep = "\ ", stringsAsFactors = FALSE)[1:3]
    ohm_tree<-OHM(tree, 
                  slice_n_min = 10, 
                  voxel = 0.02, 
                  d = 20, 
                  area = FALSE, 
                  r_max = 0.01, 
                  method = "peel", 
                  peel_pct = 20, 
                  peel_min = 10)
    
    ohm_tree1<-OHM(tree, 
                   slice_n_min = 10, 
                   voxel = 0.02, 
                   d = 20, 
                   area = FALSE, 
                   r_max = 0.01, 
                   method = "peel", 
                   peel_pct = 20, 
                   peel_min = 10)
    
    write.csv(ohm_tree1,"test.csv")
    
    ds[ds[,"TreeID"] == gsub("./","", f),"bole_tls"] <- sum(ohm_tree$V)*sg*1000
    
  }
  
  setwd("..")
}

library(ggplot2)

ggplot(ds, aes(x=bole_and_bark_dry_kg, y=bole_tls)) +
  theme_bw() + theme(text = element_text(size = 8.75, colour = "black"), axis.text=element_text(size=8.75)) +
  stat_smooth(method="lm", se=FALSE, size=1, color="black", linetype = 1) +
  geom_abline(intercept=0, size=1, color="grey", linetype = 2) +
  
  coord_fixed(ratio = 1, xlim = c(0,1000), ylim=c(0,1000)) +
  # geom_point(colour = "black", size = 2) +
  geom_point(aes(fill = factor(Species)), size=2, color = "black", pch=21) +
  scale_fill_manual(breaks = c("PP", "DF"), labels = c("Ponderosa Pine", "Douglass Fir"), values = c("white","#000000")) +
  labs(x="Reference Biomass (kg)", y="TLS-Derived Biomass (kg)") +
  theme(axis.title.x = element_text(vjust=-0.5)) +
  theme(axis.title.y = element_text(vjust=1.5)) +
  theme(plot.title = element_text(vjust=2.5)) +
  theme(legend.position="none")

annotate("text", x=0, y=1000, label = paste("y = ",round(t_m$coefficients[2],4),"x",round(t_m$coefficients[1],4),sep=""),hjust=0, vjust=0.5, size = 3.5) +
  annotate("text", 0,1000, label = "R^2 == .99", parse = TRUE, hjust=0, vjust=1.5, size = 3.5) +
  annotate("text", 0,1000, label = paste("RMSE = ",round(t_rmse,1)," kg"), hjust=0, vjust=3.7, size = 3.5) +
  # annotate("text", 0,450, label = paste("RMSE% = ",round(t_rmse_p,1),"%"), hjust=0, vjust=5.2, size = 2.5) +
  ggsave(paste("trunk", ".pdf", sep=""),path = file.path(wkdir, "figures"), width = 3.75, height = 3.75, units = "in")


sqrt(mean((ds$bole_and_bark_dry_kg-ds$bole_tls)^2, na.rm = TRUE))/mean(ds$bole_and_bark_dry_kg,na.rm = TRUE)




ohm_tree$r<-c((ohm_tree$A/pi)^(1/2))
ohm_tree$d<-ohm_tree$r*2

ohm_tree$z<-ohm_tree$z-min(ohm_tree$z)
i=1

for(i in 1:nrow(ohm_tree)){
  
  cylinder3d(p_start = c(as.numeric(ohm_tree[i,1:3])),
             orient = c(ohm_tree$x[i+1]-ohm_tree$x[i],ohm_tree$y[i+1]-ohm_tree$y[i],ohm_tree$z[i+1]-ohm_tree$z[i]),
             radius = ohm_tree$r[i],
             length = 0.02,         # Length of the cylinder
             order = 0,          # Branch order level of the cylinder
             n_seg = 10,         # Number of segments that draw the cylinder
             axis = F,           # Display the cylinder axis
             cylinders = T,      # Display the cylinder
             circles = F,        # Display circles at the end of the cylinder
             transparent = F) 
  
}

voxel <- 0.02

#find total height of tree
raw_z<-c(read.csv("trunk.csv", header = FALSE, skip = 1, sep = "\ ")[,3],
         read.csv("branch/branch.csv", header = FALSE, skip = 1, sep = " ")[,3])

max_h<-max(raw_z)-min(raw_z)

l_pred<-floor(max_h/voxel)-nrow(ohm_tree)

#create approximation of trunk diameeter
trunk_taper_dat<-data.frame(z = ohm_tree$z, d = ohm_tree$r, z2 = ohm_tree$z^2)
# colnames(trunk_taper_dat) <- c("height_m", "diameter_m")


tot_z_range<-seq(0,max_h,by=voxel)
trunk_taper_dat$z_bin<-round(trunk_taper_dat$z,2) #round to nearest 10cm
trunk_taper_bin<-aggregate(d~z_bin,data = trunk_taper_dat, FUN = "mean")
# trunk_taper_bin<-aggregate(diameter_m~z_bin,data = trunk_taper_dat, FUN = "z")

plot(trunk_taper_bin)

tree_vox$z_bin<-round(tree_vox$z-min(tree_vox$z),2)
trunk_taper_dat$z_bin<-trunk_taper_dat$z

w <- aggregate(n~z_bin, FUN = "sum", data = tree_vox)
trunk_taper_dat <- merge(w,trunk_taper_bin, by = "z_bin")

# f.prime <- (diff(as.numeric(trunk_taper_dat$d))/diff(seq(1,length(trunk_taper_dat$d),1)))
# 
# plot(f.prime)
# 
# d_h<-data.frame(z_bin = trunk_taper_dat$z[f.prime<quantile(f.prime,.70, na.rm = TRUE) & f.prime>quantile(f.prime,.30, na.rm = TRUE)],
#                 d = trunk_taper_dat$d[f.prime<quantile(f.prime,.70, na.rm = TRUE) & f.prime>quantile(f.prime,.30, na.rm = TRUE)])
# 

d_h<-trunk_taper_dat
d_h$z2<- d_h$z_bin^2

taper_est <- lm(d ~ z_bin + z2, weights = n, data = d_h)
# plot(taper_est)

plot(d_h$z_bin,d_h$d, pch=16, ylab = "diameter", cex.lab = 1.3, col = "red" )
# abline(taper_est, col="blue")
predictedtaper <- predict(taper_est,d_h)

#create taper threshold
max_predictedtaper <- predictedtaper+predictedtaper*.05
min_predictedtaper <- max_predictedtaper*0.75

lines(d_h$z_bin, predictedtaper, col = "blue", lwd = 3)
lines(d_h$z_bin, max_predictedtaper, col = "blue", lwd = 3)
lines(d_h$z_bin, min_predictedtaper, col = "blue", lwd = 3)

#Constrain trunk with min and max taper
ii=NULL
d_cor <- matrix(1,nrow = length(d_h$z_bin), ncol = 2)
d_cor[,2] <-d_h$d
for (ii in 1:length(d_h$z_bin)) {
  if (d_cor[ii,2]>max_predictedtaper[ii]) {
    d_cor[ii,2]<-max_predictedtaper[ii]
  }

  if (d_cor[ii,2]<min_predictedtaper[ii]) {
    d_cor[ii,2]<-min_predictedtaper[ii]
  }
}

d_cor[,1]<-d_h$z_bin

# return(d_cor)

# plot(d_cor)
# lines(d_cor, col = "blue", lwd = 3)






























  
  
# require(VoxR)
  # require(circular)
  # require(sp)
  # require(spatstat)
  # require(svMisc)
  # 
  # setwd("D:/TLS_Measurements/OHM_DF_ponderosa/")
  # source("OHM_functions_v2.R")
  # 
  # 
  # 
  # wkdir <- getwd()
  # ds<-cbind(read.csv(paste(wkdir,"\\PP_DF_DestSamp.csv",sep = ""), stringsAsFactors = FALSE)[,1:7],
  #           read.csv(paste(wkdir,"\\PP_DF_DestSamp.csv",sep = ""), stringsAsFactors = FALSE)[,20:23])
  # ds<-ds[,-5]
  # ds<-ds[,-5]
  # 
  # ds$bole_tls<-NA
  # 
  # 
  # setwd("D:/TLS_Measurements/OHM_DF_ponderosa/")
  # wkdir <- getwd()
  # setwd("Ponderosa")
  # list_f = list.dirs(path = ".", recursive = FALSE)
  # 
  # 
  # for (f in list_f) {
  #   
  #   setwd(f)
  #   
  #   
  #   #Step 3: Start the algorithm here
  #   
  #   list = list.files(pattern="*.csv")
  #   
  #   
  #   # tree <- read.csv("trunk_large_branch.csv")
  #   
  #   #Enter Thresholds:
  #   
  #   RMSE_max = 0.1
  #   
  #   #set conversion parameters
  #   # sg = 0.38 #lodgpole
  #   # sg = 0.53 # douglas fir
  #   sg = 0.37 # Ponderosa
  #   
  #   #Voxel is essentially the vertical resolution of the algorithm
  #   voxel = 0.02
  #   
  #   #height of first branching (m)
  #   br_h <- 0
  #   
  #   #Maximum radius of circle
  #   r_max = 100
  #   
  #   #Buffer in order to reduce occlusion of points
  #   d <- 20
  #   
  #   #   peel_pct = 100
  #   
  #   
  #   peel_pct <- -0.0163*d^2+1.8021*d+10.997
  #   #   peel_pct = 100-peel_pct
  #   
  #   
  #   
  #   #Remove voxelization step
  #   
  #   slice_n_min <- 10
  #   
  #   df <- list[1]
  #   
  #   
  #   ohm_tree<-OHM(tree, area = TRUE, r_max = 0.01)
  #   
  #   write.csv(ohm_tree, "tree_test.csv")
  #   
  #   ohm_tree<-OHM(tree, peel_min = 100, method = "circle")
  #   plot(ohm_tree$z,ohm_tree$V, col = "white")
  #   lines(ohm_tree$z,ohm_tree$V)
  #   ohm_tree<-OHM(tree, peel_min = 100, method = "convex")
  #   plot(ohm_tree$z,ohm_tree$V, col = "blue")
  #   ohm_tree<-OHM(tree, peel_min = 100, method = "peel")
  #   lines(ohm_tree$z,ohm_tree$V, col = "green")
  #   
  #   
  #   buf_no_vox<-buffer(tree)
  #   write.csv(buf_no_vox, "tree_test.csv")
  #   
  #   
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  vol_trunk <- colSums(area_trunk)*voxel
  bio_trunk <- vol_trunk*sg*1000
  bio_trunk <-as.data.frame(bio_trunk)
  colnames(bio_trunk) <- c("biomass_kg")
  print(bio_trunk)
  print(vol_trunk)
  
  #____________________________________________________________________________________________________________#
  #TRUNK TAPER
  
  raw_z<-c(read.csv("trunk.csv", header = FALSE, skip = 1, sep = "\ ")[,3],
           read.csv("branch/branch.csv", header = FALSE, skip = 1, sep = ",")[,3])
  
  max_h<-max(raw_z)-min(raw_z)
  l_pred<-floor(max_h/.02)-nrow(area_trunk)
  
  #create approximation of trunk diameeter
  trunk_taper_dat <- cbind(seq(0.02, max_h, by = voxel), c(2*(area_trunk/pi)^(1/2), rep(NA,l_pred)))
  trunk_taper_dat<-as.data.frame(trunk_taper_dat)
  colnames(trunk_taper_dat) <- c("height_m", "diameter_m")
  
  
  z_range<-seq(0,max_h,by=0.1)
  trunk_taper_dat$z_bin<-round(trunk_taper_dat$height_m,1) #round to nearest 10cm
  trunk_taper_bin<-aggregate(diameter_m~z_bin,data = trunk_taper_dat, FUN = "mean")
  # trunk_taper_bin<-aggregate(diameter_m~z_bin,data = trunk_taper_dat, FUN = "z")
  
  plot(trunk_taper_bin)
  
  
  
  #Model the taper with a quadratic function
  d <- trunk_taper_dat$diameter_m
  h <- trunk_taper_dat$height_m
  # h2 <- trunk_taper_dat$height_m^2
  
  tree_vox$z_bin<-round(tree_vox$z-min(tree_vox$z),2)
  trunk_taper_dat$z_bin<-trunk_taper_dat$height_m
  
  w <- aggregate(n~z_bin,FUN = "sum", data = tree_vox)
  trunk_taper_dat <- merge(w,trunk_taper_dat, by = "z_bin")
  
  f.prime <- (diff(as.numeric(trunk_taper_dat$diameter_m))/diff(seq(1,length(trunk_taper_dat$diameter_m),1)))
  
  plot(f.prime)
  
  d_h<-data.frame(z_bin = trunk_taper_dat$height_m[f.prime<quantile(f.prime,.70, na.rm = TRUE) & f.prime>quantile(f.prime,.30, na.rm = TRUE)],
                  d = trunk_taper_dat$diameter_m[f.prime<quantile(f.prime,.70, na.rm = TRUE) & f.prime>quantile(f.prime,.30, na.rm = TRUE)])
  
  d_h<-merge(d_h,w)
  
  taper_est <- lm(d ~ z_bin, weights = n, data = d_h)
  # plot(taper_est$weights)
  # taper_est <- lm(d ~ h + h2)
  
  # plot(taper_est)
  plot(d_h$z_bin,d_h$d, pch=16, ylab = "diameter", cex.lab = 1.3, col = "red" )
  abline(taper_est, col="blue")
  predictedtaper <- predict(taper_est,d_h)
  
  #create taper threshold
  
  max_predictedtaper <- predictedtaper+predictedtaper*.05
  min_predictedtaper <- max_predictedtaper*0.75
  
  lines(d_h$z_bin, predictedtaper, col = "blue", lwd = 3)
  lines(d_h$z_bin, max_predictedtaper, col = "blue", lwd = 3)
  lines(d_h$z_bin, min_predictedtaper, col = "blue", lwd = 3)
  
  #Constrain trunk with min and max taper
  ii=NULL
  d_cor <- matrix(1,nrow = length(d_h$z_bin), ncol = 2)
  for (ii in 1:length(d_h$z_bin)) {
    if (d_cor[ii,2]>max_predictedtaper[ii]) {
      d_cor[ii,2]<-max_predictedtaper[ii]
    }
    
    if (d_cor[ii,2]<min_predictedtaper[ii]) {
      d_cor[ii,2]<-min_predictedtaper[ii]
    }
  }
  
  d_cor[,1]<-d_h$z_bin
  plot(d_cor)
  lines(d_cor, col = "blue", lwd = 3)
  
  d <- d_cor[,2]
  h <- d_cor[,1]
  
  lines(h,d, col = "blue", lwd = 3)
  
  colnames(trunk_taper_dat)<-c("h","d","z_bin")
  
  trunk_pred<-data.frame(trunk_taper_dat,model = predict(taper_est,trunk_taper_dat))
  
  # plot(trunk_pred$h,trunk_pred$`predict(taper_est, trunk_taper_dat)`)
  # lines(trunk_pred$h,trunk_pred$d)
  
  trunk_pred$comb<-trunk_pred$d
  trunk_pred[is.na(trunk_pred$d),"comb"]<-trunk_pred$model[is.na(trunk_pred$d)]
  
  plot(trunk_pred$h,trunk_pred$comb)
  
  
  corrected_d <- cbind(pi*(d/2)^2, h)
  corrected_d <- as.data.frame(corrected_d)
  vol_trunk_corr <- colSums(corrected_d[1])*voxel
  bio_trunk_corr <- vol_trunk_corr*sg*1000
  bio_trunk_corr <-as.data.frame(bio_trunk_corr)
  colnames(bio_trunk_corr) <- c("biomass_kg")
  print(bio_trunk_corr)
  print(vol_trunk_corr)
  # print(time)
  # print(c(RMSE_max,r_max,voxel,buffer,peel_pct))
  
  write.table(corrected_d, 
              "filled/corrected_diameter_taper.csv",
              append = FALSE,
              sep = ",",
              row.names = FALSE,
              col.names = FALSE)
  
  #__________________________________________________________________________
  
  
  
  #     time_stats <- cbind(getwd(),time[3], nrow(tree), max(h))
  #     setwd("..")
  #     write.table(time_stats, "time.csv", sep=",", append = TRUE, col.names=FALSE)
  #                    rm(list= ls()[!(ls() %in% c('wkdir','list_f',"f"))])
  
  
  ds[ds[,"TreeID"] == gsub("./","", f),"bole_tls"]<-bio_trunk_corr
  
  
  
  
  
  
    
    
    
    
    
      
      
    
      
    
    #If RMSE threshold not achieved, use Peeling Convex Hull
    
    #   slice <- slice[c("x","y")]
    
    
    
    #do.call("rbind", sapply(df.trunk, FUN = function(i)c(z_list[i], length(xyz_fill)/3), simplify = FALSE))
    
    #Iterate through the remaining slices  
  
    
    for (i in 1:length(z_list)) {
      
      if (d>0) {
        
        slice <- subset(tree_vox, z >= z_list[i]-(d*voxel) & z <= z_list[i]+(d*voxel),
                        select=x:n)
      }
      
      if (d==0) {
        
        slice <- subset(tree_vox, z >= z_list[i]-(voxel/2) & z <= z_list[i]+(voxel/2),
                        select=x:n)
      }
      
      if (length(slice$z) < 3) {
        next
        #                        xyz_fill <- cbind(fill$x, fill$y, z_list[i])
        #                        xyz_fill_n <- cbind(xyz_fill, 0)
        #                        write.table(xyz_fill_n, 
        #                                    "filled/tree_fill_xyz.csv",
        #                                    append = TRUE,
        #                                    sep = ",",
        #                                    row.names = FALSE,
        #                                    col.names = FALSE)
        
      }  else
        
        #Fit a circle to the slice with ls means
        
        circle<-lsfit.circle(slice$x,slice$y)
      
      if (!is.null(circle)) {
        c <- circle$coefficients
        c <- as.data.frame(c)
        r <- c[1,]
        
        #Evaluate RMSE
        RMSE <- sqrt(mean((circle$radius-r)^2))
        
        x_center <- c[2,]
        y_center <- c[3,]
        
        #circle or Convex Hull (based on RMSE)
        
        if (r>=r_max) {
          pts = seq(0, 2 * pi, length.out = 1000)
          #plot(sin(pts), cos(pts), type = 'l', asp = 1) # test
          require(sp)
          xy = cbind(x_center + r * sin(pts), y_center + r * cos(pts))
          colnames(xy) <- c("x", "y")
          xy <- as.data.frame(xy)
          
          conv<-convexhull.xy(xy$x, xy$y)
          
          if (is.null(conv)) {
            
            xyz_fill_circ <- cbind(xyz_fill, 1)
            write.table(xyz_fill_circ, 
                        "filled/tree_fill_xyz.csv",
                        append = TRUE,
                        sep = ",",
                        row.names = FALSE,
                        col.names = FALSE)
            A <- area(conv)
            
            write.table(A, 
                        "filled/area_xyz.csv",
                        append = TRUE,
                        sep = ",",
                        row.names = FALSE,
                        col.names = FALSE)
            
          }
        }
      }  
      
      #If RMSE threshold not achieved, use Convex Hull
      slice <- slice[c("x","y")]
      
      # remove duplicate points so the ahull function doesn't error out
      treenodup <- lapply(slice,"[",which(!duplicated(as.matrix(as.data.frame(slice)))))
      conv <-convexhull.xy(treenodup)
      xy_bdry <- cbind(conv[[4]][[1]][[1]], conv[[4]][[1]][[2]])
      if (is.null(xy_bdry)) next
      c_xy_bdry<- SpatialPoints(xy_bdry)
      c_slice<- SpatialPoints(slice)
      
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
      
      if (is.null(conv)) {
        
        slice_n <- cbind(slice,0)
        write.table(slice_n, 
                    "filled/tree_fill_xyz.csv",
                    append = TRUE,
                    sep = ",",
                    row.names = FALSE,
                    col.names = FALSE)
        next
        
      } else if (!is.null(conv)) {
        
        mask <- as.mask(conv, eps=voxel)
        fill <- raster.xy(mask, drop = TRUE)
        xyz_fill <- cbind(fill$x, fill$y, z_list[i])
        
        xyz_fill_conv <- cbind(xyz_fill,2)
        
        write.table(xyz_fill_conv, 
                    "filled/tree_fill_xyz.csv",
                    append = TRUE,
                    sep = ",",
                    row.names = FALSE,
                    col.names = FALSE)
        
        A <- area(conv)
        
        write.table(A, 
                    "filled/area_xyz.csv",
                    append = TRUE,
                    sep = ",",
                    row.names = FALSE,
                    col.names = FALSE)
      }
    }
    # cat(i, fill=TRUE)
    # }
    
    time <- proc.time() - ptm
    
    #calculate biomass from main trunk section
    
    area_trunk <- read.csv("filled/area_xyz.csv")
    vol_trunk <- colSums(area_trunk)*voxel
    bio_trunk <- vol_trunk*sg*1000
    bio_trunk <-as.data.frame(bio_trunk)
    colnames(bio_trunk) <- c("biomass_kg")
    print(bio_trunk)
    print(vol_trunk)
    
    #____________________________________________________________________________________________________________#
    #TRUNK TAPER
    
    #create approximation of trunk diameeter
    trunk_taper_dat <- cbind(seq(0.02, nrow(area_trunk)*voxel, by = voxel), 2*(area_trunk/pi)^(1/2))
    trunk_taper_dat<-as.data.frame(trunk_taper_dat)
    colnames(trunk_taper_dat) <- c("height_m", "diameter_m")
    
    #Model the taper with a quadratic function
    d <- trunk_taper_dat[,2]
    h <- trunk_taper_dat[,1]
    h2 <- trunk_taper_dat[,1]^2
    
    taper_est <- lm(d ~ h)
    # taper_est <- lm(d ~ h + h2)
    
    # plot(taper_est)
    plot(h, d, pch=16, ylab = "diameter", cex.lab = 1.3, col = "red" )
    abline(taper_est, col="blue")
    predictedtaper <- predict(taper_est,list(h, h2))
    
    #create taper threshold
    
    max_predictedtaper <- predictedtaper+predictedtaper*.05
    min_predictedtaper <- max_predictedtaper*0.75
    
    lines(h, predictedtaper, col = "blue", lwd = 3)
    lines(h, max_predictedtaper, col = "blue", lwd = 3)
    lines(h, min_predictedtaper, col = "blue", lwd = 3)
    
    #Constrain trunk with min and max taper
    ii=NULL
    for (ii in 1:length(d)) {
      if (d[ii]>max_predictedtaper[ii]) {
        d[ii]<-max_predictedtaper[ii]
      }
      
      if (d[ii]<min_predictedtaper[ii]) {
        d[ii]<-min_predictedtaper[ii]
      }
    }
    
    lines(h, d, col = "blue", lwd = 3)
    
    
    corrected_d <- cbind(pi*(d/2)^2, h)
    corrected_d <- as.data.frame(corrected_d)
    vol_trunk_corr <- colSums(corrected_d[1])*voxel
    bio_trunk_corr <- vol_trunk_corr*sg*1000
    bio_trunk_corr <-as.data.frame(bio_trunk_corr)
    colnames(bio_trunk_corr) <- c("biomass_kg")
    print(bio_trunk_corr)
    print(vol_trunk_corr)
    # print(time)
    # print(c(RMSE_max,r_max,voxel,buffer,peel_pct))
    
    write.table(corrected_d, 
                "filled/corrected_diameter_taper.csv",
                append = FALSE,
                sep = ",",
                row.names = FALSE,
                col.names = FALSE)
    
    #__________________________________________________________________________
    
    
    
    #     time_stats <- cbind(getwd(),time[3], nrow(tree), max(h))
    #     setwd("..")
    #     write.table(time_stats, "time.csv", sep=",", append = TRUE, col.names=FALSE)
    #                    rm(list= ls()[!(ls() %in% c('wkdir','list_f',"f"))])
    
    
    ds[ds[,"TreeID"] == gsub("./","", f),"bole_tls"]<-bio_trunk_corr
    
  }
  