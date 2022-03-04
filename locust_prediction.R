setwd("C:......dissertation/1985_2019 DL complete")
# set memory limit 
memory.limit(9000000)
library(rgee)
library(mapview)  
library(mapedit) # (OPTIONAL) Interactive editing of vector data
library(raster)  # Manipulate raster data
library(scales)  # Scale functions for visualization
library(cptcity)  # cptcity color gradients!
library(tmap)    # Thematic Map Visualization <3
# add necessary libraries 
library(sf)
library(rgdal)
library(sp)
library(maptools)
library(adehabitatHR)
library("readxl")
library(ggplot2)
library(cowplot)
library(randomForest)
library(tidyverse)
library(dismo)
library(ncdf4)
library(InformationValue)
library(car)
library(maxnet)
library(spatstat)
library(doParallel)
library("ranger")
library(pROC)
library(rasterize)
library(pscl)
library(rgeos)
library(mlr)
# set spatial extent for analysis 
aoi_boundary_HARV <- st_read(
  "....../1985_2019 DL complete/bounds4.shp")
extent.new <- extent(21.8, 55.6, -1.6, 33.7)   

# create a bird data object
bird = read.csv("final_bird_data_selected.csv")
nrow(bird)



#locust data
loc <- read_excel("all.xls")
loc[2,]
#loc = read.csv("allb.xlsx")
loc
#create a data frame 
bird_ltlong = data.frame(x=bird$decimalLongitude, y= bird$decimalLatitude)
loc_ltlong = data.frame(x=loc$xcoord, y= loc$ycoord)
test_ltlong = data.frame(x=test_data$xcoord, y= test_data$ycoord)
crs.latlong <-crs("+init=epsg:4326")

# create spatial points 
bird.sp<-SpatialPoints(bird_ltlong,proj4string = crs.latlong)
loc.sp<-SpatialPoints(loc_ltlong,proj4string = crs.latlong)

#plot and test 
loc.sp[1]
plot(loc.sp, add = TRUE)
test.sp<-SpatialPoints(test_ltlong,proj4string = crs.latlong)

tespp = ppp(bird$decimalLongitude,bird$decimalLatitude)
tespp

#bird_data[,5]
bird_data<-SpatialPointsDataFrame(coords=bird_ltlong,data=bird,proj4string = crs.latlong)
v = as.ppp(aoi_boundary_HARV)
plot(bird_data)
head(bird_data)
head(bird_data)

# remove data which does not have individual counts 
bird_data = subset(bird_data, !is.na(bird_data$individualCount))


# Build raster surface based on bird data 
dcc = data.frame()
dd = data.frame()
for (i in 1:5){
  bv = unique (year[i])
  locb <- subset(loc_data, loc_data$year==bv)
  birdb <- subset(bird, bird$year==bv) 
  for (x in 1:12){
    y = unique(loc_data$Month[x])
    crs.latlong<-crs("+init=epsg:4326")
    locc <- subset(locb, locb$Month==y)
    if (nrow(locc)>0){
      birdc <- subset(birdb, birdb$month==y)
      r <- raster(aoi_boundary_HARV)
      bv = rasterize(birdc[21:20], r,field = birdc[12],sum, crs= "+proj=longlat +datum=WGS84")
      adindann<-crs("+init=epsg:20137")
      crs(locc) = crs(adindann)
      crs(bv)= crs(adindann)
      buffer_tt = gBuffer(locc, width = 5,byid = TRUE)
      uu = extract(bv,buffer_tt)
      jj = unlist (uu)
      kk = jj[1:nrow(locc)]
      gu = as.data.frame(kk)
      dcc =cbind(as.data.frame(locc),gu)
      dd = rbind(dd,dcc)
      
    }
  }
}

# create an adindan projection object
canisProj<-crs("+init=epsg:20137")
Canis<-spTransform(loc.sp,canisProj)
# project bird surface data to adindan and test 
tetstb = spTransform(bv,canisProj)
plot(loc_data)
plot (bv)
Canis
crs(bv)= crs(Canis)
bv
loc_data
loc.sp
crs(loc_data)= crs(Canis)
loc_data
crs(loc.sp) = crs(bv)
loc.sp
plot (bv)
bv
#change surface to raster data and store in computer
bird_datab = subset(bird_data, !is.na(bird_data$individualCount))
r <- raster(aoi_boundary_HARV)
bv = rasterize(bird_datab[21:20], r,field = bird_datab[12], crs= "+proj=longlat +datum=WGS84")

bird_datab[21:20]

bird_datab[12]

writeRaster(bv, filename= 'tiff',format="GTiff", overwrite=TRUE)


year = c(2013,2014,2015,2016,2017)

month = c(1,2,3,4,5,6,7,8,9,10,11,12)
unique(year)
dat <- data.frame()
dat
#locb <- subset(loc, loc$year==2014)
# create a bird kernel data 

for (i in 1:5){
  bv = unique (year[i])
  locb <- subset(loc, loc$year==bv)
  birdb <- subset(bird, bird$year==bv)
  
  for (x in 1:12){
    y = unique(month[x])
    crs.latlong<-crs("+init=epsg:4326")
    locc <- subset(locb, locb$Month==y)
    birdc <- subset(birdb, birdb$month==y)
    bird_ltlongc = data.frame(x=birdc$decimalLongitude, y= birdc$decimalLatitude)
    bird_datac<-SpatialPointsDataFrame(coords=bird_ltlongc,data=birdc,proj4string = crs.latlong)
    if(length(unique(bird_datac$gbifID))>5){
      bird_kernel <- kernelUD(bird_datac[,5], h = "href",kern ="bivnorm")
    }
  }
} 

# save data 
  head(bird)
  plot(bird)
  bird[,2]
  head (bird)
  bird_ltlongc = data.frame(x=birdc$decimalLongitude, y= birdc$decimalLatitude)
  bird_datac<-SpatialPointsDataFrame(coords=bird_ltlongc,data=birdc,proj4string = crs.latlong)
  bird_kernel <- kernelUD(bird_datac[,7], h = "href",kern ="bivnorm")
  r<-estUDm2spixdf(bird_kernel)
  rbird<-raster(r)
  crs(rbird) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  print(nrow(birdc))
  loc
  warnings()
  print (output)

# create empty dataframe
j = data.frame()

rastlist <- list.files(path = "C:/Users/JehoidaGM/Documents/dissertation/1985_2019 DL complete", pattern='.tif$', all.files=TRUE, full.names=FALSE)
rastlist

# store every data that extracted from google earth engine in a data frame (see the other documentation)
j= cbind(rastlist)
j

# create empty data frames 
df = data.frame()
dfb = data.frame()
dfc= data.frame()
dfd = data.frame()
dfe = data.frame()
dfg = data.frame()
dfh = data.frame()
nrow(test_f)
loc
# Extract raster vales from kernel
dates[1]
for (i in 1:length(loc_data) ){
  sub = subset(loc_data,loc_data$fid == loc_data$fid[i])
  loc_date = sub$STARTDATE_m3
  x =".tif"
  im_f = paste(loc_date,x,sep="")
  raster_r = stack(im_f)
  eA= extract(raster_r,sub)
  df = rbind(df,eA)
  print (df)
  
}
# extract data for absence points 
for (i in 1:1514) {
  bv = test[i,]
  vv <- paste0("", bv, "")
  subb = subset(loc_data,loc_data$STARTDATE_m3 == vv)
  sub2 = subset(loc_data,loc_data$STARTDATE_m == loc_data$STARTDATE_m[i])
  x =".tif"
  im_f = paste(bv,x,sep="")
  raster_r = brick(im_f)
  set.seed(11)
  back.xy <- randomPoints(raster_r, n=nrow(subb),p=nrow(subb),ext = extent(aoi_boundary_HARV))
  back<-SpatialPoints(back.xy,crs(loc_data))
  eAb<-extract(raster_r,back)
  dfc= rbind(dfc,eAb)
  dfd = rbind(dfd,as.data.frame(subb$STARTDATE_m3))
  dfh = rbind(dfh,as.data.frame(subb$fid))
  dfg = rbind(dfg,as.data.frame(back.xy))
  dfe= cbind(dfc,dfd,dfg,dfh)
  print (dfe)
  
}

df = function (a,b){
  a = subset(a, a$LOCPRESENT==1||a$CONFIRMATN==1||a$REPRELIAB==1)
  #col = c(5,1,158,159,160,161,162,163)
  col = c(155,149,158,159,160,161,164,165)
  a = a[,col]
  a = cbind (a, pres = 1,presb = "yes")
  n <- 2868
  nr <- nrow(a)
  ty = split(a, rep(1:ceiling(nr/n), each=n, length.out=nr))
  ttt = cbind(as.data.frame(ty[1]),as.data.frame(ty[2]),as.data.frame(ty[3]),as.data.frame(ty[4]),as.data.frame(ty[5]),as.data.frame(ty[6]))
  colb= c(3,2,4,5,6,7,8,9)
  b = b[,colb]
  b = cbind(b,pres = 0, presb = "no")
  nrb <- nrow(b)
  tyb = split(b, rep(1:ceiling(nrb/n), each=n, length.out=nrb))
  tttb = cbind(as.data.frame(tyb[1]),as.data.frame(tyb[2]),as.data.frame(tyb[3]),as.data.frame(tyb[4]),as.data.frame(tyb[5]),as.data.frame(tyb[6]))
  namesp = names (ttt)
  names(tttb)= c(namesp)
  final = rbind(ttt,tttb)
  return (final)
}
dfb = function (a,b){
  a = subset(a, a$LOCPRESENT==1||a$CONFIRMATN==1||a$REPRELIAB==1)
  #col = c(5,1,158,159,160,161,162,163)
  col = c(155,149,158,159,160,161,164,165)
  a = a[,col]
  a = cbind (a, pres = 1,presb = "yes")
  n <- 2868
  nr <- nrow(a)
  ty = split(a, rep(1:ceiling(nr/n), each=n, length.out=nr))
  ttt = cbind(as.data.frame(ty[1]),as.data.frame(ty[2]),as.data.frame(ty[3]),as.data.frame(ty[4]),as.data.frame(ty[5]),as.data.frame(ty[6]),as.data.frame(ty[7]),as.data.frame(ty[8]),as.data.frame(ty[9]),as.data.frame(ty[10]),as.data.frame(ty[11]),as.data.frame(ty[12]))
  colb= c(3,2,4,5,6,7,8,9)
  b = b[,colb]
  b = cbind(b,pres = 0, presb = "no")
  nrb <- nrow(b)
  tyb = split(b, rep(1:ceiling(nrb/n), each=n, length.out=nrb))
  tttb = cbind(as.data.frame(tyb[1]),as.data.frame(tyb[2]),as.data.frame(tyb[3]),as.data.frame(tyb[4]),as.data.frame(tyb[5]),as.data.frame(tyb[6]),as.data.frame(tyb[7]),as.data.frame(tyb[8]),as.data.frame(tyb[9]),as.data.frame(tyb[10]),as.data.frame(tyb[11]),as.data.frame(tyb[12]))
  namesp = names (ttt)
  names(tttb)= c(namesp)
  final = rbind(ttt,tttb)
  return (final)
}





#random forests
# build data
neww = read.csv('twokmb.csv')
nrow(neww)
sixthday_max = df(read_excel("16_day_max.xls"), read_excel("16_day_max_pa.xls"))
sixthday_mean = df(read_excel("16_day_mean.xls"), read_excel("16_day_mean_pa.xls"))
sixthday_min = df(read_excel("16_day_min.xls"), read_excel("16_day_min_pa.xls"))

eighthday_max = dfb(read_excel("8_day_max_finalf.xls"), read_excel("8_day_max_finalfpa.xls"))
eighthday_mean = dfb(read_excel("8_day_mean.xls"), read_excel("8_day_mean_pa.xls"))
eighthday_min = dfb(read_excel("8_day_min.xls"), read_excel("8_day_min_pa.xls"))
nrow(eighthday_mean)
# 8 day ssm
cc = c(7,8,3,10,13,23,33,43,53,63,73)
# 8 day susm
cc = c(7,8,5,10,15,25,35,45,55,65,75,85,95,105,115)
#susm and ssm
cc = c(7,8,3,10,13,23,33,43,53,63,73,5,15,25,35,45,55,65,75)
#ssmanamoly
cc = c(7,8,3,4,10,13,14,23,24,33,34,43,44,53,54,63,64,73,74)
#susmanamoly
cc = c(7,8,5,6,10,15,16,25,26,35,36,45,46,55,56,65,66,75,76)
#max
#susmanamoly
cc = c(7,8,5,6,10,15,16,25,26,35,36,45,46,55,56,65,66,75,76,3,4,13,14,23,24,33,34,43,44,53,54,63,64,73,74)
length(cc)

#16 day
cc = c(7,8,3,10,13,23,33,43,53)
# 8 day susm
cc = c(7,8,5,10,15,25,35,45,55)
#susm and ssm
cc = c(7,8,3,10,13,23,33,43,53,5,15,25,35,45,55)
#ssmanamoly
cc = c(7,8,3,4,10,13,14,23,24,33,34,43,44,53,54)
#susmanamoly
cc = c(7,8,5,6,10,15,16,25,26,35,36,45,46,55,56)
#max
#susmanamoly
cc = c(7,8,3,4,10,13,14,23,24,33,34,43,44,53,54,5,6,15,16,25,26,35,36,45,46,55,56)







eg_max = eighthday_max[,cc]
#eg_max = sixthday_max[,cc]
eg_max = data.frame(eg_max,log(neww$kk))
#eg_max = sixthday_mean[,cc]
#eg_max = sixthday_min[,cc]
#eg_max = eighthday_max[,cc]
#mean
#eg_max = eighthday_mean[,cc]
#min
#eg_max = eighthday_min[,cc]


oo =eg_max[complete.cases(eg_max), ]
#datab = dplyr::select(oo,-X1.X,-X1.Y )
#datab = dplyr::select(oo,-X1.X,-X1.Y )
drops <- c("X1.X","X1.Y")
datab = oo[ , !(names(oo) %in% drops)]

tail(datab)
coords = oo[, c("X1.X", "X1.Y","neww$kk")]
coords = oo[, c("X1.X", "X1.Y")]

#rf
modelbv <- ranger(X1.presb ~., probability=TRUE, num.trees= 500,oob.error = TRUE, data=datab)
b = as.data.frame(modelbv[1])
c = cbind(b,oo$X1.pres)
names(c) = c("a","b","c")
roc.mock <- roc(c$c, as.numeric(c$a))
roc.mock[9]

drops <- c("X1.X","X1.Y","X1.presb","neww$kk")
datab = oo[ , !(names(oo) %in% drops)]
x = rowMeans(datac, na.rm = FALSE, dims = 1)
#x = data.frame((datac$X1.susm_max-datac$X2.susm_max),(datac$X2.susm_max-datac$X3.susm_max),(datac$X3.susm_max-datac$X4.susm_max),(datac$X4.susm_max-datac$X5.susm_max),(datac$X5.susm_max-datac$X6.susm_max))
datad = cbind(datab,x)
datae= data.frame(datad$X1.presb,datad$x,oo$`neww$kk`)
names (datab)= c('a','b','c','d','e','f','g','h')
drops <- c("a","c",'d','e','h','f')
datac = datab[ , !(names(datab) %in% drops)]
str(datab)
datac = data.frame(log(datab$X1.susm_max),log(datab$X2.susm_max),log(datab$X3.susm_max),log(datab$X4.susm_max),log(datab$X5.susm_max),log(datab$X5.susm_max),log(datab$X6.susm_max),log(datab$X7.susm_max),log(datab$X8.susm_max),log(datab$X9.susm_max),log(datab$X10.susm_max),log(datab$X11.susm_max),log(datab$X12.susm_max),datab$X1.presb,datab$log.neww.kk.)
task = makeClassifTask(data = datac, target = "datab.X1.presb",
                       positive = "yes", coordinates = coords)

#listLearners(task, warn.missing.packages = FALSE) %>%
#dplyr::select(class, name, short.name, package) %>%
#head()
#classif.randomForest
#install.packages("XML", type = "binary")
lrn = makeLearner(cl = "classif.randomForest",
                  
                  predict.type = "prob",
                  fix.factors.prediction = TRUE)

#support vector
lrn_ksvm = makeLearner("classif.ksvm",
                       predict.type = "prob",
                       fix.factors.prediction = TRUE,
                       kernel = "rbfdot")

perf_level = makeResampleDesc(method = "SpRepCV", folds = 5, reps = 10)
# five spatially disjoint partitions
tune_level = makeResampleDesc("SpCV", iters = 5)
# use 50 randomly selected hyperparameters
ctrl = makeTuneControlRandom(maxit = 50)

paramsRF <- makeParamSet(
  makeIntegerParam("mtry",lower = 1,upper = 7),
  makeIntegerParam("nodesize",lower = 1,upper = 13)
)

ctrl = makeTuneControlRandom(maxit = 10L)
tuneRF = tuneParams (learner = lrn,
                     task=task,
                     resampling = perf_level,
                     par.set = paramsRF,
                     control = ctrl,
                     measures = mlr::auc,
                     show.info = TRUE)

paramsSVM = makeParamSet(
  makeNumericParam("C", lower = -12, upper = 15, trafo = function(x) 2^x),
  makeNumericParam("sigma", lower = -15, upper = 6, trafo = function(x) 2^x))
wrapped_lrn_ksvm = makeTuneWrapper(learner = lrn_ksvm,
                                   resampling = perf_level,
                                   par.set = paramsSVM,
                                   control = ctrl,
                                   show.info = TRUE,
                                   measures = mlr::auc)


library(parallelMap)
if (Sys.info()["sysname"] %in% c("Linux", "Darwin")) {
  parallelStart(mode = "multicore",
                # parallelize the hyperparameter tuning level
                level = "mlr.tuneParams",
                # just use half of the available cores
                cpus = round(parallel::detectCores() / 2),
                mc.set.seed = TRUE)
}

if (Sys.info()["sysname"] == "Windows") {
  parallelStartSocket(level = "mlr.tuneParams",
                      cpus =  round(parallel::detectCores() / 2))
}

set.seed(12345)
result = mlr::resample(learner = wrapped_lrn_ksvm,
                       task = task,
                       resampling = perf_level,
                       extract = getTuneResult,
                       measures = mlr::auc)


# stop parallelization
parallelStop()
# save your result, e.g.:
# saveRDS(result, "svm_sp_sp_rbf_50it.rds")



tuneSVM = tuneParams(learner = lrn_ksvm,
                     task=task,
                     resampling = perf_level,
                     par.set = paramsSVM,
                     control = ctrl,
                     measures = mlr::auc,
                     show.info = TRUE)


sp_cvBinomial = resample(learner = lrn, task =task,
                         resampling = perf_level,
                         measures = mlr::auc)
v = as.data.frame(sp_cvBinomial$measures.test$auc)
v = subset(v,!is.na(v$`sp_cvBinomial$measures.test$auc`))

mean(v$`sp_cvBinomial$measures.test$auc`)
# 8 day min









b = rf1(pres,absence)
b
secd = pres[as.integer((nrow(pres))/2):as.integer((nrow(pres))*1.5),]
tail(secd)
nrow(absenceb)
absenceb = absence[1:as.integer((nrow(absence))/6),]
dfa = as.integer((nrow(pres))*1.5)
dfa
nrow(pres)
#max-ent
maxnet.ppm <- maxnet(final2$pres,final2[,5:6],maxnet.formula(final2$pres,final2[,5:6],classes="lq"))
summary(maxnet.ppm)
maxeval<-evaluate(p=bradypus, a=back, maxnet.ppm, env)
maxnet.ppm <- maxnet(test_f$GHPARSML,test_f[,1:2],maxnet.formula(test_f$GHPARSML,test_f[,1:2],classes="lq"))
for (i in 1:length(unique(final2$STARTDATE_m3)) ){
  loc_date = unique(final2$STARTDATE_m3)
  subb = subset(final2,final2$STARTDATE_m3 %in% loc_date)
  pp = subset(subb,subb$pres==1)
  pp = pp[,3:4]
  aa = subset(subb,subb$pres==0)
  aa = aa[,3:4]
  x =".tif"
  im_f = paste(loc_date,x,sep="")[i]
  raster_r = brick(im_f)
  maxnet.ppm <- maxnet(subb$pres,subb[,5:6],maxnet.formula(subb$pres,subb[,5:6],classes="lq"))
  maxeval<-evaluate(p=pp, a=aa, maxnet.ppm, raster_r)
  maxeval
  
}
vif(maxeval)
plot (maxeval,'ROC')
raster_r = brick(im_f)
maxnet.ppm <- maxnet(subb$pres,subb[,5:6],maxnet.formula(subb$pres,subb[,5:6],classes="lq"))
maxeval<-evaluate(p=subset(subb,subb$pres==1), a=subset(subb,subb$pres==0), maxnet.ppm, raster_r)
maxeval
final2$pres==3
warnings()
warnings()
unique(final2$STARTDATE)[1018]
date_new = as.Date(final2[,1],format = "%B %d %Y")
date_new
dates <- c("05/27/84", "07/07/05")

monthhs = c(1,2,3,4,5,6,7,8,9,10,11,12)
for (i in 1:length(monthhs)){
  x =".tif"
  im_f = paste(monthhs[i],x,sep="")
  raster_r = brick(im_f)
  
}

raster_r = stack(im_f)
warnings()
length(final)
length(unique(final2$STARTDATE))
maxnet.ppm <- maxnet(test_f$GHPARSML,test_f[,1:2],maxnet.formula(test_f$GHPARSML,test_f[,1:2],classes="lq"))



