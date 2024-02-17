#Machine Learning Applications for Fish Telemetry Data
#GLATOS 2024

#Jake Brownscombe & Paul Bzonek
#contact: jakebrownscombe@gmail.com


#Working here with processed telemetry data, aggregated to daily species - location level. 
#for some examples of processing steps and considerations prior to this, check out:
#https://github.com/jakebrownscombe/Acoustic_Telemetry



#packages
packages_to_install <- c("dplyr","tidyr","data.table","ggplot2","ggmap","tmap","patchwork","rpart","partykit",
                         "randomForest","rfPermute","gbm","caret","pdp","iml","raster","sf")

# Loop through each package
for (package_name in packages_to_install) {
  # Check if the package is installed
  if (!requireNamespace(package_name, quietly = TRUE)) {
    # If not installed, install the package
    install.packages(package_name)
  }
  library(package_name, character.only = TRUE)
}

theme_set(theme_classic()) #ggplot theme




#Load Data, Plot ----

#Largemouth Bass detection data from Toronto Harbour in 2013. Courtesy of Jon Midwood, Sue Doka, Steve Cooke, and others. 
#A few related publications:

#Midwood, J. D., Rous, A. M., Doka, S. E., & Cooke, S. J. (2019). 
#Acoustic telemetry in Toronto Harbour: assessing residency, habitat selection, and 
#within-harbour movements of fishes over a five-year period. 
#Fisheries and Oceans Canada. Can. Tech. Rep. Fish. Aquat. Sci. 3331: xx + 174 p.

#Brownscombe, J. W., Midwood, J. D., & Cooke, S. J. (2021). 
#Modeling fish habitat: model tuning, fit metrics, and applications. 
#Aquatic Sciences, 83, 1-14.

#Brownscombe, J. W., Midwood, J. D., Doka, S. E., & Cooke, S. J. (2023). 
#Telemetry-based spatial–temporal fish habitat models for fishes in an urban freshwater harbour. 
#Hydrobiologia, 850(8), 1779-1800.

#detections (Largemouth Bass only), and receiver node locations:
dets <- readRDS("data/LMB.dets.rds")
nodes <- readRDS("data/nodes.rds")
nodes.sf <- st_as_sf(nodes, coords = c("lon", "lat"), crs = "+proj=longlat +datum=WGS84 +no_defs")
nodes.sf

tmap_mode('view')
tm_shape(nodes.sf %>% dplyr::select(depth, dets)) +
  tm_dots(col="depth", size="dets", alpha=0.8) +
  tm_shape(nodes.sf %>% dplyr::select(SAV, dets)) +
  tm_dots(col="SAV", size="dets", alpha=0.8) + 
  tm_shape(nodes.sf %>% dplyr::select(exposure, dets)) +
  tm_dots(col="exposure", size="dets", alpha=0.8) + 
  tmap_options(basemaps = 'Esri.WorldImagery')


#habitat rasters:
hab.ras <- readRDS("data/habitat.rasters.rds")
plot(hab.ras) #will use these for continuous spatial prediction below 


  
  
#Detection plots 
TOmap <- readRDS("data/TO_satellite.rds")
ggmap(TOmap, extent='normal')+
  scale_x_continuous(limits=c(min(nodes$lon)-0.01, max(nodes$lon)+0.01))+
  scale_y_continuous(limits=c(min(nodes$lat)-0.01, max(nodes$lat)+0.01))+
  ylab("Latitude") +
  xlab("Longitude")+
  geom_point(data=dets %>% group_by(node) %>% 
               dplyr::summarise(lat=mean(lat),lon=mean(lon),dets=sum(dets), IDcount=mean(IDcount)), 
             aes(x=lon, y=lat, size=dets, fill=IDcount), col="yellow", pch=21)+
  scale_fill_viridis_c(option="plasma")+
  
ggplot(dets, aes(day, dets, col=IDcount))+geom_point()+scale_color_viridis_c(option="plasma")
#










#Training, Test data - Designs ----

#for sake of computation time, we'll look at only 2013 data 
dets$nrow <- 1:nrow(dets)
dets.sub <- dets %>% filter(day>"2012-12-31" & day<="2013-12-31")

#common to select 70:30 train:test data 
set.seed(1987)
train <- dets.sub %>% dplyr::sample_frac(0.7)
test <- dets.sub[!(dets.sub$nrow %in% train$nrow),]

#plot datasets:
ggplot(train, aes(day, dets))+geom_point()+
  geom_point(data=test, aes(day, dets), col="red")


tmap_mode('view')
tm_shape(train %>% group_by(node) %>% summarise(lat=mean(lat), lon=mean(lon)) %>% 
           st_as_sf(coords = c("lon", "lat"), crs = "+proj=longlat +datum=WGS84 +no_defs")) +
  tm_dots(col="black", alpha=0.8, size=0.5) +
tm_shape(test %>% group_by(node) %>% summarise(lat=mean(lat), lon=mean(lon)) %>% 
           st_as_sf(coords = c("lon", "lat"), crs = "+proj=longlat +datum=WGS84 +no_defs")) +
  tm_dots(col="red", alpha=0.8, size=0.2)  

#Highlights issue of 'data leak' - models will know something about the data because of 
#spatial/temporal correlation, so not true test of ability to predict response in test data. 



#one option to help address is to select independent sites
#select nodes in node dataset:
set.seed(1987)
nodes.train <- nodes %>% sample_frac(0.7)
nodes.test <- nodes[!(nodes$Station.Group %in% nodes.train$Station.Group),]

#grab detections from these nodes:
train <- dets.sub %>% filter(node %in% nodes.train$Station.Group)
test<- dets.sub %>% filter(node %in% nodes.test$Station.Group)

#plot selected
tm_shape(train %>% group_by(node) %>% summarise(lat=mean(lat), lon=mean(lon)) %>% 
           st_as_sf(coords = c("lon", "lat"), crs = "+proj=longlat +datum=WGS84 +no_defs")) +
  tm_dots(col="blue", alpha=0.8, size=0.2) +
  tm_shape(test %>% group_by(node) %>% summarise(lat=mean(lat), lon=mean(lon)) %>% 
             st_as_sf(coords = c("lon", "lat"), crs = "+proj=longlat +datum=WGS84 +no_defs")) +
  tm_dots(col="red", alpha=0.8, size=0.2)  

nrow(test)/nrow(train) #not exact split here, may not matter hugely if you have a lot of data.

#many options for sub-setting to deal with spatial-temporal auto-correlation. For example,
#could use acf to assess spatial and temporal distances of correlation, and subset sites
#at least those distances apart. Will not go into this here. 








#one additional sub-setting technique - k-fold cross validation. 
#useful for large data sets, more comprehensive assessment of fit

dets.kfold<-dets.sub[sample(nrow(dets.sub)),]
folds <- cut(seq(1,nrow(dets.kfold)),breaks=10,labels=FALSE) #10 folds here, can alter 
dets.kfold$fold <- folds
ggplot(dets.kfold, aes(day, dets, col=as.factor(fold)))+geom_point()
#have to model each fold, predict onto remaining data x # of folds 















#Basic Tree-Based Models ----
#Also called CART - Classification and Regression Trees
#pros - intuitive approach and outputs
#cons - tends to over fit data, not as accurate as more advanced models 

#set up data - can use full dataset here, not too computationally intensive
str(dets) #mix of factor and numeric predictors

#model formula:
z <- formula(pres~season+depth+SAV)

#A few different packages for CART, we'll try rpart here:
library(rpart)
rpart <- rpart(formula=z, method="class", control=(minsplit=100), cp=0.0001, data=dets)


printcp(rpart) # display the results 
plotcp(rpart) # visualize cross-validation results 
summary(rpart) # detailed summary of splits


# plot tree 
plot(rpart, uniform=TRUE, 
     main="Largemouth Bass Presence/Absence")
text(rpart, use.n=TRUE, all=TRUE, cex=.8)

#  Greater numbers of splits improves predictive performance to a point, but increased complexity 
# increases the chances of model overfitting, which reduces predictive performance outside of the dataset
# (we'll get to that soon) and also makes the model hard to interpret

#prune tree
printcp(rpart) 
prpart<- rpart::prune(rpart, cp=0.0143605)

# plot the pruned tree 
plot(prpart, uniform=TRUE, 
     main="Pruned Largemouth Bass Presence/Absence")
text(prpart, use.n=TRUE, all=TRUE, cex=.8)
summary(prpart)


#fit to training data
pred <- predict(prpart, dets, type="class")

library(caret) #useful package for making predictions with all types of these models
confusionMatrix(pred, dets$pres, positive="1")  
#imbalanced predictions, getting presences wrong a lot. That's not even in test data. 
#will address below under model weighting 

  
  
#Conditional Inference Trees
library(partykit)
CIT <- ctree(formula=z, data = dets)
print(CIT)
plot(CIT)
#too complex to be useful in this case, but can be insightful in some datasets. 




#Random Forests ----

#stick with basic train/test here
str(dets)
dets.sub$nrow <- 1:nrow(dets.sub)
dets.sub$day.n <- as.numeric(dets.sub$day)
set.seed(1987)
train <- dets.sub %>% sample_frac(0.1) #using 10% for train here for quick fitting 
test <- dets.sub[!(dets.sub$nrow %in% train$nrow),]


#model formula
z1 <- formula(pres~season+SAV+depth+exposure)

#fit RF
set.seed(1987)
Bass.Forest <- randomForest(formula=z1, data = train, replace=FALSE, na.action=na.omit,
                          importance=TRUE, classwt=c(1,1), do.trace=1000, ntree=1000) # number of trees important, and classwt 
print(Bass.Forest)


#model fit 
RF.train.pred <- predict(Bass.Forest, train, type="response")
caret::confusionMatrix(RF.train.pred, train$pres, positive="1")

#test data
RF.test.pred <- predict(Bass.Forest, test, type="response")
caret::confusionMatrix(RF.test.pred, test$pres, positive="1")

#check out for interpretation of model fit (also pdf in resources folder)
#https://link.springer.com/article/10.1007/s00027-021-00797-5

#will look more at model fit and residuals below 






#Variable importance
Bass.varIMP <- data.frame(importance(Bass.Forest))
Bass.varIMP$predictor <- rownames(Bass.varIMP)
head(Bass.varIMP)

Bass.imp <- ggplot(data=Bass.varIMP, aes(x=0, xend=MeanDecreaseAccuracy, 
                                               y=reorder(predictor, MeanDecreaseAccuracy), yend=reorder(predictor, MeanDecreaseAccuracy)))+
  geom_segment()+
  geom_point(data=Bass.varIMP, aes(MeanDecreaseAccuracy, reorder(predictor, MeanDecreaseAccuracy)),
             pch=21, fill="#00BC59", col="black", size=3)+
  theme_bw()+xlab("Mean Decrease Accuracy")+ylab("Predictor")+ggtitle("Largemouth Bass Habitat")
Bass.imp



#season interactions
#using iml package here, useful for all sorts of models 
#this is computationally intensive with larger datasets/models 
#https://christophm.github.io/interpretable-ml-book/

preds <- train %>% dplyr::select(exposure, SAV, depth, season) # %>% droplevels()
predictor <- Predictor$new(model=Bass.Forest, data=preds, y=train$pres)
Bassinteract <- iml::Interaction$new(predictor, feature="season") 
Bassinteract_results <- Bassinteract$results %>% dplyr::rename(H =.interaction) %>% as.data.frame()
head(Bassinteract_results)

Bassint <- ggplot(data=Bassinteract_results, aes(x=0, xend=H, 
                                                 y=reorder(.feature, H), yend=reorder(.feature, H)))+
  geom_segment()+
  geom_point(data=Bassinteract_results, aes(H, reorder(.feature, H)),
             pch=21, fill="#00BC59", col="black", size=3)+
  theme_bw()+xlab("H")+ylab("Interaction")
Bassint



#partial dependencies 
BasspartialSAV <- Bass.Forest %>% pdp::partial(pred.var = "SAV", prob = TRUE, which.class='1', train=train)
Basspartialexposure <- Bass.Forest %>% pdp::partial(pred.var = "exposure", prob = TRUE, which.class='1', train=train)
Basspartialdepth <- Bass.Forest %>% pdp::partial(pred.var = "depth", prob = TRUE, which.class='1', train=train)
Basspartialseason <- Bass.Forest %>% pdp::partial(pred.var = "season", prob = TRUE, which.class='1', train=train)
BasspartialSAVseason <- Bass.Forest %>% pdp::partial(pred.var = c("SAV","season"), prob = TRUE, which.class='1', train=train)
Basspartialdepthseason <- Bass.Forest %>% pdp::partial(pred.var = c("depth","season"), prob = TRUE, which.class='1',train=train)
Basspartialexposureseason <- Bass.Forest %>% pdp::partial(pred.var = c("exposure","season"), prob = TRUE, which.class='1',train=train)

#plot
Basspartialseason$season <- factor(Basspartialseason$season, levels=c("winter", "spring", "summer", "fall"))
Bassseason <- ggplot(Basspartialseason, aes(season, yhat))+geom_boxplot(col="#00BC59")+
  ylab(bquote(~hat(y)))+xlab("Season")+theme_bw()+
  coord_cartesian(ylim=c(0,0.6))

Basssav <- ggplot(BasspartialSAV, aes(SAV, yhat))+geom_smooth(col="#00BC59")+
  ylab(bquote(~hat(y)))+theme_bw()+xlab("SAV (%)")+
  coord_cartesian(ylim=c(0,0.6))

Bassexp <- ggplot(Basspartialexposure, aes(exposure, yhat))+geom_smooth(col="#00BC59")+
  ylab(bquote(~hat(y)))+xlab("Exposure")+theme_bw()+
  coord_cartesian(ylim=c(0,0.6))

Bassdepth <- ggplot(Basspartialdepth, aes(depth, yhat))+geom_smooth(col="#00BC59")+
  ylab(bquote(~hat(y)))+xlab("Depth (m)")+theme_bw()+
  coord_cartesian(ylim=c(0,0.6))


#interactions
BasspartialSAVseason$season <- factor(BasspartialSAVseason$season, levels=c("winter", "spring", "summer", "fall"))
BassseasonSAV <- ggplot(BasspartialSAVseason, aes(season, SAV, fill=yhat))+geom_tile()+scale_fill_viridis_c()+
  theme_bw()+labs(fill=bquote(~hat(y)))+ylab("SAV (%)")+xlab("Season")

Basspartialdepthseason$season <- factor(Basspartialdepthseason$season, levels=c("winter", "spring", "summer", "fall"))
Bassseasondepth <- ggplot(Basspartialdepthseason, aes(season, depth, fill=yhat))+geom_tile()+scale_fill_viridis_c()+
  theme_bw()+labs(fill=bquote(~hat(y)))+ylab("Depth (m)")+xlab("Season")

Basspartialexposureseason$season <- factor(Basspartialexposureseason$season, levels=c("winter", "spring", "summer", "fall"))
Bassseasonexposure <- ggplot(Basspartialexposureseason, aes(season, exposure, fill=yhat))+geom_tile()+scale_fill_viridis_c()+
  theme_bw()+labs(fill=bquote(~hat(y)))+ylab("Exposure")+xlab("Season")

#all plots
Bass.imp+Bassint+Bassseason+plot_spacer()+Basssav+BassseasonSAV+Bassdepth+Bassseasondepth+Bassexp+
  Bassseasonexposure+plot_layout(ncol=2)




#more model fit.. residuals
train$pres.n <- as.numeric(as.character(train$pres))
Bass.Forest.fit <- data.frame(fitted=Bass.Forest$predicted, actual=train$pres.n)
Bass.Forest.fit$fitted <- as.numeric(as.character(Bass.Forest.fit$fitted))
Bass.Forest.fit$resid <- abs(Bass.Forest.fit$actual-Bass.Forest.fit$fitted)
Bass.Forest.fit <- cbind(Bass.Forest.fit, train)
head(Bass.Forest.fit)


ggplot(Bass.Forest.fit, aes(as.factor(fitted), resid))+stat_summary(fun="mean")+
  stat_summary(fun.data = "mean_se", geom = "errorbar", width=0.1)+scale_y_continuous(limits=c(0,1))+
  ggplot(Bass.Forest.fit, aes(season, resid))+stat_summary(fun="mean")+
  stat_summary(fun.data = "mean_se", geom = "errorbar", width=0.1)+scale_y_continuous(limits=c(0,1))+
  ggplot(Bass.Forest.fit, aes(SAV, resid))+geom_point()+geom_smooth()+
  ggplot(Bass.Forest.fit, aes(depth, resid))+geom_point()+geom_smooth()+
  ggplot(Bass.Forest.fit, aes(exposure, resid))+geom_point()+geom_smooth()+
  ggplot(Bass.Forest.fit, aes(region, resid))+stat_summary(fun="mean")+
  stat_summary(fun.data = "mean_se", geom = "errorbar", width=0.1)+scale_y_continuous(limits=c(0,1))

#spatial patterns in residuals
Bass.Forest.fit$lat <- nodes$lat[match(Bass.Forest.fit$node, nodes$Station.Group)]
Bass.Forest.fit$lon <- nodes$lon[match(Bass.Forest.fit$node, nodes$Station.Group)]

ggmap(TOmap, extent='normal')+
  scale_x_continuous(limits=c(min(nodes$lon)-0.01, max(nodes$lon)+0.01))+
  scale_y_continuous(limits=c(min(nodes$lat)-0.01, max(nodes$lat)+0.01))+
  ylab("Latitude") +
  xlab("Longitude")+
  geom_point(data=Bass.Forest.fit %>% group_by(node) %>% dplyr::summarise(lon=mean(lon),lat=mean(lat),prob=mean(actual)),
             aes(lon, lat, fill=prob, size=prob), col="yellow", pch=21)+
  scale_fill_viridis_c(option="plasma")+labs(title="Actual")+
  
  
  ggmap(TOmap, extent='normal')+
  scale_x_continuous(limits=c(min(nodes$lon)-0.01, max(nodes$lon)+0.01))+
  scale_y_continuous(limits=c(min(nodes$lat)-0.01, max(nodes$lat)+0.01))+
  ylab("Latitude") +
  xlab("Longitude")+
  geom_point(data=Bass.Forest.fit %>% group_by(node) %>% dplyr::summarise(lon=mean(lon),lat=mean(lat),prob=mean(fitted)),
             aes(lon, lat, fill=prob, size=prob), col="yellow", pch=21)+
  scale_fill_viridis_c(option="plasma")+labs(title="Fitted")+


ggmap(TOmap, extent='normal')+
  scale_x_continuous(limits=c(min(nodes$lon)-0.01, max(nodes$lon)+0.01))+
  scale_y_continuous(limits=c(min(nodes$lat)-0.01, max(nodes$lat)+0.01))+
  ylab("Latitude") +
  xlab("Longitude")+
  geom_point(data=Bass.Forest.fit %>% group_by(node) %>% dplyr::summarise(lon=mean(lon),lat=mean(lat),resid=mean(resid)),
             aes(lon, lat, fill=resid, size=resid), col="yellow", pch=21)+
  scale_fill_viridis_c(option="plasma")+labs(title="Residuals")+
  
  plot_layout(ncol=1)


#temporal patterns
ggplot(Bass.Forest.fit, aes(day, resid))+geom_point()+geom_smooth()
ggplot(Bass.Forest.fit, aes(day, resid))+geom_point()+geom_smooth()+facet_wrap(~node)

#At this point you may consider integrating more variables, or use a model with random effects, 
#spatial-temporal correlation structures






#Variable Selection ----
library(rfPermute) #this can be handy if you have a lot of variables
rpForest <- rfPermute(formula = z1, data = train, na.action=na.omit, 
                      replace = FALSE, ntree = 1000, nrep = 100, a = 0.05)

print(rpForest)
rpForest$rf$importance
#now have p-values for sig of terms:
rpForest$pval
#All predictors significant (and strangely the same p value here). 













#Model weighting ----

#original model
caret::confusionMatrix(RF.train.pred, train$pres)
#pretty well balanced model, but maybe a way to prioritize 1s more (as eg)

table(train$pres)
length(which(train$pres==1)) / length(which(train$pres==0))
#35% zeros. Try weighting model at 3:1

set.seed(1987)
Bass.Forest.weighted <- randomForest(formula=z, data = train, replace=FALSE, na.action=na.omit,
                               importance=TRUE, classwt=c(1,3), do.trace=1000, ntree=1000)
print(Bass.Forest.weighted)
print(Bass.Forest) #compare to og model 

#under-weighted (certainty of absences)
set.seed(1987)
Bass.Forest.under.weighted <- randomForest(formula=z, data = train, replace=FALSE, na.action=na.omit,
                                        importance=TRUE, classwt=c(3,1), do.trace=1000, ntree=1000)
print(Bass.Forest.under.weighted)




#compare predicted distributions:
Bass.Forest.fit$weighted.fit <- as.numeric(as.character(Bass.Forest.weighted$predicted))
Bass.Forest.fit$underweighted.fit <- as.numeric(as.character(Bass.Forest.under.weighted$predicted))
  
ggmap(TOmap, extent='normal')+
  scale_x_continuous(limits=c(min(nodes$lon)-0.01, max(nodes$lon)+0.01))+
  scale_y_continuous(limits=c(min(nodes$lat)-0.01, max(nodes$lat)+0.01))+
  ylab("Latitude") +
  xlab("Longitude")+
  geom_point(data=Bass.Forest.fit %>% group_by(node) %>% dplyr::summarise(lon=mean(lon),lat=mean(lat),prob=mean(fitted)),
             aes(lon, lat, fill=prob, size=prob), col="yellow", pch=21)+
  scale_fill_viridis_c(option="plasma")+labs(title="Fitted Original") + 
  
ggmap(TOmap, extent='normal')+
  scale_x_continuous(limits=c(min(nodes$lon)-0.01, max(nodes$lon)+0.01))+
  scale_y_continuous(limits=c(min(nodes$lat)-0.01, max(nodes$lat)+0.01))+
  ylab("Latitude") +
  xlab("Longitude")+
  geom_point(data=Bass.Forest.fit %>% group_by(node) %>% dplyr::summarise(lon=mean(lon),lat=mean(lat),prob=mean(weighted.fit)),
             aes(lon, lat, fill=prob, size=prob), col="yellow", pch=21)+
  scale_fill_viridis_c(option="plasma")+labs(title="Fitted Weighted") +
  
ggmap(TOmap, extent='normal')+
  scale_x_continuous(limits=c(min(nodes$lon)-0.01, max(nodes$lon)+0.01))+
  scale_y_continuous(limits=c(min(nodes$lat)-0.01, max(nodes$lat)+0.01))+
  ylab("Latitude") +
  xlab("Longitude")+
  geom_point(data=Bass.Forest.fit %>% group_by(node) %>% dplyr::summarise(lon=mean(lon),lat=mean(lat),prob=mean(underweighted.fit)),
             aes(lon, lat, fill=prob, size=prob), col="yellow", pch=21)+
  scale_fill_viridis_c(option="plasma")+labs(title="Fitted Under-Weighted")+
  
  plot_layout(ncol=1)






#Continuous Spatial Prediction ----
plot(hab.ras)

#turn into a dataframe, add season
hab.ras.df <- merge(as.data.frame(rasterToPoints(hab.ras)), data.frame(season=factor(c("winter","spring", "summer", "fall")))) 
head(hab.ras.df)


#predict presence probability with RF model
hab.ras.df$pred_prob <- predict(Bass.Forest, hab.ras.df, type="prob")[,2] #Probability prediction
hab.ras.df$pred_prob[hab.ras.df$land == 1] <- 0 #Prob of fish on land should be 0
hab.ras.df$pred_prob[hab.ras.df$land == 0 & hab.ras.df$depth==0 |
                       hab.ras.df$depth<min(train$depth)] <- NA #Outside domain of depth raster


#convert back to raster for basic plotting

hab.ras.winter <- rasterFromXYZ(hab.ras.df %>% filter(season=="winter") %>% dplyr::select(-season)) 
hab.ras.summer <- rasterFromXYZ(hab.ras.df %>% filter(season=="summer") %>% dplyr::select(-season)) 

plot(hab.ras.summer[["pred_prob"]], main="Summer")
plot(hab.ras.winter[["pred_prob"]], main="Winter")
#














#Boosted Regression Trees ----

#more complex to fit than Random Forests, but can be more accurate and handle some complex hierarchical interactions. 
# See Elith et al. (2008) for some guidance in using these. Lots useful functions they provide as well!

library(gbm)
source("worksheets/brt.functions.R") #from Elith et al. (2008)

#grab some data:
train.brt <- train %>% dplyr::select(pres.n, season, depth, SAV, exposure)
head(train.brt)

gbm1 <- gbm.step(data=train.brt, 
                 gbm.x = 2:5,
                 gbm.y = 1,
                 family = "bernoulli",
                 tree.complexity = 5,
                 learning.rate = 0.001,
                 bag.fraction = 0.5,
                 max.trees=20000)
print(gbm1)
summary(gbm1)

#plot fitted probability values:
gbm.plot.fits(gbm1, plot.layout = c(2,2))
#partial dependency plots:
gbm.plot(gbm1, n.plots=4, write.title = F, plot.layout = c(2,2))


#fit in test data
gbm1
brt.preds <- as.factor(ifelse(predict(gbm1, test, type="response")<0.5, 0,1))
caret::confusionMatrix(brt.preds, test$pres, positive="1")
#marginally better in test data?



#check to see if there are any uninformative predictors, to consider removing (this takes awhile not running here)
#gbm.simp <- gbm.simplify(gbm1, n.drops = 2)





#explore variable interactions:
find.int <- gbm.interactions(gbm1)
find.int$interactions


#brt.functions has a nice 3D interaction plot for continuous variables:
par(mfrow=c(1,1))
gbm.perspec(gbm1,x=2,y=3, y.range=c(0,100), z.range=c(0,0.3))


#not sure how to filter that down.. compare summer/winter models
train.brt.summer <- train.brt %>% filter(season=="summer")

gbm.summer <- gbm.step(data=train.brt.summer, 
                 gbm.x = 2:4,
                 gbm.y = 1,
                 family = "bernoulli",
                 tree.complexity = 5,
                 learning.rate = 0.001,
                 bag.fraction = 0.5,
                 max.trees=20000)

train.brt.winter <- train.brt %>% filter(season=="winter")
gbm.winter <- gbm.step(data=train.brt.winter, 
                       gbm.x = 2:4,
                       gbm.y = 1,
                       family = "bernoulli",
                       tree.complexity = 5,
                       learning.rate = 0.001,
                       bag.fraction = 0.5,
                       max.trees=20000)

par(mfrow=c(1,2))
gbm.perspec(gbm.summer,x=2,y=3, y.range=c(0,100), z.range=c(0,1))
gbm.perspec(gbm.winter,x=2,y=3, y.range=c(0,100), z.range=c(0,1))
dev.off()

#can do basically all the same model validation and prediction stuff with GBM as RF above. 
