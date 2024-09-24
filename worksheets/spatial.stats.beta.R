#Integrating spatial and random effects into machine learning protocols
#beta version

#Jake Brownscombe, Sept 2023



#select similar set of data from the Random Forests in Telemetry_ML.R
set.seed(1987)
train <- dets.sub %>% sample_frac(0.1) #using 10% for train here for quick fitting 
test <- dets.sub[!(dets.sub$nrow %in% train$nrow),]


#install.packages('mboost')
library(mboost)

options(mboost_lambdaMax = 1e+20)

controls <- boost_control(mstop=1000,
                          nu=0.1, 
                          trace=TRUE)

weights=ifelse(train$pres==1,2,1)

gamboost <- gamboost(pres ~ 
                    bbs(depth, by=season, df=10) +
                    bbs(SAV, by=season, df=10) +
                    bbs(exposure, by=season, df=10),
                  family = Binomial(),
                  weights=weights,
                  control=controls,
                  data = train)

#train data fit
gamboost.pred <- data.frame(probs=predict(gamboost, train, type="response"))
gamboost.pred$class <- as.factor(ifelse(gamboost.pred$probs > 0.5, 1, 0))
gamboost.pred$class <- factor(gamboost.pred$class, levels=c("0","1"))
caret::confusionMatrix(gamboost.pred$class, train$pres, positive="1")


#test data
gamboost.test.pred <- data.frame(probs=predict(gamboost, test, type="response"))
gamboost.test.pred$class <- as.factor(ifelse(gamboost.test.pred$probs > 0.5, 1, 0))
gamboost.test.pred$class <- factor(gamboost.test.pred$class, levels=c("0","1"))
caret::confusionMatrix(gamboost.test.pred$class, test$pres, positive="1")


#variable importance 
plot(varimp(gamboost))

#partial dependencies
depth.pdp <- gamboost %>% pdp::partial(pred.var = c("depth","season"), prob = TRUE, which.class='1', train=train)
SAV.pdp <- gamboost %>% pdp::partial(pred.var = c("SAV","season"), prob = TRUE, which.class='1', train=train)
exposure.pdp <- gamboost %>% pdp::partial(pred.var = c("exposure","season"), prob = TRUE, which.class='1', train=train)

depth.pdp$season <- factor(depth.pdp$season, levels=c("winter", "spring", "summer", "fall"))
SAV.pdp$season <- factor(SAV.pdp$season, levels=c("winter", "spring", "summer", "fall"))
exposure.pdp$season <- factor(exposure.pdp$season, levels=c("winter", "spring", "summer", "fall"))

ggplot(depth.pdp, aes(season, depth,  fill=yhat))+geom_tile()+scale_fill_viridis_c()+
  ggplot(SAV.pdp, aes(season, SAV,  fill=yhat))+geom_tile()+scale_fill_viridis_c()+
  ggplot(exposure.pdp, aes(season, exposure,  fill=yhat))+geom_tile()+scale_fill_viridis_c()+
  plot_layout(nrow=2)


#compare to RF outputs:
ggplot(depth.pdp, aes(season, depth,  fill=yhat))+geom_tile()+scale_fill_viridis_c()+ggtitle("Mboost")+
  ggplot(SAV.pdp, aes(season, SAV,  fill=yhat))+geom_tile()+scale_fill_viridis_c()+
  ggplot(exposure.pdp, aes(season, exposure,  fill=yhat))+geom_tile()+scale_fill_viridis_c()+

Bassseasondepth+ggtitle("Random Forests") + BassseasonSAV + Bassseasonexposure + plot_layout(nrow=2)








#add spatial field

gamboost.spatial <- gamboost(pres ~ 
                       bbs(depth, by=season, df=10) +
                       bbs(SAV, by=season, df=10) +
                       bbs(exposure, by=season, df=10) +
                       bspatial(lon, lat),
                     family = Binomial(),
                     weights=weights,
                     control=controls,
                     data = train)

#spatial field:
plot(gamboost.spatial, which = "bspatial(lon, lat)")


#train data fit
gamboost.spatial.pred <- data.frame(probs=predict(gamboost.spatial, train, type="response"))
gamboost.spatial.pred$class <- as.factor(ifelse(gamboost.spatial.pred$probs > 0.5, 1, 0))
gamboost.spatial.pred$class <- factor(gamboost.spatial.pred$class, levels=c("0","1"))
caret::confusionMatrix(gamboost.spatial.pred$class, train$pres, positive="1")


#test data
gamboost.spatial.test.pred <- data.frame(probs=predict(gamboost.spatial, test, type="response"))
gamboost.spatial.test.pred$class <- as.factor(ifelse(gamboost.spatial.test.pred$probs > 0.5, 1, 0))
gamboost.spatial.test.pred$class <- factor(gamboost.spatial.test.pred$class, levels=c("0","1"))
caret::confusionMatrix(gamboost.spatial.test.pred$class, test$pres, positive="1")


#variable importance 
plot(varimp(gamboost.spatial))



#partial dependencies
depth.pdp.spatial <- gamboost.spatial %>% pdp::partial(pred.var = c("depth","season"), prob = TRUE, which.class='1', train=train)
SAV.pdp.spatial <- gamboost.spatial %>% pdp::partial(pred.var = c("SAV","season"), prob = TRUE, which.class='1', train=train)
exposure.pdp.spatial <- gamboost.spatial %>% pdp::partial(pred.var = c("exposure","season"), prob = TRUE, which.class='1', train=train)

depth.pdp.spatial$season <- factor(depth.pdp.spatial$season, levels=c("winter", "spring", "summer", "fall"))
SAV.pdp.spatial$season <- factor(SAV.pdp.spatial$season, levels=c("winter", "spring", "summer", "fall"))
exposure.pdp.spatial$season <- factor(exposure.pdp.spatial$season, levels=c("winter", "spring", "summer", "fall"))

ggplot(depth.pdp.spatial, aes(season, depth,  fill=yhat))+geom_tile()+scale_fill_viridis_c()+
  ggplot(SAV.pdp.spatial, aes(season, SAV,  fill=yhat))+geom_tile()+scale_fill_viridis_c()+
  ggplot(exposure.pdp.spatial, aes(season, exposure,  fill=yhat))+geom_tile()+scale_fill_viridis_c()+
  plot_layout(nrow=2)

#













#add random effect of node
gamboost.node <- gamboost(pres ~ 
                               bbs(depth, by=season, df=10) +
                               bbs(SAV, by=season, df=10) +
                               bbs(exposure, by=season, df=10) +
                               brandom(node),
                             family = Binomial(),
                             weights=weights,
                             control=controls,
                             data = train)

#node effect:
dev.off()
plot(gamboost.node, which = "brandom(node)")


#train data fit
gamboost.node.pred <- data.frame(probs=predict(gamboost.node, train, type="response"))
gamboost.node.pred$class <- as.factor(ifelse(gamboost.node.pred$probs > 0.5, 1, 0))
gamboost.node.pred$class <- factor(gamboost.node.pred$class, levels=c("0","1"))
caret::confusionMatrix(gamboost.node.pred$class, train$pres, positive="1")


#test data
gamboost.node.test.pred <- data.frame(probs=predict(gamboost.node, test, type="response"))
gamboost.node.test.pred$class <- as.factor(ifelse(gamboost.node.test.pred$probs > 0.5, 1, 0))
gamboost.node.test.pred$class <- factor(gamboost.node.test.pred$class, levels=c("0","1"))
caret::confusionMatrix(gamboost.node.test.pred$class, test$pres, positive="1")


#variable importance 
plot(varimp(gamboost.node))



#partial dependencies
depth.pdp.node <- gamboost.node %>% pdp::partial(pred.var = c("depth","season"), prob = TRUE, which.class='1', train=train)
SAV.pdp.node <- gamboost.node %>% pdp::partial(pred.var = c("SAV","season"), prob = TRUE, which.class='1', train=train)
exposure.pdp.node <- gamboost.node %>% pdp::partial(pred.var = c("exposure","season"), prob = TRUE, which.class='1', train=train)

depth.pdp.node$season <- factor(depth.pdp.node$season, levels=c("winter", "spring", "summer", "fall"))
SAV.pdp.node$season <- factor(SAV.pdp.node$season, levels=c("winter", "spring", "summer", "fall"))
exposure.pdp.node$season <- factor(exposure.pdp.node$season, levels=c("winter", "spring", "summer", "fall"))

ggplot(depth.pdp.node, aes(season, depth,  fill=yhat))+geom_tile()+scale_fill_viridis_c()+
  ggplot(SAV.pdp.node, aes(season, SAV,  fill=yhat))+geom_tile()+scale_fill_viridis_c()+
  ggplot(exposure.pdp.node, aes(season, exposure,  fill=yhat))+geom_tile()+scale_fill_viridis_c()+
  plot_layout(nrow=2)


#look at all the pdps from each model:

ggplot(depth.pdp, aes(season, depth,  fill=yhat))+geom_tile()+scale_fill_viridis_c()+ggtitle("Base model")+
  ggplot(SAV.pdp, aes(season, SAV,  fill=yhat))+geom_tile()+scale_fill_viridis_c()+
  ggplot(exposure.pdp, aes(season, exposure,  fill=yhat))+geom_tile()+scale_fill_viridis_c()+

ggplot(depth.pdp.spatial, aes(season, depth,  fill=yhat))+geom_tile()+scale_fill_viridis_c()+ggtitle("Spatial model")+
  ggplot(SAV.pdp.spatial, aes(season, SAV,  fill=yhat))+geom_tile()+scale_fill_viridis_c()+
  ggplot(exposure.pdp.spatial, aes(season, exposure,  fill=yhat))+geom_tile()+scale_fill_viridis_c()+
  
ggplot(depth.pdp.node, aes(season, depth,  fill=yhat))+geom_tile()+scale_fill_viridis_c()+ggtitle("Random effect model")+
  ggplot(SAV.pdp.node, aes(season, SAV,  fill=yhat))+geom_tile()+scale_fill_viridis_c()+
  ggplot(exposure.pdp.node, aes(season, exposure,  fill=yhat))+geom_tile()+scale_fill_viridis_c()+
  plot_layout(nrow=3)


