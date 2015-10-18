# If you want to run anything here, first run everything 
# in libraries.R and then uncomment whatever you want to run here.
# data(BostonHousing)
# BostonHousing$chas <- as.numeric(BostonHousing$chas)-1
# inTrain <- caret::createDataPartition(BostonHousing$crim,
#                                p=.75,
#                                list=FALSE)
# 
# training <- BostonHousing[inTrain,]
# testing <- BostonHousing[-inTrain,]
# 
# ctrl <- caret::trainControl(
#   method = "repeatedcv",
#   number = 10)
#   
# 
# set.seed(4587)
# lmfit <- caret::train(crim ~ . ,
#                data = training,
#                method ="lm"
# )
# lmfit
# ################
# 
# #lmGrid <-  expand.grid(nvmax=1:(ncol(training)-1))
# 
# lmforwardfit <- caret::train(crim ~.,
#                       data = training,
#                       method ="leapForward"
#                       
#                       
# )
# lmforwardfit
# 
# 
# #################
# 
# ridgemodel <- list(type = "Regression",
#               library = "Lab7"
#               )
# 
# ridgemodel$parameters<-data.frame(parameter="lambda",
#                              class="numeric",
#                              label="lambda")
# 
# Fit<-function(x,y,lambda,param,lev,last,classProbs,...){
#   
#   dat <- as.data.frame(x)
#   
#   respvector <- NULL
#   respname <- NULL
#   respnum <- NULL
#   
#   for(i in 1:ncol(x)){
#     if(identical(y,dat[,i])){
#     respvector <- dat[,i]
#     respname <- names(x)[i]
#     respnum <- i
#     }
#   }
#   
#   formula <- paste(respname,"~", sep="")
#   
#   if(ncol(x) > 1){
#     for(i in 1:ncol(x)){
#       if(i != respnum){
#       formula <- paste(formula, "+", names(dat)[i], sep="")
#       }
#     }
#   }
#   
#   formula <- as.formula(formula)
#   model <- Lab7::ridgeregr( formula = formula, data=dat,lambda= param$lambda)
#   return(model)
# }
# 
# ridgemodel$fit<-Fit
# 
# ridgemodel$predict<-function(modelFit, newdata, preProc = NULL, submodels = NULL){
# 
#   predict(modelFit,newdata)
# }
# 
# ridgemodel$prob<- list(NULL)
# 
# ridgemodel$sort<-function (x) x[order(-x$lambda), ]
# 
# ridgemodel$label<-"Ridge Regression"
# 
# ridgemodel$grid<-function(x,y,len=NULL, search="grid"){
#   data.frame(lambda=seq(from=0, to=200, by=10))
# }
# 
# set.seed(-274819L)
# 
# #ridgeFit does not want to build like this so I comment it. Just uncomment
# #and run it. It works well in ridgereg.rmd
# 
# ridgeFit <- caret::train( y = training$crim,
#                          x = training,
#                        method = ridgemodel,
#                        trControl = ctrl
# )
# ridgeFit
# 
# ####################################
# 
# lm_testeval <-predict(lmfit,testing)
# 
# lm_testres <- testing$crim - lm_testeval
# 
# plot(lm_testres)
# qqnorm(lm_testres)
# qqline(lm_testres)
# lmfor_testeval <-predict(lmforwardfit,testing)
# 
# lmfor_testres <- testing$crim - lmfor_testeval
# 
# plot(lmfor_testres)
# qqnorm(lmfor_testres)
# qqline(lmfor_testres)
# 
# ridge_testeval <- predict(ridgeFit,testing)
# 
# ridge_testres <- testing$crim - mean(testing$crim) - ridge_testeval
# 
# plot(ridge_testres)
# qqnorm(ridge_testres)
# qqline(ridge_testres)