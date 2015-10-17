library(caret)
library(mlbench)
data(BostonHousing)
BostonHousing$chas <- as.numeric(BostonHousing$chas)-1
inTrain <- createDataPartition(BostonHousing$crim,
                               p=.75,
                               list=FALSE)

training <- BostonHousing[inTrain,]
testing <- BostonHousing[-inTrain,]

ctrl <- trainControl(
  method = "repeatedcv",
  number = 10)
  

set.seed(4587)
lmfit <- train(crim ~ . ,
               data = training,
               method ="lm"
)
lmfit
################

lmGrid <-  expand.grid(nvmax=1:(ncol(training)-1))

lmforwardfit <- train(crim ~.,
                      data = training,
                      method ="leapForward",
                      #preProc = c("center","scale"),
                      tuneGrid = lmGrid
)
lmforwardfit

#################

ridgemodel <- list(type = "Regression",
              library = "Lab7"
              )

ridgemodel$parameters<-data.frame(parameter="lambda",
                             class="numeric",
                             label="lambda")

Fit<-function(x,y,lambda,param,lev,last,classProbs,...){
  
  dat <- as.data.frame(x)
  
  respvector <- NULL
  respname <- NULL
  respnum <- NULL
  
  for(i in 1:ncol(x)){
    if(identical(y,dat[,i])){
    respvector <- dat[,i]
    respname <- names(x)[i]
    respnum <- i
    }
  }
  
  formula <- paste(respname,"~", sep="")
  
  if(ncol(x) > 1){
    for(i in 1:ncol(x)){
      if(i != respnum){
      formula <- paste(formula, "+", names(dat)[i], sep="")
      }
    }
  }
  
  formula <- as.formula(formula)
  model <- Lab7::ridgereg( formula = formula, data=dat,lambda= param$lambda)
}

ridgemodel$fit<-Fit

ridgemodel$predict<-function(modelFit, newdata, preProc = NULL, submodels = NULL){

  predvalues <- modelFit$predict(newdata)
}

ridgemodel$prob<- list(NULL)

ridgemodel$sort<-function (x) x[order(-x$lambda), ]

ridgemodel$label<-"Ridge Regression"

ridgemodel$grid<-function(x,y,len=NULL, search="grid"){
  data.frame(lambda=c( 1))
}

ridgeFit <- caret::train(y = training$crim,
                         x = training,
                       method = ridgemodel,
                       trControl = ctrl
)
ridgeFit

####################################



