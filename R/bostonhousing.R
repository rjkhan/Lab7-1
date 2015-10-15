data(BostonHousing)

inTrain <- createDataPartition(y=BostonHousing$crim,
                               p=.90,
                               list=FALSE)

training <- BostonHousing[inTrain,]
testing <- BostonHousing[-inTrain,]

ctrl <- trainControl(
  method = "repeatedcv",
  number = 10)
  


lmfit <- train(crim ~. ,
               data = training,
               method ="lm",
               tuneLength = 3 
               #preProc = c("center","scale"),
               )

lmforwardfit <- train(crim ~.,
                      data = training,
                      method ="leapForward",
                      preProc = c("center","scale"),
                      tuneLength=9
)

