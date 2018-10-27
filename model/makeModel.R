df<-read.csv('result_4.csv')
tail(df,10)
set.seed(2)
id<-sample(2,nrow(df),prob = c(0.8,0.2), replace = TRUE)
train<-df[id==1,]
test <-df[id==2,]
tail(test,2)

library(C50)
c5_options <- C5.0Control(subset = TRUE,
                          #bands = 0,
                          winnow = FALSE,
                          noGlobalPruning = FALSE,CF = 0.25, minCases = 1,
)
c5_model <- C5.0(prescription~ ., data = train, control=c5_options, rules=FALSE)
summary(c5_model)
c5_model
plot(c5_model)
#predict(c5_model, newdata = test[1:10, 1:125], type = "prob")
#predict(c5_model, newdata = test[1:10, 1:125])
saveRDS(c5_model, "c5_model.rds")
model <- readRDS("./c5_model.rds")

#predict(model, newdata = test[6:10, 1:125], type = "prob")

#predict(model, newdata = test[6:10, 1:125])

show(test[6:10,126:126])
predict(model, newdata = test1[1:1, 1:125], type = "prob")