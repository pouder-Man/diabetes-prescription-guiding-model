library(C50)
model <- readRDS("./c5_model.rds")
model <- readRDS("./model2.rds")
model
summary(model)
df<-read.csv('test2.csv')

prob <- predict(model, newdata = df[1:1, 1:125], type = "prob")
prob
prob_str <- c(prob[1:1,1:1],prob[1:1,2:2],prob[1:1,3:3],prob[1:1,4:4],prob[1:1,5:5],prob[1:1,6:6],
              prob[1:1,7:7],prob[1:1,8:8],prob[1:1,9:9],prob[1:1,10:10],prob[1:1,11:11],prob[1:1,12:12],
              prob[1:1,13:13],prob[1:1,14:14],prob[1:1,15:15],prob[1:1,16:16],prob[1:1,17:17])
prob_f <- paste(prob_str,collapse="|")
prob_f
max1 <- 0
max2 <- 0
max3 <- 0
idx1 <- -1
idx2 <- -1
idx3 <- -1

for(i in 1:17){
  if(max1 < prob[1:1,i:i]){
    max3 <- max2
    max2 <- max1
    max1 <- prob[1:1,i:i]
    idx3 <- idx2
    idx2 <- idx1
    idx1 <- i
  }
  else{
    if(max2 <prob[1:1,i:i]){
      max3 <- max2
      max2 <- prob[1:1,i:i]
      idx3 <- idx2
      idx2 <- i
    }
    else{
      if(max3 <prob[1:1,i:i]){
        max3 <-prob[1:1,i:i]
        idx3 <-i
      }
    }
  }
}

library(ggplot2)
v <-c("51","54,51","56","56,51","56,54,51","59","59,51","59,56,51","59,56,54,51",
      "61,51","61,54,51","61,56,51","61,56,54,51","61,59,51","61,59,54,51",
      "61,59,56,51","61,59,56,54,51")
v_f <-paste(v,collapse="|")
v_f

tplot <- list(data.frame(idx=toString(v[1]),probability=prob[1:1,1:1]),
              data.frame(idx=toString(v[2]),probability=prob[1:1,2:2]),
              data.frame(idx=toString(v[3]),probability=prob[1:1,3:3]),
              data.frame(idx=toString(v[4]),probability=prob[1:1,4:4]),
              data.frame(idx=toString(v[5]),probability=prob[1:1,5:5]),
              data.frame(idx=toString(v[6]),probability=prob[1:1,6:6]),
              data.frame(idx=toString(v[7]),probability=prob[1:1,7:7]),
              data.frame(idx=toString(v[8]),probability=prob[1:1,8:8]),
              data.frame(idx=toString(v[9]),probability=prob[1:1,9:9]),
              data.frame(idx=toString(v[10]),probability=prob[1:1,10:10]),
              data.frame(idx=toString(v[11]),probability=prob[1:1,11:11]),
              data.frame(idx=toString(v[12]),probability=prob[1:1,12:12]),
              data.frame(idx=toString(v[13]),probability=prob[1:1,13:13]),
              data.frame(idx=toString(v[14]),probability=prob[1:1,14:14]),
              data.frame(idx=toString(v[15]),probability=prob[1:1,15:15]),
              data.frame(idx=toString(v[16]),probability=prob[1:1,16:16]),
              data.frame(idx=toString(v[17]),probability=prob[1:1,17:17]))


plot <- list(data.frame(idx=toString(v[idx1]),probability=max1),data.frame(idx=toString(v[idx2]),probability=max2),data.frame(idx=toString(v[idx3]),probability=max3))
newplot <- do.call(rbind,plot)
p <- ggplot(newplot,aes(idx,probability))+geom_bar(stat="identity",fill = "blue")
p
newtplot <- do.call(rbind,tplot)
tp <- ggplot(newtplot,aes(idx,probability))+geom_bar(stat="identity",fill = "blue")
tp


ggsave(file="~/top3_bar_plot.png",plot=p,dpi=300)
ggsave(file="~/total_bar_plot.png",plot=tp,dpi=300)