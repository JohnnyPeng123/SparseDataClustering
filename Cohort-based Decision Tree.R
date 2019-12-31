library(stats)
library(dplyr)
library(plotly)
library(rpart)

data = read.csv('C:\\Users\\woshi\\data.csv')
data_Y = data[-which(colnames(data)=='Title')]
data_X = data[-which(colnames(data)=='Title')]
rownames(data_Y) <- data[['Title']]
rownames(data_X) <- data[['Title']]

data_Y$Title <- rownames(data_Y) 

for (i in 1:length(data_X)) {
  for (j in 1:nrow(data_X[i])){
    data_X[[i]][j] <- ifelse(data_X[[i]][j]=="NaN",0,1)
  }
}

hc <- hclust(dist(data_X), "ward.D2")
hc3 <- cutree(hc, k = 4)

# load code of A2R function
source("http://addictedtor.free.fr/packages/A2R/lastVersion/R/code.R")
# colored dendrogram
op = par(bg = "#EFEFEF")
A2Rplot(hc, k = 10, boxes = FALSE, col.up = "gray50")

x <- subset(data_X, rownames(data_X) %in% names(hc3)[as.numeric(hc3)==4])
hc_temp <- hclust(dist(x), "ward.D2")
plot(hc_temp)

hoof <- subset(data_X, rownames(data_Y) %in% names(hc3)[as.numeric(hc3)==3])
dog_cat <- subset(data_X, rownames(data_Y) %in% names(hc3)[as.numeric(hc3)==4])

#---------------------------Fitting DT without any clustering---------------------------#
rpart.control1 = rpart.control(minsplit = 0, minbucket = 0, cp = 0, 
                               maxcompete = 10000, maxsurrogate = 10000, 
                               usesurrogate = 2, xval = 0,
                               surrogatestyle = 1, maxdepth = 30)

data = data_Y
fit_all = rpart(Title ~ ., data = data, method="class", control = rpart.control1)
#rpart.plot(model.rpart, type=0)
t_pred = predict(fit_all,data[-ncol(data)],type="class")

counter <- 0
i <- 0
for (val in t_pred) {i=i + 1 
if(t_pred[i] == data$Title[i])  counter = counter + 1
}
print(counter/length(data$Title))

par(mfrow=c(1, 1))
plot(fit_all, uniform=TRUE, main="One-fits-all | Accuracy = 0.7263")
text(fit_all, fancy=FALSE, pretty=NULL, cex=0.5)
capture.output(summary(fit_all),file = "OneFitsAll.txt")

#---------------------------Dog_Cat---------------------------#
rpart.control1 = rpart.control(minsplit = 0, minbucket = 0, cp = 0, 
                               maxcompete = 0, maxsurrogate = 10000, 
                               usesurrogate = 2, xval = 0,
                               surrogatestyle = 0, maxdepth = 30)

data = dog_cat
fit = rpart(Title ~ ., data = data, method="class", control = rpart.control1)
#rpart.plot(model.rpart, type=0)
t_pred = predict(fit,data[-ncol(data)],type="class")
counter <- 0
i <- 0
for (val in t_pred) {i=i + 1 
if(t_pred[i] == data$Title[i])  counter = counter + 1
}
print(counter/length(data$Title))

par(mfrow=c(1, 1))
plot(fit, uniform=TRUE, main="dog_cat | Accuracy = 1")
text(fit, fancy=FALSE, pretty=NULL, cex=0.9)

#---------------------------Hoof---------------------------#
rpart.control1 = rpart.control(minsplit = 0, minbucket = 0, cp = 0, 
                               maxcompete = 0, maxsurrogate = 10000, 
                               usesurrogate = 2, xval = 0,
                               surrogatestyle = 0, maxdepth = 30)

data = hoof
fit = rpart(Title ~ ., data = data, method="class", control = rpart.control1)
#rpart.plot(model.rpart, type=0)
t_pred = predict(fit,data[-ncol(data)],type="class")
counter <- 0
i <- 0
for (val in t_pred) {i=i + 1 
if(t_pred[i] == data$Title[i])  counter = counter + 1
}
print(counter/length(data$Title))

par(mfrow=c(1, 1))
plot(fit, uniform=TRUE, main="hoof | Accuracy = 1")
text(fit, fancy=FALSE, pretty=NULL, cex=0.9)
