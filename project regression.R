## Data Cleaning ##
googlePlay<-read.csv("/Users/shouzenmaiwa/Desktop/googleplaystore.csv", header=TRUE)
# install.packages("data.table")
library(data.table)
# install.packages("prettyR")
library(prettyR)
## remove apps Life Made WI-Fi Touchscreen Photo Frame, wrong data saved
GP<-googlePlay[-10473,] 
GP<-GP[!duplicated(GP$App),] ## remove duplicated rows based on the App

## change the digits in Installs(remove '+' and ',' place '0+', 'Free' under '0')
GP$Installs<-as.character(GP$Installs)
GP$Installs[GP$Installs=="Free"]<-0
GP$Installs[GP$Installs=="0+"]<-0
GP$Installs[GP$Installs=="1,000,000+"]<-1000000
GP$Installs[GP$Installs=="10,000,000+"]<-10000000
GP$Installs[GP$Installs=="100,000+"]<-100000
GP$Installs[GP$Installs=="10,000+"]<-10000
GP$Installs[GP$Installs=="1,000+"]<-1000
GP$Installs[GP$Installs=="5,000,000+"]<-5000000
GP$Installs[GP$Installs=="100+"]<-100
GP$Installs[GP$Installs=="500,000+"]<-500000
GP$Installs[GP$Installs=="50,000+"]<-50000
GP$Installs[GP$Installs=="5,000+"]<-5000
GP$Installs[GP$Installs=="100,000,000+"]<-100000000
GP$Installs[GP$Installs=="10+"]<-10
GP$Installs[GP$Installs=="500+"]<-500
GP$Installs[GP$Installs=="50,000,000+"]<-50000000
GP$Installs[GP$Installs=="50+"]<-50
GP$Installs[GP$Installs=="5+"]<-5
GP$Installs[GP$Installs=="500,000,000+"]<-500000000
GP$Installs[GP$Installs=="1+"]<-1
GP$Installs[GP$Installs=="1,000,000,000+"]<-1000000000
GP$Installs<-as.factor(GP$Installs)
#GP$Installs<-as.numeric(GP$Installs)

GP$Reviews<-as.numeric(GP$Reviews)

## rating : NaN into 0
## The lowest value of rating is 1.0, if no rating is available, we assume it is 0
GP$Rating[is.nan(GP$Rating)]<-0

## Price : remove $ symbol
GP$Price <- gsub('[$]','',GP$Price)
GP$Price<-as.numeric(GP$Price)

## Type: remove 0, NaN
GP$Type<-as.character(GP$Type)
GP<-GP[!grepl("0",GP$Type),]
GP<-GP[!grepl("NaN",GP$Type),]
GP$Type<-as.factor(GP$Type)

# delete the apps with "Varies with device", and convert the M,k into bytes
GP$Size<-as.character(GP$Size)
GP<-GP[!grepl("Varies with device",GP$Size),]

convert.size <- function(x){
  size.char <- "(\\d*(.\\d+)*)(.*)"
  num  <- as.numeric(sub(size.char, "\\1", x))
  unit <- sub(size.char, "\\3", x)             
  unit[unit==""] <- "1" 
  size.list <- c("1"=1, "k"=1024, "M"=1024^2)
  num * unname(size.list[unit])
}
GP$Size<-convert.size(GP$Size)

# convert the last updated date into days
# assume the data is collected on 9 Aug 2018
GP$Last.Updated<-as.character(GP$Last.Updated)
GP$Last.Updated<- gsub('[,]','',GP$Last.Updated)
GP$Last.Updated<- gsub('[ ]','/',GP$Last.Updated)
current_date<-"August/9/2018"
current.date<-as.Date(as.character(current_date), format="%B/%d/%Y")
GP$Last.Updated<-as.Date(as.character(GP$Last.Updated), format="%B/%d/%Y")
diff<-c()
for(i in 1:length(GP$Last.Updated)){
  diff[i]<-current.date-GP$Last.Updated[i]
}
GP$Last.Updated<-diff  

# remove "and up" and any digit after the second .
# remove varies with device, NN
GP<-GP[!grepl("Varies with device",GP$Android.Ver),]
GP<-GP[!grepl("NaN",GP$Android.Ver),]
GP$Android.Ver<- gsub('[and up]','',GP$Android.Ver)
GP$Android.Ver<-substr(GP$Android.Ver,1,3)
GP$Android.Ver<-as.numeric(GP$Android.Ver)

# similar process for current ver
GP$Current.Ver<-as.character(GP$Current.Ver)
GP<-GP[!grepl("Varies with device",GP$Current.Ver),]
GP<-GP[!grepl("NaN",GP$Current.Ver),]
GP$Current.Ver<- gsub('v','',GP$Current.Ver)
GP$Current.Ver<- gsub('V','',GP$Current.Ver)
GP$Current.Ver<- gsub('Cow ','',GP$Current.Ver)
GP$Current.Ver<- gsub('ersion','',GP$Current.Ver)
GP$Current.Ver<- gsub('Android','',GP$Current.Ver)
GP$Current.Ver<-substr(GP$Current.Ver,1,3)
GP$Current.Ver<-as.numeric(GP$Current.Ver)
GP<-GP[GP$Current.Ver>=0.1,]
GP<-GP[GP$Current.Ver<=10,]

GP <- GP[ ,-1]
GP <- GP[ ,-10]
sum(is.na(GP))
## Remove those missing values 
GP = na.omit(GP) 

GP$Rating<-as.numeric(GP$Rating)
GP$Reviews<-as.numeric(GP$Reviews)
GP$Size<-as.numeric(GP$Size)
GP$Installs<-as.numeric(GP$Installs)
GP$Price<-as.numeric(GP$Price)
GP$Current.Ver<-as.numeric(GP$Current.Ver)
GP$Android.Ver<-as.numeric(GP$Android.Ver)
contrasts(GP$Category)
contrasts(GP$Type)
contrasts(GP$Content.Rating)
contrasts(GP$Genres)

lm.fit = lm(Rating ~ ., data = GP) 
summary(lm.fit)


## Best Subset Selection
# install.packages(pkgs='leaps')
library(leaps)
GP <- ts(GP)
regfit_full = regsubsets(Rating ~ ., data = GP)
reg_summary = summary(regfit_full)
reg_summary
par(mfrow=c(2,2))
plot(reg_summary$rss, xlab="Number of Variables", ylab="RSS")
(best_model = which.min(reg_summary$rss))
points(best_model, reg_summary$rss[best_model], col="red", cex=2, pch=20)
plot(reg_summary$adjr2, xlab="Number of Variables", ylab="Adjusted RSq", type="l")
(best_model = which.max(reg_summary$adjr2))
points(best_model, reg_summary$adjr2[best_model], col="red", cex=2, pch=20)
plot(reg_summary$cp, xlab="Number of Variables", ylab="Cp", type="l") 
(best_model = which.min(reg_summary$cp))
points(best_model, reg_summary$cp[best_model], col="red", cex=2, pch=20)
plot(reg_summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
(best_model = which.min(reg_summary$bic))
points(best_model, reg_summary$bic[best_model], col="red", cex=2, pch=20)

## Forward and Backward Stepwise Selection  
regfit_fwd = regsubsets(Rating ~ ., data = GP,
                        nvmax = 11,
                        method = "forward")
reg_summary1 <- summary(regfit_fwd)
reg_summary1
par(mfrow=c(2,2))
plot(reg_summary1$rss, xlab="Number of Variables", ylab="RSS")
(best_model = which.min(reg_summary1$rss))
points(best_model, reg_summary1$rss[best_model], col="red", cex=2, pch=20)
plot(reg_summary1$adjr2, xlab="Number of Variables", ylab="Adjusted RSq", type="l")
(best_model = which.max(reg_summary1$adjr2))
points(best_model, reg_summary1$adjr2[best_model], col="red", cex=2, pch=20)
plot(reg_summary1$cp, xlab="Number of Variables", ylab="Cp", type="l") 
(best_model = which.min(reg_summary1$cp))
points(best_model, reg_summary1$cp[best_model], col="red", cex=2, pch=20)
plot(reg_summary1$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
(best_model = which.min(reg_summary1$bic))
points(best_model, reg_summary1$bic[best_model], col="red", cex=2, pch=20)
par(mfrow = c(2, 2)) 
plot(regfit_fwd, scale = "r2") 
plot(regfit_fwd, scale = "adjr2") 
plot(regfit_fwd, scale = "Cp") 
plot(regfit_fwd, scale = "bic")

regfit_bwd = regsubsets(Rating ~ ., data = GP,
                        nvmax = 5,
                        method = "backward")
reg_summary2 <- summary(regfit_bwd) 
reg_summary2
par(mfrow = c(2, 2)) 
plot(reg_summary2$rss, xlab="Number of Variables", ylab="RSS")
(best_model = which.min(reg_summary2$rss))
points(best_model, reg_summary2$rss[best_model], col="red", cex=2, pch=20)
plot(reg_summary2$adjr2, xlab="Number of Variables", ylab="Adjusted RSq", type="l")
(best_model = which.max(reg_summary2$adjr2))
points(best_model, reg_summary2$adjr2[best_model], col="red", cex=2, pch=20)
plot(reg_summary2$cp, xlab="Number of Variables", ylab="Cp", type="l") 
(best_model = which.min(reg_summary2$cp))
points(best_model, reg_summary2$cp[best_model], col="red", cex=2, pch=20)
plot(reg_summary2$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
(best_model = which.min(reg_summary2$bic))
points(best_model, reg_summary2$bic[best_model], col="red", cex=2, pch=20)
par(mfrow = c(2, 2)) 
plot(regfit_bwd, scale="r2") 
plot(regfit_bwd, scale="adjr2") 
plot(regfit_bwd, scale="Cp") 
plot(regfit_bwd, scale="bic")
# best_modle = 4, Reviews, Size, Installs, Current.Ver



lm.fit = lm(Rating ~ Reviews+Installs+Current.Ver+Size, data = GP) 
summary(lm.fit)
par(mfrow = c(2, 2)) 
plot(lm.fit)

# Leave-one-out cv (LOOCV)     
library(boot)
glm_fit = glm(formula = Rating ~ Reviews+Installs+Current.Ver+Size, data = GP)
# cv.glm(GP, glm_fit)$delta ## Very slow (does not use formula (5.2) on page 180 in ISLR)
# write a simple function to use formua (5.2)
loocv = function(fit) {
  h = lm.influence(fit)$hat
  mean((residuals(fit) / (1 - h)) ^ 2) }
loocv(glm_fit)    # 2.246248 -- the test MSE: the average of n test error estimates
# poly()
cv_error = rep(0, 10)
degree = 1:10
for (i in degree) {
  glm_fit = glm(Rating ~ poly(Reviews, i), data = GP) 
  loocv = function(fit) {
    h = lm.influence(fit)$hat
    mean((residuals(fit) / (1 - h)) ^ 2) }
  cv_error[i] = loocv(glm_fit)
}
cv_error
plot(degree, cv_error, type = "b") 
cv_error = rep(0, 10)
degree = 1:10
for (i in degree) {
  glm_fit = glm(Rating ~ poly(Size, i), data = GP) 
  loocv = function(fit) {
    h = lm.influence(fit)$hat
    mean((residuals(fit) / (1 - h)) ^ 2) }
  cv_error[i] = loocv(glm_fit)
}
cv_error
plot(degree, cv_error, type = "b") 
cv_error = rep(0, 10)
degree = 1:10
for (i in degree) {
  glm_fit = glm(Rating ~ poly(Installs, i), data = GP) 
  loocv = function(fit) {
    h = lm.influence(fit)$hat
    mean((residuals(fit) / (1 - h)) ^ 2) }
  cv_error[i] = loocv(glm_fit)
}
cv_error
plot(degree, cv_error, type = "b") 
cv_error = rep(0, 10)
degree = 1:10
for (i in degree) {
  glm_fit = glm(Rating ~ poly(Current.Ver, i), data = GP) 
  loocv = function(fit) {
    h = lm.influence(fit)$hat
    mean((residuals(fit) / (1 - h)) ^ 2) }
  cv_error[i] = loocv(glm_fit)
}
cv_error
plot(degree, cv_error, type = "b") 
# Size: 6, Reviews: 6/7, Current.Ver: 7/8, Installs: 5
fit_poly = lm(Rating ~ poly(Installs,5)+poly(Reviews, 6)+poly(Size, 6)+poly(Current.Ver, 7), data = GP) 
summary(fit_poly)
loocv = function(fit) {
  h = lm.influence(fit)$hat
  mean((residuals(fit) / (1 - h)) ^ 2) }
loocv(fit_poly ) # 1.472604
par(mfrow = c(2, 2)) 
plot(fit_poly)

# regression tree
# install.packages(c("tree","randomForest", "gbm"))
library("tree")
train <- sample(1:nrow(GP), nrow(GP) / 2)
tree.GP <- tree(Rating ~ Reviews+Installs+Current.Ver+Size, GP, subset = train)
summary(tree.GP)
plot(tree.GP)
text(tree.GP, pretty = 0)
yhat <- predict(tree.GP, newdata = GP[-train,])
GP.test <- GP[-train, "Rating"]
mean((yhat - GP.test) ^ 2)

# prune the tree
cv.GP <- cv.tree(tree.GP)
plot(cv.GP$size, cv.GP$dev, type = "b")
prune.GP <- prune.tree(tree.GP, best = 2)
plot(prune.GP)
text(prune.GP, pretty = 0)
yhat <- predict(prune.GP, newdata = GP[-train,])
GP.test <- GP[-train, "Rating"]
plot(yhat, GP.test)
abline(0, 1)
mean((yhat - GP.test) ^ 2)

## Bagging and Random Forest
library(randomForest)
bag.GP <- randomForest(Rating~Reviews+Installs+Current.Ver+Size, data=GP, subset=train, mtry=10, importance=TRUE)
bag.GP
yhat.bag <-predict(bag.GP, newdata=GP[-train,])
plot(yhat.bag, GP.test)
abline(0,1, col="red")
mean((yhat.bag-GP.test)^2)
##view the importance of each variable
importance(bag.GP)
varImpPlot(bag.GP)
## The plot indicates Reviews and Installs are two most important variables

## Growing a random forest
rf.GP <- randomForest(Rating~Reviews+Installs+Current.Ver+Size, data=GP, subset=train, mtry=2, importance=TRUE, n.tree = 5000)
yhat.rf <-predict(rf.GP, newdata=GP[-train,])
mean((yhat.rf-GP.test)^2)

## Boosting
library(gbm)
boost.GP = gbm( Rating ~ Reviews+Installs+Current.Ver+Size, data = GP[train , ], distribution = "gaussian",
                n.trees = 5000, interaction.depth = 4
)
summary(boost.GP)

par(mfrow = c(1,2))
plot(boost.GP, i="Reviews")
plot(boost.GP, i="Installs")

yhat.boost = predict(boost.GP, newdata = GP[-train, ], n.trees = 5000)
mean((yhat.boost - GP.test) ^ 2)

# ridge regression: MSE = 2.292258, lambda=1.417041
# install.packages("glmnet")
library(glmnet)
x <-model.matrix(Rating~Reviews+Installs+Current.Ver+Size, data=GP)
y <-GP$Rating
train <- sample(1:nrow(GP), nrow(GP) / 2)
test <- (-train)
y.test <- y[test]
cv.out = cv.glmnet(x[train, ], y[train], alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.1se
bestlam
ridge.mod<-glmnet(x[train, ], y[train], alpha=0, lambda=bestlam)
ridge.pred <- predict(ridge.mod, s=bestlam, newx=x[test,])
mean((ridge.pred-y.test)^2)

# lasso regression: MSE=2.289571, lambda=0.122016
cv.out =cv.glmnet(x[train, ], y[train], alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.1se
bestlam
lasso.mod=glmnet(x[train, ], y[train], alpha=1, lambda=bestlam)
lasso.pred=predict(lasso.mod, s=bestlam, newx=x[test,])
mean((ridge.pred-y.test)^2)

# principal components regression: 3.066226
# install.packages("pls")
library(pls)
pcr.fit=pcr(Rating~Reviews+Installs+Current.Ver+Size, data=GP, subset=train,scale=TRUE, validation="CV")
summary(pcr.fit)
validationplot(pcr.fit, val.type="MSEP")
pcr.pred=predict(pcr.fit, newx=x[test,], ncomp=1)
mean((pcr.pred-y.test)^2)

# partial least squares: MSE=3.125727
pls.fit=plsr(Rating~Reviews+Installs+Current.Ver+Size, data=GP, subset=train,scale=TRUE, validation="CV")
validationplot(pcr.fit, val.type="MSEP")
pls.pred=predict(pls.fit, newx=x[test,], ncomp=1)
mean((pls.pred-y.test)^2)
