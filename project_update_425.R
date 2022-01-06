#Tyler Gilbert
library(funModeling) 
library(Hmisc)
library (ISLR)
library(ggplot2)
library(ggcorrplot)
library(class)
library(rpart)
library(rpart.plot)
library (MASS)
library (tree)
library (randomForest)
library (gbm)
library("factoextra")



mush = read.csv("mushrooms.csv", header = T)
attach(mush)

dim(mush)

freq(mush)


ggplot(mush, aes(odor, colour = class)) + 
  geom_bar()
#----------------------------
i = 1:23
mush[,i] = apply(mush[,i], 2, function(x) as.numeric(as.factor(x))) #making data numeric
mush = mush[,-17]
attach(mush)

#<------------------------------>
#KNN

set.seed(1209)

sample_size = round(nrow(mush)*.70)
index = sample(seq_len(nrow(mush)), size = sample_size)

mush.train = mush[index,2:22]
mush.test = mush[-index,2:22]
mush.class = mush$class[index]
mush.true = mush$class[-index]

knn.mush = knn(mush.train, mush.test, mush.class , k=1)
print(table(knn.mush, mush.true))
mush.tab = table(knn.mush, mush.true)
knn.acc = sum(diag(mush.tab))/sum(mush.tab)
knn.acc

library(caret)

trControl <- trainControl(method  = "cv",
                          number  = 5)

fit <- train(as.factor(class) ~ .,
             method     = "knn",
             tuneGrid   = expand.grid(k = 1:10),
             trControl  = trControl,
             metric     = "Accuracy",
             data       = mush.train2)

Accuracy = c()
K = c()
for (i in c(1:100)){
  knn.mush = knn(mush.train, mush.test, mush.class , k=i)
  mush.tab = table(knn.mush, mush.true)
  knn.acc = sum(diag(mush.tab))/sum(mush.tab)
  Accuracy = append(Accuracy,knn.acc)
  K = append(K,i)
}
plot(K,Accuracy,type = 'o', main = "Cross Validation")

#perfect accuracy with k = 1

#<------------------------------->
### Tree methods/Rpart

set.seed(111)
train2=sample(1: nrow(mush), 6500) 
mush.train2 = mush[train2,] # creating training set
mush.test2 =mush[-train2 ,]         # creating test set
class.test=class[-train2 ]

tree.mush = tree(as.factor(class)~.,data = mush.train2)
summary(tree.mush )

tree.pred = predict(tree.mush , mush.test2 ,type = "class")
table(tree.pred ,class.test) 
tree.tab2 = table(tree.pred,class.test) 
tree.acc2 = sum(diag(tree.tab2))/sum(tree.tab2)
tree.acc2

plot(tree.mush )
text(tree.mush , pretty =0)

cv.mush =cv.tree(tree.mush)    # cross-validation 
plot(cv.mush$size , cv.mush$dev , type="b")

prune.mush =prune.tree(tree.mush, best = 5)
plot(prune.mush )
text(prune.mush ,pretty =0)
summary(prune.mush)
#boston.test= Boston [-train , "medv"]
test.mush = mush[-train2, "class"]

tree.pred.mush = predict(prune.mush , mush.test2 ,type = "class")
table(tree.pred.mush ,class.test) 
tree.tab = table(tree.pred.mush ,class.test) 
tree.acc = sum(diag(tree.tab))/sum(tree.tab)
tree.acc
#pruned tree accuracy: 96.43%


#Bagging
bag.mush =randomForest(as.factor(class)~.,data=mush.train2,mtry=22, importance =TRUE)  
bag.mush
bag.pred = predict(bag.mush, newdata = mush.test2)
table(bag.pred, class.test)
tree.tab.bag = table(bag.pred ,class.test) 
tree.acc.bag = sum(diag(tree.tab.bag))/sum(tree.tab.bag)
tree.acc.bag

yhat.bag.mush = predict(bag.mush ,newdata =mush [-train2 ,])
plot(yhat.bag.mush , test.mush)
abline (0,1)
mean(( as.numeric(yhat.bag.mush) - as.numeric(test.mush))^2)
#perfect accuracy (i think)

#Random Forest
rf.mush =randomForest(as.factor(class)~.,data=mush.train2 ,mtry=8, importance =TRUE) 
rf.mush
rf.pred = predict(rf.mush, newdata = mush.test2)
table(rf.pred, class.test)
tree.tab.rf = table(rf.pred ,class.test) 
tree.acc.rf = sum(diag(tree.tab.rf))/sum(tree.tab.rf)
tree.acc.rf
#perfect accuracy (i think)

importance(rf.mush)
varImpPlot(rf.mush)

#Boosting
set.seed (1)
boost.mush =gbm(as.factor(class)~.,data=mush[train2,], distribution="gaussian", n.trees =5000 , interaction.depth = 4)
summary(boost.mush)

boost.pred = predict(boost.mush, newdata = mush.test2)
table(boost.pred, class.test)
tree.tab.boost = table(boost.pred ,class.test) 
tree.acc.boost = sum(diag(tree.tab.boost))/sum(tree.tab.boost)
tree.acc.boost

yhat.boost.mush =predict(boost.mush ,newdata =mush[-train2 ,], n.trees =5000)
mean(( yhat.boost.mush-test.mush)^2) 


#RPart
mush_rpart = rpart(as.factor(class)~., data = mush.train2, method = "class")
prp(mush_rpart)
plotcp(mush_rpart)
min_cp = mush_rpart$cptable[which.min(mush_rpart$cptable[,"xerror"]),"CP"]
min_cp
mush_rpart_prune = prune(mush_rpart, cp = min_cp)
prp(mush_rpart_prune)
rpart.plot(mush_rpart_prune)

rpart.pred = predict(mush_rpart_prune, mush.test2, type = "class")
table(rpart.pred ,class.test)
tree.tab.rp = table(rpart.pred ,class.test) 
tree.acc.rp = sum(diag(tree.tab.rp))/sum(tree.tab.rp)
tree.acc.rp
#<------------------------------------->
#logistic regression

mush.glm = glm(as.factor(mush$class)~., data = mush, family = "binomial")
summary(mush.glm)



#i think logistic regression is most useful for inference in this case.
#veil.color and gill.attachment, which are the two most insignificant features,
#also are the features that have the lowest mean decrease accuracy score from the
#random forest VarImpPlot.
# 
# library(rgl)
# 
# mush.pr <- prcomp(mush, center = TRUE, scale = TRUE)
# 
# plot3d(mush.pr$x[,1:3], col=mush$class)
# 
# 
# fviz_pca_ind(mush.pr, geom.ind = "point", pointshape = 21, 
#              pointsize = 2, 
#              fill.ind = mush$class, 
#              col.ind = "black", 
#              addEllipses = F,
#              label = "var",
#              col.var = "black",
#              repel = TRUE,
#              legend.title = "Class") +
#   ggtitle("2D PCA-plot from 22 feature dataset") +
#   theme(plot.title = element_text(hjust = 0.5)) 
# 
# 
# mush_rpart = rpart(as.factor(class)~., data = mush, method = "class")
# #plotcp(mush_rpart)
# min_cp = mush_rpart$cptable[which.min(mush_rpart$cptable[,"xerror"]),"CP"]
# #min_cp
# mush_rpart_prune = prune(mush_rpart, cp = min_cp)
# prp(mush_rpart_prune)
# 
# 
# 
# new.tree = prp(mush_rpart_prune, snip = TRUE)$obj
# prp(new.tree)
# 
# # return the given node and all its ancestors (a vector of node numbers)
# path.to.root <- function(node)
# {
#   if(node == 1) # root?
#     node
#   else # recurse, %/% 2 gives the parent of node
#     c(node, path.to.root(node %/% 2))
# }
# node <- 11 # 11 is our chosen node, arbitrary for this example
# nodes <- as.numeric(row.names(mush_rpart_prune$frame))
# cols <- ifelse(nodes %in% path.to.root(node), "sienna", "gray")
# prp(mush_rpart_prune, nn = T,
#     col = cols, branch.col = cols, split.col = cols, nn.col = cols)
# 
# 
# 
# mush2 = mush[,c(1,5,6,9,10,11,20,21,22)]
# 
# for (i in c(1:8124)){
#   if (mush2[i,1] == 1){
#     mush2[i,1] = "Edible"
#   } else {
#     mush2[i,1] = "Poisonous"
#   }
# }
#   
# 
# #mush2$class = c("Poisonous", "Edible")
# split.fun <- function(x, labs, digits, varlen, faclen)
# {
#   # replace commas with spaces (needed for strwrap)
#   labs <- gsub(",", " ", labs)
#   for(i in 1:length(labs)) {
#     # split labs[i] into multiple lines
#     labs[i] <- paste(strwrap(labs[i], width = 15), collapse = "\n")
#   }
#   labs
# }
# 
# mush_rpart2 = rpart(as.factor(mush2$class)~., data = mush2, method = "class")
# #plotcp(mush_rpart2)
# min_cp = mush_rpart2$cptable[which.min(mush_rpart2$cptable[,"xerror"]),"CP"]
# #min_cp
# mush_rpart_prune2 = prune(mush_rpart2, cp = min_cp)
# prp(mush_rpart_prune2, varlen = 0 )
# #rpart.plot(mush_rpart_prune2,split.fun = split.fun, )
# 
# p2 = data.frame(bruises = as.numeric(1), odor = as.numeric(2),
#                 gill.size = as.numeric(2), gill.color = as.numeric(2), stalk.shape = as.numeric(2),
#                 spore.print.color = as.numeric(2), population = as.numeric(2), habitat = as.numeric(2))
# 
# p2
# predict(mush_rpart_prune2, newdata = p2, type = "class")
# 
# 
