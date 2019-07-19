#loading the ROSE((Random Over-Sampling Examples)
library(ROSE)

#DATA
data(hacide)

#Structure of data
str(hacide.train)

#over sample example
table(hacide.train$cls)

#tree model
library(rpart)

#model
treeimb <- rpart(cls ~ ., data = hacide.train)

#prediction
pred.treeimb <- predict(treeimb, newdata = hacide.test)
head(pred.treeimb)

#accuracy matrics
accuracy.meas(hacide.test$cls, pred.treeimb[,2])

#ROC Curve
roc.curve(hacide.test$cls, pred.treeimb[,2], plotit = TRUE)

#over sampling
data.bal.ov.N <- ovun.sample(cls ~ ., data = hacide.train, method = "over",
                             N = 1960)$data
#see data
table(data.bal.ov.N$cls)

#percentage
data.bal.ov.p <- ovun.sample(cls ~ ., data = hacide.train, method = "over",
                             p = 0.5)$data
#see table
table(data.bal.ov.p$cls)

#over
data.bal.ov <- ovun.sample(cls ~ ., data = hacide.train, method = "over",
                           p = 0.5, seed = 1)$data

#under
data.bal.un <- ovun.sample(cls ~ ., data = hacide.train, method = "under",
                           p = 0.5, seed = 1)$data
table(data.bal.un$cls)
#both
data.bal.ou <- ovun.sample(cls ~ ., data = hacide.train, method = "both",
                           N = 1000, p = 0.5, seed = 1)$data

table(data.bal.ou$cls)


data.rose <- ROSE(cls ~ ., data = hacide.train, seed = 1)$data
table(data.rose$cls)


data.rose.h <- ROSE(cls ~ ., data = hacide.train, seed = 1, hmult.majo = 0.25,
                    hmult.mino = 0.5)$data

tree.rose <- rpart(cls ~ ., data = data.rose)
tree.ov <- rpart(cls ~ ., data = data.bal.ov)
tree.un <- rpart(cls ~ ., data = data.bal.un)
tree.ou <- rpart(cls ~ ., data = data.bal.ou)
pred.tree.rose <- predict(tree.rose, newdata = hacide.test)
pred.tree.ov <- predict(tree.ov, newdata = hacide.test)
pred.tree.un <- predict(tree.un, newdata = hacide.test)
pred.tree.ou <- predict(tree.un, newdata = hacide.test)

roc.curve(hacide.test$cls, pred.tree.rose[,2])

ROSE.holdout <- ROSE.eval(cls ~ ., data = hacide.train, learner = rpart,
                          method.assess = "holdout", extr.pred = function(obj)obj[,2], seed = 1)

ROSE.holdout


ROSE.boot <- ROSE.eval(cls ~ ., data = hacide.train, learner = rpart,
                       method.assess = "BOOT", B = 50, extr.pred = function(obj)obj[,2],
                       seed = 1, trace = TRUE)

summary(ROSE.boot)
