#importing the dataset
ds  = read.csv("jee.csv")

#feature selection
ds = ds[,c(5,7,8,9)]


ds$Gender = as.numeric(factor(ds$Gender,
                              levels = c('male', 'female'),
                              labels = c(1,2)))

#split datset to test and train sets
set.seed(453576)
split = sample.split(ds$Score, SplitRatio = 0.8)
training_set = subset(ds, split == TRUE)
test_set = subset(ds, split == FALSE)

# Feature Scaling
training_set[] = scale(training_set[])
test_set[] = scale(test_set[])

library(h2o)
localH2O <- h2o.init(nthreads = -1)

train.h2o = as.h2o(training_set[])
test.h2o = as.h2o(test_set[])

reg = h2o.glm( y = "Score", 
               x = c("Gender","AvgNumberofHrsStudied","PercentInClass12"), 
               training_frame = train.h2o, 
               family = "gaussian", 
               nfolds = 10)

h2o.performance(reg)

y_pred = as.data.frame(h2o.predict(reg, test.h2o))

plot(test_set$PercentInClass12 ~ test_set$Score )
plot(test_set$PercentInClass12 ~ y_pred$predict )


