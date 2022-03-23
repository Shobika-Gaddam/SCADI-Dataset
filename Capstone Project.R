library(caret)
library(dplyr)
library(Rtsne)
data <- read.csv("C:/Users/shobi/OneDrive/Desktop/Woxsen/Term 2/R Programming/SCADI.csv")

data <- mutate_at(data, vars(-one_of("Age")), as.factor)
data <- data[,lapply(data, nlevels) >= 2]
summary(data)
colors = rainbow(length(unique(data$Classes)))
names(colors) = unique(data$Classes)
tsne <- Rtsne(data[,-1], dims = 2, perplexity=15, verbose=TRUE, max_iter = 500)
plot(tsne$Y, t='n', main="T-SNE")
text(tsne$Y, labels=data$Classes, col=colors[data$Classes])
indTraining <- createDataPartition(data$Classes, p = .8, list = FALSE)
training <- data[indTraining,]
test <- data[-indTraining,]
train_control <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 10,
  verboseIter = TRUE,
  allowParallel = TRUE
)
data.tree <- train(Classes ~ .,
                   data = training,
                   method = "RRF",
                   trControl = train_control)
plot(data.tree)
plot(data.tree$finalModel)
pred_vals <- predict(data.tree, test)
true_vals <- test$Classes
confusionMatrix(pred_vals, true_vals)
