
## Directory for images
target_directory = choose.files()

target_directory = read.csv(target_directory)
unique(target_directory$Finding.Labels)

target_directory$Cardiomegaly = 3
target_directory$Effusion = 3
target_directory$Emphysema = 3
target_directory$Hernia = 3
target_directory$Mass = 3
target_directory$Infiltration = 3
target_directory$Atelectasis = 3
target_directory$Pneumothorax = 3
target_directory$Pleural_Thickening = 3
target_directory$Fibrosis = 3
target_directory$Edema = 3



for (i in 1:nrow(target_directory)){
  if(target_directory$Finding.Labels[i] == "No Finding")
  {
    target_directory$pathos[i] = 0
  }else if(target_directory$Finding.Labels[i] == "No Finding")
  {
    target_directory$pathos[i] = 0
  }else
  {
    target_directory$pathos[i] = 1
  }
  if( pmatch("Cardiomegaly", target_directory$Finding.Labels[i], nomatch = 0, duplicates.ok = FALSE)==1)
  {
    target_directory$Cardiomegaly[i] = 1
  } else {target_directory$Cardiomegaly[i] = 0}
  if( pmatch("Effusion", target_directory$Finding.Labels[i], nomatch = 0, duplicates.ok = FALSE)==1)
  {
    target_directory$Effusion[i] = 1
  }  else {target_directory$Effusion[i] = 0}
  if( pmatch("Emphysema", target_directory$Finding.Labels[i], nomatch = 0, duplicates.ok = FALSE)==1)
  {
    target_directory$Emphysema[i] = 1
  } else {target_directory$Emphysema[i] = 0}
  if( pmatch("Hernia", target_directory$Finding.Labels[i], nomatch = 0, duplicates.ok = FALSE)==1)
  {
    target_directory$Hernia[i] = 1
  } else {target_directory$Hernia[i] = 0}
  if( pmatch("Mass", target_directory$Finding.Labels[i], nomatch = 0, duplicates.ok = FALSE)==1)
  {
    target_directory$Mass[i] = 1
  } else {target_directory$Mass[i] = 0}
  if( pmatch("Edema", target_directory$Finding.Labels[i], nomatch = 0, duplicates.ok = FALSE)==1)
  {
    target_directory$Edema[i] = 1
  } else {target_directory$Edema[i] = 0}
  
  if( pmatch("Infiltration", target_directory$Finding.Labels[i], nomatch = 0, duplicates.ok = FALSE)==1)
  {
    target_directory$Infiltration[i] = 1
  } else {target_directory$Infiltration[i] = 0}
  
  if( pmatch("Atelectasis", target_directory$Finding.Labels[i], nomatch = 0, duplicates.ok = FALSE)==1)
  {
    target_directory$Atelectasis[i] = 1
  } else {target_directory$Atelectasis[i] = 0}
  
  if( pmatch("Pneumothorax", target_directory$Finding.Labels[i], nomatch = 0, duplicates.ok = FALSE)==1)
  {
    target_directory$Pneumothorax[i] = 1
  } else {target_directory$Pneumothorax[i] = 0}
  
  if( pmatch("Pleural_Thickening", target_directory$Finding.Labels[i], nomatch = 0, duplicates.ok = FALSE)==1)
  {
    target_directory$Pleural_Thickening[i] = 1
  } else {target_directory$Pleural_Thickening[i] = 0}
  
  if( pmatch("Fibrosis", target_directory$Finding.Labels[i], nomatch = 0, duplicates.ok = FALSE)==1)
  {
    target_directory$Fibrosis[i] = 1
  } else {target_directory$Fibrosis[i] = 0}
  
}


write.csv(target_directory,"appended.csv")

library(readr)

target_directory =  read_csv("appended.csv", col_types = cols(X1 = col_skip()))

noPathos = 0
yesPathos=0

firstFolder = target_directory[1:4998,1:ncol(target_directory)]


noPathos = na.omit(noPathos)
yesPathos=na.omit(yesPathos)

noPathos <- rbind(noPathos, firstFolder[which(firstFolder$pathos==0),])
yesPathos = rbind(yesPathos, firstFolder[which(firstFolder$pathos==1),])
#yesPathos =  na.omit(yesPathos)

#here is where we can generate samples at 50:50

sampleSize = 500

yesPathosSample =yesPathos[sample(nrow(yesPathos), sampleSize), ]

noPathosSample = noPathos[sample(nrow(noPathos), sampleSize), ]


trainingSample = rbind(yesPathosSample,noPathosSample)


trainingSample$Image.Index[2]
saveRDS(trainingSample,"trainingSample.rds")
image_dir
image_dir <- choose.dir()
saveRDS(image_dir, "image_dir.rds")


width <- 200
height <- 200
# example_image <- readImage(file.path(image_dir, trainingSample$Image.Index[2]))
# display(img_resized)
library(EBImage)
image_dir = readRDS("image_dir.rds")
trainingSample = readRDS("trainingSample.rds")
img <- readImage(file.path(image_dir, trainingSample$Image.Index[1]))
## Resize image
img_resized <- resize(img, w = width, h = height)
## Set to grayscale
grayimg <- channel(img_resized, "gray")
## Get the image as a matrix
img_matrix <- grayimg@.Data
## Coerce to a vector
img_vector <- (as.vector(t(img_matrix)))
output =  as.data.frame(t(img_vector))
for(i in 1:nrow(trainingSample))
  
{
  img <- readImage(file.path(image_dir, trainingSample$Image.Index[i]))
  ## Resize image
  img_resized <- resize(img, w = width, h = height)
  ## Set to grayscale
  grayimg <- channel(img_resized, "gray")
  ## Get the image as a matrix
  img_matrix <- grayimg@.Data
  ## Coerce to a vector
  img_vector <- (as.vector(t(img_matrix)))
  output = rbind(output, as.data.frame(t(img_vector)))
}

#write.csv(output[2:nrow(output),],"output.csv")

#output = read.csv("output.csv")
readyForTraining = cbind(trainingSample$pathos, output[2:nrow(output),])

#write.csv(readyForTraining,"readyForTraining.csv")
saveRDS(readyForTraining, "readyForTraining.rds")

#readyForTraining1 = readRDS("readyForTraining.rds")





## Set width and height for resizing images

# source("https://bioconductor.org/biocLite.R")
# biocLite("EBImage")
library(EBImage)
#example_cat_image <- readImage(file.path(image_dir, "cat.0.jpg"))
#display(example_cat_image)

#example_dog_image <- readImage(file.path(image_dir, "dog.0.jpg"))
#display(example_dog_image)


## pbapply is a library to add progress bar *apply functions
## pblapply will replace lapply
# library(pbapply)
# extract_feature <- function(dir_path, width, height, is_cat = TRUE, add_label = TRUE) {
#   img_size <- width*height
#   ## List images in path
#   images_names <- list.files(dir_path)
#   if (add_label) {
#     ## Select only cats or dogs images
#     images_names <- images_names[grepl(ifelse(is_cat, "cat", "dog"), images_names)]
#     ## Set label, cat = 0, dog = 1
#     label <- ifelse(is_cat, 0, 1)
#   }
#   print(paste("Start processing", length(images_names), "images"))
#   ## This function will resize an image, turn it into greyscale
#   feature_list <- pblapply(images_names, function(imgname) {
#     ## Read image
#     img <- readImage(file.path(dir_path, imgname))
#     ## Resize image
#     img_resized <- resize(img, w = width, h = height)
#     ## Set to grayscale
#     grayimg <- channel(img_resized, "gray")
#     ## Get the image as a matrix
#     img_matrix <- grayimg@.Data
#     ## Coerce to a vector
#     img_vector <- as.vector(t(img_matrix))
#     return(img_vector)
#   })
#   ## bind the list of vector into matrix
#   feature_matrix <- do.call(rbind, feature_list)
#   feature_matrix <- as.data.frame(feature_matrix)
#   ## Set names
#   names(feature_matrix) <- paste0("pixel", c(1:img_size))
#   if (add_label) {
#     ## Add label
#     feature_matrix <- cbind(label = label, feature_matrix)
#   }
#   return(feature_matrix)
# }
# 
# 
# cats_data <- extract_feature(dir_path = image_dir, width = width, height = height)
# dogs_data <- extract_feature(dir_path = image_dir, width = width, height = height, is_cat = FALSE)
# dim(cats_data)
# 
# dim(dogs_data)
# 
# saveRDS(cats_data, "cat.rds")
# saveRDS(dogs_data, "dog.rds")
# 
# library(caret)
# ## Bind rows in a single dataset
# complete_set <- rbind(cats_data, dogs_data)
# ## test/training partitions
# training_index <- createDataPartition(complete_set$label, p = .9, times = 1)
# training_index <- unlist(training_index)
# train_set <- complete_set[training_index,]
# dim(train_set)
# 
# library(caret)
# ## Bind rows in a single dataset
# complete_set <- rbind(cats_data, dogs_data)
# complete_set = readyForTraining
# #saveRDS(complete_set,"complete_set")



#-------------> Start Iteration Here <-------------------------------

complete_set=readRDS("readyForTraining.rds")
library(caret)

## test/training partitions
training_index <- createDataPartition(complete_set$`trainingSample$pathos`, p = .9, times = 1)
training_index <- unlist(training_index)
train_set <- complete_set[training_index,]
dim(train_set)

test_set <- complete_set[-training_index,]
dim(test_set)



## Fix train and test datasets
train_data <- data.matrix(train_set)
#saveRDS(train_data,"train_data")

train_x <- t(train_data[, -1])
train_y <- train_data[,1]
train_array <- train_x
width <- 200
height <- 200
dim(train_array) <- c(width, height, 1, ncol(train_x))
saveRDS(train_array,"train_array.rds")
saveRDS(train_y,"train_y.rds")

test_data <- data.matrix(test_set)
test_x <- t(test_set[,-1])
test_y <- test_set[,1]
test_array <- test_x
dim(test_array) <- c(width, height, 1, ncol(test_x))
saveRDS(test_array,"test_array.rds")
saveRDS(test_y,"test_y.rds")

library(mxnet)
## Model
## Model
mx_data <- mx.symbol.Variable('data')
## 1st convolutional layer 5x5 kernel and 20 filters.
conv_1 <- mx.symbol.Convolution(data = mx_data, kernel = c(5, 5), num_filter = 20)
tanh_1 <- mx.symbol.Activation(data = conv_1, act_type = "tanh")
pool_1 <- mx.symbol.Pooling(data = tanh_1, pool_type = "max", kernel = c(2, 2), stride = c(2,2 ))
## 2nd convolutional layer 5x5 kernel and 50 filters.
conv_2 <- mx.symbol.Convolution(data = pool_1, kernel = c(5,5), num_filter = 50)
tanh_2 <- mx.symbol.Activation(data = conv_2, act_type = "tanh")
pool_2 <- mx.symbol.Pooling(data = tanh_2, pool_type = "max", kernel = c(2, 2), stride = c(2, 2))
## 1st fully connected layer
flat <- mx.symbol.Flatten(data = pool_2)
fcl_1 <- mx.symbol.FullyConnected(data = flat, num_hidden = 500)
tanh_3 <- mx.symbol.Activation(data = fcl_1, act_type = "tanh")
## 2nd fully connected layer
fcl_2 <- mx.symbol.FullyConnected(data = tanh_3, num_hidden = 2)
## Output
NN_model <- mx.symbol.SoftmaxOutput(data = fcl_2)

## Set seed for reproducibility
mx.set.seed(100)

## Device used. Sadly not the GPU :-(
device <- mx.cpu()

## Train on 1200 samples
model <- mx.model.FeedForward.create(NN_model, X = readRDS("train_array.rds"), y = readRDS("train_y.rds"),
                                     ctx = device,
                                     
                                     num.round = 10,
                                     array.batch.size = 2,
                                     learning.rate = 0.05,
                                     momentum = 0.9,
                                     wd = 0.00001,
                                     eval.metric = mx.metric.accuracy,
                                     epoch.end.callback = mx.callback.log.train.metric(100))



## Test test set

predict_probs <- predict(model, test_array)
predict_probs
predicted_labels <- max.col(t(predict_probs)) - 1
table(test_data[, 1], predicted_labels)

paste( "Tested Model Accuracy is currently: ", (sum(diag(table(test_data[, 1], predicted_labels)))/nrow(test_data))*100,"%")

predicted_labels
