## ---------------------------------------------------
load("trainsData.RData")


## ---------------------------------------------------
# <!-- install.packages("caret") -->
library(caret)


## ---------------------------------------------------
# <!-- install.packages("xgboost") -->
library(xgboost)


## ---------------------------------------------------
# <!-- install.packages("randomForest") -->
library(randomForest)


## ---------------------------------------------------
# @title textToSeconds
#手册上的代码，用于将时间转化为数字，可以进行相减操作
#The code in the manual is used to convert time into numbers and can be subtracted.
textToSeconds <- function(textTime){
  seconds <- as.numeric(strsplit(textTime,split = ":")[[1]]) %*% c(60*60,60,1)
  return(as.numeric(seconds))
}


## ---------------------------------------------------
initializeMatrixData <- function(targetData){
  #初始化一个dataframe
  matrixData <- data.frame(
  day=rep(" ", length(targetData)),
  delayLeeds=rep(0, length(targetData)),
  delayNORMNTN=rep(0, length(targetData)),
  delayNORMNTN2=rep(0, length(targetData)),
  delayWKFLDKG=rep(0, length(targetData)),
  delayWKFLDKG2=rep(0, length(targetData)),
  delayBNSLY=rep(0, length(targetData)),
  delayBNSLY2=rep(0, length(targetData)),
  delayMEADWHL=rep(0, length(targetData)),
  delayMEADWHL2=rep(0, length(targetData)),
  delayShef=rep(0, length(targetData)),
  delayNotts=rep(0, length(targetData)),
  stayNOR=rep(0, length(targetData)),
  stayWKF=rep(0, length(targetData)),
  stayBNS=rep(0, length(targetData)),
  stayMEA=rep(0, length(targetData)),
  stringsAsFactors=FALSE
  )
  return(matrixData)
}


## ---------------------------------------------------
# # @title initializeMatrixData(targetData)
# # used for initialize a matrix(full matrix)
# initializeMatrixData <- function(targetData) {
#   len <- length(targetData)
#   # 创建一个包含所有延迟变量名的向量
#   delayVars <- c(
#     "leeds.deptime", "leeds.depsch", "leeds.depdelay", 
#     "stay.normntn","normntn.arrtime", "normntn.arrsch", "normntn.arrdelay",
#     "normntn.deptime", "normntn.depsch", "normntn.depdelay", "hold.normntn",
#     "stay.wkfldkg", "wkfldkg.arrtime", "wkfldkg.arrsch", "wkfldkg.arrdelay",
#     "wkfldkg.deptime", "wkfldkg.depsch", "wkfldkg.depdelay", "hold.wkfldkg",
#     "stay.bnsly", "bnsly.arrtime", "bnsly.arrsch", "bnsly.arrdelay",
#     "bnsly.deptime", "bnsly.depsch", "bnsly.depdelay", "hold.bnsly",
#     "stay.meadwhl", "meadwhl.arrtime", "meadwhl.arrsch", "meadwhl.arrdelay",
#     "meadwhl.deptime", "meadwhl.depsch", "meadwhl.depdelay", "hold.meadwhl",
#     "sheffld.arrtime", "sheffld.arrsch", "sheffld.arrdelay",
#     "delayNotts"
#   )
#   # 初始化数据框，只有day列开始有不同的值
#   matrixData <- data.frame(day = rep(" ", len), stringsAsFactors = FALSE)
#   
#   # 循环赋值
#   for (var in delayVars) {
#     matrixData[[var]] <- rep(0, len)
#   }
#   return(matrixData)
# }



## ---------------------------------------------------
# updateMatrixData <- function(targetData, matrixData) {
#   for(i in 1:length(targetData)) {
#     dummy <- targetData[[i]]  # Get the i-th DataFrame from targetData
#     
#     matrixData$day[i] <- dummy$timings$day.week[1] # Assign the day of the week to matrixData
#     
#     # Initialize 'stay' variables for stations - assume 0 (not stayed) by default
#     matrixData$stay.normntn[i] <- ifelse(any(dummy$timings$departure.from == "NORMNTN"), 1, 0)
#     matrixData$stay.wkfdkg[i] <- ifelse(any(dummy$timings$departure.from == "WKFLDKG"), 1, 0)
#     matrixData$stay.bnsly[i] <- ifelse(any(dummy$timings$departure.from == "BNSLY"), 1, 0)
#     matrixData$stay.meadwhl[i] <- ifelse(any(dummy$timings$departure.from == "MEADWHL"), 1, 0)
# 
#     for(j in 1:nrow(dummy$timings)) {
#       # Compute the seconds from the time strings
#       deptime <- textToSeconds(dummy$timings$departure.time[j])
#       depsch <- textToSeconds(dummy$timings$departure.schedule[j])
#       depdelay <- deptime - depsch
#       arrtime <- textToSeconds(dummy$timings$arrival.time[j])
#       arrsch <- textToSeconds(dummy$timings$arrival.schedule[j])
#       arrdelay <- arrtime - arrsch
# 
#       # Extract the station names for departure and arrival
#       stationFrom <- tolower(gsub("\\.", "", dummy$timings$departure.from[j]))
#       stationTo <- tolower(gsub("\\.", "", dummy$timings$arrival.to[j]))
# 
#       # Assign the computed values to matrixData
#       matrixData[paste0(stationFrom, ".deptime")][i] <- deptime
#       matrixData[paste0(stationFrom, ".depsch")][i] <- depsch
#       matrixData[paste0(stationFrom, ".depdelay")][i] <- depdelay
#       
#       if(j > 1) { # There is no arrival info for the first station
#         arrColName <- paste0(stationTo, ".arrtime")
#         if (!arrColName %in% names(matrixData)) {
#           stop(paste("Column", arrColName, "does not exist in matrixData."))
#         }
#         # matrixData[[arrColName]][i] <- arrtime
#         matrixData[paste0(stationTo, ".arrtime")][i] <- arrtime
#         matrixData[paste0(stationTo, ".arrsch")][i] <- arrsch
#         matrixData[paste0(stationTo, ".arrdelay")][i] <- arrdelay
#       }
#       
#       # Calculate hold time for each station, except for the last one
#       if(j < nrow(dummy$timings)) {
#         nextStationFrom <- tolower(gsub("\\.", "", dummy$timings$departure.from[j + 1]))
#         nextDepTime <- textToSeconds(dummy$timings$departure.time[j + 1])
#         holdTime <- nextDepTime - arrtime
#         
#         # Construct the column name for hold time
#         holdColName <- paste0("hold.", nextStationFrom)
#         
#         # Check if the hold time column exists in matrixData
#         if(!holdColName %in% names(matrixData)) {
#           stop(paste("Column", holdColName, "does not exist in matrixData."))
#         }
#         
#         matrixData[[holdColName]][i] <- holdTime
#       }
#     }
#     
#     # The last station does not have hold time, and it is always 'SHEFFLD' for this dataset
#     matrixData$delayNotts[i] <- dummy$arrival$delay.secs[1]
#   }
#   
#   return(matrixData)
# }




## ---------------------------------------------------
# out = preprocessData(trainingData, historicalCongestion)



## ---------------------------------------------------
#用于合并Congestion和arrival
merge_training_data <- function(trainingData) {
  # 初始化一个空的数据框用于存放最终结果
  final_df <- data.frame()
  
  # 遍历trainingData列表
  for (i in seq_along(trainingData)) {
    dummy <- trainingData[[i]]
    
    # 合并dummy$congestion和dummy$arrival，两者都是一行多列的数据框
    combined <- cbind(dummy$congestion, dummy$arrival)
    
    # 添加识别ID列
    combined$train_code <- dummy$timings$train.code[1]
    
    
    # 将合并后的数据框添加到最终结果中
    final_df <- rbind(final_df, combined)
  }
  
  return(final_df)
}
mergetraindata = merge_training_data(trainingData = trainingData)
write.csv(mergetraindata, file = "trainingDataconarr.csv", row.names=TRUE)
mergetraindata2 = merge_training_data(testData)
write.csv(mergetraindata2, file = "testDataconarr.csv", row.names=TRUE)
write.csv(historicalCongestion,file = "historicalCongestion.csv",row.names = TRUE)
#问题是，train.code实际上并不是唯一的，一共只有15种车辆，所以甚至可以onehot。真正应该做的还是加一列id




## ---------------------------------------------------
# 用于输出主表
merge_timings <- function(trainingData) {
  id <- 0
  # 使用lapply遍历列表中的每个元素，提取dummy$timings
  # 然后使用do.call和rbind将结果合并为一个数据框
  combined_timings <- do.call(rbind, lapply(trainingData, function(dummy) {
    # 在当前dummy$timings数据框中添加一个id列
    dummy$timings$id <- id
    # id递增
    id <<- id + 1
    # 返回修改后的dummy$timings数据框
    return(dummy$timings)
  }))
  
  return(combined_timings)
}


mergertimings = merge_timings(trainingData = trainingData)
write.csv(mergertimings, file = "trainingDatatimings.csv", row.names=TRUE)
mergertimings2 = merge_timings(testData)
write.csv(mergertimings2, file = "testDatatimings.csv", row.names=TRUE)

# 假设mergertimings是你的数据框，departure.from是其中一个列名
# 绘制柱状图并在顶部显示计数
ggplot(mergertimings, aes(x = departure.from)) +
  geom_bar(aes(fill = departure.from)) +  # 填充颜色可按类别区分
  geom_text(stat = 'count', aes(label = ..count.., y = ..count..), vjust = -0.5, position = position_dodge(0.9)) +
  theme_minimal() +
  labs(title = "Departure From Frequency", x = "Departure From", y = "Count") +
  theme(legend.title = element_blank())  # 隐藏图例标题
#图中可以看出，确实极个别车辆是通过WKFLDWG的，这是WKFLDKG的另一个车站。通过NORMNTN的车很少。


## ---------------------------------------------------
# @title updateMatrixData(targetData, matrixData)
# used for make the data into the matrix
updateMatrixData <- function(targetData, matrixData) {
  for(i in 1:length(targetData)){
    dummy <- targetData[[i]]  # Get the original data set first

    matrixData$day[i] <- dummy$timings$day.week[1] # First assign the time to the table

    for(j in 1:nrow(dummy$timings)){
      deptime <- textToSeconds(dummy$timings$departure.time[j])
      depsch <- textToSeconds(dummy$timings$departure.schedule[j])
      depdelay <- deptime - depsch
      arrtime <- textToSeconds(dummy$timings$arrival.time[j])
      arrsch <- textToSeconds(dummy$timings$arrival.schedule[j])
      arrdelay <- arrtime - arrsch
      x <- dummy$timings$departure.from[j]
      y <- dummy$timings$arrival.to[j]

      # Update departure delays
      if(!is.null(depdelay) && !is.na(depdelay)) {
        switch(x,
          "LEEDS" = { matrixData$delayLeeds[i] <- depdelay          },
          "NORMNTN" = {
            matrixData$delayNORMNTN[i] <- depdelay
            matrixData$stayNOR[i] <- 1
          },
          "WKFLDKG" = {
            matrixData$delayWKFLDKG[i] <- depdelay
            matrixData$stayWKF[i] <- 1
            },
          "BNSLY" = {
            matrixData$delayBNSLY[i] <- depdelay
            matrixData$stayBNS[i] <- 1
            },
          "MEADWHL" = {
            matrixData$delayMEADWHL[i] <- depdelay
            matrixData$stayMEA[i] <- 1
            }
        )
      }

      # Update arrival delays
      if(!is.null(arrdelay) && !is.na(arrdelay)) {
        switch(y,
          "NORMNTN" = { matrixData$delayNORMNTN2[i] <- arrdelay },
          "WKFLDKG" = { matrixData$delayWKFLDKG2[i] <- arrdelay },
          "BNSLY" = { matrixData$delayBNSLY2[i] <- arrdelay },
          "MEADWHL" = { matrixData$delayMEADWHL2[i] <- arrdelay },
          "SHEFFLD" = { matrixData$delayShef[i] <- arrdelay }
        )
      }
    }
    matrixData$delayNotts[i] <- dummy$arrival$delay.secs[1]
  }

  return(matrixData)
}



## ---------------------------------------------------
# @title processCongestionAndUpdateFeatures(targetData, matrixData)
processCongestionAndUpdateFeatures <- function(targetData, matrixData) {
  num_rows <- length(targetData)  # 获取行数

  # 初始化matrixData_Con数据框
  matrixData_Con <- data.frame(
    week.day = rep(" ", num_rows),  # 使用空格填充
    hour = rep(0, num_rows),
    Leeds.trains = rep(0, num_rows),
    Leeds.av.delay = rep(0, num_rows),
    Sheffield.trains = rep(0, num_rows),
    Sheffield.av.delay = rep(0, num_rows),
    Nottingham.trains = rep(0, num_rows),
    Nottingham.av.delay = rep(0, num_rows),
    stringsAsFactors = FALSE
  )

  # 遍历targetData填充matrixData_Con
  for(i in 1:length(targetData)) {
    congestion_data <- targetData[[i]]$congestion

    # 更新matrixData_Con
    matrixData_Con$week.day[i] <- congestion_data[1]
    matrixData_Con$hour[i] <- congestion_data[2]
    matrixData_Con$Leeds.trains[i] <- congestion_data[3]
    matrixData_Con$Leeds.av.delay[i] <- congestion_data[4]
    matrixData_Con$Sheffield.trains[i] <- congestion_data[5]
    matrixData_Con$Sheffield.av.delay[i] <- congestion_data[6]
    matrixData_Con$Nottingham.trains[i] <- congestion_data[7]
    matrixData_Con$Nottingham.av.delay[i] <- congestion_data[8]
  }

  # 从"list"转换成数据型单位int和dbl
  matrixData_Con <- data.frame(lapply(matrixData_Con, function(x) if(is.list(x)) unlist(x) else x))

  # 将当前时刻的拥堵情况添加到特征集中
  matrixData2 <- cbind(matrixData, matrixData_Con)

  # 移除多余的'day'列
  matrixData2 <- subset(matrixData2, select = -day)

  return(matrixData2)
}



## ---------------------------------------------------
# @title mergeWithHistoricalCongestion(matrixData2, historicalCongestion)
# 合并历史数据
mergeWithHistoricalCongestion <- function(matrixData2, historicalCongestion) {
  # 使用merge函数将matrixData2和historicalCongestion合并
  # 指定合并的列名，确保使用all.x = TRUE来包含所有matrixData2的行
  matrixData4 = merge(matrixData2, historicalCongestion, by.x=c('week.day','hour'), by.y=c('Day','Hour'), all.x = TRUE)

  return(matrixData4)
}



## ---------------------------------------------------
# @title applyOneHotEncodingAndMerge(matrixData4)
applyOneHotEncodingAndMerge <- function(matrixData4) {
  # 使用model.matrix生成一热编码
  matrixData_onehot2 <- model.matrix(~ week.day - 1, data = matrixData4)
  # 转换为数据框
  matrixData4_onehot <- as.data.frame(matrixData_onehot2)

  # 将生成的一热编码数据框与原数据框合并
  matrixData5 <- cbind(matrixData4, matrixData4_onehot)
  # 从合并后的数据框中移除'week.day'列
  matrixData5 <- subset(matrixData5, select = -week.day)

  return(matrixData5)
}



## ---------------------------------------------------
# @title extractMatrix(matrixData5)
extractMatrix <- function(matrixData5) {
  # 提取目标变量为单独的列
  target_column <- matrixData5$delayNotts

  # 删除目标列并更新数据框
  matrixData6 <- subset(matrixData5, select = -delayNotts)

  # 返回处理后的数据框和目标列
  return(list(matrixData6 = matrixData6, target_column = target_column))
}
# # 参考调用方式：
# result <- extractMatrix(matrixData5)
# matrixData6 <- result$matrixData6
# target_column <- result$target_column



## ---------------------------------------------------
#not used

runModelPipeline <- function(trainingData, testData, historicalCongestion) {
  # 预处理训练数据
  trainingDataProcessed <- processCongestionAndUpdateFeatures(trainingData)
  trainingDataProcessed <- mergeWithHistoricalCongestion(trainingDataProcessed, historicalCongestion)
  trainingDataProcessed <- applyOneHotEncodingAndMerge(trainingDataProcessed)

  # 提取目标变量和特征矩阵
  result <- extractMatrix(trainingDataProcessed)
  targetColumn <- result$target_column
  featuresMatrix <- result$matrixData6

  # 模型训练
  trainControlObj <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
  model <- train(targetColumn ~ ., data = featuresMatrix, method = "rf", trControl = trainControlObj)

  # 预处理测试数据（应用与训练数据相同的预处理步骤）
  testDataProcessed <- processCongestionAndUpdateFeatures(testData)
  testDataProcessed <- mergeWithHistoricalCongestion(testDataProcessed, historicalCongestion)
  testDataProcessed <- applyOneHotEncodingAndMerge(testDataProcessed)

  # 模型预测
  predictions <- predict(model, newdata = testDataProcessed)

  return(predictions)
}



## ---------------------------------------------------
# 数据预处理和特征工程
preprocessData <- function(trainingData, historicalCongestion) {
  # 这里实现数据预处理逻辑
  # 初始化特征矩阵
  matrixData = initializeMatrixData(trainingData)
  # print("succeed1")
  # 将数据填写到矩阵中
  matrixData2 = updateMatrixData(trainingData,matrixData)
  # print("succeed2")
  # 添加现在的congestion数据
  matrixData3 = processCongestionAndUpdateFeatures(trainingData,matrixData2)
  # print("succeed3")
  # 添加historicalCongestion
  matrixData4 = mergeWithHistoricalCongestion(matrixData3, historicalCongestion)
  # print("succeed4")
  # 将日期应用onehot 编码
  matrixData5 = applyOneHotEncodingAndMerge(matrixData4)
  # print("succeed5")

  # output = extractMatrix(matrixData5)
  output = matrixData5
  return(output)#返回一个list,特征矩阵和目标列向量
}

# 模型预测
makePrediction <- function(model, newdata) {
  prediction <- predict(model, newdata)
  return(prediction)
}



## ---------------------------------------------------

trainXGBoostCaret <- function(featureData, targetData) {
  # 确保目标变量是数值型
  if(is.data.frame(targetData)) {
    target_vector <- as.numeric(targetData[[1]]) 
  } else {
    target_vector <- as.numeric(targetData) # 直接转换为数值型向量
  }

  # 定义训练控制
  trainControl <- trainControl(method = "cv", number = 10, verboseIter = TRUE)
  
  # 定义模型的参数网格
  tuneGrid <- expand.grid(
    nrounds = c(30,50,70,100),
    eta = c(0.01, 0.05, 0.1),
    max_depth = c(3, 6, 9),
    gamma = c(0,0.5),
    colsample_bytree = 1,
    min_child_weight = 1,
    subsample = 1
  )

  # 训练模型
  model <- train(
    x = featureData, 
    y = target_vector,
    method = "xgbTree",
    trControl = trainControl,
    tuneGrid = tuneGrid,
    verbose = TRUE
  )

  return(model)
}



## ---------------------------------------------------
performPCA <- function(data, explained_variance_threshold = 95) {
  # 标准化数据：对数据进行预处理，使每个特征的平均值为0，标准差为1
  data_scaled <- scale(data)
  
  # 执行PCA
  pca_result <- prcomp(data_scaled, center = TRUE, scale. = TRUE)
  
  # 计算累计解释的方差比例
  explained_variance_ratio <- cumsum(pca_result$sdev^2 / sum(pca_result$sdev^2)) * 100
  
  # 确定根据阈值要保留的主成分数量
  n_components <- which(explained_variance_ratio >= explained_variance_threshold)[1]
  
  # 提取前n个主成分
  pca_data <- pca_result$x[, 1:n_components]
  
  # 返回降维后的数据、PCA模型，和解释的方差比例
  list(pca_data = pca_data, pca_model = pca_result, explained_variance_ratio = explained_variance_ratio)
}



## ---------------------------------------------------
# 线性回归


trainLinearModel <- function(featureData, targetData) {
  # 确保目标数据是正确的格式
  if(is.data.frame(targetData)) {
    target_vector <- targetData[[1]] # 假设目标变量是第一列
  } else {
    target_vector <- targetData # 如果已经是向量
  }

  # 创建数据框，以适应caret的train函数输入格式
  data_for_lm <- cbind(featureData, target = target_vector)
  
  # 设置交叉验证的控制参数
  train_control <- trainControl(method = "cv", number = 10) # 10折交叉验证
  
  # 使用caret的train函数训练模型
  model <- train(target ~ ., data = data_for_lm, method = "lm", trControl = train_control)
  
  return(model)
}

#随机森林
trainRandomForest <- function(featureData, targetData) {
  # 对于回归问题，确保目标数据是数值型向量
  if(is.data.frame(targetData)) {
    target_vector <- as.numeric(targetData[[1]]) # 保持为数值型，适用于回归问题
  } else {
    target_vector <- as.numeric(targetData) # 直接使用数值型向量
  }

  # 将特征数据和目标变量合并为一个新的数据框，以适应caret的train函数输入格式
  data_for_rf <- cbind(featureData, target = target_vector)
  
  # 设置交叉验证的控制参数
  train_control <- trainControl(method = "cv", number = 10) # 10折交叉验证
  
  # 使用caret的train函数训练模型
  model <- train(target ~ ., data = data_for_rf, method = "rf", trControl = train_control)
  
  return(model)
}



#xgboost
trainXGBoost <- function(featureData, targetData) {
  # 将特征数据DataFrame转换为矩阵
  data_matrix <- as.matrix(featureData)

  # 确保目标变量是数值型向量
  # 如果targetData是DataFrame，假设它只有一列，且这列就是目标变量
  if(is.data.frame(targetData)) {
    target_vector <- as.numeric(targetData[[1]]) # 假设目标变量是第一列
  } else {
    target_vector <- as.numeric(targetData) # 直接转换为数值型向量
  }

  # 定义XGBoost参数
  params <- list(
    objective = "reg:squarederror",
    eta = 0.1,
    max_depth = 6,
    subsample = 0.75,
    colsample_bytree = 0.75
  )

  # 执行XGBoost交叉验证
  cv_results <- xgb.cv(
    params = params,
    data = data_matrix,
    label = target_vector,
    nrounds = 1000,
    nfold = 10,
    metrics = "rmse",
    early_stopping_rounds = 10,
    verbose = 1
  )

  # 找到最佳迭代次数  
  best_nrounds <- cv_results$best_iteration
  print(best_nrounds)
  # 使用最佳迭代次数训练最终XGBoost模型
  final_model <- xgboost(
    params = params,
    data = data_matrix,
    label = target_vector,
    nrounds = best_nrounds,
    verbose = 0 # 减少输出
  )

  return(final_model)
}



## ---------------------------------------------------
# make some plot
# 1:distribution of delaynotts
library(ggplot2)
library(dplyr)
library(hrbrthemes)

# data = data.frame(out)
# p <- ggplot(data,aes(x= data$delayNotts))+
#   geom_histogram(binwidth = 100, fill = "blue", color = "black")
# p
# q = ggplot(data,aes(x= data$delayShef))+
#   geom_histogram(binwidth = 100, fill = "blue", color = "black")
# q


## ---------------------------------------------------
library(ggridges)

# Diamonds dataset is provided by R natively
# head(diamonds)

# basic example
# ggplot(out, aes(x = delayNotts, y = cut, fill = cut)) +
#   geom_density_ridges() +
#   theme_ridges() + 
#   theme(legend.position = "none")


## ---------------------------------------------------
# library
library(ggridges)
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)

# # Load dataset from github
# data <- read.table("https://raw.githubusercontent.com/zonination/perceptions/master/probly.csv", header=TRUE, sep=",")
# data <- data %>% 
#   gather(key="text", value="value") %>%
#   mutate(text = gsub("\\.", " ",text)) %>%
#   mutate(value = round(as.numeric(value),0)) %>%
#   filter(text %in% c("Almost Certainly","Very Good Chance","We Believe","Likely","About Even", "Little Chance", "Chances Are Slight", "Almost No Chance"))
# 
# # Plot
# data %>%
#   mutate(text = fct_reorder(text, value)) %>%
#   ggplot( aes(y=text, x=value,  fill=text)) +
#     geom_density_ridges(alpha=0.6, stat="binline", bins=20) +
#     theme_ridges() +
#     theme(
#       legend.position="none",
#       panel.spacing = unit(0.1, "lines"),
#       strip.text.x = element_text(size = 8)
#     ) +
#     xlab("") +
#     ylab("Assigned Probability (%)")


## ---------------------------------------------------
out = preprocessData(trainingData, historicalCongestion)


# 尝试删除中间小站的数据。
out = subset(out,select= -c(delayNORMNTN,delayNORMNTN2,delayWKFLDKG,delayWKFLDKG2,delayBNSLY,delayBNSLY2,delayMEADWHL,delayMEADWHL2))

out1 = extractMatrix(out)
featureData = data.frame(out1[1])
targetData = out$delayNotts
#应用PCA方法
# featureData = performPCA(featureData)[1]
# featureData = data.frame(featureData$pca_data)
# targetData = unlist(out1[2])

#删除某几列
# data_subset = select(out, -all_of(c( "delayNORMNTN"  ,        "delayNORMNTN2"       ,  "delayWKFLDKG"      ,    "delayWKFLDKG2"  , "delayBNSLY"    ,        "delayBNSLY2"   ,        "delayMEADWHL"    ,      "delayMEADWHL2" )))
# out1 = extractMatrix(data_subset)
# featureData = data.frame(out1[1])
# targetData = data_subset$delayNotts




# summary(out)
# model <- trainXGBoost(out, target_Name)
# model_xgb = trainXGBoost(featureData,targetData)
model_xgb2 = trainXGBoostCaret(featureData,targetData)
# model_rf = trainRandomForest(featureData,targetData)
# model_lm = trainLinearModel(featureData,targetData)





# 以下为预测测试集，并生成文件
test = preprocessData(testData,historicalCongestion)


# 尝试删除中间小站的数据。
test = subset(test,select= -c(delayNORMNTN,delayNORMNTN2,delayWKFLDKG,delayWKFLDKG2,delayBNSLY,delayBNSLY2,delayMEADWHL,delayMEADWHL2))
test1 = extractMatrix(test)
featureData1 = data.frame(test1[1])
prediction <- predict(model_xgb2,featureData1)
write.csv(prediction, file = "trains_group_X4.0.csv", row.names=FALSE)
# write.csv(out, file = "trains_group_all.csv", row.names=TRUE)



## ---------------------------------------------------
model_xgb = trainXGBoost(featureData,targetData)


## ---------------------------------------------------
predict = preprocessData(testData,historicalCongestion)
predict1 = extractMatrix(predict)
featureData1 = data.frame(predict1[1])
prediction = makePrediction(model_xgb, featureData1)
summary(prediction)


## ---------------------------------------------------
summary(featureData1)


## ---------------------------------------------------
out = preprocessData(trainingData, historicalCongestion)
# target_column = unlist(out[1])
# matrixData6 = data.frame(out[0])
target_column = out$delayNotts
model <- lm(target_column ~ ., out)
# model_xgb = trainModel(matrixData6,target_column, modelType= "xgb")


## ---------------------------------------------------
# 设置训练控制
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

# 训练模型
model <- train(targetData ~ . , data = out, method = "lm", trControl = train_control)

# 输出模型摘要
print(model)


## ---------------------------------------------------
# 设置训练控制
train_control <- trainControl(method = "cv", number = 10)

# 训练XGBoost模型
model_xgb <- train(targetData ~ ., data = out, method = "xgbTree", trControl = train_control)

# 输出模型摘要
print(model_xgb)


## ---------------------------------------------------
out = preprocessData(trainingData, historicalCongestion)
  # 提取目标变量为单独的列
  target_column <- out$delayNotts

  # 删除目标列并更新数据框
  matrixData6 <- subset(out, select = -delayNotts)
# model_xgb = trainModel(out, delayNotts, modelType= "xgb")
# 设置训练控制
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

# 训练XGBoost模型
model_xgb <- train(target_column ~ ., data = out, method = "xgbTree", trControl = train_control)

# 输出模型摘要
print(model_xgb)




## ---------------------------------------------------
colnames(out)


## ---------------------------------------------------
out2 = trainModel(matrixData6,target_column, modelType = "xgb")


## ---------------------------------------------------
data(mtcars)
# 目标变量是 mpg, 特征变量是mtcars数据集中mpg列以外的所有列
trainControl <- trainControl(method = "cv", number = 10)
model <- train(mpg ~ ., data = mtcars, method = "lm", trControl = trainControl)

print(model)
summary(model)



## ---------------------------------------------------
# 设置训练控制
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

# 训练模型
model <- train(target_column ~ ., data = matrixData6, method = "lm", trControl = train_control)

# 输出模型摘要
print(model)



## ---------------------------------------------------
# 设置训练控制
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

# 训练随机森林模型
model_rf <- train(target_column ~ ., data = matrixData6, method = "rf", trControl = train_control)

# 输出模型摘要
print(model_rf)



## ---------------------------------------------------
# 设置训练控制
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

# 训练XGBoost模型
model_xgb <- train(target_column ~ ., data = matrixData6, method = "xgbTree", trControl = train_control)

# 输出模型摘要
print(model_xgb)



## ---------------------------------------------------
# #通过遍历来获得所有小站的值
# #Get the values of all small stations by traversing
# for(i in 1:length(targetData)){
#   dummy <- targetData[[i]]#Get the original data set first

#   matrixData$day[i] <- dummy$timings$day.week[1]#先把时间给赋值到表里 First assign the time to the table

#   for(j in 1:nrow(dummy$timings)){

#     deptime <- textToSeconds(dummy$timings$departure.time[j])
#     depsch <- textToSeconds(dummy$timings$departure.schedule[j])
#     depdelay <- deptime - depsch
#     arrtime <- textToSeconds(dummy$timings$arrival.time[j])
#     arrsch <- textToSeconds(dummy$timings$arrival.schedule[j])
#     arrdelay <- arrtime - arrsch
#     x <- dummy$timings$departure.from[j]
#     y <- dummy$timings$arrival.to[j]
#     result1 <- switch(x,
#       "LEEDS" = {
#         matrixData$delayLeeds[i] <- depdelay
#       },
#       "NORMNTN" = {
#   matrixData$delayNORMNTN[i] <- depdelay
# },
#       "WKFLDKG" = {
#   matrixData$delayWKFLDKG[i] <- depdelay
# },
#       "BNSLY" = {
#   matrixData$delayBNSLY[i] <- depdelay
# },
#       "MEADWHL" = {
#   matrixData$delayMEADWHL[i] <- depdelay
# },
#       #{print(dummy$timings$departure.from[j])}
#     )
#     result2 <- switch(y,
#       "NORMNTN" = {
#   matrixData$delayNORMNTN2[i] <- arrdelay
# },
#       "WKFLDKG" = {
#   matrixData$delayWKFLDKG2[i] <- arrdelay
# },
#       "BNSLY" = {
#   matrixData$delayBNSLY2[i] <- arrdelay
# },
#       "MEADWHL" = {
#   matrixData$delayMEADWHL2[i] <- arrdelay
# },
#       "SHEFFLD" = {
#   matrixData$delayShef[i] <- arrdelay
# },
#       #{print(dummy$timings$arrival.to[j])}
#     )
#   }
#   matrixData$delayNotts[i] <- dummy$arrival$delay.secs[1]
# }


## ---------------------------------------------------
head(matrixData)


## ---------------------------------------------------
#now deal with congestion(not history)

num_rows <- length(targetData)  # get row num

# initialize matrixData_Con，make sure they have the same number of rows
matrixData_Con <- data.frame(
  week.day = rep(" ", num_rows),  # Use spaces to fill
  hour = rep(0, num_rows),
  Leeds.trains = rep(0, num_rows),
  Leeds.av.delay = rep(0, num_rows),
  Sheffield.trains = rep(0, num_rows),
  Sheffield.av.delay = rep(0, num_rows),
  Nottingham.trains = rep(0, num_rows),
  Nottingham.av.delay = rep(0, num_rows)
)


# Ensure matrixData_Con is correctly initialized and has enough rows
# 确保matrixData_Con已正确初始化并且有足够的行
print(dim(matrixData_Con))  # Print dimensions to see the number of rows and columns
                             # 打印维度查看行数和列数

for(i in 1:length(targetData)) {
  # Assuming trainingData[i]$congestion is a vector, and its columns correspond one-to-one with the columns of matrixData_Con
  # You need to convert this vector into a list so that you can assign values element by element
  # 假设 trainingData[i]$congestion 是一个向量，且它的列与 matrixData_Con 的列一一对应
  # 你需要将这个向量转换为列表，以便能够逐个元素地赋值
  #congestion_data <- as.list(trainingData[[i]]$congestion)

  # Update matrixData_Con column by column
  # 逐列更新 matrixData_Con
  matrixData_Con$week.day[i] <- targetData[[i]]$congestion[1]
  matrixData_Con$hour[i] <- targetData[[i]]$congestion[2]
  matrixData_Con$Leeds.trains[i] <- targetData[[i]]$congestion[3]
  matrixData_Con$Leeds.av.delay[i] <- targetData[[i]]$congestion[4]
  matrixData_Con$Sheffield.trains[i] <- targetData[[i]]$congestion[5]
  matrixData_Con$Sheffield.av.delay[i] <- targetData[[i]]$congestion[6]
  matrixData_Con$Nottingham.trains[i] <- targetData[[i]]$congestion[7]
  matrixData_Con$Nottingham.av.delay[i] <- targetData[[i]]$congestion[8]


}



## ---------------------------------------------------



## ---------------------------------------------------
#从"list"转换成数据型单位int和dbl
# Convert from "list" to data type unit int and dbl
matrixData_Con <- data.frame(lapply(matrixData_Con, function(x) if(is.list(x)) unlist(x) else x))
head(matrixData_Con)


## ---------------------------------------------------
dim(matrixData)
# Add the current congestion situation to the feature set
# 将当前时刻的拥堵情况添加到特征集中
matrixData2 <- cbind(matrixData, matrixData_Con)
# There's an extra 'day' column, need to remove it
# 多一列day，要删除
matrixData2 <- subset(matrixData2, select = -day)
# Uncomment to view the first few rows of the modified dataframe
#head(matrixData2)

# '''
# Generate one-hot encoding using model.matrix, then convert to dataframe
# 使用model.matrix生成one-hot编码，然后转换为数据框
# matrixData_onehot <- model.matrix(~ week.day - 1, data = matrixData2)
# matrixData2_onehot <- as.data.frame(matrixData_onehot)

# If needed, merge the generated one-hot encoded dataframe with the original dataframe
# 将生成的one-hot编码数据框与原数据框合并（如果需要）
# matrixData3 <- cbind(matrixData2, matrixData2_onehot)
# Remove the 'week.day' column from the merged dataframe
# matrixData3 <- subset(matrixData3, select = -week.day)

# Uncomment to view the first few rows of the final dataframe
# head(matrixData3)
# '''




## ---------------------------------------------------
colnames(matrixData2)#check the colum's name
head(matrixData2)


## ---------------------------------------------------
#colnames(matrixData3)#看看列名


## ---------------------------------------------------
head(historicalCongestion)


## ---------------------------------------------------

#接下来将historicalCongestion也加入到表中
# Now join the historicalCongestion into the table
matrixData4 <- merge(matrixData2,historicalCongestion, by.x=c('week.day','hour'), by.y=c('Day','Hour'), all.x = TRUE)
head(matrixData4)
colnames(matrixData4)


## ---------------------------------------------------
head(matrixData4)


## ---------------------------------------------------
# Generate one-hot encoding using model.matrix, then convert to dataframe
# 使用model.matrix生成one-hot编码，然后转换为数据框
matrixData_onehot2 <- model.matrix(~ week.day - 1, data = matrixData4)
matrixData4_onehot <- as.data.frame(matrixData_onehot2)

# Merge the generated one-hot encoded dataframe with the original dataframe (if needed)
# 将生成的one-hot编码数据框与原数据框合并（如果需要）
matrixData5 <- cbind(matrixData4, matrixData4_onehot)
# Remove the 'week.day' column from the merged dataframe
matrixData5 <- subset(matrixData5, select = -week.day)

# Display the first few rows of the final dataframe
head(matrixData5)



## ---------------------------------------------------

#Extract the target variable as a separate column.
#提取出目标变量为单独的列。
target_column <- matrixData5$delayNotts

#Extract columns
# 提取列
target_column <- matrixData5$delayNotts


# 删除列并更新数据框
# Delete columns and update data frame
matrixData6 <- subset(matrixData5, select = -delayNotts)



## ---------------------------------------------------
head(matrixData6)


## ---------------------------------------------------
# Build linear regression model
# 建立线性回归模型
# Model with target column not separated
#model <- lm(delayNotts ~ ., data = matrixData5)#未分离目标列的写法
model <- lm(delayNotts ~ . , data = matrixData5)

# View model summary
# 查看模型摘要
summary(model)



## ---------------------------------------------------
# 预测值
predicted_values <- predict(model, matrixData6)

# 实际值
actual_values <- target_column

# 计算MSE
mse <- mean((actual_values - predicted_values)^2)

# 打印MSE
print(mse)


## ---------------------------------------------------
#use for crossvalidation
install.packages("caret")
library(caret)



## ---------------------------------------------------


# 设置交叉验证的控制参数
# Set control parameters for cross-validation
trainControl <- trainControl(method = "cv", number = 10) # 10-fold CV

# train the model
model <- train(delayNotts ~ ., data = matrixData5, method = "lm", trControl = trainControl)



## ---------------------------------------------------
summary(model)


## ---------------------------------------------------
#PCA
# 假设df是你的数据集
df <- scale(matrixData6)  # 标准化数据

# 使用prcomp执行PCA。设置center = TRUE以确保数据在处理前被中心化。scale = TRUE将会在PCA之前对数据进行标准化（但如果你已经手动标准化了数据，就不需要再设置scale = TRUE了）。
pca_result <- prcomp(df, center = TRUE, scale. = TRUE)
# 查看PCA结果
summary(pca_result)




## ---------------------------------------------------
library(ggplot2)

# 假设pc_variance_sorted是之前排序好的包含主成分和方差贡献的数据框
# 计算累积方差贡献
pc_variance_sorted$CumulativeVariance <- cumsum(pc_variance_sorted$Variance)

# 绘制累积方差贡献的折线图
ggplot(pc_variance_sorted, aes(x = PC, y = CumulativeVariance)) +
  geom_line(color = "blue") +  # 绘制蓝色的折线
  geom_point(color = "red") +  # 在每个数据点上添加红色的点
  scale_x_continuous(breaks = 1:length(pc_variance_sorted$Variance)) +  # 确保x轴正确显示所有主成分编号
  xlab("Principal Component") +  # x轴标签
  ylab("Cumulative Variance Contribution") +  # y轴标签
  ggtitle("Cumulative Variance Contribution of Principal Components") +  # 图表标题
  theme_minimal()  # 使用简洁的主题



## ---------------------------------------------------
# 设置目标累积方差贡献阈值
target_cumulative_variance <- 0.85

# 找出累积方差贡献第一次达到或超过85%的主成分数
num_pc_for_target_variance <- which(pc_variance_sorted$CumulativeVariance >= target_cumulative_variance)[1]

# 打印所需的主成分数量
print(num_pc_for_target_variance)

# 提取前num_pc_for_target_variance个主成分的得分
new_feature_dataset <- pca_result$x[, 1:num_pc_for_target_variance]

# 如果你想将这些主成分得分作为新的数据框使用
new_feature_df <- as.data.frame(new_feature_dataset)


## ---------------------------------------------------
# 构建线性回归模型
model2 <- lm(target_column ~ ., data = new_feature_df)

# 查看模型摘要
summary(model2)

# 计算模型的残差
residuals <- model$residuals

# 计算MSE
mse <- mean(residuals^2)
print(mse)


## ---------------------------------------------------
# 预测值
predicted_values <- predict(model2, new_feature_df)

# 实际值
actual_values <- target_column

# 计算MSE
mse <- mean((actual_values - predicted_values)^2)

# 打印MSE
print(mse)


## ---------------------------------------------------
library(ggplot2)

# 假设pc_variance_sorted是之前排序好的包含主成分和方差贡献的数据框
# 绘制折线图
ggplot(pc_variance_sorted, aes(x = PC, y = Variance)) +
  geom_line() +  # 使用geom_line()来绘制折线图
  geom_point() +  # 添加点以突出每个主成分的方差贡献
  scale_x_continuous(breaks = 1:length(pc_variance_sorted$Variance)) +  # 确保x轴正确显示所有主成分编号
  xlab("Principal Component") +  # x轴标签
  ylab("Variance Contribution") +  # y轴标签
  ggtitle("Variance Contribution of Principal Components") +  # 图表标题
  theme_minimal()  # 使用简洁的主题



## ---------------------------------------------------
singular_values <- pca_result$sdev
# 计算方差贡献的百分比
variance_contribution <- (singular_values^2) / sum(singular_values^2)

# 创建一个数据框，包含主成分编号和对应的方差贡献
pc_variance <- data.frame(PC = 1:length(variance_contribution),
                          Variance = variance_contribution)

# 按方差贡献降序排序
pc_variance_sorted <- pc_variance[order(-pc_variance$Variance), ]
#4. 可视化方差贡献

library(ggplot2)
ggplot(pc_variance_sorted, aes(x = factor(PC), y = Variance)) +
  geom_bar(stat = "identity") +
  xlab("主成分") +
  ylab("方差贡献") +
  ggtitle("主成分的方差贡献")



## ---------------------------------------------------
# 获得PCA得分
scores <- pca_result$x

# 使用ggplot2绘制第一主成分和第二主成分
ggplot(data = as.data.frame(scores), aes(x = PC1, y = PC2)) +
  geom_point() +
  xlab("第一主成分") +
  ylab("第二主成分") +
  ggtitle("PCA结果可视化")



## ---------------------------------------------------
#接下来复用上面的内容，生成测试集的结果


## ---------------------------------------------------

targetData <- testData

#尝试编程比较完整的信息：利用每个站点的信息

#初始化一个dataframe
matrixData <- data.frame(
day=rep(" ", length(targetData)),
delayLeeds=rep(0, length(targetData)),
delayNORMNTN=rep(0, length(targetData)),
delayNORMNTN2=rep(0, length(targetData)),
delayWKFLDKG=rep(0, length(targetData)),
delayWKFLDKG2=rep(0, length(targetData)),
delayBNSLY=rep(0, length(targetData)),
delayBNSLY2=rep(0, length(targetData)),
delayMEADWHL=rep(0, length(targetData)),
delayMEADWHL2=rep(0, length(targetData)),
delayShef=rep(0, length(targetData)),
delayNotts=rep(0, length(targetData)),
stringsAsFactors=FALSE
)
head(matrixData)


## ---------------------------------------------------
#通过遍历来获得所有小站的值
for(i in 1:length(targetData)){
  dummy <- targetData[[i]]#将原数据集先取来，未知含义

  matrixData$day[i] <- dummy$timings$day.week[1]#先把时间给赋值到表里

  for(j in 1:nrow(dummy$timings)){

    deptime <- textToSeconds(dummy$timings$departure.time[j])
    depsch <- textToSeconds(dummy$timings$departure.schedule[j])
    depdelay <- deptime - depsch
    arrtime <- textToSeconds(dummy$timings$arrival.time[j])
    arrsch <- textToSeconds(dummy$timings$arrival.schedule[j])
    arrdelay <- arrtime - arrsch
    x <- dummy$timings$departure.from[j]
    y <- dummy$timings$arrival.to[j]
    result1 <- switch(x,
      "LEEDS" = {
        matrixData$delayLeeds[i] <- depdelay
      },
      "NORMNTN" = {
  matrixData$delayNORMNTN[i] <- depdelay
},
      "WKFLDKG" = {
  matrixData$delayWKFLDKG[i] <- depdelay
},
      "BNSLY" = {
  matrixData$delayBNSLY[i] <- depdelay
},
      "MEADWHL" = {
  matrixData$delayMEADWHL[i] <- depdelay
},
      #{print(dummy$timings$departure.from[j])}
    )
    result2 <- switch(y,
      "NORMNTN" = {
  matrixData$delayNORMNTN2[i] <- arrdelay
},
      "WKFLDKG" = {
  matrixData$delayWKFLDKG2[i] <- arrdelay
},
      "BNSLY" = {
  matrixData$delayBNSLY2[i] <- arrdelay
},
      "MEADWHL" = {
  matrixData$delayMEADWHL2[i] <- arrdelay
},
      "SHEFFLD" = {
  matrixData$delayShef[i] <- arrdelay
},
      #{print(dummy$timings$arrival.to[j])}
    )
  }
  matrixData$delayNotts[i] <- dummy$arrival$delay.secs[1]
}


## ---------------------------------------------------
#现在完成了表格，该看看如何处理congestion了


## ---------------------------------------------------
# 假设trainingData是你的大数据集
num_rows <- length(targetData)  # 获取行数

# 初始化matrixData_Con，保证它有相同数量的行
matrixData_Con <- data.frame(
  week.day = rep(" ", num_rows),  # 用空格填充
  hour = rep(0, num_rows),
  Leeds.trains = rep(0, num_rows),
  Leeds.av.delay = rep(0, num_rows),
  Sheffield.trains = rep(0, num_rows),
  Sheffield.av.delay = rep(0, num_rows),
  Nottingham.trains = rep(0, num_rows),
  Nottingham.av.delay = rep(0, num_rows)
)

# 确保matrixData_Con已正确初始化并且有足够的行
print(dim(matrixData_Con))  # 打印维度查看行数和列数


for(i in 1:length(targetData)) {
  # 假设 trainingData[i]$congestion 是一个向量，且它的列与 matrixData_Con 的列一一对应
  # 你需要将这个向量转换为列表，以便能够逐个元素地赋值
  #congestion_data <- as.list(trainingData[[i]]$congestion)

  # 逐列更新 matrixData_Con
  matrixData_Con$week.day[i] <- targetData[[i]]$congestion[1]
  matrixData_Con$hour[i] <- targetData[[i]]$congestion[2]
  matrixData_Con$Leeds.trains[i] <- targetData[[i]]$congestion[3]
  matrixData_Con$Leeds.av.delay[i] <- targetData[[i]]$congestion[4]
  matrixData_Con$Sheffield.trains[i] <- targetData[[i]]$congestion[5]
  matrixData_Con$Sheffield.av.delay[i] <- targetData[[i]]$congestion[6]
  matrixData_Con$Nottingham.trains[i] <- targetData[[i]]$congestion[7]
  matrixData_Con$Nottingham.av.delay[i] <- targetData[[i]]$congestion[8]

  # 如果有更多列，继续按照上面的模式赋值
}



## ---------------------------------------------------
#从"list"转换成数据型单位int和dbl
matrixData_Con <- data.frame(lapply(matrixData_Con, function(x) if(is.list(x)) unlist(x) else x))
head(matrixData_Con)


## ---------------------------------------------------
dim(matrixData)
matrixData2 <- cbind(matrixData,matrixData_Con)#将当前时刻的拥堵情况添加到特征集中
matrixData2 <- subset(matrixData2, select = -day)#多一列day，要删除
#head(matrixData2)
# '''
# # 使用model.matrix生成one-hot编码，然后转换为数据框
# matrixData_onehot <- model.matrix(~ week.day - 1, data = matrixData2)
# matrixData2_onehot <- as.data.frame(matrixData_onehot)

# # 将生成的one-hot编码数据框与原数据框合并（如果需要）
# matrixData3 <- cbind(matrixData2, matrixData2_onehot)
# matrixData3 <- subset(matrixData3, select = -week.day)

# head(matrixData3)
# '''



## ---------------------------------------------------
colnames(matrixData2)#看看列名


## ---------------------------------------------------
#colnames(matrixData3)#看看列名


## ---------------------------------------------------
#接下来将historicalCongestion也加入到表中
matrixData4 <- merge(matrixData2,historicalCongestion, by.x=c('week.day','hour'), by.y=c('Day','Hour'))
head(matrixData4)
colnames(matrixData4)

# 使用model.matrix生成one-hot编码，然后转换为数据框
matrixData_onehot2 <- model.matrix(~ week.day - 1, data = matrixData4)
matrixData4_onehot <- as.data.frame(matrixData_onehot2)

# 将生成的one-hot编码数据框与原数据框合并（如果需要）e
matrixData5 <- cbind(matrixData4, matrixData4_onehot)
matrixData5 <- subset(matrixData5, select = -week.day)

head(matrixData5)


## ---------------------------------------------------
#提取出目标变量为单独的列。

# 提取列
target_column <- matrixData5$delayNotts

# 删除列并更新数据框
matrixData6 <- subset(matrixData5, select = -delayNotts)


## ---------------------------------------------------
predicted_values <- predict(model, matrixData6)
# 还要将他们还原成时间，再存到csv里面
#predictions <- rnorm(length(testData),0,100) # Random predictions
write.csv(predicted_values, file = "trains_group_X.csv", row.names=FALSE)


## ---------------------------------------------------
install.packages("randomForest")
install.packages("caret")

library(randomForest)



## ---------------------------------------------------
# 建立随机森林模型
rf_model <- randomForest(delayNotts ~ ., data = matrixData5, ntree = 1000, importance = TRUE)

# 查看模型摘要
print(rf_model)

# 查看变量重要性
importance(rf_model)



## ---------------------------------------------------


library(randomForest)
library(caret) # 用于交叉验证

# 假设你的数据集和目标变量已经准备好
# data - 你的数据集
# target - 你的目标变量

# 设置不同的树的数量进行实验

ntree_values <- c(100, 200, 300, 400, 500, 1000, 1500)
results <- vector("list", length(ntree_values))

for (i in seq_along(ntree_values)) {
  set.seed(42) # 为了可重复性
  model <- randomForest(delayNotts ~ ., data = matrixData3, ntree = ntree_values[i])
  results[[i]] <- model
  # 获取最终的OOB错误率
  oob_mse <- model$mse[length(model$mse)]
  print(paste("Trees:", ntree_values[i], "OOB MSE:", oob_mse))
}

# 分析结果，选择最佳的树的数量



## ---------------------------------------------------
install.packages("glmnet")
library(glmnet)



## ---------------------------------------------------
# 准备数据
data_matrix <- as.matrix(matrixData3[, -which(names(matrixData3) == "delayNotts")])
response_vector <- matrixData3$delayNotts

set.seed(123)  # 确保结果可重复
cv_model <- cv.glmnet(data_matrix, response_vector, alpha = 1)

# 查看lambda值
plot(cv_model)
best_lambda <- cv_model$lambda.min



## ---------------------------------------------------
#虽然cv.glmnet已经给出了最优的lambda值，但通常我们会用这个lambda值来训练最终的模型：
final_model <- glmnet(data_matrix, response_vector, alpha = 1, lambda = best_lambda)




## ---------------------------------------------------



## ---------------------------------------------------
# 预测值
predicted_values <- predict(final_model, matrixData3)

# 实际值
actual_values <- matrixData3$delayNotts

# 计算MSE
mse <- mean((actual_values - predicted_values)^2)

# 打印MSE
print(mse)


## ---------------------------------------------------
# '''
# for (i in 1:length(trainingData)) {
#   dummy <- trainingData[[i]]$timings#将原数据集先取来，未知含义
#   matrixData$day[i] <- dummy$day.week[1]#先把时间给赋值到表里
#   for(k in 2:12){#在timing表的列长度上循环 #需要双引
#     for(j in 1:length(dummy$day.week)){#尝试设置一个指针，用它来代替繁琐的重复，直接将值引用到具体的位置上，ncol是行的长度 #后期需要调整特征数量
#       #matrixData[i,k]表示的是第i行第k列的值,从delayLeeds开始
#       deptime <- textToSeconds(dummy$departure.time[j])
#       depsch <- textToSeconds(dummy$departure.schedule[j])
#       matrixData[i,k] <- deptime - depsch
#       #检查arrival.to是否是NORMNTN
#       # 检查j是否不等于1或者arrival.to的第j个元素不等于'NORMNTN'
#       if (j == 1) {
#         if(dummy$arrival.to[j] == 'NORMNTN'){

#         }else{
#           k = k + 2
#         }
#       }
#       k = k + 1
#       arrtime <- textToSeconds(dummy$arrival.time[j])
#       arrsch <- textToSeconds(dummy$arrival.schedule[j])
#       matrixData[i,k] <- arrtime - arrsch
#     }
#   }
# }

# head(matrixData)
# '''


## ---------------------------------------------------
# '''
# for (i in 1:length(trainingData)){
#   dummy <- trainingData[[i]]#将原数据集先取来，未知含义
#   matrixData$day[i] <- dummy$timings$day.week[1]#先把时间给赋值到表里
#   j <- 1
#   for (k in 2:ncol(matrixData)){
#     if(k == ncol(matrixData)){
#       matrixData$delayNotts[i] <- dummy$arrival$delay.secs[1]
#       break#最后一个数是delayNotts
#     }
#     if(k %% 2 == 0){#如果偶数，则是dep
#     #matrixData[i,k]表示的是第i行第k列的值,从delayLeeds开始
#       if(j == 2){
#         if(dummy$timings$departure.from[j] == 'NORMNTN'){
#           deptime <- textToSeconds(dummy$timings$departure.time[j])
#           depsch <- textToSeconds(dummy$timings$departure.schedule[j])
#           matrixData[i,k] <- deptime - depsch
#           next
#         }else{
#           next
#         }
#       }
#     deptime <- textToSeconds(dummy$timings$departure.time[j])
#     depsch <- textToSeconds(dummy$timings$departure.schedule[j])
#     matrixData[i,k] <- deptime - depsch
#     next
#     }
#     if(k %% 2 == 1){#如果是奇数，则是arr
#     #奇数的时候记得加j
#       if(j == 1){
#         if(dummy$timings$arrival.to[j]=='NORMNTN'){
#           arrtime <- textToSeconds(dummy$timings$arrival.time[j])
#           arrsch <- textToSeconds(dummy$timings$arrival.schedule[j])
#           matrixData[i,k] <- arrtime - arrsch
#           j <- j + 1
#           next
#         }else{
#           next
#         }
#       }
#       arrtime <- textToSeconds(dummy$timings$arrival.time[j])
#       arrsch <- textToSeconds(dummy$timings$arrival.schedule[j])
#       matrixData[i,k] <- arrtime - arrsch
#       j <- j + 1
#       next
#     }
#   }
# }
# '''


## ---------------------------------------------------
# '''
# for (i in 1:length(trainingData)){
#   dummy <- trainingData[[i]]#将原数据集先取来，未知含义
#   matrixData$day[i] <- dummy$timings$day.week[1]#先把时间给赋值到表里
#   j <- 1
#   k <- 2
#   while(k <= ncol(matrixData)){
#   deptime <- textToSeconds(dummy$timings$departure.time[j])
#   depsch <- textToSeconds(dummy$timings$departure.schedule[j])
#   matrixData[i,k] <- deptime - depsch
#   k <- k + 1
#   if(j == 1){
#       if(dummy$timings$arrival.to[j]=='NORMNTN'){
#         arrtime <- textToSeconds(dummy$timings$arrival.time[j])
#         arrsch <- textToSeconds(dummy$timings$arrival.schedule[j])
#         matrixData[i,k] <- arrtime - arrsch
#         j <- j + 1
#         k <- k + 1
#         next
#       }else{
#         k <- k + 2
#       }
#   }
#   arrtime <- textToSeconds(dummy$timings$arrival.time[j])
#   arrsch <- textToSeconds(dummy$timings$arrival.schedule[j])
#   matrixData[i,k] <- arrtime - arrsch
#   k <- k + 1
#   j <- j + 1
#   if(k == ncol(matrixData)){
#       matrixData$delayNotts[i] <- dummy$arrival$delay.secs[1]
#       break#最后一个数是delayNotts
#     }
#   }

# }
# '''


## ---------------------------------------------------
# #不再使用，仅作为参考
# exeLEEDS <- function(i){
#   matrixData$delayLeeds[i] <- depdelay
# }
# exeNORMNTN <- function(i){
#   matrixData$delayNORMNTN[i] <- depdelay
# }
# exeWKFLDKG <- function(i){
#   matrixData$delayWKFLDKG[i] <- depdelay
# }
# exeBNSLY <- function(i){
#   matrixData$delayBNSLY[i] <- depdelay
# }
# exeMEADWHL <- function(i){
#   matrixData$delayMEADWHL[i] <- depdelay
# }




# exeNORMNTN2 <- function(i){
#   matrixData$delayNORMNTN2[i] <- arrdelay
# }
# exeWKFLDKG2 <- function(i){
#   matrixData$delayWKFLDKG2[i] <- arrdelay
# }
# exeBNSLY2 <- function(i){
#   matrixData$delayBNSLY2[i] <- arrdelay
# }
# exeMEADWHL2 <- function(i){
#   matrixData$delayMEADWHL2[i] <- arrdelay
# }
# exeShef <- function(i){
#   matrixData$delayShef[i] <- arrdelay
# }

