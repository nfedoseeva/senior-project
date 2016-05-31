
#----loading table
okData <- read.csv("/Users/nata/Desktop/data/groupstat.csv", header = FALSE, sep = "", col.names = c("REPORT_DATE", "ID", "NUM_USERS", "CNT_IN", "CNT_OUT", "CNT_LIKES", "CNT_LIKES_WITH_DOCS", "CNT_POSTS","CNT_COMMENTS","CNT_PHOTOS"))
#-----turning it to datatable
okData = data.table::data.table(okData)
#------changing date format to "Date"
okData$REPORT_DATE <- as.character(okData$REPORT_DATE)
okData$REPORT_DATE <- as.Date(okData$REPORT_DATE, "%Y%m%d")

#getting the first day of when group appeared

d = 7# number of days
okSVM <- function(days) {
  n = 7959
  #n = 100
  idnew = 1000000
  group = data.table::data.table(okData[ID == idnew]) 
  firstday = min(group$REPORT_DATE)
  lastday = firstday + days
  usersThen = unique(group$NUM_USERS - (sum(group$CNT_IN - group$CNT_OUT))) # finding the number of users in the very beginning
  group = data.table::data.table(group[REPORT_DATE >= firstday & REPORT_DATE < lastday])
  usersNow <- unique(group$NUM_USERS + (sum(group$CNT_IN - group$CNT_OUT)))
  usersIn <- unique(sum(group$CNT_IN - group$CNT_OUT))
  posts = sum(group$CNT_POSTS)
  photos = sum(group$CNT_PHOTOS)
  likes = sum(group$CNT_LIKES)
  comments = sum(group$CNT_COMMENTS)
  DT = data.table::data.table(firstday,
                              lastday, 
                              id = unique(group$ID),
                              posts,
                              likes,
                              photos,
                              comments,
                              usersThen,
                              usersIn,
                              usersNow)
  idnew = idnew + 1
  for(i in 1:(n-1)) {
    group = data.table::data.table(okData[ID == idnew]) 
    firstday = min(group$REPORT_DATE)
    lastday = firstday + days
    usersThen = unique(group$NUM_USERS - (sum(group$CNT_IN - group$CNT_OUT))) # finding the number of users in the very beginning
    # ---------------------making a table out of the first portion of days
    group = data.table::data.table(group[REPORT_DATE >= firstday & REPORT_DATE < lastday])
    usersNow <- unique(group$NUM_USERS + (sum(group$CNT_IN - group$CNT_OUT)))
    usersIn <- unique(sum(group$CNT_IN - group$CNT_OUT))
    posts = sum(group$CNT_POSTS)
    photos = sum(group$CNT_PHOTOS)
    likes = sum(group$CNT_LIKES)
    comments = sum(group$CNT_COMMENTS)
    DT = rbind(DT, data.table::data.table(
      firstday,
      lastday, 
      id = unique(group$ID),
      posts,
      likes,
      photos,
      comments,
      usersThen,
      usersIn,
      usersNow))
    idnew = idnew + 1 
  }
  #-------number of users now and then------------
  best = data.table::data.table(DT)
  
  #------making train and test sets------------
  # 80% of the sample size
  popular <- (best$usersNow >= best$usersThen * 2)
  popular <- as.numeric(popular)
  best <- cbind(best, pop = popular)
  sampleSize <- floor(0.9 * nrow(best))
  ## getting train and test sets
  set.seed(228)
  getsample <- sample(seq_len(nrow(best)), size = sampleSize)
  okTrain <- best[getsample, ]
  okTest <- best[-getsample, ]
  
  library(e1071)
  fit <- svm(formula = pop ~ usersThen + posts + photos + likes, data = okTrain)
  y <- predict(fit, newdata = okTest)
  okTest <- cbind(okTest, predicting = y)
  library(SDMTools)
  thrs <- mean(okTest$predicting)
  cm <- confusion.matrix(okTest$pop, okTest$predicting, threshold = 0.215) 
  prec <- cm[2,2] / (cm[2,2] + cm[2,1])
  rec<- cm[2,2] / (cm[2,2] + cm[1,2])
  f1 <- (2 * prec * rec) / (prec + rec)
  f1
}
d2 = okSVM(7)
d3 = okSVM(14)
d4 = okSVM(21)
d5 = okSVM(30)
d6 = okSVM(60)

okSVM2 <- function(days) {
  n = 7959
  #n = 100
  idnew = 1000000
  group = data.table::data.table(okData[ID == idnew]) 
  firstday = min(group$REPORT_DATE)
  lastday = firstday + days
  usersThen = unique(group$NUM_USERS - (sum(group$CNT_IN - group$CNT_OUT))) # finding the number of users in the very beginning
  group = data.table::data.table(group[REPORT_DATE >= firstday & REPORT_DATE < lastday])
  usersNow <- unique(group$NUM_USERS + (sum(group$CNT_IN - group$CNT_OUT)))
  usersIn <- unique(sum(group$CNT_IN - group$CNT_OUT))
  posts = sum(group$CNT_POSTS)
  photos = sum(group$CNT_PHOTOS)
  likes = sum(group$CNT_LIKES)
  comments = sum(group$CNT_COMMENTS)
  DT = data.table::data.table(firstday,
                              lastday, 
                              id = unique(group$ID),
                              posts,
                              likes,
                              photos,
                              comments,
                              usersThen,
                              usersIn,
                              usersNow)
  idnew = idnew + 1
  for(i in 1:(n-1)) {
    group = data.table::data.table(okData[ID == idnew]) 
    firstday = min(group$REPORT_DATE)
    lastday = firstday + days
    usersThen = unique(group$NUM_USERS - (sum(group$CNT_IN - group$CNT_OUT))) # finding the number of users in the very beginning
    # ---------------------making a table out of the first portion of days
    group = data.table::data.table(group[REPORT_DATE >= firstday & REPORT_DATE < lastday])
    usersNow <- unique(group$NUM_USERS + (sum(group$CNT_IN - group$CNT_OUT)))
    usersIn <- unique(sum(group$CNT_IN - group$CNT_OUT))
    posts = sum(group$CNT_POSTS)
    photos = sum(group$CNT_PHOTOS)
    likes = sum(group$CNT_LIKES)
    comments = sum(group$CNT_COMMENTS)
    DT = rbind(DT, data.table::data.table(
      firstday,
      lastday, 
      id = unique(group$ID),
      posts,
      likes,
      photos,
      comments,
      usersThen,
      usersIn,
      usersNow))
    idnew = idnew + 1 
  }
  #-------number of users now and then------------
  best = data.table::data.table(DT)
  
  #------making train and test sets------------
  # 80% of the sample size
  popular <- (best$usersNow >= best$usersThen * 2)
  popular <- as.numeric(popular)
  best <- cbind(best, pop = popular)
  sampleSize <- floor(0.9 * nrow(best))
  ## getting train and test sets
  set.seed(228)
  getsample <- sample(seq_len(nrow(best)), size = sampleSize)
  okTrain <- best[getsample, ]
  okTest <- best[-getsample, ]
  
  library(e1071)
  fit <- svm(formula = pop ~ usersThen, data = okTrain)
  y <- predict(fit, newdata = okTest)
  okTest <- cbind(okTest, predicting = y)
  library(SDMTools)
  thrs <- mean(okTest$predicting)
  cm <- confusion.matrix(okTest$pop, okTest$predicting, threshold = 0.215) 
  prec <- cm[2,2] / (cm[2,2] + cm[2,1])
  rec<- cm[2,2] / (cm[2,2] + cm[1,2])
  f1 <- (2 * prec * rec) / (prec + rec)
  f1
}

d8 = okSVM2(7)
d9 = okSVM2(14)
d10 = okSVM2(21)
d11 = okSVM2(30)
d12 = okSVM2(60)

fs <- c(d2,d3,d4,d5,d6)
fs2 <- c(d8,d9,d10,d11,d12)
ds <- c(7,14,21,30,60)


plot(ds,fs, type = "b", col = 2)
par(new=T)
plot(ds, fs2, type = "l", col = 3)
par(new=F)

# get the range for the x and y axis 
x <- c(0.5,0.6,0.7,0.8,0.9)
yrange <- range(x) 
xrange <- range(ds) 

# set up the plot 
plot(xrange, yrange, type="n", xlab="Days",
     ylab="F1-measure" ) 
  lines(ds, fs, type="b", col = 4, lwd=1.5) 
  lines(ds, fs2, type="b", col = 3, lwd=1.5) 
  title("SVM")
  legend(xrange[1], 0.7, c("One predictor", "Several predictors"), cex=0.6, col= c(3,4),lty = 2, title="Model")

  
  
  okDT <- function(days) {
    n = 7959
    #n = 100
    idnew = 1000000
    group = data.table::data.table(okData[ID == idnew]) 
    firstday = min(group$REPORT_DATE)
    lastday = firstday + days
    usersThen = unique(group$NUM_USERS - (sum(group$CNT_IN - group$CNT_OUT))) # finding the number of users in the very beginning
    group = data.table::data.table(group[REPORT_DATE >= firstday & REPORT_DATE < lastday])
    usersNow <- unique(group$NUM_USERS + (sum(group$CNT_IN - group$CNT_OUT)))
    usersIn <- unique(sum(group$CNT_IN - group$CNT_OUT))
    posts = sum(group$CNT_POSTS)
    photos = sum(group$CNT_PHOTOS)
    likes = sum(group$CNT_LIKES)
    comments = sum(group$CNT_COMMENTS)
    DT = data.table::data.table(firstday,
                                lastday, 
                                id = unique(group$ID),
                                posts,
                                likes,
                                photos,
                                comments,
                                usersThen,
                                usersIn,
                                usersNow)
    idnew = idnew + 1
    for(i in 1:(n-1)) {
      group = data.table::data.table(okData[ID == idnew]) 
      firstday = min(group$REPORT_DATE)
      lastday = firstday + days
      usersThen = unique(group$NUM_USERS - (sum(group$CNT_IN - group$CNT_OUT))) # finding the number of users in the very beginning
      # ---------------------making a table out of the first portion of days
      group = data.table::data.table(group[REPORT_DATE >= firstday & REPORT_DATE < lastday])
      usersNow <- unique(group$NUM_USERS + (sum(group$CNT_IN - group$CNT_OUT)))
      usersIn <- unique(sum(group$CNT_IN - group$CNT_OUT))
      posts = sum(group$CNT_POSTS)
      photos = sum(group$CNT_PHOTOS)
      likes = sum(group$CNT_LIKES)
      comments = sum(group$CNT_COMMENTS)
      DT = rbind(DT, data.table::data.table(
        firstday,
        lastday, 
        id = unique(group$ID),
        posts,
        likes,
        photos,
        comments,
        usersThen,
        usersIn,
        usersNow))
      idnew = idnew + 1 
    }
    #-------number of users now and then------------
    best = data.table::data.table(DT)
    
    #------making train and test sets------------
    # 80% of the sample size
    popular <- (best$usersNow >= best$usersThen * 2)
    popular <- as.numeric(popular)
    best <- cbind(best, pop = popular)
    sampleSize <- floor(0.9 * nrow(best))
    ## getting train and test sets
    set.seed(228)
    getsample <- sample(seq_len(nrow(best)), size = sampleSize)
    okTrain <- best[getsample, ]
    okTest <- best[-getsample, ]
    
    library(rpart)
    fit <- rpart(formula = pop ~ usersThen, method = "class", data = okTrain)
    y <- predict(fit, newdata = okTest)
    okTest <- cbind(okTest, predicting = y)
    library(SDMTools)
    thrs <- mean(okTest$predicting)
    cm <- confusion.matrix(okTest$pop, okTest$predicting, threshold = 0.215) 
    prec <- cm[2,2] / (cm[2,2] + cm[2,1])
    rec<- cm[2,2] / (cm[2,2] + cm[1,2])
    f1 <- (2 * prec * rec) / (prec + rec)
    f1
  }
  
  okDT2 <- function(days) {
    n = 7959
    #n = 100
    idnew = 1000000
    group = data.table::data.table(okData[ID == idnew]) 
    firstday = min(group$REPORT_DATE)
    lastday = firstday + days
    usersThen = unique(group$NUM_USERS - (sum(group$CNT_IN - group$CNT_OUT))) # finding the number of users in the very beginning
    group = data.table::data.table(group[REPORT_DATE >= firstday & REPORT_DATE < lastday])
    usersNow <- unique(group$NUM_USERS + (sum(group$CNT_IN - group$CNT_OUT)))
    usersIn <- unique(sum(group$CNT_IN - group$CNT_OUT))
    posts = sum(group$CNT_POSTS)
    photos = sum(group$CNT_PHOTOS)
    likes = sum(group$CNT_LIKES)
    comments = sum(group$CNT_COMMENTS)
    DT = data.table::data.table(firstday,
                                lastday, 
                                id = unique(group$ID),
                                posts,
                                likes,
                                photos,
                                comments,
                                usersThen,
                                usersIn,
                                usersNow)
    idnew = idnew + 1
    for(i in 1:(n-1)) {
      group = data.table::data.table(okData[ID == idnew]) 
      firstday = min(group$REPORT_DATE)
      lastday = firstday + days
      usersThen = unique(group$NUM_USERS - (sum(group$CNT_IN - group$CNT_OUT))) # finding the number of users in the very beginning
      # ---------------------making a table out of the first portion of days
      group = data.table::data.table(group[REPORT_DATE >= firstday & REPORT_DATE < lastday])
      usersNow <- unique(group$NUM_USERS + (sum(group$CNT_IN - group$CNT_OUT)))
      usersIn <- unique(sum(group$CNT_IN - group$CNT_OUT))
      posts = sum(group$CNT_POSTS)
      photos = sum(group$CNT_PHOTOS)
      likes = sum(group$CNT_LIKES)
      comments = sum(group$CNT_COMMENTS)
      DT = rbind(DT, data.table::data.table(
        firstday,
        lastday, 
        id = unique(group$ID),
        posts,
        likes,
        photos,
        comments,
        usersThen,
        usersIn,
        usersNow))
      idnew = idnew + 1 
    }
    #-------number of users now and then------------
    best = data.table::data.table(DT)
    
    #------making train and test sets------------
    # 80% of the sample size
    popular <- (best$usersNow >= best$usersThen * 2)
    popular <- as.numeric(popular)
    best <- cbind(best, pop = popular)
    sampleSize <- floor(0.9 * nrow(best))
    ## getting train and test sets
    set.seed(228)
    getsample <- sample(seq_len(nrow(best)), size = sampleSize)
    okTrain <- best[getsample, ]
    okTest <- best[-getsample, ]
    
    library(rpart)
    fit <- rpart(formula = pop ~ usersThen + posts + photos + likes, method = "class", data = okTrain)
    y <- predict(fit, newdata = okTest)
    okTest <- cbind(okTest, predicting = y)
    library(SDMTools)
    thrs <- mean(okTest$predicting)
    cm <- confusion.matrix(okTest$pop, okTest$predicting, threshold = 0.215) 
    prec <- cm[2,2] / (cm[2,2] + cm[2,1])
    rec<- cm[2,2] / (cm[2,2] + cm[1,2])
    f1 <- (2 * prec * rec) / (prec + rec)
    f1
  }
  
  dt1 = okDT(7)
  dt2 = okDT(14)
  dt3 = okDT(21)
  dt4 = okDT(30)
  dt5 = okDT(60)
  
  dt6 = okDT(7)
  dt7 = okDT(14)
  dt8 = okDT(21)
  dt9 = okDT(30)
  dt10 = okDT(60)
  
  dts1 <- c(dt1,dt2,dt3,dt4,dt5)
  dts2 <- c(dt6,dt7,dt8,dt9,dt10)
  dtss <- c(7,14,21,30,60)
  
  # get the range for the x and y axis 
  x <- c(0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
  yrange <- range(x) 
  xrange <- range(dtss) 
  
  # set up the plot 
  plot(xrange, yrange, type="n", xlab="Days",
       ylab="F1-measure" ) 
  lines(dtss, dts1, type="b", col = 4, lwd=1.5) 
  lines(dtss, dts2, type="b", col = 3, lwd=1.5) 
  title("DT-method")
  legend(xrange[1], 0.7, c("One predictor", "Several predictors"), cex=0.6, col= c(3,4),lty = 2, title="Model")
  
  
#------model
#require(glmnet)
#fit <- lm(formula = pop ~ usersThen + posts + photos + likes, data = okTrain)
#-----precision and recall
#library(rpart)
#fit <- rpart(formula = pop ~ usersThen, method = "class", data = okTrain)
#fit <- glm(formula = pop ~ usersIn + posts + posts + likes + comments, data = okTrain)
#------predicting
  d = 7# number of days
  okLR <- function(days) {
    n = 7959
    #n = 100
    idnew = 1000000
    group = data.table::data.table(okData[ID == idnew]) 
    firstday = min(group$REPORT_DATE)
    lastday = firstday + days
    usersThen = unique(group$NUM_USERS - (sum(group$CNT_IN - group$CNT_OUT))) # finding the number of users in the very beginning
    group = data.table::data.table(group[REPORT_DATE >= firstday & REPORT_DATE < lastday])
    usersNow <- unique(group$NUM_USERS + (sum(group$CNT_IN - group$CNT_OUT)))
    usersIn <- unique(sum(group$CNT_IN - group$CNT_OUT))
    posts = sum(group$CNT_POSTS)
    photos = sum(group$CNT_PHOTOS)
    likes = sum(group$CNT_LIKES)
    comments = sum(group$CNT_COMMENTS)
    DT = data.table::data.table(firstday,
                                lastday, 
                                id = unique(group$ID),
                                posts,
                                likes,
                                photos,
                                comments,
                                usersThen,
                                usersIn,
                                usersNow)
    idnew = idnew + 1
    for(i in 1:(n-1)) {
      group = data.table::data.table(okData[ID == idnew]) 
      firstday = min(group$REPORT_DATE)
      lastday = firstday + days
      usersThen = unique(group$NUM_USERS - (sum(group$CNT_IN - group$CNT_OUT))) # finding the number of users in the very beginning
      # ---------------------making a table out of the first portion of days
      group = data.table::data.table(group[REPORT_DATE >= firstday & REPORT_DATE < lastday])
      usersNow <- unique(group$NUM_USERS + (sum(group$CNT_IN - group$CNT_OUT)))
      usersIn <- unique(sum(group$CNT_IN - group$CNT_OUT))
      posts = sum(group$CNT_POSTS)
      photos = sum(group$CNT_PHOTOS)
      likes = sum(group$CNT_LIKES)
      comments = sum(group$CNT_COMMENTS)
      DT = rbind(DT, data.table::data.table(
        firstday,
        lastday, 
        id = unique(group$ID),
        posts,
        likes,
        photos,
        comments,
        usersThen,
        usersIn,
        usersNow))
      idnew = idnew + 1 
    }
    #-------number of users now and then------------
    best = data.table::data.table(DT)
    
    #------making train and test sets------------
    # 80% of the sample size
    popular <- (best$usersNow >= best$usersThen * 2)
    popular <- as.numeric(popular)
    best <- cbind(best, pop = popular)
    sampleSize <- floor(0.9 * nrow(best))
    ## getting train and test sets
    set.seed(228)
    getsample <- sample(seq_len(nrow(best)), size = sampleSize)
    okTrain <- best[getsample, ]
    okTest <- best[-getsample, ]
    
    #model
    fit <- glm(formula = pop ~ usersIn + posts + photos + likes, data = okTrain)
    y <- predict(fit, newdata = okTest)
    okTest <- cbind(okTest, predicting = y)
    library(Metrics)
    err<-mse(okTest$pop, y)
  }
# # mse <- mean((okTest$pop - y)^2)
# library(Metrics)
# err<-mse(okTest$pop, y)
# 
# 
# 
# pred <- ROCR::prediction(okTest$predicting, okTest$pop)
# perf <- ROCR::performance(pred,"prec","rec")
# ROCR::plot(perf, avg= "threshold", colorize=T, lwd= 3,
#            main= "Precision/Recall, MLR, SVM")
# ROCR::plot(perf, lty=3, col="grey78", add=T)




