classify.nb <- function(df.train, df.test, target.train, target.test) {
  result<-as.data.frame(c("Data"))
  names(result)<-c("Bayess")
  result$acc.train<-rep(0,1)
  result$acc.test<-rep(0,1)
  nb<-e1071::naiveBayes(df.train, target.train)
  fc<-predict(nb, df.train)
  result$acc.train[1]<-calc.accuracy(fc,target.train)
  fc2<-predict(nb, df.test)
  result$acc.test[1]<-calc.accuracy(fc2,target.test)
  return(result)
}

classify.knn <- function(df.train, df.test, target.train, target.test) {
  result<-as.data.frame(seq(1,10,1))
  names(result)<-c("neighbours")
  result$acc.train<-rep(0,10)
  result$acc.test<-rep(0,10)
  for(i in c(1:10)){
    knn.train <- class::knn(
      train=df.train,
      test=df.train,
      cl=target.train,
      k=i)
    result$acc.train[i]<-calc.accuracy(knn.train, target.train)

    knn.test<-class::knn(
      train=df.train,
      test=df.test,
      cl=target.train,
      k=i)
    result$acc.test[i]<-calc.accuracy(knn.test, target.test)
  }
  return(result)
}

calc.accuracy<-function(predicted, real) {
  t<-table(predicted, real)
  acc<-(t[1,1]+t[2,2])/sum(t)
  return(acc)
}

#' Create classify
#'
#' @param df input data - predictors
#' @param target input data - target
#' @return classify
#' @export
classify <- function(df, target) {

  index<-sample(nrow(df), round(nrow(df)*0.8,0))
  df.train<-df[index,]
  df.test<-df[-index,]
  target.train<-target[index]
  target.test<-target[-index]
  this<-list(knn=classify.knn(df.train, df.test, target.train, target.test),
             nb=classify.nb(df.train, df.test, target.train, target.test),
             df.train=df.train,
             df.test=df.test,
             target.train=target.train,
             target.test=target.test)
  class(this)<-'classify'
  return(this)
}

#' Plot classify
#'
#' @param x classify
#' @export
plot.classify<-function(x) {
  ggplot2::ggplot(x$knn, ggplot2::aes(neighbours))+
    ggplot2::geom_line(ggplot2::aes(y=acc.train, colour="train"))+
    ggplot2::geom_line(ggplot2::aes(y=acc.test, colour="test"))+
    ggplot2::geom_point(ggplot2::aes(x=neighbours,y=acc.test))+
    ggplot2::geom_point(ggplot2::aes(x=neighbours,y=acc.train))+
    ggplot2::scale_x_continuous(breaks = seq(0,10,1))+
    ggplot2::ylab("Accuracy")+
    ggplot2::xlab("Neighbour count")+
    ggplot2::ggtitle("Accuracy by groups")+
    ggplot2::labs(colour="Data")+
    ggplot2::theme(plot.title = ggplot2::element_text(hjust=0.5), legend.title = ggplot2::element_text(hjust=0.5))
}


#' Print classify
#'
#' @param x classify
#' @export
print.classify<-function(x) {
  print("Result")
  print("--------------")
  print("Naive Bayess")
  print(x$nb)
  print("--------------")
  print("K Nearest Neighbours")
  print(x$knn)
}


#' Summary of classify
#' Print summary of test and train data used for classification
#'
#' @param x classify
#' @export
summary.classify<-function(x){
  print("Train data summary")
  print(summary(x$df.train))
  print(summary(x$target.train))

  print("Test data summary")
  print(summary(x$df.test))
  print(summary(x$target.test))
}




