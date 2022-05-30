

#' finddata
#'
#' 查找数据框某字符所在的位置
#'
#' @param A1 输入要查找的数据名字
#' @param A2 输入要查找的字符
#'
#' @return position
#' @export
#'
finddata=function(A1,A2){
  eval(parse(text = paste0('data2=as.matrix(',A1,'==',A2,')')))
  eval(parse(text = paste0('position=as.vector(which(data2==TRUE,arr.ind=TRUE))')))
  return(position)
}


