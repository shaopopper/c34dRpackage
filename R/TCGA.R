

#' Title 验证二级文件夹里面是否有文件，无返回NA值，为TRUE。最后返回二级文件夹文件
#'
#' @param xx
#'
#' @return
#' @export
#'

GetSecondaryDirectoryFile <- function(xx){
  xxx=xx
  filename=1
  for (i in 1:200) {
    a=as.character(list.files(list.files()[i])[1])
    ifelse( a%in% NA==TRUE, NA,'b')
    b=paste(getwd(),"/",list.files()[i],"/",list.files(list.files()[i])[1],sep = "")
    filename[[i]]=b
  }
  filename
  filename <- as.data.frame(filename)
  # 对200行数据进行过滤
  filename2=apply(filename, 2,
                  function(x){gsub(pattern = ".*(NA).*",
                                   replacement = "\\21",x) })
  filename2 <- as.data.frame(filename2)
  filename2 <- filename2[filename2$filename!=1,]
  filename2  #这时候提取到试运行的10个文件的绝对路径

}


#' Title 当前路径创建文件夹“xx”
#'
#' @param xxx
#' @param filename2
#'
#' @return
#' @export
#'

mkdir <- function(xxx){
  xxx <- as.vector(xxx)
  #复制文件到同一文件夹
  #获得路径，在后面加入/data
  # dir.create('C:/Users/shao/Desktop/T2/data3') #创建一个目录
  eval(parse(text = paste0("dir.create('",getwd(),"/",xxx,"')")))
}



#' Title 工作目录转到文件夹"xx"
#'
#' @param xxx
#'
#' @return
#' @export
#'

cd <- function(xxx){
  eval(parse(text = paste0("setwd('",getwd(),"/",xxx,"')")))
  print(getwd())
}

#cd("data")

