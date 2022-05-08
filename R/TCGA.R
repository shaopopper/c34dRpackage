# 1. 下载数据与json文件

#  2. 不同文件夹文件提取
# 将次级目录的文件夹里面的文件提取到同一个文件夹下

# 一些基础操作


# 验证二级文件夹里面是否有文件，无返回NA值，为TRUE，
#' Title
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

#filename2=GetSecondaryDirectoryFile(1)



#' Title
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
  # 复制文件到data这个文件夹
}

#输入要创建目录的名字,并将上面的文件导入，该文件夹
#mkdir("data",filename2)



# 先读取jason文本信息,此时工作路径还没转向/data
#library(rjson)
#JSONinformation <- fromJSON(file="metadata.cart.2022-05-04.json") #先读取

#gtffinal <- read.csv("gtf最终注解文件.csv")  #先读取


# 转向data目录
#' Title
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

