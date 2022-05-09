#' GetSecondaryDirectoryFile
#'
#' 验证二级文件夹里面是否有文件，无返回NA值，有为TRUE。最后返回二级文件夹里面的第一个文件绝对路径
#'
#' @param xx 随意填入数字
#'
#' @return filename
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
  return(filename2)
}

GetSecondaryDirectoryFile(2)

#' mkdir
#'
#' 当前路径创建文件夹“xx”
#'
#' @param xx 为输入要创建的文件夹名称
#'
#' @return
#' @export
#'

mkdir <- function(xx){
  xxx <- as.vector(xx)
  #复制文件到同一文件夹
  #获得路径，在后面加入/data
  # dir.create('C:/Users/shao/Desktop/T2/data3') #创建一个目录
  eval(parse(text = paste0("dir.create('",getwd(),"/",xxx,"')")))
}

#' cd
#'
#' 工作目录转到文件夹"xx"
#'
#' @param xx 输入要转到的文件夹名称
#'
#' @return
#' @export
#'

cd <- function(xx){
  eval(parse(text = paste0("setwd('",getwd(),"/",xx,"')")))
  print(getwd())
}

#' describe_by
#'
#' 统计描述describe_by，搭配aggregate使用
#'
#' @param y 无意义
#' @param na.omit 无意义
#'
#' @return
#' @export
#'

describe_by<-function(y,na.omit=TRUE){
  describe=function(x,na.omit=TRUE){   #na.omit=TRUE不忽略缺失指
        if (na.omit)
          Miss<-sum(is.na(x)==TRUE)   #统计缺失值
        x<-x[!is.na(x)]  #去掉缺失值
        m<-mean(x)
        n<-length(x)  #不是缺失值的个数
        s<-sd(x)
        skew<-sum((x-m)^3/s^3)/n
        kurt<-sum((x-m)^4/s^4)/n-3
        #Normal_ks=ks.test(x,"pnorm")[["p.value"]]
        Noraml_sh=shapiro.test(x)[["p.value"]]
        return(round(c(n=n,Miss=Miss,mean=m,stdev=s,
                       skew=skew,kurtosis=kurt,
                       #Normal_ks=Normal_ks
                       Noraml_sh=Noraml_sh),3))}
        sapply(y,describe,na.omit=TRUE)
}


