

Pone2 <- function(n,k,nreps=10000) {
  #策略1
  if (k==1) {
    set.seed(123)  #设定随机种子
    
    flag <- F      #单次是否成功
    pass_time <- 0 #实验成功的次数
    box <- sample(1:n)  #随机箱子
    names(box) <- 1:n
    p_all <- 0    #nreps次数，总的单个犯人成功的概率
    for (i in c(1:nreps)) {
      box_time <- 0    #开箱次数
      flag <- 0
      # 实验开始
      for (j in c(1:n)) {
        k = j  #开箱号码
        while (box[k] != j & box_time <= (n/2)) {
          box_time <- box_time + 1
          k = box[k]
        }
        if (box_time > (n/2)) {
          next
        }
        flag <- flag + 1
        box_time <- 0
      }
      p1 <- flag / n             #单个犯人成功的概率
      p_all <- p_all + p1
      ###实验结束
      box <- sample(1:n)  #随机箱子
    }
    ###实验结束
    print(paste0('一个犯人成功找到自己号码的概率:',round(p_all / nreps,4)))
  }
}

Pone2(5,1,10000)
Pone2(50,1,10000)
Pone2(100,1,10000)