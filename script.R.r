
Pone <- function(n,k,nreps=10000) {
  #策略1
  if (k==1) {
 #   set.seed(123)  #设定随机种子
    
    flag <- F      #单次是否成功
    pass_time <- 0 #实验成功的次数
    box <- sample(1:n)  #随机箱子
    names(box) <- 1:n
    for (i in c(1:nreps)) {
      box_time <- 0    #开箱次数
      flag <- T
      # 实验开始
      for (j in c(1:n)) {
        k = j  #开箱号码
        while (box[k] != j & box_time <= (n/2)) {
          box_time <- box_time + 1
          k = box[k]
        }
        if (box_time > (n/2)) {
          box_time <- 0
          flag <- F
          break
        }
        box_time <- 0
      }
      ###实验结束
      if (flag == T) {
        pass_time <- pass_time +1
      }  
      box <- sample(1:n)  #随机箱子
     }
    ###实验结束
    print(paste0('犯人成功的概率解放的概率：',round(pass_time / nreps,4)))
  }
}

Pone(5,1,10000)
Pone(50,1,10000)
Pone(100,1,10000)