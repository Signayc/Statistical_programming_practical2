#############################################
#Task 1

Pone <- function(n,k,strategy,nreps=10000) {
  #Strategy 1: The first box number is fixed k.
  if (strategy==1) {
    #   set.seed(123)  
    box <- sample(1:n)  #randomly choose box
    names(box) <- 1:n
    flag <- 0    #set a flag to check whether successful or not
    #Repeated nreps times，calculate the total successful probability of individuals   
    
    # nreps times' tests begin
    for (i in c(1:nreps)) {
      box_time <- 0    #The times of openning box
      box_num = k  #number k on the first box
      while (box[box_num] != k & box_time <= (n/2)) {
        box_time <- box_time + 1
        box_num = box[box_num]
      }
      if (box_time <= (n/2)) {
        flag <- flag + 1
      }
      box <- sample(1:n)  #random boxes
      names(box) <- 1:n
    }
    ###tests end
    print(paste0('The probability of a single prisoner succeeding in finding their number:',flag / nreps))
  }
  
  
  #Strategy 2: The first box number is random.
  if (strategy==2) {
    #   set.seed(123)  
    box <- sample(1:n)  #randomly choose box
    names(box) <- 1:n
    flag <- 0    
    #Repeated nreps times，calculate the total successful probability of individuals   
    
    # nreps times' tests begin
    for (i in c(1:nreps)) {
      box_time <- 0    #The times of openning box
      box_num = sample(n,1)  #random number on the first box
      while (box[box_num] != k & box_time <= (n/2)) {
        box_time <- box_time + 1
        box_num = box[box_num]
      }
      if (box_time <= (n/2)) {
        flag <- flag + 1
      }
      box <- sample(1:n)  #random boxes
      names(box) <- 1:n
    }
    ###tests end
    print(paste0('The probability of a single prisoner succeeding in finding their number:',flag / nreps))
  }
  
  #Strategy 3: They open n boxes at random.
  if (strategy==3) {
    #   set.seed(123)  
    box <- sample(1:n)  #randomly choose box
    names(box) <- 1:n
    flag <- 0    
    #Repeated nreps times，calculate the total successful probability of individuals   
    
    # nreps times' tests begin
    for (i in c(1:nreps)) {
      box_num = sample(n,n/2)  #Randomly open box
      if (k %in% box[box_num]) {
        flag <- flag + 1
      }
      box <- sample(1:n)  #random boxes
      names(box) <- 1:n
    }
    ###Tests end
    print(paste0('The probability of a single prisoner succeeding in finding their number:',flag / nreps))
  }
  
}





########################################################################################
# Task 2

Pall <- function(n,strategy,nreps=10000) {
  #Strategy 1
  if (strategy==1) {
 #   set.seed(123)  
    
    flag <- F      # Single time success or not
    pass_time <- 0 # The Number of successful experiments
    box <- sample(1:n)  # random boxes
    names(box) <- 1:n
    for (i in c(1:nreps)) {
      box_time <- 0    # The Number of openings
      flag <- T
      # Tests begin
      for (j in c(1:n)) {
        box_num = j  # Open Box Number
        while (box[box_num] != j & box_time <= (n/2)) {
          box_time <- box_time + 1
          box_num = box[box_num]
        }
        if (box_time > (n/2)) {
          box_time <- 0
          flag <- F
          break
        }
        box_time <- 0
      }
      if (flag == T) {
        pass_time <- pass_time +1
      }  
      box <- sample(1:n)  # Random boxes
      names(box) <- 1:n
     }
    ###Tests end
    print(paste0('the probability of all prisoners finding their number：',pass_time / nreps))
  }
  
  #Strategy 2
  if (strategy==2) {
    #   set.seed(123) 
    
    flag <- F      # Single time success or not
    pass_time <- 0 # The Number of successful experiments
    box <- sample(1:n)  # Random boxes
    names(box) <- 1:n
    for (i in c(1:nreps)) {
      box_time <- 0    # The Number of openings
      flag <- T
      # Tests begin
      for (j in c(1:n)) {
        box_num = sample(n,1)  # Open Box Number
        while (box[box_num] != j & box_time <= (n/2)) {
          box_time <- box_time + 1
          box_num = box[box_num]
        }
        if (box_time > (n/2)) {
          box_time <- 0
          flag <- F
          break
        }
        box_time <- 0
      }
      if (flag == T) {
        pass_time <- pass_time +1
      }  
      box <- sample(1:n)  # Random boxes
      names(box) <- 1:n
    }
    ###Tests end
    print(paste0('the probability of all prisoners finding their number：',pass_time / nreps))
  }
  
  
  
  # Strategy 3
  if (strategy==3) {
    #   set.seed(123)  
    
    flag <- F      # Single time success or not
    pass_time <- 0 # The Number of successful experiments
    box <- sample(1:n)  # Random boxes
    names(box) <- 1:n
    for (i in c(1:nreps)) {
      flag <- F
      # Tests begin
      for (j in c(1:n)) {
        box_num = sample(n,n/2)  # Open Box Number
        if (j %in% box[box_num]) {
          flag <- T
        }else{
          flag <- F
          break
        }
      }
      if (flag == T) {
        pass_time <- pass_time +1
      }  
      box <- sample(1:n)  # Random boxes
      names(box) <- 1:n
    }
    ###Tests end
    print(paste0('the probability of all prisoners finding their number：',pass_time / nreps))
  }
  
  
  
}

# Task 3
Pone(5,3,1,10000)
Pone(50,4,1,10000)


Pone(5,3,2,10000)
Pone(50,4,2,10000)


Pone(5,3,3,10000)
Pone(50,4,3,10000)


Pall(5,1,10000)
Pall(50,1,10000)


Pall(5,2,10000)
Pall(50,2,10000)


Pall(5,3,10000)
Pall(50,3,10000)



# Task 4: After testing, we found that in each function, all strategy 1 have the highest probability compared to strategy 2 and 3.

