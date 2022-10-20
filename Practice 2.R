# Group 15
# Members: Yani Shi(s2308441); Baoyan Deng(s2402728); Qiming Xiong(s2442309)
# Github repo address:
# https://github.com/Signayc/Statistical_programming_practical2.git
#Group contribution: 
#Sally Shi: Q1&Q3-4 (approximately 1/3 of the work)
#Baoyan Deng: Q2&Q3-4 (approximately 1/3 of the work)
#Qiming Xiong: Q5-6 (approximately 1/3 of the work)

#############################################
# Task 1
# Firstly, for each strategy, the number of cards in a box of length 2n needs to 
# be reallocated in each loop, stored as a vector of length 2n named box. 
# A variable flag is set to store the cumulative times of successful escapes of
# prisoners in loops (nreps).
# For strategy 1, the default box number, box_num, chosen by the prisoner for the
# first time in each loop is k. If the value of box[box_num] is not the prisoner's 
# number k, then the number box_num of the next box chosen is box[box_num], number 
# of the card in the box chosen this time, and so on. Each prisoner has only n 
# attempts, and a variable box_time is set to store the cumulative times of 
# attempts for each prisoner in each loop. If the number of attempts exceeds n, 
# the prisoner fails to escape, and if the number of attempts does not exceed n, 
# the prisoner succeeds once and flag + 1.
# The only difference between strategy 2 and strategy 1 is that the first box 
# number, box_num, chosen by the prisoner for the first time in each loop is a 
# random number in the range 1:2n.
# The design of strategy 3 differs from strategies 1 and 2 in that it directly
# selects a vector of length n in the range 1:2n as the selected box_num, 
# and if the prisoner's number k is in the card numbers corresponding to box  
# numbers selected, box_num, then the escape is successful, otherwise then the 
# escape fails.

Pone <- function(n,k,strategy,nreps=10000) {
  #Strategy 1: The first box number is fixed k.
  if (strategy==1) {
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
    print(paste0('The probability of a single prisoner succeeding in finding their number:',
                 flag / nreps))
  }
  
  
  #Strategy 2: The first box number is random.
  if (strategy==2) {
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
    print(paste0('The probability of a single prisoner succeeding in finding their number:',
                 flag / nreps))
  }
  
  #Strategy 3: They open n boxes at random.
  if (strategy==3) {
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
    print(paste0('The probability of a single prisoner succeeding in finding their number:',
                 flag / nreps))
  }
  
}



#######################################################################
# Task 2
# The main difference between task1 and task2 is that now we want to know the
# success times of all prisoners escape. A variable flag is set to store if any
# prisoner fail to escape in each loop. If any prisoner fail then there's no
# need to continue that loop as our goal of all escape fails. If flag remains
# TRUE in one loop then we have one success of all escape. A new variable
# pass_time is created to store the success times of all escapes.

Pall <- function(n,strategy,nreps=10000) {
  #Strategy 1
  if (strategy==1) {
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
    print(paste0('the probability of all prisoners finding their number：',
                 pass_time / nreps))
  }
  
  #Strategy 2
  if (strategy==2) {
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
    print(paste0('the probability of all prisoners finding their number：',
                 pass_time / nreps))
  }
  
  
  
  # Strategy 3
  if (strategy==3) {
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
    print(paste0('the probability of all prisoners finding their number：',
                 pass_time / nreps))
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
Pall(100,1,10000)


Pall(5,2,10000)
Pall(50,2,10000)


Pall(5,3,10000)
Pall(50,3,10000)



# Task 4: 
# When comparing the three strategies, the difference in escape probability for
# a single prisoner is not very large, and at this point strategy 1 and strategy 3
# do not show a significant difference. However, when all prisoners are successfully 
# escaped, the probability of all escape for strategies 2 and 3 is close to zero 
# at n=5, while the probability of all escape for strategy 1 is still about 30%, 
# and as n increases further (e.g. n=50) the probability of all escape for strategy 
# 1 is still about 30%, while the probability of all escape for the other two 
# strategies is equal to 0. This shows the advantages of strategy 1: 1. the 
# probability of all escape is high, and 2. This reflects the advantages of 
# strategy 1: 1. the probability of all escape is high, and 2. strategy 1 
# maintains a high (around 30%) probability of all escape when the number of 
# prisoners increases.

# Task 5:
# to estimate the probabilities of loop length from 1 to 2n
dloop <- function(n,nreps){
  PR = numeric(2*n) # 2n-vector of each length's probability
  cnt = numeric(2*n) # the counts of each length occur at least once in 2n simulations
  
  # 2n simulations
  for (i in 1:nreps) {
    boxes = sample(1:(2*n),replace = F) # randomly shuffle cards to boxes
    freq = numeric(2*n) # the frequency of each length occur in one simulation
    
    # 2n prisoners complete the simulation in turn
    for (j in 1:(2*n)) {
      len = 1
      k = j # the next box to open
      
      # break until find the card with their number
      while (boxes[k] != j) {
        k = boxes[k]
        len = len + 1
      }
      freq[len] = freq[len] + 1
    }
    
    # count the number each length occur at least once in 2n simulations
    for (len in 1:(2*n)) {
      if (freq[len] != 0)
        cnt[len] = cnt[len] + 1
    }
  }
  
  # calculate the probability of each length
  for (len in 1:(2*n)) {
    PR[len] = cnt[len]/nreps
  }
  return(PR)
}

# Task 6:



# estimate the probabilities for n = 50, nreps = 10000
prob = dloop(50,10000)
prob_scale = prob/(sum(prob))
print(sum(prob_scale))

first50 = sum(prob_scale[1:50])
later50 = sum(prob_scale[51:100])
probability = first50
print(probability)

plot(prob,type = "l", xlab = "Length of Loops",ylab = "Probability",
     main = "Probability of different length of loops")


