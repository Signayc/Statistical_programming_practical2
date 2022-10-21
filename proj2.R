# Group 15
# Members: Yani Shi(s2308441); Baoyan Deng(s2402728); Qiming Xiong(s2442309)
# Github repo address:
# https://github.com/Signayc/Statistical_programming_practical2.git
#Group contribution: 
#Sally Shi: Q1&Q3-4 (approximately 1/3 of the work)
#Baoyan Deng: Q2&Q3-4 (approximately 1/3 of the work)
#Qiming Xiong: Q5-6 (approximately 1/3 of the work)


################################################################################
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
    flag <- 0    #set a flag to check whether successful or not
    number_of_boxes = 2*n
    #Repeated nreps timesï¼Œcalculate the total successful probability of individuals   
    
    # nreps times' tests begin
    for (i in c(1:nreps)) {
      # randomly allocate cards to each box in every loop
      box <- sample(c(1:number_of_boxes))  
      box_time <- 0    # the times of try in one loop
      if (strategy == 3) {
        box_num = sample(c(1:number_of_boxes),n)  # randomly open n boxes
        if (k %in% box[box_num]) {
          flag <- flag + 1
        }
      }
      else if(strategy == 1 | strategy == 2) {
        if (strategy == 1) {
          box_num = k  # open box number k on first try
        }
        else if (strategy == 2){
          box_num = sample(c(1:number_of_boxes),1)  
          # random choose the first box to open
        }
        while (box[box_num] != k & box_time <= n) {
          box_time <- box_time + 1
          box_num = box[box_num]
        }
        if (box_time <= n) {
          flag <- flag + 1
        }
      }
    }
    ###tests end
    print(paste0('The probability of a single prisoner succeeding in finding ', 
                 'their number: ',flag / nreps))
}


################################################################################
# Task 2
# The main difference between task1 and task2 is that now we want to know the
# success times of all prisoners escape. A variable single_escape_time is set 
# to store the number of prisoners successfully escape in each loop. And we test
# whether single_escape_time is equal to the number of all prisoners when each
# loop ends. If the value of single_escape_time is equal to the number of all
# prisoners then we have one success time of all escape, and store the success 
# times of all escapes in variable all_escape_times.

Pall <- function(n,strategy,nreps=10000) {
  number_of_boxes = 2*n
  all_escape_time <- 0 # success times of all escape
  for (i in c(1:nreps)) {
    single_escape_time = 0  # success times of single prisoner escape in each loop
    # randomly allocate cards to each box in every loop
    box <- sample(c(1:number_of_boxes))  
    # Tests begin
    for (j in c(1:number_of_boxes)) {
      box_time <- 0    # the times of try for each prisoner in one loop
      if (strategy == 3){
        box_num = sample(c(1:number_of_boxes),n)  # randomly open n boxes
        if (j %in% box[box_num]) {
          single_escape_time = single_escape_time + 1
        } else {
          break
        }
      }
      else if (strategy == 1 | strategy == 2) {
        if (strategy == 1) {
          box_num = j  # open box number j on first try
        }
        else if (strategy == 2) {
          box_num = sample(c(1:number_of_boxes),1)  
          # random choose the first box to open
        }
        repeat {
          box_time <- box_time + 1
          box_num = box[box_num]
          if (box_time > n) {
            break
          }
          if (box[box_num] == j) {
            single_escape_time = single_escape_time + 1
            break
          }
        }
      }
    }
    if (single_escape_time == number_of_boxes) {
      all_escape_time <- all_escape_time + 1  
      # if all prisoners escape in one loop, we have one success time of all escape
    }
  }
    ###Tests end
    print(paste0('the probability of all prisoners finding their numberï¼?',              
                 all_escape_time / nreps))
}


################################################################################
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


################################################################################
# Task 4: 
# When comparing the three strategies, the difference of escape probability for
# a single prisoner is not very large, the result of strategy 1 and strategy 3
# do not show a significant difference. However, when considering all prisoners  
# successfully escape, the probability for strategies 2 and 3 is close to zero 
# at n=5, while the probability of all escape for strategy 1 is still about 50%. 
# But as n increases further (e.g. n=50) the probability of all escape for strategy 
# 1 is still about 30%, while the probability of all escape for the other two 
# strategies is equal to 0. This shows the advantages of strategy 1: 1) the 
# probability of all escape is relatively high comparing with other strategies, 
# and 2) strategy 1 maintains the probability of all escape at around 30% when 
# the number of prisoners increases.
# Probability of all escape under strategy 1 actually can be turned into probability
# that there is no loop longer than n in a randomly arranged vector of length 2n.
# We can calculate this probability, 1-sum(1/(n+1)+...+1/2n), the result is approximately
# equal to 1-ln(2), about 0.3. So Regardless of how large n is taken, the success 
# escape rate for all prisoners under strategy 1 is always maintained at about 30%.


################################################################################
# Task 5:
# In order to calculate the probabilities, we need to find out the times each
# length appears in all simulations. The first loop is used to indicate all the
# experiments, and the second loop is used to indicate n prisoners participated
# in turn. The third loops are used to simulate the process of each prisoner
# finding their cards. The length of every prisoner will be recorded by adding
# one to the corresponding item in list freq. At the end of each simulation, those
# length with freq more than 0 will be added by one in list cnt. After all the
# simulations are completed, the probabilities can be calculated by dividing cnt
# by nreps.

# to estimate the probabilities of loop length from 1 to 2n
dloop <- function(n,nreps) {
  PR = numeric(2*n) 
  # 2n-vector of each length's probability
  cnt = numeric(2*n) 
  # the counts of each length occur at least once in 2n simulations
  
  # nreps simulations
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
    
    # record the counts each length occurs at least once
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


################################################################################
# Task 6:

# estimate the probabilities for n = 50, nreps = 10000
prob = dloop(50,10000)

# plot the probabilities the occurrence of loop of each length
plot(prob,type = "l", xlab = "Length of Loops",ylab = "Probability",
     main = "Probability of different length of loops")

# calculate the probability that there is no loop longer than 50

# The main difference between function dloop and dloop2 is that in dloop2 we 
# update and store the length of the longest loop in a simulation by comparing 
# max_len continuously, then compare max_len with n=50 at the end of each 
# simulation. If there are no loops longer than 50 in that simulation then 
# cnt+1, and finally calculate the probability that there is no loop longer than
# 50 by cnt.


dloop2 = function(n,nreps){
  PR = 0
  cnt = 0 # the counts of the simulations without loop longer than 50
  
  # nreps simulations
  for (i in 1:nreps) {
    boxes = sample(1:(2*n),replace = F) # randomly shuffle cards to boxes
    max_len = 0 # record the longest length in each simulation
    
    # 2n prisoners complete the simulation in turn
    for (j in 1:(2*n)) {
      len = 1
      k = j # the next box to open
      
      # break until find the card with their number
      while (boxes[k] != j) {
        k = boxes[k]
        len = len + 1
      }
      max_len = max(max_len,len) # change the maximum if necessary
    }
    
    # add the count if there is no loop longer than 50 in this simulation
    if (max_len <= 50)
      cnt = cnt + 1
  }
  
  # calculate the probability that there is no loop longer than 50
  PR = cnt/nreps
  return(PR)
}

# estimate the probability for n = 50, nreps = 10000
probability <- dloop2(50,10000)
print(paste0("Probability that there is no loop longer than 50 in a random ",
             "reshuffling of cards to boxes is ", probability))
