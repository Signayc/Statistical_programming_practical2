################################################################
#Task 5
'In order to calculate the probabilities, we need to find out the times each
length appears in all simulations. The first loop is used to indicate all the
experiments, and the second loop is used to indicate n prisoners participated
in turn. The third loops are used to simulate the process of each prisoner
finding their cards. The length of every prisoner will be recorded by adding
one to the corresonding item in list freq. At the end of each simulation, those
length with freq more than 0 will be added by one in list cnt. After all the
simulations are completed, the probabilities can be calculated by dividing cnt
by nreps.'



# to estimate the probabilities of loop length from 1 to 2n
dloop = function(n,nreps){
  PR = numeric(2*n) # 2n-vector of each length's probability
  cnt = numeric(2*n) # counts each length occurs at least once
  
  # nreps simulations
  for (i in 1:nreps) {
    boxes = sample(1:(2*n),replace = F) # randomly shuffle cards to boxes
    freq = numeric(2*n) # the frequency each length occur in one simulation
    
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




################################################################
#Task 6



# estimate the probabilities for n = 50, nreps = 10000
prob = dloop(50,10000)
print(prob)
plot(prob)

# calculate the probability that there is no loop longer than 50
ans = 1
for (i in 51:length(prob)) {
  ans = ans - prob[i]
}
print(ans)


# calculate the probability that there is no loop longer than 50
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
dloop2(50,10000)