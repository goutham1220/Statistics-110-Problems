library(ggplot2)

# Problem: A gambler repeatedly plays a game where in each round, he wins a dollar with
# probability 1/3 and loses a dollar with probability 2/3. His strategy is "quit
# when he is ahead by $2," though some suspect he is a gambling addict anyway.
# Suppose that he starts with a million dollars. Show that the probability that
# he'll ever be ahead by $2 is less than 1/4.

set.seed(98)
iterations = 1000
amount = 300
resultCounts = vector()
maxValue = amount + 2
amountTrack = vector()

simulation = function(amt){
  
  amt = amt + sample(c(1, -1), 1, prob = c(1/3, 2/3))
  
  if(amt == maxValue){
    return(1)
  } else if(amt == 0){
    return(0)
  }
  
  return(simulation(amt))
}

for(i in 1:iterations){
  resultCounts[i] = simulation(amount)
}

mean(resultCounts)

trackResults = function(amt, amtTrack){
  
  amt = amt + sample(c(1, -1), 1, prob = c(1/3, 2/3))
  amtTrack = c(amtTrack, amt)
  if(amt == maxValue | amt == 0){
    return(list(amt, amtTrack))
    } 
  return(trackResults(amt, amtTrack))
}

results = trackResults(amount, amountTrack)

qplot(x = 1:length(results[[2]]), y = results[[2]] , geom = "point") + labs(x = "Rounds", y = "Amount")

