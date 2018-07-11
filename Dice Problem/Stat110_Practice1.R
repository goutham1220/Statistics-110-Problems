library(ggplot2)

#Problem 1
# decide whether the blank should be filled in with =, <, or >, and give a short but clear explanation.
#(probability that the total after rolling 4 fair dice is 21)___(probability that the total after rolling 4 fair dice is 22)


sum1 = 21
sum2 = 22
iterations = 10000

die1 = sample(6, iterations, replace = TRUE)
die2 = sample(6, iterations, replace = TRUE)
die3 = sample(6, iterations, replace = TRUE)
die4 = sample(6, iterations, replace = TRUE)
sumDies = die1 + die2 + die3 + die4



prob1 = mean(sumDies == sum1)
prob2 = mean(sumDies == sum2)

prob1 > prob2

qplot(sumDies, geom = "histogram", bins = 22) +
  annotate("text", x = sum1, y = -iterations/100, label = "*", size = 15, color = 5) +
  annotate("text", x = sum2, y = -iterations/100, label = "*", size = 15, color = 4)

subsetProb1 = vector()
subsetProb2 = vector()

for(i in 1:iterations){
  subsetProb1[i] = mean(sumDies[1:i] == sum1)
  subsetProb2[i] = mean(sumDies[1:i] == sum2)
}

ggplot(data.frame(subsetProb1), aes(x = 1:iterations)) + 
  geom_line(aes(y=subsetProb1, color = 5)) +
  geom_line(aes(y=subsetProb2, color = 4))
