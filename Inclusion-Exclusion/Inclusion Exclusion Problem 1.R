library("plot3D")
library(reshape2)
library("plot3Drgl")
library("gganimate")
library(rgl)


numIterations = 1000
numPeople = 7

simulation = function(nIterations, nPeople){

  seasons = matrix(data = 1:364, nrow = 91, ncol = 4)
  results = matrix(data = NA, nrow = nIterations, ncol = ncol(seasons))

  for(j in 1:nIterations){

    people = matrix(data = sample(1:364, nPeople, replace = TRUE), nrow = 1, ncol = nPeople)
    seasonalBirthdays = matrix(data = NA, nrow = ncol(seasons), ncol = ncol(people))

    seasonalBirthdays = sapply(people, function(x) which(seasons == x, arr.ind = TRUE))[2,]
    
    results[j,] = as.vector(table(factor(seasonalBirthdays, levels = c(1, 2, 3, 4))))
  }  
   return(results)
}

results = simulation(numIterations, numPeople)

boolResults = apply(results[,1:4], MARGIN = 2, function(x) x >= 1)

prob = sum(apply(boolResults, MARGIN = 1, all))/numIterations
prob

colnames(results) = c("winter", "spring", "summer", "fall")
results = data.frame(results)
results$id = 1:numIterations
results.melt = melt(results, id = "id")


x = 1:4
y = 1:numIterations
z = results

scatter3D(x=as.numeric(factor(results.melt$variable)), y=results.melt$id, results.melt$value, phi = 0, bty = "g",  type = "h", 
          ticktype = "detailed", pch = 19, cex = 0.5)


heatmap(results[,-c("id")])
