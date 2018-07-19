numIterations = 10000
numPeople = 7

simulation = function(nIterations, nPeople){

  seasons = matrix(data = 1:364, nrow = 91, ncol = 4)
  results = matrix(data = NA, nrow = nIterations, ncol = ncol(seasons))
  seasonSums = vector(length = 4)
  
  for(j in 1:nIterations){

    people = matrix(data = sample(1:365, nPeople), nrow = 1, ncol = nPeople)
    seasonalBirthdays = matrix(data = NA, nrow = ncol(seasons), ncol = ncol(people))

    for(i in 1:ncol(seasons))
      seasonalBirthdays[i,] = apply(people, MARGIN = 2, FUN = function(x) x %in% seasons[,i]) 

    for(i in 1:nrow(seasonalBirthdays))
      seasonSums[i] = sum(seasonalBirthdays[i,])

    results[j,] = seasonSums
  }  
   return(results)
}

results = simulation(numIterations, numPeople)

boolResults = apply(results[,1:4], MARGIN = 2, function(x) x >= 1)

prob = sum(apply(boolResults, MARGIN = 1, all))/numIterations
prob
    
