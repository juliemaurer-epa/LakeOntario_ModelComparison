#skill stat function:

skill.stat <- function(predicted, actual) {
  # sum of (predicted - actual)^2
  numerator <- sum((predicted - actual)^2)
  
  #sum of (absval(predicted - avg.actual) - absval(actual - avg.actual))^2
  denominator <- sum((abs(predicted - mean(actual)) + abs(actual - mean(actual)))^2)
  
  #calculate skill stat
  skill <- 1 - numerator/denominator
  
  return(skill)
}