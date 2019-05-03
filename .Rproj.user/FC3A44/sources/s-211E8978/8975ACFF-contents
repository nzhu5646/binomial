# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

# name: Check Probability
# description: takes in a probability and checks if it's a valid number
# input: prob ~ a number to check for valid probability
# output: true or error
check_prob <- function(prob) {
  if (prob >= 0 && prob <= 1) {
    return (TRUE)
  } else {
    stop("Probability has to be a number between 0 and 1")
  }
}

# name: Check Trials
# description: takes in number of trials to check if valid
# input: trials - number of trials
# output: true or error
check_trials <- function(trials) {
  if (trials %% 1 == 0 && trials >= 0) {
    return (TRUE)
  } else {
    stop("Invalid Trials Value")
  }
}

# name: Check Success
# description: Check if sucess is a valid parameter for trials run
# input: sucess, trials
# output: true or error
check_success <- function(success, trials) {
  if (all(success %% 1 == 0) && all(success >= 0)) {
    if (all(success <= trials)) {
      return (TRUE)
    } else {
      stop ("Sucess cannot be greater than trials")
    }
  } else {
    stop("Invalid sucess value")
  }
}

# name: Mean
# description: Calculates mea
# input: trials, prob
# output: mean
aux_mean <- function(trials, prob) {
  return (trials * prob)
}

# name: Variance
# description: Calculates variance
# input: trials, prob
# output: variance
aux_variance <- function(trials, prob) {
  return (trials * prob * (1 - prob))
}

# name: Mode
# description: calculates mode
# input: trials, prob
# output: mode(s)
aux_mode <- function(trials, prob) {
  m = trials * prob + prob
  if (m %% 1 != 0) {
    return (floor(m))
  } else {
    return (c(m, m-1))
  }
}

# name: Skewness
# description: calculates skewness
# input: trials, prob
# output: skewness
aux_skewness <- function(trials, prob) {
  return ((1 - 2 * prob) / sqrt(trials * prob * (1 - prob)))
}

# name: Kurtosis
# description: calculates kurtosis
# input: trials, prob
# output: kurtosis
aux_kurtosis <- function(trials, prob) {
  return ((1 - 6 * prob * (1 - prob)) / (trials * prob * (1 - prob)))
}

#' @export
#' @title: Binomial Choose
#' @description: Calculates the number of combinations that where there are k successes in n trials
#' @param: n - number of trials; k - number of successes
#' @return: number of combinations
#' @examples: bin_choose(5, 2) = 10
bin_choose <- function(n, k) {
  if (all(k <= n)) {
    return(factorial(n) / (factorial(k) * factorial(n - k)))
  } else {
    stop("k cannot be greater than n")
  }
}

#' @export
#' @title: Binomial Probability
#' @description: Calculates the probability of number of k successes in n trials
#' @param: success, trials, prob
#' @return: probability of successes
#' @examples: bin_probability(2,5,0.5) = 0.3125
bin_probability <- function(success, trials, prob) {
  check_trials(trials)
  check_prob(prob)
  check_success(success, trials)
  return (bin_choose(trials, success) * (prob ^ success) * (1 - prob) ^ (trials - success))
}

#' @export
#' @title: Binomial Distribution
#' @description: Calculates the different probabilities of successes in n trials
#' @param: rials, prob
#' @return: dataframe of probabilities of different success in trials
#' @examples: bin_distribution(5, 0.5)
bin_distribution <- function(trials, prob) {
  df <- structure(data.frame("success"=seq(0,trials), "probability"=bin_probability(seq(0,trials), trials, prob)),
                             class=c("bindis", "data.frame"))
  return(df)
}

#' @export
plot.bindis <- function(df) {
  barplot(df$probability, names.arg=df$probability, xlab="successes", ylab="probability")
}

#' @export
#' @title: Binomial Cumulative
#' @description: Creates a dataframe with another column for cumulative probabilities
#' @param: trials, prob
#' @return: dataframe with individual and cumulative probabilities for success in trials
#' @examples: bin_cumulative(5, 0.5)
bin_cumulative <- function(trials, prob) {
  df <- structure(data.frame("success"=seq(0,trials), "probability"=bin_probability(seq(0,trials), trials, prob)),
                  class=c("bincum", "data.frame"))
  df$cumulative <- cumsum(df$probability)
  return (df)
}

#' @export
plot.bincum <- function (df) {
  plot(df$success, df$cumulative, xlab="successes", ylab="probability")
  lines(df$success, df$cumulative)
}

#' @export
#' @title: bin_variable
#' @description: creates a binomial variable
#' @param: trials, prob
#' @return: bincum object
#' @examples: bin_variable(5, 0.5)
bin_variable <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  ls <- list(trials, prob)
  names(ls) <- c("trials", "prob")
  return (structure(ls, class="binvar"))
}

#' @export
print.binvar <- function(var) {
  print("Binomial variable")
  print("Parameters")
  trials = paste("- number of trials:", var$trials)
  prob = paste("- prob of success:", var$prob)
  print(trials)
  print(prob)
}

#' @export
summary.binvar <- function(var) {
  trials <- var$trials
  prob <- var$prob
  ls <- list(trials, prob, aux_mean(trials, prob),
             aux_variance(trials, prob), aux_mode(trials, prob),
             aux_skewness(trials, prob), aux_kurtosis(trials, prob))
  names(ls) <- c("trials", "prob", "mean", "variance", "mode",
                 "skewness", "kurtosis")
  return (structure(ls, class="summary.binvar"))
}

#' @export
print.summary.binvar <- function(summary) {
  print("Summary Binomial")
  print("Parameters")
  trials <- paste("- number of trials:", summary$trials)
  print(trials)
  prob <- paste("- prob of success:", summary$prob)
  print(prob)
  print("Measures")
  mean <- paste("- mean:", summary$mean)
  print(mean)
  print(paste("- variance:", summary$variance))
  print(paste("- mode:", summary$mode))
  print(paste("- skewness:", summary$skewness))
  print(paste("-kurtosis:", summary$kurtosis))

}

#' @export
#' @title: Binomial Mean
#' @description: Calculates mean given a binomial distribution
#' @param: trials, prob
#' @return: mean
#' @examples: bin_mean(10, 0.3) = 3
bin_mean <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return (aux_mean(trials,prob))
}

#' @export
#' @title: Binomial Variance
#' @description: Calculates variance given a binomial distribution
#' @param: trials, prob
#' @return: variance
#' @examples: bin_variance(10, 0.3) = 2.1
bin_variance <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return (aux_variance(trials,prob))
}

#' @export
#' @title: Binomial Mode
#' @description: Calculates meode given a binomial distribution
#' @param: trials, prob
#' @return: mode
#' @examples: bin_mode(10, 0.3) = 3
bin_mode <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return (aux_mode(trials,prob))
}

#' @export
#' @title: Binomial Skewness
#' @description: Calculates skewness given a binomial distribution
#' @param: trials, prob
#' @return: skewness
#' @examples: bin_skewness(10, 0.3) = 0.2760202
bin_skewness <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return (aux_skewness(trials,prob))
}

#' @export
#' @title: Binomial Kurtosis
#' @description: Calculates kurtosis given a binomial distribution
#' @param: trials, prob
#' @return: kurtosis
#' @examples: bin_kurtosis(10, 0.3) = -0.1238095
bin_kurtosis <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return (aux_kurtosis(trials,prob))
}
