
# O'brien's rank sum for multiple endpoints function ----------------------

obrien_ranksum <-function(dataset, group, vars, highlow){
  groupnames <- names(table(dataset[, group]))
  o  = cbind(vars, highlow)
  n <- dim(dataset)[1]
  
  # Rank function assigns one to lowest value
  # Higher is better
  hvars = o[highlow=="high", "vars"]
  h = length(hvars)
  df_hvars = data.frame(dataset[, hvars])
  if (h != 0) {
    df_hvars = apply(df_hvars, 2, function(x) rank(x, ties.method = "first"))
    colnames(df_hvars) = paste0(hvars, "_rank")
  }
  
  # Lower is better
  lvars = o[highlow=="low", "vars"]
  l = length(lvars)
  df_lvars = data.frame(dataset[, lvars])
  if(l != 0){
    df_lvars = apply(df_lvars, 2, function(x) rank(-x, ties.method = "first"))
    colnames(df_lvars) = paste0(lvars, "_rank")
  }
  
  # Ranked data sets
  ranked.data = cbind(dataset[, c(group, vars)], df_hvars, df_lvars)
  ranked.data$ranked.sums = rowSums(ranked.data[-(1:(length(ranked.data)-length(vars)))])
  ranked.data = ranked.data[, c("ranked.sums", group)]
  
  # T-test to determine t statistic
  t.ranked <- t.test(ranked.data[,1] ~ ranked.data[,2], var.equal = TRUE, alternative = "two.sided")
  k.w.ranked <- kruskal.test(ranked.data[,1] ~ ranked.data[,2])
  p.f.ranked <- 1-pf((t.ranked$statistic)^2, 1, n-2) # F distribution
  p.t.ranked <- pt(t.ranked$statistic, n-2) # Student T distribution
  p.k.w.ranked <- k.w.ranked$p.value # Kruskal-Wallis rank sum test
  
  
  ranked.data.sum <- cbind(group = groupnames, 
                           rank.sum = as.vector(rowsum(ranked.data[, "ranked.sums"], ranked.data[, group])))
  
  f.dist.p.value <- paste0("P-value: ", round(p.f.ranked, 4))
  t.dist.p.value <- paste0("P-value: ", round(p.t.ranked, 4))
  k.w.p.value <- paste0("P-value: ", round(p.k.w.ranked, 4))
  
  
  obrien_ranksum_summary <- list("Variables and Order" = o,
                                 "Number of Observations" = n,
                                 "Rank Sums for Each Group" = ranked.data.sum, 
                                 "F Distribution" = f.dist.p.value,
                                 "Student T Distribution" = t.dist.p.value,
                                 "Kruskal Wallis Rank Sum Test" = k.w.p.value)
  
  return(obrien_ranksum_summary)
}

# O'brien's OLS for multiple endpoints function ---------------------------

obrien_ols <-function(dataset, vars, group){
  # Set parameters
  groupnames <- names(table(dataset[, group]))
  m = length(vars)
  # Separate dataset 1 and find N
  datasetg1 <- dataset[dataset[, group] == groupnames[1], c(vars)]
  n1 <- dim(datasetg1)[1]
  # Separate dataset 2 and find N
  datasetg2 <- dataset[dataset[, group] == groupnames[2], c(vars)]
  n2 <- dim(datasetg2)[1]
  # Calculate means for each variable
  xbar1.k <- colMeans(datasetg1, na.rm=TRUE)
  xbar2.k <- colMeans(datasetg2, na.rm=TRUE)
  # Calculate variance for each variable
  s21.k <- apply(datasetg1, 2, var, na.rm=TRUE)
  s22.k <- apply(datasetg2, 2, var, na.rm=TRUE)
  s2..k <- ((n1-1)*s21.k + (n2-1)*s22.k)/(n1+n2-2) # Calculate Pooled Variance
  s..k <- sqrt(s2..k) # Calculate Pooled Standard Deviation
  xbar..k <- (1/(n1+n2))*(n1*xbar1.k + n2*xbar2.k) # Calculate the Pooled Mean
  # Difference between values and pooled mean
  xstar1jk_ <- (t(t(datasetg1) - xbar..k[1:m]))
  xstar2jk_ <- (t(t(datasetg2) - xbar..k[1:m]))
  # Calculate and combine Z scores
  xstar1jk <- (t(t(xstar1jk_)/(s..k)))
  xstar2jk <- (t(t(xstar2jk_)/(s..k)))
  xstarcomb <- rbind(xstar1jk, xstar2jk)
  ### Here is where I can do an (IF OLS or IF GLS) ###
  xstarcomb <- xstarcomb %*% matrix(1,m,1) # Combine Z scores for endpoints
  xstarcomb <- cbind(xstarcomb, as.vector(c(rep(1,n1), rep(2, n2)))) # Add groups
  Rhat <- cor(rbind(xstar1jk, xstar2jk),  use = "complete.obs") # Correlations between Z scores
  # T-test to determine t statistic
  tols <- t.test(xstarcomb[,1] ~ xstarcomb[,2], var.equal = TRUE, alternative = "two.sided")
  pols <- 1-pf((tols$statistic)^2, 1, n1+n2-2) # F distribution
  pt(tols$statistic, n1+n2-2) # Student t distribution - cumulative probability
  if(pols < 0.05)
  {
    dec <- 1
  } else {dec <- 0}
  return(list(pols,dec))
}

