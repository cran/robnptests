## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
cache = TRUE,
collapse = TRUE,
comment = "#>"
)

## ---- echo = FALSE, fig.height = 10, fig.width = 8----------------------------
load(file.path("..", "R", "sysdata.rda"))

# M-estimators to plot ----
estimators <- unique(results.normal$estimator)

# Plot results ----
par(mfrow = c(3, 1))

# Normal distribution ----

# Equal sample sizes
for (i in seq_along(estimators)) {
  inds <- which(results.normal[, "estimator"] == estimators[i])
  p.value <- results.normal[inds, "size"]
  std.err <- results.normal[inds, "std.error"]
  x <- 1:length(inds)
  m <- results.normal[inds, "m"]
  n <- results.normal[inds, "n"]
  
  cols <- c("black", "red", "green")
  
  if (i == 1) {
    plot(x, p.value,
         ylim = c(0, 0.18),
         xlab = "Sample sizes",
         ylab = "Size of the test",
         main = "N(0, 1)-distribution",
         col = cols[i],
         xaxt = "n",
         yaxt = "n"
    )
    axis(side = 1, at = 1:10, labels = unique(paste0("(", m, ", ", n, ")")))
    axis(side = 2, at = seq(0.025, 0.175, by = 0.025), labels = c("", "0.05", "", "0.1", "", "0.15", ""))
  } else {
    points(x, p.value,
           col = cols[i],
    )
  }
  
  abline(h = 0.05, lty = "dashed")
}

# t2-distribution ----
for (i in seq_along(estimators)) {
  inds <- which(results.t[, "estimator"] == estimators[i])
  p.value <- results.t[inds, "size"]
  std.err <- results.t[inds, "std.error"]
  x <- 1:length(inds)
  m <- results.t[inds, "m"]
  n <- results.t[inds, "n"]
  
  cols <- c("black", "red", "green")
  
  if (i == 1) {
    plot(x, p.value,
         ylim = c(0, 0.18),
         xlab = "Sample sizes",
         ylab = "Size of the test",
         main = "t(2)-distribution",
         col = cols[i],
         xaxt = "n",
         yaxt = "n"
    )
    axis(side = 1, at = 1:10, labels = unique(paste0("(", m, ", ", n, ")")))
    axis(side = 2, at = seq(0.025, 0.175, by = 0.025), labels = c("", "0.05", "", "0.1", "", "0.15", ""))
  } else {
    points(x, p.value,
           col = cols[i],
    )
  }
  
    abline(h = 0.05, lty = "dashed")
}

# Chi-square distribution ----
for (i in seq_along(estimators)) {
  inds <- which(results.chi[, "estimator"] == estimators[i])
  p.value <- results.chi[inds, "size"]
  std.err <- results.chi[inds, "std.error"]
  x <- 1:length(inds)
  m <- results.chi[inds, "m"]
  n <- results.chi[inds, "n"]
  
  cols <- c("black", "red", "green")
  
  if (i == 1) {
    plot(x, p.value,
         ylim = c(0, 0.18),
         xlab = "Sample sizes",
         ylab = "Size of the test",
         main = "Chi^2(3)-distribution",
         col = cols[i],
         xaxt = "n",
         yaxt = "n"
    )
    axis(side = 1, at = 1:10, labels = unique(paste0("(", m, ", ", n, ")")))
    axis(side = 2, at = seq(0.025, 0.175, by = 0.025), labels = c("", "0.05", "", "0.1", "", "0.15", ""))
  } else {
    points(x, p.value,
           col = cols[i],
    )
  }
  
  legend("bottomleft", legend = c("Huber", "Hampel", "Bisquare"), col = 1:3, pch = 1)
  
  abline(h = 0.05, lty = "dashed")
}

## -----------------------------------------------------------------------------
library(robnptests)

sessionInfo()

