library(tidyverse)

load("../RData/histolist-compliant.RData")
hlist_com <- histolist

load("../RData/histolist-ITT.RData")
hlist_itt <- histolist

rm(histolist)


outcomes <- names(hlist_com)
title_names <- c("Total CPD", "CESD", "CESD (Binary)", "CO", "log TNE" ,
  "log NNAL", "log PheT", "log CEMA", "log PGEM", "log ISO", "Weight Gain")

# get xlims across compliant and ITT analyses
xl <- list()

for (outcome in outcomes)
  xl[[outcome]]<- range(c(hlist_com[[outcome]]$breaks, hlist_itt[[outcome]]$breaks))

cols <- c(rgb(248, 203, 173, maxColorValue = 255), rgb(189, 215, 238, maxColorValue = 255))

mag <- 1.5

for (outcome in outcomes) {
  # compliers histogram
  pdf(sprintf("plots/%s/%s/trt-diff-histograms-%s.pdf", "lasso", "compliant", outcome))
  par(cex.axis = mag, cex.lab = mag, cex.main = 2)
  h <- hlist_com[[outcome]]
  ccat <- cut(h$breaks, c(-Inf, 0, Inf), right = FALSE)
  plot(h, 
    freq = FALSE, 
    col = cols[ccat], 
    border = "white",
    main = sprintf("%s, Compliers Analysis", title_names[match(outcome, outcomes)]),
    xlim = xl[[outcome]],
    #xlab = title_names[match(outcome, outcomes)]
    xlab = "Estimated Treatment Effect",
    sub  = "(Control - Treatment)"
    )
  lines(rep(h$mean, 2), c(0, 1.05 * max(h$density)), lty = 2, lwd = 2)
  dev.off()

  # ITT histogram
  pdf(sprintf("plots/%s/%s/trt-diff-histograms-%s.pdf", "lasso", "ITT", outcome))
  par(cex.axis = mag, cex.lab = mag, cex.main = 2)
  h <- hlist_itt[[outcome]]
  ccat <- cut(h$breaks, c(-Inf, 0, Inf), right = FALSE)
  plot(h, 
    freq = FALSE, 
    col = cols[ccat], 
    border = "white",
    main = sprintf("%s, ITT Analysis", title_names[match(outcome, outcomes)]),
    xlim = xl[[outcome]],
    # xlab = title_names[match(outcome, outcomes)],
    xlab = "Estimated Treatment Effect",
    sub  = "(Control - Treatment)",
    ylim = c(0, 1.25 * max(h$density))
    )
  lines(rep(h$mean, 2), c(0, 1.05 * max(h$density)), lty = 2, lwd = 2)
  legend <- outcome == outcomes[1]
  if (legend) {
    legend("topleft", 
      pch    = 15, 
      cex    = mag, 
      col    = c("white", cols), 
      legend = c("Better outcome in:", "Control Group", "Immediate Reduction Group"),
      bty    = "n")  
  }

  dev.off()
}
