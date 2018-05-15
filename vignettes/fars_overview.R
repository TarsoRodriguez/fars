## ---- echo=FALSE, results='hide'-----------------------------------------
source("../R/fars_functions.R")
library(dplyr)

## ----OccurrencySummary, cache=TRUE---------------------------------------
setwd(system.file("extdata", package = "fars"))
df_occurrency_summary <- fars_summarize_years(c(2013, 2014, 2015))
head(df_occurrency_summary)

## ----Map, cache=TRUE, fig.align='center', fig.height=4, fig.width=4------
setwd(system.file("extdata", package = "fars"))
fars_map_state(1, 2013)

