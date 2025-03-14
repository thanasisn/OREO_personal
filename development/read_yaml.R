# /* #!/usr/bin/env Rscript */
# /* Copyright (C) 2025 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title:         "test"
#' author:        "Natsis Athanasios"
#' institute:     "AUTH"
#' affiliation:   "Laboratory of Atmospheric Physics"
#' documentclass: article
#' classoption:   a4paper,oneside
#' fontsize:      10pt
#' geometry:      "left=0.5in,right=0.5in,top=0.5in,bottom=0.5in"
#'
#' link-citations:  yes
#' colorlinks:      yes
#'
#' header-includes:
#' - \usepackage{caption}
#' - \usepackage{placeins}
#' - \captionsetup{font=small}
#'
#' output:
#'   bookdown::pdf_document2:
#'     number_sections:  no
#'     fig_caption:      no
#'     keep_tex:         no
#'     keep_md:          no
#'     latex_engine:     xelatex
#'     toc:              yes
#'     toc_depth:        5
#'     fig_width:        8
#'     fig_height:       4
#'   html_document:
#'     toc:        true
#'     fig_width:  9
#'     fig_height: 4
#'
#' date: "`r format(Sys.time(), '%F')`"
#'
#' ---
#+ include=F

#'
#' https://rpubs.com/alobo/vectorplot
#' https://docs.unidata.ucar.edu/idv/current/workshop/overview/WhatIsIdv.html
#'

#+ include=F
## __ Document options  --------------------------------------------------------
knitr::opts_chunk$set(comment   = ""      )
knitr::opts_chunk$set(dev       = "png"   )
knitr::opts_chunk$set(out.width = "100%"  )
knitr::opts_chunk$set(message   = FALSE   )
knitr::opts_chunk$set(fig.align = "center")
knitr::opts_chunk$set(fig.cap   = " empty caption ")
knitr::opts_chunk$set(fig.pos   = "!h"    )
knitr::opts_chunk$set(tidy = TRUE,
                      tidy.opts = list(
                        indent       = 4,
                        blank        = FALSE,
                        comment      = FALSE,
                        args.newline = TRUE,
                        arrow        = TRUE)
)

## __ Set environment  ---------------------------------------------------------
closeAllConnections()
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name  <- "~/OREO/development/test.R"

if (!interactive()) {
  pdf(file = paste0("~/OREO/REPORTS/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
}

## __ Load libraries  ----------------------------------------------------------
library(data.table, warn.conflicts = FALSE, quietly = TRUE)
library(dplyr,      warn.conflicts = FALSE, quietly = TRUE)
library(yaml,       warn.conflicts = FALSE, quietly = TRUE)


#+ include=T, echo=F, results="asis"
##  Open dataset  --------------------------------------------------------------
cnf_domus <- paste0("~/OREO/operation/run_profiles/", Sys.info()["nodename"], ".yaml")
cnf <- read_yaml(cnf_domus)


##  Daily data -----------------------------------------------
#'
#' \FloatBarrier
#' \newpage
#'
#' # Daily data
#'
#+ include=T, echo=T, results="asis", warning=FALSE


cnf$ERA5$West
cnf$ERA5$East
cnf$ERA5$LatStep

cnf$ERA5$South
cnf$ERA5$North
cnf$ERA5$LonStep

(25 %/% 3)*3
a <- seq(-19, 31 , .25)
a <- seq(19, 31 , .25)

sign(a) * (abs(a) %/% 5) * 5
sign(a) * (abs(ceiling(a)) %/% 5) * 5
sign(a) * (abs(ceiling(a*5)) %/% 5)

abs(a*5) %/% 5 * 5 /5

abs(ceiling(a*5)%/%5) * 5

sign(a) * (abs(a) + 5 - (abs(a) %% 5))

up <- c(-11, 12) # -> -10 15
dn <- c(-11, 12) # -> -15 10

sign(up) * (abs(up) + 5 - (abs(up) %% 5))

sign(up) * abs(up) %/% 5 * 5

abs(up) %% 5

up + 5 - up %% 5
dn - dn %% 5



ceiling()

#' \FloatBarrier
#+ results="asis", echo=FALSE
# goodbye()
