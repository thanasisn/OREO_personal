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
library(lubridate,  warn.conflicts = FALSE, quietly = TRUE)
library(pander,     warn.conflicts = FALSE, quietly = TRUE)
library(ggplot2,    warn.conflicts = FALSE, quietly = TRUE)
library(yaml,       warn.conflicts = FALSE, quietly = TRUE)
library(colorRamps, warn.conflicts = FALSE, quietly = TRUE)
library(RNetCDF,    warn.conflicts = FALSE, quietly = TRUE)
library(raster,     warn.conflicts = FALSE, quietly = TRUE)
library(rasterVis,  warn.conflicts = FALSE, quietly = TRUE) # devtools::install_github("oscarperpinan/rastervis")


#+ include=T, echo=F, results="asis"
##  Open dataset  --------------------------------------------------------------
cnf_domus <- paste0("~/OREO/operation/DOMOS/", Sys.info()["nodename"], ".yaml")
cnf <- read_yaml(cnf_domus)

##  Daily data -----------------------------------------------
#'
#' \FloatBarrier
#' \newpage
#'
#' # Daily data
#'
#+ include=T, echo=T, results="asis", warning=FALSE

listfiles <- list.files( cnf$ERA5$path_raw, full.names = T)

NC <- open.nc("~/OREO/operation/DOMOS/2020_DJF.nc")


print.nc(NC)

U <- var.get.nc(NC, "U")
V <- var.get.nc(NC, "V")

dim(U)
dim(V)

summary(U)
summary(V)

plot(raster(U[, , 1]))


plot(raster(t(U[, , 1])[ncol(U):1, ]))

u1 <- raster(t(U[, , 1])[ncol(U):1, ])
v1 <- raster(t(V[, , 1])[ncol(V):1, ])

u1 <- raster((U[, , 1]))
v1 <- raster((V[, , 1]))

w <- brick(u1, v1)

wlon <- var.get.nc(NC, "Longitude")
wlat <- var.get.nc(NC, "Latitude")

range(wlon)
range(wlat)

projection(w) <- CRS("EPSG:4326")
extent(w)     <- c(min(wlon), max(wlon), min(wlat), max(wlat))
w

plot(w[[1]])
plot(w[[2]])

vectorplot(w * 10, isField = "dXY", region = FALSE, margin = FALSE, narrows = 10000)

slope <- sqrt(w[[1]]^2 + w[[2]]^2)
aspect <- atan2(w[[1]], w[[2]])
vectorplot(w * 10, isField = "dXY", region = slope, margin = FALSE, par.settings = rasterTheme(region = matlab.like(n = 10)),
           narrows = 10000, at = 0:10)


vectorplot(stack(slope * 10, aspect), isField = TRUE, region = FALSE, margin = FALSE)

readOGR(dsn = "~/GISdata/Layers/world-administrative-boundaries/", layer = "countries", stringsAsFactors = TRUE)



#' \FloatBarrier
#+ results="asis", echo=FALSE
# goodbye()
