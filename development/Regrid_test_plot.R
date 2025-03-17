# /* #!/usr/bin/env Rscript */
# /* Copyright (C) 2025 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title:         "Regrid test plots"
#' author:        "Natsis Athanasios"
#' institute:     "AUTH"
#' affiliation:   "Laboratory of Atmospheric Physics"
#' documentclass: article
#' classoption:   a4paper,oneside,landscape
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
#'     fig_width:        10
#'     fig_height:       5
#'
#' date: "`r format(Sys.time(), '%F')`"
#'
#' ---
#+ include=F



#+ include=F
## __ Document options  --------------------------------------------------------
knitr::opts_chunk$set(comment   = ""      )
knitr::opts_chunk$set(dev       = "pdf"   )
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
Script.Name  <- "~/OREO/development/plot_test_output.R"

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
library(ncdf4,      warn.conflicts = FALSE, quietly = TRUE)
library(terra,      warn.conflicts = FALSE, quietly = TRUE)
library(raster,     warn.conflicts = FALSE, quietly = TRUE)
library(rasterVis,  warn.conflicts = FALSE, quietly = TRUE) # devtools::install_github("oscarperpinan/rastervis")
library(metR)
library(data.table)
library(ggplot2)


#+ include=T, echo=F, results="asis"
##  Open dataset  --------------------------------------------------------------
cnf_domus <- paste0("~/OREO/operation/run_profiles/", Sys.info()["nodename"], ".yaml")
cnf <- read_yaml(cnf_domus)


pander(t(cnf$D1))

#'
#' \newpage
#' \FloatBarrier
#'
#' # Raw ERA5 data at 0.25x0.25 for January
#'
#+ include=T, echo=F, warning=FALSE, out.width="100%"

afile <- "~/DATA/ERA5_domos_raw/ERA5_2020_44N24S-80W30E.nc"
pressure <- 1000
wind <- ReadNetCDF(afile,
                   subset = list(latitude  = cnf$D1$North:cnf$D1$South,
                                 longitude = cnf$D1$East :cnf$D1$West,
                                 pressure_level = pressure))
wind <- wind[valid_time =="2020-01-01"]

ggplot(wind, aes(longitude, latitude, fill = Mag(u + v))) +
  geom_tile(width = 0.25, height = 0.25) +
  borders('world', xlim = range(wind$longitude), ylim=range(wind$latitude),
          colour='gray90', size=.2) +
  theme_bw() +
  theme(panel.ontop=TRUE, panel.background=element_blank())+
  scale_fill_distiller(palette='Spectral') +
  coord_quickmap(xlim = c(cnf$D1$West, cnf$D1$East),
                 ylim = c(cnf$D1$South,cnf$D1$North)) +
  geom_vector(aes(
    mag = Mag(u, v),
    angle = Angle(u, v)),
    skip = 10,
    arrow.length = 0.3) +
  labs(title=basename(afile), subtitle=pressure,
       x = "Longitude", y="Latitude",
       fill = expression(m/s))

#'
#' \newpage
#' \FloatBarrier
#'
#' # Regridded ERA5 data at 5x2 for Winter by Manolis
#'
#+ include=T, echo=F, warning=FALSE, out.width="100%"

afile <- "~/DATA/ERA5_domos_regrid/test_output/2020_DJF.nc"
pressure <- 1000
wind <- ReadNetCDF(afile,
                   subset = list(lev = pressure))
wind <- wind[Longitude >= cnf$D1$West  & Longitude <= cnf$D1$East]
wind <- wind[Latitude  >= cnf$D1$South & Latitude  <= cnf$D1$North]

ggplot(wind, aes(Longitude, Latitude, fill = Mag(U + V))) +
  geom_tile(width = cnf$D1$LonStep, height = cnf$D1$LatStep) +
  borders('world', xlim = range(wind$Longitude), ylim=range(wind$Latitude),
          colour='gray90', size=.2) +
  theme_bw() +
  theme(panel.ontop=TRUE, panel.background=element_blank())+
  scale_fill_distiller(palette='Spectral') +
  coord_quickmap(xlim = c(cnf$D1$West, cnf$D1$East),
                 ylim = c(cnf$D1$South,cnf$D1$North)) +
  geom_vector(aes(
    mag = Mag(U, V),
    angle = Angle(U, V)),
    arrow.length = 0.3) +
  labs(title=basename(afile), subtitle=pressure,
       x = "Longitude", y="Latitude",
       fill = expression(m/s))



#'
#' \newpage
#' \FloatBarrier
#'
#' # Regridded ERA5 data at 5x2 for Winter
#'
#' With xarray. Very fast, some limitation of the available stats.
#'
#' The cell doesn't include points of the next cell, so the  actual centres are at every (5 - 0.25) / 2 and (2 - 0.25) / 2
#'
#+ include=T, echo=F, warning=FALSE, out.width="100%"


afile <- "~/DATA/ERA5_domos_regrid/ERA5_2020_Q1_DJF_42N25S-80W25E.nc"
pressure <- 1000
wind <- ReadNetCDF(afile,
                   subset = list(latitude  = cnf$D1$North:cnf$D1$South,
                                 longitude = cnf$D1$East :cnf$D1$West,
                                 pressure_level = pressure))

ggplot(wind, aes(longitude, latitude, fill = Mag(u + v))) +
  geom_tile(width = cnf$D1$LonStep, height = cnf$D1$LatStep) +
  borders('world', xlim = range(wind$longitude), ylim=range(wind$latitude),
          colour='gray90', size=.2) +
  theme_bw() +
  theme(panel.ontop=TRUE, panel.background=element_blank())+
  scale_fill_distiller(palette='Spectral') +
  coord_quickmap(xlim = c(cnf$D1$West, cnf$D1$East),
                 ylim = c(cnf$D1$South,cnf$D1$North)) +
  geom_vector(aes(
    mag = Mag(u, v),
    angle = Angle(u, v)),
    arrow.length = 0.3) +
  labs(title=basename(afile), subtitle=pressure,
       x = "Longitude", y="Latitude",
       fill = expression(m/s))



#'
#' \newpage
#' \FloatBarrier
#'
#' # Regridded ERA5 data at 5x2 for Winter
#'
#' With numpy array computation. Slow, flexible computations.
#'
#+ include=T, echo=F, warning=FALSE, out.width="100%"


afile   <- "~/DATA/ERA5_domos_regrid/ERA5_2020_Q1_DJF_42N25S-80W25E_test.nc"
pressure <- 1000
wind <- ReadNetCDF(afile,
                   subset = list(lev = pressure))

## long lat are inverted!
ggplot(wind, aes(Latitude, Longitude, fill = Mag(U + V))) +
  geom_tile(width = cnf$D1$LonStep, height = cnf$D1$LatStep) +
  borders('world', xlim = range(wind$Longitude), ylim=range(wind$Latitude),
          colour='gray90', size=.2) +
  theme_bw() +
  theme(panel.ontop=TRUE, panel.background=element_blank())+
  scale_fill_distiller(palette='Spectral') +
  coord_quickmap(xlim = c(cnf$D1$West, cnf$D1$East),
                 ylim = c(cnf$D1$South,cnf$D1$North)) +
  geom_vector(aes(
    mag = Mag(U, V),
    angle = Angle(U, V)),
    arrow.length = 0.3) +
  labs(title=basename(afile), subtitle=pressure,
       x = "Longitude", y="Latitude",
       fill = expression(m))






#' \FloatBarrier
#+ results="asis", echo=FALSE
# goodbye()
