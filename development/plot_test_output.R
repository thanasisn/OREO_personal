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

listfiles <- list.files( cnf$ERA5$path_raw, full.names = T)

NC <- open.nc("~/DATA/ERA5_domos_regrid/test_output/2020_DJF.nc")
print.nc(NC)
U <- var.get.nc(NC, "U")
V <- var.get.nc(NC, "V")

# NC <- open.nc("~/DATA/ERA5_domos_raw/ERA5_2019_90N-90S-180W180E.nc")
# print.nc(NC)
# U <- var.get.nc(NC, "u")
# V <- var.get.nc(NC, "v")

dim(U)
dim(V)

summary(U)
summary(V)

plot(raster(U[, , 1]))
plot(raster(t(U[, , 1])[ncol(U):1, ]))

u1 <- raster(t(U[, , 1])[ncol(U):1, ])
v1 <- raster(t(V[, , 1])[ncol(V):1, ])

# u1 <- raster((U[, , 1]))
# v1 <- raster((V[, , 1]))
#
# u1 <- raster(t(U[, , 1]))
# v1 <- raster(t(V[, , 1]))


w <- brick(u1, v1)

wlon <- var.get.nc(NC, "Longitude")
wlat <- var.get.nc(NC, "Latitude")

range(wlon)
range(wlat)

projection(w) <- CRS("EPSG:4326")
extent(w)     <- c(min(wlon), max(wlon), min(wlat), max(wlat))
w
summary(w)

# plot(w[[1]])
# plot(w[[2]])

vectorplot(w * 10, isField = "dXY", region = FALSE, margin = FALSE, narrows = 5000)

slope  <- sqrt(w[[1]]^2 + w[[2]]^2)
aspect <- atan2(w[[1]], w[[2]])
vectorplot(w * 1, isField = "dXY", region = slope, margin = FALSE,
           par.settings = rasterTheme(region = blue2red(n = 20)),
           narrows = 10000)

vectorplot(stack(slope * 10, aspect), isField = TRUE, region = FALSE, margin = FALSE)

vectorplot(w, isField = T)


## with terra
map <- vect("~/GISdata/Layers/world-administrative-boundaries/world-administrative-boundaries.shp")
# plot(map)
# uv <- rast("~/OREO/operation/DOMOS/2020_DJF.nc")
# plot(uv)

library(latticeExtra)
library(sp)

map <- as(map, "Spatial")

vectorplot(w * 10,
           isField = "dXY",
           region = slope,
           margin = FALSE,
           par.settings = rasterTheme(region = blue2red(n = 20)),
           narrows = 1000) +
  layer(sp.polygons(map))



NC <- open.nc("~/DATA/ERA5_domos_regrid/test_output/2020_DJF.nc")
print.nc(NC)
U <- var.get.nc(NC, "U")
V <- var.get.nc(NC, "V")

wlon <- var.get.nc(NC, "Longitude")
wlat <- var.get.nc(NC, "Latitude")

levels <- dim(U)[3]

for (ll in levels) {
  u <- raster(t(U[, , ll])[ncol(U):ll, ])
  v <- raster(t(V[, , ll])[ncol(V):ll, ])
  ## stack layers
  w <- brick(u, v)
  ## apply geo coordinates
  projection(w) <- CRS("EPSG:4326")
  extent(w)     <- c(min(wlon), max(wlon), min(wlat), max(wlat))
  slope         <- sqrt(w[[1]]^2 + w[[2]]^2)
  p <- vectorplot(w * 10,
             isField = "dXY",
             region = slope,
             margin = FALSE,
             par.settings = rasterTheme(region = blue2red(n = 20)),
             narrows = 1000,
             main = ll) +
    layer(sp.polygons(map))
  show(p)
}



NC <- open.nc("~/DATA/ERA5_domos_regrid/ERA5_2020_Q1_DJF_42N25S-80W25E_test.nc")
print.nc(NC)
U <- var.get.nc(NC, "U")
V <- var.get.nc(NC, "V")

wlon <- var.get.nc(NC, "Longitude")
wlat <- var.get.nc(NC, "Latitude")

levels <- dim(U)[1]

for (ll in levels) {
  u <- raster(t(U[, , ll])[ncol(U):ll, ])
  v <- raster(t(V[, , ll])[ncol(V):ll, ])
  ## stack layers
  w <- brick(u, v)
  ## apply geo coordinates
  projection(w) <- CRS("EPSG:4326")
  extent(w)     <- c(min(wlon), max(wlon), min(wlat), max(wlat))
  slope         <- sqrt(w[[1]]^2 + w[[2]]^2)
  p <- vectorplot(w * 10,
                  isField = "dXY",
                  region = slope,
                  margin = FALSE,
                  par.settings = rasterTheme(region = blue2red(n = 20)),
                  narrows = 1000,
                  main = ll) +
    layer(sp.polygons(map))
  show(p)
}


NC <- open.nc("~/DATA/ERA5_domos_regrid/ERA5_2020_Q1_DJF_42N25S-80W25E.nc")
print.nc(NC)
U <- var.get.nc(NC, "u")
V <- var.get.nc(NC, "v")

wlon <- var.get.nc(NC, "longitude")
wlat <- var.get.nc(NC, "latitude")

levels <- dim(U)[3]

for (ll in levels) {
  u <- raster(t(U[, , ll]))#[ncol(U):ll, ])
  v <- raster(t(V[, , ll]))#[ncol(V):ll, ])
  ## stack layers
  w <- brick(u, v)
  ## apply geo coordinates
  projection(w) <- CRS("EPSG:4326")
  extent(w)     <- c(min(wlon), max(wlon), min(wlat), max(wlat))
  slope         <- sqrt(w[[1]]^2 + w[[2]]^2)
  p <- vectorplot(w * 10,
                  isField = "dXY",
                  region = slope,
                  margin = FALSE,
                  par.settings = rasterTheme(region = blue2red(n = 20)),
                  narrows = 1000,
                  main = ll) +
    layer(sp.polygons(map))
  show(p)
}


## --------------------
library(metR)
library(data.table)
library(ggplot2)


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
    arrow.length = 0.1) +
  labs(title=basename(afile), subtitle=pressure,
       x = "Longitude", y="Latitude",
       fill = expression(m))

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
    arrow.length = 0.1) +
  labs(title=basename(afile), subtitle=pressure,
       x = "Longitude", y="Latitude",
       fill = expression(m))






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
    arrow.length = 0.1) +
  labs(title=basename(afile), subtitle=pressure,
       x = "Longitude", y="Latitude",
       fill = expression(m))






#' \FloatBarrier
#+ results="asis", echo=FALSE
# goodbye()
