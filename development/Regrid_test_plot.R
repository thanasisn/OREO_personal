# /* #!/usr/bin/env Rscript */
# /* Copyright (C) 2025 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title:         "ERA5 regrid test plots"
#' author:        "Natsis Athanasios"
#' institute:     "AUTH"
#' affiliation:   "Laboratory of Atmospheric Physics"
#' documentclass: article
#' classoption:   a4paper,oneside
#' fontsize:      10pt
#' geometry:      "left=20mm,right=20mm,top=20mm,bottom=20mm"
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
#'     keep_tex:         yes
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
knitr::opts_chunk$set(dev       = c("pdf", "png"))
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
# library(RNetCDF,    warn.conflicts = FALSE, quietly = TRUE)
# library(ncdf4,      warn.conflicts = FALSE, quietly = TRUE)
library(terra,      warn.conflicts = FALSE, quietly = TRUE)
# library(raster,     warn.conflicts = FALSE, quietly = TRUE)
# library(rasterVis,  warn.conflicts = FALSE, quietly = TRUE) # devtools::install_github("oscarperpinan/rastervis")
library(metR,       warn.conflicts = FALSE, quietly = TRUE)


#+ include=T, echo=F, results="asis"
##  Open dataset  --------------------------------------------------------------
cnf_domus <- paste0("~/OREO/operation/run_profiles/", Sys.info()["nodename"], ".yaml")
cnf <- read_yaml(cnf_domus)


pander(t(cnf$D1), caption = cnf$D1$name)


monthly_files <- list.files(
  path = paste0(cnf$ERA5$path_regrid,
                "/Monthly_",
                cnf$D1$LatStep, "x", cnf$D1$LonStep),
  recursive  = T,
  full.names = T)

seasonal_files <- list.files(
  path = paste0(cnf$ERA5$path_regrid,
                "/Seasonal_",
                cnf$D1$LatStep, "x", cnf$D1$LonStep),
  recursive  = T,
  full.names = T)

raw_files <- list.files(
  path = cnf$ERA5$path_raw,
  full.names = T
)

##  Choose inputs  -------------------------------------------------------------
aseas <- "Q1_DJF"
ayear <- 2020
amont <- "M01"

fl_regrid_ses <- grep(aseas, grep(ayear, seasonal_files, value = T), value = T)
fl_regrid_mon <- grep(amont, grep(ayear, monthly_files,  value = T), value = T)
fl_raw        <- grep(ayear, raw_files, value = T)



##  Raw ERA5 data  -------------------------------------------------------------
afile    <- fl_raw
pressure <- 1000

#'
#' \newpage
#' \FloatBarrier
#'
#' # Raw ERA5 data at 0.25x0.25 for `r ayear`
#'
#' **File: `r basename(afile)`**
#'
#' **Pressure: `r pressure`**
#'
#+ era5-raw, include=T, echo=F, warning=FALSE

afile    <- fl_raw
pressure <- 1000
wind     <- ReadNetCDF(afile,
                       subset = list(latitude  = cnf$D1$North:cnf$D1$South,
                                     longitude = cnf$D1$East :cnf$D1$West,
                                     pressure_level = pressure))
wind <- wind[valid_time == "2020-01-01"]



ggplot(wind, aes(longitude, latitude, fill = Mag(u, v))) +
  geom_tile(
    width  = 0.25,
    height = 0.25) +
  borders("world",
          xlim   = range(wind$longitude),
          ylim   = range(wind$latitude),
          colour = "gray10",
          size   = .4) +
  theme_bw() +
  theme(panel.ontop = TRUE, panel.background = element_blank()) +
  scale_fill_distiller(
    palette = "Spectral",
    limits  = c(0, wind[, max(Mag(u, v))])) +
  coord_quickmap(xlim = c(cnf$D1$West, cnf$D1$East),
                 ylim = c(cnf$D1$South,cnf$D1$North)) +
  geom_vector(
    aes(
      mag   =   Mag(u, v),
      angle = Angle(u, v)
    ),
    skip         = 10,
    arrow.length = 0.3) +
  labs(
    # title    = basename(afile),
    # subtitle = paste("Level", pressure),
    x        = expression(Latitude  ~ group("[",degree,"]")),
    y        = expression(Longitude ~ group("[",degree,"]")),
    fill     = expression(m/s)
  )


##  Manolis regridded ERA5 data  -----------------------------------------------

#'
#' \FloatBarrier
#'
#' # Regridded ERA5 data at 5x2 for Winter data by Manolis
#'
#+ era5-regrid-manolis, include=T, echo=F, warning=FALSE

afile    <- "~/DATA/ERA5_domos_regrid/test_output/2020_DJF.nc"
pressure <- 1000
wind     <- ReadNetCDF(afile,
                       subset = list(lev = pressure))
wind <- wind[Longitude >= cnf$D1$West  & Longitude <= cnf$D1$East]
wind <- wind[Latitude  >= cnf$D1$South & Latitude  <= cnf$D1$North]

ggplot(wind, aes(Longitude, Latitude, fill = Mag(U, V))) +
  geom_tile(width = cnf$D1$LonStep, height = cnf$D1$LatStep) +
  borders("world",
          xlim   = range(wind$Longitude),
          ylim   = range(wind$Latitude),
          colour = "gray10",
          size   = .4) +
  theme_bw() +
  theme(panel.ontop = TRUE, panel.background = element_blank()) +
  scale_fill_distiller(
    palette = "Spectral",
    limits  = c(0, wind[, max(Mag(U, V))])) +
  coord_quickmap(xlim = c(cnf$D1$West, cnf$D1$East),
                 ylim = c(cnf$D1$South,cnf$D1$North)) +
  geom_vector(
    aes(
      mag   =   Mag(U, V),
      angle = Angle(U, V)
    ),
    skip         = 0,
    arrow.length = 0.3) +
  labs(
    # title    = basename(afile),
    # subtitle = paste("Level", pressure),
    x        = expression(Latitude  ~ group("[",degree,"]")),
    y        = expression(Longitude ~ group("[",degree,"]")),
    fill     = expression(m/s)
  )

cat("Longitudes:")
sort(unique(wind$Longitude))


cat("Latitudes:")
sort(unique(wind$Latitude))



##  Thanasis regridded ERA5 data  ----------------------------------------------

afile    <- fl_regrid_ses
level    <- 1

## __ Mean seasonal ERA5 data  -------------------------------------------------
#'
#' \FloatBarrier
#'
#' # Seasonal regridded ERA5 data at `r paste0(cnf$D1$LatStep, "x", cnf$D1$LonStep)` for `r ayear`, `r aseas`
#'
#' **File: `r basename(afile)`**
#'
#' **Level: `r level`**
#'
#' ## Mean of components
#'
#+ era5-regrid-mean-seas, include=T, echo=F, warning=FALSE, out.width="100%"

wind     <- ReadNetCDF(afile)
# wind[longitude == -77.5 & latitude == 43]
wind     <- wind[pressure_level == level]

ggplot(wind, aes(longitude, latitude, fill = Mag(u_mean, v_mean))) +
  geom_tile(width = cnf$D1$LonStep, height = cnf$D1$LatStep) +
  borders("world",
          xlim   = range(wind$longitude),
          ylim   = range(wind$latitude),
          colour = "gray10",
          size   = .4) +
  theme_bw() +
  theme(panel.ontop = TRUE, panel.background = element_blank()) +
  scale_fill_distiller(
    palette = "Spectral",
    limits  = c(0, wind[, max(Mag(u_mean, v_mean))])) +
  coord_quickmap(xlim = c(cnf$D1$West,  cnf$D1$East),
                 ylim = c(cnf$D1$South, cnf$D1$North)) +
  geom_vector(
    aes(
      mag   =   Mag(u_mean, v_mean),
      angle = Angle(u_mean, v_mean)
    ),
    skip         = 0,
    arrow.length = 0.3,
    show.legend  = F) +
  labs(
    x        = expression(Latitude  ~ group("[",degree,"]")),
    y        = expression(Longitude ~ group("[",degree,"]")),
    fill     = expression(m/s)
  )


## __ Median seasonal ERA5 data  -----------------------------------------------
#'
#' ## Median of components
#'
#+ era5-regrid-median-seas, include=T, echo=F, warning=FALSE, out.width="100%"

ggplot(wind, aes(longitude, latitude, fill = Mag(u_median, v_median))) +
  geom_tile(width = cnf$D1$LonStep, height = cnf$D1$LatStep) +
  borders("world",
          xlim   = range(wind$longitude),
          ylim   = range(wind$latitude),
          colour = "gray10",
          size   = .4) +
  theme_bw() +
  theme(panel.ontop = TRUE, panel.background = element_blank()) +
  scale_fill_distiller(
    palette = "Spectral",
    limits  = c(0, wind[, max(Mag(u_median, v_median))])) +
  coord_quickmap(xlim = c(cnf$D1$West,  cnf$D1$East),
                 ylim = c(cnf$D1$South, cnf$D1$North)) +
  geom_vector(
    aes(
      mag   =   Mag(u_median, v_median),
      angle = Angle(u_median, v_median)
    ),
    skip         = 0,
    arrow.length = 0.3,
    show.legend  = F) +
  labs(
    x        = expression(Latitude  ~ group("[",degree,"]")),
    y        = expression(Longitude ~ group("[",degree,"]")),
    fill     = expression(m/s)
  )




afile <- fl_regrid_mon
level <- 1
MM    <- as.numeric(sub("M", "", amont))

#'
#' \FloatBarrier
#'
#' # Monthly regridded ERA5 data at `r paste0(cnf$D1$LatStep, "x", cnf$D1$LonStep)` for `r ayear`, `r month.name[MM]`
#'
#' **File: `r basename(afile)`**
#'
#' **Level: `r level`**
#'
#' **Month: `r MM`**
#+ include=T, echo=F, warning=FALSE, out.width="100%"


## __ Mean monthly ERA5 data  --------------------------------------------------

#'
#' ## Mean of components
#'
#+ era5-regrid-mean-month, include=T, echo=F, warning=FALSE, out.width="100%"

wind     <- ReadNetCDF(afile)
wind     <- wind[pressure_level == level]

ggplot(wind, aes(longitude, latitude, fill = Mag(u_mean, v_mean))) +
  geom_tile(width = cnf$D1$LonStep, height = cnf$D1$LatStep) +
  borders("world",
          xlim   = range(wind$longitude),
          ylim   = range(wind$latitude),
          colour = "gray10",
          size   = .4) +
  theme_bw() +
  theme(panel.ontop = TRUE, panel.background = element_blank()) +
  scale_fill_distiller(
    palette = "Spectral",
    limits  = c(0, wind[, max(Mag(u_mean, v_mean))])) +
  coord_quickmap(xlim = c(cnf$D1$West,  cnf$D1$East),
                 ylim = c(cnf$D1$South, cnf$D1$North)) +
  geom_vector(
    aes(
      mag   =   Mag(u_mean, v_mean),
      angle = Angle(u_mean, v_mean)
    ),
    skip         = 0,
    arrow.length = 0.3,
    show.legend  = F) +
  labs(
    x        = expression(Latitude  ~ group("[",degree,"]")),
    y        = expression(Longitude ~ group("[",degree,"]")),
    fill     = expression(m/s)
  )


## __ Median monthly ERA5 data  ------------------------------------------------

#'
#' ## Median of components
#'
#+ era5-regrid-median-month, include=T, echo=F, warning=FALSE, out.width="100%"

ggplot(wind, aes(longitude, latitude, fill = Mag(u_median, v_median))) +
  geom_tile(width = cnf$D1$LonStep, height = cnf$D1$LatStep) +
  borders("world",
          xlim   = range(wind$longitude),
          ylim   = range(wind$latitude),
          colour = "gray10",
          size   = .4) +
  theme_bw() +
  theme(panel.ontop = TRUE, panel.background = element_blank()) +
  scale_fill_distiller(
    palette = "Spectral",
    limits  = c(0, wind[, max(Mag(u_median, v_median))])) +
  coord_quickmap(xlim = c(cnf$D1$West,  cnf$D1$East),
                 ylim = c(cnf$D1$South, cnf$D1$North)) +
  geom_vector(
    aes(
      mag   =   Mag(u_median, v_median),
      angle = Angle(u_median, v_median)
    ),
    skip         = 0,
    arrow.length = 0.3,
    show.legend  = F) +
  labs(
    x        = expression(Latitude  ~ group("[",degree,"]")),
    y        = expression(Longitude ~ group("[",degree,"]")),
    fill     = expression(m/s)
  )





## __ Height monthly ERA5 data  ------------------------------------------------

#'
#' ## Height monthly data on level `r level`
#'
#+ era5-regrid-height-month, include=T, echo=F, warning=FALSE, out.width="100%"

wind     <- ReadNetCDF(afile)
wind     <- wind[pressure_level == level]

ggplot(wind, aes(longitude, latitude, fill = height)) +
  geom_tile(width = cnf$D1$LonStep, height = cnf$D1$LatStep) +
  borders("world",
          xlim   = range(wind$longitude),
          ylim   = range(wind$latitude),
          colour = "gray10",
          size   = .4) +
  theme_bw() +
  theme(panel.ontop = TRUE, panel.background = element_blank()) +
  scale_fill_distiller(
    palette = "Spectral",
    limits  = c(0, NA)) +
  coord_quickmap(xlim = c(cnf$D1$West,  cnf$D1$East),
                 ylim = c(cnf$D1$South, cnf$D1$North)) +
  # geom_vector(
  #   aes(
  #     mag   =   Mag(u_mean, v_mean),
  #     angle = Angle(u_mean, v_mean)
  #   ),
  #   skip         = 0,
  #   arrow.length = 0.3,
  #   show.legend  = F) +
  labs(
    x        = expression(Latitude  ~ group("[", degree, "]")),
    y        = expression(Longitude ~ group("[", degree, "]")),
    fill     = "m.a.s.l"
  )



#'
#' ## Domain specs
#'
#+ include=T, echo=F, warning=FALSE, out.width="100%"

cat("Longitudes:")
sort(unique(wind$longitude))


cat("Latitudes:")
sort(unique(wind$latitude))



#' \FloatBarrier
#+ results="asis", echo=FALSE
# goodbye()
