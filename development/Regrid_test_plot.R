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
library(terra,      warn.conflicts = FALSE, quietly = TRUE)
library(metR,       warn.conflicts = FALSE, quietly = TRUE)


#+ include=T, echo=F, results="asis"
##  Read config  ---------------------------------------------------------------
cnf_domus <- paste0("~/OREO/operation/run_profiles/", Sys.info()["nodename"], ".yaml")
cnf       <- read_yaml(cnf_domus)


pander(t(cnf$D1), caption = cnf$D1$name)
cnf$D1$North - cnf$D1$LatStep/2

# monthly_files <-
  list.files(
  path = paste0(cnf$ERA5$path_regrid,
                "/Monthly_",
                cnf$D1$LatStep, "x", cnf$D1$LonStep),
  pattern = paste0("ERA5_[0-9]{4}_M[0-9]{2}_lat_",
                   cnf$D1$South + cnf$D1$LatStep/2, "[.0-9]+_",),
  recursive  = T,
  full.names = T)

  cnf$D1$South + cnf$D1$South %% cnf$D1$LatStep
  cnf$D1$South %/% cnf$D1$LatStep

  seasonal_files <- list.files(
  path = paste0(cnf$ERA5$path_regrid,
                "/Seasonal_",
                cnf$D1$LatStep, "x", cnf$D1$LonStep),
  recursive  = T,
  full.names = T)

raw_files <- list.files(
  path    = cnf$ERA5$path_raw,
  pattern = paste0("ERA5_[0-9]{4}_lat_",
                   cnf$D1$South, "[.0-9]*_",
                   cnf$D1$North, "[.0-9]*_lon_",
                   cnf$D1$West,  "[.0-9]*_",
                   cnf$D1$East,  "[.0-9]*.nc"),
  full.names = T
)

##  Choose inputs  -------------------------------------------------------------
aseas <- "Q1_DJF"
ayear <- 2020
amont <- "M01"

fl_regrid_ses <- grep(aseas, grep(ayear, seasonal_files, value = T), value = T)
fl_regrid_mon <- grep(amont, grep(ayear, monthly_files,  value = T), value = T)
fl_raw        <- grep(ayear, raw_files, value = T)

plot_pad <- 2
P_North <- cnf$D1$North + plot_pad
P_South <- cnf$D1$South - plot_pad
P_East  <- cnf$D1$East  + plot_pad
P_West  <- cnf$D1$West  - plot_pad

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
                       subset = list(latitude  = P_North:P_South,
                                     longitude = P_East :P_West,
                                     pressure_level = pressure))
wind <- wind[valid_time == "2020-01-01"]
range(wind$latitude)
range(wind$longitude)

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
  coord_quickmap(xlim = c(P_West,  P_East),
                 ylim = c(P_South, P_North)) +
  geom_vector(
    aes(
      mag   =   Mag(u, v),
      angle = Angle(u, v)
    ),
    skip         = 10,
    arrow.length = 0.3,
    show.legend  = FALSE) +
  labs(
    # title    = basename(afile),
    # subtitle = paste("Level", pressure),
    y        = expression(Latitude  ~ group("[",degree,"]")),
    x        = expression(Longitude ~ group("[",degree,"]")),
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
wind <- wind[Longitude >= P_West  & Longitude <= P_East]
wind <- wind[Latitude  >= P_South & Latitude  <= P_North]

ggplot(wind, aes(Longitude, Latitude, fill = Mag(U, V))) +
  geom_tile(width = 5, height = 2) +
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
  coord_quickmap(xlim = c(P_West, P_East),
                 ylim = c(P_South,P_North)) +
  geom_vector(
    aes(
      mag   =   Mag(U, V),
      angle = Angle(U, V)
    ),
    skip         = 0,
    arrow.length = 0.3,
    show.legend  = F) +
  labs(
    # title    = basename(afile),
    # subtitle = paste("Level", pressure),
    y        = expression(Latitude  ~ group("[",degree,"]")),
    x        = expression(Longitude ~ group("[",degree,"]")),
    fill     = expression(m/s)
  )

cat("Longitudes:")
sort(unique(wind$Longitude))


cat("Latitudes:")
sort(unique(wind$Latitude))



##  Thanasis regridded ERA5 data  ----------------------------------------------

afile    <- fl_regrid_ses
level    <- 1

wind     <- ReadNetCDF(afile)
wind     <- wind[pressure_level == level]


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
#+ era5-regrid-mean-seas, include=T, echo=F, warning=FALSE, out.width="100%", results='asis'

cat("Longitude range:", range(wind$longitude), "\n")

cat("\nLatitude range:", range(wind$latitude), "\n")

cat("\nCell N points:", unique(wind$v_N), unique(wind$u_N), "\n")


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
  coord_quickmap(xlim = c(P_West,  P_East),
                 ylim = c(P_South, P_North)) +
  geom_vector(
    aes(
      mag   =   Mag(u_mean, v_mean),
      angle = Angle(u_mean, v_mean)
    ),
    skip         = 0,
    arrow.length = 0.3,
    show.legend  = F) +
  labs(
    y        = expression(Latitude  ~ group("[",degree,"]")),
    x        = expression(Longitude ~ group("[",degree,"]")),
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
  coord_quickmap(xlim = c(P_West,  P_East),
                 ylim = c(P_South, P_North)) +
  geom_vector(
    aes(
      mag   =   Mag(u_median, v_median),
      angle = Angle(u_median, v_median)
    ),
    skip         = 0,
    arrow.length = 0.3,
    show.legend  = F) +
  labs(
    y        = expression(Latitude  ~ group("[",degree,"]")),
    x        = expression(Longitude ~ group("[",degree,"]")),
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
#' \FloatBarrier
#'
#' ## Mean of components
#'
#+ era5-regrid-mean-month, include=T, echo=F, warning=FALSE, out.width="100%", results='asis'

wind     <- ReadNetCDF(afile)
wind     <- wind[pressure_level == level]

cat("Longitude range:", range(wind$longitude), "\n")

cat("\nLatitude range:", range(wind$latitude), "\n")

cat("\nCell N points:", unique(wind$v_N), unique(wind$u_N), "\n")



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
  coord_quickmap(xlim = c(P_West,  P_East),
                 ylim = c(P_South, P_North)) +
  geom_vector(
    aes(
      mag   =   Mag(u_mean, v_mean),
      angle = Angle(u_mean, v_mean)
    ),
    skip         = 0,
    arrow.length = 0.3,
    show.legend  = F) +
  labs(
    y        = expression(Latitude  ~ group("[",degree,"]")),
    x        = expression(Longitude ~ group("[",degree,"]")),
    fill     = expression(m/s)
  )


## __ Median monthly ERA5 data  ------------------------------------------------

#'
#' \FloatBarrier
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
  coord_quickmap(xlim = c(P_West,  P_East),
                 ylim = c(P_South, P_North)) +
  geom_vector(
    aes(
      mag   =   Mag(u_median, v_median),
      angle = Angle(u_median, v_median)
    ),
    skip         = 0,
    arrow.length = 0.3,
    show.legend  = F) +
  labs(
    y        = expression(Latitude  ~ group("[",degree,"]")),
    x        = expression(Longitude ~ group("[",degree,"]")),
    fill     = expression(m/s)
  )





## __ Height monthly ERA5 data  ------------------------------------------------

#'
#' \FloatBarrier
#'
#' ## Height monthly data on level `r level`
#'
#+ era5-regrid-height-month, include=T, echo=F, warning=FALSE, out.width="100%"

wind     <- ReadNetCDF(afile)
wind     <- wind[pressure_level == level]
pander(range(wind$longitude))
pander(range(wind$latitude))

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
  coord_quickmap(xlim = c(P_West,  P_East),
                 ylim = c(P_South, P_North)) +
  labs(
    y        = expression(Latitude  ~ group("[", degree, "]")),
    x        = expression(Longitude ~ group("[", degree, "]")),
    fill     = "km.a.s.l"
  )


ggplot(wind, aes(longitude, latitude, fill = v_N)) +
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
  coord_quickmap(xlim = c(P_West,  P_East),
                 ylim = c(P_South, P_North)) +
  labs(
    y        = expression(Latitude  ~ group("[", degree, "]")),
    x        = expression(Longitude ~ group("[", degree, "]")),
    fill     = "v_N"
  )



#'
#' \FloatBarrier
#'
#' ## Domain specs
#'
#+ include=T, echo=F, warning=FALSE, out.width="100%"

cat("Longitudes:")
sort(unique(wind$longitude))


cat("Latitudes:")
sort(unique(wind$latitude))

#'
#' \FloatBarrier
#'
#' ## Height boundary example
#'
#+ include=T, echo=T, warning=FALSE, out.width="100%"

wind     <- ReadNetCDF(afile)
pp <- wind[longitude == min(longitude) & latitude == min(latitude)] |> select(starts_with("heig"))

pander(pp)



#' \FloatBarrier
#+ results="asis", echo=FALSE
# goodbye()
