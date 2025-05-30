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
#'   bookdown::html_document2:
#'     number_sections:  no
#'     fig_caption:      no
#'     keep_tex:         yes
#'     keep_md:          yes
#'     toc:              yes
#'     toc_depth:        5
#'     fig_width:        10
#'     fig_height:       5
#'   bookdown::pdf_document2:
#'     number_sections:  no
#'     fig_caption:      no
#'     keep_tex:         yes
#'     keep_md:          yes
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
Script.Name  <- "~/OREO/development/Regrid_test_plot.R"

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


library(rgl,       warn.conflicts = FALSE, quietly = TRUE)
library(matlib,    warn.conflicts = FALSE, quietly = TRUE)

knitr::knit_hooks$set(webgl = hook_webgl)

#+ include=T, echo=F, results="asis"
##  Read config  ---------------------------------------------------------------
cnf_domus <- paste0("~/OREO/operation/run_profiles/", Sys.info()["nodename"], ".yaml")
cnf       <- read_yaml(cnf_domus)


pander((cnf$D1), caption = cnf$D1$name)


monthly_files <- list.files(
  path = paste0(cnf$ERA5$path_regrid,
                "/Monthly_",
                cnf$D1$LatStep, "x", cnf$D1$LonStep),
  pattern = paste0("ERA5_[0-9]{4}_M[0-9]{2}_lat_",
                   cnf$D1$South + cnf$D1$LatStep/2, "[.0-9]*_",
                   cnf$D1$North - cnf$D1$LatStep/2, "[.0-9]*_lon_",
                   cnf$D1$West  + cnf$D1$LonStep/2, "[.0-9]*_",
                   cnf$D1$East  - cnf$D1$LonStep/2, "[.0-9]*\\.nc"),
  recursive  = T,
  full.names = T)

seasonal_files <- list.files(
  path = paste0(cnf$ERA5$path_regrid,
                "/Seasonal_",
                cnf$D1$LatStep, "x", cnf$D1$LonStep),
  pattern = paste0("ERA5_[0-9]{4}_Q[0-9]_[MAJSONDF]{3}_lat_",
                   cnf$D1$South + cnf$D1$LatStep/2, "[.0-9]*_",
                   cnf$D1$North - cnf$D1$LatStep/2, "[.0-9]*_lon_",
                   cnf$D1$West  + cnf$D1$LonStep/2, "[.0-9]*_",
                   cnf$D1$East  - cnf$D1$LonStep/2, "[.0-9]*\\.nc"),
  recursive  = T,
  full.names = T)

raw_files <- list.files(
  path    = cnf$ERA5$path_raw,
  pattern = paste0("ERA5_[0-9]{4}_lat_",
                   cnf$D1$South, "[.0-9]*_",
                   cnf$D1$North, "[.0-9]*_lon_",
                   cnf$D1$West,  "[.0-9]*_",
                   cnf$D1$East,  "[.0-9]*.nc"),
  full.names = T)

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

cat("Longitude range: [", paste(range(wind$longitude), collapse = ", "), "]  ")
cat("Latitude range:  [", paste(range(wind$latitude),  collapse = ", "), "]\n")

cat("Latitudes:")
pander(cat(unique(wind$latitude)))

cat("Longitude:")
pander(cat(unique(wind$longitude)))


p <- ggplot(wind, aes(longitude, latitude, fill = Mag(u, v))) +
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
show(p)

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

cat("Longitude range: [", paste(range(wind$Longitude), collapse = ", "), "]")
cat("Latitude range:  [", paste(range(wind$Latitude),  collapse = ", "), "]")

cat("Latitudes:")
pander(cat(unique(wind$Latitude)))

cat("Longitude:")
pander(cat(unique(wind$Longitude)))


p <- ggplot(wind, aes(Longitude, Latitude, fill = Mag(U, V))) +
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
show(p)



##  Thanasis regridded ERA5 data  ----------------------------------------------

afile    <- fl_regrid_ses
level    <- 1

wind     <- ReadNetCDF(afile)
wind     <- wind[pressure_level == level]


## _ Seasonal ERA5 regrid  -----------------------------------------------------
#'
#' \FloatBarrier
#' \newpage
#'
#' # Seasonal regridded ERA5 data at `r paste0(cnf$D1$LatStep, "x", cnf$D1$LonStep)` for `r ayear`, `r aseas`
#'
#' **File: `r basename(afile)`**
#'
#' **Level: `r level`**
#'
#+ include=T, echo=F, warning=FALSE, out.width="100%"

cat("Longitude range: [", paste(range(wind$longitude), collapse = ", "), "]")
cat("Latitude range:  [", paste(range(wind$latitude),  collapse = ", "), "]")
cat("Cell N points:   [", paste(c(unique(wind$v_N), unique(wind$u_N)), collapse = ", "), "]")

cat("Latitudes:")
pander(cat(unique(wind$latitude)))

cat("Longitude:")
pander(cat(unique(wind$longitude)))

## __ Mean seasonal ERA5 data  -------------------------------------------------
#'
#' \FloatBarrier
#'
#' ## Mean of components
#'
#+ era5-regrid-mean-seas, include=T, echo=F, warning=FALSE, out.width="100%"

p <- ggplot(wind, aes(longitude, latitude, fill = Mag(u_mean, v_mean))) +
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
show(p)


## __ Median seasonal ERA5 data  -----------------------------------------------
#'
#' \FloatBarrier
#'
#' ## Median of components
#'
#+ era5-regrid-median-seas, include=T, echo=F, warning=FALSE, out.width="100%"

p <- ggplot(wind, aes(longitude, latitude, fill = Mag(u_median, v_median))) +
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
show(p)


##  Vertical bounds and magnitude  ---------------------------------------------

wind     <- ReadNetCDF(afile)
## select a random location
alon <- sample(unique(wind$longitude), 1)
alat <- sample(unique(wind$latitude),  1)

#'
#' \FloatBarrier
#'
#' ## Vertical bounds and wind magnitude at Lat: `r alat`, Lon: `r alon`
#'
#+ era5-regrid-vertical-magnitude-seas, include=T, echo=F, warning=FALSE, out.width="100%",height=10,width=5

pp <- wind[longitude == alon & latitude == alat]
pp[, cc := .I %% 2]

p <- ggplot(pp) +
  geom_rect(
    aes(ymin = height_low,
        ymax = height_up,
        xmin = 0,
        xmax = Mag(u_mean, v_mean),
        fill = cc)) +
  labs(
    x = expression(Wind~magnitude ~ group("[",m/s,"]")),
    y = expression(Altitude ~ group("[",m,"]"))
  ) +
  theme_bw() +
  theme(legend.position = "none")
show(p)




#'
#' \FloatBarrier
#'
#' ## Wind profile vectors
#'
#+ era5-regrid-3Dvec-seas, include=T, echo=F, warning=FALSE

start <- pp[, .(longitude,          latitude,          height)]
end   <- pp[, .(longitude + u_mean, latitude + v_mean, height)]

# rglwidget()
# plot3d(1,1,1,
#        xlim = range(start[, 1], end[, 1]),
#        ylim = range(start[, 2], end[, 2]),
#        zlim = range(start[, 3], end[, 3]),
#        xlab = "Longiture",
#        ylab = "Latitude",
#        zlab = "Altutude"
#        )
# bg3d(color = "white")
# for (a in 1:nrow(start)) {
#   arrow3d(matrix(start[a,], ncol = 3),
#           matrix(end  [a,], ncol = 3), type = "lines", col = "green")
# }
#
#
#
# rglwidget()
# plot3d(1,1,1,
#        xlim = range(start[, 1], end[, 1]),
#        ylim = range(start[, 2], end[, 2]),
#        zlim = range(start[, 3], end[, 3]),
#        xlab = "Longiture",
#        ylab = "Latitude",
#        zlab = "Altutude"
# )
# bg3d(color = "white")
# for (a in 1:nrow(start)) {
#   arrow3d(matrix(start[a,], ncol = 3),
#           matrix(end  [a,], ncol = 3), type = "rotation", col = "red")
# }





## __ Consistency of means  ----------------------------------------------------
#'
#' \FloatBarrier
#'
#' ## Consistency of means
#'
#+ era5-regrid-N-seas, include=T, echo=F, warning=FALSE, out.width="100%"

p <- ggplot(wind, aes(longitude, latitude, fill = v_N)) +
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
show(p)



afile <- fl_regrid_mon
level <- 1
MM    <- as.numeric(sub("M", "", amont))

## _ Monthly ERA5 regrid  ------------------------------------------------------
#'
#' \FloatBarrier
#' \newpage
#'
#' # Monthly regridded ERA5 data at `r paste0(cnf$D1$LatStep, "x", cnf$D1$LonStep)` for `r ayear`, `r month.name[MM]`
#'
#' **File: `r basename(afile)`**
#'
#' **Level: `r level`**
#' **Month: `r MM`**
#+ include=T, echo=F, warning=FALSE, out.width="100%"


wind     <- ReadNetCDF(afile)
wind     <- wind[pressure_level == level]

cat("Longitude range: [", paste(range(wind$longitude), collapse = ", "), "]")
cat("Latitude range:  [", paste(range(wind$latitude),  collapse = ", "), "]")
cat("Cell N points:   [", paste(c(unique(wind$v_N), unique(wind$u_N)), collapse = ", "), "]")

cat("Latitudes:")
pander(cat(unique(wind$latitude)))

cat("Longitude:")
pander(cat(unique(wind$longitude)))


## __ Mean monthly ERA5 data  --------------------------------------------------

#'
#' \FloatBarrier
#'
#' ## Mean of components
#'
#+ era5-regrid-mean-month, include=T, echo=F, warning=FALSE, out.width="100%", results='asis'

p <- ggplot(wind, aes(longitude, latitude, fill = Mag(u_mean, v_mean))) +
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
show(p)

## __ Median monthly ERA5 data  ------------------------------------------------

#'
#' \FloatBarrier
#'
#' ## Median of components
#'
#+ era5-regrid-median-month, include=T, echo=F, warning=FALSE, out.width="100%"


p <- ggplot(wind, aes(longitude, latitude, fill = Mag(u_median, v_median))) +
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
show(p)




## __ Height monthly ERA5 data  ------------------------------------------------

#'
#' \FloatBarrier
#'
#' ## Height monthly data on level `r level`
#'
#+ era5-regrid-height-month, include=T, echo=F, warning=FALSE, out.width="100%"

p <- ggplot(wind, aes(longitude, latitude, fill = height)) +
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
show(p)

## __ Consistency of means  ----------------------------------------------------
#'
#' \FloatBarrier
#'
#' ## Consistency of means
#'
#+ era5-regrid-N-month, include=T, echo=F, warning=FALSE, out.width="100%"

p <- ggplot(wind, aes(longitude, latitude, fill = v_N)) +
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
show(p)


## Height boundary example ----------------------
#'
#' \FloatBarrier
#'
#' ## Height boundary example
#'
#+ include=T, echo=T, warning=FALSE, out.width="100%"

wind  <- ReadNetCDF(afile)
pp    <- wind[longitude == min(longitude) & latitude == min(latitude)] |> select(starts_with("heig"))

pander(pp)



#' \FloatBarrier
#+ results="asis", echo=FALSE
# goodbye()
