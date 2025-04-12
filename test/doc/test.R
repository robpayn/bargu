graphics.off()
rm(list = ls())

source(file = "../../pkg/R/Project3D.R")

data_path <- "./table_doc_wy2021.csv"
pdf_path <- "./test.pdf"

table <- read.table(
  file = data_path,
  sep = ",",
  header = TRUE,
  colClasses = c(
    rep("character", 3),
    "numeric"
  )
)
ta <- aggregate(
  x = table$doc,
  by = list(table$siteID, table$dateString),
  FUN = mean
)
names(ta) <- c("siteID", "dateString", "doc")
indices <- !grepl(pattern = "a$", x = ta$siteID)
ta <- ta[indices, ]
siteIDs <- unique(ta$siteID)

pdf(file = pdf_path, width = 10, height = 6)

par(mai = c(0.5, 0.5, 0, 0.5))

x <- seq(1, 13, 2)
y <- 1:365
z <- c(0, max(ta$doc, na.rm = TRUE))

x_at <- x
y_at <- pretty(y)
z_at <- pretty(z)

xmin <- min(x_at)
xmax <- max(x_at)
ymin <- min(y_at)
ymax <- max(y_at)
zmin <- min(z_at)
zmax <- max(z_at)

plot.new()

proj <- Project3D$new(
  xlim = c(xmin, xmax),
  ylim = c(ymin, ymax),
  zlim = c(zmin, zmax),
  phi = (12 / 180) * pi,
  theta = (70 / 180) * pi,
  r = 2,
  xexp = 0.6,
  zexp = 0.5,
  scale = TRUE
)

proj$window.xy(boxes = TRUE, box.xvals = xmin, box.yvals = ymax, box.zvals = zmin)

# x axis ####
proj$ticks.x(
  at = x_at,
  ylims = c(ymin, ymin - 0.015 * (ymax - ymin)),
  zlims = c(zmin, zmin - 0.015 * (zmax - zmin))
)
proj$text(
  xvals = x_at,
  yvals = ymin - 0.04 * (ymax - ymin),
  zvals = zmin - 0.04 * (zmax - zmin),
  labels = siteIDs[14 - x_at]
)
proj$text(
  xvals = 0.5 * (xmax - xmin),
  yvals = ymin - 0.08 * (ymax - ymin),
  zvals = zmin - 0.08 * (zmax - zmin),
  labels = "Site",
  srt = -74
)

# y axis ####
proj$ticks.y(
  at = y_at,
  xlims = c(xmax, xmax + 0.02 * (xmax - xmin)),
  zlims = c(zmin, zmin - 0.02 * (zmax - zmin))
)
proj$text(
  xvals = xmax + 0.04 * (xmax - xmin),
  yvals = y_at,
  zvals = zmin - 0.04 * (zmax - zmin),
  labels = y_at
)
proj$text(
  xvals = xmax + 0.09 * (xmax - xmin),
  yvals = 0.5 * (ymax - ymin),
  zvals = zmin - 0.09 * (zmax - zmin),
  labels = "Day of water year",
  srt = 8
)

# z axis ####
proj$ticks.z(
  at = z_at,
  xlims = c(xmax, xmax + 0.015 * (xmax - xmin)),
  ylims = c(ymax, ymax + 0.015 * (ymax - ymin))
)
proj$text(
  xvals = xmax + 0.03 * (xmax - xmin),
  yvals = ymax + 0.03 * (ymax - ymin),
  zvals = z_at,
  labels = z_at
)
proj$text(
  xvals = xmax + 0.06 * (xmax - xmin),
  yvals = ymax + 0.06 * (ymax - ymin),
  zvals = 0.5 * (zmax - zmin),
  labels = bquote("[DOC] (mg C" ~ L^{-1} * ")"),
  srt = 87
)

for (index in length(siteIDs):1) {

  indices <- ta$siteID == siteIDs[index]
  subset <- ta[indices, ]

  doy <- as.POSIXlt(subset$dateString, format = "%Y%m%d")$yday
  doy[doy < 273] <- doy[doy < 273] + 365
  doy <- doy - 273
  mean_doy <- (doy[1:(length(doy) - 1)] + doy[2:length(doy)] ) / 2

  coords <- proj$coords.xy(
    x = rep(14 - index, nrow(subset)),
    y = c(doy),
    z = c(subset$doc)
  )

  segments(
    x0 = coords$x[1:(length(coords$x) - 1)],
    y0 = coords$y[1:(length(coords$y) - 1)],
    x1 = coords$x[2:(length(coords$x))],
    y1 = coords$y[2:(length(coords$y))],
    lwd = 2,
    col = hcl(360 * mean_doy / 365, c = 60, l = 60)
  )

}

proj$box.x(xvals = xmax, lty = "dotted")
proj$box.y(yvals = ymin, lty = "dotted")

invisible(dev.off())


# windows(width = 10, height = 8)
# persp(
#   x = c(xmin, xmin+0.01),
#   xlim = c(xmin, xmax),
#   y = c(ymin, ymin+0.01),
#   ylim = c(ymin, ymax),
#   z = matrix(c(100,100,100,100), nrow = 2, ncol = 2),
#   zlim = c(zmin, zmax),
#   theta = 70,
#   phi = 12,
#   r = 2,
#   ticktype = "detailed",
#   expand = 0.5
# )
