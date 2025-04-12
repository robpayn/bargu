# Dependencies for ROxygen ####
#' @importFrom R6 R6Class

# Common documentation
doco <- c(
  phi = paste0(
    "The altitudinal rotation angle for placing the observer ",
    "in 3D space relative to the centroid of the graphing volume. ",
    "Expected data type is a single-element numeric vector."
  ),
  theta = paste0(
    "The azimuthal rotation angle for placing the observer in 3D space ",
    "relative to the centroid of the graphing volume. ",
    "Expected data type is a single-element numeric vector."
  ),
  r = paste0(
    "The radial distance from the centroid of the graph placing the ",
    "observer in 3D space relative to the centroid of the graphing volume. ",
    "Expected data type is a single-element numeric vector."
  ),
  d = paste0(
    "Parameter controlling the magnitude of the vanishing effect in ",
    "the perspective of the projection. Values greater than one ",
    "diminish the perspective effect and values less than one exaggerate ",
    "the perspective effect. ",
    "Expected data type is a single-element numeric vector."
  ),
  xpd = paste0(
    "Value for the base R graphics parameter xpd.",
    "Expected value is a single-element logical vector.\n\n",
    "Default value is TRUE to be sure the rendered element will not be cropped",
    "to the 2D graphing area by the graphics device."
  )
)

# R6 Class Project3D ####

#' @export
#' @title
#'   Projects 3D graphing elements to a 2D canvas
#' @description
#'   This is an R6 class that allows establishment of a 3D coordinate
#'   system on a 2D canvas. After the class is instantiated and initiated,
#'   simple base R graphics tools can be used to render projections of 3D
#'   graphs.
Project3D <- R6::R6Class(

  classname = "Project3D",

  public = list(

    ## Fields ####

    #' @field lims
    #'   Graphing region limits for each axis in 3D space.
    #'   Expected data type is a three-element numeric vector, where the
    #'   elements represent the limits in the x, y, and z
    #'   directions, respectively.
    lims = NULL,

    #' @field phi
    #'   `r doco["phi"]`
    phi = NULL,

    #' @field theta
    #'   `r doco["theta"]`
    theta = NULL,

    #' @field r
    #'   `r doco["r"]`
    r = NULL,

    #' @field d
    #'   `r doco["d"]`
    d = NULL,

    #' @field expand
    #'   Expansion factors for each axis in 3D space.
    #'   Expected data type is a three-element numeric vector, where the
    #'   elements represent the expansion vectors in the x, y, and z
    #'   directions, respectively.
    expand = NULL,

    #' @field center
    #'   Coordinates of the centroid of the graphing volume in 3D space.
    #'   Expected data type is a three-element numeric vector, where the
    #'   elements represent the coordinates of the centroid in the x, y, and z
    #'   directions, respectively.
    center = NULL,

    #' @field size
    #'   The size of the graphing volume in 3D space.
    #'   Expected data type is a three-element numeric vector, where the
    #'   elements represent the lengths of the volume in the x, y, and z
    #'   directions, respectively.
    size = NULL,

    #' @field vt
    #'   The visual transformation matrix.
    #'   Expected data type is a four by four numeric matrix.
    vt = NULL,

    #' @field vtt
    #'   The transpose of the visual transformation matrix
    #'   Expected data type is a four by four numeric matrix.
    vtt = NULL,

    #' @field lims_2d
    #'   The limits of the graphing volume projected to 2D space.
    #'   Expected data type is a two-element list of two-element numeric
    #'   vectors. The elements of the list correspond to the limits in the
    #'   x and y directions, respectively. The elements of each vector in the
    #'   list correspond to the minimum and maximum values for a given axis,
    #'   respectively.
    lims_2d = NULL,

    ## Methods ####

    #' @description
    #'   Constructs a new object that is an instance of the class.
    #'
    #' @param xlim
    #'   Limits of the graphing volume in the x direction.
    #'   Expected data type is a two-element numeric vector with the
    #'     first element as the minimum and the second element as the
    #'     maximum.
    #' @param ylim
    #'   Limits of the graphing volume in the y direction
    #'   Expected data type is a two-element numeric vector with the
    #'     first element as the minimum and the second element as the
    #'     maximum.
    #' @param zlim
    #'   Limits of the graphing volume in the z direction
    #'   Expected data type is a two-element numeric vector with the
    #'     first element as the minimum and the second element as the
    #'     maximum.
    #' @param phi
    #'   `r doco["phi"]`
    #' @param theta
    #'   `r doco["theta"]`
    #' @param r
    #'   `r doco["r"]`
    #' @param d
    #'   `r doco["d"]`
    #'
    #'   Default value is 1.
    #' @param xexp
    #'   Expansion factor in the x direction.
    #'   Allows adjustment of the relationship of the rendered length
    #'     of the axis to the limits of the scale. A value less than one will
    #'     shorten the axis relative to the default scale and a value greater
    #'     than one will lengthen the axis relative to the default scale.
    #'   Expected type is a single-element numeric vector.
    #'
    #'   Default value is 1.
    #' @param yexp
    #'   Expansion factor in the y direction.
    #'   Allows adjustment of the relationship of the rendered length
    #'     of the axis to the limits of the scale. A value less than one will
    #'     shorten the axis relative to the default scale and a value greater
    #'     than one will lengthen the axis relative to the default scale.
    #'   Expected type is a single-element numeric vector.
    #'
    #'   Default value is 1.
    #' @param zexp
    #'   Expansion factor in the z direction.
    #'   Allows adjustment of the relationship of the rendered length
    #'     of the axis to the limits of the scale. A value less than one will
    #'     shorten the axis relative to the default scale and a value greater
    #'     than one will lengthen the axis relative to the default scale.
    #'   Expected type is a single-element numeric vector.
    #'
    #'   Default value is 1.
    #' @param scale
    #'   Flag indicating if the axes should maintain default lengths
    #'     independent of their scales.
    #'   A value of TRUE indicates axes of common size
    #'     independent of scale, and a value of FALSE indicates
    #'     axes with differing sizes dependent on scales.
    #'   Note that the expansion factor arguments xexp, yexp, and zexp
    #'     provide an independent method of controlling the size of an axis
    #'     relative to its scale.
    #'   Expected data type is a single-element logical vector.
    #'
    #'   Default value is TRUE.
    #' @param mode
    #'   Name of the mode for calculating the visual transformation matrix.
    #'   Expected data type is a single-element character string vector.
    #'
    #'   Default value is "default".
    #' @return
    #'   A reference to the new object as an R6 extension of an environment.
    #'
    initialize = function(
      xlim, ylim, zlim,
      phi, theta, r, d = 1,
      xexp = 1, yexp = 1, zexp = 1,
      scale = TRUE,
      mode = "default"
    )
    {

      self$lims <- vector(mode = "list", length = 3)
      self$lims[[1]] <- xlim
      self$lims[[2]] <- ylim
      self$lims[[3]] <- zlim

      self$phi <- phi
      self$theta <- theta
      self$r <- r
      self$d <- d

      self$expand <- numeric(length = 3)
      self$expand[1] <- xexp
      self$expand[2] <- yexp
      self$expand[3] <- zexp

      self$center <- sapply(
        X = 1:3,
        FUN = function(coord) {
          return(0.5 * (self$lims[[coord]][1] + self$lims[[coord]][2]))
        }
      )

      self$size <- sapply(
        X = 1:3,
        FUN = function(coord) {
          return(0.5 * abs(self$lims[[coord]][2] - self$lims[[coord]][1]))
        }
      )
      if (!scale) {
        smax <- rep(max(self$size), 3)
      }

      if (!is.null(mode) && mode == "default") {
        self$mode.default()
      }

    },

    #' @description
    #'   Creates an identity matrix with all zeros except for ones on the
    #'     diagonal.
    #' @return
    #'   A four by four identity matrix.
    #'   Data type is a four-by-four element numeric matrix.
    #'
    createIdentity = function()
    {
      return(
        matrix(
          data = c(
            1, 0, 0, 0,
            0, 1, 0, 0,
            0, 0, 1, 0,
            0, 0, 0, 1
          ),
          nrow = 4,
          ncol = 4,
          byrow = TRUE
        )
      )
    },

    #' @description
    #'   Transforms the current visual transformation matrix according to the
    #'     provided matrix.
    #' @param m
    #'   A four by four matrix used in the transformation.
    #'   Expected data type is a four-by-four-element numeric matrix.
    #' @return
    #'   The new visual transformation matrix.
    #'   Data type is a four-by-four-element numeric matrix.
    #'
    transform = function(m) {
      self$vt <- m %*% self$vt
      invisible(self$vt)
    },

    #' @description
    #'   Transforms the current visual transformation matrix based on a
    #'     translation in 3D space.
    #' @param coords
    #'   The change in coordinates in 3D space.
    #'   Expected data type is a three-element numeric vector, where the
    #'     elements correspond to translation in the x, y, and z directions,
    #'     respectively.
    #' @return
    #'   The new visual transformation matrix.
    #'   Data type is a four-by-four-element numeric matrix.
    #'
    translate = function(coords) {
      t = self$createIdentity()
      t[1, 4] <- coords[1]
      t[2, 4] <- coords[2]
      t[3, 4] <- coords[3]
      invisible(
        self$transform(m = t)
      )
    },

    #' @description
    #'   Transforms the current visual transformation matrix based on a
    #'     rescaling of the 3D axes.
    #' @param coords
    #'   The scaling factors for the axes in 3D space.
    #'   Expected data type is a three-element numeric vector, where the
    #'     elements correspond to scaling factors for the x, y, and z axes,
    #'     respectively.
    #' @return
    #'   The new visual transformation matrix.
    #'   Data type is a four-by-four-element numeric matrix.
    #'
    scale = function(coords) {
      t = self$createIdentity()
      t[1, 1] <- coords[1]
      t[2, 2] <- coords[2]
      t[3, 3] <- coords[3]
      invisible(
        self$transform(m = t)
      )
    },

    #' @description
    #'   Transforms the current visual transformation matrix based on a
    #'     rotation around the x axis.
    #' @param angle
    #'   The angle for rotating around the x axis.
    #'   Expected data type is a single-element numeric vector.
    #' @return
    #'   The new visual transformation matrix.
    #'   Data type is a four-by-four-element numeric matrix.
    #'
    rotate.x = function(angle)
    {
      t = self$createIdentity()
      c = cos(angle)
      s = sin(angle)
      t[2, 2] <- c
      t[2, 3] <- -s
      t[3, 2] <- s
      t[3, 3] <- c
      invisible(
        self$transform(m = t)
      )
    },

    #' @description
    #'   Transforms the current visual transformation matrix based on a
    #'     rotation around the y axis.
    #' @param angle
    #'   The angle for rotating around the y axis.
    #'   Expected data type is a single-element numeric vector.
    #' @return
    #'   The new visual transformation matrix.
    #'   Data type is a four-by-four-element numeric matrix.
    #'
    rotate.y = function(angle)
    {
      t = self$createIdentity()
      c = cos(angle)
      s = sin(angle)
      t[1, 1] <- c
      t[1, 3] <- s
      t[3, 1] <- -s
      t[3, 3] <- c
      invisible(
        self$transform(m = t)
      )
    },

    #' @description
    #'   Transforms the current visual transformation matrix based on a
    #'     changing the vanishing point perspective.
    #' @param d
    #'   `r doco["d"]`
    #' @return
    #'   The new visual transformation matrix.
    #'   Data type is a four-by-four-element numeric matrix.
    #'
    perspective = function(d)
    {
      t <- self$createIdentity()
      t[4, 3] <- -1 / d
      invisible(
        self$transform(m = t)
      )
    },

    #' @description
    #'   Creates the visual transformation matrix based on the default
    #'     projection strategy.
    #' @return
    #'   The new visual transformation matrix.
    #'   Data type is a four-by-four-element numeric matrix.
    #'
    mode.default = function()
    {

      # Generate the 4x4 visual transformation matrix needed
      #   to project from 3D coordinates to a 2D canvas
      self$vt = self$createIdentity()
      self$translate(coords = -self$center)
      self$scale(
        coords = c(
          self$expand[1] / self$size[1],
          self$expand[2] / self$size[2],
          self$expand[3] / self$size[3]
        )
      )
      self$rotate.x(angle = -pi/2)
      self$rotate.y(angle = -self$theta)
      self$rotate.x(angle = self$phi)
      self$translate(
        coords = c(0, 0, -self$r - self$d)
      )
      self$perspective(d = self$d)
      self$vtt <- t(self$vt)

      # Find the plot region limits on the projected 2D canvas
      #   based on the limits in 3D space
      corners <- self$coords.xy(
        x = c(rep(self$lims[[1]][1], 4), rep(self$lims[[1]][2], 4)),
        y = rep(c(rep(self$lims[[2]][1], 2), rep(self$lims[[2]][2], 2)), 2),
        z = rep(c(self$lims[[3]][1], self$lims[[3]][2]), 4)
      )
      self$lims_2d <- list(
        x = c(min(corners$x), max(corners$x)),
        y = c(min(corners$y), max(corners$y))
      )

      # Return the resulting transformation matrix
      invisible(self$vt)

    },

    #' @description
    #'   Uses base R graphing function plot.window() to create a 2D
    #'     plotting area appropriate for projections of the 3D graphing volume.
    #'   Optionally allows rendering of the bounding boxes defining the 3D
    #'     graphing volume.
    #' @param xlim
    #'   The 2D graphing limits passed to plot.window().
    #'
    #'   Defaults to the field value for lims_2d$x
    #' @param ylim
    #'   The 2D graphing limits passed to plot.window().
    #'
    #'   Defaults to the field value for lims_2d$y
    #' @param boxes
    #'   Flag indicating if bounding boxes should be rendered.
    #'
    #'   Default value is FALSE, which prevents rendering of any bounding
    #'     boxes.
    #' @param box.xvals
    #'   Values on the 3D x axis scale where the bounding
    #'     boxes should be rendered.
    #'   Expected data type is a numerical vector with one or more values.
    #'
    #'   Default values are the minimum and maximum defined in field lims$x.
    #' @param box.xargs
    #'   List of additional arguments to send to the box.x() method.
    #'   Expected data type is a named list where the names represent arguments
    #'     passed to the box.x() method.
    #'
    #'   Default value is an empty list, resulting in no additional arguments
    #'     being passed to box.x().
    #' @param box.yvals
    #'   Values on the 3D y axis scale where the bounding
    #'     boxes should be rendered.
    #'   Expected data type is a numerical vector with one or more values.
    #'
    #'   Default values are the minimum and maximum defined in field lims$y.
    #' @param box.yargs
    #'   List of additional arguments to send to the box.y() method.
    #'   Expected data type is a named list where the names represent arguments
    #'     passed to the box.y() method.
    #'
    #'   Default value is an empty list, resulting in no additional arguments
    #'     being passed to box.y().
    #' @param box.zvals
    #'   Values on the 3D z axis scale where the bounding
    #'     boxes should be rendered.
    #'   Expected data type is a numerical vector with one or more values.
    #'
    #'   Default values are the minimum and maximum defined in field lims$z.
    #' @param box.zargs
    #'   List of additional arguments to send to the box.z() method.
    #'   Expected data type is a named list where the names represent arguments
    #'     passed to the box.z() method.
    #'
    #'   Default value is an empty list, resulting in no additional arguments
    #'     being passed to box.z().
    #' @param ...
    #'   Generic parameters that will be passed to the base R graphics
    #'     plot.window() function used to establish the projected
    #'     2D coordinate system
    #' @return
    #'   Returns a NULL reference.
    #'
    window.xy = function(
      xlim = self$lims_2d$x, ylim = self$lims_2d$y,
      boxes = FALSE,
      box.xvals = self$lims$x, box.xargs = vector(mode = "list"),
      box.yvals = self$lims$y, box.yargs = box.xargs,
      box.zvals = self$lims$z, box.zargs = box.xargs,
      ...
    )
    {

      plot.window(xlim = xlim, ylim = ylim, ...)

      if (boxes) {
        do.call(
          what = self$box.x,
          args = c(
            list(xvals = box.xvals),
            box.xargs
          )
        )
        do.call(
          what = self$box.y,
          args = c(
            list(yvals = box.yvals),
            box.yargs
          )
        )
        do.call(
          what = self$box.z,
          args = c(
            list(zvals = box.zvals),
            box.zargs
          )
        )
      }

      invisible(NULL)

    },

    #' @description
    #'   Calculate the sets of projected 2D coordinates corresponding to
    #'     the provided sets of 3D coordinates
    #' @param x
    #'   3D x coordinates to be projected.
    #'   Expected data type is a numeric vector matching the length of the
    #'     y and z arguments.
    #' @param y
    #'   3D y coordinates to be projected.
    #'   Expected data type is a numeric vector matching the length of the
    #'     x and z arguments.
    #' @param z
    #'   3D z coordinates to be projected.
    #'   Expected data type is a numeric vector matching the length of the
    #'     x and y arguments.
    #' @return
    #'   A list with the projected 2D x and y coordinates.
    #'   Data type is a two-element list, each of which is a numerical vector
    #'     of values for the 2D x and y axes, respectively. The lenght of the
    #'     vectors will match the length of the provided 3D coordinate vectors.
    #'
    coords.xy = function(x, y, z)
    {
      m2d <- cbind(x, y, z, 1, deparse.level = 0L) %*% self$vtt
      return(
        list(x = m2d[, 1]/m2d[, 4], y = m2d[, 2]/m2d[, 4])
      )
    },

    #' @description
    #'   Draws boxes normal to the 3D x axis extending to the y and z limits.
    #' @param xvals
    #'   Values on the x axis at which boxes should be drawn.
    #'   Expected data type is a numeric vector.
    #' @param xpd
    #'   `r doco["xpd"]`
    #' @param ...
    #'   Generic parameters that will be passed to the base R graphics lines()
    #'     function used to render the boxes
    #' @return
    #'   Returns a NULL reference.
    #'
    box.x = function(xvals, xpd = TRUE, ...)
    {
      for (xval in xvals) {
        lines(
          x = self$coords.xy(
            x = rep(xval, 5),
            y = c(
              self$lims[[2]][1],
              rep(self$lims[[2]][2], 2),
              rep(self$lims[[2]][1], 2)
            ),
            z = c(
              rep(self$lims[[3]][1], 2),
              rep(self$lims[[3]][2], 2),
              self$lims[[3]][1]
            )
          ),
          xpd = xpd,
          ...
        )
      }
      invisible(NULL)
    },

    #' @description
    #'   Draws boxes normal to the 3D y axis extending to the x and z limits.
    #' @param yvals
    #'   Values on the y axis at which boxes should be drawn.
    #'   Expected data type is a numeric vector.
    #' @param xpd
    #'   `r doco["xpd"]`
    #' @param ...
    #'   Generic parameters that will be passed to the base R graphics lines()
    #'     function used to render the boxes
    #' @return
    #'   Returns a NULL reference.
    #'
    box.y = function(yvals, xpd = TRUE, ...)
    {
      for (yval in yvals) {
        lines(
          x = self$coords.xy(
            x = c(
              self$lims[[1]][1],
              rep(self$lims[[1]][2], 2),
              rep(self$lims[[1]][1], 2)
            ),
            y = rep(yval, 5),
            z = c(
              rep(self$lims[[3]][1], 2),
              rep(self$lims[[3]][2], 2),
              self$lims[[3]][1]
            )
          ),
          xpd = xpd,
          ...
        )
      }
      invisible(NULL)
    },

    #' @description
    #'   Draws boxes normal to the 3D z axis extending to the x and y limits.
    #' @param zvals
    #'   Values on the z axis at which boxes should be drawn.
    #'   Expected data type is a numeric vector.
    #' @param xpd
    #'   `r doco["xpd"]`
    #' @param ...
    #'   Generic parameters that will be passed to the base R graphics lines()
    #'     function used to render the boxes
    #' @return
    #'   Returns a NULL reference.
    #'
    box.z = function(zvals, xpd = TRUE, ...)
    {
      for (zval in zvals) {
        lines(
          x = self$coords.xy(
            x = c(
              self$lims[[1]][1],
              rep(self$lims[[1]][2], 2),
              rep(self$lims[[1]][1], 2)
            ),
            y = c(
              rep(self$lims[[2]][1], 2),
              rep(self$lims[[2]][2], 2),
              self$lims[[2]][1]
            ),
            z = rep(zval, 5)
          ),
          xpd = xpd,
          ...
        )
      }
      invisible(NULL)
    },

    #' @description
    #'   Adds text to a graph at locations defined in 3D space.
    #' @param xvals
    #'   Coordinates for text on the x axis in 3D space.
    #'   Expected data type is a numeric vector of the same length as the
    #'     y and z arguments.
    #' @param yvals
    #'   Coordinates for text on the y axis in 3D space.
    #'   Expected data type is a numeric vector of the same length as the
    #'     x and z arguments.
    #' @param zvals
    #'   Coordinates for text on the z axis in 3D space.
    #'   Expected data type is a numeric vector of the same length as the
    #'     x and y arguments.
    #' @param labels
    #'   Vector of text elements to be rendered on the graph.
    #'   Expected data type is a character string vector of the same length as
    #'     the x, y, and z arguments.
    #' @param xpd
    #'   `r doco["xpd"]`
    #' @param ...
    #'   Generic parameters that will be passed to the base R graphics text()
    #'     function used to render the text.
    #' @return
    #'   Returns a NULL reference.
    #'
    text = function(xvals, yvals, zvals, labels, xpd = TRUE, ...)
    {
      text(
        x = self$coords.xy(x = xvals, y = yvals, z = zvals),
        labels = labels,
        xpd = xpd,
        ...
      )
      invisible(NULL)
    },

    #' @description
    #'   Draw tick mars along a line defined in the x direction
    #'
    #' @param at
    #'   Locations on the x axis for the tick marks.
    #'   Expected data type is a numeric vector.
    #' @param ylims
    #'   A vector of the minimum and maximum extent of the tick marks in the
    #'     y direction.
    #'   Expected data type is a two-element numeric vector, where the first
    #'     element is the minimum value and the second element is the
    #'     maximum value.
    #' @param zlims
    #'   A vector of the minimum and maximum extent of the tick marks in the
    #'     z direction.
    #'   Expected data type is a two-element numeric vector, where the first
    #'     element is the minimum value and the second element is the
    #'     maximum value.
    #' @param xpd
    #'   `r doco["xpd"]`
    #' @param ...
    #'   Generic parameters that will be passed to the base R graphics
    #'     segments() functions used to render the tick marks.
    #' @return
    #'   Returns a NULL reference.
    #'
    ticks.x = function(at, ylims, zlims, xpd = TRUE, ...)
    {
      xy0_tick <- self$coords.xy(x = at, y = ylims[1], z = zlims[1])
      xy1_tick <- self$coords.xy(x = at, y = ylims[2], z = zlims[2])
      segments(
        x0 = xy0_tick$x,
        y0 = xy0_tick$y,
        x1 = xy1_tick$x,
        y1 = xy1_tick$y,
        xpd = xpd,
        ...
      )
      invisible(NULL)
    },

    #' @description
    #'   Draw tick mars along a line defined in the y direction
    #'
    #' @param at
    #'   Locations on the y axis for the tick marks.
    #'   Expected data type is a numeric vector.
    #' @param xlims
    #'   A vector of the minimum and maximum extent of the tick marks in the
    #'     x direction.
    #'   Expected data type is a two-element numeric vector, where the first
    #'     element is the minimum value and the second element is the
    #'     maximum value.
    #' @param zlims
    #'   A vector of the minimum and maximum extent of the tick marks in the
    #'     z direction.
    #'   Expected data type is a two-element numeric vector, where the first
    #'     element is the minimum value and the second element is the
    #'     maximum value.
    #' @param xpd
    #'   `r doco["xpd"]`
    #' @param ...
    #'   Generic parameters that will be passed to the base R graphics
    #'     segments() functions used to render the tick marks.
    #' @return
    #'   Returns a NULL reference.
    #'
    ticks.y = function(at, xlims, zlims, xpd = TRUE, ...)
    {
      xy0_tick <- self$coords.xy(x = xlims[1], y = at, z = zlims[1])
      xy1_tick <- self$coords.xy(x = xlims[2], y = at, z = zlims[2])
      segments(
        x0 = xy0_tick$x,
        y0 = xy0_tick$y,
        x1 = xy1_tick$x,
        y1 = xy1_tick$y,
        xpd = xpd,
        ...
      )
      invisible(NULL)
    },

    #' @description
    #'   Draw tick mars along a line defined in the z direction
    #'
    #' @param at
    #'   Locations on the z axis for the tick marks.
    #'   Expected data type is a numeric vector.
    #' @param xlims
    #'   A vector of the minimum and maximum extent of the tick marks in the
    #'     x direction.
    #'   Expected data type is a two-element numeric vector, where the first
    #'     element is the minimum value and the second element is the
    #'     maximum value.
    #' @param ylims
    #'   A vector of the minimum and maximum extent of the tick marks in the
    #'     y direction.
    #'   Expected data type is a two-element numeric vector, where the first
    #'     element is the minimum value and the second element is the
    #'     maximum value.
    #' @param xpd
    #'   `r doco["xpd"]`
    #' @param ...
    #'   Generic parameters that will be passed to the base R graphics
    #'     segments() functions used to render the tick marks.
    #' @return
    #'   Returns a NULL reference.
    #'
    ticks.z = function(at, xlims, ylims, xpd = TRUE, ...)
    {
      xy0_tick <- self$coords.xy(x = xlims[1], y = ylims[1], z = at)
      xy1_tick <- self$coords.xy(x = xlims[2], y = ylims[2], z = at)
      segments(
        x0 = xy0_tick$x,
        y0 = xy0_tick$y,
        x1 = xy1_tick$x,
        y1 = xy1_tick$y,
        xpd = xpd,
        ...
      )
      invisible(NULL)
    }

  )

)
