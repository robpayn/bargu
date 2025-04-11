Project3D <- R6::R6Class(
  
  classname = "Project3D",
  
  public = list(
    
    lims = NULL,
    phi = NULL,
    theta = NULL,
    r = NULL,
    d = NULL,
    expand = NULL,
    center = NULL,
    size = NULL,
    vt = NULL,
    vtt = NULL,
    lims_2d = NULL,
    
    initialize = function(
      xlim, ylim, zlim, 
      phi, theta, r, d = 1, 
      xexp = 1, yexp = 1, zexp = 1, 
      scale = TRUE,
      mode = "normal"
    )
    {
    
      coords <- c("x", "y", "z")
      self$lims <- vector(mode = "list", length = 3)
      names(self$lims) <- coords
      
      self$lims[["x"]] <- xlim
      self$lims[["y"]] <- ylim
      self$lims[["z"]] <- zlim
      
      self$phi <- phi
      self$theta <- theta
      self$r <- r
      self$d <- d
      
      self$expand <- numeric(length = 3)
      names(self$expand) <- coords
      self$expand["x"] <- xexp
      self$expand["y"] <- yexp
      self$expand["z"] <- zexp
      
      self$center <- vector(mode = "numeric", length = 3)
      self$size <- vector(mode = "numeric", length = 3)
      names(self$center) <- coords
      names(self$size) <- coords
      for (coord in coords) {
        self$center[coord] <- 
          0.5 * 
          (self$lims[[coord]][1] + self$lims[[coord]][2])
        self$size[coord] <- 
          0.5 * 
          abs(self$lims[[coord]][2] - self$lims[[coord]][1])
      }
      
      if (!scale) {
        smax <- max(self$size)
        for (index in 1:3) {
          self$size[index] <- smax
        }
      }
      
      if (!is.null(mode) && mode == "normal") {
        self$mode.normal()
      }
      
    },
    
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
    
    transform = function(m) {
      self$vt <- m %*% self$vt
      invisible(self$vt)
    },
    
    translate = function(coords) {
      t = self$createIdentity()
      t[1, 4] <- coords[1]
      t[2, 4] <- coords[2]
      t[3, 4] <- coords[3]
      invisible(
        self$transform(m = t)
      )
    },
    
    scale = function(coords) {
      t = self$createIdentity()
      t[1, 1] <- coords[1]
      t[2, 2] <- coords[2]
      t[3, 3] <- coords[3]
      invisible(
        self$transform(m = t)
      )
    },
    
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
    
    perspective = function(d)
    {
      t <- self$createIdentity()
      t[4, 3] <- -1 / d
      invisible(
        self$transform(m = t)
      )
    },
    
    mode.normal = function()
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
    
    window.xy = function(
      xlim = self$lims_2d$x, ylim = self$lims_2d$y,
      boxes = FALSE,
      box.x = self$lims$x,
      box.x.args = vector(mode = "list"),
      box.y = self$lims$y,
      box.y.args = box.x.args,
      box.z = self$lims$z,
      box.z.args = box.x.args,
      ...
    )
    {
      
      plot.window(xlim = xlim, ylim = ylim, ...)
      
      if (boxes) {
        do.call(
          what = self$box.x,
          args = c(
            list(xvals = box.x),
            box.x.args
          )
        )
        do.call(
          what = self$box.y,
          args = c(
            list(yvals = box.y),
            box.y.args
          )
        )
        do.call(
          what = self$box.z,
          args = c(
            list(zvals = box.z),
            box.z.args
          )
        )
      }
      
      invisible(NULL)
      
    },
    
    coords.xy = function(x, y, z)
    {
      m2d <- cbind(x, y, z, 1, deparse.level = 0L) %*% self$vtt
      return(
        list(x = m2d[, 1]/m2d[, 4], y = m2d[, 2]/m2d[, 4])
      )
    },
    
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
    
    text.x = function(at, yvals, zvals, labels = at, xpd = TRUE, ...)
    {
      text(
        x = self$coords.xy(x = at, y = yvals, z = zvals),
        labels = labels,
        xpd = xpd,
        ...
      )
      invisible(NULL)
    },
    
    text.y = function(at, xvals, zvals, labels = at, xpd = TRUE, ...)
    {
      text(
        x = self$coords.xy(x = xvals, y = at, z = zvals),
        labels = labels,
        xpd = xpd,
        ...
      )
      invisible(NULL)
    },
    
    text.z = function(at, xvals, yvals, labels = at, xpd = TRUE, ...)
    {
      text(
        x = self$coords.xy(x = xvals, y = yvals, z = at),
        labels = labels,
        xpd = xpd,
        ...
      )
      invisible(NULL)
    },
    
    ticks.x = function(at, yvals, zvals, xpd = TRUE, ...) 
    {
      xy0_tick <- self$coords.xy(x = at, y = yvals[1], z = zvals[1])
      xy1_tick <- self$coords.xy(x = at, y = yvals[2], z = zvals[2])
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

    ticks.y = function(at, xvals, zvals, xpd = TRUE, ...) 
    {
      xy0_tick <- self$coords.xy(x = xvals[1], y = at, z = zvals[1])
      xy1_tick <- self$coords.xy(x = xvals[2], y = at, z = zvals[2])
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
    
    ticks.z = function(at, xvals, yvals, xpd = TRUE, ...) 
    {
      xy0_tick <- self$coords.xy(x = xvals[1], y = yvals[1], z = at)
      xy1_tick <- self$coords.xy(x = xvals[2], y = yvals[2], z = at)
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