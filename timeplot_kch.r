timePlot_kch <- function (mydata1, mydata2, pollutant = "nox", group = FALSE, stack = FALSE, 
          normalise = NULL, avg.time = "default", data.thresh = 0, 
          statistic = "mean", percentile = NA, date.pad = FALSE, type = "default", 
          cols = "brewer1", plot.type = "l", key = TRUE, log = FALSE, 
          windflow = NULL, smooth = FALSE, ci = TRUE, y.relation = "same", 
          ref.x = NULL, ref.y = NULL, key.columns = 1, key.position = "bottom", 
          name.pol = pollutant, date.breaks = 7, date.format = NULL, 
          auto.text = TRUE, ...) 
{
  variable <- year <- NULL
  if (log) 
    nlog <- 10
  else nlog <- FALSE
  vars <- c("date", pollutant)
  if (length(cols) == 1 && cols == "greyscale") {
    trellis.par.set(list(strip.background = list(col = "white")))
  }
  current.strip <- trellis.par.get("strip.background")
  current.font <- trellis.par.get("fontsize")
  on.exit(trellis.par.set(fontsize = current.font))
  Args <- list(...)
  Args$xlab <- if ("xlab" %in% names(Args)) {
    quickText(Args$xlab, auto.text)
  }
  else {
    quickText("", auto.text)
  }
  Args$ylab <- if ("ylab" %in% names(Args)) {
    quickText(Args$ylab, auto.text)
  }
  else {
    NULL
  }
  if ("main" %in% names(Args)) {
    if (!is.list(Args$main)) 
      Args$main <- quickText(Args$main, auto.text)
  }
  if ("fontsize" %in% names(Args)) {
    trellis.par.set(fontsize = list(text = Args$fontsize))
  }
  xlim <- if ("xlim" %in% names(Args)) {
    Args$xlim
  }
  else {
    NULL
  }
  if (!"pch" %in% names(Args)) {
    Args$pch <- NA
  }
  if (!"lwd" %in% names(Args)) {
    Args$lwd <- 1
  }
  if (!"lty" %in% names(Args)) {
    Args$lty <- NULL
  }
  if (!"layout" %in% names(Args)) {
    Args$layout <- NULL
  }
  strip <- if ("strip" %in% names(Args)) {
    Args$strip
  }
  else {
    TRUE
  }
  len.all <- length(mydata1$date)
  len.unique <- length(unique(mydata1$date))
  if (type == "default" & "site" %in% names(mydata1) & len.all != 
      len.unique) {
    if (length(unique(factor(mydata1$site))) > 1) 
      stop("More than one site has been detected: choose type = 'site' and pollutant(s)")
  }
  if (length(percentile) > 1 & length(pollutant) > 1) {
    stop("Only one pollutant allowed when considering more than one percentile")
  }
  if (!missing(statistic) & missing(avg.time)) {
    message("No averaging time applied, using avg.time ='month'")
    avg.time <- "month"
  }
  if (!is.null(windflow)) {
    vars <- unique(c(vars, "wd", "ws"))
  }
  mydata1 <- checkPrep(mydata1, vars, type, remove.calm = FALSE)
  theStrip <- strip
  if (date.pad) 
    mydata1 <- date.pad(mydata1, type = type)
  mydata1 <- cutData(mydata1, type, ...)
  if (avg.time != "default") {
    if (length(percentile) > 1) {
      mydata1 <- group_by(mydata1, UQS(syms(type))) %>% do(calcPercentile(., 
                                                                        pollutant = pollutant, avg.time = avg.time, data.thresh = data.thresh, 
                                                                        percentile = percentile))
      pollutant <- paste("percentile.", percentile, sep = "")
      if (missing(group)) 
        group <- TRUE
    }
    else {
      mydata1 <- timeAverage(mydata1, pollutant = pollutant, 
                            type = type, statistic = statistic, avg.time = avg.time, 
                            data.thresh = data.thresh, percentile = percentile)
    }
  }
  if (type == "default") 
    mydata1$default <- "default"
  if (!is.null(windflow)) {
    mydata1 <- gather(mydata1, key = variable, value = value, 
                     UQS(syms(pollutant)))
  }
  else {
    mydata1 <- gather(mydata1, key = variable, value = value, 
                     UQS(syms(pollutant)))
  }
  if (type != "default") 
    group <- TRUE
  npol <- length(unique(mydata1$variable))
  if (is.null(Args$layout) & !group & !stack) 
    Args$layout <- c(1, npol)
  divide.by.mean <- function(x) {
    Mean <- mean(x$value, na.rm = TRUE)
    x$value <- x$value/Mean
    x
  }
  norm.by.date <- function(x, thedate) {
    temp <- na.omit(x)
    id <- which(abs(temp$date - thedate) == min(abs(temp$date - 
                                                      thedate)))
    id <- temp$date[id]
    x$value <- 100 * x$value/x$value[x$date == id]
    x
  }
  if (!missing(normalise)) {
    mydata1 <- mutate(mydata1, variable = factor(variable, 
                                               levels = unique(variable)))
    if (is.null(Args$ylab)) {
      Args$ylab <- "normalised level"
    }
    if (normalise == "mean") {
      mydata1 <- group_by(mydata1, variable) %>% do(divide.by.mean(.))
    }
    else {
      thedate <- as.POSIXct(strptime(normalise, format = "%d/%m/%Y", 
                                     tz = "GMT"))
      mydata1 <- group_by(mydata1, variable) %>% do(norm.by.date(., 
                                                               thedate = thedate))
    }
  }
  if (is.null(Args$ylab)) {
    Args$ylab <- quickText(paste(pollutant, collapse = ", "), 
                           auto.text)
  }
  mylab <- sapply(seq_along(pollutant), function(x) quickText(pollutant[x], 
                                                              auto.text))
  if (!missing(name.pol)) {
    mylab <- sapply(seq_along(name.pol), function(x) quickText(name.pol[x], 
                                                               auto.text))
  }
  myColors <- if (length(cols) == 1 && cols == "greyscale") {
    openColours(cols, npol + 1)[-1]
  }
  else {
    openColours(cols, npol)
  }
  myform <- formula(paste("value ~ date |", type))
  if (is.null(Args$strip)) {
    strip <- TRUE
  }
  strip.left <- FALSE
  dates <- dateBreaks(mydata1$date, date.breaks)$major
  if (is.null(date.format)) {
    formats <- dateBreaks(mydata1$date, date.breaks)$format
  }
  else {
    formats <- date.format
  }
  scales <- list(x = list(at = dates, format = formats), y = list(log = nlog, 
                                                                  relation = y.relation, rot = 0))
  if (!group) {
    if (is.null(Args$strip)) {
      strip <- FALSE
    }
    myform <- formula("value ~ date | variable")
    if (npol == 1) {
      strip.left <- FALSE
    }
    else {
      strip.left <- strip.custom(par.strip.text = list(cex = 0.9), 
                                 horizontal = FALSE, factor.levels = mylab)
    }
    scales <- list(x = list(at = dates, format = formats), 
                   y = list(relation = y.relation, rot = 0, log = nlog))
    if (is.null(Args$lty)) 
      Args$lty <- 1
  }
  if (is.null(Args$lty)) {
    Args$lty <- 1:length(pollutant)
  }
  if (type == "default") 
    strip <- FALSE
  if (stack) {
    mydata1$year <- year(mydata1$date)
    if (is.null(Args$layout)) {
      Args$layout <- c(1, length(unique(mydata1$year)))
    }
    strip <- FALSE
    myform <- formula("value ~ date | year")
    strip.left <- strip.custom(par.strip.text = list(cex = 0.9), 
                               horizontal = FALSE)
    dates <- as.POSIXct(unique(paste(format(mydata1$date, 
                                            "%Y-%m"), "-01", sep = "")), "GMT")
    scales <- list(x = list(at = dates, format = "%d-%b", 
                            relation = "sliced"), y = list(log = nlog))
    xlim <- lapply(split(mydata1, mydata1["year"]), function(x) range(x$date))
  }
  if (missing(key.columns)) 
    key.columns <- npol
  if (key) {
    if (any(!is.na(Args$pch))) {
      key <- list(lines = list(col = myColors[1:npol], 
                               lty = Args$lty, lwd = Args$lwd), points = list(pch = Args$pch, 
                                                                              col = myColors[1:npol]), text = list(lab = mylab), 
                  space = key.position, columns = key.columns)
    }
    else {
      key <- list(lines = list(col = myColors[1:npol], 
                               lty = Args$lty, lwd = Args$lwd), text = list(lab = mylab), 
                  space = key.position, columns = key.columns)
    }
  }
  else {
    key <- NULL
  }
  if (theStrip) {
    strip <- strip
    strip.left <- strip.left
  }
  else {
    strip <- FALSE
    strip.left <- FALSE
  }
  if (length(type) == 1 & type[1] == "wd" & is.null(Args$layout)) {
    wds <- c("NW", "N", "NE", "W", "E", "SW", "S", "SE")
    mydata1$wd <- ordered(mydata1$wd, levels = wds)
    wd.ok <- sapply(wds, function(x) {
      if (x %in% unique(mydata1$wd)) 
        FALSE
      else TRUE
    })
    skip <- c(wd.ok[1:4], TRUE, wd.ok[5:8])
    mydata1$wd <- factor(mydata1$wd)
    Args$layout <- c(3, 3)
    if (!"skip" %in% names(Args)) {
      Args$skip <- skip
    }
  }
  if (!"skip" %in% names(Args)) {
    Args$skip <- FALSE
  }
  gap <- difftime(max(mydata1$date), min(mydata1$date), units = "secs")/80
  if (is.null(xlim)) 
    xlim <- range(mydata1$date) + c(-1 * gap, gap)
  mydata1$variable <- factor(mydata1$variable, levels = pollutant)
  xyplot.args1 <- list(x = myform, data = mydata1, groups = mydata1$variable, 
                      as.table = TRUE, par.strip.text = list(cex = 0.8), scales = scales, 
                      key = key, xlim = xlim, strip = strip, strip.left = strip.left, 
                      windflow = windflow, yscale.components = yscale.components.log10ticks, 
                      panel = panel.superpose, panel.groups = function(x, y, 
                                                                       col.line, col.symbol, col, col.se, type, group.number, 
                                                                       lty, lwd, pch, subscripts, windflow, ...) {
                        if (group.number == 1) {
                          panel.grid(-1, 0)
                          panel.abline(v = dates, col = "grey90")
                        }
                        if (!group & !stack) {
                          panel.abline(v = dates, col = "grey90")
                          panel.grid(-1, 0)
                        }
                        panel.xyplot(x, y, type = plot.type, lty = lty, lwd = lwd, 
                                     pch = pch, col.line = myColors[group.number], 
                                     col.symbol = myColors[group.number], ...)
                        if (any(!is.na(Args$pch))) {
                          lpoints(x, y, type = "p", pch = Args$pch[group.number], 
                                  col.symbol = myColors[group.number], ...)
                        }
                        if (!is.null(windflow)) {
                          list1 <- list(x, y, dat = mydata1, subscripts)
                          list2 <- windflow
                          flow.args <- listUpdate(list1, list2)
                          do.call(panel.windflow, flow.args)
                        }
                        if (smooth) {
                          panel.gam(x, y, col = myColors[group.number], 
                                    col.se = myColors[group.number], lty = 1, lwd = 1, 
                                    se = ci, k = NULL, ...)
                        }
                        if (!is.null(ref.x)) do.call(panel.abline, ref.x)
                        if (!is.null(ref.y)) do.call(panel.abline, ref.y)
                      })
  mydata2$variable <- factor(mydata2$variable, levels = pollutant)
  xyplot.args2 <- list(x = myform, data = mydata2, groups = mydata2$variable, 
                      as.table = TRUE, par.strip.text = list(cex = 0.8), scales = scales, 
                      key = key, xlim = xlim, strip = strip, strip.left = strip.left, 
                      windflow = windflow, yscale.components = yscale.components.log10ticks, 
                      panel = panel.superpose, panel.groups = function(x, y, 
                                                                       col.line, col.symbol, col, col.se, type, group.number, 
                                                                       lty, lwd, pch, subscripts, windflow, ...) {
                        if (group.number == 1) {
                          panel.grid(-1, 0)
                          panel.abline(v = dates, col = "grey90")
                        }
                        if (!group & !stack) {
                          panel.abline(v = dates, col = "grey90")
                          panel.grid(-1, 0)
                        }
                        panel.xyplot(x, y, type = plot.type, lty = lty, lwd = lwd, 
                                     pch = pch, col.line = myColors[group.number], 
                                     col.symbol = myColors[group.number], ...)
                        if (any(!is.na(Args$pch))) {
                          lpoints(x, y, type = "p", pch = Args$pch[group.number], 
                                  col.symbol = myColors[group.number], ...)
                        }
                        if (!is.null(windflow)) {
                          list1 <- list(x, y, dat = mydata1, subscripts)
                          list2 <- windflow
                          flow.args <- listUpdate(list1, list2)
                          do.call(panel.windflow, flow.args)
                        }
                        if (smooth) {
                          panel.gam(x, y, col = myColors[group.number], 
                                    col.se = myColors[group.number], lty = 1, lwd = 1, 
                                    se = ci, k = NULL, ...)
                        }
                        if (!is.null(ref.x)) do.call(panel.abline, ref.x)
                        if (!is.null(ref.y)) do.call(panel.abline, ref.y)
                      })
  xyplot.args <- listUpdate(xyplot.args1, Args)
  plt <- do.call(xyplot, xyplot.args)
  xyplot.args <- listUpdate(xyplot.args2, Args)
  plt <- do.call(xyplot, xyplot.args)
  plot(plt)
  newdata <- mydata1
  output <- list(plot = plt, data = newdata, call = match.call())
  class(output) <- "openair"
  invisible(output)
}
  