#' @title Cell constructor
#'
#' @description Constructs a 4-sided polygon at a given \eqn{x} and \eqn{y}
#'   coordinates.
#'
#' @param x Cell position on coordinate \eqn{x}.
#' @param y Cell position on coordinate \eqn{y}.
#' @param cell_width Overall width of cell to be drawn.
#' @param walls Boolean vector to indicate whether wall of cell should be
#'   drawn.
#'
#' @details \code{walls} will need to be a boolean vector. To import this
#'   parameter, the order is \code{bottom, left, top, and right}. If element
#'   is set to \code{FALSE}, no wall will be drawn.
#'
#' @importFrom graphics segments
cell <- function(x,
                 y,
                 cell_width = 1,
                 walls = c(TRUE, TRUE, TRUE, TRUE),
                 cell_color = "lightblue",
                 visited = FALSE) {

    ## Vist check
    if (visited) {
        cell_color <- "pink"
    }

    ## Polygon with color
    graphics::polygon(
        x = c(x, x, x + cell_width, x + cell_width),
        y = c(y, y + cell_width, y + cell_width, y),
        border = NA,
        col = cell_color
    )

    ## Bottom walls
    if (walls[1]) {
        graphics::segments(
            x0 = x,
            y0 = y,
            x1 = x + cell_width,
            y1 = y
        )
    }

    ## Left walls
    if (walls[2]) {
        graphics::segments(
            x0 = x,
            y0 = y + cell_width,
            x1 = x,
            y1 = y
        )
    }

    ## Top walls
    if (walls[3]) {
        graphics::segments(
            x0 = x,
            y0 = y + cell_width,
            x1 = x + cell_width,
            y1 = y + cell_width
        )
    }

    ## Right walls
    if (walls[4]) {
        graphics::segments(
            x0 = x + cell_width,
            y0 = y + cell_width,
            x1 = x + cell_width,
            y1 = y
        )
    }
}



#' @title Canvas generator
#'
#' @description Generates the overall canvas with respective dimensions for
#'   maze generation.
#'
#' @param x_max Maximum canvas width of maze for \eqn{x}-axis.
#' @param y_max Maximum canvas width of maze for \eqn{y}-axis.
#' @param col_width Size of columns for canvas. Rounds to nearest whole number.
#' @param border_pad Uniform margin width for canvas.
#'
#' @importFrom graphics par
#' @importFrom graphics plot.new
#' @importFrom graphics plot.window
canvas <- function(x_max = 10, y_max = 10, col_width = 1, border_pad = 1) {

    ## Set initial canvas coordinate
    canvas_start <- 1

    ## Set margins
    par(mar = rep(border_pad, 4))

    ## Get row and column dimensions
    cols <- floor(x_max / col_width)
    rows <- floor(y_max / col_width)

    ## Start new plot window with given dimensions
    plot.new()
    plot.window(
        xlim = c(canvas_start, cols + border_pad),
        ylim = c(canvas_start, rows + border_pad)
    )
}
