#' @title Path finder for mazes
#'
#' @description An example algorithm for random maze generation.
#'
#' @param x_max Maximum canvas width of maze for \eqn{x}-axis.
#' @param y_max Maximum canvas width of maze for \eqn{y}-axis.
#' @param col_width Size of columns for canvas. Rounds to nearest whole number.
#' @param border_pad Uniform margin width for canvas.
#'
#' @importFrom graphics par
#' @importFrom graphics plot.new
#' @importFrom graphics plot.window
#' @importFrom graphics segments
#'
#' @export
pathr <- function(x_max = 15,
                  y_max = 15,
                  col_width = 1,
                  border_pad = 1,
                  walls = c(TRUE, TRUE, TRUE, TRUE)) {

    ## Generate canvas
    canvas(
        x_max = x_max,
        y_max = y_max,
        col_width = col_width,
        border_pad = border_pad
    )

    ## Get column and row data
    cols <- floor(x_max / col_width)
    rows <- floor(y_max / col_width)

    ## Iterate through cell functions (optimize later)
    for (i in seq_len(rows)) {
        for (j in seq_len(cols)) {
            if (i == 1 & j == 1) {
                cell(j, i, col_width, walls = walls, visited = TRUE)
            } else {
                cell(j, i, col_width, walls = walls)
            }
        }
    }

    ## Debug
    cat(
        "--- pathr DEBUG ---",
        "\nNumber of cols:", cols,
        "\nNumber of rows:", rows,
        "\n",
        "\n--- wall parameters ---",
        "\n[1] Bottom:", walls[1],
        "\n[2] Left  :", walls[2],
        "\n[3] Top   :", walls[3],
        "\n[4] Right :", walls[4]
    )
}
