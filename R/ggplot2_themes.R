#' A Clean Theme with Moffitt Style
#'
#' A clean theme based on [hrbrthemes::theme_ispum()] that uses
#' [Overpass](https://fonts.google.com/specimen/Overpass), a free Google Font
#' that resembles Freeway, the (proprietary) font chosen by MCC branding. This
#' theme works best when set globally using [ggplot2::theme_set()] as it will
#' automatically download and register the correct fonts from Google Font using
#' [sysfonts] and [showtext] (if installed) and will change the default
#' [ggplot2::geom_text()] related fonts as well. For an alternative look that I
#' prefer, use `theme_grk()`, which simply uses "Fira Sans" as the base font.
#'
#' @examples
#' \dontrun{
#' theme_set(theme_moffitt())
#' theme_set(theme_grk())
#' # Set base theme without changing geom defaults
#' theme_set(theme_moffitt(default_geom_font = NULL, default_geom_color = NULL))
#' }
#'
#' library(ggplot2)
#' g <- ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point() +
#'   geom_text(aes(label = cyl), vjust = 0.5, hjust = 1.65) +
#'   labs(x = "Fuel efficiency (mpg)",
#'        y = "Weight (tons)",
#'        title = "Seminal ggplot2 scatterplot example",
#'        subtitle = "A plot that is only useful for demonstration purposes",
#'        caption = "Brought to you by the letter 'g'")
#' g + theme_moffitt()
#' g + theme_moffitt(default_geom_color = moffitt_colors$green)
#' g + theme_grk()
#'
#' @inheritParams hrbrthemes::theme_ipsum
#' @inheritDotParams hrbrthemes::theme_ipsum
#' @param default_color Changes default colors of bars and points to
#'   the value given. Set to `NULL` to avoid changing these colors. Note that
#'   these colors are used only when color or fill is not mapped to the data.
#' @param default_geom_font Change the default ggplot2 geom fonts to the
#'   specified font. The default is "Fira Sans Condensed", which tends to look
#'   good in constrained space.
#' @param axis_text_family The font family for axis ticks text labels.
#'   Passed to `family` in `axis.text` in [ggplot2::theme()].
#' @param axis_title_bold If `TRUE`, the axis title's will be bold.
#' @param axis_text_color Color of axis text
#' @param plot_caption_color Color of the plot caption text, or `NULL` to disable
#' @param panel_border_color Color of the panel border, or `NULL` to disable
#' @param use_showtext Should [showtext] and [sysfonts] be used to register
#'   font families from Google? Default is `TRUE`.
#' @param panel_grid One of "major", "minor", "both", or "none"
#' @export
theme_moffitt <- function(
  base_family        = "Overpass",
  axis_text_family   = "Overpass Mono",
  axis_title_family  = base_family,
  axis_title_bold    = FALSE,
  axis_title_just    = "cc",
  axis_title_size    = 13,
  subtitle_size      = 13,
  default_geom_font  = "Fira Sans Condensed",
  default_geom_color = grkmisc::moffitt_colors$blue,
  axis_text_color    = "#6e6e6e",
  plot_caption_color = axis_text_color,
  panel_border_color = axis_text_color,
  panel_background_color = "#FFFFFF",
  ...,
  use_showtext = TRUE,
  panel_grid = c("major", "minor", "both", "none")
) {
  theme_grk(
    base_family        = base_family,
    axis_text_family   = axis_text_family,
    axis_title_family  = axis_title_family,
    axis_title_bold    = axis_title_bold,
    axis_title_just    = axis_title_just,
    axis_title_size    = axis_title_size,
    subtitle_size      = subtitle_size,
    default_geom_font  = default_geom_font,
    default_geom_color = default_geom_color,
    axis_text_color    = axis_text_color,
    plot_caption_color = plot_caption_color,
    panel_border_color = panel_border_color,
    use_showtext       = use_showtext,
    panel_background_color = panel_background_color,
    panel_grid         = panel_grid,
    ...
  )
}

#' @rdname theme_moffitt
#' @export
theme_grk <- function(
  base_family        = "PT Sans",
  axis_text_family   = "PT Mono",
  axis_title_family  = base_family,
  axis_title_bold    = FALSE,
  axis_title_just    = "cc",
  axis_title_size    = 13,
  subtitle_size      = 13,
  default_geom_font  = "PT Sans Narrow",
  default_geom_color = grkmisc::moffitt_colors$blue,
  axis_text_color    = "#6e6e6e",
  plot_caption_color = axis_text_color,
  panel_border_color = axis_text_color,
  panel_background_color = "grey96",
  ...,
  use_showtext = TRUE,
  panel_grid   = c("major", "minor", "both", "none")
) {
  axis_title_family_name <- axis_title_family
  # Get and set fonts - this works across all devices
  has_showtext <- requireNamespace("sysfonts", quietly = TRUE) &&
    requireNamespace("showtext", quietly = TRUE)
  if (use_showtext && has_showtext) {
    axis_title_family_name <- if (axis_title_bold) {
      paste(sub(" ?([bB]old|[sS]emi-?[bB]old)", "", axis_title_family), "Bold")
    } else axis_title_family

    try_font_add_google(base_family, regular.wt = 400, bold.wt = 600)
    if (axis_title_family_name != base_family)
      try_font_add_google(axis_title_family, axis_title_family_name, regular.wt = 600)
    try_font_add_google(default_geom_font)
    try_font_add_google(axis_text_family)
    showtext::showtext_auto()
  } else {
    if (!isTRUE(getOption("grkmisc.warned_install_showtext"))) {
      rlang::warn("For consistent font display, `install.packages(c(\"sysfonts\", \"showtext\")")
      options("grkmisc.warned_install_showtext" = TRUE)
    }
  }

  if (!is.null(default_geom_font)) hrbrthemes::update_geom_font_defaults(default_geom_font)
  if (!is.null(default_geom_color)) update_geom_moffitt_defaults(default = default_geom_color)

  theme <-
    hrbrthemes::theme_ipsum(
      base_family = base_family,
      subtitle_size = subtitle_size,
      axis_title_size = axis_title_size,
      axis_title_family = axis_title_family_name,
      axis_title_just = axis_title_just,
      ...
    ) +
    ggplot2::theme(
      axis.text = ggplot2::element_text(
        family = axis_text_family, color = axis_text_color, inherit.blank = TRUE),
      plot.caption = ggplot2::element_text(
        family = base_family, face = "plain", color = plot_caption_color, inherit.blank = TRUE)
    )

  theme <- if (is.null(panel_border_color)) {
    theme + ggplot2::theme(panel.border = ggplot2::element_blank())
  } else {
    theme + ggplot2::theme(
      panel.border = ggplot2::element_rect(fill = NA, color = panel_border_color, inherit.blank = TRUE)
    )
  }

  theme <- if (is.null(panel_background_color)) {
    theme + ggplot2::theme(panel.background = ggplot2::element_blank())
  } else {
    theme + ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = panel_background_color, color = NA, inherit.blank = TRUE)
    )
  }

  theme <- theme +
    switch(
      match.arg(panel_grid),
      "major" = ggplot2::theme(
        panel.grid.minor.x = ggplot2::element_blank(),
        panel.grid.minor.y = ggplot2::element_blank()
      ),
      "minor" = ggplot2::theme(
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_blank()
      ),
      "none" = ggplot2::theme(
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.minor.x = ggplot2::element_blank(),
        panel.grid.minor.y = ggplot2::element_blank()
      ),
      NULL
    )

  theme
}

# Try to get font from Google, otherwise returns null
try_font_add_google <- function(font, ...) {
  x <- purrr::possibly(sysfonts::font_add_google, NULL)(font, ...)
  if (is.null(x)) rlang::warn(
    glue::glue('Font "{font}" wasn\'t found on Google Fonts, but may be installed locally.')
  )
}

update_geom_moffitt_defaults <- function(
  geom = c("bar" = "fill", "col" = "fill", "dotplot" = "color", "point" = "color"),
  default = grkmisc::moffitt_colors$blue
) {
  purrr::iwalk(geom, ~ .update_geom_default(.y, .x, value = default))
}

.update_geom_default <- function(geom, attr, value) {
  ggplot2::update_geom_defaults(geom, setNames(list(value), attr))
}

# ---- Scales ----

moffitt_pal <- function(color_other = "grey", direction = 1) {
  function(n) {
    if (n > 7) rlang::warn("Moffitt Color Palette only has 7 colors.")

    x <- if (n == 2) {
      color_other <- if (!color_other %in% names(grkmisc::moffitt_colors)) {
        color_other
      } else grkmisc::moffitt_colors[color_other]
      c(grkmisc::moffitt_colors["blue"], color_other)
    } else grkmisc::moffitt_colors[1:n]

    x <- unname(unlist(x))
    if (direction > 0) x else rev(x)
  }
}

#' Moffitt Color Scales for ggplot2
#'
#' Color scales based on the Moffitt Branding Guidelines, 2014.
#'
#' @seealso [moffitt_colors] [theme_moffitt()]
#' @inheritDotParams ggplot2::discrete_scale
#' @param color_other When the data contains two values, the second value takes
#'   this color. Can be any of the colors in [moffitt_colors] other than blue:
#'   green, red, orange, light_blue, yellow, or grey (default).
#' @param direction Reverses the direction of the color scale when `direction`
#'   is less than 0, i.e. -1.
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars) +
#'   aes(mpg, wt, color = paste(vs)) +
#'   geom_point(size = 2) +
#'   theme_moffitt() +
#'   scale_color_moffitt()
#'
#' ggplot(mtcars) +
#'   aes(mpg, wt, color = paste(vs)) +
#'   geom_point(size = 2) +
#'   theme_moffitt() +
#'   scale_color_moffitt(color_other = "green")
#'
#' ggplot(mtcars) +
#'   aes(mpg, wt, color = paste(carb)) +
#'   geom_point(size = 2) +
#'   theme_moffitt() +
#'   scale_color_moffitt()
#'
#' dplyr::count(mpg, class, sort = TRUE) %>%
#'   dplyr::mutate(class = factor(class, levels = class)) %>%
#'   ggplot() +
#'   aes(class, n, fill = class) +
#'   geom_col() +
#'   coord_flip() +
#'   theme_moffitt() +
#'   scale_fill_moffitt()
#'
#' @name scale_moffitt
#' @export
scale_colour_moffitt <- function(color_other = "grey", direction = 1, ...) {
  ggplot2::discrete_scale("colour", "moffitt", moffitt_pal(color_other, direction), ...)
}

#' @name scale_moffitt
#' @export
scale_color_moffitt <- scale_colour_moffitt

#' @name scale_moffitt
#' @export
scale_fill_moffitt <- function(color_other = "grey", direction = 1, ...) {
  ggplot2::discrete_scale("fill", "moffitt", moffitt_pal(color_other, direction), ...)
}
