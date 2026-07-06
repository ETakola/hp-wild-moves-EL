# Make pretty plots (interactive or non-interactive) of species included in Wild Moves ----
# Author: Dr. Elina Takola
# Date: 2026-07-06 (06 July 2026)
# Description: This is a script that finds common names of species in various languages, from each species' scientific (latin) name using WikiData.
# The last chunk is optional and contains code to create an interactive visualization and a static PNG of the species networks. 

# Import species names, packages & build functions to get Greek common names ----

library(dplyr)
library(stringr)
library(scales)
library(plotly)
library(htmlwidgets)
library(jsonlite)
library(htmltools)
library(ragg)
library(rstudioapi)

set.seed(161)        # Sets seed for reproducibility if random steps are used later

# Import data ----

sp <- read.csv(file.choose(), header = TRUE, sep = ",", stringsAsFactors = FALSE) # Choose the input file ("species.csv")
str(sp)                                           # Check structure

# Visualize interactive species network with sp data frame and static png ----
# Assumes your data frame is already called sp

# Helpers
is_missing <- function(x) {
  is.na(x) | str_trim(x) == ""
}

hex_to_rgba <- function(hex, alpha = 1) {
  rgb <- grDevices::col2rgb(hex)
  sprintf(
    "rgba(%s,%s,%s,%.3f)",
    rgb[1, 1], rgb[2, 1], rgb[3, 1], alpha
  )
}

# Simple force-based label repulsion
repel_label_positions <- function(df,
                                  iterations = 500,
                                  step = 0.025,
                                  pull = 0.015,
                                  point_padding = 0.35) {
  
  n <- nrow(df)
  
  lx <- df$label_x
  ly <- df$label_y
  ax <- df$x
  ay <- df$y
  
  # approximate label "radius" using text length
  label_radius <- pmin(1.15, pmax(0.35, nchar(df$display_name) * 0.035))
  
  for (iter in seq_len(iterations)) {
    
    # label-label repulsion
    dx <- outer(lx, lx, "-")
    dy <- outer(ly, ly, "-")
    dist <- sqrt(dx^2 + dy^2) + 1e-6
    
    diag(dist) <- Inf
    
    min_dist <- outer(label_radius, label_radius, "+")
    overlap <- pmax(0, min_dist - dist)
    
    fx <- rowSums((dx / dist) * overlap, na.rm = TRUE) * step
    fy <- rowSums((dy / dist) * overlap, na.rm = TRUE) * step
    
    # label-point repulsion, so labels avoid sitting on stars
    px <- outer(lx, df$x, "-")
    py <- outer(ly, df$y, "-")
    pdist <- sqrt(px^2 + py^2) + 1e-6
    
    point_overlap <- pmax(0, point_padding - pdist)
    
    fx_point <- rowSums((px / pdist) * point_overlap, na.rm = TRUE) * step
    fy_point <- rowSums((py / pdist) * point_overlap, na.rm = TRUE) * step
    
    # gentle pull back toward each species point
    fx_pull <- (ax - lx) * pull
    fy_pull <- (ay - ly) * pull
    
    lx <- lx + fx + fx_point + fx_pull
    ly <- ly + fy + fy_point + fy_pull
  }
  
  df$label_x <- lx
  df$label_y <- ly
  
  df
}


# Clean data
sp_clean <- sp %>%
  mutate(
    occurrenceCount = as.numeric(occurrenceCount),
    occurrenceCount = if_else(is.na(occurrenceCount), 0, occurrenceCount),
    speciesGroup = if_else(is_missing(speciesGroup), "Unknown group", speciesGroup),
    display_name = case_when(
      !is_missing(vernacularNameEn) & vernacularNameEn != species ~ vernacularNameEn,
      TRUE ~ species
    ),
    occ_log = log10(occurrenceCount + 1)
  )


# Colours
group_names <- sort(unique(sp_clean$speciesGroup))

group_cols <- setNames(
  hcl.colors(length(group_names), palette = "Dark 3"),
  group_names
)


# Group centres
group_centres <- sp_clean %>%
  group_by(speciesGroup) %>%
  summarise(
    total_occurrence = sum(occurrenceCount, na.rm = TRUE),
    n_species = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(total_occurrence)) %>%
  mutate(
    group_id = row_number(),
    n_groups = n(),
    angle = seq(0, 2 * pi, length.out = n_groups + 1)[1:n_groups],
    centre_radius = 7,
    centre_x = centre_radius * cos(angle),
    centre_y = centre_radius * sin(angle)
  )


# Species positions

constellation_df <- sp_clean %>%
  left_join(group_centres, by = "speciesGroup") %>%
  group_by(speciesGroup) %>%
  arrange(desc(occurrenceCount), .by_group = TRUE) %>%
  mutate(
    rank_in_group = row_number(),
    
    # abundant species closer to the group centre
    local_radius = rescale(rank_in_group, to = c(0.45, 3.1)) +
      runif(n(), -0.35, 0.35),
    
    local_angle = runif(n(), 0, 2 * pi),
    
    x = centre_x + local_radius * cos(local_angle),
    y = centre_y + local_radius * sin(local_angle),
    
    # initial label position: slightly outside each species point
    outward_x = if_else(
      sqrt((x - centre_x)^2 + (y - centre_y)^2) == 0,
      cos(local_angle),
      (x - centre_x) / sqrt((x - centre_x)^2 + (y - centre_y)^2)
    ),
    outward_y = if_else(
      sqrt((x - centre_x)^2 + (y - centre_y)^2) == 0,
      sin(local_angle),
      (y - centre_y) / sqrt((x - centre_x)^2 + (y - centre_y)^2)
    ),
    
    label_x = x + 0.45 * outward_x,
    label_y = y + 0.45 * outward_y
  ) %>%
  ungroup() %>%
  arrange(desc(occurrenceCount)) %>%
  mutate(
    label_rank = row_number(),
    point_size = rescale(sqrt(occurrenceCount + 1), to = c(5, 28)),
    hover_text = paste0(
      "<b>", htmlEscape(display_name), "</b>",
      "<br><i>", htmlEscape(species), "</i>",
      "<br>Species group: ", htmlEscape(speciesGroup),
      "<br>Occurrences: ", comma(occurrenceCount),
      "<br>Species key: ", speciesKey
    )
  )

# repel all species labels from one another and from stars
constellation_df <- repel_label_positions(constellation_df)


# Cluster label positions

cluster_labels <- constellation_df %>%
  group_by(speciesGroup, centre_x, centre_y, angle) %>%
  summarise(
    cluster_extent = max(
      sqrt((x - centre_x)^2 + (y - centre_y)^2),
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    label_offset = cluster_extent + 1.35,
    label_x = centre_x + label_offset * cos(angle),
    label_y = centre_y + label_offset * sin(angle),
    label_xanchor = case_when(
      cos(angle) > 0.25 ~ "left",
      cos(angle) < -0.25 ~ "right",
      TRUE ~ "center"
    ),
    label_yanchor = case_when(
      sin(angle) > 0.25 ~ "bottom",
      sin(angle) < -0.25 ~ "top",
      TRUE ~ "middle"
    )
  )


# Annotations/labels
initial_labels <- 18

species_annotations <- lapply(seq_len(nrow(constellation_df)), function(i) {
  
  d <- constellation_df[i, ]
  
  list(
    x = d$label_x,
    y = d$label_y,
    xref = "x",
    yref = "y",
    text = htmlEscape(d$display_name),
    showarrow = FALSE,
    visible = d$label_rank <= initial_labels,
    font = list(
      size = 11,
      color = group_cols[[d$speciesGroup]]
    ),
    bgcolor = "rgba(11,16,32,0.72)",
    bordercolor = "rgba(255,255,255,0)",
    borderpad = 2,
    opacity = 0.98
  )
})

cluster_annotations <- lapply(seq_len(nrow(cluster_labels)), function(i) {
  
  d <- cluster_labels[i, ]
  
  list(
    x = d$label_x,
    y = d$label_y,
    xref = "x",
    yref = "y",
    text = paste0("<b>", htmlEscape(d$speciesGroup), "</b>"),
    showarrow = FALSE,
    visible = TRUE,
    xanchor = d$label_xanchor,
    yanchor = d$label_yanchor,
    font = list(
      size = 14,
      color = "white"
    ),
    bgcolor = group_cols[[d$speciesGroup]],
    bordercolor = "rgba(255,255,255,0)",
    borderpad = 5,
    opacity = 0.98
  )
})

all_annotations <- c(species_annotations, cluster_annotations)


# Plotly interactive plot

p <- plot_ly()

# connection lines, one trace per group
for (g in group_names) {
  
  d <- constellation_df %>%
    filter(speciesGroup == g)
  
  seg_x <- as.vector(rbind(d$centre_x, d$x, NA))
  seg_y <- as.vector(rbind(d$centre_y, d$y, NA))
  
  p <- p %>%
    add_trace(
      x = seg_x,
      y = seg_y,
      type = "scatter",
      mode = "lines",
      line = list(
        color = hex_to_rgba(group_cols[[g]], 0.20),
        width = 0.8
      ),
      hoverinfo = "none",
      showlegend = FALSE
    )
}

# species stars
for (g in group_names) {
  
  d <- constellation_df %>%
    filter(speciesGroup == g)
  
  p <- p %>%
    add_trace(
      data = d,
      x = ~x,
      y = ~y,
      type = "scatter",
      mode = "markers",
      name = g,
      text = ~hover_text,
      hoverinfo = "text",
      marker = list(
        color = group_cols[[g]],
        size = d$point_size,
        opacity = 0.86,
        line = list(
          color = "rgba(255,255,255,0.35)",
          width = 0.5
        )
      )
    )
}

# cluster centre stars
p <- p %>%
  add_trace(
    data = group_centres,
    x = ~centre_x,
    y = ~centre_y,
    type = "scatter",
    mode = "markers",
    hoverinfo = "none",
    showlegend = FALSE,
    marker = list(
      color = unname(group_cols[group_centres$speciesGroup]),
      size = 16,
      opacity = 0.95,
      line = list(
        color = "white",
        width = 1
      )
    )
  )

all_x <- c(constellation_df$x, constellation_df$label_x, cluster_labels$label_x)
all_y <- c(constellation_df$y, constellation_df$label_y, cluster_labels$label_y)

x_range <- range(all_x, na.rm = TRUE) + c(-1.2, 1.2)
y_range <- range(all_y, na.rm = TRUE) + c(-1.2, 1.2)

p <- p %>%
  layout(
    title = list(
      text = paste0(
        "<b>Constellations of species in Wild Moves</b>",
        "<br><sup>",
        "Each cluster is a species group. Larger stars and stars closer to the cluster centre represent species with more occurrences.",
        "</sup>"
      ),
      font = list(color = "white", size = 21),
      x = 0.5
    ),
    annotations = all_annotations,
    xaxis = list(
      visible = FALSE,
      zeroline = FALSE,
      showgrid = FALSE,
      range = x_range,
      scaleanchor = "y"
    ),
    yaxis = list(
      visible = FALSE,
      zeroline = FALSE,
      showgrid = FALSE,
      range = y_range
    ),
    plot_bgcolor = "#0B1020",
    paper_bgcolor = "#0B1020",
    legend = list(
      orientation = "h",
      x = 0.5,
      xanchor = "center",
      y = -0.05,
      font = list(color = "white")
    ),
    margin = list(l = 20, r = 20, t = 80, b = 40)
  ) %>%
  config(
    displayModeBar = TRUE,
    scrollZoom = TRUE
  )


# Reveal more labels when zooming in
label_meta <- constellation_df %>%
  transmute(
    rank = label_rank,
    x = x,
    y = y
  )

js_payload <- list(
  labels = label_meta,
  nSpeciesAnnotations = nrow(constellation_df),
  xRange = x_range,
  yRange = y_range
)

p <- htmlwidgets::onRender(
  p,
  sprintf(
    "
    function(el, x) {
      var gd = document.getElementById(el.id);
      var payload = %s;
      var labels = payload.labels;
      var nSpecies = payload.nSpeciesAnnotations;
      var fullX = payload.xRange;
      var fullY = payload.yRange;
      var updating = false;

      function currentRange(axisName, fallback) {
        var axis = gd._fullLayout[axisName];
        if (axis && axis.range) {
          return axis.range;
        }
        return fallback;
      }

      function maxRankForZoom(areaRatio) {
        if (areaRatio < 0.035) return 1000000;  // very zoomed in: all labels
        if (areaRatio < 0.080) return 220;
        if (areaRatio < 0.160) return 150;
        if (areaRatio < 0.300) return 90;
        if (areaRatio < 0.550) return 45;
        return 18;                              // zoomed out: only top species
      }

      function updateLabels() {
        if (updating) return;
        updating = true;

        var xr = currentRange('xaxis', fullX);
        var yr = currentRange('yaxis', fullY);

        var fullArea = Math.abs((fullX[1] - fullX[0]) * (fullY[1] - fullY[0]));
        var viewArea = Math.abs((xr[1] - xr[0]) * (yr[1] - yr[0]));
        var areaRatio = viewArea / fullArea;

        var maxRank = maxRankForZoom(areaRatio);
        var update = {};

        for (var i = 0; i < labels.length; i++) {
          var d = labels[i];

          var inView =
            d.x >= Math.min(xr[0], xr[1]) &&
            d.x <= Math.max(xr[0], xr[1]) &&
            d.y >= Math.min(yr[0], yr[1]) &&
            d.y <= Math.max(yr[0], yr[1]);

          update['annotations[' + i + '].visible'] =
            inView && d.rank <= maxRank;
        }

        // cluster labels are always visible
        var totalAnnotations = gd.layout.annotations.length;
        for (var j = nSpecies; j < totalAnnotations; j++) {
          update['annotations[' + j + '].visible'] = true;
        }

        Plotly.relayout(gd, update).then(function() {
          updating = false;
        });
      }

      gd.on('plotly_relayout', updateLabels);
      setTimeout(updateLabels, 400);
    }
    ",
    jsonlite::toJSON(js_payload, auto_unbox = TRUE)
  )
)
p

# save as HTML
htmlwidgets::saveWidget(p, file = "wildmoves_constellation_interactive.html", selfcontained = TRUE)

# and a static PNG 
# Change this if you want more or fewer labels
label_fraction <- 0.20

# Re-repel label positions for the static PNG
constellation_png <- repel_label_positions(
  constellation_df,
  iterations = 2000,
  step = 0.03,
  pull = 0.012,
  point_padding = 0.60
)

# Label only the most abundant 20% of species within each group
species_labels_png <- constellation_png %>%
  group_by(speciesGroup) %>%
  arrange(desc(occurrenceCount), .by_group = TRUE) %>%
  mutate(
    label_rank_in_group = row_number(),
    n_in_group = n(),
    n_to_label = pmax(1L, ceiling(label_fraction * n_in_group))
  ) %>%
  filter(label_rank_in_group <= n_to_label) %>%
  ungroup()

# Recompute cluster label positions
cluster_png <- constellation_png %>%
  group_by(speciesGroup, centre_x, centre_y, angle) %>%
  summarise(
    cluster_extent = max(
      sqrt((x - centre_x)^2 + (y - centre_y)^2),
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    label_offset = cluster_extent + 1.45,
    label_x = centre_x + label_offset * cos(angle),
    label_y = centre_y + label_offset * sin(angle)
  )

# Dynamic plot boundaries based on the actual plotted points and visible labels
all_x_png <- c(
  constellation_png$x,
  group_centres$centre_x,
  species_labels_png$label_x,
  cluster_png$label_x
)

all_y_png <- c(
  constellation_png$y,
  group_centres$centre_y,
  species_labels_png$label_y,
  cluster_png$label_y
)

x_span <- diff(range(all_x_png, na.rm = TRUE))
y_span <- diff(range(all_y_png, na.rm = TRUE))

x_pad <- max(1.8, 0.14 * x_span)
y_pad <- max(1.8, 0.14 * y_span)

x_range_png <- range(all_x_png, na.rm = TRUE) + c(-x_pad, x_pad)
y_range_png <- range(all_y_png, na.rm = TRUE) + c(-y_pad, y_pad)

# Dynamic PNG size based on the plotted coordinate range
plot_width_units <- diff(x_range_png)
plot_height_units <- diff(y_range_png)

px_per_unit <- 120

main_width_px <- round(plot_width_units * px_per_unit)
main_height_px <- round(plot_height_units * px_per_unit)

# Keep output within practical bounds
main_width_px <- min(max(main_width_px, 2400), 5200)
main_height_px <- min(max(main_height_px, 1800), 4200)

# Extra height for title/subtitle and legend
legend_rows <- ceiling(length(group_names) / 5)
extra_height_px <- 360 + legend_rows * 120

png_width_px <- main_width_px
png_height_px <- main_height_px + extra_height_px

# Helper to draw label boxes
draw_label <- function(x, y, label, text_col, fill_col, cex = 0.75, font = 1) {
  w <- strwidth(label, cex = cex, font = font) * 1.20
  h <- strheight(label, cex = cex, font = font) * 1.70
  
  rect(
    x - w / 2, y - h / 2,
    x + w / 2, y + h / 2,
    col = fill_col,
    border = NA
  )
  
  text(
    x,
    y,
    label,
    col = text_col,
    cex = cex,
    font = font
  )
}

# Save PNG
ragg::agg_png(
  filename = "wildmoves_constellation_static_top20pct_labels.png",
  width = png_width_px,
  height = png_height_px,
  units = "px",
  res = 220,
  background = "#0B1020"
)

# Separate main plot and legend so the legend is not cut off
layout(
  matrix(c(1, 2), ncol = 1),
  heights = c(main_height_px, extra_height_px)
)

# Main plot
par(
  bg = "#0B1020",
  mar = c(1.2, 1.2, 5.2, 1.2),
  xpd = NA
)

plot(
  NA,
  xlim = x_range_png,
  ylim = y_range_png,
  asp = 1,
  axes = FALSE,
  xlab = "",
  ylab = "",
  main = ""
)

title(
  main = "Constellations of species in Wild Moves",
  col.main = "white",
  cex.main = 1.9,
  font.main = 2,
  line = 3.2
)

mtext(
  "Each cluster is a species group. Larger stars and stars closer to the cluster centre represent species with more occurrences.",
  side = 3,
  line = 1.4,
  col = adjustcolor("white", alpha.f = 0.82),
  cex = 0.9
)

# Connection lines
for (g in group_names) {
  d <- constellation_png %>% filter(speciesGroup == g)
  
  segments(
    x0 = d$centre_x,
    y0 = d$centre_y,
    x1 = d$x,
    y1 = d$y,
    col = adjustcolor(group_cols[[g]], alpha.f = 0.22),
    lwd = 1
  )
}

# Species points
for (g in group_names) {
  d <- constellation_png %>% filter(speciesGroup == g)
  
  points(
    d$x,
    d$y,
    pch = 21,
    bg = adjustcolor(group_cols[[g]], alpha.f = 0.86),
    col = adjustcolor("white", alpha.f = 0.35),
    lwd = 0.8,
    cex = scales::rescale(d$point_size, to = c(0.9, 2.4))
  )
}

# Cluster centre points
points(
  group_centres$centre_x,
  group_centres$centre_y,
  pch = 21,
  bg = group_cols[group_centres$speciesGroup],
  col = "white",
  lwd = 1.2,
  cex = 2.1
)

# Species labels: top 20% most abundant within each group
for (i in seq_len(nrow(species_labels_png))) {
  d <- species_labels_png[i, ]
  
  draw_label(
    x = d$label_x,
    y = d$label_y,
    label = d$display_name,
    text_col = group_cols[[d$speciesGroup]],
    fill_col = adjustcolor("#0B1020", alpha.f = 0.72),
    cex = 0.72,
    font = 1
  )
}

# Cluster labels
for (i in seq_len(nrow(cluster_png))) {
  d <- cluster_png[i, ]
  
  draw_label(
    x = d$label_x,
    y = d$label_y,
    label = d$speciesGroup,
    text_col = "white",
    fill_col = group_cols[[d$speciesGroup]],
    cex = 1.05,
    font = 2
  )
}

# Legend panel
par(
  bg = "#0B1020",
  mar = c(0, 1, 0, 1),
  xpd = NA
)

plot.new()

legend(
  "center",
  legend = group_names,
  ncol = min(5, length(group_names)),
  bty = "n",
  text.col = "white",
  col = adjustcolor("white", alpha.f = 0.65),
  pt.bg = group_cols[group_names],
  pch = 21,
  pt.cex = 1.8,
  cex = 0.9
)

dev.off()

# Save script ----
writeLines(rstudioapi::getActiveDocumentContext()$contents, file.path(getwd(), "wikidata_GR_specnames_plot.R"), useBytes = TRUE)

# Session Info ----
#sessionInfo()
#R version 4.5.1 (2025-06-13 ucrt)
#Platform: x86_64-w64-mingw32/x64
#Running under: Windows 11 x64 (build 26200)
#Matrix products: default
#LAPACK version 3.12.1
#locale:
#[1] LC_COLLATE=English_United States.utf8 
#[2] LC_CTYPE=English_United States.utf8   
#[3] LC_MONETARY=English_United States.utf8
#[4] LC_NUMERIC=C                          
#[5] LC_TIME=English_United States.utf8    
#time zone: Europe/Berlin
#tzcode source: internal
#attached base packages:
#  [1] stats     graphics  grDevices utils     datasets  methods  base     
#loaded via a namespace (and not attached):
#  [1] compiler_4.5.1    cli_3.6.5         tools_4.5.1       rstudioapi_0.17.1    
