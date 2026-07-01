#setwd("/Users/divya2/Documents/RMIT Lectures/sem3/data visualisation/A3")

library(shiny)
library(tidyverse)
library(plotly)
library(scales)



# The Conversation Brand Colors

TC_ORANGE  <- "#E64514"   
TC_DARK    <- "#333333"   
TC_BLUE    <- "#1380A1"   
TC_GRID    <- "#EEEEEE"   

TSX_COL <- c(
  Amphibians = "#2B9E4E",   
  Birds      = "#1380A1",   
  Mammals    = "#E64514",   
  Plants     = "#9467BD",   
  Reptiles   = "#C47A1E"    
)

# Taxonomic group colours 
GRP_PAL <- c(
  Birds         = "#1380A1",
  Fish          = "#0D6B86",
  Frogs         = "#2B9E4E",
  Invertebrates = "#8B6914",
  Mammals       = "#E64514",
  Plants        = "#9467BD",   
  Reptiles      = "#C47A1E"
)

# Threat multiplicity bands 
BAND_PAL <- c(
  "1 threat"   = "#C6DBEF",
  "2 threats"  = "#6BAED6",
  "3 threats"  = "#E97040",
  "4+ threats" = "#E64514"
)

# Canonical broad threat order for axes
THREAT_ORDER <- c(
  "Habitat loss", "Invasive species", "Fire regimes",
  "Climate change", "Ecosystem disruption", "Water regimes",
  "Overexploitation", "Pollution"
)


#Load data
df_raw <- read_csv("Species-Threat-Impact-Table 1.csv", show_col_types = FALSE)

# Standardise column names
names(df_raw) <- names(df_raw) |>
  str_replace_all(" ",    "_") |>
  str_replace_all("[()]", ""  ) |>
  str_replace_all("/",    "_per_")

colnames(df_raw)

threat_data <- df_raw |>
  select(
    species_name = Species_name,
    common_name  = Common_name,
    group        = Group,
    epbc_status  = EPBC_Act_status,
    broad_threat = Broad_level_threat,
    sub_threat   = Sub_category_threat,
    impact_score = Impact_score
  ) |>
  mutate(
    # Clean EPBC status - CE (36 rows) treated as Critically Endangered
    epbc_clean = case_when(
      epbc_status %in% c("CR", "CE", "CR (PE)") ~ "Critically Endangered",
      epbc_status == "EN"                         ~ "Endangered",
      epbc_status == "VU"                         ~ "Vulnerable",
      TRUE                                        ~ NA_character_
    ),
    epbc_clean = factor(epbc_clean,
                        levels = c("Vulnerable", "Endangered",
                                   "Critically Endangered")),
    # Short threat labels for axes
    broad_short = case_when(
      broad_threat == "Habitat loss, fragmentation and degradation"                   ~ "Habitat loss",
      broad_threat == "Invasive species and diseases"                                 ~ "Invasive species",
      broad_threat == "Adverse fire regimes"                                          ~ "Fire regimes",
      broad_threat == "Climate change and severe weather"                             ~ "Climate change",
      broad_threat == "Disrupted ecosystem and population processes"                  ~ "Ecosystem disruption",
      broad_threat == "Changed surface and groundwater regimes"                       ~ "Water regimes",
      broad_threat == "Overexploitation and other direct harm from human activities"  ~ "Overexploitation",
      broad_threat == "Pollution"                                                     ~ "Pollution",
      TRUE ~ broad_threat
    ),
    broad_short = factor(broad_short, levels = THREAT_ORDER)
  ) |>
  filter(!is.na(epbc_clean))

#tsx data

.read_tsx <- function(path, grp) {
  read_csv(path, show_col_types = FALSE) |>
    mutate(group = grp) |>
    rename(index = value, lower = low, upper = high) |>
    select(year, group, index, lower, upper)
}

tsx_data <- bind_rows(
  .read_tsx("tsx_amphibians_trend.csv", "Amphibians"),
  .read_tsx("tsx_birds_trend.csv",      "Birds"),
  .read_tsx("tsx_mammals_trend.csv",    "Mammals"),
  .read_tsx("tsx_reptiles_trend.csv",   "Reptiles"),
  .read_tsx("tsx_plants_trend.csv",     "Plants")
) |>
  # Limit to years 1985–2022 to keep the plot clean
  filter(year >= 1985, year <= 2022)


# Charts pre-processing

# Chart 2: prevalence vs severity quadrant scatter 
chart2 <- threat_data |>
  group_by(broad_short) |>
  summarise(
    n_spp        = n_distinct(species_name),
    pct_high_med = round(mean(impact_score %in% c("High", "Medium")) * 100, 1),
    .groups = "drop"
  ) |>
  mutate(
    is_inv  = (broad_short == "Invasive species"),
    pt_col  = if_else(is_inv, TC_ORANGE, TC_BLUE),
    pt_size = rescale(n_spp, to = c(14, 34)),
    tip     = paste0(
      "<b>", broad_short, "</b><br>",
      "Species affected: <b>", n_spp, "</b><br>",
      "High/medium severity: <b>", pct_high_med, "%</b>"
    )
  )

# Chart 3: heatmap (taxonomic group × broad threat) 
grp_totals <- threat_data |>
  group_by(group) |>
  summarise(total = n_distinct(species_name), .groups = "drop")

chart3 <- threat_data |>
  filter(impact_score %in% c("High", "Medium")) |>
  mutate(broad_short = as.character(broad_short)) |>
  group_by(group, broad_short) |>
  summarise(n = n_distinct(species_name), .groups = "drop") |>
  complete(
    group       = unique(threat_data$group),
    broad_short = THREAT_ORDER,
    fill        = list(n = 0)
  ) |>
  left_join(grp_totals, by = "group") |>
  mutate(
    pct = round(n / total * 100, 1),
    tip = paste0(
      "<b>", group, " \u00d7 ", broad_short, "</b><br>",
      n, " of ", total, " species (", pct, "%)<br>",
      "at high or medium severity"
    )
  )
chart3_grp_order <- c("Frogs","Reptiles","Fish","Mammals","Birds","Invertebrates","Plants")

# Chart 4: top invasive subcategories stacked bar 
inv_top <- threat_data |>
  filter(broad_threat == "Invasive species and diseases",
         impact_score  %in% c("High", "Medium")) |>
  group_by(sub_threat) |>
  summarise(tot = n_distinct(species_name), .groups = "drop") |>
  slice_max(tot, n = 12) |>
  pull(sub_threat)

chart4 <- threat_data |>
  filter(
    broad_threat == "Invasive species and diseases",
    impact_score  %in% c("High", "Medium"),
    sub_threat    %in% inv_top
  ) |>
  group_by(sub_threat, group) |>
  summarise(n = n_distinct(species_name), .groups = "drop") |>
  group_by(sub_threat) |>
  mutate(total = sum(n)) |>
  ungroup() |>
  mutate(
    sub_threat = fct_reorder(sub_threat, total),
    tip        = paste0("<b>", group, "</b>: ", n, " species")
  )
chart4_groups <- sort(unique(chart4$group))

#  Chart 5: threat multiplicity 100%-stacked bar 
chart5 <- threat_data |>
  filter(impact_score %in% c("High", "Medium")) |>
  group_by(species_name, group) |>
  summarise(n_cats = n_distinct(broad_threat), .groups = "drop") |>
  mutate(
    band = case_when(
      n_cats == 1 ~ "1 threat",
      n_cats == 2 ~ "2 threats",
      n_cats == 3 ~ "3 threats",
      TRUE        ~ "4+ threats"
    ),
    band = factor(band,
                  levels = c("1 threat", "2 threats","3 threats", "4+ threats"))
  ) |>
  group_by(group, band) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(group) |>
  mutate(
    pct   = round(n / sum(n) * 100, 1),
    total = sum(n)
  ) |>
  ungroup() |>
  mutate(
    group = fct_reorder(group, total, .desc = TRUE),
    tip   = paste0(
      "<b>", group, " - ", band, "</b><br>",
      n, " species (", pct, "%)"
    )
  ) 

#helpers

HOVER_STYLE <- list(
  bgcolor     = "white",
  bordercolor = "#666666",
  font        = list(size = 12, family = "Georgia, serif", color = "#222222")
)
W600 <- "max-width:600px; margin:0 auto;"   

#ui
ui <- fluidPage(
  plotlyOutput("p1")
)

FROGS_NOTE <- tags$p(
  style = "font-size:11px;color:#888;margin:2px 0 8px 0;font-style:italic;",
  "Note: 'Frogs' in the threat data (Ward et al.) and 'Amphibians' in the
   population trend data (TSX) refer to the same taxonomic group - the
   difference is source labelling convention, not taxonomy."
)

ui <- navbarPage(
  title       = "Australia's Hidden Extinction Crisis",
  collapsible = TRUE,
  id          = "main_nav",
  
  header = tags$head(tags$style(HTML("
 
    body {
      font-family: Georgia, 'Times New Roman', serif;
      color: #333333;
      background: #ffffff;
    }
 
    .navbar { background-color: #E64514 !important; border: none !important; }
    .navbar .navbar-brand,
    .navbar-nav > li > a          { color: #ffffff !important; }
    .navbar-nav > li > a:hover,
    .navbar-nav > li.active > a   { background-color: #C23B0F !important; color: #fff !important; }
 
    .tab-content { padding: 22px 16px 36px; max-width: 640px; margin: 0 auto; }
 
    h4.ct {
      font-size: 20px; font-weight: 700; color: #111111;
      border-left: 5px solid #E64514; padding-left: 12px;
      margin: 4px 0 8px; line-height: 1.35;
    }
 
    p.cd {
      font-size: 13.5px; color: #555555; margin: 0 0 14px 17px;
      font-style: italic; line-height: 1.65;
    }
 
    p.cs {
      font-size: 11px; color: #999999; margin-top: 10px;
      border-top: 1px solid #eeeeee; padding-top: 8px; line-height: 1.55;
    }
 
    .about-body { font-size: 13.5px; line-height: 1.85; max-width: 580px; }
    .about-body h5 { color: #E64514; font-size: 15px; margin-top: 24px; }
  "))),
  
  # Tab 1: Population decline
  tabPanel("1 \u00b7 The Collapse",
           tags$h4("Australia\u2019s threatened species are in freefall", class="ct"),
           tags$p(
             "Since 1985, Australia's threatened species have lost between 12% and 88%
           of their populations. The scale varies dramatically by group - reptiles
           and amphibians have nearly vanished from monitoring baselines, while
           mammals show the shallowest but still alarming decline. Hover each
           endpoint to see confidence intervals.",
             class="cd"
           ),
           div(class="filter-row",
               radioButtons("p1_start", "Show data from:",
                            choices  = c("1985 \u2014 full record" = 1985,
                                         "2000 \u2014 since turn of century" = 2000,
                                         "2010 \u2014 recent decade" = 2010),
                            selected = 1985, inline = TRUE)
           ),
           div(style=W600, plotlyOutput("p1", height="420px")),
           tags$p(
             HTML("Source: TERN (2025). <em>Australia\u2019s Threatened Species Index 2025</em>.
            Terrestrial Ecosystem Research Network, University of Queensland.
            Retrieved June 2025 from tsx.org.au. EPBC-listed threatened species only;
            1985 = 100% baseline.
            Data ends 2022 (latest published TSX release)."),
             class="cs"
           )
  ),
  
  # Tab 2: Prevalence vs severity
  tabPanel("2 · The Twist",
           tags$h4("The most widespread threat is not the most deadly", class = "ct"),
           tags$p(
             "Each bubble is one of 8 broad threat categories. Position shows
       how many species it affects (x-axis) versus how often it causes
       high or medium severity harm (y-axis). Bubble size encodes the
       raw count of high/medium-impact records. Hover any bubble for
       exact figures.",
             class = "cd"
           ),
           div(style = W600, plotlyOutput("p2", height = "460px")),
           tags$p(
             HTML("Source: Ward, M., et al. (2021). A national-scale dataset for threats
            impacting Australia's imperiled flora and fauna.
            <em>Ecology and Evolution</em>, 11, 11749–11761.
            doi.org/10.1002/ece3.7920"),
             class = "cs"
           )
  ),
  
  #  Tab 3: Heatmap 
  tabPanel("3 · Different Enemies",
           tags$h4("Different animals face entirely different threats", class = "ct"),
           tags$p(
             "Each cell shows the percentage of that taxonomic group's threatened
       species facing high or medium severity impact from that threat.
       Darker blue = more severely affected. Use the filter to isolate
       any group. Hover cells for exact counts.",
             class = "cd"
           ),
           FROGS_NOTE,           
           div(class = "filter-row",
               selectInput("p3_group", "Show group:",
                           choices  = c("All groups" = "all",
                                        setNames(chart3_grp_order, chart3_grp_order)),
                           selected = "all", width = "220px")
           ),
           div(style = W600, plotlyOutput("p3", height = "380px")),
           tags$p(
             HTML("Source: Ward, M., et al. (2021). Cells show the percentage of each
            group's EPBC-listed threatened species facing high or medium severity
            from each broad threat category."),
             class = "cs"
           )
  ),
  
  # Tab 4: Invasive culprits 
  tabPanel("4 · The Culprits",
           tags$h4(
             "Cats, foxes, weeds and fungus: the cast of Australia's extinction crisis",
             class = "ct"
           ),
           tags$p(
             "The 11 invasive and disease subcategories ranked by threatened
       species affected at high or medium severity. Bars are coloured by
       taxonomic group. Filter to see which culprits target specific groups
       \u2014 the ranking re-orders automatically. Hover segments for exact counts.",
             class = "cd"
           ),
           FROGS_NOTE,
           div(class = "filter-row",
               selectInput("p4_group", "Filter by taxonomic group:",
                           choices  = c("All groups" = "all",
                                        setNames(chart4_groups, chart4_groups)),
                           selected = "all", width = "220px")
           ),
           div(style = W600, plotlyOutput("p4", height = "490px")),
           tags$p(
             HTML("Source: Ward, M., et al. (2021). Top 12 invasive subcategories at
            high or medium severity. Filtering to a specific group re-ranks
            threats by that group's most impactful invaders."),
             class = "cs"
           )
  ),
  
  
  # Tab 5: Threat multiplicity 
  tabPanel("5 · The Burden",
           tags$h4(
             "Most threatened species aren't fighting one battle - they're fighting many",
             class = "ct"
           ),
           tags$p(
             "Each bar shows the proportion of a group's threatened species facing
               1, 2, 3 or 4+ broad threat categories simultaneously at high or
               medium severity. Blue tones = manageable; orange = crisis level.
               Use the sort toggle to reorder by group name or by total species count.
               Hover segments for exact counts. Single-issue conservation cannot
               save species that face this many simultaneous threats.",
             class = "cd"
           ),
           div(class="filter-row",
               radioButtons("p5_sort", "Sort bars by:",
                            choices  = c("Total species (descending)" = "total",
                                         "Group name (A\u2013Z)"     = "alpha"),
                            selected = "total", inline = TRUE)
           ),           
           div(style = W600, plotlyOutput("p5", height = "400px")),
           tags$p(
             HTML("Source: Ward, M., et al. (2021). Each bar shows the proportion
            of each group's EPBC-listed threatened species facing 1, 2, 3 or
            4+ simultaneous broad threat categories at high or medium severity."),
             class = "cs"
           )
  ),
  
  
  # About & Data 
  tabPanel("About & Data",
           div(class = "about-body",
               tags$h5("The story"),
               tags$p(
                 "This article outline explores why Australia's extinction crisis is
                  deeper than most public discourse acknowledges. While habitat loss
                  dominates headlines, expert-validated threat data shows that invasive
                  species and diseases cause more severe harm per species, that different
                  groups face fundamentally different enemies, and that most threatened
                  fauna face multiple simultaneous threats that single-issue policy
                  cannot address."
               ),
               tags$h5("Data sources"),
               tags$p(HTML(
                 "<b>Primary - threats data:</b> Ward, M., Carwardine, J., Yong, C.J.,
                      Watson, J.E.M., Reside, A.E., Maron, M., … & Possingham, H.P. (2021).
                      A national-scale dataset for threats impacting Australia's imperiled
                      flora and fauna. <em>Ecology and Evolution</em>, 11(17), 11749–11761.
                      <a href='https://doi.org/10.1002/ece3.7920' target='_blank'>
                      doi.org/10.1002/ece3.7920</a>.
                      Dataset: <a href='https://doi.org/10.6084/m9.figshare.13150943.v1'
                      target='_blank'>Figshare (open access)</a>."
               )),
               tags$p(HTML(
                 "<b>Secondary - population trends:</b> TERN (2025).
                  <em>Australia's Threatened Species Index 2025</em>.
                  Terrestrial Ecosystem Research Network, University of Queensland.
                  <a href='https://tsx.org.au' target='_blank'>tsx.org.au</a>.
                  Trend (CSV) downloads, EPBC-listed threatened species, all groups."
               )),
               tags$h5("Acknowledgements"),
               tags$p(
                 "Generative AI Gemini (Google, gemini.google.com, accessed June 2025) was used to assist with R code formatting. All analytical
                  decisions, chart design, narrative framing and interpretation are my own."
               )
           )
  )
)


#server

server <- function(input, output, session) {
  
  # ── Chart 1: TSX multi-line trend ─────────────────────────────────────────
  

  output$p1 <- renderPlotly({
    start_year <- as.integer(input$p1_start)
    tsx_plot <- tsx_data |> filter(year >= start_year)
    # Two-point slope: 1985 baseline and latest available year per group
    slope_data <- tsx_plot |>
      group_by(group) |>
      filter(year == min(year) | year == max(year)) |>
      mutate(
        point_type = if_else(year == min(year), "start", "end"),
        pct        = round(index * 100, 0),
        pct_lower  = round(lower * 100, 0),
        pct_upper  = round(upper * 100, 0),
        label      = if_else(
          point_type == "end",
          paste0(group, "\n", pct, "%"),
          ""
        ),
        tip = paste0(
          "<b>", group, " \u2014 ", year, "</b><br>",
          "Population: ", pct, "% of 1985 level<br>",
          "95% CI: ", pct_lower, "\u2013", pct_upper, "%"
        )
      ) |>
      ungroup()
    
    # Decline magnitude for sorting (largest decline at top)
    end_vals <- slope_data |>
      filter(point_type == "end") |>
      arrange(pct) |>
      pull(group)
    
    p <- plot_ly()
    
    for (grp in end_vals) {
      d   <- filter(slope_data, group == grp)
      col <- TSX_COL[[grp]]
      d_s <- filter(d, point_type == "start")
      d_e <- filter(d, point_type == "end")
      decline <- 100 - d_e$pct
      
      # Slope line
      p <- add_lines(p,
                     x = c(d_s$year, d_e$year),
                     y = c(d_s$index, d_e$index),
                     line      = list(color = col, width = 2.5),
                     name      = grp,
                     showlegend = TRUE,
                     hoverinfo = "none"
      )
      # Start point
      p <- add_markers(p,
                       x         = d_s$year, y = d_s$index,
                       marker    = list(color = col, size = 9, line = list(color="white", width=1.5)),
                       name      = grp, showlegend = FALSE,
                       text      = d_s$tip, hoverinfo = "text"
      )
      # End point
      p <- add_markers(p,
                       x         = d_e$year, y = d_e$index,
                       marker    = list(color = col, size = 9, line = list(color="white", width=1.5)),
                       name      = grp, showlegend = FALSE,
                       text      = d_e$tip, hoverinfo = "text"
      )
      y_offset <- case_when(
        #grp == "Reptiles"   ~ -0.12,
        grp == "Amphibians" ~  0.12,
        TRUE                ~  0
      )
      # End label: group name + % remaining + decline
      p <- add_annotations(p,
                           x = d_e$year + 0.8, 
                           y = d_e$index + y_offset,
                           text      = paste0("<b>", grp, "</b>  ", d_e$pct, "%<br>",
                                              "<span style='color:", col, "'>&#9660; ", decline, "% lost</span>"),
                           showarrow = FALSE, xanchor = "left",
                           font      = list(size = 10, family = "Georgia", color = TC_DARK)
      )
    }
    
    p |>
      layout(
        xaxis = list(
          title     = "",
          showline  = TRUE, linecolor = "#AAAAAA", linewidth = 1,
          gridcolor = TC_GRID, zeroline = FALSE,
          tickvals  = c(1985, max(tsx_data$year)),
          ticktext  = c("1985\n(baseline)", as.character(max(tsx_data$year))),
          tickfont  = list(size = 11, family = "Georgia"),
          range     = c(1983, 2028)
        ),
        yaxis = list(
          title      = "Relative population size (1985 = 100%)",
          tickformat = ".0%",
          showline   = TRUE, linecolor = "#AAAAAA", linewidth = 1,
          gridcolor  = TC_GRID, zeroline = FALSE,
          range      = c(-0.05, 1.15),
          tickfont   = list(size = 11, family = "Georgia")
        ),
        shapes = list(
          list(type="line", x0=1985, x1=max(tsx_data$year),
               y0=1.0, y1=1.0,
               line=list(color="#E5E5E5", width=0.7, dash="dash"))
        ),
        legend = list(
          orientation="h", xanchor="center",
          x=0.5, y=-0.18, bgcolor="white",
          font=list(size=11, family="Georgia")
        ),
        plot_bgcolor  = "white",
        paper_bgcolor = "white",
        margin     = list(l=65, r=100, t=10, b=60),
        hoverlabel = HOVER_STYLE
      ) |>
      config(displayModeBar = FALSE)
  })
  
  # ── Chart 2: prevalence vs severity quadrant scatter ─────────────────────
  
  output$p2 <- renderPlotly({
    
    avg_x <- mean(chart2$n_spp)
    avg_y <- mean(chart2$pct_high_med)
    x_max <- max(chart2$n_spp) * 1.20
    
    
    chart2b <- chart2 |>
      mutate(
        n_hm = threat_data |>
          group_by(broad_short) |>
          summarise(n = sum(impact_score %in% c("High","Medium")), .groups="drop") |>
          right_join(chart2 |> select(broad_short), by = "broad_short") |>
          pull(n),
        pt_size2 = rescale(n_hm, to = c(14, 36))
      )
    
    plot_ly(
      chart2b,
      x            = ~n_spp,
      y            = ~pct_high_med,
      type         = "scatter",
      mode         = "markers+text",
      text         = ~broad_short,
      textposition = "top center",
      textfont     = list(size = 9.5, color = TC_DARK, family = "Georgia"),
      hovertext    = ~tip,
      hoverinfo    = "text",
      marker       = list(
        color   = ~pt_col,
        size    = ~pt_size2,
        opacity = 0.85,
        line    = list(color = "white", width = 2.5)
      )
    ) |>
      layout(
        xaxis = list(
          title     = list(
            text = "Number of EPBC-listed species affected (prevalence)",
            font = list(size = 11, family = "Georgia")
          ),
          showline  = TRUE, linecolor = "#888888", linewidth = 1.2,
          mirror    = FALSE,
          gridcolor = TC_GRID, zeroline = FALSE,
          range     = c(0, x_max),
          tickfont  = list(family = "Georgia", size = 11)
        ),
        yaxis = list(
          title = list(
            text = "% of records at high or medium severity (lethality)",
            font = list(size = 11, family = "Georgia")
          ),

          showline   = TRUE, linecolor = "#888888", linewidth = 1.2,
          mirror     = FALSE,
          ticksuffix = "%",
          gridcolor  = TC_GRID, zeroline = FALSE,
          tickfont   = list(family = "Georgia", size = 11)
        ),
        shapes = list(
          list(type = "line",
               x0 = avg_x, x1 = avg_x, y0 = 0, y1 = 100,
               line = list(color = "#CCCCCC", width = 1, dash = "dot")),
          list(type = "line",
               x0 = 0, x1 = x_max, y0 = avg_y, y1 = avg_y,
               line = list(color = "#CCCCCC", width = 1, dash = "dot"))
        ),
        annotations = list(

          list(x = x_max * 0.95, y = max(chart2$pct_high_med),
               text = "<b>High prevalence<br>High severity</b>",
               showarrow = FALSE, xanchor = "right",
               font = list(size = 9, color = "#DDDDDD", family = "Georgia")),
          list(x = 5,  y = 5,
               text = "<b>Low prevalence<br>Low severity</b>",
               showarrow = FALSE, xanchor = "left",
               font = list(size = 9, color = "#DDDDDD", family = "Georgia")),

          list(x = x_max * 0.5, y = -8,
               text = "Bubble size = total number of high/medium severity threat records",
               showarrow = FALSE, xanchor = "center",
               font = list(size = 9.5, color = "#888888", family = "Georgia")),

          list(
            x = chart2$n_spp[chart2$broad_short == "Invasive species"],
            y = chart2$pct_high_med[chart2$broad_short == "Invasive species"] - 6,
            text      = "<b style='color:#E64514'>The silent killer</b>",
            showarrow = TRUE, arrowhead = 2, arrowsize = 0.8,
            arrowcolor = TC_ORANGE, ax = 45, ay = 35,
            font = list(size = 10.5, color = TC_ORANGE, family = "Georgia")
          )
        ),
        plot_bgcolor  = "white",
        paper_bgcolor = "white",
        margin     = list(l = 70, r = 20, t = 20, b = 75),
        hoverlabel = HOVER_STYLE
      ) |>
      config(displayModeBar = FALSE)
  })
  
  
  # ── Chart 3: heatmap ──────────────────────────────────────────────────────
  output$p3 <- renderPlotly({
    
    grp_order <- chart3_grp_order
    
    mat <- chart3 |>
      filter(group %in% grp_order)
    

    if (input$p3_group != "all") {
      mat <- mat |> filter(group == input$p3_group)
      grp_order <- input$p3_group
    }
    
    mat <- mat |>
      mutate(
        group       = factor(group,       levels = grp_order),
        broad_short = factor(broad_short, levels = THREAT_ORDER)
      ) |>
      arrange(group, broad_short)
    

    plot_ly(
      mat,
      x          = ~broad_short,
      y          = ~group,
      z          = ~pct,
      type       = "heatmap",
      colorscale = list(
        list(0.0, "#FFFFFF"),
        list(0.3, "#9ECAE1"),
        list(0.65, "#2171B5"),
        list(1.0,  "#08306B")
      ),
      text          = ~tip,
      hovertemplate = "%{text}<extra></extra>",
      showscale  = TRUE,
      colorbar   = list(
        title     = list(text = "% high/med severity",
                         font = list(size = 10, family = "Georgia")),
        ticksuffix = "%",
        len = 0.7, thickness = 12
      ),
      xgap = 2, ygap = 2
    ) |>
      layout(
        xaxis = list(
          title    = "", tickangle = -35,
          tickfont = list(size = 10, family = "Georgia"), showgrid = FALSE
        ),
        yaxis = list(
          title     = "",
          tickfont  = list(size = 10, family = "Georgia"),
          showgrid  = FALSE, autorange = "reversed"
        ),
        plot_bgcolor  = "white",
        paper_bgcolor = "white",
        margin     = list(l = 110, r = 70, t = 12, b = 95),
        hoverlabel = HOVER_STYLE
      ) |>
      config(displayModeBar = FALSE)
  })
  
  # ── Chart 4: invasive subcategory horizontal stacked bar ──────────────────
  output$p4 <- renderPlotly({
    

    if (input$p4_group == "all") {
      c4 <- chart4
    } else {
      c4 <- chart4 |>
        filter(group == input$p4_group) |>
        group_by(sub_threat) |>
        mutate(total = sum(n)) |>
        ungroup() |>
        filter(total > 0) |>
        mutate(sub_threat = fct_reorder(sub_threat, total))
    }
    
    p <- ggplot(c4,
                aes(x = sub_threat, y = n, fill = group, text = tip)) +
      geom_bar(stat = "identity", position = "stack", width = 0.70) +
      coord_flip() +
      scale_fill_manual(values = GRP_PAL, name = NULL) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
      labs(x = NULL,
           y = "Threatened species affected (high or medium severity)") +
      theme_minimal(base_size = 11) +
      theme(
        text               = element_text(colour = TC_DARK),
        panel.grid.minor   = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = TC_GRID, linewidth = 0.35),

        axis.line.x        = element_line(colour = "#AAAAAA", linewidth = 0.6),
        legend.position    = "bottom",
        legend.text        = element_text(size = 9),
        plot.background    = element_rect(fill = "white", colour = NA),
        panel.background   = element_rect(fill = "white", colour = NA)
      )
    
    ggplotly(p, tooltip = "text") |>
      layout(
        legend     = list(orientation = "h", xanchor = "center",
                          x = 0.5, y = -0.13),
        margin     = list(l = 160, r = 20, t = 10, b = 65),
        hoverlabel = HOVER_STYLE
      ) |>
      config(displayModeBar = FALSE)
  })
  
  # ── Chart 5: threat multiplicity 100%-stacked bar ─────────────────────────
  output$p5 <- renderPlotly({
 
    c5 <- chart5 |>
      mutate(
        group = if (input$p5_sort == "total") {
          fct_reorder(group, total, .desc = TRUE)
        } else {
          factor(group, levels = rev(sort(unique(as.character(group))))) 
        },
        tip = paste0(
          "<b>", group, " \u2014 ", band, "</b><br>",
          n, " species (", pct, "%)"
        )
      )
    
    p <- ggplot(c5,
                aes(x = group, y = pct, fill = band, text = tip)) +
      geom_bar(stat = "identity", position = "stack", width = 0.70) +
      coord_flip() +

      scale_fill_manual(values = BAND_PAL,
                        name   = "Simultaneous broad\nthreat categories") +
      scale_y_continuous(
        labels = function(x) paste0(x, "%"),
        breaks = seq(0, 100, 25),
        expand = expansion(mult = c(0, 0.02))
      ) +
      labs(x = NULL, y = "Percentage of threatened species") +
      theme_minimal(base_size = 11) +
      theme(
        text               = element_text(colour = TC_DARK),
        panel.grid.minor   = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = TC_GRID, linewidth = 0.35),

        axis.line.x        = element_line(colour = "#AAAAAA", linewidth = 0.6),
        legend.position    = "bottom",
        legend.text        = element_text(size = 9),
        legend.title       = element_text(size = 9),
        plot.background    = element_rect(fill = "white", colour = NA),
        panel.background   = element_rect(fill = "white", colour = NA)
      )
    
    ggplotly(p, tooltip = "text") |>
      layout(
        legend     = list(orientation = "h", xanchor = "center",
                          x = 0.5, y = -0.18),
        margin     = list(l = 120, r = 20, t = 10, b = 60),
        hoverlabel = HOVER_STYLE
      ) |>
      config(displayModeBar = FALSE)
  })
}

#launching
shinyApp(ui, server)


