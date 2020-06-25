# how to deploy dashr app:
# https://dashr.plotly.com/deployment

library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(plotly)
library(dplyr)
# library(jsonlite)

app <- Dash$new()

weekly_stats <- read.csv("weekly_stats.csv", header = TRUE, stringsAsFactors = FALSE)
peak_stats <- weekly_stats %>%
  rename(case_rate = positive_rate) %>%
  group_by(state) %>%
  mutate(
    peak_deaths = max(deathIncrease, na.rm = TRUE),
    peak_death_rate = max(death_rate, na.rm = TRUE),
    peak_cases = max(positiveIncrease, na.rm = TRUE),
    # Due to errors in the data (or early weeks with little testing),
    # some weeks can show really high positive test rates (even over 100%).
    # Remove these to provide a more accurate representation in the map.
    case_rate = ifelse(case_rate >= 100, 0, case_rate),
    case_rate = ifelse(
      totalTestResultsIncrease < .10 * max(totalTestResultsIncrease, na.rm = TRUE),
      0, case_rate
    ),
    peak_case_rate = max(case_rate, na.rm = TRUE),
    peak_hosp = max(hospitalizedCurrently, na.rm = TRUE),
    peak_hosp = ifelse(is.infinite(peak_hosp), NA, peak_hosp),
    peak_hosp_rate = max(hosp_rate, na.rm = TRUE),
    peak_hosp_rate = ifelse(is.infinite(peak_hosp_rate), NA, peak_hosp_rate)
  ) %>%
  slice(1) %>%
  select(-week, -week_id)



app$layout(
  htmlDiv(list(
    
    htmlDiv(list(
      htmlH1("Weekly COVID-19 Statistics")
    )),
    
    htmlDiv(list(
      dccDropdown(
        id = "stat-dropdown",
        options = list(
          list(
            label = "Deaths",
            value = "Deaths"
          ),
          list(
            label = "Hospitalized",
            value = "Hospitalized"
          ),
          list(
            label = "Cases",
            value = "Cases"
          )
        ),
        value = 'Deaths'
      )), style = list(width = "49%", padding = "0px 0px 20px 0px")
    ),
    
    htmlDiv(list(
      dccRadioItems(
        id = "radio",
        options = list(list(label = 'Total', value = 'Total'),
                       list(label = 'Rate', value = 'Rate')),
        value = 'Rate',
        labelStyle = list(display = 'inline-block')
      )
    )),
    
    htmlDiv(list(
      dccGraph(
        id = "map",
        figure = fig,
        config = list(displayModeBar = FALSE)
      )),
      style = list(
        display = 'inline-block', 
        width = '49%',
        padding = "0 20"
      )
    ),
    
    htmlDiv(list(
      dccGraph(id='graph', config = list(displayModeBar = FALSE))
    ), style = list(
      display = 'inline-block', width = '30%'
    ))
    
    # htmlDiv(list(
    #   dccMarkdown(
    #     "
    #     ** Test **
    #     Hover over a state
    #     "
    #   ),
    #   htmlPre(
    #     id = "hover-data"
    #   )
    # ))
  ))
  
)

# # use this callback to see what values are available on hover
# app$callback(
#   output = list(id = "hover-data", property = "children"),
#   params = list(input(id = 'map', property = 'hoverData')),
#   function(hoverData) {
#     return(prettify(toJSON(hoverData),indent = 2))
#   }
# )

# Callback for graph
app$callback(
  output = list(id = "graph", property = "figure"),
  params = list(
    input(id = "map", property = "hoverData"),
    input(id = "stat-dropdown", property = "value"),
    input(id = "radio", property = "value")
  ),
  function(hoverData, stat, rate_flag) {
    state <- hoverData$points[[1]]$location
    create_graph(state, stat, rate_flag)
  }
)

# Callback for map
app$callback(
  output = list(id = "map", property = "figure"),
  params = list(input(id = "stat-dropdown", property = "value")),
  function(stat) {
    create_map(stat)
  }
)

# Determines which map to create based on UI selection
create_map <- function(stat) {
  
  # # determine which column to graph
  # column <- case_when(
  #   stat == "Deaths" & rate_flag == "Total" ~ "peak_deaths",
  #   stat == "Deaths" & rate_flag == "Rate" ~ "peak_death_rate",
  #   stat == "Hospitalized" & rate_flag == "Total" ~ "peak_hosp",
  #   stat == "Hospitalized" & rate_flag == "Rate" ~ "peak_hosp_rate",
  #   stat == "Cases" & rate_flag == "Total" ~ "peak_cases",
  #   stat == "Cases" & rate_flag == "Rate" ~ "peak_case_rate",
  #   TRUE ~ ""
  # )
  
  if (stat == "Cases") map <- map_case_rate()
  if (stat == "Hospitalized") map <- map_hosps()
  if (stat == "Deaths") map <- map_deaths()
  
  map
}

map_case_rate <- function() {
  
  plot_geo() %>%
    add_trace(
      z = ~peak_stats$peak_case_rate,
      hoverinfo = "text",
      text = paste0(
        "<br> ", peak_stats$state_full,
        "<br> Peak Rate: ", round(peak_stats$peak_case_rate, 1)
      ),
      span = I(0),
      locations = peak_stats$state,
      locationmode = 'USA-states'
    ) %>%
    layout(
      title = "Peak Cases by State",
      geo = list(
        scope = 'usa',
        projection = list(type = 'albers usa'),
        lakecolor = toRGB('white')
      )
    ) %>%
    colorbar(
      title = paste0(
        "Peak Cases",
        "<br> (per 100 tests)"
      )
    )
}

map_hosps <- function() {
  
  plot_geo() %>%
    add_trace(
      z = ~peak_stats$peak_hosp_rate,
      hoverinfo = "text",
      text = paste0(
        "<br> ", peak_stats$state_full,
        "<br> Peak Rate: ", round(peak_stats$peak_hosp_rate, 1)
      ),
      span = I(0),
      locations = peak_stats$state,
      locationmode = 'USA-states'
    ) %>%
    layout(
      title = "Peak Hospitalized by State",
      geo = list(
        scope = 'usa',
        projection = list(type = 'albers usa'),
        lakecolor = toRGB('white')
      )
    ) %>%
    colorbar(
      title = paste0(
        "Peak Hospitalized",
        "<br> (per million)"
      )
    )
}

map_deaths <- function() {
  
  plot_geo() %>%
    add_trace(
      z = ~peak_stats$peak_death_rate,
      hoverinfo = "text",
      text = paste0(
        "<br> ", peak_stats$state_full,
        "<br> Peak Rate: ", round(peak_stats$peak_death_rate, 1)
      ),
      span = I(0),
      locations = peak_stats$state,
      locationmode = 'USA-states'
    ) %>%
    layout(
      title = "Peak Deaths by State",
      geo = list(
        scope = 'usa',
        projection = list(type = 'albers usa'),
        lakecolor = toRGB('white')
      )
    ) %>%
    colorbar(
      title = paste0(
        "Peak Deaths",
        "<br> (per million)"
      )
    )
}

# Determines which graph to make based on UI selection
create_graph <- function(state, stat, rate_flag, df = weekly_stats) {
  
  if (stat == "Deaths") {
    if (rate_flag == "Total") {
      column <- "deathIncrease"
      graph_title <- "Weekly Deaths Total"
      y_title <- "Deaths"
      y_range <- NA
    } else {
      column <- "death_rate"
      graph_title <- "Weekly Deaths per 1M Pop"
      y_title <- "Deaths per 1M Pop"
      y_range <- NA
    }
  } else if (stat == "Hospitalized") {
    if (rate_flag == "Total") {
      column <- "hospitalizedCurrently"
      graph_title <- "Weekly Hospitalized Total"
      y_title <- "Hospitalized"
      y_range <- NA
    } else {
      column <- "hosp_rate"
      graph_title <- "Weekly Hospitalized per 1M Pop"
      y_title <- "Hospitalized per 1M Pop"
      y_range <- NA
    }    
  } else if (stat == "Cases") {
    if (rate_flag == "Total") {
      column <- "positiveIncrease"
      graph_title <- "Weekly Cases Total"
      y_title <- "Cases"
      y_range <- NA
    } else {
      column <- "positive_rate"
      graph_title <- "Weekly Cases per 100 Tests"
      y_title <- "Cases per 100 Tests"
      y_range <- c(0, 100)
    }        
  }
  graph_title <- paste0(graph_title, " (", state, ")")
  
  # Determine two y scales. A single scale (with max values from NY) will
  # make it hard to view smaller states.
  national_max_y <- max(df[[column]], na.rm = TRUE)
  df_sub <- df[df$state == state, c("week", column)]
  colnames(df_sub) <- c("week", "value")
  df_sub$value <- round(df_sub$value, 0)
  state_max_y <- max(df_sub$value, na.rm = TRUE)
  max_y <- ifelse(
    state_max_y > national_max_y / 5,
    national_max_y,
    round(national_max_y / 5, -1)
  )
  if (column == "positive_rate") max_y <- 100
  
  graph <- list(
    data = list(list(
      x = df_sub$week,
      y = df_sub$value, 
      type = "bar"
    )),
    layout = list(
      title = graph_title,
      yaxis = list(
        title = y_title,
        range = c(0, max_y)
      ),
      xaxis = list(title = NA)
    )    
  )
  
  # if (stat == "Cases") graph <- plot_case_rate(state, df_sub)
  # if (stat == "Hospitalized") graph <- plot_hosps(state, df_sub)
  # if (stat == "Deaths") graph <- plot_deaths(state, df_sub)
  
  graph
}

# # Plots new Cases of a single state
# plot_case_rate <- function(state, df) {
#   
#   list(
#     data = list(list(
#       x = single_state_df$week,
#       y = single_state_df$case_rate, 
#       type = "bar"
#     )),
#     layout = list(
#       title = paste0("New Cases per 100 Tests (", state, ")"),
#       yaxis = list(
#         title = "New Cases (per 100 tests)",
#         range = c(0, 100)
#       ),
#       xaxis = list(title = NA)
#     )
#   )   
# }

# Plots hospitalizations of a single state
plot_hosps <- function(state, df = weekly_stats) {
  
  single_state_df <- df[df$state == state, ]
  max_y <- max(single_state_df$hosp_rate, na.rm = TRUE)
  max_y <- ifelse(max_y > 450, 950, 450)
  
  list(
    data = list(list(
      x = single_state_df$week,
      y = single_state_df$hosp_rate, 
      type = "bar"
    )),
    layout = list(
      title = paste0("Hospitalized per Million (", state, ")"),
      yaxis = list(
        title = "Hospitalizations",
        range = c(0, max_y)
      ),
      xaxis = list(title = NA)
    )
  )    
}

# This function simply  narrows the state data to a single state and
# creates a plotly figure of daily deaths.
plot_deaths <- function(state, df = weekly_stats) {
  
  single_state_df <- df[df$state == state, ]
  max_y <- max(single_state_df$death_rate, na.rm = TRUE)
  max_y <- ifelse(max_y > 100, 250, 100)
  
  list(
    data = list(list(
      x = single_state_df$week,
      y = single_state_df$death_rate, 
      type = "bar"
    )),
    layout = list(
      title = paste0("Deaths per Million (", state, ")"),
      yaxis = list(
        title = "Deaths",
        range = c(0, max_y)
      ),
      xaxis = list(title = NA)
    )
  )  
}




app$run_server(host = '0.0.0.0', port = Sys.getenv('PORT', 8050))
