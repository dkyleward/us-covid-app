# how to deploy dashr app:
# https://dashr.plotly.com/deployment

library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(plotly)
# library(jsonlite)

app <- Dash$new()

weekly_stats <- read.csv("weekly_stats.csv", header = TRUE, stringsAsFactors = FALSE)
peak_stats <- weekly_stats %>%
  group_by(state) %>%
  mutate(
    peak_death_rate = max(death_rate, na.rm = TRUE),
    # Due to errors in the data (or early weeks with little testing),
    # some weeks can show really high positive test rates (even over 100%).
    # Remove these to provide a more accurate representation in the map.
    positive_rate = ifelse(positive_rate >= 100, 0, positive_rate),
    positive_rate = ifelse(
      totalTestResultsIncrease < .10 * max(totalTestResultsIncrease, na.rm = TRUE),
      0, positive_rate
    ),
    peak_case_rate = max(positive_rate, na.rm = TRUE),
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
            label = "Peak Death Rate (deaths per 1 million population)",
            value = "Death Rate"
          ),
          list(
            label = "Peak Hospitalized Rate (weekly average per 1 million population)",
            value = "Hospitalized Rate"
          ),
          list(
            label = "Peak Case Rate (positive cases per 100 tests)",
            value = "Case Rate"
          )
        ),
        value = 'Death Rate'
      )), style = list(width = "49%", padding = "0px 0px 20px 0px")
    ),
    
    # htmlDiv(list(
    #   dccRadioItems(
    #     id = "radio",
    #     options = list(list(label = 'Total', value = 'Total'),
    #                    list(label = 'Rate', value = 'Rate')),
    #     value = 'Rate',
    #     labelStyle = list(display = 'inline-block')        
    #   )
    # )),
    
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
    input(id = "stat-dropdown", property = "value")
  ),
  function(hoverData, value) {
    state <- hoverData$points[[1]]$location
    create_graph(state, value)
  }
)

# Callback for map
app$callback(
  output = list(id = "map", property = "figure"),
  params = list(input(id = "stat-dropdown", property = "value")),
  function(value) {
    create_map(value)
  }
)

# Determines which map to create based on UI selection
create_map <- function(value) {
  
  if (value == "Case Rate") map <- map_case_rate()
  if (value == "Hospitalized Rate") map <- map_hosps()
  if (value == "Death Rate") map <- map_deaths()
  
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
      title = "Peak Case Rate by State",
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
      title = "Peak Hospitalized Rate by State",
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
      title = "Peak Death Rate by State",
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
create_graph <- function(state, value) {
  
  if (value == "Case Rate") graph <- plot_case_rate(state)
  if (value == "Hospitalized Rate") graph <- plot_hosps(state)
  if (value == "Death Rate") graph <- plot_deaths(state)
  
  graph
}

# Plots new case rate of a single state
plot_case_rate <- function(state, df = weekly_stats) {
  
  single_state_df <- df[df$state == state, ]
  
  list(
    data = list(list(
      x = single_state_df$week,
      y = single_state_df$positive_rate, 
      type = "bar"
    )),
    layout = list(
      title = paste0("New Cases per 100 Tests (", state, ")"),
      yaxis = list(
        title = "New Cases (per 100 tests)",
        range = c(0, 100)
      ),
      xaxis = list(title = NA)
    )
  )   
}

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
