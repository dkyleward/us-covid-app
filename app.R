# how to deploy dashr app:
# https://dashr.plotly.com/deployment

library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(plotly)
# library(jsonlite)

app <- Dash$new()

weekly_stats <- read.csv("weekly_stats.csv", header = TRUE, stringsAsFactors = FALSE)
peak_deaths <- weekly_stats %>%
  group_by(state) %>%
  mutate(peak_death_rate = max(death_rate)) %>%
  filter(death_rate == peak_death_rate) %>%
  slice(1)

fig <- plot_geo() %>%
  add_trace(
    z = ~peak_deaths$peak_death_rate,
    hoverinfo = "text",
    # text = state.name,
    text = paste0(
      "<br> ", peak_deaths$state_full,
      "<br> Peak Rate: ", round(peak_deaths$peak_death_rate, 1)
    ),
    span = I(0),
    locations = peak_deaths$state,
    locationmode = 'USA-states'
  ) %>%
  layout(
    geo = list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      lakecolor = toRGB('white')    
    )
  ) %>%
  colorbar(
    title = paste0(
      "Peak Death Rate",
      "<br> (deaths per million)"
    )
  )

app$layout(
  htmlDiv(list(
    
    htmlDiv(list(
      htmlH1("COVID-19")
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
      # dccGraph(id='case_graph', config = list(displayModeBar = FALSE)),
      # dccGraph(id='hosp_graph', config = list(displayModeBar = FALSE)),
      dccGraph(id='death_graph', config = list(displayModeBar = FALSE))
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

# use this callback to see what values are available on hover
app$callback(
  output = list(id = "hover-data", property = "children"),
  params = list(input(id = 'map', property = 'hoverData')),
  function(hoverData) {
    return(prettify(toJSON(hoverData),indent = 2))
  }
)

# Case rate graph
app$callback(
  output = list(id = "case_graph", property = "figure"),
  params = list(input(id = "map", property = "hoverData")),
  function(hoverData) {
    plot_case_rate(hoverData$points[[1]]$location)
  }
)

# Hospitalized graph
app$callback(
  output = list(id = "hosp_graph", property = "figure"),
  params = list(input(id = "map", property = "hoverData")),
  function(hoverData) {
    plot_hosps(hoverData$points[[1]]$location)
  }
)

# Death graph
app$callback(
  output = list(id = "death_graph", property = "figure"),
  params = list(input(id = "map", property = "hoverData")),
  function(hoverData) {
    plot_deaths(hoverData$points[[1]]$location)
  }
)

# Plots new case rate of a single state
plot_case_rate <- function(state, df = weekly_stats) {
  
  single_state_df <- df[df$state == state, ]
  
  list(
    data = list(list(
      x = single_state_df$date,
      y = single_state_df$positive_rate, 
      type = "bar"
    )),
    layout = list(
      # title = "New Case Rate",
      yaxis = list(title = "New Cases (per 1k tests)"),
      xaxis = list(title = NA)
    )
  )   
}

# Plots hospitalizations of a single state
plot_hosps <- function(state, df = weekly_stats) {
  
  single_state_df <- df[df$state == state, ]
  
  list(
    data = list(list(
      x = single_state_df$date,
      y = single_state_df$hospitalizedIncrease, 
      type = "bar"
    )),
    layout = list(
      # title = "Daily Hospitalized",
      yaxis = list(title = "Hospitalizations"),
      xaxis = list(title = NA)
    )
  )    
}

# This function simply  narrows the state data to a single state and
# creates a plotly figure of daily deaths.
plot_deaths <- function(state, df = weekly_stats) {
  
  single_state_df <- df[df$state == state, ]
  max_y <- max(single_state_df$death_rate)
  max_y <- ifelse(max_y > 100, 250, 100)
  
  list(
    data = list(list(
      x = single_state_df$date,
      # y = single_state_df$deathIncrease, 
      y = single_state_df$death_rate, 
      type = "bar"
    )),
    layout = list(
      # title = "Daily Deaths",
      yaxis = list(
        title = "Deaths",
        range = c(0, max_y)
      ),
      xaxis = list(title = NA)
    )
  )  
}


app$run_server()
