# how to deploy dashr app:
# https://dashr.plotly.com/deployment

library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(plotly)
library(dplyr)
library(RColorBrewer)
# library(jsonlite)

app <- Dash$new()

weekly_stats <- read.csv(
  "weekly_stats.csv", header = TRUE, stringsAsFactors = FALSE)
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
    
    htmlDiv(
      list(
        htmlH1("Weekly COVID-19 Statistics")
      ),
      style = list(
        "padding-left" = "10px"
      )
    ),
    
    htmlDiv(
      list(
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
        )
      ),
      style = list(
        width = "20%",
        padding = "0px 0px 20px 10px"
      )
    ),
    
    htmlDiv(
      list(
        dccRadioItems(
          id = "radio",
          options = list(list(label = 'Total', value = 'Total'),
                         list(label = 'Rate', value = 'Rate')),
          value = 'Rate',
          labelStyle = list(display = 'inline-block')
        )
      )
    ),
    
    htmlDiv(list(
      dccGraph(
        id = "map",
        # figure = fig,
        config = list(displayModeBar = FALSE)
      )),
      style = list(
        display = 'inline-block', 
        align = "left"
        # height = 400
        # width = '70%',
        # padding = "0 20"
      )
    ),
    
    htmlDiv(list(
      dccGraph(id='graph', config = list(displayModeBar = FALSE))
    ), style = list(
      display = 'inline-block',
      width = 400
    )),
    
    htmlDiv(
      list(
        dccMarkdown(
          '
        By: [D. Kyle Ward](https://dkyleward.netlify.app/)  
        Data Source: [https://covidtracking.com/](https://covidtracking.com/)  
        GitHub: [https://github.com/dkyleward/us-covid-app](https://github.com/dkyleward/us-covid-app)
        '
        )
      ),
      style = list(
        "padding-left" = "10px"
      )
    )
    
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
  ), style = list(
    width = 1100, 
    border = "1px solid black"
  )
  )
)

# # use this callback to see what values are available on hover
# app$callback(
#   output = list(id = "hover-data", property = "children"),
#   params = list(input(id = 'map', property = 'hoverData')),
#   function(hoverData) {
#     return(prettify(toJSON(hoverData),indent = 2))
#   }
# )

# Callback for map
app$callback(
  output = list(id = "map", property = "figure"),
  params = list(
    input(id = "stat-dropdown", property = "value"),
    input(id = "radio", property = "value")
  ),
  function(stat, rate_flag) {
    create_map(stat, rate_flag)
  }
)

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

# Determines which map to create based on UI selection
create_map <- function(stat, rate_flag, df = peak_stats) {
  
  if (stat == "Deaths") {
    map_color <- "Greys"
    
    if (rate_flag == "Total") {
      column <- "peak_deaths"
      map_title <- "Peak Weekly Deaths"
    } else {
      column <- "peak_death_rate"
      map_title <- "Peak Weekly Deaths per 1M Pop"
    }
  } else if (stat == "Hospitalized") {
    map_color <- "Reds"
    
    if (rate_flag == "Total") {
      column <- "peak_hosp"
      map_title <- "Peak Weekly Hospitalized"
    } else {
      column <- "peak_hosp_rate"
      map_title <- "Peak Weekly Hospitalized per 1M Pop"
    }    
  } else if (stat == "Cases") {
    map_color <- "Blues"
    
    if (rate_flag == "Total") {
      column <- "peak_cases"
      map_title <- "Peak Weekly Cases"
    } else {
      column <- "peak_case_rate"
      map_title <- "Peak Weekly Cases per 100 Tests"
    }        
  }
  
  df_sub <- df[, c("state", "state_full", column)]
  colnames(df_sub) <- c("state", "state_full", "value")
  hover_text <- paste0(
    "<br> ", df_sub$state_full,
    "<br> Peak ", rate_flag, ": ", round(df_sub$value, 0)
  )
  
  map <- plot_geo() %>%
    add_trace(
      z = ~df_sub$value,
      colors = map_color,
      hoverinfo = "text",
      text = hover_text,
      span = I(0),
      locations = df_sub$state,
      locationmode = 'USA-states'
    ) %>%
    layout(
      title = list(
        text = map_title,
        y = .95
      ),
      geo = list(
        scope = 'usa',
        projection = list(type = 'albers usa'),
        lakecolor = toRGB('white')
      ),
      autosize = TRUE,
      margin = list(
        l = 0, r = 0, t = 0, b = 0
      )
    ) %>%
    colorbar(
      title = paste0("Peak ", stat)
      # lenmode = "fraction",
      # len = .25
    )
  
  map
}

# Determines which graph to make based on UI selection
create_graph <- function(state, stat, rate_flag, df = weekly_stats) {
  
  # colors
  default_blue <- rgb(31, 119, 180, maxColorValue = 255)
  hospital_red <- rgb(222, 45, 38, maxColorValue = 255)
  death_gray <- rgb(99, 99, 99, maxColorValue = 255)
  
  if (stat == "Deaths") {
    chart_color <- death_gray
    
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
    chart_color <- hospital_red
    
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
    chart_color <- default_blue
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
      type = "bar",
      marker = list(
        color = chart_color
      )
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

  graph
}

# locally, open the app in the browswer using 127.0.0.1:8050
app$run_server(host = '0.0.0.0', port = Sys.getenv('PORT', 8050))