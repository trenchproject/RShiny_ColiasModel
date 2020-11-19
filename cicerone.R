
guide_map <- Cicerone$
  new()$
  step(
    el = "viz-wrapper",
    title = "Visualization",
    description = "Welcome to our RMBL phenology visualization tool. Notice there are three tabs on the top. We will go over them one by one. Click next to get started."
  )$
  step(
    el = "mymap_gg",
    title = "Plot",
    description = HTML("The map shows the population growth rate of Colias butterflies in western Colorado in 2020. Brighter color represents a greater growth rate.
                       Try clicking anywhere on the map that is colored.")
  )$
  step(
    el = "info",
    title = "Values",
    description = "It shows the elevation and the population growth rate at that specific location."
  )$
  step(
    el = "labels-wrapper",
    title = "Labels",
    description = "You can show and hide the location names. Turn them off for now."
  )$
  step(
    el = "layer-wrapper",
    title = "Switch layers",
    description = "Hit the switch once and see what happens. Click next."
  )$
  step(
    el = "main",
    title = "Topography",
    description = HTML("The map just shows the topography, which was covered by the data layers. Sometimes, it is useful to understand the geometry of the area.
                       Click on the switch again to show the data back and move forward.")
  )$
  step(
    el = "metric-wrapper",
    title = "Adding metric",
    description = HTML("Let's see some other variables that are related to butterflies fitness.<br> 
                       Add <b>Body temperature</b> while keeping the population growth rate on. ")
  )$
  step(
    el = "facet-wrapper",
    title = "Facets",
    description = HTML("Make sure the facet is at <b>Wing absorptivity</b> for now. This allows the display of multiple values of absorptivity at once.<br>
                       Hit next to add some absorptivities.")
  )$
  step(
    el = "abs-wrapper",
    title = "Adding values",
    description = "Try adding 0.55 and 0.7 by simply clicking them from the list."
  )$
  step(
    el = "mymap_gg",
    title = "How did the map change?",
    description = HTML("Now, we have maps showing population growth rate and body temperature for each wing absorptivity we picked.<br>
                       You can see that the population growth rate is greater and body temperature is somewhat higher as absorptivity increases.")
  )$
  step(
    el = "year-wrapper",
    title = "Slider",
    description = "You can use this slider to show data for different years."
  )$
  step(
    el = "gen-wrapper",
    title = "Generation",
    description = "You can see how it changes between generations as well."
  )$
  step(
    el = "viz-wrapper",
    title = "End of tour",
    description = "That's it for the tour! Switch around the parameters and get the visualization you want!"
  )


guide_plot <- Cicerone$
  new()$
  step(
    el = "panel-wrapper",
    title = "Make a plot!",
    description = HTML("This platform allows for a flexible plot creation by dragging the arrowed variables. 
                       Try dragging <b>Year</b> to <b>x axis</b>, <b>Absorptivity</b> to <b>Color</b>, and <b>Generation</b> to <b>Facets</b> and hit next.")
  )$
  step(
    el = "widgetInput",
    title = "New sliders",
    description = "Notice that new sliders popped up to specify what data we want to plot. Keep them as they are for now."
  )$
  step(
    el = "plot-wrapper",
    title = "Plot",
    description = HTML("Now there's a plot as well! As we specified, year is the x-axis and we see a legend on the right on absorptivity and a number '1' on the top that represents generaion.
                       Let's add more data to the plot.")
  )$
  step(
    el = "variables-wrapper",
    title = "Variables",
    description = "Try adding absorptivity of 0.7 and the second generation and hit next."
  )$
  step(
    el = "trend-wrapper",
    title = "Trend line",
    description = "Turn on the trend line here."
  )$
  step(
    el = "plot-wrapper",
    title = "Updated plot",
    description = HTML("Now, we see two plots, each with two colors and two trend lines. 
                       Population growth rate for individuals with absorptivity of 0.4 is likely to increase while the rate for those with absorptivity of 0.7 is expected to go down in both generations.")
  )$
  step(
    el = "viz-wrapper",
    title = "End of tour",
    description = "That's it for the tour. Go ahead and move around the arrows and change the Y axes to plot what you want!"
  )