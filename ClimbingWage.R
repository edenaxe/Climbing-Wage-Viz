### Load required libraries ###

library(tidyverse)


### Load cleaned up version of the raw Climbing Salary Data from Reddit ###

Climbing_Wage <- read.csv("FTClimbingSalary_Clean.csv") %>%
  # Add count column and a state + count (y-axis label) column 
  mutate(Count = map_int(.x = State_Clean, 
                         .f = ~table(State_Clean)[.]),
         State = case_when(
           Count > 2 ~ paste0(State_Clean, " (N = ", Count, ")"),
           Count <= 2 ~ "Other (N = 15)"))



### Create the graph ###

CW_Plot <- Climbing_Wage %>%
  # Group by "State" label column and find min/mean/max for each group
  group_by(State) %>%
  summarise(Min = min(Primary_Clean), 
            Mean = mean(Primary_Clean), 
            Max = max(Primary_Clean)) %>%
  # Round numeric columns and reorder "State" by maximum wage value
  mutate(across(.cols = 2:4, .fns = round, digits = 2),
         State = fct_reorder(State, Max)) %>%
  # Create the ggplot
  ggplot() +
  # Generate a gray segment that spans from min to max value for each group
  geom_segment(aes(x = State, 
                   xend = State, 
                   y = Min, 
                   yend = Max), 
               color = "gray", 
               size = 2) +
  # Add points along the segment where colors correspond to the primary job type
  geom_point(data = Climbing_Wage,
             aes(x = State, 
                 y = Primary_Clean, 
                 color = Main_Clean,
                 # Text element will only be used for the interactive plotly version (ignore warning)
                 text = paste0("State: ", State_Clean, 
                               "<br>Primary Job: ", Main_Clean,
                               "<br>Experience: ", Experience_Clean, " year(s)",
                               "<br>Hourly Wage: $",  Primary_Clean)),
             size = 3) +
  # Customize the wage axis 
  scale_y_continuous(limits = c(5, 40), 
                     breaks = (seq(from = 5, to = 40, by = 5)),
                     labels = c(paste0("$",seq(from = 5, to = 40, by = 5)))) +
  # Flip the coordinates
  coord_flip() +
  # Adjust the color palette to brewer Set2
  scale_color_brewer(palette = "Set2") +
  # Make thematic changes with classic as a starting point
  theme_classic() +
  theme(axis.line.x = element_line(color = "#3b3d3d", size = 1, linetype = "solid"),
        axis.line.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 13),
        panel.background = element_rect(fill = "#f5f1e6"),
        panel.grid.major.x = element_line(linetype = "dashed", color = "white"),
        text = element_text(size = 12, family = "serif", color = "#3b3d3d")) +
  # Add/amend labels
  labs(title = "Climbing Salaries",
       subtitle = "By Primary Job & State",
       color = "Primary Job")

CW_Plot


### Export/save the plot ###

ggsave("CW_Plot.png", units = "in", width = 11, height = 6, dpi = 350)
dev.off()



### Create an interactive version ### 

# Generate the interactive version using plotly and call the tooltip from text in geom_point
plotly::ggplotly(CW_Plot, tooltip = "text") %>%
  # Remove unwanted buttons
  plotly::config(modeBarButtonsToRemove = c("pan2d", "autoScale2d", "zoom2d", "zoomIn2d", "zoomOut2d")) %>% 
  # Remove plotly logo from mode bar
  plotly::config(displaylogo = FALSE) %>% 
  # Change the cursor from crosshair to default
  htmlwidgets::onRender("function(el, x) {Plotly.d3.select('.cursor-crosshair').style('cursor', 'default')}") 
