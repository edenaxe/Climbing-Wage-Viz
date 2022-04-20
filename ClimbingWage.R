library(tidyverse)


### Load cleaned up version of the raw Climbing Salary Data from Reddit
Climbing_Wage <- read.csv("FTClimbingSalary_Clean.csv") %>%
  # Add count column and a state + count (y-axis label) column 
  mutate(Count = map_int(.x = State_Clean, 
                         .f = ~table(State_Clean)[.]),
         State_N = case_when(
           Count > 2 ~ paste0(State_Clean, " (N = ", Count, ")"),
           Count <= 2 ~ "Other (N = 15)"))


### Create the lollipop chart
Climbing_Wage %>%
  # Group by "State_N" label column and find min/mean/max for each group
  group_by(State_N) %>%
  summarise(Min = min(Primary_Clean), 
            Mean = mean(Primary_Clean), 
            Max = max(Primary_Clean)) %>%
  # Round numeric columns and reorder "State_N" by maximum wage value
  mutate(across(.cols = 2:4, .fns = round, digits = 2),
         State_N = fct_reorder(State_N, Max)) %>%
  # Create the ggplot
  ggplot() +
  # Generate a gray segment that spans from min to max value for each group
  geom_segment(aes(x = State_N, 
                   xend = State_N, 
                   y = Min, 
                   yend = Max), 
               color = "gray", 
               size = 2) +
  # Add points along the segment where colors correspond to the primary job type
  geom_point(data = Climbing_Wage,
             aes(x = State_N, 
                 y = Primary_Clean, 
                 color = Main_Clean),
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


### Export/save the plot
ggsave("CW_lollipop.png", units = "in", width = 11, height = 6, dpi = 350)
dev.off()
