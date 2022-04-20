library(tidyverse)

# Load cleaned up version of the raw Climbing Salary Data from Reddit
Climbing_Wage <- read.csv("~/FTClimbingSalary_Clean.csv") 


# Create a summary table for line segment and y-axis label 
# Shows total range by state and provides count of responses by state
CW_Table <- Climbing_Wage %>%
  mutate(Diff = Primary_Clean-MinWage_Clean) %>%
  group_by(State_Clean) %>%
  summarise(Count = n(), 
            Min = min(Primary_Clean), 
            Mean = mean(Primary_Clean), 
            Max = max(Primary_Clean),
            MeanDiff = mean(Diff),
            MinWage = mean(MinWage_Clean)) %>%
  mutate(State_N = case_when(
    Count > 2 ~ paste0(State_Clean, " (N = ", Count, ")"),
    Count <= 2 ~ "Other (N = 15)"))


# Create the lollipop chart
CW_Table %>%
  group_by(State_N) %>%
  summarise(Min = min(Min), 
            Mean = mean(Mean), 
            Max = max(Max)) %>%
  mutate(across(.cols = 2:4, .fns = round, digits = 2),
         State_N = fct_reorder(State_N, Max)) %>%
  arrange(desc(Mean)) %>%
  ggplot() +
  geom_segment(aes(x = State_N, 
                   xend = State_N, 
                   y = Min, 
                   yend = Max), 
               color = "gray", 
               size = 2) +
  geom_point(data = left_join(Climbing_Wage, CW_Table, by = "State_Clean"),
             aes(x = State_N, 
                 y = Primary_Clean, 
                 color = Main_Clean),
                 size = 4) +
  scale_color_brewer(palette = "Set2") +
  ylim(0, 45) +
  coord_flip() +
  scale_y_continuous(limits = c(5, 40), 
                     breaks = (seq(from = 5, to = 40, by = 5)),
                     labels = c(paste0("$",seq(from = 5, to = 40, by = 5)))) +
  theme_classic() +
  theme(axis.line.x = element_line(color = "#3b3d3d", size = 1, linetype = "solid"),
        axis.line.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(face = "bold", size = 15),
        plot.subtitle = element_text(size = 15),
        panel.background = element_rect(fill = "#f5f1e6"),
        panel.grid.major.x = element_line(linetype = "dashed", color = "white"),
        text = element_text(size = 14, family = "serif", color = "#3b3d3d")) +
  labs(title = "Climbing Salaries",
       subtitle = "By Primary Job & State",
       color = "Primary Job")

dev.off()
