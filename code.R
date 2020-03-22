library('tidyverse')
library('readxl')
library('janitor')
library('zoo')

states <- read_excel('states_dates.xlsx', 
                     sheet = 2) %>% clean_names() %>% 
  filter(state != 'Unassigned')

states.old <- states %>% group_by(date) %>% 
  summarise(delegates = sum(state_total)) %>% 
  mutate(cumulative = cumsum(delegates),
         cumulative.perc = cumulative/sum(delegates) * 100) %>% 
  mutate(timepd = 'Before postponements')

states.new <- states %>% group_by(date_new) %>% 
  summarise(delegates = sum(state_total)) %>% 
  mutate(cumulative = cumsum(delegates),
         cumulative.perc = cumulative/sum(delegates) * 100) %>% 
  mutate(timepd = 'After postponements') %>% 
  rename(date = date_new)

states.all <- rbind(states.old,
                    states.new)

skyblue1


ggplot(data = states.all,
       aes(x = date,
           y = cumulative.perc,
           color = timepd)) + 
  geom_line(size = 2, alpha = 0.75) + 
  labs(x = '', y = '') +
  scale_colour_manual(values = c('#003399',  'skyblue1')) +
  theme(panel.background = element_rect(fill = "white"), 
        plot.background = element_rect(fill = "white"), 
        axis.ticks.y = element_blank(),
        legend.position = 'top', 
        panel.grid.major.y = element_line(color = 'gray90', size = .3), 
        panel.grid.major.x = element_blank(),
        axis.text = element_text(size = 11), 
        plot.title = element_text(size = 14, face = "bold"),
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 11),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(hjust = -.01, color = 'gray47', size = 9)) + 
  labs(title = "How delegate allocation changes with postponement\nof Democratic primaries in 9* states or territories",
       caption = '\n*As of Mar 21. Assuming Pennsylvania primary postponed to Jun 2 and Puerto Rico to Apr 26\nChart: @christinezhang | Data: FrontloadingHQ, news reports', 
       subtitle = "Cumulative % of delegates allocated by date\n") 

ggsave('plot.png', width = 8, height = 6)



