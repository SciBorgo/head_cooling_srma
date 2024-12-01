

# RoB2
d <- read_xlsx('data/ROB2.Crossover data.xlsx') %>%
  as.data.frame()

sum(is.na(d))

# Plot summary (weighted)
robvis::rob_summary(data = d,
                    tool = "ROB1", # this is actually a 'generic' plot function, not just RoB1
                    weighted = T,
                    overall = T,
                    colour = c("#f442c8","#bef441","#000000")) # use a colour blind friendly palette

ggsave(filename = 'plot_rob_summary.pdf',
       width = 8,                           
       height = 2.75,                       
       dpi = 1000)

#### End


