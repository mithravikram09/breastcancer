# Arrange all panels in a grid
Fig2 <- grid.arrange(com_mean, com_worst,
                     con_mean, con_worst,
                     con1_mean, con1_worst,
                     sym_mean, sym_worst,
                     fra_mean, fra_worst, ncol = 2)

# Save the figure (increase width/height for bigger panels if needed)
ggsave(filename = "Fig2.jpeg",  
       plot = Fig2,
       width = 1920,    # increase as needed for bigger panels
       height = 2400,   # increase as needed for bigger panels
       dpi = 150,             
       units = "px",          
       limitsize = F)
