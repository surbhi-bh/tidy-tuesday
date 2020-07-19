                                                                                                                                                 
library(readr)
library(ggplot2)

astro <- read.csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-14/astronauts.csv')
head(astro)

astro <- astro[order(astro$year_of_mission),]

a = seq(0, pi, length.out = 59)

# line length
x = 40 * cos(a)
y = 40 * sin(a)

try <- aggregate(name ~ year_of_mission,
          FUN = length,
          data = astro)


abc <- table(astro$year_of_mission, astro$sex)
year_of_mission  <- row.names(abc)
abc <- data.frame(year_of_mission, unclass(abc))

try <- merge(try, abc, by = "year_of_mission")
try <- try[order(-try$year_of_mission),]


# points length
x1 <- try$female * cos(a)
y1 <-  try$female* sin(a)
x2 <- try$male* cos(a)
y2 <- try$male* sin(a)
xy <- data.frame(try, x,y, x1, y1, x2, y2)


# line labels
lab1 <- 41 * cos(a) + 0.2
lab2 <- 41 * sin(a) + 0.5

xy <- data.frame(xy, lab1, lab2)
xy$labnames <- ifelse(xy$year_of_mission %% 2 == 0, xy$year_of_mission, NA)

library(grid)
library(ggfx)

                   
p <- ggplot(xy) +
    geom_segment(aes(x = 0, y = 0, xend = x, yend = y,
                     linetype = "12345678", color = "#A9A9A9")) + 
       #           arrow = arrow(length = unit(0.15, "cm"))) +
    with_blur(geom_point(aes(x1, y1,
                             colour = "#FFD700", size = y1,
                        shape = 8, stroke = 1.5), sigma = y1*5)) +
    with_blur(geom_point(aes(x2, y2,  colour = "navajowhite2",
                             size = y2,
                     shape = 8, stroke = 1.5), sigma  = y2*5))  +
    scale_colour_identity() +
    scale_linetype_identity() +
    scale_shape_identity() +
    geom_text(aes(label = labnames,
                  x = x, y = y, color = "#A9A9A9"),
              hjust = 0.5, vjust = -0.5) +   
    theme_void() +
    theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#414962", colour = NA),
    plot.margin = margin(-5, 2, -10, 1)
    ) +
    annotate("text", x = -30, y = 60,
             label = "Starring in the sky",
             size = 13, family = "Ume P Mincho S3",
             colour = "navajowhite2")  +
    scale_x_continuous(expand = c(0.07, 0)) +
    annotate("text", x = 18,
             y = -2,
             hjust = 0, vjust = 0,
             label = "Source: M. Stavnichuk & T. Corlett (https://doi.org/10.1016/j.lssr.2020.06.003)  \n Graphic: Surbhi Bhatia", size = 2.2,
             colour = "white") +
    annotate("text",
             x = c(0, -10, -20, -30, -40),
             y = -1.3,
              hjust = 0, vjust = 0,
             label = seq(0, 40, 10),
             col = "#A9A9A9") +
        annotate("text", x = -30,
             y = -3,
             hjust = 0, vjust = 0,
             label = "Number of astronauts",
             size = 4.5,
             colour = "#A9A9A9") +
    annotate("text",
             x = -11.5,
             y = 58,
             label = "\nOut of 564 astronauts who travelled to space between 1961-2019, only 11% were women",
             size = 6,
             colour = "#A9A9A9",
             lineheight = 1.1) 

p

ggsave("p.png",  width = 14.55, height = 8.7)
dev.off() 


