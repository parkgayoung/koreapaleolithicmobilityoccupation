
korean_archaeological_site_locations <-
  readr::read_csv(here::here("analysis/data/raw_data/korean_palaeolithic_site_locations.csv"))

library(ggplot2)

# check the basic look
ggplot(korean_archaeological_site_locations) +
  aes(x = long_dd,
      y = lat_dd) +
  geom_point()


library(ggmap)

# bounding box
# 39.534214, 124.159154 ... 39.657415, 129.138604
# 33.694662, 123.491142 ... 34.338454, 130.569658

map <-
  get_stamenmap(bbox = c(left = 123,
                         bottom = 33,
                         right = 	131,
                         top = 39),
                zoom = 10)

library(ggrepel)
#map with site names next to the points
#ggmap(map)  +
#  geom_point(data = korean_archaeological_site_locations,
#                         aes(long_dd ,
#                             lat_dd),
#                         colour = "red",
#                         size = 2) +
#  geom_label_repel(data = korean_archaeological_site_locations,
#                   aes(long_dd ,
#                       lat_dd,
#                       label = site_name),
#                   size = 2)
# use numbering for site name, then the caption has the key to
# numbers and names







# Put index number, rather than site name

korean_archaeological_site_locations <- tibble::rowid_to_column(korean_archaeological_site_locations, "ID")

map_index<-
ggmap(map)  +
  geom_point(data = korean_archaeological_site_locations,
             aes(long_dd ,
                 lat_dd),
             colour = "red",
             size = 2) +
  geom_text_repel(data = korean_archaeological_site_locations,
                   aes(long_dd ,
                       lat_dd,
                       label = ID),
                   size = 3)

library(tidyverse)
index_table <- korean_archaeological_site_locations %>%
  select(ID, site_name)


# create a table
table_index <- data.frame(Index = korean_archaeological_site_locations$ID,
                         Site = korean_archaeological_site_locations$site_name)

# print(table_index)

#save a table
library(here)
#write.csv(table_index, file=here("analysis/figures/001-Map-site-name.csv", row.names = FALSE))

library(gridExtra)
library(grid)
library(gtable)
mytheme <- gridExtra::ttheme_minimal(
  core = list(fg_params=list(cex = 0.6)),
  colhead = list(fg_params=list(cex = 0.5)),
  rowhead = list(fg_params=list(cex = 0.25)))

g <- tableGrob(table_index, rows = NULL, theme = mytheme)
g <- gtable_add_grob(g,
                     grobs = rectGrob(gp = gpar(fill = NA, lwd = 1)),
                     t = 1, b = nrow(g), l = 1, r = ncol(g))

#g$heights <- unit(rep(5/nrow(g), nrow(g), nrow(g)), "cm")

grid.newpage()
grid.draw(g)
dev.off()

#grid.arrange(map_index, g, ncol=2)

library(cowplot)
ggdraw(map_index) +
  draw_plot(g,
            .123, .124,
            .35, .38)

ggsave(here("analysis/figures/001-site-map.png"))



