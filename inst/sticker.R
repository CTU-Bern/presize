library(ggplot2)
library(emojifont)
library(scales)
library(hexSticker)

load.fontawesome()

arrow <- data.frame(x = c(1, 1.55, 1.41, 1.4),
                    y = c(.9, 1.4, 1.4, 1.55))
arrowhead <- data.frame(x = c(1,  1.15, 1.05, 1.01),
                        y = c(.9, .95,  .97, 1.05))


icon <- ggplot() +
  geom_text(aes(x = c(1), y = c(1), label = fontawesome('fa-bullseye')),
            family = "fontawesome-webfont",
            size = 90) +
  # geom_text(aes(x = c(1.1), y = c(1), label = fontawesome('fa-arrow-left')),
  #           family = "fontawesome-webfont",
  #           size = 45, col = "grey") +
  geom_polygon(data = arrow, aes(x = x, y = y), fill = CTUtemplate::unibeRed()) +
  geom_polygon(data = arrowhead, aes(x = x, y = y), fill = CTUtemplate::unibeRed()) +
  xlim(0,2) +
  ylim(0,2) +
  theme_void() + theme_transparent()



s <- sticker(icon, package="",
             s_x=1, s_y=1.15, s_width=1.8, s_height=2,
             filename="man/figures/logo.png",
             h_fill = colorRampPalette(c("white", CTUtemplate::unibeRed()))(6)[3],
             h_color = CTUtemplate::unibeRed(),
             h_size = 2,
             url = "presize",
             u_size = 12,
             u_x = 1,
             u_y = 0.15
             )
s



icons <- emojifont:::fontawesome_data$aliases


