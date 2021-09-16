library(tidyverse)
library(ggalt)
max <- 200
min <- 120
media <- 160

intervalo <- data.frame(ic = "intervalo confianza",limite_inferior=min,media=media,limite_superior=max)
ggplot(data = intervalo) +
  geom_dumbbell(aes(y = ic,
                    x = limite_inferior,
                    xend = limite_superior),
                size = 1.5,
                color="#b2b2b2",
                size_x=3,
                size_xend = 3,
                colour_x = "red",
                colour_xend = "blue")  +
  geom_dumbbell(aes(y = ic,
                 x = media,
                 xend = media),
             size = 1.5,
             size_x=3,
             colour_x = "darkgreen") +
  labs(y="",x="") +
  geom_text(color="black", size=3, vjust=2.5,
            aes(y = ic, x=limite_inferior, label=limite_inferior))+
  geom_text(aes(y = ic, x=limite_superior, label=limite_superior),
            color="black", size=3, vjust=2.5) +
  geom_text(color="black", size=3, vjust=-2.5, hjust = +0.1,
            aes(y = ic, x=limite_inferior, label="limite inferior"))+
  geom_text(aes(y = ic, x=limite_superior, label="limite superior"),
            color="black", size=3, vjust=-2.5, hjust=+0.85) +
  geom_text(aes(y = ic, x=media, label="media"),
            color="black", size=3, vjust=-2.5) +
  geom_text(color="black", size=3, vjust=2.5,
            aes(y = ic, x=media, label=media))

