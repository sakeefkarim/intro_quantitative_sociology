# ------------------------------------------------------------------------------

# Week 5, Session I: Introduction to ggplot2 (II)
# SOCI 269: Introduction to Quantitative Sociology—Culture and Power 
# Sakeef M. Karim
# Script File
# CC-BY-SA 4.0

# SAME PACKAGES AS LAST WEEK ----------------------------------------------

library(tidyverse)

# Engines behind the data frames:

library(palmerpenguins)
library(gapminder)

# library(demography)
# library(covdata)
# library(cansim)
# library(WDI)

# Themes, Colour Schemes

library(ggthemes)
library(hrbrthemes)
library(see)
library(paletteer)
library(gglgbtq)
library(colorspace)

# Additional packages, geoms, tools

library(lemon)
library(summarytools)
library(skimr)
library(lattice)
library(tinyplot)
library(ggrepel)
library(ggridges)
library(ggtext)
library(ggdist)

# An alternative procedure
# 
# pacman::p_load(
#   ggthemes,
#   hrbrthemes,
#   see,
#   paletteer,
#   gglgbtq,
#   colorspace,
#   lemon,
#   summarytools,
#   skimr,
#   lattice,
#   tinyplot,
#   ggrepel,
#   ggridges,
#   ggtext,
#   ggdist
# )


# ONE NEW PACKAGE ---------------------------------------------------------

install.packages("ggraph")

library(ggraph)

# LOADING LAST WEEK'S DATA ------------------------------------------------

load(url("https://github.com/sakeefkarim/intro_quantitative_sociology/raw/refs/heads/main/data/week%204/week4.RData"))

# REVIEW ------------------------------------------------------------------

ggplot(gapminder|>filter(year == max(year) |
                           year == min(year)), 
       aes(x = log(gdpPercap), y = lifeExp))   +
  facet_wrap(~year) +
  geom_point(aes(colour = continent, 
                 # Sizing plots based on log of population:
                 size = log(pop)), 
             alpha = 0.65)  +
  geom_smooth(colour = "black", 
              alpha = 0.35,
              method = "lm",
              linewidth = 0.5) +
  labs(title = "Relationship Between GDP and Life Expectancy",
       subtitle = "Over 50 Years Apart",
       x = "Log of Per Capita GDP", 
       y = "Life Expectancy in Years", colour = "",
       size = "Log of Population") +
  scale_size_binned(# Range of plot sizes:
    range = c(0.1, 3.5),
    labels = function(x) paste(x, "+")) +
  scale_colour_brewer(palette = "Dark2") +
  theme_bw(base_family = "IBM Plex Sans") + 
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(colour = "grey45"),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 15)),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.key.size = unit(1, "cm"),
        legend.box = "vertical",
        # Push legend title to the bottom:
        legend.title.position = "bottom",
        # Centring legend title:
        legend.title = element_text(hjust = 0.5)) +
  guides(# Rearranging order of legends; colour now appears first.
    colour = guide_legend(order = 1,
                          # Overriding aes - all keys are 
                          # at size = 5.
                          override.aes = list(size = 8))) 

# ADDITIONAL GEOMS -------------------------------------------------------------

# ggridges ---------------------------------------------------------------------
 
ggplot(gapminder |>
       filter(year == max(year),
             !continent == "Oceania"), 
       aes(x = lifeExp, y = fct_rev(continent),
           fill = continent,
           colour = continent)) +
       geom_density_ridges(alpha = 0.35,
                           jittered_points = TRUE) +
       scale_colour_brewer(palette = "Dark2") +
       scale_fill_brewer(palette = "Dark2") +
       theme_ridges() +
       labs(x = "Life Expectancy in 2007", y = "") +
       theme(text = element_text(family = "IBM Plex Sans"),
             legend.position = "none") +
      # Removes all padding around y-axis:
       scale_y_discrete(expand = c(0, 0)) +
      # Removes all padding around x-axis:
       scale_x_continuous(expand = c(0, 0))


# AN IMPORTANT ASIDE: SAVING A PLOT ---------------------------------------


ggridges_plot <- ggplot(gapminder |>
                             filter(year == max(year),
                                    !continent == "Oceania"), 
                           aes(x = lifeExp, y = fct_rev(continent),
                               fill = continent,
                               colour = continent)) +
                      geom_density_ridges(alpha = 0.35,
                                          jittered_points = TRUE) +
                      scale_colour_brewer(palette = "Dark2") +
                      scale_fill_brewer(palette = "Dark2") +
                      theme_ridges() +
                      labs(x = "Life Expectancy in 2007", y = "") +
                      theme(text = element_text(family = "IBM Plex Sans"),
                            legend.position = "none") +
                      # Removes all padding around y-axis:
                      scale_y_discrete(expand = c(0, 0)) +
                      # Removes all padding around x-axis:
                      scale_x_continuous(expand = c(0, 0))

ggsave(ggridges_plot, 
       filename = "ridges_plot.png",
       # In inches;
       height = 8, 
       width = 9,
       # Background colour?
       bg = "white",
       # What kind of file are we producing?
       device = grDevices::png,
       dpi = 300)

# geom_repel() -----------------------------------------------------------------

select_countries |>
  # Zooming on 1970 and 2020
  filter(year %in% c(1970, 2020)) |>
  # Creating a label variable:
  mutate(label = paste(as.character(round(age_dependency, 1)),
                       "per 100")) |>
  ggplot(mapping = aes(x = as_factor(year),
                       y = age_dependency, 
                       group = country, 
                       label = label,
                       colour = country, 
                       fill = country)) +
  facet_rep_wrap(~country, 
                 nrow = 4, 
                 # Repeats axis text for each facet (via lemon):
                 repeat.tick.labels = TRUE) + 
  geom_point(size = 3) + 
  geom_line() +
  # Adding labels to the points:
  geom_label_repel(segment.color = "grey85",
                   colour = "white",
                   # Moving label down:
                   nudge_y = -10, 
                   show.legend = FALSE,
                   size = 4.5,
                   family = "IBM Plex Sans") +
  scale_colour_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  labs(title = "Rising Old Age Dependency",
       subtitle = "In the Last Half Century",
       x = "", 
       y = "Old Age Dependency Ratio",
       caption = "Old Age Dependency =\nRatio of Elderly Population (64+) to Working Age Population (15-64)") +
  theme_classic(base_family = "IBM Plex Sans") +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(colour = "grey50"),
        axis.title.y = element_text(size = 12, margin = margin(r = 15)),
        strip.text = element_text(size = 12),
        plot.caption = element_text(hjust = 0),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 10))

# Networks

ggraph(toy_network, 
       # Kamada-Kawai algorithm:
       layout = "kk") + 
  # Adding layer of edges
  geom_edge_diagonal(mapping = aes(edge_alpha = 0.8), 
                     show.legend = FALSE, color = "lightgrey") + 
  # Adding nodes
  geom_node_point(size = 6,
                  shape = 21,
                  colour = "white",
                  fill = "darkred") +  
  # Adding labels
  geom_node_label(aes(label = name),
                  family = "IBM Plex Sans",
                  nudge_y = -0.1,
                  fill = "darkred",
                  colour = "white",
                  size = 4) +
  # Removing grid lines, axis labels, tickets and so on:
  theme_void() +
  # Creating black background:
  theme(panel.background = element_rect(fill = "black"))



# MORE THEMES, SCHEMES ---------------------------------------------------------

# Colour blind friendly themes -------------------------------------------------

select_countries |>
  pivot_longer(!c(country, year),
               names_to = "indicator",
               values_to = "value") |>
  mutate(indicator = ifelse(str_detect(indicator, "age_"), 
                            "Old Age Dependency",
                            "Fertility Rate")) |>
  ggplot(mapping = aes(x = year, y = value, 
                       colour = country, fill = country)) +
  geom_smooth(alpha = 0.5) +
  scale_x_continuous(breaks = seq(1970, 2020, by = 25)) +
  facet_grid(fct_rev(indicator) ~ country, 
             scales = "free") +
  labs(x = "", y = "") +
  # Colour/fill themes that should be visible to individuals with
  # colour blindness:
  scale_colour_colorblind() +
  scale_fill_colorblind() +
  # Classic theme from the "see" package:
  theme_classic(base_family = "Inconsolata") +
  theme(strip.text.y = element_text(angle = 0),
        panel.spacing = unit(1, "cm"),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(colour = "grey50"),
        axis.title.y = element_text(size = 12, margin = margin(r = 15)),
        strip.text = element_text(size = 12),
        plot.caption = element_text(hjust = 0),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 10))

# gglgbtq ----------------------------------------------------------------------

ggplot(gapminder |>
         filter(year == max(year),
                !continent == "Oceania"), 
       aes(x = lifeExp, y = fct_rev(continent),
           fill = continent,
           colour = continent)) +
  geom_density_ridges(alpha = 0.35,
                      jittered_points = TRUE)  +
  scale_colour_manual(values = palette_lgbtq("rainbow")) +
  scale_fill_manual(values = palette_lgbtq("rainbow")) +
  theme_ridges() +
  labs(x = "Life Expectancy in 2007", y = "") +
  theme(text = element_text(family = "IBM Plex Sans"),
        legend.position = "none") +
  # Removes all padding around y-axis:
  scale_y_discrete(expand = c(0, 0)) +
  # Removes all padding around x-axis:
  scale_x_continuous(expand = c(0, 0))
  # Allows plotting outside of the plot margin/panel:
  # coord_cartesian(clip = "off") 

# For more palettes, run:

# paletteer::palettes_c_names

# OR

# paletteer::palettes_d_names