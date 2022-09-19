title <- "#08306b"
subtitle <- "#525252"
caption <- "#36383A"

axis_text <- "#4A4C45"
axis_title <- "black"

strip_background <- "#e0e0e0"
background <- "#f0f0f0"




main_font <- "Lato"
axis_title_font <- NULL
axis_line_col <- "#403d39"

strip_text <- "#2E2E2E"


base_size <- 14

theme_set(
    theme(
        text = element_text(family = main_font, size = base_size),
        plot.title = element_text(
            face = "bold",
            colour = title,
            size = base_size * 1.4,
            hjust = 0,
            margin = margin(t = 5, r = 0, b = 5, l = 0)
        ),
        plot.subtitle = element_text(
            colour = subtitle,
            size = base_size * 1,
            hjust = 0,
            margin = margin(t = 0, r = 0, b = 5, l = 5)
        ),
        plot.caption = element_text(
            colour = caption,
            hjust = 1,
            size = 0.5 * base_size, 
            margin = margin(t = 7, r = 5, b = 5, l = 5)
        ),
        plot.caption.position = "panel",
        panel.background = element_rect(fill = background, colour = NA),
        plot.background = element_rect(fill = background, colour = NA),
        panel.grid = element_blank(),
        axis.title = element_text(
            size = base_size ,
            family = axis_title_font,
            color = "black",
            vjust = 1,
            margin = margin(t = 0, r = 0, b = 0, l = 0)
        ),
        axis.text = element_text(
            size = base_size * 0.7,
            colour = axis_text
        ),
        axis.line = element_line(
            colour = "black"
        ),
        # axis.line = element_blank(),
        axis.ticks = element_line(
            size = 0.6,
            colour = axis_line_col
        ),
        strip.background = element_rect(
            fill = strip_background,
            colour = axis_line_col, 
            size = 0.8
        ),
        strip.text = element_text(
            size = 0.6 * base_size,
            margin = margin(t = 2, r = 3, b = 2, l = 3),
            colour = strip_text
            ),
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5), 
        legend.background = element_rect(fill = background, colour = NA)
    )
)

# p <- plot_dat2 |> 
#     ggplot(aes(ar, laun_per)) +
#     geom_hline(yintercept = 1, lty = 2) +
#     geom_line(data = plot_dat2 |> rename(atv = atvinnugreinahopur),
#               aes(group = atv), alpha = 0.1) +
#     geom_line(aes()) +
#     scale_x_continuous(breaks = seq(2008, 2020, by = 2)) +
#     scale_y_log10(labels = function(x) percent(x - 1),
#                   breaks = c(0.8, 1, 1.2, 1.4)) +
#     scale_colour_brewer(type = "div", palette = "RdYlBu") +
#     facet_wrap("atvinnugreinahopur") +
#     coord_cartesian() +
#     labs(x = NULL,
#          y = NULL,
#          title = "Breyting á launakostnaði fyrirtækja sem hlutfall af tekjum miðað við 2008",
#          subtitle = "Sýnt fyrir fyrirtæki með 10 eða fleiri starfsfólk",
#          caption = "Mynd eftir @bggjonsson út frá gögnum Hagstofu um afkomu fyrirtækja eftir atvinnugrein og stærð")
# 
# p
# 
# ggsave(plot = p, filename = "Figures/fyrirtaeki.png",
#        width = 8, height = 0.621 * 8, scale = 1.5, bg = background)

p <- plot_dat |>
    ggplot(aes(ar, value)) +
    geom_col(aes(fill = flokkur),
             position = "stack",
             colour = background,
             width = 1,
             size = 0.3) +
    scale_x_continuous(breaks = 2007:2020) +
    scale_y_continuous(labels = isk) +
    scale_fill_manual(
        values = c(
            plot_dat |> arrange(flokkur) |> distinct(colour) |> pull(colour)
        )
    ) +
    coord_cartesian(expand = FALSE) +
    guides(fill = guide_legend(reverse = T)) +
    theme(legend.position = "top") +
    labs(x = NULL, y = NULL, fill = NULL,
         title = "Eigið fé íslenskra stjórnmálaflokka",
         subtitle = "Árið 2007 átti Sjálfstæðisflokkur 100% alls eigin fjár flokkanna en 2020 á hann 43,5%",
         caption = "Mynd eftir @bggjonsson unnin úr ársreikningum stjórnmálaflokka frá Ríkisendurskoðun")

p

ggsave(plot = p, filename = "Figures/eigidfe.png",
       width = 8, height = 0.55 * 8, scale = 1.3, bg = "white")