# canada <- 
#   data  %>%
#   filter(geo == "Canada") %>%
#   mutate(
#     quad = factor(
#       quad,
#       levels = c(
#         "VM Women+", 
#         "non-VM Women+", 
#         "VM Men+", 
#         "non-VM Men+"
#       )
#     )
#   ) %>%
#   ggplot() +
#   geom_segment(
#     data = line_segments,
#     aes(x = xstart, y = ystart, xend = xend, yend = yend),
#     color = "#E7D6C5"
#   ) +
#   geom_rect(
#     aes(xmin = 0, ymin = 0, xmax = income, ymax = education, fill = quad),
#     color = NA, alpha = 0.5
#   ) +
#   scale_y_continuous(
#     limits = c(0, 110),
#     breaks = c(0, 50, 100),
#     labels = scales::percent_format(scale = 1),
#     position = "left"
#   ) +
#   scale_x_continuous(
#     limits = c(0, 110000),
#     breaks = c(0, 50000, 100000),
#     labels = scales::dollar_format()
#   ) +
#   scale_fill_manual(
#     values = pal[c(1:2, 4:5)],
#     name = NULL,
#     guide = guide_none()
#   ) +
#   theme_void() +
#   theme(
#     aspect.ratio = 1,
#     text = element_text(color = "#4D4519", family = "oxygen"),
#     plot.background = element_rect(fill = pal[3], color = NA)
#     # axis.text = element_text(size = 8, margin = margin(l = 2, t = 2))
#   )