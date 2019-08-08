library(tidyverse)

d <- as.factor(2:5)
s <- paste0("2019-01-01 12:", 
        str_pad(2*(1:4), 2, pad = "0"), 
        ":00") %>%
    as.POSIXct

delay <- c(30, 60, 90)
a <- s[1:3] + delay

pdf('figures/02_gtfs_delays.pdf', width = 7, height = 2.5, onefile = FALSE)
egg::ggarrange(
    ggplot() + 
        geom_point(aes(s, d)) +
        xlab("ETA") +
        ylab("Stop Index") +
        theme_classic() +
        # theme(panel.grid = element_blank()) +
        geom_point(aes(a, d[1:3]),
            shape = 4) +
        geom_text(aes(s[4] + delay, d[4],
            label = paste0("hat(A)[", 1:3, "]")),
            parse = TRUE,
            nudge_y = 0.3) +
        geom_segment(aes(a, d[1:3],
            xend = s[4] + delay,
            yend = d[4]),
            lty = 3) +
        geom_point(aes(s[4] + delay, d[4]),
            shape = 21, fill = "white"),
    ggplot() +
        geom_point(
            aes(
                c(s[2], a[-1]),
                c(4.5, 4, 2)
            )
        ) +
        geom_path(
            aes(
                c(s[2], a[2], a[2], a[3], a[3]),
                c(4.5, 3.5, 4, 1.5, 2)
                # as.integer(c(
                #     s[4] - a[1], 
                #     s[4] - a[1] - (a[2] - s[1]),
                #     s[4] - a[2],
                #     s[4] - a[2] - (a[3] - s[2]),
                #     s[4] - a[3]
                # ) / 60)
            ),
            lty = 2
        ) +
        xlab("Time") +
        ylab("ETA (minutes)") +
        theme_classic(),
    nrow = 1,
    widths = c(3, 2)
)
dev.off()
