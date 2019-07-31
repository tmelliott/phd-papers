library(tidyverse)
library(RSQLite)
library(dbplyr)
library(grid)
library(gridExtra)

gtfs <- dbConnect(SQLite(), file.path("..", "..", "transitr", "fulldata.db"))
route <- gtfs %>% tbl("routes") %>% 
    filter(route_short_name == "110" && route_long_name %like% '%To City%') %>%
    arrange(desc(version)) %>% head(1) %>% collect()
trip <- gtfs %>% tbl("trips") %>% 
    filter(route_id == route$route_id) %>% head(1) %>% collect()
shape <- gtfs %>% tbl("shapes") %>%
    filter(shape_id == trip$shape_id) %>% arrange(shape_pt_sequence) %>% collect() %>%
    mutate(shape_dist_traveled = (.) %>% 
        select(shape_pt_lon, shape_pt_lat) %>%
        as.matrix() %>% geosphere::distGeo() %>% cumsum %>% c(0, .))
dbDisconnect(gtfs)

shapef <- shape %>% filter(shape_dist_traveled > 8000 & shape_dist_traveled < 15000)

p0 <- ggplot() +
    geom_path(aes(shape_pt_lon, shape_pt_lat), data = shapef) +
    coord_fixed(1.6) +
    theme_minimal() +
    theme(panel.grid = element_blank(), axis.text = element_blank()) +
    scale_x_continuous(position = 'top') +
    ylab("Latitude") + xlab("Longitude")
d0 <- ggplot() +
    geom_path(aes(x = xx, y = yy), data = tibble(xx = c(8000, 15000), yy = c(0, 0))) +
    theme_minimal() + 
    theme(panel.grid = element_blank(), axis.text = element_blank()) +
    xlab("Distance Travelled") + ylab("") +
    xlim(8000, 15000)

h <- function(x, shape = shapef) {
    x <- do.call(rbind, lapply(x, function(X) {
        if (X <= min(shapef$shape_dist_traveled))
            return(shapef[1, c('shape_pt_lon', 'shape_pt_lat')])
        if (X >= max(shapef$shape_dist_traveled))
            return(shapef[nrow(shapef), c('shape_pt_lon', 'shape_pt_lat')])
        i <- which(shapef$shape_dist_traveled >= X)[1]
        p1 <- shapef[i, c('shape_pt_lon', 'shape_pt_lat')]
        p2 <- shapef[i + 1, c('shape_pt_lon', 'shape_pt_lat')]
        b <- geosphere::bearing(p1, p2)
        d <- X - shapef$shape_dist_traveled[i]
        geosphere::destPoint(p1, b, d) %>% as.numeric
    }))
    colnames(x) <- c('lon', 'lat')
    as.tibble(x)
}

WIDTH = 5
HEIGHT = 2.5
set.seed(873624)
N <- 20
x <- sample(rnorm(N, 9000, 50), replace = TRUE)

pdf('figures/03_particle_filter_1.pdf', width = WIDTH, height = HEIGHT)
grid.arrange(
    p0 + geom_point(aes(lon, lat), data = h(x)),
    d0 + geom_dotplot(aes(x), data = tibble(x = x), binwidth = 50),
    ncol = 1)
pushViewport(viewport(0.24, 0.45, 0.1, 0.45))
# grid.rect()
grid.segments(0.3, 0.15, 0.75, 0.9, arrow = arrow(length = unit(0.15, "inches")))
grid.roundrect(0.5, 0.5, unit(0.4, "inches"), unit(0.25, 'inches'), gp = gpar(fill = 'white'))
grid.text("h(x)", 0.5, 0.5, gp = gpar(fontface = 3, fontfamily = 'HersheySerif'))
dev.off()


x2 <- x + runif(N, c(10, 20), c(20, 25)) * 120

pdf('figures/03_particle_filter_2.pdf', width = WIDTH, height = HEIGHT)
gridExtra::grid.arrange(
    p1 <- p0 + 
        geom_point(aes(lon, lat), data = h(x), colour = 'gray') +
        geom_point(aes(lon, lat), data = h(x2)),
    d1 <- d0 + 
        geom_dotplot(aes(x), data = tibble(x = x), binwidth = 50, color = 'gray', fill = 'gray') +
        geom_dotplot(aes(x), data = tibble(x = x2), binwidth = 50),
    ncol = 1)
pushViewport(viewport(0.34, 0.3, 0.25, 0.15))
# grid.rect()
grid.segments(0, 0.6, 1, 0.6, arrow = arrow(length = unit(0.15, "inches")))
grid.roundrect(0.5, 0.6, unit(0.4, "inches"), unit(0.25, "inches"), gp = gpar(fill = 'white'))
grid.text("f(x)", 0.5, 0.6, gp = gpar(fontface = 3, fontfamily = 'HersheySerif'))
dev.off()

y <- h(10800) - rnorm(2, 0, c(0.002, 0.001))
pdf('figures/03_particle_filter_3.pdf', width = WIDTH, height = HEIGHT)
gridExtra::grid.arrange(
    p1 + geom_point(aes(lon, lat), data = y, col = "orangered", pch = 4, size = 2, stroke = 1.3),
    d1,
    ncol = 1)
dev.off()

dist <- geosphere::distGeo(y %>% as.matrix, h(x2) %>% as.matrix)
lh <- dexp((dist / 100)^2, 0.5)
wt <- lh / sum(lh)
x3 <- sample(x2, replace = TRUE, prob = wt)

pdf('figures/03_particle_filter_4.pdf', width = WIDTH, height = HEIGHT)
gridExtra::grid.arrange(
    p0 + 
        # geom_point(aes(lon, lat), data = h(x), colour = 'gray') +
        geom_point(aes(lon, lat), data = h(x2), colour = 'gray') +
        geom_point(aes(lon, lat), data = h(x3)) +
        geom_point(aes(lon, lat), data = y, col = "orangered", pch = 4, size = 2, stroke = 1.3),
    d0 + 
        # geom_dotplot(aes(x), data = tibble(x = x), binwidth = 50, color = 'gray', fill = 'gray') +
        geom_dotplot(aes(x), data = tibble(x = x2), binwidth = 50, color = 'gray', fill = 'gray') +
        geom_dotplot(aes(x), data = tibble(x = x3), binwidth = 50),
    ncol = 1)
dev.off()

x4 <- x3 + runif(N, 10, 20) * 120
pdf('figures/03_particle_filter_5.pdf', width = WIDTH, height = HEIGHT)
gridExtra::grid.arrange(
    p0 + 
        geom_point(aes(lon, lat), data = h(x), colour = 'gray') +
        geom_point(aes(lon, lat), data = h(x3), colour = 'gray') +
        geom_point(aes(lon, lat), data = h(x4)),
    d0 + 
        geom_dotplot(aes(x), data = tibble(x = x), binwidth = 50, color = 'gray', fill = 'gray') +
        geom_dotplot(aes(x), data = tibble(x = x3), binwidth = 50, color = 'gray', fill = 'gray') +
        geom_dotplot(aes(x), data = tibble(x = x4), binwidth = 50),
    ncol = 1)
dev.off()

shapef2 <- shapef %>% filter(shape_dist_traveled > 10200 & shape_dist_traveled < 11500)
yl <- extendrange(shapef2$shape_pt_lat, f = 4)
pdf('figures/03_particle_filter_6.pdf', width = WIDTH, height = HEIGHT)
ggplot() +
    geom_path(aes(shape_pt_lon, shape_pt_lat), data = shapef2) +
    coord_fixed(1.6) +
    ylim(yl[1], yl[2]) + 
    theme_minimal() +
    theme(panel.grid = element_blank(), axis.text = element_blank()) +
    scale_x_continuous(position = 'top') +
    ylab("Latitude") + xlab("Longitude") +
    geom_point(aes(lon, lat), data = h(x2[x2 < 11500]), col = 'black') +
    # geom_point(aes(lon, lat), data = h(x2[x2 < 11500][1]), col = 'black') +
    geom_point(aes(lon, lat), data = y, col = "orangered", pch = 4, size = 2, stroke = 1.3) +
    geom_segment(aes(lon, lat, xend = y$lon, yend = y$lat), data = h(x2)[1,], lty = 2)
dev.off()

