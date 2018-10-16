library(tidyverse)

stops <- tibble(x = c(0, 0, 1, 1, 1.2, 2, 2, 3, 4, 5, 5, 6, 6),
                y = c(2, 1, 2, 1, 0.3, 1.5, 0.8, 1.5, 1.5, 2, 1, 2, 1))

roads <- tibble(id = c(1, 1, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 6, 6, 7, 7, 7, 8, 8, 
                       9, 9, 9, 10, 10, 10, 11, 11, 12, 12, 13, 13,
                       14, 14, 15, 15, 16, 16, 16, 17, 17),
                x = c(0, 1, 0, 1, 1, 1.2, 2, 1, 1.2, 2, 2, 3, 1.2, 2, 2, 2.5, 3, 
                      3, 4, 4, 4.3, 5, 4, 4.3, 5, 5, 6, 5, 6, 2, 3, 3, 4, 3, 4, 4, 4.295, 5,
                      5, 6),
                y = c(2, 2, 1, 1, 2, 1.51, 1.51, 1, 1.49, 1.49, 1.49, 1.49, 0.3, 0.8, 0.8, 1.47, 1.47,
                      1.49, 1.49, 1.51, 1.51, 2, 1.49, 1.49, 1, 2, 2, 1.01, 1.01, 1.51, 1.51,
                      1.51, 1.51, 1.47, 1.47, 1.47, 1.47, 0.975, 0.99, 0.99)) %>%
    mutate(lty = ifelse(id %in% c(6, 7, 15, 16, 17), 2, 
                        ifelse(id %in% c(1, 3, 13, 14, 9, 11), 3, 1)))

p1 <- ggplot() +
    geom_point(aes(x, y), data = stops, size = 5, pch = 19) +
    geom_path(aes(x, y, group = id, lty = as.factor(lty)), data = roads) + 
    theme_void() + theme(legend.position = 'none') +
    coord_fixed(1)

roads2 <- roads %>% filter(!id %in% c(5, 14, 15, 16, 17)) %>%
    group_by(id) %>% do({
        if ((.)$id[1] == 12) {
            (.) %>% mutate(y = 1)
        } else if ((.)$id[1] %in% c(13, 8)) {
            (.) %>% mutate(y = 1.5)
        } else if ((.)$id[1] %in% c(10)) {
            (.) %>% mutate(y = y - 0.01)
        } else {
            (.)
        }
    }) %>% 
    mutate(lwd = ifelse(id %in% c(13, 10, 12), 2, 
                        ifelse(id %in% c(8), 5, 1)))
p2 <- ggplot() +
    geom_point(aes(x, y), data = stops, size = 5, pch = 19) +
    geom_path(aes(x, y, group = id, size = lwd), data = roads2) + 
    theme_void() + theme(legend.position = 'none') +
    scale_size_continuous(range = c(0.5, 2.5)) +
    coord_fixed(1)

ggsave("figures/02_network_segments_1.pdf", p1, device = "pdf", width = 6, height = 2)
ggsave("figures/02_network_segments_2.pdf", p2, device = "pdf", width = 6, height = 2)





library(RSQLite)
library(dbplyr)

DBDIR <- file.path("..", "..", "transitr")

gtfs <- dbConnect(SQLite(), file.path(DBDIR, "fulldata.db"))

## get some routes
vmax <- gtfs %>% tbl("routes") %>% 
    summarize(version = max(version, na.rm = TRUE)) %>% collect %>% pluck('version')
routes_to_use <- c("22N", "22A", "24B", "27W", "30", "70")
routes <- gtfs %>% tbl("routes") %>% 
    filter(route_short_name %in% routes_to_use && version == vmax &&
           (route_long_name %like% '%To City%' || route_long_name %like% '%To Britomart%')) %>%
    select(route_id, route_short_name) %>% collect %>%
    group_by(route_short_name) %>%
    summarize(route_id = first(route_id))

trips <- gtfs %>% tbl("trips") %>% filter(route_id %in% routes$route_id) %>% 
    collect %>% group_by(trip_id) %>% summarize(shape_id = first(shape_id))

shapes <- gtfs %>% tbl("shapes") %>%
    filter(shape_id %in% trips$shape_id) %>%
    arrange(shape_id, shape_pt_sequence) %>%
    collect

stop_ids <- gtfs %>% tbl("stop_times") %>%
    filter(trip_id %in% trips$trip_id) %>%
    select(stop_id) %>% distinct %>% collect %>% pluck('stop_id')

stops <- gtfs %>% tbl("stops") %>% 
    filter(stop_id %in% stop_ids) %>% collect

ggplot(shapes, aes(shape_pt_lon, shape_pt_lat, group = shape_id)) +
    geom_path(alpha = 0.5) +
    geom_point(aes(stop_lon, stop_lat, group = NULL), data = stops,
               pch = 21, fill = "white", size = 1) +
    coord_fixed(ratio = 1.2)+#, xlim = c(174.74, 174.78), ylim = c(-36.88, -36.84)) +
    theme(axis.title = element_blank(), axis.ticks = element_blank(), axis.text = element_blank())
