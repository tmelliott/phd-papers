library(tidyverse)

times <- do.call(bind_rows, lapply(list.files("../../transitr/simulations", pattern = "sim_", full = TRUE), function(dir) {
    if (!file.exists(file.path(dir, "timings.csv"))) return(NULL)
    sim <- basename(dir)
    siminfo <- strsplit(sim, "_")[[1]][-1]
    if (grepl("e", siminfo[3])) siminfo[3] <- format(as.numeric(siminfo[3]), scientific = FALSE)
    siminfo <- as.numeric(gsub("-", ".", siminfo))
    read_csv(file.path(dir, "timings.csv")) %>%
        mutate(sim = sim, n_particles = siminfo[1], gps_error = siminfo[2], system_noise = siminfo[3],
            timestamp = as.POSIXct(timestamp, origin = "1970-01-01"))
}))

# ggplot(times, aes(x = timestamp)) +
#     geom_point(aes(y = wall/1000, colour = as.factor(n_particles))) +
#     facet_grid(what~., scales = "free_y")
#     # geom_point(aes(y = cpu), colour = "orangered") +

## between 13:45 and 14:00
date <- format(times$timestamp[1], "%Y-%m-%d")
trange <- as.POSIXct(paste(date, c("13:30", "14:00")))

ttot <- times %>% group_by(sim, iteration) %>%
    summarize(cpu = sum(cpu), wall = sum(wall), timestamp = first(timestamp), nvehicles = first(nvehicles),
        n_particles = first(n_particles), gps_error = first(gps_error), system_noise = first(system_noise)) %>%
    mutate(what = "all")
tsmry <- times %>% bind_rows(ttot) %>%
    filter(timestamp >= trange[1] & timestamp <= trange[2]) %>%
    group_by(sim, what) %>%
    summarize(cpu = mean(cpu), wall = mean(wall),
        n_particles = first(n_particles), gps_error = first(gps_error), system_noise = first(system_noise)) %>%
    ungroup() %>%
    filter(what %in% c("all", "predicting ETAs", "updating vehicle states", "writing ETAs to protobuf feed"))

# ggplot(tsmry) +
#     geom_point(aes(n_particles, cpu, color = what)) +
#     ylab("Wall time (seconds)")

lvls <- c("predicting ETAs", "updating vehicle states", "writing ETAs to protobuf feed", "all")
tdat <- bind_rows(times, ttot) %>% filter(what %in% lvls) %>%
    mutate(what = factor(what, levels = lvls)) %>%
    group_by(what, n_particles)


## Wall timings
pdf('figures/04_model_results_timing.pdf', width = 6, height = 3)
gridExtra::grid.arrange(
ggplot(tdat %>% summarize(cpu.hat = mean(cpu), cpu.var = sd(cpu))) +
    geom_pointrange(aes(n_particles, cpu.hat,
        ymin = cpu.hat - cpu.var, ymax = cpu.hat + cpu.var, 
        shape = what)) +
    xlab("Number of particles") +
    ylab("Wall Time (seconds)") +
    labs(shape = ""),
ggplot(tdat %>% summarize(wall.hat = mean(wall), wall.var = sd(wall))) +
    geom_pointrange(aes(n_particles, wall.hat,
        ymin = wall.hat - wall.var, ymax = wall.hat + wall.var, 
        shape = what)) +
    xlab("Number of particles") +
    ylab("CPU Time (seconds)") +
    labs(shape = ""),
    nrow = 1, widths = c(3, 2))
dev.off()

# ## CPU timings
# ggplot(tsmry %>% group_by(what, n_particles) %>% 
#         summarize(wall.hat = mean(wall/1000), wall.var = sd(wall/1000))) +
#     geom_errorbar(aes(n_particles, 
#         ymin = wall.hat - wall.var, ymax = wall.hat + wall.var, 
#         color = what, shape = what), 
#         width = 100) +
#     xlab("Number of particles") +
#     ylab("CPU Time (seconds)")




get_sim_files <- function(sim) {
    if (file.exists(file.path("../../transitr/simulations", sim, "modeleval.rds"))) {
        return(readRDS(file.path("../../transitr/simulations", sim, "modeleval.rds")))
    }
    x <- try({
        siminfo <- strsplit(sim, "_")[[1]][-1]
        if (grepl("e", siminfo[3])) siminfo[3] <- format(as.numeric(siminfo[3]), scientific = FALSE)
        siminfo <- as.numeric(gsub("-", ".", siminfo))
        do.call(bind_rows, 
            lapply(list.files(file.path("../../transitr/simulations", sim, "modeleval"), pattern="vehicle_.*\\.csv", full.names = TRUE), 
                function(x) 
                    read_csv(x, 
                        col_names = c("vehicle_id", "trip_id", "ts", "prior_mse", "posterior_mse", #"sumwt", "varwt",
                                      "post_speed", "prior_speed_var", "posterior_speed_var", "dist_to_path", 
                                      "Neff", "resample", "n_resample", "bad_sample"),
                        col_types = "ccidddddddiii", progress = FALSE) %>%
                    mutate(ts = as.POSIXct(ts, origin = "1970-01-01"))
            )
        ) %>% mutate(sim = sim, n_particles = siminfo[1], gps_error = siminfo[2], system_noise = siminfo[3])
    })
    if (inherits(x, "try-error")) return(NULL)
    saveRDS(x, file.path("../../transitr/simulations", sim, "modeleval.rds"))
    x
}


res <- do.call(bind_rows, lapply(list.files("../../transitr/simulations", pattern = "sim_"), get_sim_files))

ggplot(res %>% filter(dist_to_path < 5), aes(dist_to_path)) + geom_density()

pdf('figures/04_model_results_dist.pdf', width = 6, height = 3)
ggplot(res %>% filter(dist_to_path < 20 & dist_to_path > 0), aes(dist_to_path)) + geom_density()
dev.off()

sims <- res %>% filter(dist_to_path < 20)

pdf('figures/04_model_results_neff.pdf', width = 6, height = 3)
ggplot(sims %>% filter(Neff <= n_particles & Neff > 0 & bad_sample == 0) %>% 
    group_by(n_particles, system_noise, gps_error) %>%
    summarize(Neff = mean(Neff))) + 
    geom_point(aes(x = factor(system_noise), Neff / n_particles * 100, shape = factor(n_particles), colour = factor(gps_error))) +
    labs(x = "System Noise", y = "Effective Sample Size (% of Number of Particles)",
         shape = "Number of Particles", colour = "GPS Error (m)")
dev.off()
