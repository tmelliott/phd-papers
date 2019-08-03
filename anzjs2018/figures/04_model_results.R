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
    summarize(
        cpu = sum(cpu),
        wall = sum(wall), 
        timestamp = first(timestamp), 
        nvehicles = first(nvehicles),
        n_particles = first(n_particles), 
        gps_error = first(gps_error), 
        system_noise = first(system_noise)
    ) %>%
    mutate(what = "all")
tsmry <- times %>% bind_rows(ttot) %>%
    filter(timestamp >= trange[1] & timestamp <= trange[2]) %>%
    group_by(sim, what) %>%
    summarize(
        cpu = mean(cpu), 
        wall = mean(wall),
        n_particles = first(n_particles), 
        gps_error = first(gps_error), 
        system_noise = first(system_noise)
    ) %>%
    ungroup() #%>%
    #filter(what %in% olvls)
olvls <- sort(unique(tsmry$what)) #c("all", "predicting ETAs", "updating vehicle states", "writing ETAs to protobuf feed")

# ggplot(tsmry) +
#     geom_point(aes(n_particles, cpu, color = what)) +
#     ylab("Wall time (seconds)")

ordlvls <- olvls[c(2, 5, 6, 4, 3, 7, 1)]
lvls <- c(
    "Load Vehicle Data",
    "Update Vehicle Information",
    "Update Vehicle States", 
    "Update Network States",
    "Predict ETAs", 
    "Write ETAs to file", 
    "Total"
)
tdat <- bind_rows(times, ttot) %>% filter(what %in% olvls) %>%
    mutate(what = factor(what, levels = ordlvls, labels = lvls)) %>%
    group_by(what, n_particles)


# Wall timings
pdf('figures/04_model_results_timing.pdf', width = 6, height = 4, onefile = FALSE)
# gridExtra::grid.arrange(
# ggplot(tdat %>% summarize(cpu.hat = mean(cpu), cpu.var = sd(cpu))) +
#     geom_pointrange(aes(n_particles, cpu.hat,
#         ymin = cpu.hat - cpu.var, ymax = cpu.hat + cpu.var, 
#         shape = what)) +
#     theme(legend.position = "none") +
#     xlab("Number of particles") +
#     ylab("Wall Time (seconds)") +
#     labs(shape = ""),
# ggplot(tdat %>% summarize(wall.hat = mean(wall), wall.var = sd(wall))) +
#     geom_pointrange(aes(n_particles, wall.hat,
#         ymin = wall.hat - wall.var, ymax = wall.hat + wall.var, 
#         shape = what)) +
#     xlab("Number of particles") +
#     ylab("CPU Time (seconds)") +
#     labs(shape = ""),
#     nrow = 1, widths = c(3, 4))
# dev.off()

# ggplot(tsmry %>% group_by(what, n_particles) %>% 
#         summarize(wall.hat = mean(wall/1000), wall.var = sd(wall/1000)) %>%
#         ungroup() %>%
#         mutate(what = factor(what, levels = ordlvls, labels = lvls))) +
#     geom_pointrange(aes(n_particles, wall.hat,
#         ymin = wall.hat - wall.var, ymax = wall.hat + wall.var, 
#         shape = what)) +
#     xlab("Number of particles") +
#     ylab("Total Time (seconds)") +
#     labs(shape = "Program component")
    
figdat <- tsmry %>% 
    group_by(what, n_particles) %>% 
    summarize(wall.hat = mean(wall/1000), wall.var = sd(wall/1000)) %>%
    ungroup() %>%
    mutate(what = factor(what, levels = ordlvls, labels = lvls)) %>%
    filter(what %in% lvls[c(3, 5, 6, 7)])

egg::ggarrange(
    ggplot(figdat, aes((as.factor(n_particles)))) +
        geom_point(aes(y = wall.hat)) +
        geom_linerange(aes(ymin = 0, ymax = wall.hat)) +
        facet_grid(~what) +
        xlab("Number of particles") +
        ylab("Average iteration time (s)") + 
        coord_flip() +
        theme_minimal(),
    ggplot(figdat, aes((as.factor(n_particles)))) +
        geom_point(aes(y = 1000 * wall.hat / n_particles)) +
        geom_linerange(aes(ymin = 0, ymax = 1000 * wall.hat / n_particles)) +
        facet_grid(~what) +
        xlab("Number of particles") +
        ylab("Standardised time (ms / particle)") + 
        coord_flip() +
        theme_minimal(),
    nrow = 2
)
dev.off()



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

# ggplot(res %>% filter(dist_to_path < 5), aes(dist_to_path)) + geom_density()

pdf('figures/04_model_results_dist.pdf', width = 6, height = 3)
ggplot(res %>% filter(dist_to_path < 20 & dist_to_path > 0), aes(dist_to_path)) + 
    geom_density(fill = "gray") +
    xlab("Distance to path (m)") + ylab("Density") +
    theme_minimal()
dev.off()

sims <- res %>% filter(dist_to_path < 20) %>% 
    group_by(n_particles, gps_error, system_noise) %>%
    summarize(
        Neff = mean(Neff, na.rm = TRUE),
        p_bad = mean(bad_sample), 
        n = n()
    )

pdf('figures/04_model_results_neff.pdf', width = 7.5, height = 4, onefile = FALSE)
egg::ggarrange(
    ggplot(sims, aes(gps_error, Neff / n_particles, 
        group = system_noise,
        lty = as.factor(system_noise))) +
        geom_path() +
        geom_point() +
        facet_grid(~n_particles, 
            labeller = as_labeller(function(x) paste("N =", x))) +
        labs(lty = "System noise") +
        theme_minimal() +
        xlab("GPS error (m)") +
        ylab(expression(bar(N)[eff]/N)),
    ggplot(sims, aes(gps_error, p_bad, 
        group = system_noise,
        lty = as.factor(system_noise))) +
        geom_path() +
        geom_point() +
        facet_grid(~n_particles, 
            labeller = as_labeller(function(x) paste("N =", x))) +
        labs(lty = "System noise") +
        theme(legend.position = "none") +
        theme_minimal() +
        xlab("GPS error (m)") +
        ylab("Degeneration rate"),
    # ggplot(sims, aes(system_noise*100, Neff / n_particles, 
    #     group = gps_error,
    #     lty = as.factor(gps_error))) +
    #     geom_path() +
    #     geom_point() +
    #     facet_grid(~n_particles) +
    #     labs(lty = "GPS error (m)") +
    #     # scale_x_continuous(breaks = c(0, 0.025, 0.05))
    #     xlab(expression(paste("System noise (x", 10^-2, ")"))) +
    #     ylab(expression(bar(N)[eff]/N)),
    nrow = 2
)
dev.off()

# pdf('figures/04_model_results_neff.pdf', width = 6, height = 4)
# sims %>% 
#     group_by(n_particles, gps_error, system_noise) %>%
#     summarize(Neff.hat = mean(Neff, na.rm = TRUE)) %>%
#     ggplot(aes(as.factor(gps_error))) +
#     geom_point(aes(y = Neff.hat)) +
#     geom_linerange(aes(ymin = 0, ymax = Neff.hat)) +
#     facet_grid(n_particles ~ system_noise, scales = "free_y") +
#     # scale_x_continuous(
#     #     breaks = 1:4, 
#     #     labels = levels(as.factor(sims$gps_error))
#     # ) +
#     theme(panel.grid.major.x = element_blank()) +
#     xlab("GPS Error (m)") +
#     ylab("Average effective sample size") +
#     labs(shape = "Number of particles") +
#     theme_minimal()
#     # coord_flip()

# dev.off()





# pdf('figures/04_model_results_degen.pdf', width = 7.5, height = 3, onefile = FALSE)
# egg::ggarrange(
#     ggplot(sims, aes(gps_error, p_bad, 
#         group = system_noise,
#         lty = as.factor(system_noise))) +
#         geom_path() +
#         geom_point() +
#         facet_grid(~n_particles) +
#         labs(lty = "System noise") +
#         xlab("GPS error (m)") +
#         ylab(expression(bar(N)[eff]/N)),
#     ggplot(sims, aes(system_noise*100, p_bad, 
#         group = gps_error,
#         lty = as.factor(gps_error))) +
#         geom_path() +
#         geom_point() +
#         facet_grid(~n_particles) +
#         labs(lty = "GPS error (m)") +
#         # scale_x_continuous(breaks = c(0, 0.025, 0.05))
#         xlab(expression(paste("System noise (x", 10^-2, ")"))) +
#         ylab(expression(bar(N)[eff]/N)),
#     nrow = 2
# )
# # ggplot(sims %>% group_by(n_particles, gps_error, system_noise) %>%
# #         summarize(p_bad = mean(bad_sample), n = n())) + 
# #     geom_pointrange(aes(as.numeric(as.factor(gps_error)) + as.numeric(as.factor(n_particles)) / 10 - 0.25, 
# #         100*p_bad,
# #         ymin = 100 * (p_bad - 2*sqrt(p_bad * (1 - p_bad) / n)),
# #         ymax = 100 * (p_bad + 2*sqrt(p_bad * (1 - p_bad) / n)),
# #         shape = as.factor(n_particles))) +
# #     facet_grid(~system_noise) +
# #     scale_x_continuous(breaks = 1:4, labels = levels(as.factor(sims$gps_error))) +
# #     theme(panel.grid.major.x = element_blank()) +
# #     xlab("GPS Error (m)") +
# #     ylab("Degeneration rate (% of iterations)") +
# #     labs(shape = "Number of particles")
#     # geom_point(aes(100 * Neff / n_particles, 100*p_bad, shape = as.factor(system_noise))) +
#     # facet_grid(gps_error ~ n_particles) +
#     # xlab("Effective Sample Size (% of N)") + ylab("Degeneration Rate (% of Iterations)") +
#     # labs(shape = "System noise")
# # ggplot(sims %>% group_by(n_particles, gps_error, system_noise) %>%
# #         summarize(p_bad = mean(bad_sample))) + 
# #     geom_col(aes(as.factor(n_particles), y = p_bad, fill = as.factor(system_noise)), position = "dodge") +
# #     facet_grid( ~ gps_error) +
# #     xlab("Numer of particles") + ylab("Degeneration Rate") +
# #     labs(fill = "System Noise")
# dev.off()




get_nw_times <- function(sim) {
    siminfo <- strsplit(sim, "_")[[1]][-1]
    if (grepl("e", siminfo[3])) siminfo[3] <- format(as.numeric(siminfo[3]), scientific = FALSE)
    siminfo <- as.numeric(gsub("-", ".", siminfo))
    read_csv(file.path("../../transitr/simulations", sim, "segment_states.csv"),
        col_names = c("segment_id", "timestamp", "mean", "var"), col_types = "cinn") %>%
        mutate(timestamp = as.POSIXct(timestamp, origin = "1970-01-01"),
               segment_id = factor(segment_id),
               sim = sim, n_particles = siminfo[1], gps_error = siminfo[2], system_noise = siminfo[3])
}

get_all_nw_times <- function() {
    sims <- list.files("../../transitr/simulations", pattern = "sim_")
    sims <- sims[sapply(sims, function(s) file.exists(file.path("../../transitr/simulations", s, "timings.csv")))]
    lapply(sims, get_nw_times) %>% bind_rows
}

if (file.exists("nwtimes.rda")) {
    load("nwtimes.rda")
} else {
    nwtimes <- get_all_nw_times()
    save(nwtimes, file = "nwtimes.rda")
}

segids <- table(nwtimes$segment_id) %>% sort %>% tail(10) %>% names

## plot just one segment
sdata <- nwtimes %>% #filter(segment_id %in% segids) %>%
    group_by(n_particles, gps_error, system_noise) %>%
    do((.) %>% filter(timestamp > min((.)$timestamp))) %>%
    ungroup %>% group_by(segment_id) %>% 
    do((.) %>% mutate(tt.sd = sd(mean, na.rm = TRUE))) %>% 
    ungroup %>% group_by(n_particles, gps_error, system_noise, segment_id) %>%
    summarize(varp = sd(mean, na.rm = TRUE) / first(tt.sd)) %>%
    ungroup %>% group_by(n_particles, gps_error, system_noise) %>%
    summarize(varp.mean = mean(varp, na.rm = TRUE), varp.sd = sd(varp, na.rm = TRUE) / sqrt(n()))

pdf('figures/04_model_results_times.pdf', width = 7.5, height = 2.2, onefile = FALSE)
ggplot(sdata, aes((gps_error), varp.mean)) +
    geom_linerange(
        aes(
            ymin = varp.mean - 2*varp.sd, 
            ymax = varp.mean + 2*varp.sd
        )
    ) +
    geom_point() + 
    geom_path(aes(group = system_noise, lty = as.factor(system_noise))) +
    facet_grid(~n_particles, 
        labeller = as_labeller(function(x) paste("N =", x))) +
    labs(lty = "System noise") +
    theme_minimal() +
    xlab("GPS error (m)") +
    ylab("Relative variance")
dev.off()

# ## old
# ggplot(sdata) + 
#     geom_path(aes(timestamp, mean, colour = as.factor(n_particles))) +
#     geom_linerange(aes(timestamp, ymin = mean - var, ymax = mean + var, colour = as.factor(n_particles))) +
#     facet_grid(gps_error ~ system_noise)

# ggplot(sdata) + 
#     geom_path(aes(timestamp, mean, colour = as.factor(gps_error))) +
#     geom_linerange(aes(timestamp, ymin = mean - 2*var, ymax = mean + 2*var, colour = as.factor(gps_error))) +
#     facet_grid(n_particles ~ system_noise)

# ggplot(sdata) + 
#     geom_path(aes(timestamp, mean, colour = as.factor(system_noise))) +
#     geom_linerange(aes(timestamp, ymin = mean - 2*var, ymax = mean + 2*var, colour = as.factor(system_noise))) +
#     facet_grid(n_particles ~ gps_error)

# ## "how many SDs away from overall mean"
# ggplot(sdata) + 
#     geom_path(aes(timestamp, (mean - overall.mean) / var, colour = as.factor(n_particles))) +
#     facet_grid(gps_error ~ system_noise)

# ggplot(sdata) + 
#     geom_path(aes(timestamp, (mean - overall.mean) / var, colour = as.factor(gps_error))) +
#     facet_grid(n_particles ~ system_noise)

# ggplot(sdata) + 
#     geom_path(aes(timestamp, (mean - overall.mean) / var, colour = as.factor(system_noise))) +
#     facet_grid(n_particles ~ gps_error)




# nwtimes.smry1 <- nwtimes %>% filter(segment_id %in% segids) %>% 
#     group_by(n_particles, gps_error, system_noise, segment_id) %>%
#     summarize(mean.tt = mean(mean, na.rm = TRUE), var.tt = sd(mean, na.rm = TRUE),
#         sd.time = sd(var, na.rm = TRUE), mean.var = mean(var, na.rm = TRUE)) %>% ungroup

# nwtimes.smry <- nwtimes.smry1 %>% group_by(n_particles, gps_error, system_noise) %>%
#     summarize(mean = mean(sd.time, na.rm = TRUE), sd = mean(mean.var, na.rm = TRUE/mean.tt)) %>% ungroup()

# smry2 <- 
#     nwtimes %>% filter(segment_id %in% segids) %>%
#     group_by(n_particles, gps_error, system_noise, segment_id) %>%
#     summarize(sd.time = sd(var, na.rm = TRUE), mean.var = mean(var, na.rm = TRUE)) %>%
#     group_by(n_particles, gps_error, system_noise) %>%
#     summarize(mean = mean(sd.time, na.rm = TRUE), sd = mean(mean.var, na.rm = TRUE)) %>% 
#     ungroup()

# ggplot(nwtimes %>% filter(segment_id %in% segids) %>% 
#     group_by(n_particles, gps_error, system_noise, segment_id) %>%
#     do((.) %>% mutate(mbar = mean - mean(mean, na.rm = TRUE))) %>% 
#     summarize(mean = mean(mbar^2, na.rm = TRUE),
#               sd = sd(mbar^2, na.rm = TRUE))) +
#     geom_point(aes(as.factor(system_noise), log(sd), color = as.factor(n_particles))) +
#     facet_grid(~gps_error)

# ggplot(nwtimes.smry1) +
#     geom_point(aes(mean.tt, log(var.tt/sqrt(n_particles)),
#         shape = as.factor(system_noise))) +
#     facet_grid(n_particles ~ gps_error) +
#     xlab("Mean travel time (seconds)") +
#     ylab("Travel time uncertainty") +
#     labs(color = "GPS Error (m)", shape = "System noise")

# pdf('figures/04_model_results_times.pdf', width = 10, height = 2)
# ggplot(nwtimes.smry) +
#     geom_point(aes(sd, as.factor(gps_error), 
#         shape = as.factor(system_noise))) +
#     facet_grid(~n_particles,scales="free_x") +
#     ylab("GPS Error (m)") +
#     xlab("Travel time uncertainty") +
#     labs(color = "GPS Error (m)", shape = "System noise")
# dev.off()

# library(lme4)

# nwtimes %>% filter(segment_id %in% segids) %>%
#     lmer(var ~ mean + n_particles + as.factor(gps_error) + (1 | segment_id), data = .) %>%
#     summary
