ca <- commandArgs(TRUE)
files <- list.files("sections", pattern = ".Rmd", full.names = TRUE)
text <- suppressWarnings(do.call(c, sapply(files, readLines)))

if (!requireNamespace("stringr", quietly = TRUE))
    install.packages("stringr")

pkgs <- c(
    stringr::str_match(text, "library\\((.+)\\)")[,2],
    stringr::str_match(text, "([a-zA-Z0-9]+)::")[,2]
)
pkgs <- unique(pkgs[!is.na(pkgs)])
if (length(ca)) saveRDS(pkgs, file = ca[1], version = 2)
pkgs <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]

if (length(pkgs))
    install.packages(pkgs)
