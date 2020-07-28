ca <- commandArgs(trailingOnly = TRUE)

f <- ca[1]

txt <- readLines(f)
from <- grep("\\maketitle", txt, fixed = TRUE) + 1L
to <- grep("\\end{document}", txt, fixed = TRUE) - 1L
txt <- txt[from:to]

# convert @cite:Paper_2000,Paper2_2001; to
# `\cite{Paper_2000, Paper2_2001}`{=latex}

regx <- "@([a-z]+):([a-zA-Z0-9,_\\\\]+);"
m <- do.call(rbind, stringr::str_match_all(txt, regx))
if (nrow(m)) {
    for (i in seq_len(nrow(m))) {
        rep <- paste0("\\", m[i, 2], "{", m[i, 3], "}")
        rep <- gsub("[\\]+_", "_", rep)
        cat("\n ", m[i, 2], "->", rep)
        txt <- stringr::str_replace_all(txt, stringr::fixed(m[i, 1]), rep)
    }
    cat("")
}

writeLines(txt, f)
