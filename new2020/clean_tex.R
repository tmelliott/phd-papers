ca <- commandArgs(trailingOnly = TRUE)

f <- ca[1]

txt <- readLines(f)
from <- grep("\\maketitle", txt, fixed = TRUE) + 1L
to <- grep("\\end{document}", txt, fixed = TRUE) - 1L
txt <- txt[from:to]
writeLines(txt, f)
