setwd(relative_work());

# a helper script for combine all R script files as a single text file
# for run deepseek analysis

let R = list.files("../R", pattern = "*.R");
let txt = sapply(R, filepath -> readText(filepath));

writeLines(txt, con = "marker.txt");
