setwd(relative_work());

let R = list.files("../R", pattern = "*.R");
let txt = sapply(R, filepath -> readText(filepath));

writeLines(txt, con = "marker.txt");
