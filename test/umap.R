require(umap);

let data = read.csv(relative_work("aa55ac5e-7a48-45fd-9fb5-7e013805b247.csv"), row.names =1, check.names = FALSE);
let scatter = umap(data, dimension = 9, numberOfNeighbors = 64);

scatter = as.data.frame(scatter$umap, labels = scatter$labels);

write.csv(scatter, file = relative_work("umap.csv"));

