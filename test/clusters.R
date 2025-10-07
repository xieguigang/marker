require(clustering);

let data = read.csv(relative_work("umap.csv"), row.names = 1, check.names = FALSE);
let clusters = as.data.frame(kmeans(data, centers = 2));

clusters[,"Cluster"] = sprintf("class_%s", clusters$Cluster);

write.csv(clusters, file = relative_work("clusters.csv"));

bitmap(file = relative_work("clusters.png")) {
    plot(as.numeric(clusters$dimension_1), as.numeric(clusters$dimension_2), 
        class = clusters[,"Cluster"], 
        colors = "paper",
        fill = "white",
        point.size = 8
    );
}