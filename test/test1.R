require(marker);

clusters = read.csv("clusters.csv", row.names = 1, check.names = FALSE);
clusters = data.frame(Cluster = clusters$Cluster, row.names = rownames(clusters));
expr = read.csv("aa55ac5e-7a48-45fd-9fb5-7e013805b247.csv", row.names = 1, check.names = FALSE);

merged_data <- merge(expr, clusters, by = "row.names", all.x = TRUE, sort = FALSE)
rownames(merged_data) <- merged_data$Row.names
merged_data$Row.names <- NULL
names(merged_data)[names(merged_data) == "Cluster"] <- "class"

top = limma_filter(merged_data, CON = "class_1", treatment = "class_2", top = 1000, 
                       adj.P.Val = 0.05, logFC = 0);

write.csv(top, file = "limma_sigdiffs.csv");

marker(merged_data, class = c("class_1","class_2"), sel_features = NULL, training_size = 0.7, top_features = 9, save_dir= "./demo")
