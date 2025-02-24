#!/usr/bin/env Rscript
# =============================================================================
# 2025Q1-DAAD-sc-spatial course by Genomics UA
# Session M1S13, 2025-02-24
# Author: Oleksandr Petrenko
# =============================================================================

cat("🚀 Script execution started successfully!\n\n")

# [Step 1/6] Setup and Environment Preparation 🛠️
setwd("/research/lab_reiberger/users/opetrenko/tmp")
if (dir.exists("./outputs")) {
  unlink("./outputs", recursive = TRUE)
}
cat("✅ [Step 1/6] Environment setup completed! 🎉\n\n")

# [Step 2/6] Load Required Libraries 📚
suppressMessages(library(Seurat))
suppressMessages(library(Matrix))
suppressMessages(library(dplyr))
library(here)
cat("✅ [Step 2/6] Libraries loaded successfully! 📦\n\n")

# [Step 3/6] Data Loading and Seurat Object Creation 🔍
raw_counts <- read.csv("./annotated_BE1500.csv", row.names = 1)
sparce_matrix <- Reduce(cbind2, lapply(raw_counts, Matrix, sparse = TRUE))
gene_names <- rownames(raw_counts)
cell_names <- names(raw_counts)
dimnames(sparce_matrix) <- list(gene_names, cell_names)
seurat_object <- CreateSeuratObject(counts = sparce_matrix)
seurat_object$celltype <- sapply(colnames(seurat_object@assays$RNA$counts),
                                 function(x) strsplit(x, "_")[[1]][2])
cat("✅ [Step 3/6] Data loaded and Seurat object created successfully! 🔍\n\n")

# [Step 4/6] Pre-processing: Subset, Normalize, PCA, Clustering, UMAP 🧹
seurat_object <- subset(seurat_object, downsample = 1000)
seurat_object <- NormalizeData(seurat_object)
seurat_object <- FindVariableFeatures(seurat_object, selection.method = "vst", nfeatures = 2000)
seurat_object <- ScaleData(seurat_object)
seurat_object <- RunPCA(seurat_object, npcs = 30, verbose = FALSE)
seurat_object <- FindNeighbors(seurat_object, dims = 1:30)
seurat_object <- FindClusters(seurat_object, resolution = 0.5)
seurat_object <- RunUMAP(seurat_object, dims = 1:30)
cat("✅ [Step 4/6] Pre-processing completed successfully! 🔬\n\n")

# [Step 5/6] Differential Expression Analysis to Identify Cluster Markers 🔬
Idents(seurat_object) <- "celltype"
celltype_markers <- FindAllMarkers(seurat_object, only.pos = TRUE, 
                                   min.pct = 0.25, logfc.threshold = 0.25)
celltype_markers_top_5 <- celltype_markers %>% 
  filter(p_val_adj < 0.05) %>% 
  group_by(cluster) %>% 
  top_n(5, avg_log2FC) %>% 
  arrange(cluster, desc(avg_log2FC))
cat("✅ [Step 5/6] Differential expression analysis completed successfully! 🔍\n\n")

# [Step 6/6] Save Visualizations and Outputs 💾
if (!dir.exists("./outputs")) {
  dir.create("./outputs")
}
saveRDS(seurat_object, "./outputs/seurat_object_processed.rds")

png("./outputs/qc_plots.png", width = 10, height = 5, units = "in", res = 300)
VlnPlot(seurat_object, features = c("nFeature_RNA", "nCount_RNA"), ncol = 2)
dev.off()

png("./outputs/umap_cluster_based.png", width = 6, height = 6, units = "in", res = 300)
DimPlot(seurat_object, label = TRUE, group.by = "seurat_clusters", 
        repel = TRUE, label.box = TRUE, label.size = 6)
dev.off()

png("./outputs/umap_known_celltypes.png", width = 6, height = 6, units = "in", res = 300)
DimPlot(seurat_object, label = TRUE, group.by = "celltype", 
        repel = TRUE, label.box = TRUE, label.size = 6)
dev.off()

png("./outputs/heatmap_cluster_markers.png", width = 10, height = 5, units = "in", res = 300)
DoHeatmap(seurat_object, features = celltype_markers_top_5$gene, 
          group.by = "celltype", assay = "RNA", label = TRUE, 
          size = 3, angle = 45, raster = TRUE)
dev.off()

write.csv(celltype_markers_top_5, "./outputs/celltype_markers_top_5.csv", row.names = FALSE)
cat("✅ [Step 6/6] Visualizations and outputs saved successfully! 💾\n\n")

cat("\n✅ Script executed successfully! 🎉\n")

# Close any remaining graphics devices to avoid default PDF creation (Rplots.pdf)
while (!is.null(dev.list())) {
  dev.off()
}
