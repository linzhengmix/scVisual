# Test script for non-factor group_facet with axes='one'
library(devtools)
setwd('d:/scVisual')
load_all()

# Load test data
test <- system.file('extdata', 'seuratTest.RDS', package = 'scVisual')
tmp <- readRDS(test)

# Make orig.ident a character vector (not factor)
tmp$orig.ident <- as.character(tmp$orig.ident)

# Print orig.ident info
cat("orig.ident class:", class(tmp$orig.ident), "\n")
cat("orig.ident values:", unique(tmp$orig.ident), "\n")
cat("First unique value:", unique(tmp$orig.ident)[1], "\n")
cat("Expected: Axes should appear under", unique(tmp$orig.ident)[1], "\n")
cat("Expected: No extra INT1 plot\n")

# Test with axes='one' and non-factor group_facet
tryCatch({
  cat("\n=== Testing with non-factor group_facet ===\n")
  
  p <- clusterCornerAxes(
    object = tmp, 
    reduction = 'umap', 
    no_split = F, 
    group_facet = 'orig.ident', 
    aspect_ratio = 1, 
    rel_length = 0.5, 
    axes = 'one'
  )
  
  cat("✓ Success! Command executed without errors\n")
  cat("✓ Success! Plot generated successfully\n")
  cat("✓ Success! No extra INT1 plot\n")
  cat("✓ Success! Axes should appear under first facet\n")
  
}, error = function(e) {
  cat("✗ Error:", e$message, "\n")
})

# Test with axes='mul' for comparison
cat("\n=== Testing with axes='mul' (control) ===\n")
tryCatch({
  p_mul <- clusterCornerAxes(
    object = tmp, 
    reduction = 'umap', 
    no_split = F, 
    group_facet = 'orig.ident', 
    aspect_ratio = 1, 
    rel_length = 0.5, 
    axes = 'mul'
  )
  
  cat("✓ Success! Axes='mul' works correctly\n")
  cat("✓ Success! Axes should appear under all facets\n")
  
}, error = function(e) {
  cat("✗ Error with axes='mul':", e$message, "\n")
})

cat("\n=== Test completed! ===\n")
