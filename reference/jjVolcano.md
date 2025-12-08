# using jjVolcano to visualize marker genes

using jjVolcano to visualize marker genes

## Arguments

- diff_data:

  diff results with data.frame format, default NULL.

- my_markers:

  whether supply your own gene labels, default NULL.

- log2fc_cutoff:

  log2FoldChange cutoff, default 0.25.

- pvalue_cutoff:

  pvalue cutoff to filter, default 0.05.

- adjust_p_cutoff:

  adjusted pvalue cutoff to be colored in plot, default 0.01.

- top_gene_n:

  top genes to be labeled in plot, default 5.

- col_type:

  point color type("updown/adjustP"), default "updown".

- back_col:

  background color, default "grey93".

- p_size:

  point size, default 0.75.

- aes_col:

  point mapping color, default c("#0099CC","#CC3333").

- legend_position:

  legend position in plot, default c(0.7,0.9).

- base_size:

  theme base size, default 14.

- tile_col:

  cluster tile fill color, default jjAnno::useMyCol("paired",n = 9).

- ...:

  other arguments passed by "geom_text_repel".

- cluster_order:

  whether given your cluster orders, default NULL.

- polar:

  whether make the plot to br polar, default FALSE.

- expand:

  the y axis expand, default c(-1,1).

- flip:

  whether flip the plot, default FALSE.

- order_by:

  top marker gene selection method, how the order is, default
  c("avg_log2FC").

## Value

a ggplot object.

## Author

mixfruit

## Examples

``` r
if (FALSE) { # \dontrun{
jjVolcano(diff_data = pbmc.markers)
} # }
```
