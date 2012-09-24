library(RColorBrewer)
jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F",
        "yellow", "#FF7F00", "red", "#7F0000"))
redblue.colors <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7",
        "#F7F7F7", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061"))
redblue.ramp <- colorRamp(c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7",
  			"#F7F7F7", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061"))

bluered.colors <- colorRampPalette(c("#053061",  "#2166AC",  "#4393C3",  "#92C5DE",  "#D1E5F0",
        "#F7F7F7",  "#FDDBC7",  "#F4A582",  "#D6604D",  "#B2182B","#67001F"))
heat.colors2 <- colorRampPalette(c("black", "#5C0000", "#B10000", "#FF0000", "orange", "yellow", "white"))
RdYlBu.colors <- colorRampPalette(brewer.pal(11, "RdYlBu"))
RdBlBu.colors <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#A9A9A9", "#4393C3", "#2166AC", "#053061"))
RdBlBu.ramp <- colorRamp(c("#67001F", "#B2182B", "#D6604D", "#A9A9A9", "#4393C3", "#2166AC", "#053061"))

purple.ramp <- colorRamp(brewer.pal(9, "Purples")[2:9], bias=2)
RdYlGn.ramp <- colorRamp(brewer.pal(11, "RdYlGn")[1:11])
RdYlBu.ramp <- colorRamp(brewer.pal(11, "RdYlBu")[1:11])
Spectral.ramp <- colorRamp(brewer.pal(11, "Spectral")[1:11])

scale01 <- function(x) {
	(x-min(x, na.rm=TRUE))/max(x-min(x, na.rm=TRUE), na.rm=TRUE)
}
