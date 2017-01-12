
#' Build map image of dust roses for shoreline TEOM stations
#' 
#' @param day Date. Date for which to build dust roses. 
#' @param path String. Path to image file.
#' @import dplyr
#' @import ggplot2
#' @import rgdal
#' @return Text string denoting location of plot image file.

shoreline_roses <- function(day=Sys.Date()-1, path=tempdir()){
    mfile_sites <- c("LonePine", "Keeler", "NorthBch", "LizardTl", 
                     "MillSite", "ShellCut", "DirtySox", 
                     "Olancha", "Stanley")
    query1 <- paste0("SELECT i.deployment, m.site, m.datetime, m.dir, ",
                     "m.aspd, m.teom, m.qaqc_level_id, ", 
                     "st_y(st_transform(i.geom, 26911)) AS y, ",
                     "st_x(st_transform(i.geom, 26911)) AS x ",
                     "FROM archive.mfile_data m ", 
                     "JOIN instruments.deployments i ",
                     "ON m.deployment_id=i.deployment_id ",
                     "WHERE (m.datetime - ('1 second')::interval)::date='",
                     day, "'::date AND m.site IN ('", 
                     paste0(mfile_sites, collapse="', '"), "')")
    mfile_df <- query_owens(query1)
    site_labels <- mfile_df %>% distinct(deployment, x, y)

    daily_summary <- mfile_df %>% group_by(deployment) %>%
        summarize(daily.pm10=round(sum(teom, na.rm=T)/24, 0))
    daily_summary$exceed <- sapply(daily_summary$daily.pm10, 
                                   function(x) ifelse(x>150, T, F))
    daily_summary$flag.color <- ifelse(daily_summary$exceed, "red", "black")

    legend.plot <- mfile_df %>% filter(deployment==mfile_df$deployment[1]) %>%
        plot_rose(., value='teom', dir='dir', valueseq=valueseq,
                  legend.title=bquote('P'*M[10]~'('*mu*'g/'*m^3*')'))
    legnd <- g_legend(legend.plot)

    p1 <- ggplot(data=shoreline$polygons, mapping=aes(x=x, y=y)) +
        geom_path(mapping=aes(group=objectid)) +
        geom_point(data=site_labels, mapping=aes(x=x, y=y)) +
        coord_equal() 
    info <- ggplot_build(p1)
    xrange <- info[[2]]$panel_ranges[[1]]$x.range
    yrange <- info[[2]]$panel_ranges[[1]]$y.range
    valueseq <- c(10, 50, 100, 150)
    p2 <- p1 + 
        xlim(xrange[1] - 1000, xrange[2] + 1000) +
        ylim(yrange[1] - 1000, yrange[2] + 1000) +
        annotation_custom(legnd,
                          xmin=xrange[2] - 4000, 
                          xmax=xrange[2], 
                          ymin=yrange[2] - 8000, 
                          ymax=yrange[2]) +
        theme(axis.line=element_blank(),
              axis.text.x=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              legend.position="none",
              panel.background=element_blank(),
              panel.border=element_blank(),
              panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),
              plot.background=element_blank(),
              plot.title=element_text(size=12))

roses <- list(grobs=c(), centers=c())
for (j in site_labels$deployment){
    if (sum(!is.na(filter(mfile_df, deployment==j)$teom))==0) next 
    p_rose <- mfile_df %>% filter(deployment==j) %>%
        plot_rose_image_only(., value='teom', dir='dir', 
                             valueseq=valueseq)
    fl <- tempfile()
    png(filename=fl, bg="transparent")
    print(p_rose)
    dev.off()
    ras <- grid::rasterGrob(png::readPNG(fl), interpolate=TRUE)
    roses$grob[[j]] <- ras
    roses$center[[j]] <- c(filter(site_labels, deployment==j)$x, 
                           filter(site_labels, deployment==j)$y)
}

buffer <- 4000
p3 <- p2
for (m in names(roses$grob)){
    label_data <- daily_summary %>% filter(deployment==m) %>%
        left_join(site_labels, by="deployment")
    p3 <- p3 +
        annotation_custom(roses$grob[[m]],
                          xmin=roses$center[[m]][1] - buffer, 
                          xmax=roses$center[[m]][1] + buffer, 
                          ymin=roses$center[[m]][2] - buffer, 
                          ymax=roses$center[[m]][2] + buffer) +
geom_label(data=label_data, mapping=aes(x=x, y=y, label=daily.pm10), 
           color=label_data$flag.color, nudge_x=1000, nudge_y=1000) 
}

p4 <- p3 +
    ggtitle(substitute(atop(paste("Hourly P",  M[10], " Roses for ", d), 
                            paste("Site Label = 24-hour P", M[10])),  
                       list(d=format(as.Date(day), "%m-%d-%Y"))))
fl <- paste0(path, "/", format(as.Date(day), "%m-%d-%Y"), "_shoreline.jpg")
jpeg(filename=fl, width=6, height=8, units="in", quality=100, res=300)
print(p4)
dev.off()
return(fl)
}
