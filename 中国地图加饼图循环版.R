rm(list = ls())

# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)
library(ggspatial)
library(cowplot) # 用于嵌套图
library(grid)
library(scatterpie)

LOCfilename <- "LOC_Os02g32490.txt"

location <- read.table("location.txt",header = T)
mydata <- read.table(LOCfilename,header = T)

china_shp <- "中国省级地图GS（2019）1719号.geojson"
nine <- "九段线GS（2019）1719号.geojson"
china <- sf::read_sf(china_shp)
nine_line <- sf::read_sf(nine)

# 获取中国地图的边界
bbox <- st_bbox(china)
# 自定义边框函数
custom_border <- function() {
  annotation_custom(
    grob = rectGrob(
      x = unit(0.5, "npc"), y = unit(0.5, "npc"),
      width = unit(1, "npc"), height = unit(1, "npc"),
      gp = gpar(col = "black", fill = NA, lwd = 2)
    ),
    xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
  )
}

plotlist <- list()

for (year in c("1950s","1960s","1970s","1980s","1990s","2000s","2010s")) {
  # Filter data for 1950s
  data_year <- mydata %>% filter(years == year)
  
  # Calculate frequencies for haplotypes A and C in 1950s
  hap_freq_year <- data_year %>%
    filter(Hap %in% c("C", "T")) %>%
    group_by(country, Hap) %>%
    summarise(freq = n()) %>%
    ungroup()
  
  # Convert data to wide format and add sample_size column
  hap_freq_wide_year <- hap_freq_year %>%
    pivot_wider(names_from = Hap, values_from = freq, values_fill = list(freq = 0)) %>%
    mutate(sample_size = C + T, region = row_number(),long = NA,lat = NA)
  
  for (i in hap_freq_wide_year$country) {
    hap_freq_wide_year[hap_freq_wide_year$country==i,6] <- location[location$location==i,2]
    hap_freq_wide_year[hap_freq_wide_year$country==i,7] <- location[location$location==i,3]
  }
  
  print(hap_freq_wide_year)
  
  d <- hap_freq_wide_year[,2:ncol(hap_freq_wide_year)]
  main_plot <- ggplot() + 
    geom_sf(data = china,fill="NA",size=1) + 
    geom_sf(data = nine_line,color='black',size=1)+
    coord_sf(xlim = range(75,135), ylim = range(15,55), crs = st_crs(china))+
    annotation_scale(location = "bl",style = "bar", width_hint = 0.25) +
    annotation_north_arrow(location = "tl", which_north = "false",style = north_arrow_nautical)+
    theme_minimal()+
    custom_border()+
    ggtitle(paste(year," (n=",sum(d$sample_size),")",sep = ""))+  # 添加标题
    theme(panel.background = element_rect(fill = "white", color = NA), # 设置面板背景为白色
          panel.grid.major = element_line(color = "grey80"), # 添加主要网格线
          panel.grid.minor = element_blank(), # 去除次要网格线
          plot.background = element_rect(fill = NA, color = NA),   # 设置图形外边距
          axis.title.x = element_text(margin = margin(t = 10)), # 为x轴标签添加外边距
          axis.title.y = element_text(margin = margin(r = 10)), # 为y轴标签添加外边距
          axis.text.x = element_text(family = "serif",size = 15,margin = margin(t = 5)), # 为x轴刻度添加外边距
          axis.text.y = element_text(family = "serif",size = 15,margin = margin(r = 5)),
          plot.title = element_text(size = 20, face = "bold", hjust = 0.5, vjust = 1,family = "serif"))# 修改标题字体大小、粗细、水平和垂直位置
  
  
  nine_map <- ggplot() +
    geom_sf(data = china,fill='NA') + 
    geom_sf(data = nine_line,color='black')+
    coord_sf(xlim = range(107,122), ylim = range(3,22), crs = st_crs(china))+
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(fill=NA,color="grey10",linetype=1,linewidth = 1),
          plot.margin=unit(c(0,0,0,0),"mm"))
  
  
  # 设置饼图半径的映射范围
  min_radius <- 0.5
  max_radius <- 1.5
  # 重新缩放样本大小以适应半径范围
  d$scaled_size <- scales::rescale(d$sample_size, to = c(min_radius, max_radius))
  
  pie_plot <- ggplot()+
    geom_scatterpie(aes(x = long, y = lat, group = region,r = scaled_size), cols = c("C", "T"), data = d)+
    coord_sf(xlim = range(67,135.5), ylim = range(16.5,55), crs = st_crs(china))+
    theme_minimal()+
    theme(panel.background = element_rect(fill = NA, color = NA), # 设置面板背景为白色
          panel.grid.major = element_line(color = NA), # 添加主要网格线
          panel.grid.minor = element_blank(),
          axis.text = element_blank(),  # 隐藏坐标轴文本
          axis.ticks = element_blank(),  # 隐藏坐标轴刻度线
          axis.title = element_blank(),  # 隐藏坐标轴标题
          legend.position = "right",  # 将图例放置在右边
          legend.text = element_text(size = 10))+  # 设置图例文本的大小
    labs(fill = "Hap")
  
  gg_inset_map = ggdraw() +
    draw_plot(main_plot) +
    draw_plot(nine_map, x = 0.805, y = 0.022, width = 0.12, height = 0.26)+
    draw_plot(pie_plot)
  
  plotlist[[year]] <- gg_inset_map
  
}

plot_grid(plotlist = plotlist,
          align = "h", 
          ncol = 3
          #labels = c("a","b","c")
          #label_size = 14,
          #label_fontfamily = "serif",
          #label_fontface = "bold"
          )

geneID <- strsplit(x = LOCfilename,split = ".txt")[[1]]
ggsave(filename = paste(geneID,"pdf",sep = "."),,width = 30.18,height = 21.87)


