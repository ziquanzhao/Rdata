library(ggplot2)
library(sf)
library(cowplot)
library(scatterpie)

rm(list = ls())

#读取数据
mydata <- read.table("HapTable.txt",header = T,sep = "\t")
china <- sf::read_sf("ChinaMap2019.geojson")
nine_line <- sf::read_sf("NineLine2019.geojson")

# 设置饼图半径的映射范围,并重新缩放样本大小以适应半径范围，意思是把饼图半径映射到0.5~1.5的范围
mydata$scaled_size <- scales::rescale(mydata$pie_size, to = c(0.5, 1.5))

main_plot <- ggplot() + 
  geom_sf(data = china,fill="NA",size=1) +   #中国地图主图
  geom_sf(data = nine_line,color='black',size=1)+   #九段线
  geom_scatterpie(aes(x = long, y = lat, group = region,r = scaled_size), cols = c("A", "C"), data = mydata)+  #添加饼图数据
  coord_sf(crs = st_crs(china),ylim = range(16,55))+ #设置地图投影及地图范围
  annotation_scale(location = "bl",style = "bar", width_hint = 0.25) +
  annotation_north_arrow(location = "tl", which_north = "false",style = north_arrow_nautical)+
  theme_minimal()+
  labs(fill = "Hap",title = "LOC_Os01g10110-1950s")+  #设置图例标题，主标题，可以用xlab=“”/ylab=“”设置x轴/y轴标题
  theme(
    panel.border = element_rect(fill = NA,colour = "black",linewidth = 1,linetype = 1),
    axis.text.x = element_text(family = "sans",size = 15),  #x轴标签字体大小及样式，可以用axis.title.x指定x轴标题字体、位置、颜色等
    axis.text.y = element_text(family = "sans",size = 15),  #y轴标签字体大小及样式
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5, vjust = 1,family = "sans"),# 修改标题字体大小、粗细、水平和垂直位置
    legend.position = "right",
    legend.text = element_text(size = 12)
    )


nine_map <- ggplot() +
  geom_sf(data = china,fill='NA') + 
  geom_sf(data = nine_line,color='black')+
  coord_sf(xlim = range(107,123), ylim = range(3,25), crs = st_crs(china))+
  theme(axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(fill=NA,color="grey10",linetype=1,linewidth = 1),
    plot.margin=unit(c(0,0,0,0),"mm"))  #图片框线最外面还有些空白部分，用这个去掉



gg_inset_map = ggdraw() +
  draw_plot(main_plot) +
  draw_plot(nine_map, x = 0.758, y = 0.045, width = 0.13, height = 0.26)


ggsave(plot = gg_inset_map,filename = "china.pdf",width = 9.76,height = 6.81)



#这是绘制饼图的数据表，region只是为了分组而已，可以指定为任何数字，别重复就行，一般是1，2，3，4...
#pie_size是绘制饼图的半径，一般是A，C两列之和即可
#location这一列不用，只是让你整理数据时更方便而已
#location	  long	     lat	     region	  A	  C	  pie_size
#Xinjiang	  85.1886	   41.1153	4	        1	  7	  8
#Guizhou	  106.877	   26.824	  5	        1	  0	  1
#Shanxi	    112.285	   37.5653	6	        2	  0	  2

