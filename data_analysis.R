# Retrevieng data from 2002 census
# http://std.gmcrosstata.ru/webapi/opendatabase?id=vpn2002_pert
# ТЕРСОН Alue + "Город подчинения субъекта РФ"
# 

setwd("~/workspace/courses/oslo2013/workspace_oslo/oslo_essay")

## Karelia
raw <-  read.csv("data/housematerial_karelia.csv", sep=";", skip=6)
names(raw) <- c("region","measure","material","value")

df <- raw
n.row <-  nrow(df)
df <- df[-n.row:-(n.row-2),]
head(df)
tail(df)

# translate material
df$material <- as.character(df$material)
df$material[df$material == "Не указано"] <- "NotSpecified"
df$material[df$material == "Кирпич, камень"] <- "BrickStone"
df$material[df$material == "Панель"] <- "Panel"
df$material[df$material == "Блок"] <- "Block"
df$material[df$material == "Дерево"] <- "Timber"
df$material[df$material == "Смешанный материал"] <- "MixedMaterial"
df$material[df$material == "Другой материал"] <- "OtherMaterial"
df$material <- as.factor(df$material)

summary(df)
# make number relative
library(reshape2)
df.wide <- dcast(df, region + measure ~ material, value.var = "value")

head(df.wide)

df.wide$sum <- rowSums(df.wide[,3:9])
df.wide$ShareBlock <- round(df.wide[,3]/df.wide[,10]*100,2)
df.wide$ShareBrickStone <- round(df.wide[,4]/df.wide[,10]*100,2)
df.wide$ShareMixedMaterial <- round(df.wide[,5]/df.wide[,10]*100,2)
df.wide$ShareNotSpecified <- round(df.wide[,6]/df.wide[,10]*100,2)
df.wide$ShareOtherMaterial <- round(df.wide[,7]/df.wide[,10]*100,2)
df.wide$SharePanel <- round(df.wide[,8]/df.wide[,10]*100,2)
df.wide$ShareTimber <- round(df.wide[,9]/df.wide[,10]*100,2)

# lets order the regions by size for plotting
df.wide <- df.wide[order(df.wide$sum), ]
df.wide$region <- factor(df.wide$region, 
                         levels = as.character(df.wide[order(df.wide$sum), 1]))
df.wide.karelia <- df.wide
# back to long for plotting
df.long <- melt(df.wide, id.vars = "region", 
                measure.vars=c("ShareBlock",
                               "ShareBrickStone",
                               "ShareMixedMaterial",
                               "ShareNotSpecified",
                               "ShareOtherMaterial",
                               "SharePanel",
                               "ShareTimber"))
head(df.long)
df.long <- df.long[!is.na(df.long$value), ]
df.long.karelia <- df.long


# plotting
library(ggplot2)
ggplot(df.long, aes(x=region,y=value,fill=variable)) +
  geom_bar(stat="identity",position="dodge") + 
  coord_flip() +
  labs(title="sorted by the size of settlement - Karelia")
ggsave(file="hist_karelia.png",  width=14, height=14)

# Nizhni
raw <-  read.csv("data/housematerial_nizhni.csv", sep=";", skip=6)
names(raw) <- c("region","measure","material","value")

df <- raw
n.row <-  nrow(df)
df <- df[-n.row:-(n.row-2),]
head(df)
tail(df)

# translate material
df$material <- as.character(df$material)
df$material[df$material == "Не указано"] <- "NotSpecified"
df$material[df$material == "Кирпич, камень"] <- "BrickStone"
df$material[df$material == "Панель"] <- "Panel"
df$material[df$material == "Блок"] <- "Block"
df$material[df$material == "Дерево"] <- "Timber"
df$material[df$material == "Смешанный материал"] <- "MixedMaterial"
df$material[df$material == "Другой материал"] <- "OtherMaterial"
df$material <- as.factor(df$material)

summary(df)
# make number relative
library(reshape2)
df.wide <- dcast(df, region + measure ~ material, value.var = "value")

head(df.wide)

df.wide$sum <- rowSums(df.wide[,3:9])
df.wide$ShareBlock <- round(df.wide[,3]/df.wide[,10]*100,2)
df.wide$ShareBrickStone <- round(df.wide[,4]/df.wide[,10]*100,2)
df.wide$ShareMixedMaterial <- round(df.wide[,5]/df.wide[,10]*100,2)
df.wide$ShareNotSpecified <- round(df.wide[,6]/df.wide[,10]*100,2)
df.wide$ShareOtherMaterial <- round(df.wide[,7]/df.wide[,10]*100,2)
df.wide$SharePanel <- round(df.wide[,8]/df.wide[,10]*100,2)
df.wide$ShareTimber <- round(df.wide[,9]/df.wide[,10]*100,2)

# lets order the regions by size for plotting
df.wide <- df.wide[order(df.wide$sum), ]
df.wide$region <- factor(df.wide$region, 
                         levels = as.character(df.wide[order(df.wide$sum), 1]))
df.wide.nizhni <- df.wide
# back to long for plotting
df.long <- melt(df.wide, id.vars = "region", 
                measure.vars=c("ShareBlock",
                               "ShareBrickStone",
                               "ShareMixedMaterial",
                               "ShareNotSpecified",
                               "ShareOtherMaterial",
                               "SharePanel",
                               "ShareTimber"))
head(df.long)
df.long <- df.long[!is.na(df.long$value), ]
df.long.nizhni <- df.long


# plotting
library(ggplot2)
ggplot(df.long, aes(x=region,y=value,fill=variable)) +
  geom_bar(stat="identity",position="dodge") + 
  coord_flip() +
  labs(title="sorted by the size of settlement - Karelia")
ggsave(file="hist_nizhni.png",  width=14, height=14)

# plot share of timber on map

library(rustfare)
shapefile <- GetRusGADM("rayon")

shape_karelia <- shapefile[shapefile$NAME_1 == "Karelia", ]
shape_nizhni <- shapefile[shapefile$NAME_1 == "Nizhegorod", ]

plot(shape_karelia)
plot(shape_nizhni)

## Karelia
region_key_karelia <- read.csv("~/workspace/general/region_coding/karelia_key_rayon.csv")
df.map.karelia <- merge(df.long.karelia,
                        region_key_karelia,
                        by="region")

library(ggplot2)
library(rgeos)
shape_karelia$id <- rownames(shape_karelia@data)
map.points <- fortify(shape_karelia, region = "id")
map.df <- merge(map.points, shape_karelia, by = "id")

choro <- merge(map.df,df.map.karelia,by="ID_2")
choro <- choro[order(choro$order), ]

ggplot(choro, aes(long,lat,group=group)) +
  geom_polygon(aes(fill = value)) +
  geom_polygon(data = map.df, aes(long,lat), 
               fill=NA, 
               color = "white",
               size=0.1) + # white borders
  coord_map(project="orthographic") +
  facet_wrap(~variable)
ggsave(file="map_karelia.png",  width=14, height=14)

# Nizhni

# tehdään nizhnin avain
write.csv(shape_nizhni@data[,6:7], file="temp/shape_name.csv")
write.csv(factor(df.wide.nizhni$region), file="temp/census_name.csv")
