#アマモラインデータの整理、マップ作製、面積変動
library(tidyverse)
library(lubridate)
library(gnnlab)
library(magick)
library(showtext)
library(readxl)
library(patchwork)

#abcde評価含み
df1 = tibble(path = dir("~/Lab_Data/kabeyamam/line/abcde",
                        full = T)) |> 
  mutate(data = map(path, function(x)
  {read_csv(x, col_names = "file")})) |> 
  unnest(data) |>
  mutate(date = ym(str_remove_all(basename(path), pattern = "[_.csv]"))) |>
  separate(file,c("line","point","amamo","percent"),sep ="_") |> 
  select(-path) |> 
  mutate(line=str_remove(line,pattern = "L"))|> 
  mutate(across(.cols=c(line,point),.fns=as.double)) |> 
  mutate(point=abs(point-166)) |> 
  mutate(line=2.5*(2*line-1)) |> 
  select(-amamo)

#％評価のみ
df2 = tibble(path = dir("~/Lab_Data/kabeyamam/line/202205kara",
                        full = T)) |> 
  mutate(data = map(path, function(x)
  {read_csv(x, col_names = "file")})) |> 
  unnest(data) |>
  mutate(date = ym(str_remove_all(basename(path), pattern = "[_.csv]"))) |>
  separate(file,c("line","point","percent"),sep ="_") |> 
  select(-path) |> 
  mutate(line=str_remove(line,pattern = "L"))|> 
  mutate(across(.cols=c(line,point),.fns=as.double)) |> 
  mutate(point=abs(point-166)) |> 
  mutate(line=2.5*(2*line-1)) 

df = df1 |> full_join(df2)
df_count = df

df |> summarise(point_max = max(point))

lp_info  = df |> summarise(across(c(point, line), list(min = min, max = max))) |> 
  mutate(point_min = 0,
         point_max = 66,
         line_max = 52.5)

date_info = df |> distinct(date)

matrix = date_info |> 
  bind_cols(lp_info) |> 
  mutate(date = as.character(date)) |> 
  mutate(by = ifelse(str_detect(date,pattern =  "2021-05-01"), "10", "5")) |> 
  mutate(date = ymd(date),
         by = as.double(by)) |> 
  mutate(line = pmap(list(line_min, line_max, by), function(x, y, z){
    seq(x, y, by = z)
  })) |>
  select(date, line) |> 
  unnest(line) |>
  mutate(point = list(seq(0, 66, by = 2))) |> 
  unnest(point)


df = df |> full_join(matrix, by = c("line", "point", "date")) |> 
  mutate(percent = replace_na(percent, "0")) |> 
  mutate(percent1 = factor(percent,
                           levels = c("95","75","65","40","30","20","15","8","1","0")))

df_count = df

df = df |>
  mutate(year= year(date)) |> 
  mutate(month = month(date)) |> 
  filter(month %in% 5:7) |> 
  mutate(month = paste0(month,"月"))

df |> 
  ggplot()+
  geom_tile(aes(y=point,
                x=line,
                fill=percent1))+
  # annotate(geom = "text",
  #          x=20,
  #          y=-5,size = 3,
  #          label="Tetrapod",
  #          vjust=0,hjust=0,
  #          color="black")+
  scale_y_continuous(name="距離 (m)",
                     breaks = seq(0,70,
                                  by=10))+
  scale_x_continuous(name="距離 (m)",
                     breaks = seq(0,50,
                                  by=10))+
  scale_fill_viridis_d(name="被度",
                       end=1,
                       direction = -1,
                       option = "D")+
  facet_grid(rows = vars(year), cols = vars(month))
# ggpubr::theme_pubr() 
# theme(axis.text = element_text(size = 15),#横軸数値
#       axis.title = element_text(size= 20),#軸タイトルdistance
#       legend.text = element_text(size =20),#fill ABCDE
#       legend.title = element_text(size = 20),#fillタイトル　coverage
#       strip.text = element_text(size = 20))#月名のタイトル

# save_plot("amamonohido.pdf", 
#           width = 200, height = 150, units = "mm")

# 仮の緯度経度をつくる.
longlat = 
  expand_grid(long = seq(129.1179, by = 0.00005, length = 11), 
              lat = seq(32.98795, by = 0.000018, length = 34)) |>
  nest_by(long) |> 
  ungroup() |> 
  mutate(line = seq(2.5, 52.5, by = 5))  |> 
  unnest() |>
  nest_by(lat) |> 
  ungroup() |> 
  mutate(point = seq(0, 66, by = 2))  |> 
  unnest()

kabe1 = df |> full_join(longlat, by = c("line", "point"))

# 地図データの読み込み.
long = min(longlat$long)
lat = min(longlat$lat)

width = 10 # 横 km.
height = 10 # 縦 km.
map_type = "AdmArea" # 地図の種類.

# 1 km to longitude.  
equator_r = 6378137
long_degree = (360*1000)/(2*pi*(equator_r*cos(lat*pi/180)))

# 1 km to latitude.
pole_r = 6356752.314
lat_degree = (360*1000)/(2*pi*pole_r)

# 図の緯度経度の幅.
longA = long - long_degree*width/2
longB = long + long_degree*width/2
latA = lat - lat_degree*height/2
latB = lat + lat_degree*height/2

# xml ファイルがたくさんあるので, 不要なファイルは読まないようにする.
# 四隅をとり, 緯度・経度 0.01 刻みの行列を作る. 
# coords_to_mesh()で gml id が一致したファイルのみ残す.
corner = tibble(LONG = c(longA, longA, longB, longB),
                LAT = c(latA, latB, latA, latB))

c = tibble(LONG = c(seq(longA, longB, by = 0.01), longB)) |> distinct() |> 
  expand_grid(tibble(LAT = c(seq(latA, latB, by = 0.01), latB)) |> distinct()) |> 
  mutate(gml_id = jpmesh::coords_to_mesh(LONG, LAT, mesh_size = 10)) |> 
  summarise(gml_id = as.character(unique(gml_id))) |> 
  mutate(gml_id = str_extract(gml_id, pattern = "[0-9]{6}")) |> 
  pull(gml_id)

# 長崎のマップデータ.
gsi = tibble(xml = dir("~/Lab_Data/Japan_map_data/FG/",
                       full.names = TRUE)) |>
  mutate(fname = basename(xml)) |>
  separate(fname, into = c("fg", "gml", "id", "type", "date", "num"))

gsi = gsi |>
  filter(id %in% c) |> # gml id.
  filter(str_detect(type, map_type)) # map type.

try_read_fgd = possibly(fgdr::read_fgd, NULL)

gsi = gsi |>
  mutate(data = map(xml, try_read_fgd)) |>
  pull(data) |> 
  bind_rows()

# 図の緯度経度の幅.
LONGa = 129.1174
LONGb = 129.1187
LATa = 32.98788
LATb = 32.98872

# 軸ラベルの緯度経度の幅.
longA = 129.1175
longB = 129.1185
latA = 32.9880
latB = 32.9885

#軸ラベルの区切り間隔.
dist_long = 0.0005
dist_lat  = 0.0005

#10進法表記から60進法表記への変換
dms = seq(latA, latB, by = dist_lat)
hms = seq(longA, longB, by = dist_long)

#軸ラベルの作成
dmsa = vector(mode = "character", length = length(dms))
for (i in 1:length(dms)) {
  dmsa[i] = parse(text = paste(dms[i], "^", "degree", "*", "N",  sep = ""))
}

hmsa = vector(mode = "character", length = length(hms))
for (i in 1:length(hms)) {
  hmsa[i] = parse(text = paste(hms[i], "^", "degree", "*", "E", sep = ""))
}

# 地図に重ねる.
# pl1 = 
kabe1 |> 
  filter(percent1 != 0) |> 
  mutate(label = paste0(year, "年 ", month)) |> 
  ggplot() +
  geom_sf(data = gsi) +
  geom_tile(aes(y=lat, x=long, fill=percent1))+
  # guides(color = F) + 
  geom_text(aes(x = 129.1176, y = 32.98862, label = label), 
            check_overlap = T, size = 5) +
  coord_sf(xlim = c(LONGa, LONGb), ylim = c(LATa, LATb)) +
  scale_y_continuous(breaks = seq(latA, latB, by = dist_lat), labels = c(dmsa)) +
  scale_x_continuous(breaks = seq(longA, longB, by = dist_long), labels = c(hmsa)) +
  scale_fill_viridis_d(name="被度",
                       end=0.9,
                       direction = -1,
                       option = "D")+
  facet_grid(rows = vars(month), cols = vars(year)) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
  theme_void() +
  theme(strip.text = element_blank(),
        legend.position = "top")
# theme_pubr(base_family = "notoserifcjk", border = TRUE)  +
# theme(axis.title = element_blank())

# save_plot("amamonohido1.pdf", 
#           width = 200, height = 250, units = "mm")



df |> 
  mutate(percent = as.double(percent)) |> 
  mutate(percent = ifelse(str_detect(date ,pattern = "2021-05-01"),percent*1.83,percent)) |> 
  group_by(date) |>
  summarise(sum =sum(percent)) |> 
  add_row(date=ym(2201),.before = 9) |> 
  ggplot()+
  geom_line(aes(date,sum))+
  scale_x_date(breaks = "1 month", 
               labels = label_date_short())

df |> 
  mutate(percent = as.double(percent)) |> 
  mutate(percent = ifelse(str_detect(date ,pattern = "2021-05-01"),percent*1.83,percent)) |> 
  group_by(date) |>
  summarise(sum =sum(percent)) |> 
  add_row(date=ym(2201),.before = 9) |> 
  ggplot()+
  geom_point(aes(x = date,y = sum))+
  geom_smooth(aes(x = date,y = sum),method = "gam",
              formula = y~s(x,k = 10))




# df |> 
#   mutate(amamo_per=as.character(amamo_per)) |> 
#   mutate(amamo_per=as.double(amamo_per)) |> 
#   group_by(date,amamo) |> 
#   mutate(n=n())

df_count |> 
  mutate(month = month(date)) |>
  filter(month == c(6,7)) |> 
  select(-percent) |> 
  group_by(date,percent1) |> 
  summarise(n = n()) |> 
  ggplot()+
  geom_bar(aes(x = n,y = percent1,fill = percent1),stat = "identity")+
  scale_x_continuous(trans = "log1p")+
  scale_fill_viridis_d(end = 0.8,direction = -1)+
  facet_wrap(vars(date))


