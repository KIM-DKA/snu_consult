
####################################################################################
# vq contain  
####################################################################################

install.packages(c("dplyr", "vegan", "plotly", "htmlwidgets"))

# Library Import 
library(dplyr)
library(vegan)
library(plotly)
library(htmlwidgets)

# Import data 
setwd("//Users//home//Desktop//snu")
df = read.csv("./di_data.csv")

# Group with vq 
grouped_dfv = 
  df %>% 
  group_by(group,choice,onset,vq,df1) %>%
  summarise(cnt=n(),df2 = mean(df2)) %>% 
  ungroup()

grouped_dfv$cnt = (grouped_dfv$cnt - min(grouped_dfv$cnt)) / (max(grouped_dfv$cnt) - min(grouped_dfv$cnt))
grouped_dfv$df2 = (grouped_dfv$df2 - min(grouped_dfv$df2)) / (max(grouped_dfv$df2) - min(grouped_dfv$df2))

grouped_dfv = as.data.frame(grouped_dfv)
head(grouped_dfv)


model_matrix = cbind(
  model.matrix(~choice+onset+vq+df1-1,data=grouped_dfv),
  avg_df2=grouped_dfv$df2, 
  cnt = grouped_dfv$cnt
)

head(model_matrix)

dv <- vegdist(model_matrix,binary=FALSE,method='gower')
grouped_dfv


###3D-vq 포함

resv <- monoMDS(dv, k = 3)
resv
resv_df <- data.frame(Group =grouped_dfv$group,resv$points)
head(resv_df)
cat(capture.output(resv), file = "NMDS(withvq)3D.csv", sep='\n')


####################################################################################
# vq contain rgl 라이브러리 
####################################################################################

# rgl 라이브러리 설치전 X Quartz 설치 필요 : https://www.xquartz.org/

install.packages("rgl")

# 라이브러리 불러오기
library(rgl)

# 그룹별로 색상과 모양(문자)을 정의합니다.
group_colors <- c(G1 = "red", G2 = "blue", G3 = "green", G4 = "orange")
group_texts <- c(G1 = "G1", G2 = "G2", G3 = "G3", G4 = "G4")  

# 먼저 빈 그래프를 그립니다.
plot3d(1, 1, 1, type = "n", xlim = range(resv_df$MDS1), ylim = range(resv_df$MDS2), zlim = range(resv_df$MDS3), xlab="MDS1", ylab="MDS2", zlab="MDS3")

# 그룹별로 text3d 함수를 사용하여 그림을 그립니다.
for (group in unique(resv_df$Group)) {
  subset_df <- resv_df[resv_df$Group == group, ]
  with(subset_df, text3d(MDS1, MDS2, MDS3, text=group_texts[group], color=group_colors[group]))
}

# 범례에 문자(모양)를 추가합니다.
legend3d(x=0.1, y=0.95, legend = names(group_colors), fill = group_colors, border = group_colors, 
         inset = c(-0.2, 0), horiz=TRUE, pch = as.character(group_texts))

# MDS1 축
lines3d(c(-1, 1), c(0, 0), c(0, 0), col = "black", lwd = 2)

# MDS2 축
lines3d(c(0, 0), c(-1, 1), c(0, 0), col = "black", lwd = 2)

# MDS3 축
lines3d(c(0, 0), c(0, 0), c(-1, 1), col = "black", lwd = 2)

# (0,0,0) 위치에 텍스트 추가
text3d(0, 0, 0, text = "(0,0,0)", col = "black", pos = 4)