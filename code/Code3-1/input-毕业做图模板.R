##############################################################################
# Windows 7, 64x
# Using R 3.5.2 (2018-12-20)
# Using Rstudio 1.1.463
# bobo first write in 2019.02.07
# bobo changed this code a lot in 2019.02.22
# bobo changed this code a lot in 2019.02.25
# bobo changed this code a lot in 2019.03.14
# bobo rewrite and optimize the code in 2019.10.17
##############################################################################
# 设置工作路径,改为自己的实际路径
setwd("E:/实验/01.实时定量/cB1 Acaca已完成")
# 载入所需要的包
# install.packages("agricolae") # 第一次需安装包
# install.packages("tidyverse")
# install.packages("gridExtra")
library(agricolae) # 用于进行LSD事后检验
library(tidyverse) # 一组数据处理与可视化R包的集合，其中包含了ggplot2与dplyr
library(gridExtra) # 用于图片排版
library(export) # 导出图片
##############################################################################
# 创建输出文件夹
dir.create("OutPut")
# 设置字体 set windows font
#showtext.auto(enable = TRUE)
#font.add("tnr", "times.ttf")
windowsFonts("Times New Roman" = windowsFont("Times New Roman"))
##############################################################################
# 按照utf-8代码格式读取数据,首先是甘肃鼢鼠的数据，并转换为数据框格式
vData <- read.csv("Input-甘肃鼢鼠原始倍数.csv", encoding = "UTF-8")
vData <- data.frame(vData)
##############################################################################
# 构建标准误函数
fStd <- function(x){sd(x) / sqrt(length(x))}
##############################################################################
# 定义一个用于绘制单个箱图的函数fBox，以y轴长度、y轴的单位长度以及图例的xy坐标为参数
fBox <- function(vDatas, vUnitLen, vYAxisLen, vLegendX, vLegendY, vTitle){
  ggplot(vData,
         aes(x = factor(分组, level = c("n", "c", "a")),
             y = 倍数,
             fill = 分组)) +
    geom_boxplot(outlier.size = 0.2) +
    theme_bw() +
    # 设定全局参数
    theme(line = element_line(size = 0.01, colour = "black", linetype = 1)) +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank()) +
    # title
    ggtitle(vTitle)+
    theme(plot.title = element_text(size = 10, family = "Times New Roman" )) +
    # legend
    scale_fill_discrete(breaks = c("n","c","a"),
                        labels = c(expression("n: 21%O"[2]),
                                   expression("c: 10.5%O"[2]),
                                   expression("a: 6.5%O"[2]))) +
    theme(legend.position = c(vLegendX, vLegendY),
          legend.key.size = unit(0.25, "cm"),
          legend.text = element_text(size = 9, family = "Times New Roman"),
          legend.title = element_blank())+

    # specify tick marks
    coord_cartesian(ylim = c(0, vYAxisLen))+
    scale_y_continuous(breaks = seq(0, vYAxisLen, vUnitLen)) +
    # axis title
    xlab("分组 (group)")+
    ylab("倍数 (multiple)")+
    theme(axis.title.x = element_text(size = 10, family = "Times New Roman"),
          axis.title.y = element_text(size = 10, family = "Times New Roman")) +
    # axis
    theme(
      axis.ticks.x = element_blank(),
      axis.line = element_line(size = 0.0001, colour = "black", linetype = 1),
      axis.text.x = element_text(color="black", size = 10, family = "Times New Roman"),
      axis.text.y = element_text(color="black", size = 10, family = "Times New Roman")
    )
}
##############################################################################
# draw the Box-plot with fBox
# 得到y轴最大值
vYAxisLen <- max(vData$倍数) * 1.1
# 设定其他参数
vDatas <- vData; vLegendX <- 0.50; vLegendY <- 0.85; vUnitLen <- 0.5; vTitle <- "A"
vBox1 <- fBox(vDatas, vUnitLen, vYAxisLen, vLegendX, vLegendY, vTitle)
##############################################################################
# creat a function to remove extreme values
fRemoveExtreme <- function(vGroup){
  aa <- vGroup[, 2]
  vIqr <- IQR(aa)
  vQ1 <- as.numeric(quantile(aa, 0.25))
  vQ3 <- as.numeric(quantile(aa, 0.75))
  vBottom <- vQ1 - 1.5 * vIqr
  vTop <- vQ3 + 1.5 * vIqr
  vRemoveExtreme <- vGroup %>%
    filter(vGroup[, 2] >= vBottom & vGroup[, 2] <= vTop)
  return(vRemoveExtreme)
}
##############################################################################
# 按组别拆分数据
vDatam <- split(vData, vData$分组)
n <- vDatam$n
c <- vDatam$c
a <- vDatam$a
# remove extreme values use the fRemoveExtreme
n_r <- fRemoveExtreme(n)
c_r <- fRemoveExtreme(c)
a_r <- fRemoveExtreme(a)
##############################################################################
# creat data for Box-plot
vDataCleaned <- merge(n_r, c_r, all = TRUE)
vDataCleaned <- merge(vDataCleaned, a_r, all = TRUE)
##############################################################################
# draw the Box-plot with fBox
# 得到y轴最大值
vYAxisLen <- max(vData$倍数) * 1.1
# 设定其他参数
vDatas <- vDataCleaned; vLegendX <- 0.50; vLegendY <- 0.85; vUnitLen <- 0.5; vTitle <- "B"
vBox2 <- fBox(vDatas, vUnitLen, vYAxisLen, vLegendX, vLegendY, vTitle)
##############################################################################
# output Box-plot
png("OutPut/OutPut-E_box.png", width = 4100,height = 2000,res = 300)
grid.arrange(vBox1, vBox2, ncol = 2)
dev.off()
wd1 <- getwd()

graph2doc(x = vBox1, file="OutPut/OutPut-E_box1.png",
          height = 2.66,
          width = 3.00,
          font = "Times New Roman")
graph2doc(x = vBox2, file="OutPut/OutPut-E_box2.png",
          height = 2.66,
          width = 3.00,
          font = "Times New Roman")
# creat data for t test and lsd test

n_b2 <- n_r$倍数
c_b2 <- c_r$倍数
a_b2 <- a_r$倍数
v_b <- stack(list(n = n_b2, c = c_b2, a = a_b2))
v_b2 <- stack(list(T = n_b2, T = c_b2, T = a_b2))
v_b3 <- stack(list(Tn = n_b2, Tc = c_b2, Ta = a_b2))
v_b <- cbind(v_b, v_b2[,2], v_b3[,2]) 
names(v_b) <- c("values", "ind", "group", "type")
v_b <- data.frame(v_b)
vb_t <- v_b

# 正态性检验，P值大于0.05时代表正态分布
shapiro.test(n_r$倍数)
shapiro.test(c_r$倍数)
shapiro.test(a_r$倍数)
# 方差齐性检验，P值大于0.05时代表方差齐性
bartlett.test(v_b$values~v_b$ind, data = v_b)
# 进行单因素方差分析
v_b.aov <- aov(v_b$values~v_b$ind, data = v_b) 
v_b.aov.summary <- summary(v_b.aov) # 查看单因素方差分析结果
# LSD事后多重比较分析：
summary(v_b.aov)
v_b.lsd <- LSD.test(v_b.aov, "v_b$ind", p.adj = "none")
vb_t_lsd <- v_b.lsd
v_b.lsd$groups
TukeyHSD(v_b.aov)
plot(v_b.lsd)
# 手动计算两两间的P值
vTTest_tnc <- t.test(n_b2,c_b2)
vTTest_tac <- t.test(a_b2,c_b2)
vTTest_tan <- t.test(a_b2,n_b2)
##############################################################################
# creat data for th bar plot
# 平均值
n1 <- mean(n_r$倍数,na.rm = TRUE)
c1 <- mean(c_r$倍数,na.rm = TRUE) 
a1 <- mean(a_r$倍数,na.rm = TRUE)
##############################################################################
# 标准误
tn_sd <- fStd(n_b2)
tc_sd <- fStd(c_b2)
ta_sd <- fStd(a_b2)
sd <- c(tn_sd, tc_sd, ta_sd)
v_mean <- stack(list(n = n1, c = c1, a = a1))
v_mean2 <- stack(list(T = n1, T = c1, T = a1))
v_mean <- cbind(v_mean, v_mean2[,2],sd)
v_mean <- data.frame(v_mean)
# 用于最后做分组的条形图
names(v_mean) <- c("values","ind", "group", "sd")
v_mean3 <- data.frame(v_mean)
# draw a bar
p <- ggplot(data = v_mean, aes(ind, values))+
  geom_bar(stat = "identity", 
           position = position_dodge(0), width = 0.4,
           color = "black", 
           fill = "white")+
  # remove the background, gridlines and border line and some other beautification settings
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))+
  # title  
  ggtitle("A")+
  theme(plot.title =element_text(size = 17, family = "Times New Roman" ))+
  # axis title
  xlab("分组 (group)")+
  ylab("倍数 (multiple)")+
  theme(axis.title.x = element_text(size = 17, family = "Times New Roman"),
        axis.title.y = element_text(size = 17, family = "Times New Roman"))+
  # axis
  theme(
    axis.ticks.x = element_blank(),
    axis.line = element_line(size=0, colour = "black", linetype = 1),
    axis.text.x = element_text(color="black", size = 17, family = "Times New Roman"),
    axis.text.y = element_text(color="black", size = 17, family = "Times New Roman"))
p
##############################################################################
# creat data for point plot
n <- n_r$倍数
c <- c_r$倍数 
a <- a_r$倍数
vDataCleaned <- stack(list(n = n, c = c,a = a ))
vDataCleaned <- data.frame(vDataCleaned)
# add point plot
p2 <- p + geom_point(data=vDataCleaned,
                     aes(x= ind, y = values, color = ind), 
                     size = 2)+
  # legend text and prision
  scale_color_discrete(breaks = c("n","c","a"),
                       labels = c(expression("n: 21%O"[2]),
                                  expression("c: 10.5%O"[2]),
                                  expression("a: 6.5%O"[2]))) +
  theme(legend.position = c(0.85, 0.9),
        legend.text = element_text(size = 17, family = "tnr"),
        legend.title = element_blank()) +
  # specify tick marks
  coord_cartesian(ylim = c(0, 1.6)) +
  scale_y_continuous(breaks=seq(0,1.6,0.3), expand = c(0, 0))
##############################################################################
# 开始SD大鼠的数据处理
# 按照utf-8代码格式读取数据
vData <- read.csv("Input-SD大鼠原始倍数.csv", encoding = "UTF-8")
# 将数据整理为数据框格式
vData <- data.frame(vData)
##############################################################################
# draw the Box-plot with fBox
# 得到y轴最大值
vYAxisLen <- max(vData$倍数) + 0.1
# 设定其他参数
vDatas <- vData; vLegendX <- 0.15; vLegendY <- 0.90; vUnitLen <- 2; vTitle <- "A"
vBox3 <- fBox(vDatas, vUnitLen, vYAxisLen, vLegendX, vLegendY, vTitle)
##############################################################################
# clean up the data
vDatam <- split(vData,vData$分组)
n <- vDatam$n
c <- vDatam$c
a <- vDatam$a
##############################################################################
# remove extreme values use the fRemoveExtreme
n_r <- fRemoveExtreme(n)
c_r <- fRemoveExtreme(c)
a_r <- fRemoveExtreme(a)
##############################################################################
# 合并去除极值后的数据
vDataCleaned <- merge(n_r, c_r, all = TRUE)
vDataCleaned <- merge(vDataCleaned, a_r, all = TRUE)
##############################################################################
# draw the Box-plot with fBox
# 得到y轴最大值
vYAxisLen <- max(vData$倍数) + 0.1
# 设定其他参数
vDatas <- vDataCleaned; vLegendX <- 0.15; vLegendY <- 0.90; vUnitLen <- 2; vTitle <- "B"
vBox4 <- fBox(vDatas, vUnitLen, vYAxisLen, vLegendX, vLegendY, vTitle)
##############################################################################
# output Box-plot
png("OutPut/OutPut-R_box.png", width = 4100, height = 2000, res = 300)
grid.arrange(vBox3, vBox4, ncol = 2)
dev.off()
wd2 <- getwd()
##############################################################################
# creat data for t test and lsd test
# v_r <- merge(a_r, c_r, all=TRUE)
# v_r <- merge(v_r, n_r, all=TRUE)
n_b <- n_r$倍数
c_b <- c_r$倍数
a_b <- a_r$倍数
v_b <- stack(list(n = n_b, c = c_b, a = a_b))
v_b2 <- stack(list(R = n_b, R = c_b, R = a_b) )
v_b3 <- stack(list(Rn = n_b, Rc = c_b, Ra = a_b) )
v_b <- cbind(v_b, v_b2[, 2], v_b3[, 2])
names(v_b) <- c("values", "ind", "group", "type")
v_b <- data.frame(v_b)
vb_r <- v_b
# 正态性检验，P值大于0.05时代表正态分布
shapiro.test(n_r$倍数)
shapiro.test(c_r$倍数)
shapiro.test(a_r$倍数)
# 方差齐性检验，P值大于0.05时代表方差齐性
bartlett.test(v_b$values~v_b$ind, data = v_b)
# 进行单因素方差分析
v_b.aov <- aov(v_b$values~v_b$ind, data = v_b) 
summary(v_b.aov) # 查看单因素方差分析结果
# LSD事后多重比较分析：
v_b.lsd <- LSD.test(v_b.aov, "v_b$ind", p.adj = "none")
vb_r_lsd <- v_b.lsd
v_b.lsd$groups
plot(v_b.lsd)
# 手动计算两两间的P值
vTTest_rnc <- t.test(n_b, c_b)
vTTest_rac <- t.test(a_b, c_b)
vTTest_ran <- t.test(a_b, n_b)
##############################################################################
# creat data for th bar plot
n2 <- mean(n_r$倍数, na.rm = TRUE)
c2 <- mean(c_r$倍数, na.rm = TRUE) 
a2 <- mean(a_r$倍数, na.rm = TRUE)


tn_sd <- fStd(n_b)
tc_sd <- fStd(c_b)
ta_sd <- fStd(a_b)
v_sd <- c(tn_sd, tc_sd, ta_sd)

v_mean <- stack(list(n = n2, c = c2, a = a2))
v_mean2 <- stack(list(R = n2, R = c2, R = a2))
v_mean <- cbind(v_mean, v_mean2[, 2], v_sd)
v_mean <- data.frame(v_mean)
names(v_mean) <- c("values","ind", "group","sd")
# 用于最后做分组的条形图
v_mean4 <- data.frame(v_mean)
# draw a bar
p3 <- ggplot(data = v_mean, aes(ind, values)) +
  geom_bar(stat = "identity", 
           position = position_dodge(0), width = 0.4,
           color = "black", 
           fill = "white")+
  # remove the background, gridlines and border line and some other beautification settings
  theme(panel.grid.major =element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  # title  
  ggtitle("B") +
  theme(plot.title = element_text(size = 17, family = "Times New Roman" )) +
  # axis title
  xlab("分组 (group)")+
  ylab("倍数 (multiple)")+
  theme(axis.title.x = element_text(size = 17, family = "Times New Roman"),
        axis.title.y = element_text(size = 17, family = "Times New Roman")) +
  # axis
  theme(
    axis.ticks.x = element_blank(),
    axis.line = element_line(size = 0, colour = "black", linetype = 1),
    axis.text.x = element_text(color = "black", size = 17, family = "Times New Roman"),
    axis.text.y = element_text(color = "black", size = 17, family = "Times New Roman"))
p3
##############################################################################
# creat data for point plot
n <- n_r$倍数
c <- c_r$倍数 
a <- a_r$倍数
vDataCleaned <- stack(list(n = n, c = c,a = a ))
vDataCleaned <- data.frame(vDataCleaned)
# add point plot
p4 <- p3 + geom_point(data = vDataCleaned,
                      aes(x = ind, y = values, color = ind), 
                      size = 2)+
  # legend text and prision
  scale_color_discrete(breaks = c("n","c","a"),
                       labels = c(expression("n: 21%O"[2]),
                                  expression("c: 10.5%O"[2]),
                                  expression("a: 6.5%O"[2]))) +
  theme(legend.position = c(0.85, 0.9),
        legend.text = element_text(size = 17, family = "tnr"),
        legend.title=element_blank())+
  # specify tick marks
  coord_cartesian(ylim = c(0, 1.6))+
  scale_y_continuous(breaks = seq(0,1.6,0.3), expand = c(0, 0))
p4
# putout the picture
png("OutPut/分组散点图.png", width = 4100,height = 2000,res = 300)
grid.arrange(p2, p4, ncol = 2)
dev.off()
##############################################################################
# 准备分组条形图的数据
v_mean3
v_mean4

# 需要修改的做图参数
# 显著性标记
v_sig <- c("a","b","b","A","Bb","C")
vTimesandp[, 3:4]
# 基因名字
vGeneName <- "Acaca"
# 误差线最高点
vYAxisMax <- (max(v_mean3$values, v_mean4$values) + max(v_mean$sd)) * 1.1 + 1

vYLabName <- paste(vGeneName,"mRNA expression levels")
v_group2 <- c("21%","10.5%","6.5%","21%","10.5%","6.5%")
v_group2 <- factor(v_group2, levels = c("21%", "10.5%", "6.5%"))
v_allgroup <- c("tn","tc","ta","rn","rc","ra")
v_mean <- rbind(v_mean3, v_mean4)
v_mean <- cbind(v_mean, v_allgroup, v_group2, v_sig)
# draw a bar
z <- factor(v_mean$group, level = c("R", "T"))
p5 <- ggplot(data = v_mean, aes(x = v_group2, 
                               y = values, 
                               fill = z)) +
  #  添加柱形图
  geom_bar( stat = "identity", 
            position =  position_dodge(0.85),
            width = 0.85,
            colour = "#000000",
            size = 0.1) +
  # 设定全局参数
  theme(line = element_line(size = 0.1, colour = "black", linetype = 1)) +
  # 添加标准误
  geom_errorbar(aes(ymin = as.numeric(as.character(v_mean$values)),
                    ymax = as.numeric(as.character(v_mean$values)) + as.numeric(as.character(v_mean$sd)),
                    width = 0.1),
                size = 0.1,
                position =  position_dodge(0.9)) +
  # 标准误的文字
  geom_text(aes(y = as.numeric(as.character(v_mean$values)) + as.numeric(as.character(v_mean$sd)) + 3, 
                label = v_sig), 
            position = position_dodge(0.9), size = 3, family = "Times New Roman") +
  # specify tick marks
  coord_cartesian(ylim = c(0, vYAxisMax)) +
  scale_y_continuous(breaks = seq(0, vYAxisMax, 4), expand = c(0, 0)) +
  # remove the background, gridlines and border line and some other beautification settings
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  # legend
  theme(legend.position = c(0.33, 0.95),
        # legend.position = "top",
        legend.background = element_rect(fill = NULL),
        legend.key.size = unit(0.32, "cm"),
        legend.text = element_text(size = 10, family = "Times New Roman"),
        legend.direction = "horizontal",
        legend.title = element_blank()) +
  scale_fill_manual(breaks = c("R","T"),
                    labels = c("SD rat","Gansu zoker"),
                    values = c("white","black")) +
  # axis title
  xlab("Oxygen concentration") +
  ylab(vYLabName) +
  theme(axis.title = element_text(size = 10, family = "Times New Roman")) +
  # axis
  theme(
    axis.ticks.x = element_blank(),
    axis.line = element_line(size = 0.1, colour = "black", linetype = 1),
    axis.text = element_text(size = 10, colour = "black", family = "Times New Roman")
    )
p5
##############################################################################
graph2doc(x = p5, file = "OutPut/PICTURE",
          height = 2.36,
          width = 3.35,
          font = "Times New Roman")
##############################################################################
# 计算上调或者下调的倍数
# 甘肃鼢鼠内部变化
mean_tnc <- n1/c1
mean_tac <- a1/c1
mean_tan <- a1/n1
# SD大鼠内部变化
mean_rnc <- n2/c2
mean_rac <- a2/c2
mean_ran <- a2/n2
# 同浓度下SD大鼠和甘肃鼢鼠比对，全部用r/t
mean_atr <- a1/a2
mean_ctr <- c1/c2
mean_ntr <- n1/n2
##############################################################################
# shihou多重比较显著性de shuju 
vb_rt <- rbind(vb_r, vb_t)
# lsd
vb_t_lsd
vb_r_lsd
# 卡房检验作为参比
kruskal.test(x = vb_rt$values, g = vb_rt$type)
# 方差不为齐性时方差分析的事后多重分析
pairwise.t.test(x = vb_rt$values, g = vb_rt$type, p.adjust.method="bonferroni")
#手动检验同浓度下两种鼠类的差别
vTTest_atr <- t.test(a_b2,n_b)
vTTest_ctr <- t.test(c_b2,c_b)
vTTest_ntr <- t.test(n_b2,n_b)
# 手动两两检验
# print("vTTest_tnc:")
v_t <- vTTest_tnc$statistic
vDataf <- vTTest_tnc$parameter
v_p <- vTTest_tnc$p.value
vTTest <- data.frame(cbind(v_t, vDataf, v_p))
vTTest[1,4] <- "Tnc"
names(vTTest) <- c("t", "df", "p", "type")

# print("vTTest_tac:")
v_t <- vTTest_tac$statistic
vDataf <- vTTest_tac$parameter
v_p <- vTTest_tac$p.value
vTTest2 <- data.frame(cbind(v_t, vDataf, v_p))
vTTest2[1, 4] <- "Tac"
names(vTTest2) <- c("t", "df", "p", "type")

# print("vTTest_tan:")
v_t <- vTTest_tan$statistic
vDataf <- vTTest_tan$parameter
v_p <- vTTest_tan$p.value
vTTest3 <- data.frame(cbind(v_t, vDataf, v_p))
vTTest3[1, 4] <- "Tan"
names(vTTest3) <- c("t", "df", "p", "type")

# print("vTTest_rnc:")
v_t <- vTTest_rnc$statistic
vDataf <- vTTest_rnc$parameter
v_p <- vTTest_rnc$p.value
vTTest4 <- data.frame(cbind(v_t, vDataf, v_p))
vTTest4[1, 4] <- "Rnc"
names(vTTest4) <- c("t", "df", "p", "type")

# print("vTTest_rac:")
v_t <- vTTest_rac$statistic
vDataf <- vTTest_rac$parameter
v_p <- vTTest_rac$p.value
vTTest5 <- data.frame(cbind(v_t, vDataf, v_p))
vTTest5[1,4] <- "Rac"
names(vTTest5) <- c("t", "df", "p", "type")

# print("vTTest_ran:")
v_t <- vTTest_ran$statistic
vDataf <- vTTest_ran$parameter
v_p <- vTTest_ran$p.value
vTTest6 <- data.frame(cbind(v_t, vDataf, v_p))
vTTest6[1, 4] <- "Ran"
names(vTTest6) <- c("t", "df", "p", "type")

# print("vTTest_atr:")
v_t <- vTTest_atr$statistic
vDataf <- vTTest_atr$parameter
v_p <- vTTest_atr$p.value
vTTest7 <- data.frame(cbind(v_t, vDataf, v_p))
vTTest7[1, 4] <- "Atr"
names(vTTest7) <- c("t", "df", "p", "type")

# print("vTTest_ctr:")
v_t <- vTTest_ctr$statistic
vDataf <- vTTest_ctr$parameter
v_p <- vTTest_ctr$p.value
vTTest8 <- data.frame(cbind(v_t, vDataf, v_p))
vTTest8[1,4] <- "Ctr"
names(vTTest8) <- c("t", "df", "p", "type")
# print("vTTest_ntr:")
v_t <- vTTest_ntr$statistic
vDataf <- vTTest_ntr$parameter
v_p <- vTTest_ntr$p.value
vTTest9 <- data.frame(cbind(v_t, vDataf, v_p))
vTTest9[1,4] <- "Ntr"
names(vTTest9) <- c("t", "df", "p", "type")
# 输出上调或者下调的倍数的结果
vGroups <- c("Tnc", "Tac", "Tan", "Rnc", "Rac",
             "Ran", "Atr", "Ctr", "Ntr")
v_content <- c(mean_tnc, mean_tac, mean_tan, mean_rnc, mean_rac, 
               mean_ran, mean_atr, mean_ctr, mean_ntr)
v_times <- data.frame(cbind(vGroups, v_content))
names(v_times) <- c("content", "times")
# 合并显著性
vTTest <- rbind(vTTest, vTTest2, vTTest3, vTTest4, vTTest5,
               vTTest6 ,vTTest7, vTTest8, vTTest9)
row.names(vTTest) <- vTTest$type

vTimesandp <- cbind(vTTest[, -4],v_times[, -1])
names(vTimesandp) <- c("t", "df", "p", "times")
write.csv(vTimesandp, file = "OutPut/OutPut-显著性以及倍数.csv")
# lsd
vb_t_lsd$groups
vb_r_lsd$groups