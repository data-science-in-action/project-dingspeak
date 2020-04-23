setwd(".\Ding_project")
n<-c(18,20,36,60,63,67,69,79,85,88,95,111,123,143,155,
     189,228,269,314,353,427,474,541,595,649,963,
     723,771,806,841,870,888,913,951,983,1042,1103,1141,
     1183,1280,1378,1464,1500,1534,1549,1566,1575,1583,1587,1610,1616)
m<-(0:50)
df<-as.data.frame(cbind(m,n))

##寻找参数,改变参数形式
library(deSolve)
SS <- getInitial(n ~ SSlogis(m, alpha, xmid, scale), data = df)
K_start <- SS["alpha"]
R_start <- 1/SS["scale"]
N0_start <- SS["alpha"]/(exp(SS["xmid"]/SS["scale"])+1)

##方程
log_formula <- formula(n ~ K*N0*exp(R*m)/(K + N0*(exp(R*m) - 1)))
formu<-nls(log_formula, start = list(K = K_start, R = R_start, N0 = N0_start))
summary(formu)
##计算拟合优度,并保存数据
cor(n,predict(formu))
write.table(predict(formu),"pre.csv",sep=",")
##画曲线
library(ggplot2)
ggplot(df,aes(m, predict(formu)))+geom_line()+
  geom_point(aes(y=n))+
  ##白色背景主题
  theme_bw()+
  ##主次网络线都为空
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  xlab("Date")+ylab("Cumulative Imported Cases")

#绘制并列柱状图
library(ggplot2)
library(reshape2)
t <- c("4.10", "4.11", "4.12", "4.13", "4.14", "4.15", "4.16", "4.17", "4.18", "4.19")
sample1 <- c(4, 2, 10, 3, 10, 12, 11, 10, 7, 4)
sample2 <- c(42, 97, 98, 86, 36, 34, 15, 17, 9, 8)
data=data.frame(t, sample1, sample2)
names(data) = c("日期","国内新增","境外输入新增")
##数据变形
data = melt(data, variable.name = "Sample", value.name = "Num")
head(data)
p=ggplot(data, aes(x=日期, y=Num, fill=Sample))+
  geom_bar(stat = "identity", width = 0.6, position="dodge")+   
       ##这部分position="dodge"，并排肩并肩柱状图 
  scale_fill_manual(values = c("blue","red"))+  ##设定柱状图颜色
  labs(title="国内和境外输入新增人数比较")+   ##设定图片title
  ##设置柱子上的标签文字，position_dodge(width=0.6)设置保证分隔宽度
  geom_text(aes(label=Num),position=position_dodge(width=0.6),size=3,vjust=-0.25)+
  guides(fill=guide_legend(reverse = F))+       ##图例顺序翻转
  ##设置title字号，字体，位置（hjust水平，vjust垂直位置微调）
  theme(plot.title=element_text(size=18,face="bold",vjust=0.5,hjust=0.5),
        ##图例名称、文字字体、字号、位置、大小
        legend.title=element_blank(),  
        legend.text=element_text(size=10,face="bold"),  ##图例文字字体，字号
        legend.position='right',      
        legend.key.size=unit(0.6,'cm'))
print(p)
