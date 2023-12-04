#thiết lập folder làm việc (nơi lưu file data)
setwd("C:/Data_XSTK")
#đọc dữ liệu 2 file vào
gpus <- read.csv("All_GPUs.csv", header = TRUE)
cpus <- read.csv("Intel_CPUs.csv", header = TRUE)

head(gpus) #in dữ liệu 6 dòng đầu
head(cpus)

str(gpus) #in cấu trúc của data (kiểu dữ liệu và thành phần)
str(cpus)
#tạo data mới chỉ gồm các cột cần dùng
df_gpus <- gpus[,c("DVI_Connection","HDMI_Connection","VGA_Connection","TMUs","Open_GL","Shader","Memory","Memory_Speed")]
df_cpus <- cpus[,c("Vertical_Segment","Lithography","nb_of_Cores","nb_of_Threads","Max_Turbo_Frequency")]

apply(is.na(df_gpus), 2, sum) #kiểm tra dữ liệu NA của từng cột trong data
apply(is.na(df_cpus), 2, sum)

print("Xử lý số liệu:")
#GPUs
df_gpus$Memory <- gsub(" MB", '', df_gpus$Memory) #dùng để thay thế ký tự này thành ký tự khác
df_gpus$Memory <- as.numeric(df_gpus$Memory) #ép kiểu dữ liệu thành kiểu số (num)
df_gpus$Memory_Speed <- gsub(" MHz", '', df_gpus$Memory_Speed)
df_gpus$Memory_Speed <- as.numeric(df_gpus$Memory_Speed)
#thay các dữ liệu NA bằng giá trị trung bình
df_gpus$DVI_Connection[is.na(df_gpus$DVI_Connection)] = mean(df_gpus$DVI_Connection, na.rm = TRUE)
df_gpus$HDMI_Connection[is.na(df_gpus$HDMI_Connection)] = mean(df_gpus$HDMI_Connection, na.rm = TRUE)
df_gpus$VGA_Connection[is.na(df_gpus$VGA_Connection)] = mean(df_gpus$VGA_Connection, na.rm = TRUE)
df_gpus$TMUs[is.na(df_gpus$TMUs)] = mean(df_gpus$TMUs, na.rm = TRUE)
df_gpus$Open_GL[is.na(df_gpus$Open_GL)] = mean(df_gpus$Open_GL, na.rm = TRUE)
df_gpus$Shader[is.na(df_gpus$Shader)] = mean(df_gpus$Shader, na.rm = TRUE)
df_gpus$Memory[is.na(df_gpus$Memory)] = mean(df_gpus$Memory, na.rm = TRUE)
df_gpus$Memory_Speed <- gsub(" MHz", '', df_gpus$Memory_Speed)
df_gpus$Memory_Speed <- as.numeric(df_gpus$Memory_Speed)
df_gpus$Memory_Speed[is.na(df_gpus$Memory_Speed)] = mean(df_gpus$Memory_Speed, na.rm = TRUE)
#CPUs
df_cpus$Lithography <- gsub(" nm", '', df_cpus$Lithography)
df_cpus$Lithography <- as.numeric(df_cpus$Lithography)
df_cpus$Lithography[is.na(df_cpus$Lithography)] = mean(df_cpus$Lithography, na.rm = TRUE)

df_cpus$Max_Turbo_Frequency <- gsub(" GHz", '', df_cpus$Max_Turbo_Frequency)
df_cpus$Max_Turbo_Frequency <- as.numeric(df_cpus$Max_Turbo_Frequency)
df_cpus$Max_Turbo_Frequency[is.na(df_cpus$Max_Turbo_Frequency)] = mean(df_cpus$Max_Turbo_Frequency, na.rm = TRUE)
#---------------------------

print("Tìm khoảng tin cậy:")
t.test(df_gpus$Open_GL, conf.level = 0.95)

#---------------------------

print("Kiểm định một mẫu:")
#H0: mu = 3072
#H1: mu < 3072
t.test(df_gpus$Memory, alternative = "less", mu = 3072, paired = FALSE, var.equal = TRUE, conf.level = 0.95)

#---------------------------

print("Kiểm định hai mẫu:")
t.test(df_gpus$Open_GL, df_gpus$Shader, alternative = "two.sided", mu = 0, paired = FALSE, var.equal = TRUE, conf.level = 0.95)

#---------------------------

print("Anova mot yếu tố:")
VS <- factor(df_cpus$Vertical_Segment) #biến độc lập
MTF <- df_cpus$Max_Turbo_Frequency #biến phụ thuộc

table(df_cpus$Vertical_Segment) #số lượng mẫu từng biến độc lập

#Kiểm định giả thiết:
# - Các mẫu độc lập
# - Biến phụ thuộc là biến liên tục
# - Các nhóm được lấy ra từ tổng thể có phân phối chuẩn hoặc gần chuẩn

#Kiểm định Shapiro-Wilk để biết có phân phối chuẩn hoặc gần chuẩn:
library(nortest)
shapiro.test(MTF[VS=="Desktop"])
shapiro.test(MTF[VS=="Embedded"])
shapiro.test(MTF[VS=="Mobile"])
shapiro.test(MTF[VS=="Sever"])

plot(aov(MTF~VS), 2) #vẽ đồ thị

av_res <- rstandard(aov(MTF~VS))
shapiro.test(av_res)

library(car)
leveneTest(MTF~VS) #kiểm tra các nhóm có phương sai đồng nhất

#phân tích anova
av <- aov(MTF~VS, data = df_cpus)
summary(av) #hiển thị kết quả

TukeyHSD(av) #phân tích xem có sự khác nhau giữa các biến độc lập

#---------------------------

print("Hồi quy tuyến tính đơn:")
#vẽ đồ thị hồi quy
library(ggpubr)
ggscatter(data = df_gpus, y = "Memory", x = "Memory_Speed") + geom_smooth(method = "lm", se = T)
#hồi quy tuyến tính đơn
lm <- lm(df_gpus$Memory~df_gpus$Memory_Speed, data = df_gpus)
summary(lm)
