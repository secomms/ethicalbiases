install.packages("readxl")
library(readxl)
rm(list = ls(all = TRUE))
#set the path
path = "/Users/..../Desktop/"
setwd(path)
Tweets = read_excel ("Tweets.xlsx")
head(Tweets)


# Set API key <- PerspectiveAPI
Sys.setenv(perspective_api_key = "XXXX")

library (peRspective)
library(tibble)

#auth_setup_default()

# Data processing 
text_sample <- tibble(ctext = Tweets$full_text , textid = Tweets$text_id) 
b=c(
  peRspective::prsp_models,
  peRspective::prsp_exp_models
)
a= c(text_sample %>%
       
       prsp_stream(text = ctext,
                   text_id = textid,
                   score_model = b,
                   safe_output = T,
                   verbose = T))        

# Visualization and analysis of results     
my_df <- as.data.frame(a)
my_df

# Package and library to export results to excel
install.packages("writexl")
library("writexl")
#set the path
write_xlsx(my_df,"/Users/..../Desktop/Scores.xlsx")

#save(my_df, file="DataCollection")


#################

install.packages("readxl")
library(readxl)
rm(list = ls(all = TRUE))
#set the path
path = "/Users/..../Desktop/"
setwd(path)
my_df = read_excel ("Scores.xlsx")
my_df = as.data.frame(my_df)
head(my_df)

#load("DataCollection")


#one=WHO
#two=WTA
#three=IMF
#four=NATO


#ANALISYS
Summary = summary (my_df[,3:24])
Summarydf = data.frame(Summary)


#test for normality
AA = c()

for (v in 3:24) {
  A = shapiro.test(my_df[,v])
  if (A$p.value < 0.05){
    AA = c(A$p.value,AA)
    V = length(AA)}}

my_dfok = my_df[,3:24]
vectorIQR = c()
for (xxx in 1:22) {IQRelem = IQR((my_dfok[,xxx]),na.rm=TRUE)
vectorIQR = c(vectorIQR,IQRelem)}


my_vecG = c()
ycollection = c()
list_datameann <- list()
list_datamean <- list()
list_datay <-list()
list_kmediany1 = list ()
list_kmeany1 = list ()
list_median.over = list()
list_mean.over = list()
list_dataIQR = list()
list_dataSD = list()
CDFy1 = c()


for (m in 1:4) {
  
  attributes = 3:24
  n.tweets = 100
  listseq = vector(mode="list", length=4)
  
  y= my_df[((n.tweets*(m-1))+1):(n.tweets*(m)),attributes] 
  
  ycollection = c(y,ycollection)
  
  for (u in 0:3) { 
    
    f = 22 
    listseq[[1+u]] = ycollection [((f*u)+1):(f*(u+1))]
    
  }
  
  
  dmy_vecG = c()
  MEANy1 = c()
  MEDIANy1 = c()
  pos.Median1 = c()
  Kmediany1 = c()
  kmeany1 = c()
  IQRy1 = c()
  SDy1 = c()
  median.over = c()
  mean.over = c()
  
  
  for (g in 1:22) {G = shapiro.test(y[, g])
  my_vecG = c(my_vecG, G$p.value)
  
  listG = vector(mode="list", length=4)
  
  
  for (u in 0:3) { 
    
    f=22
    listG[[1+u]] = my_vecG [((f*u)+1):(f*(u+1))]
    
  }
  
  
  if (G$p.value<=0.05) {dmy_vecG = c(dmy_vecG,g)
  mediany1 = median (y[, g], na.rm=TRUE)
  MEDIANy1 = c(MEDIANy1,mediany1)
  if (mediany1>=0.70) {Kmediany1 = c(Kmediany1, g)
  median.over = c(median.over, mediany1)}
  iqry1 = IQR(y[, g], na.rm=TRUE)
  IQRy1 = c(IQRy1,iqry1)
  
  }
  
  
  else {meany1 = mean (y[, g], na.rm=TRUE)
  
  MEANy1 = c(MEANy1,meany1)
  if (meany1>=0.70) {kmeany1 = c(kmeany1, g) 
  mean.over = c(mean.over, meany1)}
  sdy1 = sd(y[, g], na.rm=TRUE)
  SDy1 = c(SDy1,sdy1)}#else
  
  }
  
  
  list_datameann = list (MEDIANy1, list_datameann)
  list_datamean = list (MEANy1, list_datamean)
  list_datay = list (ycollection,list_datay)
  list_kmediany1 = list ( Kmediany1,list_kmediany1)
  list_kmeany1 = list(kmeany1,list_kmeany1)
  list_median.over = list(median.over,list_median.over)
  list_mean.over = list( mean.over, list_mean.over)
  list_dataIQR = list(IQRy1, list_dataIQR)
  list_dataSD = list(SDy1, list_dataSD) 
  
  
  ################
  
  #CDF
  
  
  listCDF = vector(mode="list", length=4)
  
  
  for (w in 1:22) {
    
    cdfy1 = ecdf(y[,w])
    CDFy1 = c(CDFy1,cdfy1)
    
  }
}

for (r in 1:4) {
  
  t = 22
  listCDF[[r]] = CDFy1[((t*(r-1))+1):(t*r)]
  
}


#Plot cdf for SPAM 
options(tikzLatex = '/path/to/pdflatex')
install.packages('latex2exp')
library(latex2exp)
install.packages('tikzDevice')
library(tikzDevice)
tikz("tikz-test.tex",width=15/2.54,height=12/2.54)

setEPS()

# naming the eps file
Name = c("F.severe toxicity.eps", "F.identity attack.eps", "F.threat.eps", "F.likely to reject.eps", "F.spam.eps")

l=c(2,3,7,13,15)

for (n in 1:5){
  
  postscript(Name[n])
  
  Qx = environment(listCDF[[1]][[l[n]]])[["x"]]
  Qy = environment(listCDF[[1]][[l[n]]])[["y"]]
  
  par(las=1)
  dev.new(width=6,height=6,noRStudioGD=TRUE)
  
  line = c("solid","dashed", "dotted", "dotdash")
  
  plot(Qx, Qy, type="l", lty=line[1], xlab=TeX(r'($x$)'), ylab=TeX(r'($F_{n}(x)$)'), xlim=c(0,1), pch=19)
  
  grid ()
  for (x in 2:4){
    
    Vx = environment(listCDF[[x]][[l[n]]])[["x"]]
    Vy = environment(listCDF[[x]][[l[n]]])[["y"]]
    
    points(Vx, Vy, type="l", lty=line[x])
    
    legend("bottomright", legend=c("WHO", "WTO", "IMF", "NATO"), lty=line, cex=1)
    
  }
}
dev.off()


#analysis over cdfs 
environment(listCDF[[1]][[3]])[["x"]][which(environment(listCDF[[1]][[3]])[["y"]]>=0.9)]
min(environment(listCDF[[1]][[3]])[["x"]][which(environment(listCDF[[1]][[3]])[["y"]]>=0.9)])
min(environment(listCDF[[1]][[2]])[["x"]][which(environment(listCDF[[1]][[2]])[["y"]]>=0.9)])

#######

#statistical test
res1.2inizW = c()
res1.2inizT = c()

k1.2W = c()
k1.2T = c()

res1.2p.value.sign = c()
t1.2p.value.sign = c()

pW = c()
pT = c()
yW = c()
yT = c()

pW = c()
pT = c()
yW = c()
yT = c()

for (k in 1:22) {
  
  for (p in 1:(length(listseq)-1)) {
    
    for (y in 1:(length(listseq))){
      
      if (listG[[p]][[k]]<= 0.05){
        
        res1.2 <- wilcox.test(listseq[[p]][[k]], listseq[[y]][[k]], paired = FALSE)
        res1.2inizW = c(res1.2inizW, res1.2$p.value)
        if (res1.2$p.value<=0.05){ 
          res1.2p.value.sign = c(res1.2p.value.sign, res1.2$p.value)
          k1.2W = c(k1.2W,k)
          pW = c(pW,p)
          yW = c(yW,y)}
        
      }
      
      else  {
        
        if (listG[[p+1]][[k]]> 0.05){
          
          t1.2 = t.test(listseq[[p]][[k]], listseq[[y]][[k]], paired=FALSE)
          res1.2inizT = c(res1.2inizT,  t1.2$p.value)
          if (t1.2$p.value<=0.05){ 
            t1.2p.value.sign = c(t1.2p.value.sign,t1.2$p.value)
            k1.2T = c(k1.2T,k)
            pT = c(pT,p)
            yT = c(yT,y)}
          
        } 
        
      }
      
    }
    
  }
  
}

Significance=data.frame(pW,yW,k1.2W)

install.packages("writexl")
library("writexl")
#set the path
write_xlsx(Significance,"/Users/..../Desktop/Significance.xlsx")



########