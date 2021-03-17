rm(list = ls())

install.packages("jpeg")
install.packages("tiff")
install.packages("rJava")
install.packages("xlsx")
install.packages("BB")
library(jpeg)
library(tiff)
library(rJava)
library(xlsx)
library(BB)

logisticsg<-function(xx,yy,name){
  d<-min(yy);
  c<-max(yy)-d;
  half<-floor(length(xx)/2);
  func<-function(coe){
    f<-numeric(length(coe));
    f[1]<-c/(1+exp(coe[1]+(coe[2]*xx[half-3])))+d-yy[half-3];
    f[2]<-c/(1+exp(coe[1]+(coe[2]*xx[half+3])))+d-yy[half+3];
  }
  s1<-trunc(length(xx)/2-3);
  s2<-trunc(length(xx)/2+3);
  startx<-c(yy[s1],yy[s2]);
  result<-dfsane(startx,func,control = list(maxit=1,trace=FALSE));
  theta = result$par;
  a0<-round(theta[1], digits = 4);
  b0<-round(theta[2], digits = 4);
  #剔除虚数根
  new1<-c();
  new2<-c();
  for(i in c(1:length(a0))){
    if(Im(a0[i])==0){
      new1[i]<-a0[i];
    }
  }
  for(j in c(1:length(b0))){
    if(Im(b0[j])==0){
      new2[j]<-b0[j];
    }
  }
  new<-c(new1,new2);
  real<-new[-which(is.na(new))];
  beta0<-c(Re(real));
  #求拟合结果
  a_1<-0;
  a_2<-0;
  ff<-c/(1+exp(a_1+a_2*xx))+d;
  fun<-function(a,xx){
    a_1<-a[1];
    a_2<-a[2];
    return(ff)
  }
  beta_inf<-nls(ff~c/(1+exp(a_1+a_2*xx))+d,start=list(a_1<-a0,a_2 <- b0),data = list(xx,yy),trace = TRUE);
  beta<-coef(beta_inf);
  con<-c(beta,c,d);
  x<-seq(xx[1],xx[length(xx)],0.01);
  y<-fun(beta,x);
  #绘图
  g1_path<-paste0("G://PhenoResult/re/",name,"g1.jpg");
  jpeg(file=g1_path);     #绘制图片的存放路径
  plot(x,y,type = "l",col=6,xlab = "天数(DOY)",ylab = name,cex = 18,xaxt="n",lwd=2);
  axis(1,seq(100,300,50));
  points(xx,yy,pch=16,col=2);
  dev.off();
  #斜率计算
  dx<-c();
  dy<c();
  dddy<-c();
  for(i in (1:length(x)-1)){
    dx[i]<-x[i+1]-x[i];
    dy[i]<-y[i+1]-y[i];
    dddy[i]<-dy[i]/dx[i];
  }
  
  #曲率计算
  ddx<-c();
  ddy<c();
  k<-c();
  for(i in (1:length(x)-2)){
    ddx[i] = dx[i+1] - dx[i];
    ddy[i] = dy[i+1] - dy[i];      
    K[i]=(dx[i]*ddy[i]-dy[i]*ddx[i])/((dx[i]*dx[i]+dy[i]*dy[i])^1.5);
  }
  
  #曲率导数计算 
  n=1;
  kdx<-c();
  kdy<-c();
  ky<-c();
  grotime<-matrix();
  gromax<-matrix();
  for(i in (1:length(x)-3)){
    kdx[i]=x[i+1]-x[i];  
    kdy[i]=K[i+1]-K[i];               
    ky[i]= kdy[i]/kdx[i];
    if(i>1&& ky[i]*ky[i-1] < 0){
      #if(ky[i]>0){
      #grotime[n,2]=floor(ky[i]);
      #}else{
      #  grotime[n,2]=ceiling(ky[i]);
      #}
      #if(x[i]>0){
      #  grotime[n,1]=floor(x[i]);
      #}else{
      #  grotime[n,1]=ceiling(x[i]);
      #}
      grotime[n,2]=trunc(ky[i]);      #生长期的开始和结束
      grotime[n,1]=trunc(x[i]);
      n=n+1;
    }
  }
  
  gromax[1,2]<-max(ky);
  loc1<-which.max(ky);
  gromax[2,2]<-min(ky);
  loc2<-which.min(ky);
  gromax[1,1]<-x[loc1];         
  gromax[2,1]<-x[loc2];
  
  #绘图
  g2_path<-paste0("G://PhenoResult/re/",name,"g2.jpg");
  jpeg(file=g2_path);   #绘制图片的存放路径
  uk<-seq(xx[1],xx[length(xx)],length(x)-3);
  plot(uk,ky,"l",xlab = "天数（DOY）",ylab=name,"曲率导数",main = "period of growth",cex = 18,xaxt="n",lwd=2);
  axis(1,seq(100,200,50));
  xu1<-c(xx[1],xx[length(xx)]);
  yu1<-c(0,0);
  points(xu1,yu1,"l",lty=5);
  xu2<-c(gromax[2,1],gromax[2,1]);
  yu2<-c(gromax[2,2],0);
  points(xu2,yu2,"l",lty=5);
  dev.off();
  #保存
  xx_path<-paste0("G://PhenoResult/re/",name,"g_xx.xlsx");
  write.xlsx(xx,xx_path);
  yy_path<-paste0("G://PhenoResult/re/",name,"g_yy.xlsx");
  write.xlsx(yy,yy_path);
  x_path<-paste0("G://PhenoResult/re/",name,"g_x.xlsx");
  write.xlsx(x,x_path);
  y_path<-paste0("G://PhenoResult/re/",name,"g_y.xlsx");
  write.xlsx(y,y_path);
  uk_path<-paste0("G://PhenoResult/re/",name,"g_uk.xlsx");
  write.xlsx(uk,uk_path);
  ky_path<-paste0("G://PhenoResult/re/",name,"g_ky.xlsx");
  write.xlsx(ky,ky_path);
  gromax_path<-paste0("G://PhenoResult/re/",name,"g_gromax.xlsx");
  write.xlsx(gromax,gromax_path);
  #grotime_path<-paste0("G://PhenoResult/re/",name,"g_grotime.xlsx");
  #write.xlsx(grotime,grotime_path);
  #con_path<-paste0("G://PhenoResult/re/",name,"g_con.xlsx");
  #write.xlsx(con,con_path);
  #I_path<-paste0("G://PhenoResult/re/",name,"g_I.xlsx");
  #write.xlsx(I,I_path);
}