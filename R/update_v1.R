rm(list = ls())

library(jpeg)
library(tiff)
library(rJava)
library(xlsx)
library(BB)

PhenoIndex<-function(path,canum){
  #path为所有文件所在路径,应为“”格式
  #处理视频
  filepath<-file.path(path,"video")
  preimgpath<-file.path(path,"preimgpath")#视频帧的存放路径
  imgpath<-file.path(path,"picture")
  gpath<-file.path(path,"g")                         	
  Gccpath<-file.path(path,"Gcc")                        
  ExGpath<-file.path(path,"ExG")                         
  GRVIpath<-file.path(path,"GRVI")					
  numd = 0
  
  setwd(filepath)   #获取视频所在地址
  videoname<-list.files(getwd(),pattern=(".AVI"))   
  name<-paste0(filepath,videoname)   #设置读取视频的路径
  
  for (i in c(1:length(videoname))){
    #videofile=videoname[i]
    name<-paste0(filepath,videoname)
    greend=(i+numd)
    #提取视频帧，在linux系统中用ffmpeg而不是ffmpeg.exe
    shellrun <- paste0("ffmpeg.exe -i ",name[i]," -ss 00:00:00  -r 12   -vframes 24  specifiedpath/TCL_",i,"_","%d.tif") #这里specifiedpath/TCL_用preimgpath的绝对路径/TCL_   
    system(shellrun)
    
    for(j in c(1:24)){
      img_path<-paste0( preimgpath,"/TCL_",i,"_",j,".tif")   #读取的视频帧图片的存放路径
      img<-readTIFF(img_path)
      if(greend<10){
        if(j<10){
          filename<-paste0(imgpath,"canum_day_00",greend,"_0",j,".tif")
          writeTIFF(img,filename)
          
        }else{
          filename<-paste0(imgpath,"canum_day_00",greend,"_",j,".tif")
          writeTIFF(img,filename)
        }
        
      }else if(greend < 100 && greend >= 10){
        if(j<10){
          filename<-paste0(imgpath,"canum_day_0",greend,"_0",j,".tif")
          writeTIFF(img,filename)
        }else{
          filename<-paste0(imgpath,"canum_day_0",greend,"_",j,".tif")
          writeTIFF(img,filename)
        }
        
      }else{
        if(j<10){
          filename<-paste0(imgpath,"canum_day_",greend,"_0",j,".tif")
          writeTIFF(img,filename)
        }else{
          filename<-paste0(imgpath,"canum_day_",greend,"_",j,".tif")
          writeTIFF(img,filename)
        }
      }
    }
  }
  #'Video cut is over'
  
  #处理图片
  setwd(imgpath)   #获取图片所在路径
  picturename = dir(pattern=".tif")
  for (k in c(1:length(picturename))){
    readfile_path<-paste0(imgpath,picturename[k])
    images<-readTIFF(readfile_path) #读取图片
    #test<-readJPEG(imgpath,"/R_pro/canum_day_121_01.tif");
    r<-images[,,1]
    g<-images[,,2]
    b<-images[,,3]
    Gcc_function<-function(r,g,b){
      return(g/r+g+b)
    }
    ExG_function<-function(r,g,b){
      return(2*g-(r+b))
    }
    GRVI_function<-function(r,g){
      return((g-r)/(g+r))
    }
    writeg_path<-paste0(gpath,picturename[k])
    writeTIFF(g,writeg_path)
    writegcc_path<-paste0(Gccpath,picturename[k])
    writeTIFF(Gcc_function(r,g,b), writegcc_path)
    writeexg_path<-paste0(ExGpath,picturename[k])
    writeTIFF(ExG_function(r,g,b),writeexg_path)
    writegrvi_path<-paste0(GRVIpath,picturename[k])
    writeTIFF(GRVI_function(r,g),writegrvi_path)
  }
  #'PhenoIndex is over'
}

  
  logisticsg<-function(xx,yy,name){
    d<-min(yy)
    c<-max(yy)-d
    half<-floor(length(xx)/2)
    func<-function(coe){
      f<-numeric(length(coe))
      f[1]<-c/(1+exp(coe[1]+(coe[2]*xx[half-3])))+d-yy[half-3]
      f[2]<-c/(1+exp(coe[1]+(coe[2]*xx[half+3])))+d-yy[half+3]
    }
    s1<-trunc(length(xx)/2-3)
    s2<-trunc(length(xx)/2+3)
    startx<-c(yy[s1],yy[s2])
    result<-dfsane(startx,func,control = list(maxit=1,trace=FALSE))
    theta = result$par
    a0<-round(theta[1], digits = 4)
    b0<-round(theta[2], digits = 4)
    #剔除虚数根
    new1<-c()
    new2<-c()
    for(i in c(1:length(a0))){
      if(Im(a0[i])==0){
        new1[i]<-a0[i]
      }
    }
    for(j in c(1:length(b0))){
      if(Im(b0[j])==0){
        new2[j]<-b0[j]
      }
    }
    new<-c(new1,new2)
    real<-new[-which(is.na(new))]
    beta0<-c(Re(real))
    #a0<-a0-a0*(is.complex(a0))
    #b0<-b0-b0*(is.complex(b0))
    #beta0<-c(new_a0,new_b0);
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
    
    #c(gromax[1,2],loc1)<-max(ky);
    #c(gromax[2,2],loc2)<-min(ky);
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
  
  
  logisticsd<-function(xx,yy,name){
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
    #a0<-a0-a0*(is.complex(a0));
    #b0<-b0-b0*(is.complex(b0));
    #beta0<-c(a0,b0);
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
    d1_path<-paste0("G://PhenoResult/re/",name,"d1.jpg")
    jpeg(file=d1_path)   #绘制图像的存储路径
    plot(x,y,type = "l",col=6,xlab = "天数(DOY)",ylab = name,cex = 18,xaxt="n",lwd=2)
    axis(1,seq(100,300,50))
    points(xx,yy,pch=16,col=2)
    dev.off()
    #斜率计算
    dx<-c()
    dy<-c()
    dddy<-c()
    for(i in (1:length(x)-1)){
      dx[i]<-x[i+1]-x[i];
      dy[i]<-y[i+1]-y[i];
      dddy[i]<-dy[i]/dx[i];
    }
    #曲率计算
    ddx<-c();
    ddy<-c();
    k<-c();
    for(i in (1:length(x)-2)){
      ddx[i] = dx[i+1] - dx[i]; 
      ddy[i] = dy[i+1] - dy[i];      
      K[i]=(dx[i]*ddy[i]-dy[i]*ddx[i])/((dx[i]*dx[i]+dy[i]*dy[i])^1.5);
    }
    
    #曲率导数计算 
    n=1;
    kdx<-c();
    kdy<c();
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
        #grotime[n,2]=ceiling(ky[i]);
        #}
        #if(x[i]>0){
        #grotime[n,1]=floor(x[i]);
        #}else{
        #grotime[n,1]=ceiling(x[i]);
        # }
        grotime[n,2]=trunc(ky[i]);      #生长期的开始和结束
        grotime[n,1]=trunc(x[i]);
        n=n+1;
      }
    }
    
    
    #c(gromax[1,2],loc1)<-max(ky);
    #c(gromax[2,2],loc2)<-min(ky);
    gromax[1,2]<-max(ky);
    loc1<-which.max(ky);
    gromax[2,2]<-min(ky);
    loc2<-which.min(ky);
    gromax[1,1]<-x[loc1];          
    gromax[2,1]<-x[loc2];
    
    #绘图
    d2_path<-paste0("G://PhenoResult/re/",name,"d2.jpg");
    jpeg(file=d2_path);    #绘制图片的存放路径
    uk<-seq(xx[1],xx[length(xx)],length(x)-3);
    plot(uk,ky,"l",xlab = "天数（DOY）",ylab = name,"曲率导数",main = "Decline period",cex = 18,xaxt="n",lwd=2);
    axis(1,seq(200,350,50));
    axis(2,seq(-1.4e-8,-0.6e-8,0.4e-8));
    xu1<-c(xx[1],xx[length(xx)]);
    yu1<-c(0,0);
    points(xu1,yu1,"l",lty=1);
    xu2<-c(gromax[1,1],gromax[1,1]);
    yu2<-c(gromax[1,2],0);
    points(xu2,yu2,"l",lty=5);
    dev.off();
    #保存
    xx_path<-paste0("G://Pheno Result/re/",name,"d_xx.xlsx");
    write.xlsx(xx,xx_path);
    yy_path<-paste0("G://Pheno Result/re/",name,"d_yy.xlsx");
    write.xlsx(yy,yy_path);
    x_path<-paste0("G://Pheno Result/re/",name,"d_x.xlsx");
    write.xlsx(x,x_path);
    y_path<-paste0("G://Pheno Result/re/",name,"d_y.xlsx");
    write.xlsx(y,y_path);
    uk_path<-paste0("G://Pheno Result/re/",name,"d_uk.xlsx");
    write.xlsx(uk,uk_path);
    ky_path<-paste0("G://Pheno Result/re/",name,"d_ky.xlsx");
    write.xlsx(ky,ky_path);
    gromax_path<-paste0("G://Pheno Result/re/",name,"d_gromax.xlsx");
    write.xlsx(gromax,gromax_path);
    grotime_path<-paste0("G://Pheno Result/re/",name,"d_grotime.xlsx");
    write.xlsx(grotime,grotime_path);
    con_path<-paste0("G://Pheno Result/re/",name,"d_con.xlsx");
    write.xlsx(con,con_path);
  }
  
  
  logistics<-function(y,thres){
    shadow<-function(y,name,thres){
      return(xx)
      return(yy)
    }
    half<-floor(length(xx)/2);
    setwd(path);
    source("update000.R");
    xx1<-xx[half:length(xx)];
    yy1<-yy[half:length(xx)];
    logisticsd(xx1,yy1,name)
    cond<-con;
    grotimed<-grotime;
    gromaxd<-gromax;
  }

