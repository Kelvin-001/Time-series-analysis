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

PhenoIndex<-function(path,canum){
  #path为所有文件所在路径,应为“”格式
  #处理视频
  filepath<-file.path(path,"video");
  preimgpath<-file.path(path,"preimgpath")   #视频帧的存放路径
  imgpath<-file.path(path,"picture");
  gpath<-file.path(path,"g");                              	
  Gccpath<-file.path(path,"Gcc");                          
  ExGpath<-file.path(path,"ExG");                         
  GRVIpath<-file.path(path,"GRVI");					
  numd = 0;
  
  setwd(filepath);   #获取视频所在地址
  videoname<-list.files(getwd(),pattern=(".AVI"));   
  name<-paste0(filepath,videoname)   #设置读取视频的路径
  
  for (i in c(1:length(videoname))){
    name<-paste0(filepath,videoname);
    greend=(i+numd);
    #提取视频帧，在linux系统中用ffmpeg而不是ffmpeg.exe
    shellrun <- paste0("ffmpeg.exe -i ",name[i]," -ss 00:00:00  -r 12   -vframes 24 ",preimgpath,"/TCL_",i,"_","%d.tif");   #vframes 24与后边的引号之间要有空格   
    system(shellrun);
    
    for(j in c(1:24)){
      img_path<-paste0( preimgpath,"/TCL_",i,"_",j,".tif");   #读取的视频帧图片的存放路径
      img<-readTIFF(img_path);
      if(greend<10){
        if(j<10){
          filename<-paste0(imgpath,"canum_day_00",greend,"_0",j,".tif");
          writeTIFF(img,filename);
          
        }else{
          filename<-paste0(imgpath,"canum_day_00",greend,"_",j,".tif");
          writeTIFF(img,filename);
        }
        
      }else if(greend < 100 && greend >= 10){
        if(j<10){
          filename<-paste0(imgpath,"canum_day_0",greend,"_0",j,".tif");
          writeTIFF(img,filename);
        }else{
          filename<-paste0(imgpath,"canum_day_0",greend,"_",j,".tif");
          writeTIFF(img,filename);
        }
        
      }else{
        if(j<10){
          filename<-paste0(imgpath,"canum_day_",greend,"_0",j,".tif");
          writeTIFF(img,filename);
        }else{
          filename<-paste0(imgpath,"canum_day_",greend,"_",j,".tif");
          writeTIFF(img,filename);
        }
      }
    }
  }
  #'Video cut is over'
  
  #处理图片
  setwd(imgpath);   #获取图片所在路径
  picturename = dir(pattern=".tif");
  for (k in c(1:length(picturename))){
    readfile_path<-paste0(imgpath,picturename[k]);
    images<-readTIFF(readfile_path);   #读取图片
    r<-images[,,1];
    g<-images[,,2];
    b<-images[,,3];
    Gcc_function<-function(r,g,b){
      return(g/r+g+b)
    }
    ExG_function<-function(r,g,b){
      return(2*g-(r+b))
    }
    GRVI_function<-function(r,g){
      return((g-r)/(g+r))
    }
    writeg_path<-paste0(gpath,picturename[k]);
    writeTIFF(g,writeg_path);
    writegcc_path<-paste0(Gccpath,picturename[k]);
    writeTIFF(Gcc_function(r,g,b), writegcc_path);
    writeexg_path<-paste0(ExGpath,picturename[k]);
    writeTIFF(ExG_function(r,g,b),writeexg_path);
    writegrvi_path<-paste0(GRVIpath,picturename[k]);
    writeTIFF(GRVI_function(r,g),writegrvi_path);
  }
  #'PhenoIndex is over'
}