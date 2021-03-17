
filepath<-file.path("/video");						
imgpath<-file.path("/picture");					
gpath<-file.path("/g");                              	
Gccpath<-file.path("/Gcc");                          
ExGpath<-file.path("/ExG");                         
GRVIpath<-file.path("/GRVI");					
numd = 0;

setwd("E://data/R_pro/")
videoname<-list.files(getwd(),pattern=("avi"));   

for i=1:1:length(videoname){
    videofile=videoname[i]; 
    greend=(i+numd); 
    for(j in c(1:24)){
        m<-j/12
        img<-system("ffmpeg.exe -i videofile -ss m -r 1 -t 1 %d.tif")
        if(greend<10){
            if(j<10){
                filename<-paste0("E://data/R_pro/","canum_day_00",greend,"_0",j,".tif")
                writeJPEG(img,target = filename,quality = 1)
                
            }else{
                filename<-paste0("E://data/R_pro/","canum_day_00",greend,"_",j,".tif")
                writeJPEG(img,target = filename,quality = 1)
            }
            
        }else if(greend < 100 && greend >= 10){
            if(j<10){
                filename<-paste0("E://data/R_pro/","canum_day_0",greend,"_0",j,".tif")
                writeJPEG(img,target = filename,quality = 1)
            }else{
                filename<-paste0("E://data/R_pro/","canum_day_0",greend,"_",j,".tif")
                writeJPEG(img,target = filename,quality = 1)
            }
            
        }else{
            if(j<10){
                filename<-paste0("E://data/R_pro/","canum_day_",greend,"_0",j,".tif")
                writeJPEG(img,target = filename,quality = 1)
            }else{
                filename<-paste0("E://data/R_pro/","canum_day_",greend,"_",j,".tif")
                writeJPEG(img,target = filename,quality = 1)
            }
        }
    }
}
picturename = dir(pattern=".tif")
