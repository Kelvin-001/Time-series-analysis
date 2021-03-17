function PhenoIndex( path,canum )
%path为所有文件所在路径;
%   必须包括视频所在位置video;
%   生成图片的存储位置picture;
%   生成绿色波段的存储位置g;
%   生成相对绿度指数的存储位置Gcc,Gcc=g/(r+g+b);
%   生成绝对绿度指数的存储位置ExG,ExG=2*g-(r+b);
%   生成绿红植被指数的存储位置GRVI,GRVI=（g-r)/(g+r);
%   canum为相机编号，为生成图片存储名称使用

filepath = strcat(path,'\video');						%视频文件位置	
imgpath = strcat(path,'\picture');						%照片文件位置
gpath = strcat(path,'\g');                              %绿色波段图文件位置	
Gccpath = strcat(path,'\Gcc');                          %相对绿度指数图文件位置
ExGpath = strcat(path,'\ExG');                          %绝对绿度指数图文件位置
GRVIpath = strcat(path,'\GRVI');						%绿红植被指数图文件位置
numd = 0;											   	%当同一年文件在不同文件夹时使用，是前一个文件夹的天数

videoname = dir(strcat(filepath,'\*.avi'));             %获取视频文件路径下的所有文件后缀为.avi的文件名称
                                                        %dir获取指定文件夹下所有子文件夹和文件
for i=1:1:length(videoname)                             %正常matlab文件路径获取时前两个为'.’'..'，但是本次只取.avi
    videofile=strcat(filepath,'\',videoname(i).name);   %获取第i个视频文件的文件名
    m=VideoReader(videofile);                           %读取视频文件
    greend=(i+numd);                                    %为图片文件存储文件名的天数
    
    %为了使文件按时间书序存储，需要将小于10的天数前加00，即生成001形式；大于10小于100前加0，生成010形式；后正常存储即可。
    %同一天内24张图片排序同样使用这种方法。
    for j=1:1:24
        img=read(m,j);
        if greend <10 %需注意此处应为数值比较
            if j < 10									  
                 imwrite(img,strcat(imgpath,'\',canum,'_day00',num2str(greend),'_0',num2str(j),'.tif'));   %一天内图片顺序小于10存为01形式
            else
                imwrite(img,strcat(imgpath,'\',canum,'_day00',num2str(greend),'_',num2str(j),'.tif'));
            end
        elseif greend < 100 && greend >= 10;
            if j < 10									  
                 imwrite(img,strcat(imgpath,'\',canum,'_day0',num2str(greend),'_0',num2str(j),'.tif'));   
            else
                imwrite(img,strcat(imgpath,'\',canum,'_day0',num2str(greend),'_',num2str(j),'.tif'));
            end
        else   
            if j < 10									 
                 imwrite(img,strcat(imgpath,'\',canum,'_day',num2str(greendd),'_0',num2str(j),'.tif'));   
            else
                imwrite(img,strcat(imgpath,'\',canum,'_day',num2str(greend),'_',num2str(j),'.tif'));
            end
        end
    end
end
'Video cut is over';    %视频分割

picturename = dir(strcat(imgpath,'\*.tif'));                    %提取图片文件夹下.tif格式的文件名
for k=1:1:length(picturename)
    image = imread(strcat(imgpath,'\',picturename(k).name));
    r = image(:,:,1);
    g = image(:,:,2);
    b = image(:,:,3);                                           %提取图片rgb三个波段
    Gcc=double(g)./(double(r)+double(b)+double(g));
    ExG = (2.*double(g))-(double(r)+double(b));	
    GRVI = double((double(g)-double(r))./(double(g)+double(r)));
    imwrite(g,strcat(gpath,'\',picturename(k).name));
    imwrite(Gcc,strcat(Gccpath,'\',picturename(k).name));
    imwrite(ExG,strcat(ExGpath,'\',picturename(k).name));
    imwrite(GRVI,strcat(GRVIpath,'\',picturename(k).name));      %将生成的图片存储到目的位置
end
'PhenoIndex is over'   %现象指数

end

