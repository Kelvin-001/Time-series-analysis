clc;
clear;
     fileName = 'TLC00010.AVI';
    obj = VideoReader(fileName);
     numFrames = obj.NumberOfFrames;% 帧的总数
     for k = 1 : numFrames% 读取数据
     frame = read(obj,k);
     imshow(frame);%显示帧
     imwrite(frame,strcat(num2str(k),'.jpg'),'jpg');% 保存帧
end
