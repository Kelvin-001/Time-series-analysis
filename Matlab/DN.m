rgb=imread('12.jpg');
gr=rgb2gray(rgb);
R=rgb(:,:,1);
DNR=mean(mean(R));
G=(rgb(:,:,2));
DNG=mean(mean(G));
B=(rgb(:,:,3));
DNB=mean(mean(B));
RGB=[DNR,DNG,DNB];
dlmwrite('12.txt',RGB,'precision','%.4f')
