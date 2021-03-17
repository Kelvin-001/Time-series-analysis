function [con,grotime,gromax] = logisticsd( xx,yy,name)
%% logistics拟合
%name = inputname(2);   %用于得到一个输入参数的标识符名字
%求初始解
d = min(yy);    %最小值
c = max(yy)-d;   %差值
half = fix(length(xx)/2);   %fix向0靠近取整,取中值half
f1=strcat(num2str(c),'/(1+exp(a0+(b0*',num2str(xx(half-3)),')))+',num2str(d),'=',num2str(yy(half-3)));    %表征物候期的指数公式,3d为一段
f2=strcat(num2str(c),'/(1+exp(a0+(b0*',num2str(xx(half+3)),')))+',num2str(d),'=',num2str(yy(half+3)));
[a0,b0]=solve(f1,f2,'a0,b0');  %求多项式解析式
a0=roundn(double(a0),-4);    %roundn四舍五入，小数点后四位
b0=roundn(double(b0),-4);
a0=a0(abs(imag(a0))<eps(a0));%剔除虚数根，abs(imag(a0)虚数部分绝对值
b0=b0(abs(imag(b0))<eps(b0));
beta0 = [a0 b0];    %初始解
%求拟合结果
ff=strcat(num2str(c),'./(1+exp(a(1)+(a(2).*xx)))+',num2str(d));    %表征物候期的指数
fun=inline(ff,'a','xx');      %用来定义内联函数，第一个参数是表达式，第三个是函数变量
beta = nlinfit(xx,yy,fun,beta0);   %beta：估计出的回归系数，beta0：回归系数初始值，nlinfit用于非线性拟合
con = [beta c d];      %参数集
x = xx(1):0.01:xx(length(xx));
y = fun(beta,x);
%绘图
figure    %能够创建一个用来显示图形输出的一个窗口对象
%subplot(211) 
plot(xx,yy,'r.',x,y)    
axis([min(x),max(x),min(y)*0.98,max(y)*1.02]);      %设置当前坐标轴x轴和y轴的限制范围
set(gca,'tickdir','out')    %gca返回当前axes对象的句柄值，set设置图像属性，tickdir控制尺标的位置，out尺标将会标在坐标轴外侧
xlabel('天数（DOY）','fontsize',18);  %x轴，fontsize字体大小18
ylabel(strcat(name),'fontsize',18);  %y轴，strcat水平串联字符串
set(gca,'LineWidth',2);   %指定线宽为2
set(gca, 'FontSize', 18);   %字体大小18
set(gca,'XTick',250:50:300); 
set(gcf,'Position',[100 100 600 250]);   %gcf返回当前figure对象的句柄值
%mean%grvi
%set(gca,'YTick',0.01:0.01:0.03);%%grvi
%set(gca,'YTick',0.45:0.1:0.55);%h
%per90
%set(gca,'YTick',0.2:0.2:1);%ExG
%set(gca,'YTick',0.85:0.05:0.95);%GCC
%set(gca,'YTickLabel',{'0.85','0.90','0.95'})%GCC
%set(gca,'YTick',0.55:0.05:0.65);%h
%set(gca,'YTickLabel',{'0.55','0.60','0.65'})%h
%set(gca,'YTick',0.06:0.01:0.08);%GRVI
box off   %隐藏图轴边框

set(gcf,'color','white'); %设定figure的背景颜色
A=getframe(gcf);  %获取整个窗口内容的图像

imwrite(A.cdata,strcat('G:\Pheno Result\re\',name,'d1.jpg')) %存储调整过大小的图片

imwrite(A.cdata,strcat('G:\Pheno Result\re\',name,'d1.jpg')) %存储调整过大小的图片

%% 斜率计算  
for i=1:(length(x)-1)  
   dx(i)=x(i+1)-x(i);  
   dy(i)=y(i+1)-y(i);              % 离散一次导（相当于连续一次导数）  
   dddy(i)= dy(i)/dx(i);      %△y/△x
end  
  
%% 曲率计算  
for i = 1 : (length(x)-2)  
   ddx(i) = dx(i+1) - dx(i);  
   ddy(i) = dy(i+1) - dy(i);      % 离散二次差分(相当于连续二次导)  
   K(i)=(dx(i)*ddy(i)-dy(i)*ddx(i))/((dx(i)*dx(i)+dy(i)*dy(i))^1.5); % 曲率，曲率半径k  
end     

%% 曲率导数计算 
n = 1;
for i=1:(length(x)-3)  
   kdx(i)=x(i+1)-x(i);  
   kdy(i)=K(i+1)-K(i);                
   ky(i)= kdy(i)/kdx(i);      %导数
   if i > 1 && ky(i)*ky(i-1) < 0
       grotime(n,2)=fix(ky(i));      %生长期的开始和结束
       grotime(n,1)=fix(x(i));
       n=n+1;
   end 
end

[gromax(1,2),loc1] = max(ky);   %loc位置属性
[gromax(2,2),loc2] = min(ky);
gromax(1,1)=x(loc1);            %生长最旺盛的时候
gromax(2,1)=x(loc2);
hold on       %在画完函数虚线之后保持曲线图（保持图像不被刷新）
figure       %能够创建一个用来显示图形输出的一个窗口对象
uk=linspace(xx(1),xx(length(xx)),(length(x)-3)); %linspace用于产生xx(1),xx(length(xx))之间的(length(x)-3)点行矢量
                                                 %xx(1),xx(length(xx))分别为起始值和中止值，(length(x)-3)为元素个数  
plot(uk,ky) 
axis([min(xx),max(xx),min(ky)*1.2,max(ky)*1.2]);     %设置当前坐标轴x轴和y轴的限制范围
hold on;        %在画完函数虚线之后保持曲线图
plot([xx(1),xx(length(xx))],[0,0],'-'); %画两个虚线
plot([gromax(1,1),gromax(1,1)],[gromax(1,2),0],'--'); %画最小点虚线

set(gca,'tickdir','out')   %gca返回当前axes对象的句柄值，set设置图像属性，tickdir控制尺标的位置，out尺标将会标在坐标轴外侧
hold off;    %取消原来的图
xlabel('天数（DOY）','fontsize',18);  %x轴
ylabel(strcat(name,'曲率导数'),'fontsize',18);   %y轴
box off   %隐藏图轴边框
set(gcf,'Name','Decline period')    %名字，衰退期
set(gca,'LineWidth',2);
set(gca, 'FontSize', 18);
set(gca,'XTick',200:50:350);
set(gcf,'Position',[100 100 600 250]);
%mean
%set(gca,'YTick',0:4e-6:8e-6);%exg
%set(gca,'YTick',0:10e-8:10e-8);%gcc
%set(gca,'YTick',0:4e-8:4e-8);%grvi
%set(gca,'YTick',0:4e-7:4e-7);%hue
%per90
%set(gca,'YTick',0:4e-6:4e-6);%exg
%set(gca,'YTick',0:4e-8:4e-8);%gcc
set(gca,'YTick',-1.4e-8:0.4e-8:-0.6e-8);%grvi
set(gcf,'color','white'); %设定figure的背景颜色
A=getframe(gcf);  %获取整个窗口内容的图像
imwrite(A.cdata,strcat('G:\Pheno Result\re\',name,'d2.jpg')) %存储调整过大小的图片
warning off MATLAB:xlswrite:AddSheet    %让matlab程序运行不显示警告

xlswrite(strcat('G:\Pheno Result\re\',name,'d.xlsx'), xx, 'xx'); 
xlswrite(strcat('G:\Pheno Result\re\',name,'d.xlsx'), yy, 'yy');
xlswrite(strcat('G:\Pheno Result\re\',name,'d.xlsx'), x, 'x'); 
xlswrite(strcat('G:\Pheno Result\re\',name,'d.xlsx'), y, 'y');
xlswrite(strcat('G:\Pheno Result\re\',name,'d.xlsx'), uk, 'uk'); 
xlswrite(strcat('G:\Pheno Result\re\',name,'d.xlsx'), ky, 'ky');
xlswrite(strcat('G:\Pheno Result\re\',name,'d.xlsx'), gromax, 'gromax');
xlswrite(strcat('G:\Pheno Result\re\',name,'d.xlsx'), con, 'con'); 
%grotime=unique(grotime,'rows');
xlswrite(strcat('G:\Pheno Result\re\',name,'d.xlsx'), grotime, 'grotime'); 


end

