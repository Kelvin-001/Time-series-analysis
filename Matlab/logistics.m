function  logistics( y,thres)    %thres阈值（临界值）
name = inputname(1);   %用于得到一个输入参数的标识符名字
[xx,yy]=shadow( y,name,thres);    %阴影
half = fix(length(xx)/2);   %fix向0靠近取整,取中值half
%[cong,grotimeg,gromaxg] = logisticsg( xx(1:half),yy(1:half),name)  %生长期
[cond,grotimed,gromaxd] = logisticsd( xx(half:length(xx)),yy(half:length(xx)),name)   %衰减期
end

