clear
data_table = readtable("SortedWithModelPredictionFromModelData.xlsx");
index = data_table.Index;
real_recieved_psd = data_table.real_recieved_psd;
transmitted_psd = data_table.transmitted_psd;
transformed_distance = data_table.transformed_distance;
pred_psd = data_table.pred_psd;

%X axis will be transformed distance
%Y axis will be transmitted_psd
%Z axis will be pred_psd
transformed_distance_axis = linspace(min(transformed_distance),max(transformed_distance), 100);
transmitted_psd_axis = linspace(min(transmitted_psd),max(transmitted_psd), 100);

% X is a 2D gird of x coordinates
% Y is a 2D gird of y coordinates
[X,Y] = meshgrid(transformed_distance_axis,transmitted_psd_axis);

% Z generated from the meshgrid
b0 = -8788.9303
b1 = 0.9993 % transmitted_psd's coef.
b2 = 8729.5698 % transformed_distance's coef.

% My axis order coeff order so I corrected it.
Z = b0 + b2*X + b1*Y;

z = b0 + b2*(500^(-0.001)) + b1*-60;

% Set Colormap
[viridis, n] = viridis(10);
colormap viridis;

fig = surf(X,Y,Z);

% Increase Font Size
fontsize(gcf,scale=1.3)

% Create zlabel
zlabel({'Interfering PSD (dBW / 200 MHz)'});
% Create ylabel
ylabel({'transmitted PSD (dBW / 200 MHz)'});
% Create xlabel
xlabel({'transformed distance'});

