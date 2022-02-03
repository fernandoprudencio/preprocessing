path_ft = 'C:\Users\ASUS\Documents\TESIS\parsivel\PSVTB';
addpath(path_ft)

% direccion de los archivos:
ruta2='C:\Users\ASUS\Documents\TESIS\parsivel\psvb\';

%parsivel = PSVncread(ruta2);

% lectura de PSV2a
files1=dir([ruta2,'*.nc']);
%files1([files1.bytes]~=5209)=[];
for ii = 1:length(files1)
    filename=[ruta2,files1(ii).name];
    datos(ii)=PSVncread(filename);
end
PSV2b=catPSVstruct(datos); %hecha para el txt.

writematrix(nueva_mtrxb,"psvb2.xls")
writematrix(nueva_mtrxb, 'psvb2.csv', 'Delimiter', ',')
csvwrite('psvb2.csv', nueva_mtrxb)
%% Plot raw data
figure(1)
plotrawdata(PSV2b.D,PSV2b.vel,PSV2b.raw)
%% Plot DSD vs time
figure(2)
plotDSDbytime(PSV2b.time,PSV2b.D,PSV2b.N_d)
%% Mean DSD
figure(3)
plotmeanDSD(PSV2b.D,PSV2b.N_d)

tiempo = datevec(PSV2b.time(:));
matrix_pb = [tiempo PSV2b.RI(:) PSV2b.dbZ(:) PSV2b.SYNOP4680(:)];

save PSVdata matrix_pb

