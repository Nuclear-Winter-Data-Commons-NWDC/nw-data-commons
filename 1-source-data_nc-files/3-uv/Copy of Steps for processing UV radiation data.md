**Processing country level UV radiation data**

UV radiation data for the 150 Tg US and Russia case as well as the control have been uploaded to [Data (temporary)](https://drive.google.com/drive/u/0/folders/1ygCctc9ICnq5eKKyS5gg5cr5fZ2yibna). 

* [uv\_150.nc](https://drive.google.com/open?id=1Pu1gjXgieVZ_ijO2SwXdp5PTWfkCFcP4&usp=drive_copy) (time=180,lat=96,lon=144)  
* [uv\_control.nc](https://drive.google.com/open?id=1mToNo-AHnzOlRMkdQIDf8LP531xgn-HW&usp=drive_copy) (time=180+,lat=96,lon=144)

Country level masks that were used for the crop model can be found in the above google drive folder under: [gadm0.mask.nc4](https://drive.google.com/file/d/1LoQ91TBY4mT3RuHLJ9NTnQhkPFqO2nrc/view?usp=sharing)   
Names for each country index can be found in the excel spreadsheet in the Google Drive folder: [nationIDname.xlsx](https://docs.google.com/spreadsheets/d/1_CFoZnUSyQfUNtMvxgWqv4eGNOGAn3Kr/edit?usp=sharing&ouid=118402062199571142375&rtpof=true&sd=true)

Ideally, the final product would be CSV files where the rows are countries and the columns are country-level averaged radiation variables in the netCDF files from the model (example  
[https://osf.io/rm268?view\_only=7cb9ba870fa8411cbb0ee36c2d3edb45](https://osf.io/rm268?view_only=7cb9ba870fa8411cbb0ee36c2d3edb45) )

gadm0.mask.nc4 should be used to calculate country-level averages for the following variables:

TUV\_UVA, TUV\_UVB, TUV\_UVC, TUV\_UVINDEX, TUV\_INDEXMX

for all 180 months in the time dimension.

**Possible workflow**

1) Read in netCDF file  
2) Read in country level masks  
3) Average together UV radiation data for each country level mask  
4) Output data country by country (row by row) and variable by variable (column by column) into a CSV file

**Notes from Victoria**  
Paths to the raw UV data are located at   
/glade/work/jcoupe/misc\_files/uv\_150.nc (150 Tg)  
/glade/work/jcoupe/misc\_files/uv\_stats.nc (5 Tg, not my naming convention)  
/glade/work/jcoupe/misc\_files/uv\_control.nc (control, nw\_cntrl\_03 I believe)

Information about regridding is located at /glade/work/jcoupe/misc\_files/nuclear\_data :  
gadm0.mask.nc4 was regridded into gadm0\_regrid.mask.nc4 in:  
/glade/work/jcoupe/misc\_files/nuclear\_data/gadm0\_regrid.mask.nc4  
scp [jcoupe@cheyenne.ucar.edu](mailto:jcoupe@cheyenne.ucar.edu):/glade/work/jcoupe/misc\_files/nuclear\_data/gadm0\_regrid.mask.nc4 .  
scp [jcoupe@cheyenne.ucar.edu](mailto:jcoupe@cheyenne.ucar.edu):/glade/work/jcoupe/misc\_files/nuclear\_data/gadm0\_gridarea.mask.nc4 .  
scp [jcoupe@cheyenne.ucar.edu](mailto:jcoupe@cheyenne.ucar.edu):/glade/work/jcoupe/misc\_files/nuclear\_data/nationIDname.csv .

Idea for figure: maximum UV index for each country in control run and 150 Tg simulation.   
scp [jcoupe@cheyenne.ucar.edu](mailto:jcoupe@cheyenne.ucar.edu):/glade/work/jcoupe/misc\_files/nuclear\_data/output/uv\_150\_TUV\_UVINDEX\_country\_mean.csv .

/glade/work/jcoupe/misc\_files/nuclear\_data/output/uv\_150\_TUV\_UVINDEX\_country\_mean.csv  
scp [jcoupe@cheyenne.ucar.edu](mailto:jcoupe@cheyenne.ucar.edu):/glade/work/jcoupe/misc\_files/nuclear\_data/uv\_control\_TUV\_UVINDEX\_country\_mean.csv .