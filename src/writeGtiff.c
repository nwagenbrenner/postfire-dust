#include "gdal.h"
#include <boost/lexical_cast.hpp>
#include <iostream>

 int main()
 {
     //const char *pszTimeStamp = "561535";
     GDALDatasetH hDS = NULL;
     GDALRasterBandH hBand = NULL;
     GDALDriverH hDriver;

     int i;

     GDALAllRegister();
     hDriver = GDALGetDriverByName( "GTiff" );
     hDS = GDALCreate( hDriver, "test.tif", 1, 1, 9000, GDT_Float64, NULL );

     for( i = 0; i < 9000; i++ )
     {
         std::string s(boost::lexical_cast<std::string>(i));
         std::cout<<"s = "<<s.c_str()<<std::endl;
         hBand = GDALGetRasterBand( hDS, i + 1 );
         GDALSetMetadataItem( hBand, "DT", s.c_str(), NULL );
     }
     GDALClose( hDS );
     return 0;
 }
