#include<fstream>
#include<iostream>
#include<string>
#include<math.h>
#include<stdlib.h>

using namespace std;
#define ROW 354
#define COL 720

int mday[12]={31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};

float GetValue(char filename[], string maskname, string landarea, float &total, float &number, int is_annual, int is_packed)
{
//float Value = 0;
FILE *mask;
FILE *infile;
FILE *gridareafile;

mask = fopen(maskname.c_str(), "rb");
infile = fopen(filename, "rb");
gridareafile = fopen(landarea.c_str(), "rb");

float *maskvalue;
float *invalue;
float *gridareavalue;


int size2, read_size2;
fseek(gridareafile, 0L, SEEK_SET);
size2 = ROW * COL;
if((gridareavalue = (float *)malloc(size2 * sizeof(float))) == NULL)
return 0;

read_size2 = fread(gridareavalue, sizeof(float), size2, gridareafile);
fclose(gridareafile);
if(read_size2 != size2)
{
free(gridareavalue);
return 0;
}


int size1, read_size1;
fseek(mask, 0L, SEEK_SET);
size1 = ROW * COL;
if((maskvalue = (float *)malloc(size1 * sizeof(float))) == NULL)
return 0;

read_size1 = fread(maskvalue, sizeof(float), size1, mask);
fclose(mask);
if(read_size1 != size1)
{
free(maskvalue);
return 0;
}


int size, read_size;
fseek(infile, 0L, SEEK_SET);
if(is_annual == 0 & is_packed == 1) size = ROW * COL * 365;
//size = ROW * COL;
else size = ROW * COL;

if((invalue = (float *)malloc(size * sizeof(float))) == NULL)
return 0;
read_size = fread(invalue, sizeof(float), size, infile);
fclose(infile);

if(read_size != size)
{
free(invalue);
return 0;
}

float i = 0; float temp = 0.; int intnumber = 0;

if(is_annual == 0 & is_packed == 1) {
for(int n = 0; n < 365 * ROW * COL; n++)
        {
        if(maskvalue[n % (ROW * COL)] == 1)
                {
                temp += invalue[n] * gridareavalue[n % (ROW * COL)];
                i += gridareavalue[n % (ROW * COL)];
                intnumber++;
                }
        else
                {
                temp += 0;
                }
        }
}
else {
for(int n = 0; n < ROW * COL; n++)
        {
        if(maskvalue[n] == 1)
                {
                temp += invalue[n] * gridareavalue[n];
                i += gridareavalue[n];
                intnumber++;
                }
        else
                {
                temp += 0;
                }
        }
}
number = i;
total = temp;

free(maskvalue);
free(invalue);
free(gridareavalue);

fclose(mask);
fclose(infile);
fclose(gridareafile);

cout<<temp<<" "<<i<<" "<<intnumber<<endl;
if(number == 0) return 0;
else
return (total / number);
}

int main()
{
int row, col;
string prefix, suffix;
string outfilename;
string maskname;
string landareafile;
int is_annual;
int is_packed;

char filename[200];

int sy, sm, sd;
int ey, em, ed;
cout<<"Please input the start year, month, day for data reading: "<<endl;
cin>>sy>>sm>>sd;
cout<<"Please input the end date as year, month, day for data reading: "<<endl;
cin>>ey>>em>>ed;
cout<<"Please input the prefix file name for data reading: "<<endl;
cin>>prefix;
cout<<"Please input the surfix file name for data reading: "<<endl;
cin>>suffix;
cout<<"Please input the output file name: "<<endl;
cin>>outfilename;

cout<<"Please input the mask file name: "<<endl;
cin>>maskname;
cout<<"is the data annual basis or daily basis, 0 for daily, 1 for annual"<<endl;
cin>>is_annual;

cout<<"if the data is daily basis, how is it organized? packed or not, 1 for yes, 0 for no "<<endl;
cin>>is_packed;

cout<<"Please input the name of file for land area: "<<endl;
cin>>landareafile;

ofstream outfile;
outfile.open(outfilename.c_str());

float total;
float number;

for(int y = sy; y <= ey; y++)
{
if(is_annual == 0)
{
        if(is_packed == 1)
        {
        sprintf(filename, "%s\\y%d%s", prefix.c_str(), y, suffix.c_str());
        if(!filename) cout<<"could not open the file "<<filename<<endl;
        GetValue(filename, maskname, landareafile, total, number, is_annual, is_packed);
        outfile<<y<<" "<<total<<" "<<number<<endl;
        cout<<" year "<<y<<" data read finished! "<<endl;
        }
        else
        {
        for(int m = 1; m <= 12; m++)
        {
                for(int d = 1; d <= mday[m - 1]; d++)
                {
                        sprintf(filename, "%s\\y%d\\y%dm%dd%d%s", prefix.c_str(), y, y, m, d, suffix.c_str());
                        if(!filename) cout<<"could not open the file "<<filename<<endl;
                        GetValue(filename, maskname, landareafile, total, number, is_annual, is_packed);
                        outfile<<y<<" "<<m<<" "<<d<<" "<<total<<" "<<number<<endl;
                        cout<<" year "<<y<<" month "<<m<<" day "<<d<<" data read finished! "<<endl;
                }
        }
        }
}
else
        {
                        sprintf(filename, "%sy%d%s", prefix.c_str(), y, suffix.c_str());
                        GetValue(filename, maskname, landareafile, total, number, is_annual, is_packed);
                        outfile<<y<<" "<<total<<" "<<number<<endl;
                        cout<<" year "<<y<<" data read finished! "<<endl;
        }
}

outfile.close();
return 0;
}
