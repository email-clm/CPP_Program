/*
Simple linear regression for regional output
all the algorithm was based on the equations at http://en.wikipedia.org/wiki/Simple_linear_regression
Xiaofeng Xu
December, 2009
*/

#include <math>
#include <iostream>
#include <string>
#include <fstream>
#include <vector>

using namespace std;

#define NUM 11

float Sx, Sy, Sxx, Sxy, Syy;
float Mslope, Minter, SEslope, SEinter;

int ncols = 720;
int nrows = 360;
float xllcorner = -180;
float yllcorner = -90;
float cellsize = 0.5;
float NODATA = -9999;
string scols;
string srows;
string sxllcorner;
string syllcorner;
string scellsize;
string snodate;

void main()
{
cout<<NUM<<" data point will be processed for simple linear regression "<<endl;
string filename[NUM];
        for(int i = 0; i < NUM; i++)
	{
	cout<<(i+1)<<" filename: "<<endl;
	cin>>filename[i];
	}
        vector<vector<float> >value1(nrows, vector<float>(ncols));
        vector<vector<float> >value2(nrows, vector<float>(ncols));
        vector<vector<float> >value3(nrows, vector<float>(ncols));
        vector<vector<float> >value4(nrows, vector<float>(ncols));
        vector<vector<float> >value5(nrows, vector<float>(ncols));
        vector<vector<float> >value6(nrows, vector<float>(ncols));
        vector<vector<float> >value7(nrows, vector<float>(ncols));
        vector<vector<float> >value8(nrows, vector<float>(ncols));
        vector<vector<float> >value9(nrows, vector<float>(ncols));
        vector<vector<float> >value10(nrows, vector<float>(ncols));
        vector<vector<float> >value11(nrows, vector<float>(ncols));
/*
        vector<vector<float> >value12(nrows, vector<float>(ncols));
        vector<vector<float> >value13(nrows, vector<float>(ncols));
        vector<vector<float> >value14(nrows, vector<float>(ncols));
        vector<vector<float> >value15(nrows, vector<float>(ncols));
        vector<vector<float> >value16(nrows, vector<float>(ncols));
        vector<vector<float> >value17(nrows, vector<float>(ncols));
        vector<vector<float> >value18(nrows, vector<float>(ncols));
        vector<vector<float> >value19(nrows, vector<float>(ncols));
        vector<vector<float> >value20(nrows, vector<float>(ncols));
        vector<vector<float> >value21(nrows, vector<float>(ncols));
        vector<vector<float> >value22(nrows, vector<float>(ncols));
        vector<vector<float> >value23(nrows, vector<float>(ncols));
        vector<vector<float> >value24(nrows, vector<float>(ncols));
        vector<vector<float> >value25(nrows, vector<float>(ncols));
        vector<vector<float> >value26(nrows, vector<float>(ncols));
        vector<vector<float> >value27(nrows, vector<float>(ncols));
        vector<vector<float> >value28(nrows, vector<float>(ncols));
        vector<vector<float> >value29(nrows, vector<float>(ncols));
        vector<vector<float> >value30(nrows, vector<float>(ncols));
*/

vector<vector<float> >outputMS(nrows, vector<float>(ncols));
vector<vector<float> >outputMI(nrows, vector<float>(ncols));
vector<vector<float> >outputSSE(nrows, vector<float>(ncols));
vector<vector<float> >outputISE(nrows, vector<float>(ncols));


ifstream infile1;
infile1.open(filename[0].c_str());
ifstream infile2;
infile2.open(filename[1].c_str());
ifstream infile3;
infile3.open(filename[2].c_str());
ifstream infile4;
infile4.open(filename[3].c_str());
ifstream infile5;
infile5.open(filename[4].c_str());
ifstream infile6;
infile6.open(filename[5].c_str());
ifstream infile7;
infile7.open(filename[6].c_str());
ifstream infile8;
infile8.open(filename[7].c_str());
ifstream infile9;
infile9.open(filename[8].c_str());
ifstream infile10;
infile10.open(filename[9].c_str());
ifstream infile11;
infile11.open(filename[10].c_str());

/*
ifstream infile12;
infile12.open(filename[11].c_str());
ifstream infile13;
infile13.open(filename[12].c_str());
ifstream infile14;
infile14.open(filename[13].c_str());
ifstream infile15;
infile15.open(filename[14].c_str());
ifstream infile16;
infile16.open(filename[15].c_str());
ifstream infile17;
infile17.open(filename[16].c_str());
ifstream infile18;
infile18.open(filename[17].c_str());
ifstream infile19;
infile19.open(filename[18].c_str());
ifstream infile20;
infile20.open(filename[19].c_str());
ifstream infile21;
infile21.open(filename[20].c_str());
ifstream infile22;
infile22.open(filename[21].c_str());
ifstream infile23;
infile23.open(filename[22].c_str());
ifstream infile24;
infile24.open(filename[23].c_str());
ifstream infile25;
infile25.open(filename[24].c_str());
ifstream infile26;
infile26.open(filename[25].c_str());
ifstream infile27;
infile27.open(filename[26].c_str());
ifstream infile28;
infile28.open(filename[27].c_str());
ifstream infile29;
infile29.open(filename[28].c_str());
ifstream infile30;
infile30.open(filename[29].c_str());
*/
ofstream outfile1;
outfile1.open("meanslope.asc");
ofstream outfile2;
outfile2.open("meaninter.asc");
ofstream outfile3;
outfile3.open("seslope.asc");
ofstream outfile4;
outfile4.open("seinter.asc");



infile1>>scols>>ncols;
infile1>>srows>>nrows;
infile1>>sxllcorner>>xllcorner;
infile1>>syllcorner>>yllcorner;
infile1>>scellsize>>cellsize;
infile1>>snodate>>NODATA;
for(int i = 0; i < nrows; i++)
{
        for(int j = 0; j < ncols; j++)
        {
        infile1>>value1[i][j];
        }
}

infile2>>scols>>ncols;
infile2>>srows>>nrows;
infile2>>sxllcorner>>xllcorner;
infile2>>syllcorner>>yllcorner;
infile2>>scellsize>>cellsize;
infile2>>snodate>>NODATA;
for(int i = 0; i < nrows; i++)
{
        for(int j = 0; j < ncols; j++)
        {
        infile2>>value2[i][j];
        }
}

infile3>>scols>>ncols;
infile3>>srows>>nrows;
infile3>>sxllcorner>>xllcorner;
infile3>>syllcorner>>yllcorner;
infile3>>scellsize>>cellsize;
infile3>>snodate>>NODATA;
for(int i = 0; i < nrows; i++)
{
        for(int j = 0; j < ncols; j++)
        {
        infile3>>value3[i][j];
        }
}

infile4>>scols>>ncols;
infile4>>srows>>nrows;
infile4>>sxllcorner>>xllcorner;
infile4>>syllcorner>>yllcorner;
infile4>>scellsize>>cellsize;
infile4>>snodate>>NODATA;
for(int i = 0; i < nrows; i++)
{
        for(int j = 0; j < ncols; j++)
        {
        infile4>>value4[i][j];
        }
}

infile5>>scols>>ncols;
infile5>>srows>>nrows;
infile5>>sxllcorner>>xllcorner;
infile5>>syllcorner>>yllcorner;
infile5>>scellsize>>cellsize;
infile5>>snodate>>NODATA;
for(int i = 0; i < nrows; i++)
{
        for(int j = 0; j < ncols; j++)
        {
        infile5>>value5[i][j];
        }
}

infile6>>scols>>ncols;
infile6>>srows>>nrows;
infile6>>sxllcorner>>xllcorner;
infile6>>syllcorner>>yllcorner;
infile6>>scellsize>>cellsize;
infile6>>snodate>>NODATA;
for(int i = 0; i < nrows; i++)
{
        for(int j = 0; j < ncols; j++)
        {
        infile6>>value6[i][j];
        }
}

infile7>>scols>>ncols;
infile7>>srows>>nrows;
infile7>>sxllcorner>>xllcorner;
infile7>>syllcorner>>yllcorner;
infile7>>scellsize>>cellsize;
infile7>>snodate>>NODATA;
for(int i = 0; i < nrows; i++)
{
        for(int j = 0; j < ncols; j++)
        {
        infile7>>value7[i][j];
        }
}

infile8>>scols>>ncols;
infile8>>srows>>nrows;
infile8>>sxllcorner>>xllcorner;
infile8>>syllcorner>>yllcorner;
infile8>>scellsize>>cellsize;
infile8>>snodate>>NODATA;
for(int i = 0; i < nrows; i++)
{
        for(int j = 0; j < ncols; j++)
        {
        infile8>>value8[i][j];
        }
}

infile9>>scols>>ncols;
infile9>>srows>>nrows;
infile9>>sxllcorner>>xllcorner;
infile9>>syllcorner>>yllcorner;
infile9>>scellsize>>cellsize;
infile9>>snodate>>NODATA;
for(int i = 0; i < nrows; i++)
{
        for(int j = 0; j < ncols; j++)
        {
        infile9>>value9[i][j];
        }
}


infile10>>scols>>ncols;
infile10>>srows>>nrows;
infile10>>sxllcorner>>xllcorner;
infile10>>syllcorner>>yllcorner;
infile10>>scellsize>>cellsize;
infile10>>snodate>>NODATA;
for(int i = 0; i < nrows; i++)
{
        for(int j = 0; j < ncols; j++)
        {
        infile10>>value10[i][j];
        }
}

infile11>>scols>>ncols;
infile11>>srows>>nrows;
infile11>>sxllcorner>>xllcorner;
infile11>>syllcorner>>yllcorner;
infile11>>scellsize>>cellsize;
infile11>>snodate>>NODATA;
for(int i = 0; i < nrows; i++)
{
        for(int j = 0; j < ncols; j++)
        {
        infile11>>value11[i][j];
        }
}

/*
infile12>>scols>>ncols;
infile12>>srows>>nrows;
infile12>>sxllcorner>>xllcorner;
infile12>>syllcorner>>yllcorner;
infile12>>scellsize>>cellsize;
infile12>>snodate>>NODATA;
for(int i = 0; i < nrows; i++)
{
        for(int j = 0; j < ncols; j++)
        {
        infile12>>value12[i][j];
        }
}

infile13>>scols>>ncols;
infile13>>srows>>nrows;
infile13>>sxllcorner>>xllcorner;
infile13>>syllcorner>>yllcorner;
infile13>>scellsize>>cellsize;
infile13>>snodate>>NODATA;
for(int i = 0; i < nrows; i++)
{
        for(int j = 0; j < ncols; j++)
        {
        infile13>>value13[i][j];
        }
}

infile14>>scols>>ncols;
infile14>>srows>>nrows;
infile14>>sxllcorner>>xllcorner;
infile14>>syllcorner>>yllcorner;
infile14>>scellsize>>cellsize;
infile14>>snodate>>NODATA;
for(int i = 0; i < nrows; i++)
{
        for(int j = 0; j < ncols; j++)
        {
        infile14>>value14[i][j];
        }
}

infile15>>scols>>ncols;
infile15>>srows>>nrows;
infile15>>sxllcorner>>xllcorner;
infile15>>syllcorner>>yllcorner;
infile15>>scellsize>>cellsize;
infile15>>snodate>>NODATA;
for(int i = 0; i < nrows; i++)
{
        for(int j = 0; j < ncols; j++)
        {
        infile15>>value15[i][j];
        }
}

infile16>>scols>>ncols;
infile16>>srows>>nrows;
infile16>>sxllcorner>>xllcorner;
infile16>>syllcorner>>yllcorner;
infile16>>scellsize>>cellsize;
infile16>>snodate>>NODATA;
for(int i = 0; i < nrows; i++)
{
        for(int j = 0; j < ncols; j++)
        {
        infile16>>value16[i][j];
        }
}

infile17>>scols>>ncols;
infile17>>srows>>nrows;
infile17>>sxllcorner>>xllcorner;
infile17>>syllcorner>>yllcorner;
infile17>>scellsize>>cellsize;
infile17>>snodate>>NODATA;
for(int i = 0; i < nrows; i++)
{
        for(int j = 0; j < ncols; j++)
        {
        infile17>>value17[i][j];
        }
}

infile18>>scols>>ncols;
infile18>>srows>>nrows;
infile18>>sxllcorner>>xllcorner;
infile18>>syllcorner>>yllcorner;
infile18>>scellsize>>cellsize;
infile18>>snodate>>NODATA;
for(int i = 0; i < nrows; i++)
{
        for(int j = 0; j < ncols; j++)
        {
        infile18>>value18[i][j];
        }
}

infile19>>scols>>ncols;
infile19>>srows>>nrows;
infile19>>sxllcorner>>xllcorner;
infile19>>syllcorner>>yllcorner;
infile19>>scellsize>>cellsize;
infile19>>snodate>>NODATA;
for(int i = 0; i < nrows; i++)
{
        for(int j = 0; j < ncols; j++)
        {
        infile19>>value19[i][j];
        }
}

infile20>>scols>>ncols;
infile20>>srows>>nrows;
infile20>>sxllcorner>>xllcorner;
infile20>>syllcorner>>yllcorner;
infile20>>scellsize>>cellsize;
infile20>>snodate>>NODATA;
for(int i = 0; i < nrows; i++)
{
        for(int j = 0; j < ncols; j++)
        {
        infile20>>value20[i][j];
        }
}

infile21>>scols>>ncols;
infile21>>srows>>nrows;
infile21>>sxllcorner>>xllcorner;
infile21>>syllcorner>>yllcorner;
infile21>>scellsize>>cellsize;
infile21>>snodate>>NODATA;
for(int i = 0; i < nrows; i++)
{
        for(int j = 0; j < ncols; j++)
        {
        infile21>>value21[i][j];
        }
}

infile22>>scols>>ncols;
infile22>>srows>>nrows;
infile22>>sxllcorner>>xllcorner;
infile22>>syllcorner>>yllcorner;
infile22>>scellsize>>cellsize;
infile22>>snodate>>NODATA;
for(int i = 0; i < nrows; i++)
{
        for(int j = 0; j < ncols; j++)
        {
        infile22>>value22[i][j];
        }
}

infile23>>scols>>ncols;
infile23>>srows>>nrows;
infile23>>sxllcorner>>xllcorner;
infile23>>syllcorner>>yllcorner;
infile23>>scellsize>>cellsize;
infile23>>snodate>>NODATA;
for(int i = 0; i < nrows; i++)
{
        for(int j = 0; j < ncols; j++)
        {
        infile23>>value23[i][j];
        }
}

infile24>>scols>>ncols;
infile24>>srows>>nrows;
infile24>>sxllcorner>>xllcorner;
infile24>>syllcorner>>yllcorner;
infile24>>scellsize>>cellsize;
infile24>>snodate>>NODATA;
for(int i = 0; i < nrows; i++)
{
        for(int j = 0; j < ncols; j++)
        {
        infile24>>value24[i][j];
        }
}

infile25>>scols>>ncols;
infile25>>srows>>nrows;
infile25>>sxllcorner>>xllcorner;
infile25>>syllcorner>>yllcorner;
infile25>>scellsize>>cellsize;
infile25>>snodate>>NODATA;
for(int i = 0; i < nrows; i++)
{
        for(int j = 0; j < ncols; j++)
        {
        infile25>>value25[i][j];
        }
}

infile26>>scols>>ncols;
infile26>>srows>>nrows;
infile26>>sxllcorner>>xllcorner;
infile26>>syllcorner>>yllcorner;
infile26>>scellsize>>cellsize;
infile26>>snodate>>NODATA;
for(int i = 0; i < nrows; i++)
{
        for(int j = 0; j < ncols; j++)
        {
        infile26>>value26[i][j];
        }
}

infile27>>scols>>ncols;
infile27>>srows>>nrows;
infile27>>sxllcorner>>xllcorner;
infile27>>syllcorner>>yllcorner;
infile27>>scellsize>>cellsize;
infile27>>snodate>>NODATA;
for(int i = 0; i < nrows; i++)
{
        for(int j = 0; j < ncols; j++)
        {
        infile27>>value27[i][j];
        }
}

infile28>>scols>>ncols;
infile28>>srows>>nrows;
infile28>>sxllcorner>>xllcorner;
infile28>>syllcorner>>yllcorner;
infile28>>scellsize>>cellsize;
infile28>>snodate>>NODATA;
for(int i = 0; i < nrows; i++)
{
        for(int j = 0; j < ncols; j++)
        {
        infile28>>value28[i][j];
        }
}

infile29>>scols>>ncols;
infile29>>srows>>nrows;
infile29>>sxllcorner>>xllcorner;
infile29>>syllcorner>>yllcorner;
infile29>>scellsize>>cellsize;
infile29>>snodate>>NODATA;
for(int i = 0; i < nrows; i++)
{
        for(int j = 0; j < ncols; j++)
        {
        infile29>>value29[i][j];
        }
}

infile30>>scols>>ncols;
infile30>>srows>>nrows;
infile30>>sxllcorner>>xllcorner;
infile30>>syllcorner>>yllcorner;
infile30>>scellsize>>cellsize;
infile30>>snodate>>NODATA;
for(int i = 0; i < nrows; i++)
{
        for(int j = 0; j < ncols; j++)
        {
        infile30>>value30[i][j];
        }
}

*/

for(int i = 0; i < nrows; i++)
{
        for(int j = 0; j < ncols; j++)
        {
        if(value1[i][j] > -9990)
                {
                        Sx = 1.+2.+3.+4.+5.+6.+7.+8.+9.+10.+11.;
                        Sy = value1[i][j] + value2[i][j] + value3[i][j] + value4[i][j] + value5[i][j] + value6[i][j] + value7[i][j] + value8[i][j] + value9[i][j] + value10[i][j] + value11[i][j];
                        float meanx = Sx / 11.;
                        float meany = Sy / 11.;

                        Sxx = powl((1. - meanx), 2.) + powl((2. - meanx), 2.) + powl((3. - meanx), 2.) + powl((4 - meanx), 2) + powl((5 - meanx), 2) + powl((6 - meanx), 2) + powl((7 - meanx), 2) + powl((8 - meanx), 2) + powl((9 - meanx), 2) + powl((10 - meanx), 2) + powl((11 - meanx), 2);
                        Sxy = (1. - meanx) * (value1[i][j] - meany) + (2. - meanx) * (value2[i][j] - meany) + (3 - meanx) * (value3[i][j] - meany) + (4 - meanx) * (value4[i][j] - meany) + (5 - meanx) * (value5[i][j] - meany) + (6 - meanx) * (value6[i][j] - meany) + (7 - meanx) * (value7[i][j] - meany) + (8 - meanx) * (value8[i][j] - meany) + (9 - meanx) * (value9[i][j] - meany) + (10 - meanx) * (value10[i][j] - meany) + (11 - meanx) * (value11[i][j] - meany);
                        Syy = powl((value1[i][j]- meany), 2.) + powl((value2[i][j] - meany), 2.) + powl((value3[i][j] - meany), 2.) + powl((value4[i][j] - meany), 2.) + powl((value5[i][j] - meany), 2.) + powl((value6[i][j] - meany), 2.) + powl((value7[i][j] - meany), 2) + powl((value8[i][j] - meany), 2.) + powl((value9[i][j] - meany), 2.) + powl((value10[i][j] - meany), 2.) + powl((value11[i][j] - meany), 2.);
                        float MSresidual = Syy / (NUM - 2.);
                        Mslope = Sxy / Sxx;
                        Minter = meany - Mslope * meanx;
                        SEslope = powl(MSresidual / Sxx, 0.5);
                        SEinter = powl(MSresidual* (1. / NUM + powl(meanx, 2.0) / Sxx), 0.5);
//                        Sxx = powl(1, 2) + powl(2, 2) + powl(3, 2) + powl(4, 2) + powl(5, 2) + powl(6, 2) + powl(7, 2) + powl(8, 2) + powl(9, 2);
//                        Sxy = 1*value1[i][j] + 2*value2[i][j] + 3*value3[i][j] + 4*value4[i][j] + 5*value5[i][j] + 6*value6[i][j] + 7*value7[i][j] + 8*value8[i][j] + 9*value9[i][j];
//                        Syy = powl(value1[i][j], 2.) + powl(value2[i][j], 2.) + powl(value3[i][j], 2.) + powl(value4[i][j], 2.) + powl(value5[i][j], 2.) + powl(value6[i][j], 2.) + powl(value7[i][j], 2) + powl(value8[i][j], 2.) + powl(value9[i][j], 2.);
//                        Mslope = (NUM * Sxy - Sx * Sy) / (NUM * Sxx - powl(Sx, 2.));
//                        Minter = 1. / NUM * Sy - Mslope * 1. / NUM * Sx;
//                        float temp = 1. / NUM / (NUM - 2) * (NUM * Syy - powl(Sy, 2) - powl(Mslope, 2) * (NUM * Sxx - powl(Sx, 2)));
//                        cout<<temp<<" "<<SEslope<<" "<<Sxx<<" "<<Sx<<" "<<Mslope<<""<<" "<<" "<<endl;

//                        SEslope = powl((NUM * temp / (NUM * Sxx - powl(Sx, 2))), 0.5);
//                        SEinter = powl((powl(SEslope, 2) * 1 / NUM * Sxx), 0.5);
                        outputMS[i][j] = Mslope;
                        outputMI[i][j] = Minter;
                        outputSSE[i][j] = SEslope;
                        outputISE[i][j] = SEinter;
                }
        else
                {
                        outputMS[i][j] = -9999;
                        outputMI[i][j] = -9999;
                        outputSSE[i][j] = -9999;
                        outputISE[i][j] = -9999;
                }
        }
}


outfile1<<scols<<" "<<ncols<<endl;
outfile1<<srows<<" "<<nrows<<endl;
outfile1<<sxllcorner<<" "<<xllcorner<<endl;
outfile1<<syllcorner<<" "<<yllcorner<<endl;
outfile1<<scellsize<<" "<<cellsize<<endl;
outfile1<<snodate<<" "<<NODATA<<endl;
for(int i = 0; i < nrows; i++)
{
        for(int j = 0; j < ncols; j++)
        {
        outfile1<<outputMS[i][j]<<" ";
        }
        outfile1<<endl;
}

outfile2<<scols<<" "<<ncols<<endl;
outfile2<<srows<<" "<<nrows<<endl;
outfile2<<sxllcorner<<" "<<xllcorner<<endl;
outfile2<<syllcorner<<" "<<yllcorner<<endl;
outfile2<<scellsize<<" "<<cellsize<<endl;
outfile2<<snodate<<" "<<NODATA<<endl;
for(int i = 0; i < nrows; i++)
{
        for(int j = 0; j < ncols; j++)
        {
        outfile2<<outputMI[i][j]<<" ";
        }
        outfile2<<endl;
}

outfile3<<scols<<" "<<ncols<<endl;
outfile3<<srows<<" "<<nrows<<endl;
outfile3<<sxllcorner<<" "<<xllcorner<<endl;
outfile3<<syllcorner<<" "<<yllcorner<<endl;
outfile3<<scellsize<<" "<<cellsize<<endl;
outfile3<<snodate<<" "<<NODATA<<endl;
for(int i = 0; i < nrows; i++)
{
        for(int j = 0; j < ncols; j++)
        {
        outfile3<<outputSSE[i][j]<<" ";
        }
        outfile3<<endl;
}

outfile4<<scols<<" "<<ncols<<endl;
outfile4<<srows<<" "<<nrows<<endl;
outfile4<<sxllcorner<<" "<<xllcorner<<endl;
outfile4<<syllcorner<<" "<<yllcorner<<endl;
outfile4<<scellsize<<" "<<cellsize<<endl;
outfile4<<snodate<<" "<<NODATA<<endl;
for(int i = 0; i < nrows; i++)
{
        for(int j = 0; j < ncols; j++)
        {
        outfile4<<outputISE[i][j]<<" ";
        }
        outfile4<<endl;
}

infile1.close();
infile2.close();
infile3.close();
infile4.close();
infile5.close();
infile6.close();
infile7.close();
infile8.close();
infile9.close();
infile10.close();
infile11.close();

outfile1.close();
outfile2.close();
outfile3.close();
outfile4.close();

}
