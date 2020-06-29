#include<iostream>
#include<cmath>
//#include<cstring>
#include<fstream>
using namespace std;

int Tactive = 1;
int Mactive = 1;

double CMN_func(double Csub, double k1, double fst, double fsm)
{
       double a;
       a = Csub * k1 * fst * fsm;
       return a;
       }

double Rm_func(double Cbio, double k2, double fmt, double fmm)
{
       double rm;
       rm = Cbio * k2 * fmt * fmm;
	     if(rm >= Cbio) rm = Cbio;
       return rm;
       }

double fst_func(double Tsmin, double T, double Qs10, double Tsref)
{
       double fst;
       fst = pow(Qs10, (T-Tsref) / 10.);
       return fst;
       }

double fmt_func(double Tmmin, double T, double Qm10, double Tmref)
{
       double fmt;
       if(T <= Tmmin) {
		   fmt = 0.0;
		   Tactive = 0;
	     }
       else 
       {
	     fmt = pow(Qm10, (T-Tmref) / 10.);
		   Tactive = 1;
	     }
       return fmt;
       }

double fsm_func(double M, double Msmin, double Msmax)
{
       double fsm;
       if(M<Msmin) M = Msmin;
       if(M>Msmax) M = Msmax;
       fsm = log(Msmin/M) / log(Msmin / Msmax);
       return fsm;
       }

double fmm_func(double M, double Mmmin)
{
       double fmm;
       if(M <= Mmmin) 
		{
		fmm = 0.;
		Mactive = 0;
	       }
       else 	
       {
		fmm = 1.;
		Mactive = 1;
	}
	
       return fmm;
       }
int ii;
int main()
{
// parameter declaration
string s_k1, s_k2, s_Csub, s_Qs10, s_Qm10, s_Tsmin, s_Tmmin, s_Tsref, s_Tmref, s_Msmin, s_Msmax, s_Mmmin, s_cueref, s_CNratio, s_map;
string filedrivingforce;
double k1, k2, Csub, Qs10, Qm10, Tsmin, Tmmin, Tsref, Tmref, Msmin, Msmax, Mmmin, cueref, cn;
int map;
double cue;
int iteration;
int MAP[365];

ifstream parafile;
parafile.open("para.txt");
parafile>>s_k2>>k2;
parafile>>s_Csub>>Csub;
parafile>>s_Qs10>>Qs10;
parafile>>s_Qm10>>Qm10;
parafile>>s_Tsmin>>Tsmin;
parafile>>s_Tmmin>>Tmmin;
parafile>>s_Tsref>>Tsref;
parafile>>s_Tmref>>Tmref;
parafile>>s_Msmin>>Msmin;
parafile>>s_Msmax>>Msmax;
parafile>>s_Mmmin>>Mmmin;
parafile>>s_cueref>>cueref;
parafile>>s_CNratio>>cn;
parafile>>s_map>>map;
parafile.close();

cout<<"How many iteration you want to run the model (Daily time step):"<<endl;
cin>>iteration;
cout<<"The file name for driving force:"<<endl;
cin>>filedrivingforce;

double * temp = new double[iteration];
double * prec = new double[iteration];

double * Cbio = new double[iteration+1];
Cbio[0] = 0.;

ifstream infile;
infile.open(filedrivingforce.c_str());
for(int i = 0; i < iteration; i++)
infile>>temp[i]>>prec[i];    
infile.close();

int temmap;
for(int i = 0; i < 365; i++)
{
	MAP[i] = 0;
}
if(map > 0 && map < 366)
{
	MAP[210] = 1;
	temmap = (map+1)/2;
	
	if((map%2)==0)
	MAP[210-map/2] = 1;
		
	for(int i = 1; i<temmap; i++) 
	{
		MAP[210-i] = 1;
		
		if(i < 155)
		MAP[210+i] = 1;
		else
		MAP[210+i-365] = 1;
	}
}
	//for(int i = 0; i < 365; i++)
	//{
	//cout<<i<<" "<<MAP[i]<<endl;
	//}

	// calcuate the decomspitonrate based on CN ratio
k1 = 1 - exp((0.0079 * cn - 1.2049)/365.);

double a1, a2, a3, a4, a5, a6;
for(int i = 0; i < iteration; i++)
{
	cue = (cueref - (0.012 * (temp[i] - 15.))) * pow((8./cn), 0.6);
        a1 = fst_func(Tsmin, temp[i], Qs10, Tsref);
        a2 = fmt_func(Tmmin, temp[i], Qm10, Tmref);
        a3 = fsm_func(prec[i], Msmin, Msmax);
        a4 = fmm_func(prec[i], Mmmin);
        a5 = CMN_func(Csub, k1, a1, a3);
        a6 = Rm_func(Cbio[i], k2, a2, a4);
        
	//ii = i % 365;
	//if(MAP[ii])
	if(Tactive == 1 && Mactive == 1)
  Cbio[i+1] = Cbio[i] + a5 * cue - a6;
	else Cbio[i+1] = Cbio[i];
	//cout<<i+1<<" "<<a1<<" "<<a2<<" "<<a3<<" "<<a4<<" "<<a5<<" "<<a6<<" "<<cue<<" "<<Cbio[i+1]<<endl;
	//cout<<ii<<" ii and i "<<i+1<<" "<<cue<<" "<<Cbio[i+1]<<" "<<Cbio[i+1]/Csub<<endl;
        }

double ave = 0.;
	for(int i = 0; i < 365; i++)
	{
		ave += Cbio[iteration-i];
		}
		cout<<"average: "<<ave / 365. / Csub<<endl;
	
ofstream outfile;
outfile.open("out.txt");
for(int i = 0; i < iteration; i++)
outfile<<i+1<<" "<<Csub<<" "<<Cbio[i]<<endl;    
outfile.close();

delete[]temp;
delete[]prec;
delete[]Cbio;

return 0;
}
