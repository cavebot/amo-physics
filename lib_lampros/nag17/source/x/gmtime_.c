#include <time.h>
void gmtime_(t,itemp)
long *t,*itemp;
{
 extern long gttmdy_();
 struct tm gmdummy ,*gmtim;
 long i;
 i= gttmdy_(t);
 gmtim=&gmdummy;
 gmtim = localtime(&i);
 *itemp= gmtim->tm_sec;
 *(itemp+1) = gmtim->tm_min;
 *(itemp+2) = gmtim->tm_hour;
 *(itemp+3) = gmtim->tm_mday;
 *(itemp+4) = gmtim->tm_mon;
 *(itemp+5) = gmtim->tm_year;
 *(itemp+6) = gmtim->tm_wday;
 *(itemp+7) = gmtim->tm_yday;
 *(itemp+8) = gmtim->tm_isdst; 
 return;
}

