#include <sys/time.h>
long gttmdy_(usecs)
long *usecs;
{
 int gettimeofday();
 struct timeval tp;
 struct timezone tzp;
 int i;

 i=gettimeofday(&tp,&tzp);
 *usecs = tp.tv_usec;
 return(tp.tv_sec);
}

