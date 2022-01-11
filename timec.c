#include <time.h>


void timec_(double* t1)
{
   clock_t t;
   t=clock();
   *t1=(double)t;
}
