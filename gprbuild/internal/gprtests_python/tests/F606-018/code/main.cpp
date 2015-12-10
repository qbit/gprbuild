#include "simcompfactory.h"
//#include "Logger.h"

#ifdef USES_ADA
extern "C" void adainit() ;
extern "C" void adafinal() ;
#endif

int main(int argc, char* argv[])
{
#ifdef USES_ADA
    adainit();
#endif

   SimCompFactory processorFactory;
   //processorFactory.Create("name");

     //Logger::test();

#ifdef USES_ADA
    adafinal();
#endif
    return 0;
}
