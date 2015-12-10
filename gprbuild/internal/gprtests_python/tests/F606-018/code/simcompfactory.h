#ifndef _SimCompFactory_h_
#define _SimCompFactory_h_

#include <string>

class SimCompFactory
{
    public:
        virtual int Create(std::string i_className);
};

#endif
