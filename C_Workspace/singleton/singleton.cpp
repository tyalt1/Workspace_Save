#include "singleton.h"

Singleton* Singleton::_instance = nullptr;

Singleton* Singleton::getInstance()
{
    if( _instance == nullptr ) _instance = new Singleton();

    return _instance;
}

Singleton::~Singleton()
{
    delete _instance;

    // Other class cleanup
}

Singleton::Singleton()
{
    // Init class
}

// Other public and private members.
