#ifndef _SINGLETON_H_
#define _SINGLETON_H_

class Singleton
{
public:
    static Singleton* getInstance();
    ~Singleton();

    //Other Public Members

private:
    Singleton();
    static Singleton* _instance;

    //Declare so no default is created. Do not Implement.
    Singleton(const Singleton&);
    Singleton& operator=(const Singleton&);

    //Other Private Members
};

#endif
