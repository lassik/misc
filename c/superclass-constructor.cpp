#include <iostream>

using namespace std;

class Super
{
public:
    Super()
    {
        cout << "super" << endl;
    }
};

class Sub: public Super
{
public:
    Sub()
    {
        cout << "sub" << endl;
    }
};

int main(void)
{
    Sub sub[2];
    return(0);
}
