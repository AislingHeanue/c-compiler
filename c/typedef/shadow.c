// Examples lifted from https://eli.thegreenplace.net/2011/05/02/the-context-sensitivity-of-cs-grammar-revisited/
typedef unsigned int AA;

int main(void)
{
    AA AA = 1;
    int CC = AA * 2;
    typedef double BB;
    BB BB[3] = {CC + 5, 0,0};
    return !(BB[0] == 7);
}
