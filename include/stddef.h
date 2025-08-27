#ifndef AISLING_STDDEF_H
#define AISLING_STDDEF_H

// NULL is a macro for any uninitialised void pointer
#define NULL ((void*)0)

typedef __PTRDIFF_TYPE__ ptrdiff_t;
typedef __SIZE_TYPE__ size_t;
typedef __WCHAR_TYPE__ wchar_t;

// get the pointer offset of member in a struct type (without actually initialising a struct)
#define offsetof(type, member) ((size_t) &(((type*)0)->member))

#endif /* defined AISLING_STDDEF_H */
