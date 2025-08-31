#ifndef AISLING_STDARG_H
#define AISLING_STDARG_H

typedef struct {
    unsigned int gp_offset;   // offset for general-purpose regs
    unsigned int fp_offset;   // offset for floating-point regs
    void *overflow_arg_area;  // where stack args start
    void *reg_save_area;      // copy of regs
} __va_list[1];

typedef __va_list va_list;

#define va_start(ap, last_named_param) __builtin_va_start(ap, last_named_param)
#define va_arg(ap, type)               __builtin_va_arg(ap, type)
#define va_end(ap)                     __builtin_va_end(ap)
#define va_copy(dst, src)              __builtin_va_copy(dst, src)

#endif /* defined AISLING_STDARG_H */
