typedef long ptrdiff_t;
typedef unsigned long size_t;
typedef char wchar_t;

typedef struct {
    unsigned int gp_offset;
    unsigned int fp_offset;
    void *overflow_arg_area;
    void *reg_save_area;
} __va_list[1];

typedef __va_list va_list;

typedef unsigned char __u_char;
typedef unsigned short int __u_short;
typedef unsigned int __u_int;
typedef unsigned long int __u_long;

typedef signed char __int8_t;
typedef unsigned char __uint8_t;
typedef signed short int __int16_t;
typedef unsigned short int __uint16_t;
typedef signed int __int32_t;
typedef unsigned int __uint32_t;
typedef signed long int __int64_t;
typedef unsigned long int __uint64_t;

typedef __int8_t __int_least8_t;
typedef __uint8_t __uint_least8_t;
typedef __int16_t __int_least16_t;
typedef __uint16_t __uint_least16_t;
typedef __int32_t __int_least32_t;
typedef __uint32_t __uint_least32_t;
typedef __int64_t __int_least64_t;
typedef __uint64_t __uint_least64_t;

typedef long int __quad_t;
typedef unsigned long int __u_quad_t;

typedef long int __intmax_t;
typedef unsigned long int __uintmax_t;

typedef unsigned long int __dev_t;
typedef unsigned int __uid_t;
typedef unsigned int __gid_t;
typedef unsigned long int __ino_t;
typedef unsigned long int __ino64_t;
typedef unsigned int __mode_t;
typedef unsigned long int __nlink_t;
typedef long int __off_t;
typedef long int __off64_t;
typedef int __pid_t;
typedef struct { int __val[2]; } __fsid_t;
typedef long int __clock_t;
typedef unsigned long int __rlim_t;
typedef unsigned long int __rlim64_t;
typedef unsigned int __id_t;
typedef long int __time_t;
typedef unsigned int __useconds_t;
typedef long int __suseconds_t;
typedef long int __suseconds64_t;

typedef int __daddr_t;
typedef int __key_t;

typedef int __clockid_t;

typedef void * __timer_t;

typedef long int __blksize_t;

typedef long int __blkcnt_t;
typedef long int __blkcnt64_t;

typedef unsigned long int __fsblkcnt_t;
typedef unsigned long int __fsblkcnt64_t;

typedef unsigned long int __fsfilcnt_t;
typedef unsigned long int __fsfilcnt64_t;

typedef long int __fsword_t;

typedef long int __ssize_t;

typedef long int __syscall_slong_t;

typedef unsigned long int __syscall_ulong_t;

typedef __off64_t __loff_t;
typedef char *__caddr_t;

typedef long int __intptr_t;

typedef unsigned int __socklen_t;

typedef int __sig_atomic_t;
typedef struct
{
  int __count;
  union
  {
    unsigned int __wch;
    char __wchb[4];
  } __value;
} __mbstate_t;

typedef struct _G_fpos_t
{
  __off_t __pos;
  __mbstate_t __state;
} __fpos_t;
typedef struct _G_fpos64_t
{
  __off64_t __pos;
  __mbstate_t __state;
} __fpos64_t;
struct _IO_FILE;
typedef struct _IO_FILE __FILE;
struct _IO_FILE;

typedef struct _IO_FILE FILE;
struct _IO_FILE;
struct _IO_marker;
struct _IO_codecvt;
struct _IO_wide_data;

typedef void _IO_lock_t;

struct _IO_FILE
{
  int _flags;

  char *_IO_read_ptr;
  char *_IO_read_end;
  char *_IO_read_base;
  char *_IO_write_base;
  char *_IO_write_ptr;
  char *_IO_write_end;
  char *_IO_buf_base;
  char *_IO_buf_end;

  char *_IO_save_base;
  char *_IO_backup_base;
  char *_IO_save_end;

  struct _IO_marker *_markers;

  struct _IO_FILE *_chain;

  int _fileno;
  int _flags2:24;

  char _short_backupbuf[1];
  __off_t _old_offset;

  unsigned short _cur_column;
  signed char _vtable_offset;
  char _shortbuf[1];

  _IO_lock_t *_lock;
  __off64_t _offset;

  struct _IO_codecvt *_codecvt;
  struct _IO_wide_data *_wide_data;
  struct _IO_FILE *_freeres_list;
  void *_freeres_buf;
  struct _IO_FILE **_prevchain;
  int _mode;
  int _unused3;
  __uint64_t _total_written;

  char _unused2[12 * sizeof (int) - 5 * sizeof (void *)];
};

typedef __ssize_t cookie_read_function_t (void *__cookie, char *__buf,
                                          size_t __nbytes);

typedef __ssize_t cookie_write_function_t (void *__cookie, const char *__buf,
                                           size_t __nbytes);

typedef int cookie_seek_function_t (void *__cookie, __off64_t *__pos, int __w);

typedef int cookie_close_function_t (void *__cookie);

typedef struct _IO_cookie_io_functions_t
{
  cookie_read_function_t *read;
  cookie_write_function_t *write;
  cookie_seek_function_t *seek;
  cookie_close_function_t *close;
} cookie_io_functions_t;

typedef __off_t off_t;

typedef __ssize_t ssize_t;

typedef __fpos_t fpos_t;

extern FILE *stdin;
extern FILE *stdout;
extern FILE *stderr;

extern int remove (const char *__filename) ;

extern int rename (const char *__old, const char *__new) ;

extern int renameat (int __oldfd, const char *__old, int __newfd,
		     const char *__new) ;

extern int fclose (FILE *__stream) ;

extern FILE *tmpfile (void)
    ;

extern char *tmpnam (char[20])  ;

extern char *tmpnam_r (char __s[20])  ;

extern char *tempnam (const char *__dir, const char *__pfx)
      ;

extern int fflush (FILE *__stream);

extern int fflush_unlocked (FILE *__stream);

extern FILE *fopen (const char *restrict __filename,
		    const char *restrict __modes)
    ;

extern FILE *freopen (const char *restrict __filename,
		      const char *restrict __modes,
		      FILE *restrict __stream)  ;

extern FILE *fdopen (int __fd, const char *__modes) 
    ;

extern FILE *fopencookie (void *restrict __magic_cookie,
			  const char *restrict __modes,
			  cookie_io_functions_t __io_funcs) 
    ;

extern FILE *fmemopen (void *__s, size_t __len, const char *__modes)
     ;

extern FILE *open_memstream (char **__bufloc, size_t *__sizeloc) 
    ;

extern void setbuf (FILE *restrict __stream, char *restrict __buf) 
  ;

extern int setvbuf (FILE *restrict __stream, char *restrict __buf,
		    int __modes, size_t __n)  ;

extern void setbuffer (FILE *restrict __stream, char *restrict __buf,
		       size_t __size)  ;

extern void setlinebuf (FILE *__stream)  ;

extern int fprintf (FILE *restrict __stream,
		    const char *restrict __format, ...) ;

extern int printf (const char *restrict __format, ...);

extern int sprintf (char *restrict __s,
		    const char *restrict __format, ...) ;

extern int vfprintf (FILE *restrict __s, const char *restrict __format,
		     __gnuc_va_list __arg) ;

extern int vprintf (const char *restrict __format, __gnuc_va_list __arg);

extern int vsprintf (char *restrict __s, const char *restrict __format,
		     __gnuc_va_list __arg) ;

extern int snprintf (char *restrict __s, size_t __maxlen,
		     const char *restrict __format, ...)
      ;

extern int vsnprintf (char *restrict __s, size_t __maxlen,
		      const char *restrict __format, __gnuc_va_list __arg)
      ;

extern int vasprintf (char **restrict __ptr, const char *restrict __f,
		      __gnuc_va_list __arg)
       ;
extern int __asprintf (char **restrict __ptr,
		       const char *restrict __fmt, ...)
       ;
extern int asprintf (char **restrict __ptr,
		     const char *restrict __fmt, ...)
       ;

extern int vdprintf (int __fd, const char *restrict __fmt,
		     __gnuc_va_list __arg)
     ;
extern int dprintf (int __fd, const char *restrict __fmt, ...)
     ;

extern int fscanf (FILE *restrict __stream,
		   const char *restrict __format, ...)  ;

extern int scanf (const char *restrict __format, ...) ;

extern int sscanf (const char *restrict __s,
		   const char *restrict __format, ...) ;

typedef float _Float32;

typedef double _Float64;

typedef double _Float32x;

typedef long double _Float64x;
extern int __isoc99_fscanf (FILE *restrict __stream,
			    const char *restrict __format, ...) 
  ;
extern int __isoc99_scanf (const char *restrict __format, ...) ;
extern int __isoc99_sscanf (const char *restrict __s,
			    const char *restrict __format, ...) ;

extern int vfscanf (FILE *restrict __s, const char *restrict __format,
		    __gnuc_va_list __arg)
       ;

extern int vscanf (const char *restrict __format, __gnuc_va_list __arg)
      ;

extern int vsscanf (const char *restrict __s,
		    const char *restrict __format, __gnuc_va_list __arg)
      ;

extern int __isoc99_vfscanf (FILE *restrict __s,
			     const char *restrict __format,
			     __gnuc_va_list __arg)  ;
extern int __isoc99_vscanf (const char *restrict __format,
			    __gnuc_va_list __arg) ;
extern int __isoc99_vsscanf (const char *restrict __s,
			     const char *restrict __format,
			     __gnuc_va_list __arg) ;

extern int fgetc (FILE *__stream) ;
extern int getc (FILE *__stream) ;

extern int getchar (void);

extern int getc_unlocked (FILE *__stream) ;
extern int getchar_unlocked (void);

extern int fgetc_unlocked (FILE *__stream) ;

extern int fputc (int __c, FILE *__stream) ;
extern int putc (int __c, FILE *__stream) ;

extern int putchar (int __c);

extern int fputc_unlocked (int __c, FILE *__stream) ;

extern int putc_unlocked (int __c, FILE *__stream) ;
extern int putchar_unlocked (int __c);

extern int getw (FILE *__stream) ;

extern int putw (int __w, FILE *__stream) ;

extern char *fgets (char *restrict __s, int __n, FILE *restrict __stream)
       ;

extern __ssize_t __getdelim (char **restrict __lineptr,
                             size_t *restrict __n, int __delimiter,
                             FILE *restrict __stream)  ;
extern __ssize_t getdelim (char **restrict __lineptr,
                           size_t *restrict __n, int __delimiter,
                           FILE *restrict __stream)  ;

extern __ssize_t getline (char **restrict __lineptr,
                          size_t *restrict __n,
                          FILE *restrict __stream)  ;

extern int fputs (const char *restrict __s, FILE *restrict __stream)
  ;

extern int puts (const char *__s);

extern int ungetc (int __c, FILE *__stream) ;

extern size_t fread (void *restrict __ptr, size_t __size,
		     size_t __n, FILE *restrict __stream) 
  ;

extern size_t fwrite (const void *restrict __ptr, size_t __size,
		      size_t __n, FILE *restrict __s) ;

extern size_t fread_unlocked (void *restrict __ptr, size_t __size,
			      size_t __n, FILE *restrict __stream) 
  ;
extern size_t fwrite_unlocked (const void *restrict __ptr, size_t __size,
			       size_t __n, FILE *restrict __stream)
  ;

extern int fseek (FILE *__stream, long int __off, int __whence)
  ;

extern long int ftell (FILE *__stream)  ;

extern void rewind (FILE *__stream) ;

extern int fseeko (FILE *__stream, __off_t __off, int __whence)
  ;

extern __off_t ftello (FILE *__stream)  ;

extern int fgetpos (FILE *restrict __stream, fpos_t *restrict __pos)
  ;

extern int fsetpos (FILE *__stream, const fpos_t *__pos) ;

extern void clearerr (FILE *__stream)  ;

extern int feof (FILE *__stream)   ;

extern int ferror (FILE *__stream)   ;

extern void clearerr_unlocked (FILE *__stream)  ;
extern int feof_unlocked (FILE *__stream)   ;
extern int ferror_unlocked (FILE *__stream)   ;

extern void perror (const char *__s) ;

extern int fileno (FILE *__stream)   ;

extern int fileno_unlocked (FILE *__stream)   ;

extern int pclose (FILE *__stream) ;

extern FILE *popen (const char *__command, const char *__modes)
    ;

extern char *ctermid (char *__s) 
  ;

extern void flockfile (FILE *__stream)  ;

extern int ftrylockfile (FILE *__stream)   ;

extern void funlockfile (FILE *__stream)  ;

extern int __uflow (FILE *);
extern int __overflow (FILE *, int);

int main(void) {
  return 1;
}
