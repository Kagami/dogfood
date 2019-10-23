#include "test.h"

// This is a line comment.

/*
 * This is a block comment.
 */

int g1;
int g2[4];

typedef int MyInt;

char g3 = 3;
short g4 = 4;
int g5 = 5;
long g6 = 6;
int *g7 = &g5;
char *g8 = "abc";
int g9[3] = {0, 1, 2};
char *g10[] = {"foo", "bar"};
struct {char a; int b;} g11[2] = {{1, 2}, {3, 4}};
struct {int a[2];} g12[2] = {{{1, 2}}};
struct {int a[2];} g13[2] = {{1, 2}, 3, 4};
struct {int a[2];} g14[2] = {1, 2, 3, 4};
char *g15 = {"foo"};
char g16[][4] = {'f', 'o', 'o', 0, 'b', 'a', 'r', 0};
char g17[] = "foobar";
char g18[10] = "foobar";
char g19[3] = "foobar";
char *g20 = g17+0;
char *g21 = g17+3;
char *g22 = &g17-3;
char *g23[] = {g17+0, g17+3, g17-3};
int g24=3;
int *g25=&g24;
int g26[3] = {1, 2, 3};
int *g27 = g26 + 1;
// int g28[3] = {};
// struct G29 {int a;}; struct G29 g29 = {};

typedef struct Tree {
  int val;
  struct Tree *lhs;
  struct Tree *rhs;
} Tree;

Tree *tree = &(Tree){
  1,
  &(Tree){
    2,
    &(Tree){ 3, 0, 0 },
    &(Tree){ 4, 0, 0 },
  },
  0,
};

int ext1;
int *ext2;
static int ext3 = 3;

int;
struct {char a; int b;};
typedef struct {char a; int b;} Ty1;

int ret3() {
  return 3;
  return 5;
}

int add2(int x, int y) {
  return x + y;
}

int sub2(int x, int y) {
  return x - y;
}

int add6(int a, int b, int c, int d, int e, int f) {
  return a + b + c + d + e + f;
}

int addx(int *x, int y) {
  return *x + y;
}

int sub_char(char a, char b, char c) {
  return a - b - c;
}

int sub_short(short a, short b, short c) {
  return a - b - c;
}

int sub_long(long a, long b, long c) {
  return a - b - c;
}

int *g1_ptr() {
  return &g1;
}

int fib(int x) {
  if (x<=1)
    return 1;
  return fib(x-1) + fib(x-2);
}

void ret_none() {
  return;
}

static int static_fn() { return 3; }

int param_decay(int x[]) { return x[0]; }

void voidfn(void) {}

int counter() {
  static int i;
  static int j = 1+1;
  return i++ + j++;
}

_Bool true_fn() { return 513; }
_Bool false_fn() { return 512; }

void test() {
  expect(8, ({ int a=3; int z=5; a+z; }), "int a=3; int z=5; a+z;");

  expect(0, 0, "0");
  expect(42, 42, "42");
  expect(5, 5, "0");
  expect(41,  12 + 34 - 5 , " 12 + 34 - 5 ");
  expect(5, 5, "0");
  expect(15, 5*(9-6), "5*(9-6)");
  expect(4, (3+5)/2, "(3+5)/2");
  expect(-10, -10, "0");
  expect(10, - -10, "- -10");
  expect(10, - - +10, "- - +10");

  expect(0, 0==1, "0==1");
  expect(1, 42==42, "42==42");
  expect(1, 0!=1, "0!=1");
  expect(0, 42!=42, "42!=42");

  expect(1, 0<1, "0<1");
  expect(0, 1<1, "1<1");
  expect(0, 2<1, "2<1");
  expect(1, 0<=1, "0<=1");
  expect(1, 1<=1, "1<=1");
  expect(0, 2<=1, "2<=1");

  expect(1, 1>0, "1>0");
  expect(0, 1>1, "1>1");
  expect(0, 1>2, "1>2");
  expect(1, 1>=0, "1>=0");
  expect(1, 1>=1, "1>=1");
  expect(0, 1>=2, "1>=2");

  expect(3, ({ int a; a=3; a; }), "int a; a=3; a;");
  expect(8, ({ int a; int z; a=3; z=5; a+z; }), "int a; int z; a=3; z=5; a+z;");
  expect(3, ({ int a=3; a; }), "int a=3; a;");
  expect(8, ({ int a=3; int z=5; a+z; }), "int a=3; int z=5; a+z;");

  expect(3, ({ int foo=3; foo; }), "int foo=3; foo;");
  expect(8, ({ int foo123=3; int bar=5; foo123+bar; }), "int foo123=3; int bar=5; foo123+bar;");

  expect(3, ret3(), "ret3();");

  expect(3, ({ int x=0; if (0) x=2; else x=3; x; }), "int x=0; if (0) x=2; else x=3; x;");
  expect(3, ({ int x=0; if (1-1) x=2; else x=3; x; }), "int x=0; if (1-1) x=2; else x=3; x;");
  expect(2, ({ int x=0; if (1) x=2; else x=3; x; }), "int x=0; if (1) x=2; else x=3; x;");
  expect(2, ({ int x=0; if (2-1) x=2; else x=3; x; }), "int x=0; if (2-1) x=2; else x=3; x;");

  expect(3, ({ 1; {2;} 3; }), "1; {2;} 3;");
  expect(10, ({ int i=0; i=0; while(i<10) i=i+1; i; }), "int i=0; i=0; while(i<10) i=i+1; i;");
  expect(55, ({ int i=0; int j=0; while(i<=10) {j=i+j; i=i+1;} j; }), "int i=0; int j=0; while(i<=10) {j=i+j; i=i+1;} j;");
  expect(55, ({ int i=0; int j=0; for (i=0; i<=10; i=i+1) j=i+j; j; }), "int i=0; int j=0; for (i=0; i<=10; i=i+1) j=i+j; j;");

  expect(8, add2(3, 5), "add(3, 5)");
  expect(2, sub2(5, 3), "sub(5, 3)");
  expect(21, add6(1,2,3,4,5,6), "add6(1,2,3,4,5,6)");
  expect(55, fib(9), "fib(9)");

  expect(3, ({ int x=3; *&x; }), "int x=3; *&x;");
  expect(3, ({ int x=3; int *y=&x; int **z=&y; **z; }), "int x=3; int *y=&x; int **z=&y; **z;");
  expect(5, ({ int x=3; int y=5; *(&x+1); }), "int x=3; int y=5; *(&x+1);");
  expect(5, ({ int x=3; int y=5; *(1+&x); }), "int x=3; int y=5; *(1+&x);");
  expect(3, ({ int x=3; int y=5; *(&y-1); }), "int x=3; int y=5; *(&y-1);");
  expect(2, ({ int x=3; (&x+2)-&x; }), "int x=3; (&x+2)-&x;");

  expect(5, ({ int x=3; int y=5; int *z=&x; *(z+1); }), "int x=3; int y=5; int *z=&x; *(z+1);");
  expect(3, ({ int x=3; int y=5; int *z=&y; *(z-1); }), "int x=3; int y=5; int *z=&y; *(z-1);");
  expect(5, ({ int x=3; int *y=&x; *y=5; x; }), "int x=3; int *y=&x; *y=5; x;");
  expect(7, ({ int x=3; int y=5; *(&x+1)=7; y; }), "int x=3; int y=5; *(&x+1)=7; y;");
  expect(7, ({ int x=3; int y=5; *(&y-1)=7; x; }), "int x=3; int y=5; *(&y-1)=7; x;");
  expect(8, ({ int x=3; int y=5; addx(&x, y); }), "int x=3; int y=5; addx(&x, y);");

  expect(3, ({ int x[2]; int *y=&x; *y=3; *x; }), "int x[2]; int *y=&x; *y=3; *x;");

  expect(3, ({ int x[3]; *x=3; *(x+1)=4; *(x+2)=5; *x; }), "int x[3]; *x=3; *(x+1)=4; *(x+2)=5; *x;");
  expect(4, ({ int x[3]; *x=3; *(x+1)=4; *(x+2)=5; *(x+1); }), "int x[3]; *x=3; *(x+1)=4; *(x+2)=5; *(x+1);");
  expect(5, ({ int x[3]; *x=3; *(x+1)=4; *(x+2)=5; *(x+2); }), "int x[3]; *x=3; *(x+1)=4; *(x+2)=5; *(x+2);");

  expect(0, ({ int x[2][3]; int *y=x; *y=0; **x; }), "int x[2][3]; int *y=x; *y=0; **x;");
  expect(1, ({ int x[2][3]; int *y=x; *(y+1)=1; *(*x+1); }), "int x[2][3]; int *y=x; *(y+1)=1; *(*x+1);");
  expect(2, ({ int x[2][3]; int *y=x; *(y+2)=2; *(*x+2); }), "int x[2][3]; int *y=x; *(y+2)=2; *(*x+2);");
  expect(3, ({ int x[2][3]; int *y=x; *(y+3)=3; **(x+1); }), "int x[2][3]; int *y=x; *(y+3)=3; **(x+1);");
  expect(4, ({ int x[2][3]; int *y=x; *(y+4)=4; *(*(x+1)+1); }), "int x[2][3]; int *y=x; *(y+4)=4; *(*(x+1)+1);");
  expect(5, ({ int x[2][3]; int *y=x; *(y+5)=5; *(*(x+1)+2); }), "int x[2][3]; int *y=x; *(y+5)=5; *(*(x+1)+2);");
  expect(6, ({ int x[2][3]; int *y=x; *(y+6)=6; **(x+2); }), "int x[2][3]; int *y=x; *(y+6)=6; **(x+2);");

  expect(3, ({ int x[3]; *x=3; x[1]=4; x[2]=5; *x; }), "int x[3]; *x=3; x[1]=4; x[2]=5; *x;");
  expect(4, ({ int x[3]; *x=3; x[1]=4; x[2]=5; *(x+1); }), "int x[3]; *x=3; x[1]=4; x[2]=5; *(x+1);");
  expect(5, ({ int x[3]; *x=3; x[1]=4; x[2]=5; *(x+2); }), "int x[3]; *x=3; x[1]=4; x[2]=5; *(x+2);");
  expect(5, ({ int x[3]; *x=3; x[1]=4; x[2]=5; *(x+2); }), "int x[3]; *x=3; x[1]=4; x[2]=5; *(x+2);");
  expect(5, ({ int x[3]; *x=3; x[1]=4; 2[x]=5; *(x+2); }), "int x[3]; *x=3; x[1]=4; 2[x]=5; *(x+2);");

  expect(0, ({ int x[2][3]; int *y=x; y[0]=0; x[0][0]; }), "int x[2][3]; int *y=x; y[0]=0; x[0][0];");
  expect(1, ({ int x[2][3]; int *y=x; y[1]=1; x[0][1]; }), "int x[2][3]; int *y=x; y[1]=1; x[0][1];");
  expect(2, ({ int x[2][3]; int *y=x; y[2]=2; x[0][2]; }), "int x[2][3]; int *y=x; y[2]=2; x[0][2];");
  expect(3, ({ int x[2][3]; int *y=x; y[3]=3; x[1][0]; }), "int x[2][3]; int *y=x; y[3]=3; x[1][0];");
  expect(4, ({ int x[2][3]; int *y=x; y[4]=4; x[1][1]; }), "int x[2][3]; int *y=x; y[4]=4; x[1][1];");
  expect(5, ({ int x[2][3]; int *y=x; y[5]=5; x[1][2]; }), "int x[2][3]; int *y=x; y[5]=5; x[1][2];");
  expect(6, ({ int x[2][3]; int *y=x; y[6]=6; x[2][0]; }), "int x[2][3]; int *y=x; y[6]=6; x[2][0];");

  expect(4, ({ int x; sizeof(x); }), "int x; sizeof(x);");
  expect(4, ({ int x; sizeof x; }), "int x; sizeof x;");
  expect(8, ({ int *x; sizeof(x); }), "int *x; sizeof(x);");
  expect(16, ({ int x[4]; sizeof(x); }), "int x[4]; sizeof(x);");
  expect(48, ({ int x[3][4]; sizeof(x); }), "int x[3][4]; sizeof(x);");
  expect(16, ({ int x[3][4]; sizeof(*x); }), "int x[3][4]; sizeof(*x);");
  expect(4, ({ int x[3][4]; sizeof(**x); }), "int x[3][4]; sizeof(**x);");
  expect(5, ({ int x[3][4]; sizeof(**x) + 1; }), "int x[3][4]; sizeof(**x) + 1;");
  expect(5, ({ int x[3][4]; sizeof **x + 1; }), "int x[3][4]; sizeof **x + 1;");
  expect(8, ({ int x[3][4]; sizeof(**x + 1); }), "int x[3][4]; sizeof(**x + 1);");

  expect(0, g1, "g1");
  g1=3;
  expect(3, g1, "g1");

  g2[0]=0; g2[1]=1; g2[2]=2; g2[3]=3;
  expect(0, g2[0], "g2[0]");
  expect(1, g2[1], "g2[1]");
  expect(2, g2[2], "g2[2]");
  expect(3, g2[3], "g2[3]");

  expect(4, sizeof(g1), "sizeof(g1)");
  expect(16, sizeof(g2), "sizeof(g2)");

  expect(1, ({ char x=1; x; }), "char x=1; x;");
  expect(1, ({ char x=1; char y=2; x; }), "char x=1; char y=2; x;");
  expect(2, ({ char x=1; char y=2; y; }), "char x=1; char y=2; y;");

  expect(1, ({ char x; sizeof(x); }), "char x; sizeof(x);");
  expect(10, ({ char x[10]; sizeof(x); }), "char x[10]; sizeof(x);");
  expect(1, sub_char(7, 3, 3), "sub_char(7, 3, 3)");

  expect(97, "abc"[0], "\"abc\"[0]");
  expect(98, "abc"[1], "\"abc\"[1]");
  expect(99, "abc"[2], "\"abc\"[2]");
  expect(0, "abc"[3], "\"abc\"[3]");
  expect(4, sizeof("abc"), "sizeof(\"abc\")");

  expect(7, "\a"[0], "\"\\a\"[0]");
  expect(8, "\b"[0], "\"\\b\"[0]");
  expect(9, "\t"[0], "\"\\t\"[0]");
  expect(10, "\n"[0], "\"\\n\"[0]");
  expect(11, "\v"[0], "\"\\v\"[0]");
  expect(12, "\f"[0], "\"\\f\"[0]");
  expect(13, "\r"[0], "\"\\r\"[0]");
  expect(23, "\27"[0], "\"\\27\"[0]");
  expect(0, "\0"[0], "\"\\0\"[0]");

  expect(70, "\106"[0], "\"\\106\"[0]");
  expect(71, "\107"[0], "\"\\107\"[0]");
  expect(8,  "\108"[0], "\"\\108\"[0]");

  expect(2, ({ int x=2; { int x=3; } x; }), "int x=2; { int x=3; } x;");
  expect(2, ({ int x=2; { int x=3; } int y=4; x; }), "int x=2; { int x=3; } int y=4; x;");
  expect(3, ({ int x=2; { x=3; } x; }), "int x=2; { x=3; } x;");

  expect(2, ({ int x[5]; int *y=x+2; y-x; }), "int x[5]; int *y=x+2; y-x;");

  expect(1, ({ struct {int a; int b;} x; x.a=1; x.b=2; x.a; }), "struct {int a; int b;} x; x.a=1; x.b=2; x.a;");
  expect(2, ({ struct {int a; int b;} x; x.a=1; x.b=2; x.b; }), "struct {int a; int b;} x; x.a=1; x.b=2; x.b;");
  expect(1, ({ struct {char a; int b; char c;} x; x.a=1; x.b=2; x.c=3; x.a; }), "struct {char a; int b; char c;} x; x.a=1; x.b=2; x.c=3; x.a;");
  expect(2, ({ struct {char a; int b; char c;} x; x.b=1; x.b=2; x.c=3; x.b; }), "struct {char a; int b; char c;} a; x.b=x; x.a=2; x.b=3; x.b;");
  expect(3, ({ struct {char a; int b; char c;} x; x.a=1; x.b=2; x.c=3; x.c; }), "struct {char a; int b; char c;} x; x.a=1; x.b=2; x.c=3; x.c;");

  expect(0, ({ struct {int a; int b;} x[3]; int *p=x; p[0]=0; x[0].a; }), "struct {int a; int b;} x[3]; int *p=x; p[0]=0; x[0].a;");
  expect(1, ({ struct {int a; int b;} x[3]; int *p=x; p[1]=1; x[0].b; }), "struct {int a; int b;} x[3]; int *p=x; p[1]=1; x[0].b;");
  expect(2, ({ struct {int a; int b;} x[3]; int *p=x; p[2]=2; x[1].a; }), "struct {int a; int b;} x[3]; int *p=x; p[2]=2; x[1].a;");
  expect(3, ({ struct {int a; int b;} x[3]; int *p=x; p[3]=3; x[1].b; }), "struct {int a; int b;} x[3]; int *p=x; p[3]=3; x[1].b;");

  expect(6, ({ struct {int a[3]; int b[5];} x; int *p=&x; x.a[0]=6; p[0]; }), "struct {int a[3]; int b[5];} x; int *p=&x; x.a[0]=6; p[0];");
  expect(7, ({ struct {int a[3]; int b[5];} x; int *p=&x; x.b[0]=7; p[3]; }), "struct {int a[3]; int b[5];} x; int *p=&x; x.b[0]=7; p[3];");

  expect(6, ({ struct { struct { int b; } a; } x; x.a.b=6; x.a.b; }), "struct { struct { int b; } a; } x; x.a.b=6; x.a.b;");

  expect(4, ({ struct {int a;} x; sizeof(x); }), "struct {int a;} x; sizeof(x);");
  expect(8, ({ struct {int a; int b;} x; sizeof(x); }), "struct {int a; int b;} x; sizeof(x);");
  expect(12, ({ struct {int a[3];} x; sizeof(x); }), "struct {int a[3];} x; sizeof(x);");
  expect(16, ({ struct {int a;} x[4]; sizeof(x); }), "struct {int a;} x[4]; sizeof(x);");
  expect(24, ({ struct {int a[3];} x[2]; sizeof(x); }), "struct {int a[3];} x[2]; sizeof(x)};");
  expect(2, ({ struct {char a; char b;} x; sizeof(x); }), "struct {char a; char b;} x; sizeof(x);");
  expect(8, ({ struct {char a; int b;} x; sizeof(x); }), "struct {char a; int b;} x; sizeof(x);");
  expect(8, ({ struct {int a; char b;} x; sizeof(x); }), "struct {int a; char b;} x; sizeof(x);");

  expect(7, ({ int x; char y; int a=&x; int b=&y; b-a; }), "int x; char y; int a=&x; int b=&y; b-a;");
  expect(1, ({ char x; int y; int a=&x; int b=&y; b-a; }), "char x; int y; int a=&x; int b=&y; b-a;");

  expect(2, ({ struct t {char a[2];}; { struct t {char a[4];}; } struct t y; sizeof(y); }), "struct t {char a[2];}; { struct t {char a[4];}; } struct t y; sizeof(y);");
  expect(3, ({ struct t {int x;}; int t=1; struct t y; y.x=2; t+y.x; }), "struct t {int x;}; int t=1; struct t y; y.x=2; t+y.x;");

  expect(3, ({ struct t {char a;} x; struct t *y = &x; x.a=3; y->a; }), "struct t {char a;} x; struct t *y = &x; x.a=3; y->a;");
  expect(3, ({ struct t {char a;} x; struct t *y = &x; y->a=3; x.a; }), "struct t {char a;} x; struct t *y = &x; y->a=3; x.a;");

  expect(1, ({ typedef int t; t x=1; x; }), "typedef int t; t x=1; x;");
  expect(1, ({ typedef struct {int a;} t; t x; x.a=1; x.a; }), "typedef struct {int a;} t; t x; x.a=1; x.a;");
  expect(1, ({ typedef int t; t t=1; t; }), "typedef int t; t t=1; t;");
  expect(2, ({ typedef struct {int a;} t; { typedef int t; } t x; x.a=2; x.a; }), "typedef struct {int a;} t; { typedef int t; } t x; x.a=2; x.a;");

  expect(8, ({ struct t {int a; int b;} x; struct t y; sizeof(y); }), "struct t {int a; int b;} x; struct t y; sizeof(y);");
  expect(8, ({ struct t {int a; int b;}; struct t y; sizeof(y); }), "struct t {int a; int b;}; struct t y; sizeof(y);");

  expect(2, ({ short x; sizeof(x); }), "short x; sizeof(x);");
  expect(4, ({ struct {char a; short b;} x; sizeof(x); }), "struct {char a; short b;} x; sizeof(x);");

  expect(8, ({ long x; sizeof(x); }), "long x; sizeof(x);");
  expect(16, ({ struct {char a; long b;} x; sizeof(x); }), "struct {char a; long b} x; sizeof(x);");

  expect(1, sub_short(7, 3, 3), "sub_short(7, 3, 3)");
  expect(1, sub_long(7, 3, 3), "sub_long(7, 3, 3)");

  expect(24, ({ int *x[3]; sizeof(x); }), "int *x[3]; sizeof(x);");
  expect(8, ({ int (*x)[3]; sizeof(x); }), "int (*x)[3]; sizeof(x);");
  expect(3, ({ int *x[3]; int y; x[0]=&y; y=3; x[0][0]; }), "int *x[3]; int y; x[0]=&y; y=3; x[0][0];");
  expect(4, ({ int x[3]; int (*y)[3]=x; y[0][0]=4; y[0][0]; }), "int x[3]; int (*y)[3]=x; y[0][0]=4; y[0][0];");

  expect(3, *g1_ptr(), "*g1_ptr()");

  { void *x; }

  expect(0, ({ _Bool x=0; x; }), "_Bool x=0; x;");
  expect(1, ({ _Bool x=1; x; }), "_Bool x=1; x;");
  expect(1, ({ _Bool x=2; x; }), "_Bool x=2; x;");

  expect(1, ({ char x; sizeof(x); }), "char x; sizeof(x);");
  expect(2, ({ short int x; sizeof(x); }), "short int x; sizeof(x);");
  expect(2, ({ int short x; sizeof(x); }), "int short x; sizeof(x);");
  expect(4, ({ int x; sizeof(x); }), "int x; sizeof(x);");
  expect(4, ({ typedef t; t x; sizeof(x); }), "typedef t; t x; sizeof(x);");
  expect(4, ({ typedef typedef t; t x; sizeof(x); }), "typedef typedef t; t x; sizeof(x);");
  expect(8, ({ long int x; sizeof(x); }), "long int x; sizeof(x);");
  expect(8, ({ int long x; sizeof(x); }), "int long x; sizeof(x);");
  expect(8, ({ long long x; sizeof(x); }), "long long x; sizeof(x);");
  expect(8, ({ long int long x; sizeof(x); }), "long int long x; sizeof(x);");

  expect(3, ({ MyInt x=3; x; }), "MyInt x=3; x;");

  expect(1, sizeof(char), "sizeof(char)");
  expect(2, sizeof(short), "sizeof(short)");
  expect(2, sizeof(short int), "sizeof(short int)");
  expect(2, sizeof(int short), "sizeof(int short)");
  expect(4, sizeof(int), "sizeof(int)");
  expect(8, sizeof(long), "sizeof(long)");
  expect(8, sizeof(long int), "sizeof(long int)");
  expect(8, sizeof(long int), "sizeof(long int)");
  expect(8, sizeof(char *), "sizeof(char *)");
  expect(8, sizeof(int *), "sizeof(int *)");
  expect(8, sizeof(long *), "sizeof(long *)");
  expect(8, sizeof(int **), "sizeof(int **)");
  expect(8, sizeof(int(*)[4]), "sizeof(int(*)[4])");
  expect(32, sizeof(int*[4]), "sizeof(int(*)[4])");
  expect(16, sizeof(int[4]), "sizeof(int[4])");
  expect(48, sizeof(int[3][4]), "sizeof(int[3][4])");
  expect(8, sizeof(struct {int a; int b;}), "sizeof(struct {int a; int b;})");

  expect(131585, (int)8590066177, "(int)8590066177");
  expect(513, (short)8590066177, "(short)8590066177");
  expect(1, (char)8590066177, "(char)8590066177");
  expect(1, (_Bool)1, "(_Bool)1");
  expect(1, (_Bool)2, "(_Bool)2");
  expect(0, (_Bool)(char)256, "(_Bool)(char)256");
  expect(1, (long)1, "(long)1");
  expect(0, (long)&*(int *)0, "(long)&*(int *)0");
  expect(5, ({ int x=5; long y=(long)&x; *(int*)y; }), "int x=5; long y=(long)&x; *(int*)y");

  expect(97, 'a', "'a'");
  expect(10, '\n', "\'\\n\'");

  expect(0, ({ enum { zero, one, two }; zero; }), "enum { zero, one, two }; zero;");
  expect(1, ({ enum { zero, one, two }; one; }), "enum { zero, one, two }; one;");
  expect(2, ({ enum { zero, one, two }; two; }), "enum { zero, one, two }; two;");
  expect(5, ({ enum { five=5, six, seven }; five; }), "enum { five=5, six, seven }; five;");
  expect(6, ({ enum { five=5, six, seven }; six; }), "enum { five=5, six, seven }; six;");
  expect(0, ({ enum { zero, five=5, three=3, four }; zero; }), "enum { zero, five=5, three=3, four }; zero;");
  expect(5, ({ enum { zero, five=5, three=3, four }; five; }), "enum { zero, five=5, three=3, four }; five;");
  expect(3, ({ enum { zero, five=5, three=3, four }; three; }), "enum { zero, five=5, three=3, four }; three;");
  expect(4, ({ enum { zero, five=5, three=3, four }; four; }), "enum { zero, five=5, three=3, four }; four;");
  expect(4, ({ enum { zero, one, two } x; sizeof(x); }), "enum { zero, one, two } x; sizeof(x);");
  expect(4, ({ enum t { zero, one, two }; enum t y; sizeof(y); }), "enum t { zero, one, two }; enum t y; sizeof(y);");

  expect(3, static_fn(), "static_fn()");

  expect(55, ({ int j=0; for (int i=0; i<=10; i=i+1) j=j+i; j; }), "int j=0; for (int i=0; i<=10; i=i+1) j=j+i; j;");
  expect(3, ({ int i=3; int j=0; for (int i=0; i<=10; i=i+1) j=j+i; i; }), "int i=3; int j=0; for (int i=0; i<=10; i=i+1) j=j+i; i;");

  expect(3, (1,2,3), "(1,2,3)");

  expect(3, ({ int i=2; ++i; }), "int i=2; ++i;");
  expect(1, ({ int i=2; --i; }), "int i=2; --i;");
  expect(2, ({ int i=2; i++; }), "int i=2; i++;");
  expect(2, ({ int i=2; i--; }), "int i=2; i--;");
  expect(3, ({ int i=2; i++; i; }), "int i=2; i++; i;");
  expect(1, ({ int i=2; i--; i; }), "int i=2; i--; i;");
  expect(1, ({ int a[3]; a[0]=0; a[1]=1; a[2]=2; int *p=a+1; *p++; }), "int a[3]; a[0]=0; a[1]=1; a[2]=2; int *p=a+1; *p++;");
  expect(2, ({ int a[3]; a[0]=0; a[1]=1; a[2]=2; int *p=a+1; ++*p; }), "int a[3]; a[0]=0; a[1]=1; a[2]=2; int *p=a+1; ++*p;");
  expect(1, ({ int a[3]; a[0]=0; a[1]=1; a[2]=2; int *p=a+1; *p--; }), "int a[3]; a[0]=0; a[1]=1; a[2]=2; int *p=a+1; *p--;");
  expect(0, ({ int a[3]; a[0]=0; a[1]=1; a[2]=2; int *p=a+1; --*p; }), "int a[3]; a[0]=0; a[1]=1; a[2]=2; int *p=a+1; --*p;");

  expect(0, ({ int a[3]; a[0]=0; a[1]=1; a[2]=2; int *p=a+1; (*p++)--; a[0]; }), "int a[3]; a[0]=0; a[1]=1; a[2]=2; int *p=a+1; (*p++); a[0];");
  expect(0, ({ int a[3]; a[0]=0; a[1]=1; a[2]=2; int *p=a+1; (*p++)--; a[1]; }), "int a[3]; a[0]=0; a[1]=1; a[2]=2; int *p=a+1; (*p++); a[0];");
  expect(2, ({ int a[3]; a[0]=0; a[1]=1; a[2]=2; int *p=a+1; (*p++)--; a[2]; }), "int a[3]; a[0]=0; a[1]=1; a[2]=2; int *p=a+1; (*p++); a[0];");
  expect(2, ({ int a[3]; a[0]=0; a[1]=1; a[2]=2; int *p=a+1; (*p++)--; *p; }), "int a[3]; a[0]=0; a[1]=1; a[2]=2; int *p=a+1; (*p++); a[0];");

  expect(7, ({ int i=2; i+=5; i; }), "int i=2; i+=5; i;");
  expect(7, ({ int i=2; i+=5; }), "int i=2; i+=5;");
  expect(3, ({ int i=5; i-=2; i; }), "int i=5; i-=2; i;");
  expect(3, ({ int i=5; i-=2; }), "int i=5; i-=2;");
  expect(6, ({ int i=3; i*=2; i; }), "int i=3; i*=2; i;");
  expect(6, ({ int i=3; i*=2; }), "int i=3; i*=2;");
  expect(3, ({ int i=6; i/=2; i; }), "int i=6; i/=2; i;");
  expect(3, ({ int i=6; i/=2; }), "int i=6; i/=2;");

  expect(511, 0777, "0777");
  expect(0, 0x0, "0x0");
  expect(10, 0xa, "0xa");
  expect(10, 0XA, "0XA");
  expect(48879, 0xbeef, "0xbeef");
  expect(48879, 0xBEEF, "0xBEEF");
  expect(48879, 0XBEEF, "0XBEEF");
  expect(0, 0b0, "0b0");
  expect(1, 0b1, "0b1");
  expect(47, 0b101111, "0b101111");
  expect(47, 0B101111, "0B101111");

  expect(0, !1, "!1");
  expect(0, !2, "!2");
  expect(1, !0, "!0");

  expect(-1, ~0, "~0");
  expect(0, ~-1, "~-1");

  expect(0, 0&1, "0&1");
  expect(1, 3&1, "3&1");
  expect(3, 7&3, "7&3");
  expect(10, -1&10, " -1&10");

  expect(1, 0|1, "0|1");
  expect(0b10011, 0b10000|0b00011, "01000|0b0011");

  expect(0, 0^0, "0^0");
  expect(0, 0b1111^0b1111, "0b1111^0b1111");
  expect(0b110100, 0b111000^0b001100, "0b111000^0b001100");

  expect(1, 0||1, "0||1");
  expect(1, 0||(2-2)||5, "0||(2-2)||5");
  expect(0, 0||0, "0||0");
  expect(0, 0||(2-2), "0||(2-2)");

  expect(0, 0&&1, "0&&1");
  expect(0, (2-2)&&5, "(2-2)&&5");
  expect(1, 1&&5, "1&&5");

  expect(3, ({ int x[2]; x[0]=3; param_decay(x); }), "int x[2]; x[0]=3; param_decay(x);");

  expect(8, ({ struct *foo; sizeof(foo); }), "struct *foo; sizeof(foo);");
  expect(4, ({ struct T *foo; struct T {int x;}; sizeof(struct T); }), "struct T *foo; struct T {int x;}; sizeof(struct T);");
  expect(1, ({ struct T { struct T *next; int x; } a; struct T b; b.x=1; a.next=&b; a.next->x; }), "struct T { struct T *next; int x; } a; struct T b; b.x=1; a.next=&b; a.next->x;");

  expect(3, ({ int i=0; for(;i<10;i++) { if (i == 3) break; } i; }), "int i=0; for(;i<10;i++) { if (i == 3) break; } i;");
  expect(4, ({ int i=0; while (1) { if (i++ == 3) break; } i; }), "int i=0; while { if (i == 3) break; } i;");
  expect(3, ({ int i=0; for(;i<10;i++) { for (;;) break; if (i == 3) break; } i; }), "int i=0; for(;i<10;i++) { if (i == 3) break; } i;");
  expect(4, ({ int i=0; while (1) { while(1) break; if (i++ == 3) break; } i; }), "int i=0; while { if (i == 3) break; } i;");

  expect(10, ({ int i=0; int j=0; for (;i<10;i++) { if (i>5) continue; j++; } i; }), "int i=0; int j=0; for (;i<10;i++) { if (i>5) continue; j++; } i;");
  expect(6, ({ int i=0; int j=0; for (;i<10;i++) { if (i>5) continue; j++; } j; }), "int i=0; int j=0; for (;i<10;i++) { if (i>5) continue; j++; } j;");
  expect(10, ({ int i=0; int j=0; for(;!i;) { for (;j!=10;j++) continue; break; } j; }), "int i=0; int j=0; for(;!i;) { for (;j!=10;j++) continue; break; } j;");
  expect(11, ({ int i=0; int j=0; while (i++<10) { if (i>5) continue; j++; } i; }), "int i=0; int j=0; while (i++<10) { if (i>5) continue; j++; } i;");
  expect(5, ({ int i=0; int j=0; while (i++<10) { if (i>5) continue; j++; } j; }), "int i=0; int j=0; while (i++<10) { if (i>5) continue; j++; } j;");
  expect(11, ({ int i=0; int j=0; while(!i) { while (j++!=10) continue; break; } j; }), "int i=0; int j=0; while(!i) { while (j++!=10) continue; break; } j;");

  expect(3, ({ int i=0; goto a; a: i++; b: i++; c: i++; i; }), "int i=0; goto a; a: i++; b: i++; c: i++; i;");
  expect(2, ({ int i=0; goto e; d: i++; e: i++; f: i++; i; }), "int i=0; goto d; d: i++; e: i++; f: i++; i;");
  expect(1, ({ int i=0; goto i; g: i++; h: i++; i: i++; i; }), "int i=0; goto g; h: i++; i: i++; j: i++; i;");

  expect(5, ({ int i=0; switch(0) { case 0:i=5;break; case 1:i=6;break; case 2:i=7;break; } i; }), "int i=0; switch(0) { case 0:i=5;break; case 1:i=6;break; case 2:i=7;break; } i;");
  expect(6, ({ int i=0; switch(1) { case 0:i=5;break; case 1:i=6;break; case 2:i=7;break; } i; }), "int i=0; switch(1) { case 0:i=5;break; case 1:i=6;break; case 2:i=7;break; } i;");
  expect(7, ({ int i=0; switch(2) { case 0:i=5;break; case 1:i=6;break; case 2:i=7;break; } i; }), "int i=0; switch(2) { case 0:i=5;break; case 1:i=6;break; case 2:i=7;break; } i;");
  expect(0, ({ int i=0; switch(3) { case 0:i=5;break; case 1:i=6;break; case 2:i=7;break; } i; }), "int i=0; switch(3) { case 0:i=5;break; case 1:i=6;break; case 2:i=7;break; } i;");
  expect(5, ({ int i=0; switch(0) { case 0:i=5;break; default:i=7; } i; }), "int i=0; switch(0) { case 0:i=5;break; default:i=7; } i;");
  expect(7, ({ int i=0; switch(1) { case 0:i=5;break; default:i=7; } i; }), "int i=0; switch(1) { case 0:i=5;break; default:i=7; } i;");
  expect(2, ({ int i=0; switch(1) { case 0: 0; case 1: 0; case 2: 0; i=2; } i; }), "int i=0; switch(1) { case 0: 0; case 1: 0; case 2: 0; i=2; } i;");
  expect(0, ({ int i=0; switch(3) { case 0: 0; case 1: 0; case 2: 0; i=2; } i; }), "int i=0; switch(3) { case 0: 0; case 1: 0; case 2: 0; i=2; } i;");

  voidfn();

  expect(1, 1<<0, "1<<0");
  expect(8, 1<<3, "1<<3");
  expect(10, 5<<1, "5<<1");
  expect(2, 5>>1, "5>>1");
  expect(-1, -1>>1, "-1>>1");
  expect(1, ({ int i=1; i<<=0; i; }), "int i=1; i<<0;");
  expect(8, ({ int i=1; i<<=3; i; }), "int i=1; i<<3;");
  expect(10, ({ int i=5; i<<=1; i; }), "int i=5; i<<1;");
  expect(2, ({ int i=5; i>>=1; i; }), "int i=5; i>>1;");
  expect(-1, -1, "-1");
  expect(-1, ({ int i=-1; i; }), "int i=-1; i;");
  expect(-1, ({ int i=-1; i>>=1; i; }), "int i=1; i>>1;");

  expect(2, 0?1:2, "0?1:2");
  expect(1, 1?1:2, "0?1:2");

  expect(10, ({ enum { ten=1+2+3+4, }; ten; }), "enum { ten=1+2+3+4, }; ten;");
  expect(1, ({ int i=0; switch(3) { case 5-2+0*3: i++; } i; }), "int i=0; switch(3) { case 5-2+0*3: i++; ); i;");
  expect(8, ({ int x[1+1]; sizeof(x); }), "int x[1+1]; sizeof(x);");
  expect(2, ({ char x[1?2:3]; sizeof(x); }), "char x[0?2:3]; sizeof(x);");
  expect(3, ({ char x[0?2:3]; sizeof(x); }), "char x[1?2:3]; sizeof(x);");

  expect(1, ({ int x[3]={1,2,3}; x[0]; }), "int x[3]={1,2,3}; x[0];");
  expect(2, ({ int x[3]={1,2,3}; x[1]; }), "int x[3]={1,2,3}; x[0];");
  expect(3, ({ int x[3]={1,2,3}; x[2]; }), "int x[3]={1,2,3}; x[0];");
  expect(3, ({ int x[3]={1,2,3,}; x[2]; }), "int x[3]={1,2,3}; x[0];");

  expect(0, ({ int x[3]={0}; x[0]; }), "int x[3]={}; x[0];");
  expect(0, ({ int x[3]={0}; x[1]; }), "int x[3]={}; x[1];");
  expect(0, ({ int x[3]={0}; x[2]; }), "int x[3]={}; x[2];");

  expect(2, ({ int x[2][3]={{1,2,3},{4,5,6}}; x[0][1]; }), "int x[2][3]={{1,2,3},{4,5,6}}; x[0][1];");
  expect(4, ({ int x[2][3]={{1,2,3},{4,5,6}}; x[1][0]; }), "int x[2][3]={{1,2,3},{4,5,6}}; x[1][0];");
  expect(6, ({ int x[2][3]={{1,2,3},{4,5,6}}; x[1][2]; }), "int x[2][3]={{1,2,3},{4,5,6}}; x[1][2];");

  expect(2, ({ int x[2][3]={{1,2}}; x[0][1]; }), "int x[2][3]={{1,2}}; x[0][1];");
  expect(0, ({ int x[2][3]={{1,2}}; x[1][0]; }), "int x[2][3]={{1,2}}; x[1][0];");
  expect(0, ({ int x[2][3]={{1,2}}; x[1][2]; }), "int x[2][3]={{1,2}}; x[1][2];");

  expect('a', ({ char x[4]="abc"; x[0]; }), "char x[4]=\"abc\"; x[0]; }");
  expect('c', ({ char x[4]="abc"; x[2]; }), "char x[4]=\"abc\"; x[2]; }");
  expect(0, ({ char x[4]="abc"; x[3]; }), "char x[4]=\"abc\"; x[3]; }");
  expect('a', ({ char x[2][4]={"abc","def"}; x[0][0]; }), "char x[2][4]=\"abc\",\"def\"}; x[0][0]; }");
  expect(0, ({ char x[2][4]={"abc","def"}; x[0][3]; }), "char x[2][4]=\"abc\",\"def\"}; x[0][3]; }");
  expect('d', ({ char x[2][4]={"abc","def"}; x[1][0]; }), "char x[2][4]=\"abc\",\"def\"}; x[1][0]; }");
  expect('f', ({ char x[2][4]={"abc","def"}; x[1][2]; }), "char x[2][4]=\"abc\",\"def\"}; x[1][2]; }");

  expect(4, ({ int x[]={1,2,3,4}; x[3]; }), "int x[]={1,2,3,4}; x[3];");
  expect(16, ({ int x[]={1,2,3,4}; sizeof(x); }), "int x[]={1,2,3,4}; sizeof(x);");
  expect(4, ({ char x[]="foo"; sizeof(x); }), "char x[]=\"foo\"; sizeof(x); }");

  expect(1, ({ struct {int a; int b; int c;} x={1,2,3}; x.a; }), "struct {int a; int b; int c;} x={1,2,3}; x.a;");
  expect(2, ({ struct {int a; int b; int c;} x={1,2,3}; x.b; }), "struct {int a; int b; int c;} x={1,2,3}; x.b;");
  expect(3, ({ struct {int a; int b; int c;} x={1,2,3}; x.c; }), "struct {int a; int b; int c;} x={1,2,3}; x.c;");
  expect(1, ({ struct {int a; int b; int c;} x={1}; x.a; }), "struct {int a; int b; int c;} x={1}; x.a;");
  expect(0, ({ struct {int a; int b; int c;} x={1}; x.b; }), "struct {int a; int b; int c;} x={1}; x.b;");
  expect(0, ({ struct {int a; int b; int c;} x={1}; x.c; }), "struct {int a; int b; int c;} x={1}; x.c;");

  expect(1, ({ struct {int a; int b;} x[2]={{1,2},{3,4}}; x[0].a; }), "struct {int a; int b;} x[2]={{1,2},{3,4}}; x[0].a;");
  expect(2, ({ struct {int a; int b;} x[2]={{1,2},{3,4}}; x[0].b; }), "struct {int a; int b;} x[2]={{1,2},{3,4}}; x[0].b;");
  expect(3, ({ struct {int a; int b;} x[2]={{1,2},{3,4}}; x[1].a; }), "struct {int a; int b;} x[2]={{1,2},{3,4}}; x[1].a;");
  expect(4, ({ struct {int a; int b;} x[2]={{1,2},{3,4}}; x[1].b; }), "struct {int a; int b;} x[2]={{1,2},{3,4}}; x[1].b;");

  expect(0, ({ struct {int a; int b;} x[2]={{1,2}}; x[1].b; }), "struct {int a; int b;} x[2]={{1,2}}; x[1].b;");

  expect(0, ({ struct {int a; int b;} x={0}; x.a; }), "struct {int a; int b;} x={}; x.a;");
  expect(0, ({ struct {int a; int b;} x={0}; x.b; }), "struct {int a; int b;} x={}; x.b;");

  expect(3, g3, "g3");
  expect(4, g4, "g4");
  expect(5, g5, "g5");
  expect(6, g6, "g6");
  expect(5, *g7, "*g7");
  expect(0, strcmp(g8, "abc"), "strcmp(g8, \"abc\")");

  expect(0, g9[0], "g9[0]");
  expect(1, g9[1], "g9[1]");
  expect(2, g9[2], "g9[2]");

  expect(0, strcmp(g10[0], "foo"), "strcmp(g10[0], \"foo\")");
  expect(0, strcmp(g10[1], "bar"), "strcmp(g10[1], \"bar\")");
  expect(0, g10[1][3], "g10[1][3]");
  expect(2, sizeof(g10) / sizeof(*g10), "sizeof(g10) / sizeof(*g10)");

  expect(1, g11[0].a, "g11[0].a");
  expect(2, g11[0].b, "g11[0].b");
  expect(3, g11[1].a, "g11[1].a");
  expect(4, g11[1].b, "g11[1].b");

  expect(1, g12[0].a[0], "g12[0].a[0]");
  expect(2, g12[0].a[1], "g12[0].a[1]");
  expect(0, g12[1].a[0], "g12[1].a[0]");
  expect(0, g12[1].a[1], "g12[1].a[1]");

  expect(1, g13[0].a[0], "g13[0].a[0]");
  expect(2, g13[0].a[1], "g13[0].a[1]");
  expect(3, g13[1].a[0], "g13[1].a[0]");
  expect(4, g13[1].a[1], "g13[1].a[1]");

  expect(1, g14[0].a[0], "g14[0].a[0]");
  expect(2, g14[0].a[1], "g14[0].a[1]");
  expect(3, g14[1].a[0], "g14[1].a[0]");
  expect(4, g14[1].a[1], "g14[1].a[1]");

  expect(0, ({ int x[2][3]={0,1,2,3,4,5,}; x[0][0]; }), "int x[2][3]={0,1,2,3,4,5,}; x[0][0];");
  expect(3, ({ int x[2][3]={0,1,2,3,4,5,}; x[1][0]; }), "int x[2][3]={0,1,2,3,4,5,}; x[1][0];");

  expect(0, ({ struct {int a; int b;} x[2]={0,1,2,3}; x[0].a; }), "struct {int a; int b;} x[2]={0,1,2,3}; x[0].a;");
  expect(2, ({ struct {int a; int b;} x[2]={0,1,2,3}; x[1].a; }), "struct {int a; int b;} x[2]={0,1,2,3}; x[1].a;");

  expect(0, strcmp(g15, "foo"), "strcmp(g15, \"foo\")");
  expect(0, strcmp(g16[0], "foo"), "strcmp(g16[0], \"foo\")");
  expect(0, strcmp(g16[1], "bar"), "strcmp(g16[1], \"bar\")");

  expect(7, sizeof(g17), "sizeof(g17)");
  expect(10, sizeof(g18), "sizeof(g18)");
  expect(3, sizeof(g19), "sizeof(g19)");

  expect(0, memcmp(g17, "foobar", 7), "memcmp(g17, \"foobar\", 7)");
  expect(0, memcmp(g18, "foobar\0\0\0", 10), "memcmp(g18, \"foobar\\0\\0\\0\", 10)");
  expect(0, memcmp(g19, "foo", 3), "memcmp(g19, \"foo\", 3)");

  expect(0, strcmp(g20, "foobar"), "strcmp(g20, \"foobar\")");
  expect(0, strcmp(g21, "bar"), "strcmp(g21, \"bar\")");
  expect(0, strcmp(g22+3, "foobar"), "strcmp(g22+3, \"foobar\")");

  expect(0, strcmp(g23[0], "foobar"), "strcmp(g23[0], \"foobar\")");
  expect(0, strcmp(g23[1], "bar"), "strcmp(g23[1], \"bar\")");
  expect(0, strcmp(g23[2]+3, "foobar"), "strcmp(g23[2]+3, \"foobar\")");

  expect(3, g24, "g24");
  expect(3, *g25, "*g25");
  expect(2, *g27, "*g27");

  ext1 = 5;
  expect(5, ext1, "ext1");

  ext2 = &ext1;
  expect(5, *ext2, "*ext2");

  ;

  expect(1, _Alignof(char), "_Alignof(char)");
  expect(2, _Alignof(short), "_Alignof(short)");
  expect(4, _Alignof(int), "_Alignof(int)");
  expect(8, _Alignof(long), "_Alignof(long)");
  expect(8, _Alignof(long long), "_Alignof(long long)");
  expect(1, _Alignof(char[3]), "_Alignof(char[3])");
  expect(4, _Alignof(int[3]), "_Alignof(int[3])");
  expect(1, _Alignof(struct {char a; char b;}[2]), "_Alignof(struct {char a; char b;}[2])");
  expect(8, _Alignof(struct {char a; long b;}[2]), "_Alignof(struct {char a; long b;}[2])");

  expect(2, counter(), "counter()");
  expect(4, counter(), "counter()");
  expect(6, counter(), "counter()");

  expect(1, (int){1}, "(int){1}");
  expect(2, ((int[]){0,1,2})[2], "(int[]){0,1,2}[2]");
  expect('a', ((struct {char a; int b;}){'a', 3}).a, "((struct {char a; int b;}){'a', 3}).a");
  expect(3, ({ int x=3; (int){x}; }), "int x=3; (int){x};");

  expect(1, tree->val, "tree->val");
  expect(2, tree->lhs->val, "tree->lhs->val");
  expect(3, tree->lhs->lhs->val, "tree->lhs->lhs->val");
  expect(4, tree->lhs->rhs->val, "tree->lhs->rhs->val");

  ret_none();

  expect(3, ext3, "ext3");

  expect(2, ({ int i=6; i&=3; i; }), "int i=6; i&=3; i;");
  expect(7, ({ int i=6; i|=3; i; }), "int i=6; i|=3; i;");
  expect(10, ({ int i=15; i^=5; i; }), "int 15; i^=5; i;");

  expect(7, ({ int i=0; int j=0; do { j++; } while (i++ < 6); j; }), "int i=0; int j=0; do { j++; } while (i++ < 6); j;");
  expect(4, ({ int i=0; int j=0; int k=0; do { if (++j > 3) break; continue; k++; } while (1); j; }), "int i=0; int j=0; int k=0; do { if (j++ > 3) break; continue; k++; } while (1); j;");

  expect(1, true_fn(), "true_fn()");
  expect(0, false_fn(), "false_fn()");

  expect(1, sizeof(char), "sizeof(char)");
  expect(1, sizeof(signed char), "sizeof(signed char)");
  expect(1, sizeof(signed char signed), "sizeof(signed char signed)");

  expect(2, sizeof(short), "sizeof(short)");
  expect(2, sizeof(int short), "sizeof(int short)");
  expect(2, sizeof(short int), "sizeof(short int)");
  expect(2, sizeof(signed short), "sizeof(signed short)");
  expect(2, sizeof(int short signed), "sizeof(int short signed)");

  expect(4, sizeof(int), "sizeof(int)");
  expect(4, sizeof(signed int), "sizeof(signed int)");
  expect(4, sizeof(signed), "sizeof(signed)");
  expect(4, sizeof(signed signed), "sizeof(signed signed)");

  expect(8, sizeof(long), "sizeof(long)");
  expect(8, sizeof(signed long), "sizeof(signed long)");
  expect(8, sizeof(signed long int), "sizeof(signed long int)");

  expect(8, sizeof(long long), "sizeof(long long)");
  expect(8, sizeof(signed long long), "sizeof(signed long long)");
  expect(8, sizeof(signed long long int), "sizeof(signed long long int)");

  expect(4, sizeof(0), "sizeof(0)");
  expect(8, sizeof(0L), "sizeof(0L)");
  expect(8, sizeof(0LL), "sizeof(0LL)");
  expect(8, sizeof(0l), "sizeof(0l)");
  expect(8, sizeof(0ll), "sizeof(0ll)");
  expect(8, sizeof(0x0L), "sizeof(0x0L)");
  expect(8, sizeof(0b0L), "sizeof(0b0L)");
  expect(4, sizeof(2147483647), "sizeof(2147483647)");
  expect(8, sizeof(2147483648), "sizeof(2147483648)");

  typedef enum { TestEnumA = 1, TestEnumB } TestEnum;
  TestEnum enumValue = TestEnumB;
  TestEnum *enumValue2 = &enumValue;
  expect(1, enumValue - 1, "enumValue - 1");
  expect(0, *enumValue2 - 2, "*enumValue2 - 2");
}
