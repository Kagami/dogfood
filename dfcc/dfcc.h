#include <stdbool.h>
#include <stdio.h>

typedef struct Type Type;
typedef struct Token Token;
typedef struct Stream Stream;
typedef struct Member Member;
typedef struct Initializer Initializer;

//
// error.c
//

void error_init(bool warn_is_error);
void error(const char *fmt, ...);
void error_at(const char *loc, const char *fmt, ...);
void error_tok(Token *tok, const char *fmt, ...);
void warn_at(const char *loc, const char *fmt, ...);
void warn_tok(Token *tok, const char *fmt, ...);

//
// cpp.c
//

void cpp_idir(const char *dir);
Token *cpp(void);

//
// lex.c
//

// Token
typedef enum {
  TK_DIRECTIVE, // Preprocessing directive
  TK_RESERVED,  // Keywords or punctuators
  TK_IDENT,     // Identifiers
  TK_STR,       // String literals
  TK_NUM,       // Integer literals
  TK_EOF,       // End-of-file markers
  TK_NEWLINE,   // End-of-line markers
} TokenKind;

// Token type
struct Token {
  TokenKind kind;  // Token kind
  Token *next;     // Next token
  long val;        // If kind is TK_NUM, its value
  Type *ty;        // Used if TK_NUM
  const char *str; // Token string
  int len;         // Token length
  char *contents;  // String literal contents including terminating '\0'
  int cont_len;    // string literal length
  Stream *origin;  // Token origin stream
};

Token *new_token(TokenKind kind, const char *str, int len);
Token *token_copy(Token *tok);
Token *token_deepcopy(Token *tok, Token **last);
bool token_match(Token *tok, const char *str);
Token *lex_headername(bool *is_angle);
Token *lex_one(void);

//
// parse.c
//

// Variable
typedef struct Var Var;
struct Var {
  char *name;    // Variable name
  Type *ty;      // Type
  bool is_local; // local or global

  // Local variable
  int offset;    // Offset from RBP

  // Global variable
  bool is_static;
  Initializer *initializer;
};

typedef struct VarList VarList;
struct VarList {
  VarList *next;
  Var *var;
};

// AST node
typedef enum {
  ND_ADD,        // num + num
  ND_PTR_ADD,    // ptr + num or num + ptr
  ND_SUB,        // num - num
  ND_PTR_SUB,    // ptr - num
  ND_PTR_DIFF,   // ptr - ptr
  ND_MUL,        // *
  ND_DIV,        // /
  ND_BITAND,     // &
  ND_BITOR,      // |
  ND_BITXOR,     // ^
  ND_SHL,        // <<
  ND_SHR,        // >>
  ND_EQ,         // ==
  ND_NE,         // !=
  ND_LT,         // <
  ND_LE,         // <=
  ND_ASSIGN,     // =
  ND_TERNARY,    // ?:
  ND_PRE_INC,    // pre ++
  ND_PRE_DEC,    // pre --
  ND_POST_INC,   // post ++
  ND_POST_DEC,   // post --
  ND_ADD_EQ,     // +=
  ND_PTR_ADD_EQ, // +=
  ND_SUB_EQ,     // -=
  ND_PTR_SUB_EQ, // -=
  ND_MUL_EQ,     // *=
  ND_DIV_EQ,     // /=
  ND_SHL_EQ,     // <<=
  ND_SHR_EQ,     // >>=
  ND_BITAND_EQ,  // &=
  ND_BITOR_EQ,   // |=
  ND_BITXOR_EQ,  // ^=
  ND_COMMA,      // ,
  ND_MEMBER,     // . (struct member access)
  ND_ADDR,       // unary &
  ND_DEREF,      // unary *
  ND_NOT,        // !
  ND_BITNOT,     // ~
  ND_LOGAND,     // &&
  ND_LOGOR,      // ||
  ND_RETURN,     // "return"
  ND_IF,         // "if"
  ND_WHILE,      // "while"
  ND_FOR,        // "for"
  ND_DO,         // "do"
  ND_SWITCH,     // "switch"
  ND_CASE,       // "case"
  ND_BLOCK,      // { ... }
  ND_BREAK,      // "break"
  ND_CONTINUE,   // "continue"
  ND_GOTO,       // "goto"
  ND_LABEL,      // Labeled statement
  ND_FUNCALL,    // Function call
  ND_EXPR_STMT,  // Expression statement
  ND_STMT_EXPR,  // Statement expression
  ND_VAR,        // Variable
  ND_NUM,        // Integer
  ND_CAST,       // Type cast
  ND_NULL,       // Empty statement
} NodeKind;

// AST node type
typedef struct Node Node;
struct Node {
  NodeKind kind; // Node kind
  Node *next;    // Next node
  Type *ty;      // Type, e.g. int or pointer to int
  Token *tok;    // Representative token

  Node *lhs;     // Left-hand side
  Node *rhs;     // Right-hand side

  // "if, "while" or "for" statement
  Node *cond;
  Node *then;
  Node *els;
  Node *init;
  Node *inc;

  // Block or statement expression
  Node *body;

  // Struct member access
  Member *member;

  // Function call
  const char *funcname;
  Node *args;

  // Goto or labeled statement
  const char *label_name;

  // Switch-cases
  Node *case_next;
  Node *default_case;
  int case_label;
  int case_end_label;

  // Variable
  Var *var;

  // Integer literal
  long val;
};

// Global variable initializer. Global variables can be initialized
// either by a constant expression or a pointer to another global
// variable with an addend.
struct Initializer {
  Initializer *next;

  // Constant expression
  int sz;
  long val;

  // Reference to another global variable
  const char *label;
  long addend;
};

typedef struct Function Function;
struct Function {
  Function *next;
  const char *name;
  VarList *params;
  bool is_static;
  bool has_varargs;

  Node *node;
  VarList *locals;
  int stack_size;
};

typedef struct {
  VarList *globals;
  Function *fns;
} Program;

Program *parse(Token *token);

//
// gen.c
//

void gen_offsets(Program *prog);
void gen_prog(Program *prog, const char *outpath, bool dump_asm);

//
// type.c
//

typedef enum {
  TY_VOID,
  TY_BOOL,
  TY_CHAR,
  TY_SHORT,
  TY_INT,
  TY_LONG,
  TY_ENUM,
  TY_PTR,
  TY_ARRAY,
  TY_STRUCT,
  TY_FUNC,
} TypeKind;

struct Type {
  TypeKind kind;
  int size;           // sizeof() value
  int align;          // alignment
  bool is_incomplete; // incomplete type

  Type *base;         // pointer or array
  int array_len;      // array
  Member *members;    // struct
  Type *return_ty;    // function
};

// Struct member
struct Member {
  Member *next;
  Type *ty;
  Token *tok; // for error message
  const char *name;
  int offset;
};

extern Type *gVoidType;
extern Type *gBoolType;
extern Type *gCharType;
extern Type *gShortType;
extern Type *gIntType;
extern Type *gLongType;

bool is_integer(Type *ty);
int align_to(int n, int align);
Type *pointer_to(Type *base);
Type *array_of(Type *base, int size);
Type *func_type(Type *return_ty);
Type *enum_type(void);
Type *struct_type(void);
void add_type(Node *node);

//
// stream.c
//

struct Stream {
  const char *name; // human-readable
  const char *path; // NULL = stdin
  const char *contents;
  const char *pos;
  Stream *prev;
};

void stream_push(const char *path);
Stream *stream_pop(void);
Stream *stream_head(void);
const char *stream_path(void);
const char *stream_pos(void);
void stream_setpos(const char *pos);
bool stream_atbol(void);

//
// vec.c
//

typedef struct {
  const void **elems;
  size_t size;
  size_t cap;
} Vec;

Vec *new_vec(void);
void vec_push(Vec *v, const void *elem);

typedef struct {
  unsigned char *data;
  size_t size;
  size_t cap;
} Buf;

Buf *new_buf(void);
size_t buf_write(Buf *b, const void *chunk, size_t chunk_size);
size_t buf_writestr(Buf *b, const char *str);

//
// map.c
//

typedef struct {
  const char **keys;
  const void **vals;
  size_t size;
  size_t nused;
} Map;

Map *new_map(void);
const void *map_get(Map *m, const char *key);
const void *map_getbyview(Map *m, const char *key, size_t key_len);
void map_put(Map *m, const char *key, const void *val);
