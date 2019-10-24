#include <assert.h>
#include <elf.h>
#include <errno.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/wait.h>
#include "dfcc.h"

static const char *gArgreg1[] = {"dil", "sil",  "dl",  "cl", "r8b", "r9b"};
static const char *gArgreg2[] = { "di",  "si",  "dx",  "cx", "r8w", "r9w"};
static const char *gArgreg4[] = {"edi", "esi", "edx", "ecx", "r8d", "r9d"};
static const char *gArgreg8[] = {"rdi", "rsi", "rdx", "rcx", "r8",  "r9"};

typedef struct {
  uint8_t *data;
  size_t size;
} Section;

typedef struct {
  FILE *outfp;
  int labelseq;
  int brkseq;
  int contseq;
  const char *funcname;
  bool elf;
} GenContext;

static GenContext *gCtx;

static Section *new_section() {
  Section *s = calloc(1, sizeof(Section));
  return s;
}

static size_t section_write(Section *s, const void *chunk, size_t chunk_size) {
  // TODO(Kagami): use capacity
  s->data = realloc(s->data, s->size + chunk_size);
  memcpy(s->data + s->size, chunk, chunk_size);
  const size_t old_size = s->size;
  s->size += chunk_size;
  return old_size;
}

static size_t section_writestr(Section *s, const char *str) {
  return section_write(s, str, strlen(str) + 1);
}

static void emit(const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vfprintf(gCtx->outfp, fmt, ap);
  va_end(ap);
}

static void emit_global(const char *name) {
  if (gCtx->elf) {
    emit("global %s\n", name);
  } else {
    emit(".global %s\n", name);
  }
}

static void gen_node(Node *node);

// Pushes the given node's address to the stack.
static void gen_addr(Node *node) {
  switch (node->kind) {
  case ND_VAR: {
    if (node->init)
      gen_node(node->init);

    Var *var = node->var;
    if (var->is_local) {
      emit("  lea rax, [rbp-%d]\n", var->offset);
      emit("  push rax\n");
    } else {
      emit("  push offset %s\n", var->name);
    }
    return;
  }
  case ND_DEREF:
    gen_node(node->lhs);
    return;
  case ND_MEMBER:
    gen_addr(node->lhs);
    emit("  pop rax\n");
    emit("  add rax, %d\n", node->member->offset);
    emit("  push rax\n");
    return;
  default:
    error_tok(node->tok, "not an lvalue");
  }
}

static void gen_lval(Node *node) {
  if (node->ty->kind == TY_ARRAY)
    error_tok(node->tok, "not an lvalue");
  gen_addr(node);
}

static void load(Type *ty) {
  emit("  pop rax\n");

  if (ty->size == 1) {
    emit("  movsx rax, byte ptr [rax]\n");
  } else if (ty->size == 2) {
    emit("  movsx rax, word ptr [rax]\n");
  } else if (ty->size == 4) {
    emit("  movsxd rax, dword ptr [rax]\n");
  } else {
    assert(ty->size == 8);
    emit("  mov rax, [rax]\n");
  }

  emit("  push rax\n");
}

static void store(Type *ty) {
  emit("  pop rdi\n");
  emit("  pop rax\n");

  if (ty->kind == TY_BOOL) {
    emit("  cmp rdi, 0\n");
    emit("  setne dil\n");
    emit("  movzb rdi, dil\n");
  }

  if (ty->size == 1) {
    emit("  mov [rax], dil\n");
  } else if (ty->size == 2) {
    emit("  mov [rax], di\n");
  } else if (ty->size == 4) {
    emit("  mov [rax], edi\n");
  } else {
    assert(ty->size == 8);
    emit("  mov [rax], rdi\n");
  }

  emit("  push rdi\n");
}

static void cast_truncate(Type *ty) {
  emit("  pop rax\n");

  if (ty->kind == TY_BOOL) {
    emit("  cmp rax, 0\n");
    emit("  setne al\n");
  }

  if (ty->size == 1) {
    emit("  movsx rax, al\n");
  } else if (ty->size == 2) {
    emit("  movsx rax, ax\n");
  } else if (ty->size == 4) {
    emit("  movsxd rax, eax\n");
  }
  emit("  push rax\n");
}

static void inc(Type *ty) {
  emit("  pop rax\n");
  emit("  add rax, %d\n", ty->base ? ty->base->size : 1);
  emit("  push rax\n");
}

static void dec(Type *ty) {
  emit("  pop rax\n");
  emit("  sub rax, %d\n", ty->base ? ty->base->size : 1);
  emit("  push rax\n");
}

static void gen_binary(Node *node) {
  emit("  pop rdi\n");
  emit("  pop rax\n");

  switch (node->kind) {
  case ND_ADD:
  case ND_ADD_EQ:
    emit("  add rax, rdi\n");
    break;
  case ND_PTR_ADD:
  case ND_PTR_ADD_EQ:
    emit("  imul rdi, %d\n", node->ty->base->size);
    emit("  add rax, rdi\n");
    break;
  case ND_SUB:
  case ND_SUB_EQ:
    emit("  sub rax, rdi\n");
    break;
  case ND_PTR_SUB:
  case ND_PTR_SUB_EQ:
    emit("  imul rdi, %d\n", node->ty->base->size);
    emit("  sub rax, rdi\n");
    break;
  case ND_PTR_DIFF:
    emit("  sub rax, rdi\n");
    emit("  cqo\n");
    emit("  mov rdi, %d\n", node->lhs->ty->base->size);
    emit("  idiv rdi\n");
    break;
  case ND_MUL:
  case ND_MUL_EQ:
    emit("  imul rax, rdi\n");
    break;
  case ND_DIV:
  case ND_DIV_EQ:
    emit("  cqo\n");
    emit("  idiv rdi\n");
    break;
  case ND_BITAND:
  case ND_BITAND_EQ:
    emit("  and rax, rdi\n");
    break;
  case ND_BITOR:
  case ND_BITOR_EQ:
    emit("  or rax, rdi\n");
    break;
  case ND_BITXOR:
  case ND_BITXOR_EQ:
    emit("  xor rax, rdi\n");
    break;
  case ND_SHL:
  case ND_SHL_EQ:
    emit("  mov cl, dil\n");
    emit("  shl rax, cl\n");
    break;
  case ND_SHR:
  case ND_SHR_EQ:
    emit("  mov cl, dil\n");
    emit("  sar rax, cl\n");
    break;
  case ND_EQ:
    emit("  cmp rax, rdi\n");
    emit("  sete al\n");
    emit("  movzb rax, al\n");
    break;
  case ND_NE:
    emit("  cmp rax, rdi\n");
    emit("  setne al\n");
    emit("  movzb rax, al\n");
    break;
  case ND_LT:
    emit("  cmp rax, rdi\n");
    emit("  setl al\n");
    emit("  movzb rax, al\n");
    break;
  case ND_LE:
    emit("  cmp rax, rdi\n");
    emit("  setle al\n");
    emit("  movzb rax, al\n");
    break;
  default:
    break;
  }

  emit("  push rax\n");
}

// Generate code for a given node.
static void gen_node(Node *node) {
  switch (node->kind) {
  case ND_NULL:
    return;
  case ND_NUM:
    if (node->val == (int)node->val) {
      emit("  push %ld\n", node->val);
    } else {
      emit("  movabs rax, %ld\n", node->val);
      emit("  push rax\n");
    }
    return;
  case ND_EXPR_STMT:
    gen_node(node->lhs);
    emit("  add rsp, 8\n");
    return;
  case ND_VAR:
    if (node->init)
      gen_node(node->init);
    gen_addr(node);
    if (node->ty->kind != TY_ARRAY)
      load(node->ty);
    return;
  case ND_MEMBER:
    gen_addr(node);
    if (node->ty->kind != TY_ARRAY)
      load(node->ty);
    return;
  case ND_ASSIGN:
    gen_lval(node->lhs);
    gen_node(node->rhs);
    store(node->ty);
    return;
  case ND_TERNARY: {
    int seq = gCtx->labelseq++;
    gen_node(node->cond);
    emit("  pop rax\n");
    emit("  cmp rax, 0\n");
    emit("  je  .L.else.%d\n", seq);
    gen_node(node->then);
    emit("  jmp .L.end.%d\n", seq);
    emit(".L.else.%d:\n", seq);
    gen_node(node->els);
    emit(".L.end.%d:\n", seq);
    return;
  }
  case ND_PRE_INC:
    gen_lval(node->lhs);
    emit("  push [rsp]\n");
    load(node->ty);
    inc(node->ty);
    store(node->ty);
    return;
  case ND_PRE_DEC:
    gen_lval(node->lhs);
    emit("  push [rsp]\n");
    load(node->ty);
    dec(node->ty);
    store(node->ty);
    return;
  case ND_POST_INC:
    gen_lval(node->lhs);
    emit("  push [rsp]\n");
    load(node->ty);
    inc(node->ty);
    store(node->ty);
    dec(node->ty);
    return;
  case ND_POST_DEC:
    gen_lval(node->lhs);
    emit("  push [rsp]\n");
    load(node->ty);
    dec(node->ty);
    store(node->ty);
    inc(node->ty);
    return;
  case ND_ADD_EQ:
  case ND_PTR_ADD_EQ:
  case ND_SUB_EQ:
  case ND_PTR_SUB_EQ:
  case ND_MUL_EQ:
  case ND_DIV_EQ:
  case ND_SHL_EQ:
  case ND_SHR_EQ:
  case ND_BITAND_EQ:
  case ND_BITOR_EQ:
  case ND_BITXOR_EQ:
    gen_lval(node->lhs);
    emit("  push [rsp]\n");
    load(node->lhs->ty);
    gen_node(node->rhs);
    gen_binary(node);
    store(node->ty);
    return;
  case ND_COMMA:
    gen_node(node->lhs);
    gen_node(node->rhs);
    return;
  case ND_ADDR:
    gen_addr(node->lhs);
    return;
  case ND_DEREF:
    gen_node(node->lhs);
    if (node->ty->kind != TY_ARRAY)
      load(node->ty);
    return;
  case ND_NOT:
    gen_node(node->lhs);
    emit("  pop rax\n");
    emit("  cmp rax, 0\n");
    emit("  sete al\n");
    emit("  movzb rax, al\n");
    emit("  push rax\n");
    return;
  case ND_BITNOT:
    gen_node(node->lhs);
    emit("  pop rax\n");
    emit("  not rax\n");
    emit("  push rax\n");
    return;
  case ND_LOGAND: {
    int seq = gCtx->labelseq++;
    gen_node(node->lhs);
    emit("  pop rax\n");
    emit("  cmp rax, 0\n");
    emit("  je  .L.false.%d\n", seq);
    gen_node(node->rhs);
    emit("  pop rax\n");
    emit("  cmp rax, 0\n");
    emit("  je  .L.false.%d\n", seq);
    emit("  push 1\n");
    emit("  jmp .L.end.%d\n", seq);
    emit(".L.false.%d:\n", seq);
    emit("  push 0\n");
    emit(".L.end.%d:\n", seq);
    return;
  }
  case ND_LOGOR: {
    int seq = gCtx->labelseq++;
    gen_node(node->lhs);
    emit("  pop rax\n");
    emit("  cmp rax, 0\n");
    emit("  jne .L.true.%d\n", seq);
    gen_node(node->rhs);
    emit("  pop rax\n");
    emit("  cmp rax, 0\n");
    emit("  jne .L.true.%d\n", seq);
    emit("  push 0\n");
    emit("  jmp .L.end.%d\n", seq);
    emit(".L.true.%d:\n", seq);
    emit("  push 1\n");
    emit(".L.end.%d:\n", seq);
    return;
  }
  case ND_IF: {
    int seq = gCtx->labelseq++;
    if (node->els) {
      gen_node(node->cond);
      emit("  pop rax\n");
      emit("  cmp rax, 0\n");
      emit("  je  .L.else.%d\n", seq);
      gen_node(node->then);
      emit("  jmp .L.end.%d\n", seq);
      emit(".L.else.%d:\n", seq);
      gen_node(node->els);
      emit(".L.end.%d:\n", seq);
    } else {
      gen_node(node->cond);
      emit("  pop rax\n");
      emit("  cmp rax, 0\n");
      emit("  je  .L.end.%d\n", seq);
      gen_node(node->then);
      emit(".L.end.%d:\n", seq);
    }
    return;
  }
  case ND_WHILE: {
    int seq = gCtx->labelseq++;
    int brk = gCtx->brkseq;
    int cont = gCtx->contseq;
    gCtx->brkseq = gCtx->contseq = seq;

    emit(".L.continue.%d:\n", seq);
    gen_node(node->cond);
    emit("  pop rax\n");
    emit("  cmp rax, 0\n");
    emit("  je  .L.break.%d\n", seq);
    gen_node(node->then);
    emit("  jmp .L.continue.%d\n", seq);
    emit(".L.break.%d:\n", seq);

    gCtx->brkseq = brk;
    gCtx->contseq = cont;
    return;
  }
  case ND_FOR: {
    int seq = gCtx->labelseq++;
    int brk = gCtx->brkseq;
    int cont = gCtx->contseq;
    gCtx->brkseq = gCtx->contseq = seq;

    if (node->init)
      gen_node(node->init);
    emit(".L.begin.%d:\n", seq);
    if (node->cond) {
      gen_node(node->cond);
      emit("  pop rax\n");
      emit("  cmp rax, 0\n");
      emit("  je  .L.break.%d\n", seq);
    }
    gen_node(node->then);
    emit(".L.continue.%d:\n", seq);
    if (node->inc)
      gen_node(node->inc);
    emit("  jmp .L.begin.%d\n", seq);
    emit(".L.break.%d:\n", seq);

    gCtx->brkseq = brk;
    gCtx->contseq = cont;
    return;
  }
  case ND_DO: {
    int seq = gCtx->labelseq++;
    int brk = gCtx->brkseq;
    int cont = gCtx->contseq;
    gCtx->brkseq = gCtx->contseq = seq;

    emit(".L.begin.%d:\n", seq);
    gen_node(node->then);
    emit(".L.continue.%d:\n", seq);
    gen_node(node->cond);
    emit("  pop rax\n");
    emit("  cmp rax, 0\n");
    emit("  jne .L.begin.%d\n", seq);
    emit(".L.break.%d:\n", seq);

    gCtx->brkseq = brk;
    gCtx->contseq = cont;
    return;
  }
  case ND_SWITCH: {
    int seq = gCtx->labelseq++;
    int brk = gCtx->brkseq;
    gCtx->brkseq = seq;
    node->case_label = seq;

    gen_node(node->cond);
    emit("  pop rax\n");

    for (Node *n = node->case_next; n; n = n->case_next) {
      n->case_label = gCtx->labelseq++;
      n->case_end_label = seq;
      emit("  cmp rax, %ld\n", n->val);
      emit("  je .L.case.%d\n", n->case_label);
    }

    if (node->default_case) {
      int i = gCtx->labelseq++;
      node->default_case->case_end_label = seq;
      node->default_case->case_label = i;
      emit("  jmp .L.case.%d\n", i);
    }

    emit("  jmp .L.break.%d\n", seq);
    gen_node(node->then);
    emit(".L.break.%d:\n", seq);

    gCtx->brkseq = brk;
    return;
  }
  case ND_CASE:
    emit(".L.case.%d:\n", node->case_label);
    gen_node(node->lhs);
    return;
  case ND_BLOCK:
  case ND_STMT_EXPR:
    for (Node *n = node->body; n; n = n->next)
      gen_node(n);
    return;
  case ND_BREAK:
    if (gCtx->brkseq == 0)
      error_tok(node->tok, "stray break");
    emit("  jmp .L.break.%d\n", gCtx->brkseq);
    return;
  case ND_CONTINUE:
    if (gCtx->contseq == 0)
      error_tok(node->tok, "stray continue");
    emit("  jmp .L.continue.%d\n", gCtx->contseq);
    return;
  case ND_GOTO:
    emit("  jmp .L.label.%s.%s\n", gCtx->funcname, node->label_name);
    return;
  case ND_LABEL:
    emit(".L.label.%s.%s:\n", gCtx->funcname, node->label_name);
    gen_node(node->lhs);
    return;
  case ND_FUNCALL: {
    // Gen function arguments
    int nargs = 0;
    for (Node *arg = node->args; arg; arg = arg->next) {
      gen_node(arg);
      nargs++;
    }
    // Assign arguments to corresponding register
    for (int i = nargs - 1; i >= 0; i--) {
      emit("  pop %s\n", gArgreg8[i]);
    }

    if (strcmp(node->funcname, "__builtin_va_start") == 0) {
      // Get gp_offset
      emit("  mov edx, dword ptr [rbp-8]\n");
      // Get reg_save_area
      emit("  lea rcx, [rbp-56]\n");
      // Fill va_list
      emit("  mov dword ptr [rdi], edx\n"); // gp_offset
      emit("  mov dword ptr [rdi+4], 0\n"); // fp_offset
      emit("  mov qword ptr [rdi+8], 0\n"); // overflow_arg_area
      emit("  mov qword ptr [rdi+16], rcx\n"); // reg_save_area
      // Adjust for ND_EXPR_STMT
      emit("  sub rsp, 8\n");
      return;
    }

    // We need to align RSP to a 16 byte boundary before
    // calling a function because it is an ABI requirement.
    // RAX is set to 0 for variadic function.
    int seq = gCtx->labelseq++;
    emit("  mov rax, rsp\n");
    emit("  and rax, 15\n");
    emit("  jnz .L.call.%d\n", seq);
    emit("  mov rax, 0\n");
    emit("  call %s\n", node->funcname);
    emit("  jmp .L.end.%d\n", seq);
    emit(".L.call.%d:\n", seq);
    emit("  sub rsp, 8\n");
    emit("  mov rax, 0\n");
    emit("  call %s\n", node->funcname);
    emit("  add rsp, 8\n");
    emit(".L.end.%d:\n", seq);
    if (node->ty->kind == TY_BOOL)
      emit("  movzb rax, al\n");
    emit("  push rax\n");
    return;
  }
  case ND_RETURN:
    if (node->lhs) {
      gen_node(node->lhs);
      emit("  pop rax\n");
    }
    emit("  jmp .L.return.%s\n", gCtx->funcname);
    return;
  case ND_CAST:
    gen_node(node->lhs);
    cast_truncate(node->ty);
    return;
  default:
    break;
  }

  gen_node(node->lhs);
  gen_node(node->rhs);
  gen_binary(node);
}

static void gen_data(Program *prog) {
  for (VarList *vl = prog->globals; vl; vl = vl->next)
    if (!vl->var->is_static) {
      emit_global(vl->var->name);
    }

  if (!gCtx->elf) {
    emit(".bss\n");
  }

  for (VarList *vl = prog->globals; vl; vl = vl->next) {
    Var *var = vl->var;
    if (var->initializer)
      continue;

    emit(".align %d\n", var->ty->align);
    emit("%s:\n", var->name);
    emit("  .zero %d\n", var->ty->size);
  }

  if (!gCtx->elf) {
    emit(".data\n");
  }

  for (VarList *vl = prog->globals; vl; vl = vl->next) {
    Var *var = vl->var;
    if (!var->initializer)
      continue;

    emit(".align %d\n", var->ty->align);
    emit("%s:\n", var->name);

    for (Initializer *init = var->initializer; init; init = init->next) {
      if (init->label)
        emit("  .quad %s%+ld\n", init->label, init->addend);
      else if (init->sz == 1)
        emit("  .byte %ld\n", init->val);
      else
        emit("  .%dbyte %ld\n", init->sz, init->val);
    }
  }
}

static void load_arg(Var *var, int idx) {
  int sz = var->ty->size;
  if (sz == 1) {
    emit("  mov [rbp-%d], %s\n", var->offset, gArgreg1[idx]);
  } else if (sz == 2) {
    emit("  mov [rbp-%d], %s\n", var->offset, gArgreg2[idx]);
  } else if (sz == 4) {
    emit("  mov [rbp-%d], %s\n", var->offset, gArgreg4[idx]);
  } else {
    assert(sz == 8);
    emit("  mov [rbp-%d], %s\n", var->offset, gArgreg8[idx]);
  }
}

static void gen_text(Program *prog) {
  if (!gCtx->elf) {
    emit(".text\n");
  }

  for (Function *fn = prog->fns; fn; fn = fn->next) {
    if (!fn->is_static) {
      emit_global(fn->name);
    }
    emit("%s:\n", fn->name);
    gCtx->funcname = fn->name;

    // Prologue
    emit("  push rbp\n");
    emit("  mov rbp, rsp\n");
    emit("  sub rsp, %d\n", fn->stack_size);

    // Save arg registers if function is variadic
    if (fn->has_varargs) {
      // Num of regular parameters to calculate gp_offset
      int n = 0;
      for (VarList *vl = fn->params; vl; vl = vl->next) {
        n++;
      }
      // Register save area
      emit("  mov [rbp-56], rdi\n");
      emit("  mov [rbp-48], rsi\n");
      emit("  mov [rbp-40], rdx\n");
      emit("  mov [rbp-32], rcx\n");
      emit("  mov [rbp-24], r8\n");
      emit("  mov [rbp-16], r9\n");
      // gp_offset
      emit("  mov dword ptr [rbp-8], %d\n", n * 8);
    }

    // Push arguments to the stack
    int i = 0;
    for (VarList *vl = fn->params; vl; vl = vl->next)
      load_arg(vl->var, i++);

    // Emit code
    for (Node *node = fn->node; node; node = node->next)
      gen_node(node);

    // Epilogue
    emit(".L.return.%s:\n", gCtx->funcname);
    emit("  mov rsp, rbp\n");
    emit("  pop rbp\n");
    emit("  ret\n");
  }
}

// Assign offsets to local variables.
void gen_offsets(Program *prog) {
  for (Function *fn = prog->fns; fn; fn = fn->next) {
    int offset = fn->has_varargs ? 56 : 0;
    for (VarList *vl = fn->locals; vl; vl = vl->next) {
      Var *var = vl->var;
      offset = align_to(offset, var->ty->align);
      offset += var->ty->size;
      var->offset = offset;
    }
    fn->stack_size = align_to(offset, 8);
  }
}

// Use nasm for partial codegen for now.
static Section *nasm(const char *src) {
  const char *tmp = "/tmp/dfcc.tmp";
  pid_t pid = fork();
  assert(pid >= 0);
  if (pid == 0) {
    execlp("nasm", "nasm", "-o", tmp, src, (char*)NULL);
    assert(false);
  }
  int status;
  waitpid(pid, &status, 0);
  assert(status >= 0);

  Section *text = new_section();
  FILE *fp = fopen(tmp, "rb");
  assert(fp);
  fseek(fp, 0, SEEK_END);
  text->size = ftell(fp);
  rewind(fp);

  text->data = malloc(text->size);
  fread(text->data, 1, text->size, fp);
  fclose(fp);
  assert(unlink(tmp) == 0);
  return text;
}

static void write_elf(const char *dst, const char *src) {
  Section *text = nasm(src);
  FILE *fp = fopen(dst, "wb");
  if (!fp) error("cannot open %s (%s)", dst, strerror(errno));

  // Main header
  Elf64_Ehdr elf_header = {
    {
      ELFMAG0, ELFMAG1, ELFMAG2, ELFMAG3,
      ELFCLASS64,
      ELFDATA2LSB,
      EV_CURRENT,
      ELFOSABI_SYSV,
      0, // ABI version
      0, // padding
      EI_NIDENT,
    },
    ET_REL,
    EM_X86_64,
    EV_CURRENT,
    0, // entry point address
    0, // program header offset
    sizeof(Elf64_Ehdr), // section header offset
    0, // flags
    sizeof(Elf64_Ehdr), // ELF header size
    0, // program header size
    0, // program headers number
    sizeof(Elf64_Shdr), // section header size
    0, // section headers number
    0, // shstrtab index
  };

  // Empty section
  Elf64_Shdr null_header = { 0 };
  Section *shstrtab = new_section();
  section_writestr(shstrtab, "");

  // Section names section
  Elf64_Shdr shstrtab_header = { 0 };
  shstrtab_header.sh_name = section_writestr(shstrtab, ".shstrtab");
  shstrtab_header.sh_type = SHT_STRTAB;
  shstrtab_header.sh_addralign = 1;

  // Symbol names section
  Elf64_Shdr strtab_header = { 0 };
  strtab_header.sh_name = section_writestr(shstrtab, ".strtab");
  strtab_header.sh_type = SHT_STRTAB;
  strtab_header.sh_addralign = 1;

  // Symbol section
  Elf64_Shdr symtab_header = { 0 };
  symtab_header.sh_name = section_writestr(shstrtab, ".symtab");
  symtab_header.sh_type = SHT_SYMTAB;
  symtab_header.sh_link = 2;
  symtab_header.sh_info = 1;
  symtab_header.sh_addralign = 8;
  symtab_header.sh_entsize = sizeof(Elf64_Sym);

  // Text section
  Elf64_Shdr text_header = { 0 };
  text_header.sh_name = section_writestr(shstrtab, ".text");
  text_header.sh_type = SHT_PROGBITS;
  text_header.sh_flags = SHF_EXECINSTR | SHF_ALLOC;
  text_header.sh_addralign = 16;

  // Write symbols
  Section *strtab = new_section();
  Section *symtab = new_section();
  Elf64_Sym entry = { 0 };
  section_writestr(strtab, "");
  section_write(symtab, &entry, sizeof(Elf64_Sym)); // emptry entry
  entry.st_name = section_writestr(strtab, "main");
  entry.st_info = (STB_GLOBAL << 4) | STT_FUNC;
  entry.st_shndx = 4;
  section_write(symtab, &entry, sizeof(Elf64_Sym));

  // Fix offsets
  elf_header.e_shnum = 5;
  elf_header.e_shstrndx = 1;
  shstrtab_header.sh_offset = sizeof(Elf64_Ehdr) + elf_header.e_shnum * sizeof(Elf64_Shdr);
  shstrtab_header.sh_size = shstrtab->size;
  strtab_header.sh_offset = shstrtab_header.sh_offset + shstrtab_header.sh_size;
  strtab_header.sh_size = strtab->size;
  symtab_header.sh_offset = strtab_header.sh_offset + strtab_header.sh_size;
  symtab_header.sh_size = symtab->size;
  text_header.sh_offset = symtab_header.sh_offset + symtab_header.sh_size;
  text_header.sh_size = text->size;

  // Dump all
  fwrite(&elf_header, 1, sizeof(elf_header), fp);
  fwrite(&null_header, 1, sizeof(null_header), fp);
  fwrite(&shstrtab_header, 1, sizeof(shstrtab_header), fp);
  fwrite(&strtab_header, 1, sizeof(strtab_header), fp);
  fwrite(&symtab_header, 1, sizeof(symtab_header), fp);
  fwrite(&text_header, 1, sizeof(text_header), fp);
  fwrite(shstrtab->data, 1, shstrtab->size, fp);
  fwrite(strtab->data, 1, strtab->size, fp);
  fwrite(symtab->data, 1, symtab->size, fp);
  fwrite(text->data, 1, text->size, fp);
  fclose(fp);
}

// Generate code for the entire program.
void gen_prog(Program *prog, const char *path) {
  gCtx = calloc(1, sizeof(GenContext));
  gCtx->labelseq = 1;
  gCtx->outfp = path ? fopen(path, "wb") : stdout;
  if (!gCtx->outfp) error("cannot open %s (%s)", path, strerror(errno));
  if (path && strcmp(path, ".tmp2/test/elf.c.s") == 0) {
    gCtx->elf = true;
  }

  emit(gCtx->elf ? "bits 64\n" : ".intel_syntax noprefix\n");
  gen_data(prog);
  gen_text(prog);
  if (path) {
    fclose(gCtx->outfp);
  }
  if (gCtx->elf) {
    write_elf(".tmp2/test/elf.c.o", path);
  }
}
