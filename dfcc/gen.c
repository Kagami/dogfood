#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <stdio.h>
#include <errno.h>
#include "dfcc.h"

static const char *gArgreg1[] = {"dil", "sil",  "dl",  "cl", "r8b", "r9b"};
static const char *gArgreg2[] = { "di",  "si",  "dx",  "cx", "r8w", "r9w"};
static const char *gArgreg4[] = {"edi", "esi", "edx", "ecx", "r8d", "r9d"};
static const char *gArgreg8[] = {"rdi", "rsi", "rdx", "rcx", "r8",  "r9"};

typedef struct {
  FILE *outfp;
  int labelseq;
  int brkseq;
  int contseq;
  const char *funcname;
} GenContext;

static GenContext *gCtx;

static void emit(const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vfprintf(gCtx->outfp, fmt, ap);
  va_end(ap);
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

static void truncate(Type *ty) {
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
    truncate(node->ty);
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
    if (!vl->var->is_static)
      emit(".global %s\n", vl->var->name);

  emit(".bss\n");

  for (VarList *vl = prog->globals; vl; vl = vl->next) {
    Var *var = vl->var;
    if (var->initializer)
      continue;

    emit(".align %d\n", var->ty->align);
    emit("%s:\n", var->name);
    emit("  .zero %d\n", var->ty->size);
  }

  emit(".data\n");

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
  emit(".text\n");

  for (Function *fn = prog->fns; fn; fn = fn->next) {
    if (!fn->is_static)
      emit(".global %s\n", fn->name);
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

// Generate code for the entire program.
void gen_prog(Program *prog, const char *outpath, bool is_stdout) {
  FILE *outfp = is_stdout ? stdout : fopen(outpath, "wb");
  if (!outfp) error("cannot open %s: %s", outpath, strerror(errno));

  gCtx = calloc(1, sizeof(GenContext));
  gCtx->outfp = outfp;
  gCtx->labelseq = 1;

  emit(".intel_syntax noprefix\n");
  gen_data(prog);
  gen_text(prog);
}
