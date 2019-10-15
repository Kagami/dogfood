#include "dfcc.h"

static char *argreg1[] = {"dil", "sil", "dl", "cl", "r8b", "r9b"};
static char *argreg2[] = {"di", "si", "dx", "cx", "r8w", "r9w"};
static char *argreg4[] = {"edi", "esi", "edx", "ecx", "r8d", "r9d"};
static char *argreg8[] = {"rdi", "rsi", "rdx", "rcx", "r8", "r9"};

static int labelseq = 1;
static int brkseq;
static int contseq;
static char *funcname;

static void gen(Node *node);

// Pushes the given node's address to the stack.
static void gen_addr(Node *node) {
  switch (node->kind) {
  case ND_VAR: {
    if (node->init)
      gen(node->init);

    Var *var = node->var;
    if (var->is_local) {
      printf("  lea rax, [rbp-%d]\n", var->offset);
      printf("  push rax\n");
    } else {
      printf("  push offset %s\n", var->name);
    }
    return;
  }
  case ND_DEREF:
    gen(node->lhs);
    return;
  case ND_MEMBER:
    gen_addr(node->lhs);
    printf("  pop rax\n");
    printf("  add rax, %d\n", node->member->offset);
    printf("  push rax\n");
    return;
  default:
    ;/* skip */
  }

  error_tok(node->tok, "not an lvalue");
}

static void gen_lval(Node *node) {
  if (node->ty->kind == TY_ARRAY)
    error_tok(node->tok, "not an lvalue");
  gen_addr(node);
}

static void load(Type *ty) {
  printf("  pop rax\n");

  if (ty->size == 1) {
    printf("  movsx rax, byte ptr [rax]\n");
  } else if (ty->size == 2) {
    printf("  movsx rax, word ptr [rax]\n");
  } else if (ty->size == 4) {
    printf("  movsxd rax, dword ptr [rax]\n");
  } else {
    assert(ty->size == 8);
    printf("  mov rax, [rax]\n");
  }

  printf("  push rax\n");
}

static void store(Type *ty) {
  printf("  pop rdi\n");
  printf("  pop rax\n");

  if (ty->kind == TY_BOOL) {
    printf("  cmp rdi, 0\n");
    printf("  setne dil\n");
    printf("  movzb rdi, dil\n");
  }

  if (ty->size == 1) {
    printf("  mov [rax], dil\n");
  } else if (ty->size == 2) {
    printf("  mov [rax], di\n");
  } else if (ty->size == 4) {
    printf("  mov [rax], edi\n");
  } else {
    assert(ty->size == 8);
    printf("  mov [rax], rdi\n");
  }

  printf("  push rdi\n");
}

static void truncate(Type *ty) {
  printf("  pop rax\n");

  if (ty->kind == TY_BOOL) {
    printf("  cmp rax, 0\n");
    printf("  setne al\n");
  }

  if (ty->size == 1) {
    printf("  movsx rax, al\n");
  } else if (ty->size == 2) {
    printf("  movsx rax, ax\n");
  } else if (ty->size == 4) {
    printf("  movsxd rax, eax\n");
  }
  printf("  push rax\n");
}

static void inc(Type *ty) {
  printf("  pop rax\n");
  printf("  add rax, %d\n", ty->base ? ty->base->size : 1);
  printf("  push rax\n");
}

static void dec(Type *ty) {
  printf("  pop rax\n");
  printf("  sub rax, %d\n", ty->base ? ty->base->size : 1);
  printf("  push rax\n");
}

static void gen_binary(Node *node) {
  printf("  pop rdi\n");
  printf("  pop rax\n");

  switch (node->kind) {
  case ND_ADD:
  case ND_ADD_EQ:
    printf("  add rax, rdi\n");
    break;
  case ND_PTR_ADD:
  case ND_PTR_ADD_EQ:
    printf("  imul rdi, %d\n", node->ty->base->size);
    printf("  add rax, rdi\n");
    break;
  case ND_SUB:
  case ND_SUB_EQ:
    printf("  sub rax, rdi\n");
    break;
  case ND_PTR_SUB:
  case ND_PTR_SUB_EQ:
    printf("  imul rdi, %d\n", node->ty->base->size);
    printf("  sub rax, rdi\n");
    break;
  case ND_PTR_DIFF:
    printf("  sub rax, rdi\n");
    printf("  cqo\n");
    printf("  mov rdi, %d\n", node->lhs->ty->base->size);
    printf("  idiv rdi\n");
    break;
  case ND_MUL:
  case ND_MUL_EQ:
    printf("  imul rax, rdi\n");
    break;
  case ND_DIV:
  case ND_DIV_EQ:
    printf("  cqo\n");
    printf("  idiv rdi\n");
    break;
  case ND_BITAND:
  case ND_BITAND_EQ:
    printf("  and rax, rdi\n");
    break;
  case ND_BITOR:
  case ND_BITOR_EQ:
    printf("  or rax, rdi\n");
    break;
  case ND_BITXOR:
  case ND_BITXOR_EQ:
    printf("  xor rax, rdi\n");
    break;
  case ND_SHL:
  case ND_SHL_EQ:
    printf("  mov cl, dil\n");
    printf("  shl rax, cl\n");
    break;
  case ND_SHR:
  case ND_SHR_EQ:
    printf("  mov cl, dil\n");
    printf("  sar rax, cl\n");
    break;
  case ND_EQ:
    printf("  cmp rax, rdi\n");
    printf("  sete al\n");
    printf("  movzb rax, al\n");
    break;
  case ND_NE:
    printf("  cmp rax, rdi\n");
    printf("  setne al\n");
    printf("  movzb rax, al\n");
    break;
  case ND_LT:
    printf("  cmp rax, rdi\n");
    printf("  setl al\n");
    printf("  movzb rax, al\n");
    break;
  case ND_LE:
    printf("  cmp rax, rdi\n");
    printf("  setle al\n");
    printf("  movzb rax, al\n");
    break;
  default:
    ;/* skip */
  }

  printf("  push rax\n");
}

// Generate code for a given node.
static void gen(Node *node) {
  switch (node->kind) {
  case ND_NULL:
    return;
  case ND_NUM:
    if (node->val == (int)node->val) {
      printf("  push %ld\n", node->val);
    } else {
      printf("  movabs rax, %ld\n", node->val);
      printf("  push rax\n");
    }
    return;
  case ND_EXPR_STMT:
    gen(node->lhs);
    printf("  add rsp, 8\n");
    return;
  case ND_VAR:
    if (node->init)
      gen(node->init);
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
    gen(node->rhs);
    store(node->ty);
    return;
  case ND_TERNARY: {
    int seq = labelseq++;
    gen(node->cond);
    printf("  pop rax\n");
    printf("  cmp rax, 0\n");
    printf("  je  .L.else.%d\n", seq);
    gen(node->then);
    printf("  jmp .L.end.%d\n", seq);
    printf(".L.else.%d:\n", seq);
    gen(node->els);
    printf(".L.end.%d:\n", seq);
    return;
  }
  case ND_PRE_INC:
    gen_lval(node->lhs);
    printf("  push [rsp]\n");
    load(node->ty);
    inc(node->ty);
    store(node->ty);
    return;
  case ND_PRE_DEC:
    gen_lval(node->lhs);
    printf("  push [rsp]\n");
    load(node->ty);
    dec(node->ty);
    store(node->ty);
    return;
  case ND_POST_INC:
    gen_lval(node->lhs);
    printf("  push [rsp]\n");
    load(node->ty);
    inc(node->ty);
    store(node->ty);
    dec(node->ty);
    return;
  case ND_POST_DEC:
    gen_lval(node->lhs);
    printf("  push [rsp]\n");
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
    printf("  push [rsp]\n");
    load(node->lhs->ty);
    gen(node->rhs);
    gen_binary(node);
    store(node->ty);
    return;
  case ND_COMMA:
    gen(node->lhs);
    gen(node->rhs);
    return;
  case ND_ADDR:
    gen_addr(node->lhs);
    return;
  case ND_DEREF:
    gen(node->lhs);
    if (node->ty->kind != TY_ARRAY)
      load(node->ty);
    return;
  case ND_NOT:
    gen(node->lhs);
    printf("  pop rax\n");
    printf("  cmp rax, 0\n");
    printf("  sete al\n");
    printf("  movzb rax, al\n");
    printf("  push rax\n");
    return;
  case ND_BITNOT:
    gen(node->lhs);
    printf("  pop rax\n");
    printf("  not rax\n");
    printf("  push rax\n");
    return;
  case ND_LOGAND: {
    int seq = labelseq++;
    gen(node->lhs);
    printf("  pop rax\n");
    printf("  cmp rax, 0\n");
    printf("  je  .L.false.%d\n", seq);
    gen(node->rhs);
    printf("  pop rax\n");
    printf("  cmp rax, 0\n");
    printf("  je  .L.false.%d\n", seq);
    printf("  push 1\n");
    printf("  jmp .L.end.%d\n", seq);
    printf(".L.false.%d:\n", seq);
    printf("  push 0\n");
    printf(".L.end.%d:\n", seq);
    return;
  }
  case ND_LOGOR: {
    int seq = labelseq++;
    gen(node->lhs);
    printf("  pop rax\n");
    printf("  cmp rax, 0\n");
    printf("  jne .L.true.%d\n", seq);
    gen(node->rhs);
    printf("  pop rax\n");
    printf("  cmp rax, 0\n");
    printf("  jne .L.true.%d\n", seq);
    printf("  push 0\n");
    printf("  jmp .L.end.%d\n", seq);
    printf(".L.true.%d:\n", seq);
    printf("  push 1\n");
    printf(".L.end.%d:\n", seq);
    return;
  }
  case ND_IF: {
    int seq = labelseq++;
    if (node->els) {
      gen(node->cond);
      printf("  pop rax\n");
      printf("  cmp rax, 0\n");
      printf("  je  .L.else.%d\n", seq);
      gen(node->then);
      printf("  jmp .L.end.%d\n", seq);
      printf(".L.else.%d:\n", seq);
      gen(node->els);
      printf(".L.end.%d:\n", seq);
    } else {
      gen(node->cond);
      printf("  pop rax\n");
      printf("  cmp rax, 0\n");
      printf("  je  .L.end.%d\n", seq);
      gen(node->then);
      printf(".L.end.%d:\n", seq);
    }
    return;
  }
  case ND_WHILE: {
    int seq = labelseq++;
    int brk = brkseq;
    int cont = contseq;
    brkseq = contseq = seq;

    printf(".L.continue.%d:\n", seq);
    gen(node->cond);
    printf("  pop rax\n");
    printf("  cmp rax, 0\n");
    printf("  je  .L.break.%d\n", seq);
    gen(node->then);
    printf("  jmp .L.continue.%d\n", seq);
    printf(".L.break.%d:\n", seq);

    brkseq = brk;
    contseq = cont;
    return;
  }
  case ND_FOR: {
    int seq = labelseq++;
    int brk = brkseq;
    int cont = contseq;
    brkseq = contseq = seq;

    if (node->init)
      gen(node->init);
    printf(".L.begin.%d:\n", seq);
    if (node->cond) {
      gen(node->cond);
      printf("  pop rax\n");
      printf("  cmp rax, 0\n");
      printf("  je  .L.break.%d\n", seq);
    }
    gen(node->then);
    printf(".L.continue.%d:\n", seq);
    if (node->inc)
      gen(node->inc);
    printf("  jmp .L.begin.%d\n", seq);
    printf(".L.break.%d:\n", seq);

    brkseq = brk;
    contseq = cont;
    return;
  }
  case ND_DO: {
    int seq = labelseq++;
    int brk = brkseq;
    int cont = contseq;
    brkseq = contseq = seq;

    printf(".L.begin.%d:\n", seq);
    gen(node->then);
    printf(".L.continue.%d:\n", seq);
    gen(node->cond);
    printf("  pop rax\n");
    printf("  cmp rax, 0\n");
    printf("  jne .L.begin.%d\n", seq);
    printf(".L.break.%d:\n", seq);

    brkseq = brk;
    contseq = cont;
    return;
  }
  case ND_SWITCH: {
    int seq = labelseq++;
    int brk = brkseq;
    brkseq = seq;
    node->case_label = seq;

    gen(node->cond);
    printf("  pop rax\n");

    for (Node *n = node->case_next; n; n = n->case_next) {
      n->case_label = labelseq++;
      n->case_end_label = seq;
      printf("  cmp rax, %ld\n", n->val);
      printf("  je .L.case.%d\n", n->case_label);
    }

    if (node->default_case) {
      int i = labelseq++;
      node->default_case->case_end_label = seq;
      node->default_case->case_label = i;
      printf("  jmp .L.case.%d\n", i);
    }

    printf("  jmp .L.break.%d\n", seq);
    gen(node->then);
    printf(".L.break.%d:\n", seq);

    brkseq = brk;
    return;
  }
  case ND_CASE:
    printf(".L.case.%d:\n", node->case_label);
    gen(node->lhs);
    return;
  case ND_BLOCK:
  case ND_STMT_EXPR:
    for (Node *n = node->body; n; n = n->next)
      gen(n);
    return;
  case ND_BREAK:
    if (brkseq == 0)
      error_tok(node->tok, "stray break");
    printf("  jmp .L.break.%d\n", brkseq);
    return;
  case ND_CONTINUE:
    if (contseq == 0)
      error_tok(node->tok, "stray continue");
    printf("  jmp .L.continue.%d\n", contseq);
    return;
  case ND_GOTO:
    printf("  jmp .L.label.%s.%s\n", funcname, node->label_name);
    return;
  case ND_LABEL:
    printf(".L.label.%s.%s:\n", funcname, node->label_name);
    gen(node->lhs);
    return;
  case ND_FUNCALL: {
    // Gen function arguments
    int nargs = 0;
    for (Node *arg = node->args; arg; arg = arg->next) {
      gen(arg);
      nargs++;
    }
    // Assign arguments to corresponding register
    for (int i = nargs - 1; i >= 0; i--) {
      printf("  pop %s\n", argreg8[i]);
    }

    if (strcmp(node->funcname, "__builtin_va_start") == 0) {
      // Get gp_offset
      printf("  mov edx, dword ptr [rbp-8]\n");
      // Get reg_save_area
      printf("  lea rcx, [rbp-56]\n");
      // Fill va_list
      printf("  mov dword ptr [rdi], edx\n"); // gp_offset
      printf("  mov dword ptr [rdi+4], 0\n"); // fp_offset
      printf("  mov qword ptr [rdi+8], 0\n"); // overflow_arg_area
      printf("  mov qword ptr [rdi+16], rcx\n"); // reg_save_area
      // Adjust for ND_EXPR_STMT
      printf("  sub rsp, 8\n");
      return;
    }

    // We need to align RSP to a 16 byte boundary before
    // calling a function because it is an ABI requirement.
    // RAX is set to 0 for variadic function.
    int seq = labelseq++;
    printf("  mov rax, rsp\n");
    printf("  and rax, 15\n");
    printf("  jnz .L.call.%d\n", seq);
    printf("  mov rax, 0\n");
    printf("  call %s\n", node->funcname);
    printf("  jmp .L.end.%d\n", seq);
    printf(".L.call.%d:\n", seq);
    printf("  sub rsp, 8\n");
    printf("  mov rax, 0\n");
    printf("  call %s\n", node->funcname);
    printf("  add rsp, 8\n");
    printf(".L.end.%d:\n", seq);
    if (node->ty->kind == TY_BOOL)
      printf("  movzb rax, al\n");
    printf("  push rax\n");
    return;
  }
  case ND_RETURN:
    if (node->lhs) {
      gen(node->lhs);
      printf("  pop rax\n");
    }
    printf("  jmp .L.return.%s\n", funcname);
    return;
  case ND_CAST:
    gen(node->lhs);
    truncate(node->ty);
    return;
  default:
    ;/* skip */
  }

  gen(node->lhs);
  gen(node->rhs);
  gen_binary(node);
}

static void emit_data(Program *prog) {
  for (VarList *vl = prog->globals; vl; vl = vl->next)
    if (!vl->var->is_static)
      printf(".global %s\n", vl->var->name);

  printf(".bss\n");

  for (VarList *vl = prog->globals; vl; vl = vl->next) {
    Var *var = vl->var;
    if (var->initializer)
      continue;

    printf(".align %d\n", var->ty->align);
    printf("%s:\n", var->name);
    printf("  .zero %d\n", var->ty->size);
  }

  printf(".data\n");

  for (VarList *vl = prog->globals; vl; vl = vl->next) {
    Var *var = vl->var;
    if (!var->initializer)
      continue;

    printf(".align %d\n", var->ty->align);
    printf("%s:\n", var->name);

    for (Initializer *init = var->initializer; init; init = init->next) {
      if (init->label)
        printf("  .quad %s%+ld\n", init->label, init->addend);
      else if (init->sz == 1)
        printf("  .byte %ld\n", init->val);
      else
        printf("  .%dbyte %ld\n", init->sz, init->val);
    }
  }
}

static void load_arg(Var *var, int idx) {
  int sz = var->ty->size;
  if (sz == 1) {
    printf("  mov [rbp-%d], %s\n", var->offset, argreg1[idx]);
  } else if (sz == 2) {
    printf("  mov [rbp-%d], %s\n", var->offset, argreg2[idx]);
  } else if (sz == 4) {
    printf("  mov [rbp-%d], %s\n", var->offset, argreg4[idx]);
  } else {
    assert(sz == 8);
    printf("  mov [rbp-%d], %s\n", var->offset, argreg8[idx]);
  }
}

static void emit_text(Program *prog) {
  printf(".text\n");

  for (Function *fn = prog->fns; fn; fn = fn->next) {
    if (!fn->is_static)
      printf(".global %s\n", fn->name);
    printf("%s:\n", fn->name);
    funcname = fn->name;

    // Prologue
    printf("  push rbp\n");
    printf("  mov rbp, rsp\n");
    printf("  sub rsp, %d\n", fn->stack_size);

    // Save arg registers if function is variadic
    if (fn->has_varargs) {
      // Num of regular parameters to calculate gp_offset
      int n = 0;
      for (VarList *vl = fn->params; vl; vl = vl->next) {
        n++;
      }
      // Register save area
      printf("  mov [rbp-56], rdi\n");
      printf("  mov [rbp-48], rsi\n");
      printf("  mov [rbp-40], rdx\n");
      printf("  mov [rbp-32], rcx\n");
      printf("  mov [rbp-24], r8\n");
      printf("  mov [rbp-16], r9\n");
      // gp_offset
      printf("  mov dword ptr [rbp-8], %d\n", n * 8);
    }

    // Push arguments to the stack
    int i = 0;
    for (VarList *vl = fn->params; vl; vl = vl->next)
      load_arg(vl->var, i++);

    // Emit code
    for (Node *node = fn->node; node; node = node->next)
      gen(node);

    // Epilogue
    printf(".L.return.%s:\n", funcname);
    printf("  mov rsp, rbp\n");
    printf("  pop rbp\n");
    printf("  ret\n");
  }
}

void codegen(Program *prog) {
  printf(".intel_syntax noprefix\n");
  emit_data(prog);
  emit_text(prog);
}
