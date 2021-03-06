#include <assert.h>
#include <elf.h>
#include <errno.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "dfcc.h"

// [ u4 width ] [ u4 base ]
// [0001][0000] AL
// [0010][0000] AX
// [0100][0000] EAX
// [1000][0000] RAX
typedef enum {
  RAX = 1 << 7, RCX, RDX, RBX, RSP, RBP, RSI, RDI,
  R8, R9, R10, R11, R12, R13, R14, R15,
} Reg;

static const Reg gArgRegs[] = {RDI, RSI, RDX, RCX, R8, R9};

static const char *gRegNames[] = {
  "al",   "ax",   "eax",  "rax",
  "cl",   "cx",   "ecx",  "rcx",
  "dl",   "dx",   "edx",  "rdx",
  "bl",   "bx",   "ebx",  "rbx",
  "spl",  "sp",   "esp",  "rsp",
  "bpl",  "bp",   "ebp",  "rbp",
  "sil",  "si",   "esi",  "rsi",
  "dil",  "di",   "edi",  "rdi",
  "r8b",  "r8w",  "r8d",  "r8",
  "r9b",  "r9w",  "r9d",  "r9",
  "r10b", "r10w", "r10d", "r10",
  "r11b", "r11w", "r11d", "r11",
  "r12b", "r12w", "r12d", "r12",
  "r13b", "r13w", "r13d", "r13",
  "r14b", "r14w", "r14d", "r14",
  "r15b", "r15w", "r15d", "r15",
};

// Instructions
// Borrowed from lacc ((c) 2015 Lars Kirkholt Melhus, MIT)
typedef enum {
  I_ADD = 0,
  I_OR,
  I_AND,
  I_SUB,
  I_XOR,
  I_CMP,
  I_PUSH,
  I_POP,
  I_IMUL,
  I_MOV,
  I_LEA,
  I_CQO,
  I_SHL,
  I_SAR,
  I_RETN,
  I_CALL,
  I_JMP,
  I_NOT,
  I_IDIV,
  I_Jcc,
  I_SETcc,
  I_MOVZX,
  I_MOVSX,
} Instruction;

// Instruction operand(s)
typedef enum {
  OP_NONE = 0,
  OP_IMM = 1,
  OP_REG = 2,
  OP_MEM = 4,
  OP_REG_REG = 8,
  OP_MEM_REG = 16,
  OP_REG_MEM = 32,
  OP_REG_IMM = 64,
  OP_MEM_IMM = 128,
} Operand;

// Instruction encoding rules
typedef struct {
  Instruction inst;
  const char *mnemonic;
  uint8_t opcodes[3];
} Encoding;

static Encoding gEncodings[] = {
  {I_ADD,   "add",   {0x00}},

  {I_OR,    "or",    {0x08}},

  {I_AND,   "and",   {0x20}},

  {I_SUB,   "sub",   {0x28}},

  {I_XOR,   "xor",   {0x30}},

  {I_CMP,   "cmp",   {0x38}},

  {I_PUSH,  "push",  {0x50}},

  {I_POP,   "pop",   {0x58}},

  {I_IMUL,  "imul",  {0x69}},

  {I_MOV,   "mov",   {0x88}},

  {I_LEA,   "lea",   {0x8D}},

  {I_CQO,   "cqo",   {0x99}},

  {I_SHL,   "shl",   {0xC0}},

  {I_SAR,   "sar",   {0xC0}},

  {I_RETN,  "ret",  {0xC3}},

  {I_CALL,  "call",  {0xE8}},

  {I_JMP,   "jmp",   {0xE9}},

  {I_NOT,   "not",   {0xF6}},

  {I_IDIV,  "idiv",  {0xF6}},

  {I_Jcc,   "j",     {0x0F, 0x80}},

  {I_SETcc, "set",   {0x0F, 0x91}},

  {I_MOVZX, "movzx", {0x0F, 0xB6}},

  {I_MOVSX, "movsx", {0x0F, 0xBE}},
};

// Binary code for single instruction, 15 bytes max
typedef struct {
  uint8_t buf[15];
  char len;
} Code;

#define S_NUM 5
#define SID_SHSTRTAB 1
#define SID_STRTAB 2
#define SID_SYMTAB 3
#define SID_TEXT 4

typedef struct {
  bool dump_asm;
  FILE *outfp;
  Elf64_Shdr shdr[S_NUM];
  Buf *sect[S_NUM];
  int labelseq;
  int brkseq;
  int contseq;
  const char *funcname;
} GenContext;

static GenContext *gCtx;

//
// ELF
//

static void write_data(const void *data, size_t data_size) {
  fwrite(data, 1, data_size, gCtx->outfp);
}

static void elf_symbol(int sym_type, const char *sym_name, bool is_global) {
  Elf64_Sym entry = { 0 };
  entry.st_name = buf_writestr(gCtx->sect[SID_STRTAB], sym_name);
  entry.st_value = 0;
  entry.st_info = (is_global ? STB_GLOBAL : STB_LOCAL) << 4 | sym_type;
  entry.st_shndx = 4;
  buf_write(gCtx->sect[SID_SYMTAB], &entry, sizeof(Elf64_Sym));
}

static void elf_text(Code *code) {
  assert(code->len <= 15);
  buf_write(gCtx->sect[SID_TEXT], code->buf, code->len);
}

static void elf_start() {
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
    S_NUM, // section headers number
    SID_SHSTRTAB, // shstrtab index
  };
  write_data(&elf_header, sizeof(elf_header));

  // Create sections
  for (int i = 1; i < S_NUM; i++) {
    gCtx->sect[i] = new_buf();
  }
  Buf *shstrtab = gCtx->sect[SID_SHSTRTAB];

  // Empty section
  buf_writestr(shstrtab, "");

  // Section names section
  Elf64_Shdr *shstrtab_header = &gCtx->shdr[SID_SHSTRTAB];
  shstrtab_header->sh_name = buf_writestr(shstrtab, ".shstrtab");
  shstrtab_header->sh_type = SHT_STRTAB;
  shstrtab_header->sh_addralign = 1;

  // Symbol names section
  Elf64_Shdr *strtab_header = &gCtx->shdr[SID_STRTAB];
  strtab_header->sh_name = buf_writestr(shstrtab, ".strtab");
  strtab_header->sh_type = SHT_STRTAB;
  strtab_header->sh_addralign = 1;

  // Symbol section
  Elf64_Shdr *symtab_header = &gCtx->shdr[SID_SYMTAB];
  symtab_header->sh_name = buf_writestr(shstrtab, ".symtab");
  symtab_header->sh_type = SHT_SYMTAB;
  symtab_header->sh_link = SID_STRTAB;
  symtab_header->sh_info = 2; // FIXME
  symtab_header->sh_addralign = 8;
  symtab_header->sh_entsize = sizeof(Elf64_Sym);
  // Null symbol
  Elf64_Sym null_entry = { 0 };
  buf_writestr(gCtx->sect[SID_STRTAB], "");
  buf_write(gCtx->sect[SID_SYMTAB], &null_entry, sizeof(Elf64_Sym));
  // Text section symbol
  elf_symbol(STT_SECTION, "", false);

  // Text section
  Elf64_Shdr *text_header = &gCtx->shdr[SID_TEXT];
  text_header->sh_name = buf_writestr(shstrtab, ".text");
  text_header->sh_type = SHT_PROGBITS;
  text_header->sh_flags = SHF_EXECINSTR | SHF_ALLOC;
  text_header->sh_addralign = 16;

  // Fix first offset
  gCtx->shdr[1].sh_offset = sizeof(Elf64_Ehdr) + S_NUM * sizeof(Elf64_Shdr);
  gCtx->shdr[1].sh_size = gCtx->sect[1]->size;
}

static void elf_end() {
  // Fix offsets
  for (int i = 2; i < S_NUM; i++) {
    gCtx->shdr[i].sh_offset = gCtx->shdr[i-1].sh_offset + gCtx->shdr[i-1].sh_size;
    gCtx->shdr[i].sh_size = gCtx->sect[i]->size;
  }

  // Dump headers
  write_data(&gCtx->shdr, sizeof(Elf64_Shdr) * S_NUM);

  // Dump data
  for (int i = 1; i < S_NUM; i++) {
    write_data(gCtx->sect[i]->data, gCtx->sect[i]->size);
  }
}

//
// Encoding
//

// Pack several parameters of register/memory offset into one integer.
// TODO(Kagami): Macro functions/inline?
// TODO(Kagami): Or pass as a struct instead?

// [ u4 width ] [ u4 base ]
static int reg_width(Reg reg) { return (reg>>4)&0xf; }
static Reg reg_base(Reg reg) { return reg&0xf; }
static Reg regw(Reg base, int width) { return width<<4 | (base&0xf); }
static Reg argregw(int idx, int width) { return regw(gArgRegs[idx], width); }

// [ u8 width ] [ u8 reg ] [ i32 offset ]
static int off_width(uint64_t arg) { return (arg>>40)&0xff; }
static Reg off_reg(uint64_t arg) { return (arg>>32)&0xff; }
static int off_offset(uint64_t arg) { return arg; }
static uint64_t offw(Reg reg, int offset, int width) {
  return ((uint64_t)width&0xff)<<40 | ((uint64_t)reg&0xff)<<32 | (offset&0xffffffff);
}
static uint64_t off(Reg reg, int offset) { return offw(reg, offset, 0); }

static const char *reg2s(Reg reg) {
  int i = reg_base(reg);
  int j;
  switch (reg_width(reg)) {
  case 1: j = 0; break;
  case 2: j = 1; break;
  case 4: j = 2; break;
  case 8: j = 3; break;
  default: assert(false);
  }
  return gRegNames[i*4 + j];
}

static const char *i2s(Instruction inst) {
  return gEncodings[inst].mnemonic;
}

static const char *off2s(uint64_t arg) {
  static char buf[100];
  const char *wprefix = "";
  switch (off_width(arg)) {
  case 8: wprefix = "qword ptr "; break;
  case 4: wprefix = "dword ptr "; break;
  case 2: wprefix = "word ptr "; break;
  case 1: wprefix = "byte ptr "; break;
  }
  if (off_offset(arg)) {
    sprintf(buf, "%s[%s%+d]", wprefix, reg2s(off_reg(arg)), off_offset(arg));
  } else {
    sprintf(buf, "%s[%s]", wprefix, reg2s(off_reg(arg)));
  }
  return buf;
}

static void emit_fmt(const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vfprintf(gCtx->outfp, fmt, ap);
  fprintf(gCtx->outfp, "\n");
  va_end(ap);
}

static void emit_global(const char *name) {
  if (gCtx->dump_asm) {
    emit_fmt(".global %s", name);
  }
}

static void emit_label(const char *label_type, const char *label_name) {
  if (gCtx->dump_asm) {
    emit_fmt(".L.%s.%s:", label_type, label_name);
  }
}

static void emit_labelseq(const char *label_type, int seq) {
  if (gCtx->dump_asm) {
    emit_fmt(".L.%s.%d:", label_type, seq);
  }
}

static void emit_section(const char *name) {
  if (gCtx->dump_asm) {
    emit_fmt(".%s", name);
  }
}

static void emit_symbol(int sym_type, const char *sym_name, bool is_global) {
  if (gCtx->dump_asm) {
    if (is_global) {
      emit_global(sym_name);
    }
    emit_fmt("%s:", sym_name);
  } else {
    elf_symbol(sym_type, sym_name, is_global);
  }
}

static void emit_asm(Instruction inst, Operand op, uint64_t arg1, uint64_t arg2) {
  switch (op) {
  case OP_NONE:
    emit_fmt("  %s", i2s(inst));
    break;
  case OP_IMM:
    // TODO(Kagami): unsigned long?
    emit_fmt("  %s %ld", i2s(inst), arg1);
    break;
  case OP_REG:
    emit_fmt("  %s %s", i2s(inst), reg2s(arg1));
    break;
  case OP_MEM:
    emit_fmt("  %s %s", i2s(inst), off2s(arg1));
    break;
  case OP_REG_REG:
    emit_fmt("  %s %s, %s", i2s(inst), reg2s(arg1), reg2s(arg2));
    break;
  case OP_MEM_REG:
    emit_fmt("  %s %s, %s", i2s(inst), off2s(arg1), reg2s(arg2));
    break;
  case OP_REG_MEM:
    emit_fmt("  %s %s, %s", i2s(inst), reg2s(arg1), off2s(arg2));
    break;
  case OP_REG_IMM:
    emit_fmt("  %s %s, %ld", i2s(inst), reg2s(arg1), arg2);
    break;
  case OP_MEM_IMM:
    emit_fmt("  %s %s, %ld", i2s(inst), off2s(arg1), arg2);
    break;
  }
}

static void emit_code(Instruction inst, Operand op, uint64_t arg1, uint64_t arg2) {
  Code code = { 0 };
  switch (op) {
  case OP_NONE:
    break;
  case OP_IMM:
    break;
  case OP_REG:
    break;
  case OP_MEM:
    break;
  case OP_REG_REG:
    break;
  case OP_MEM_REG:
    break;
  case OP_REG_MEM:
    break;
  case OP_REG_IMM:
    break;
  case OP_MEM_IMM:
    break;
  }
  elf_text(&code);
}

// TODO(Kagami): Implement with va_arg.
static void emit2(Instruction inst, Operand op, uint64_t arg1, uint64_t arg2) {
  if (gCtx->dump_asm) {
    emit_asm(inst, op, arg1, arg2);
  } else {
    emit_code(inst, op, arg1, arg2);
  }
}

static void emit1(Instruction inst, Operand op, uint64_t arg) {
  emit2(inst, op, arg, 0);
}

static void emit0(Instruction inst) {
  emit2(inst, OP_NONE, 0, 0);
}

//
// Tree walking
//

static void gen_node(Node *node);

// Pushes the given node's address to the stack.
static void gen_addr(Node *node) {
  switch (node->kind) {
  case ND_VAR: {
    if (node->init) {
      gen_node(node->init);
    }
    Var *var = node->var;
    if (var->is_local) {
      emit2(I_LEA, OP_REG_MEM, RAX, off(RBP, -var->offset));
      emit1(I_PUSH, OP_REG, RAX);
    } else {
      emit_fmt("  push offset %s", var->name);
    }
    return;
  }
  case ND_DEREF:
    gen_node(node->lhs);
    return;
  case ND_MEMBER:
    gen_addr(node->lhs);
    emit1(I_POP, OP_REG, RAX);
    emit2(I_ADD, OP_REG_IMM, RAX, node->member->offset);
    emit1(I_PUSH, OP_REG, RAX);
    return;
  default:
    error_tok(node->tok, "not an lvalue");
  }
}

static void gen_lval(Node *node) {
  if (node->ty->kind == TY_ARRAY) {
    error_tok(node->tok, "not an lvalue");
  }
  gen_addr(node);
}

static void load(Type *ty) {
  emit1(I_POP, OP_REG, RAX);
  if (ty->size < 8) {
    emit2(I_MOVSX, OP_REG_MEM, RAX, offw(RAX, 0, ty->size));
  } else {
    emit2(I_MOV, OP_REG_MEM, RAX, off(RAX, 0));
  }
  emit1(I_PUSH, OP_REG, RAX);
}

static void store(Type *ty) {
  emit1(I_POP, OP_REG, RDI);
  emit1(I_POP, OP_REG, RAX);
  if (ty->kind == TY_BOOL) {
    emit2(I_CMP, OP_REG_IMM, RDI, 0);
    emit_fmt("  setne dil");
    emit2(I_MOVZX, OP_REG_REG, RDI, regw(RDI, 1));
  }
  emit2(I_MOV, OP_MEM_REG, off(RAX, 0), regw(RDI, ty->size));
  emit1(I_PUSH, OP_REG, RDI);
}

static void cast_truncate(Type *ty) {
  emit1(I_POP, OP_REG, RAX);
  if (ty->kind == TY_BOOL) {
    emit2(I_CMP, OP_REG_IMM, RAX, 0);
    emit_fmt("  setne al");
  }
  if (ty->size < 8) {
    emit2(I_MOVSX, OP_REG_REG, RAX, regw(RAX, ty->size));
  }
  emit1(I_PUSH, OP_REG, RAX);
}

static void inc(Type *ty) {
  emit1(I_POP, OP_REG, RAX);
  emit2(I_ADD, OP_REG_IMM, RAX, ty->base ? ty->base->size : 1);
  emit1(I_PUSH, OP_REG, RAX);
}

static void dec(Type *ty) {
  emit1(I_POP, OP_REG, RAX);
  emit2(I_SUB, OP_REG_IMM, RAX, ty->base ? ty->base->size : 1);
  emit1(I_PUSH, OP_REG, RAX);
}

static void gen_binary(Node *node) {
  emit1(I_POP, OP_REG, RDI);
  emit1(I_POP, OP_REG, RAX);

  switch (node->kind) {
  case ND_ADD:
  case ND_ADD_EQ:
    emit2(I_ADD, OP_REG_REG, RAX, RDI);
    break;
  case ND_PTR_ADD:
  case ND_PTR_ADD_EQ:
    emit2(I_IMUL, OP_REG_IMM, RDI, node->ty->base->size);
    emit2(I_ADD, OP_REG_REG, RAX, RDI);
    break;
  case ND_SUB:
  case ND_SUB_EQ:
    emit2(I_SUB, OP_REG_REG, RAX, RDI);
    break;
  case ND_PTR_SUB:
  case ND_PTR_SUB_EQ:
    emit2(I_IMUL, OP_REG_IMM, RDI, node->ty->base->size);
    emit2(I_SUB, OP_REG_REG, RAX, RDI);
    break;
  case ND_PTR_DIFF:
    emit2(I_SUB, OP_REG_REG, RAX, RDI);
    emit0(I_CQO);
    emit2(I_MOV, OP_REG_IMM, RDI, node->lhs->ty->base->size);
    emit1(I_IDIV, OP_REG, RDI);
    break;
  case ND_MUL:
  case ND_MUL_EQ:
    emit2(I_IMUL, OP_REG_REG, RAX, RDI);
    break;
  case ND_DIV:
  case ND_DIV_EQ:
    emit0(I_CQO);
    emit1(I_IDIV, OP_REG, RDI);
    break;
  case ND_BITAND:
  case ND_BITAND_EQ:
    emit2(I_AND, OP_REG_REG, RAX, RDI);
    break;
  case ND_BITOR:
  case ND_BITOR_EQ:
    emit2(I_OR, OP_REG_REG, RAX, RDI);
    break;
  case ND_BITXOR:
  case ND_BITXOR_EQ:
    emit2(I_XOR, OP_REG_REG, RAX, RDI);
    break;
  case ND_SHL:
  case ND_SHL_EQ:
    emit2(I_MOV, OP_REG_REG, regw(RCX, 1), regw(RDI, 1));
    emit2(I_SHL, OP_REG_REG, RAX, regw(RCX, 1));
    break;
  case ND_SHR:
  case ND_SHR_EQ:
    emit2(I_MOV, OP_REG_REG, regw(RCX, 1), regw(RDI, 1));
    emit2(I_SAR, OP_REG_REG, RAX, regw(RCX, 1));
    break;
  case ND_EQ:
    emit2(I_CMP, OP_REG_REG, RAX, RDI);
    emit_fmt("  sete al");
    emit2(I_MOVZX, OP_REG_REG, RAX, regw(RAX, 1));
    break;
  case ND_NE:
    emit2(I_CMP, OP_REG_REG, RAX, RDI);
    emit_fmt("  setne al");
    emit2(I_MOVZX, OP_REG_REG, RAX, regw(RAX, 1));
    break;
  case ND_LT:
    emit2(I_CMP, OP_REG_REG, RAX, RDI);
    emit_fmt("  setl al");
    emit2(I_MOVZX, OP_REG_REG, RAX, regw(RAX, 1));
    break;
  case ND_LE:
    emit2(I_CMP, OP_REG_REG, RAX, RDI);
    emit_fmt("  setle al");
    emit2(I_MOVZX, OP_REG_REG, RAX, regw(RAX, 1));
    break;
  default:
    break;
  }

  emit1(I_PUSH, OP_REG, RAX);
}

// Generate code for a given node.
static void gen_node(Node *node) {
  switch (node->kind) {
  case ND_NULL:
    return;
  case ND_NUM:
    if (node->val == (int)node->val) {
      emit1(I_PUSH, OP_IMM, node->val);
    } else {
      emit2(I_MOV, OP_REG_IMM, RAX, node->val);
      emit1(I_PUSH, OP_REG, RAX);
    }
    return;
  case ND_EXPR_STMT:
    gen_node(node->lhs);
    emit2(I_ADD, OP_REG_IMM, RSP, 8);
    return;
  case ND_VAR:
    if (node->init)
      gen_node(node->init);
    gen_addr(node);
    if (node->ty->kind != TY_ARRAY) {
      load(node->ty);
    }
    return;
  case ND_MEMBER:
    gen_addr(node);
    if (node->ty->kind != TY_ARRAY) {
      load(node->ty);
    }
    return;
  case ND_ASSIGN:
    gen_lval(node->lhs);
    gen_node(node->rhs);
    store(node->ty);
    return;
  case ND_TERNARY: {
    int seq = gCtx->labelseq++;
    gen_node(node->cond);
    emit1(I_POP, OP_REG, RAX);
    emit2(I_CMP, OP_REG_IMM, RAX, 0);
    emit_fmt("  je  .L.else.%d", seq);
    gen_node(node->then);
    emit_fmt("  jmp .L.end.%d", seq);
    emit_labelseq("else", seq);
    gen_node(node->els);
    emit_labelseq("end", seq);
    return;
  }
  case ND_PRE_INC:
    gen_lval(node->lhs);
    emit1(I_PUSH, OP_MEM, off(RSP, 0));
    load(node->ty);
    inc(node->ty);
    store(node->ty);
    return;
  case ND_PRE_DEC:
    gen_lval(node->lhs);
    emit1(I_PUSH, OP_MEM, off(RSP, 0));
    load(node->ty);
    dec(node->ty);
    store(node->ty);
    return;
  case ND_POST_INC:
    gen_lval(node->lhs);
    emit1(I_PUSH, OP_MEM, off(RSP, 0));
    load(node->ty);
    inc(node->ty);
    store(node->ty);
    dec(node->ty);
    return;
  case ND_POST_DEC:
    gen_lval(node->lhs);
    emit1(I_PUSH, OP_MEM, off(RSP, 0));
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
    emit1(I_PUSH, OP_MEM, off(RSP, 0));
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
    if (node->ty->kind != TY_ARRAY) {
      load(node->ty);
    }
    return;
  case ND_NOT:
    gen_node(node->lhs);
    emit1(I_POP, OP_REG, RAX);
    emit2(I_CMP, OP_REG_IMM, RAX, 0);
    emit_fmt("  sete al");
    emit2(I_MOVZX, OP_REG_REG, RAX, regw(RAX, 1));
    emit1(I_PUSH, OP_REG, RAX);
    return;
  case ND_BITNOT:
    gen_node(node->lhs);
    emit1(I_POP, OP_REG, RAX);
    emit1(I_NOT, OP_REG, RAX);
    emit1(I_PUSH, OP_REG, RAX);
    return;
  case ND_LOGAND: {
    int seq = gCtx->labelseq++;
    gen_node(node->lhs);
    emit1(I_POP, OP_REG, RAX);
    emit2(I_CMP, OP_REG_IMM, RAX, 0);
    emit_fmt("  je  .L.false.%d", seq);
    gen_node(node->rhs);
    emit1(I_POP, OP_REG, RAX);
    emit2(I_CMP, OP_REG_IMM, RAX, 0);
    emit_fmt("  je  .L.false.%d", seq);
    emit1(I_PUSH, OP_IMM, 1);
    emit_fmt("  jmp .L.end.%d", seq);
    emit_labelseq("false", seq);
    emit1(I_PUSH, OP_IMM, 0);
    emit_labelseq("end", seq);
    return;
  }
  case ND_LOGOR: {
    int seq = gCtx->labelseq++;
    gen_node(node->lhs);
    emit1(I_POP, OP_REG, RAX);
    emit2(I_CMP, OP_REG_IMM, RAX, 0);
    emit_fmt("  jne .L.true.%d", seq);
    gen_node(node->rhs);
    emit1(I_POP, OP_REG, RAX);
    emit2(I_CMP, OP_REG_IMM, RAX, 0);
    emit_fmt("  jne .L.true.%d", seq);
    emit1(I_PUSH, OP_IMM, 0);
    emit_fmt("  jmp .L.end.%d", seq);
    emit_labelseq("true", seq);
    emit1(I_PUSH, OP_IMM, 1);
    emit_labelseq("end", seq);
    return;
  }
  case ND_IF: {
    int seq = gCtx->labelseq++;
    if (node->els) {
      gen_node(node->cond);
      emit1(I_POP, OP_REG, RAX);
      emit2(I_CMP, OP_REG_IMM, RAX, 0);
      emit_fmt("  je  .L.else.%d", seq);
      gen_node(node->then);
      emit_fmt("  jmp .L.end.%d", seq);
      emit_labelseq("else", seq);
      gen_node(node->els);
      emit_labelseq("end", seq);
    } else {
      gen_node(node->cond);
      emit1(I_POP, OP_REG, RAX);
      emit2(I_CMP, OP_REG_IMM, RAX, 0);
      emit_fmt("  je  .L.end.%d", seq);
      gen_node(node->then);
      emit_labelseq("end", seq);
    }
    return;
  }
  case ND_WHILE: {
    int seq = gCtx->labelseq++;
    int brk = gCtx->brkseq;
    int cont = gCtx->contseq;
    gCtx->brkseq = gCtx->contseq = seq;

    emit_labelseq("continue", seq);
    gen_node(node->cond);
    emit1(I_POP, OP_REG, RAX);
    emit2(I_CMP, OP_REG_IMM, RAX, 0);
    emit_fmt("  je  .L.break.%d", seq);
    gen_node(node->then);
    emit_fmt("  jmp .L.continue.%d", seq);
    emit_labelseq("break", seq);

    gCtx->brkseq = brk;
    gCtx->contseq = cont;
    return;
  }
  case ND_FOR: {
    int seq = gCtx->labelseq++;
    int brk = gCtx->brkseq;
    int cont = gCtx->contseq;
    gCtx->brkseq = gCtx->contseq = seq;

    if (node->init) {
      gen_node(node->init);
    }
    emit_labelseq("begin", seq);
    if (node->cond) {
      gen_node(node->cond);
      emit1(I_POP, OP_REG, RAX);
      emit2(I_CMP, OP_REG_IMM, RAX, 0);
      emit_fmt("  je  .L.break.%d", seq);
    }
    gen_node(node->then);
    emit_labelseq("continue", seq);
    if (node->inc) {
      gen_node(node->inc);
    }
    emit_fmt("  jmp .L.begin.%d", seq);
    emit_labelseq("break", seq);

    gCtx->brkseq = brk;
    gCtx->contseq = cont;
    return;
  }
  case ND_DO: {
    int seq = gCtx->labelseq++;
    int brk = gCtx->brkseq;
    int cont = gCtx->contseq;
    gCtx->brkseq = gCtx->contseq = seq;

    emit_labelseq("begin", seq);
    gen_node(node->then);
    emit_labelseq("continue", seq);
    gen_node(node->cond);
    emit1(I_POP, OP_REG, RAX);
    emit2(I_CMP, OP_REG_IMM, RAX, 0);
    emit_fmt("  jne .L.begin.%d", seq);
    emit_labelseq("break", seq);

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
    emit1(I_POP, OP_REG, RAX);

    for (Node *n = node->case_next; n; n = n->case_next) {
      n->case_label = gCtx->labelseq++;
      n->case_end_label = seq;
      emit2(I_CMP, OP_REG_IMM, RAX, n->val);
      emit_fmt("  je .L.case.%d", n->case_label);
    }

    if (node->default_case) {
      int i = gCtx->labelseq++;
      node->default_case->case_end_label = seq;
      node->default_case->case_label = i;
      emit_fmt("  jmp .L.case.%d", i);
    }

    emit_fmt("  jmp .L.break.%d", seq);
    gen_node(node->then);
    emit_labelseq("break", seq);

    gCtx->brkseq = brk;
    return;
  }
  case ND_CASE:
    emit_labelseq("case", node->case_label);
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
    emit_fmt("  jmp .L.break.%d", gCtx->brkseq);
    return;
  case ND_CONTINUE:
    if (gCtx->contseq == 0)
      error_tok(node->tok, "stray continue");
    emit_fmt("  jmp .L.continue.%d", gCtx->contseq);
    return;
  case ND_GOTO:
    emit_fmt("  jmp .L.label.%s.%s", gCtx->funcname, node->label_name);
    return;
  case ND_LABEL:
    emit_fmt(".L.label.%s.%s:", gCtx->funcname, node->label_name);
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
      emit1(I_POP, OP_REG, argregw(i, 8));
    }

    if (strcmp(node->funcname, "__builtin_va_start") == 0) {
      // Get gp_offset
      emit2(I_MOV, OP_REG_MEM, regw(RDX, 4), off(RBP, -8));
      // Get reg_save_area
      emit2(I_LEA, OP_REG_MEM, RCX, off(RBP, -56));
      // Fill va_list
      emit2(I_MOV, OP_MEM_REG, offw(RDI, 0, 4), regw(RDX, 4)); // gp_offset
      emit2(I_MOV, OP_MEM_IMM, offw(RDI, 4, 4), 0); // fp_offset
      emit2(I_MOV, OP_MEM_IMM, offw(RDI, 8, 4), 0); // overflow_arg_area
      emit2(I_MOV, OP_MEM_REG, offw(RDI, 16, 8), RCX); // reg_save_area
      // Adjust for ND_EXPR_STMT
      emit2(I_SUB, OP_REG_IMM, RSP, 8);
      return;
    }

    // We need to align RSP to a 16 byte boundary before
    // calling a function because it is an ABI requirement.
    // RAX is set to 0 for variadic function.
    int seq = gCtx->labelseq++;
    emit2(I_MOV, OP_REG_REG, RAX, RSP);
    emit2(I_AND, OP_REG_IMM, RAX, 15);
    emit_fmt("  jnz .L.call.%d", seq);
    emit2(I_MOV, OP_REG_IMM, RAX, 0);
    emit_fmt("  call %s", node->funcname);
    emit_fmt("  jmp .L.end.%d", seq);
    emit_labelseq("call", seq);
    emit2(I_SUB, OP_REG_IMM, RSP, 8);
    emit2(I_MOV, OP_REG_IMM, RAX, 0);
    emit_fmt("  call %s", node->funcname);
    emit2(I_ADD, OP_REG_IMM, RSP, 8);
    emit_labelseq("end", seq);
    if (node->ty->kind == TY_BOOL) {
      emit2(I_MOVZX, OP_REG_REG, RAX, regw(RAX, 1));
    }
    emit1(I_PUSH, OP_REG, RAX);
    return;
  }
  case ND_RETURN:
    if (node->lhs) {
      gen_node(node->lhs);
      emit1(I_POP, OP_REG, RAX);
    }
    if (gCtx->dump_asm) {
      emit_fmt("  jmp .L.return.%s", gCtx->funcname);
    }
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

static void gen_start() {
  if (gCtx->dump_asm) {
    emit_fmt(".intel_syntax noprefix");
  } else {
    elf_start();
  }
}

static void gen_data(Program *prog) {
  for (VarList *vl = prog->globals; vl; vl = vl->next) {
    if (!vl->var->is_static) {
      emit_global(vl->var->name);
    }
  }

  emit_section("bss");
  for (VarList *vl = prog->globals; vl; vl = vl->next) {
    Var *var = vl->var;
    if (var->initializer) continue;
    emit_fmt(".align %d", var->ty->align);
    emit_fmt("%s:", var->name);
    emit_fmt("  .zero %d", var->ty->size);
  }

  emit_section("data");
  for (VarList *vl = prog->globals; vl; vl = vl->next) {
    Var *var = vl->var;
    if (!var->initializer) continue;
    emit_fmt(".align %d", var->ty->align);
    emit_fmt("%s:", var->name);
    for (Initializer *init = var->initializer; init; init = init->next) {
      if (init->label) {
        emit_fmt("  .quad %s%+ld", init->label, init->addend);
      } else if (init->sz == 1) {
        emit_fmt("  .byte %ld", init->val);
      } else {
        emit_fmt("  .%dbyte %ld", init->sz, init->val);
      }
    }
  }
}

static void load_arg(Var *var, int idx) {
  emit2(I_MOV, OP_MEM_REG, off(RBP, -var->offset), argregw(idx, var->ty->size));
}

static void gen_text(Program *prog) {
  emit_section("text");
  for (Function *fn = prog->fns; fn; fn = fn->next) {
    emit_symbol(STT_FUNC, fn->name, !fn->is_static);
    gCtx->funcname = fn->name;

    // Prologue
    emit1(I_PUSH, OP_REG, RBP);
    emit2(I_MOV, OP_REG_REG, RBP, RSP);
    emit2(I_SUB, OP_REG_IMM, RSP, fn->stack_size);

    // Save arg registers if function is variadic
    if (fn->has_varargs) {
      // Num of regular params to calculate gp_offset
      int n = 0;
      for (VarList *vl = fn->params; vl; vl = vl->next) { n++; }
      // Register save area
      emit2(I_MOV, OP_MEM_REG, off(RBP, -56), RDI);
      emit2(I_MOV, OP_MEM_REG, off(RBP, -48), RSI);
      emit2(I_MOV, OP_MEM_REG, off(RBP, -40), RDX);
      emit2(I_MOV, OP_MEM_REG, off(RBP, -32), RCX);
      emit2(I_MOV, OP_MEM_REG, off(RBP, -24), R8);
      emit2(I_MOV, OP_MEM_REG, off(RBP, -16), R9);
      // gp_offset
      emit2(I_MOV, OP_MEM_IMM, offw(RBP, -8, 4), n*8);
    }

    // Push arguments to the stack
    int i = 0;
    for (VarList *vl = fn->params; vl; vl = vl->next) {
      load_arg(vl->var, i++);
    }

    // Body
    for (Node *node = fn->node; node; node = node->next) {
      gen_node(node);
    }

    // Epilogue
    emit_label("return", gCtx->funcname);
    emit2(I_MOV, OP_REG_REG, RSP, RBP);
    emit1(I_POP, OP_REG, RBP);
    emit0(I_RETN);
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

static void gen_end() {
  if (!gCtx->dump_asm) {
    elf_end();
  }
}

// Generate code for the entire program.
void gen_prog(Program *prog, const char *path, bool dump_asm) {
  gCtx = calloc(1, sizeof(GenContext));
  gCtx->dump_asm = dump_asm;
  gCtx->labelseq = 1;
  gCtx->outfp = path ? fopen(path, "wb") : stdout;
  if (!gCtx->outfp) error("cannot open %s (%s)", path, strerror(errno));

  gen_start();
  gen_data(prog);
  gen_text(prog);
  gen_end();

  if (path) {
    fclose(gCtx->outfp);
  }
}
