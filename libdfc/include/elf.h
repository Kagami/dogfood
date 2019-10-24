#include <stdint.h>

#define EI_NIDENT 16
#define ELFMAG0 0x7f
#define ELFMAG1 'E'
#define ELFMAG2 'L'
#define ELFMAG3 'F'
#define ELFCLASS64 2
#define ELFDATA2LSB 1
#define EV_CURRENT 1
#define ELFOSABI_SYSV 0
#define ET_REL 1
#define EM_X86_64 62

#define SHT_PROGBITS 1
#define SHT_SYMTAB 2
#define SHT_STRTAB 3

#define SHF_ALLOC (1 << 1)
#define SHF_EXECINSTR (1 << 2)

#define STB_GLOBAL 1

#define STT_FUNC 2

typedef uint16_t Elf64_Half;
typedef uint32_t Elf64_Word;
typedef int32_t Elf64_Sword;
typedef uint64_t Elf64_Xword;
typedef int64_t Elf64_Sxword;
typedef uint64_t Elf64_Addr;
typedef uint64_t Elf64_Off;
typedef uint16_t Elf64_Section;

typedef struct {
 unsigned char e_ident[EI_NIDENT];
 Elf64_Half e_type;
 Elf64_Half e_machine;
 Elf64_Word e_version;
 Elf64_Addr e_entry;
 Elf64_Off e_phoff;
 Elf64_Off e_shoff;
 Elf64_Word e_flags;
 Elf64_Half e_ehsize;
 Elf64_Half e_phentsize;
 Elf64_Half e_phnum;
 Elf64_Half e_shentsize;
 Elf64_Half e_shnum;
 Elf64_Half e_shstrndx;
} Elf64_Ehdr;

typedef struct {
 Elf64_Word sh_name;
 Elf64_Word sh_type;
 Elf64_Xword sh_flags;
 Elf64_Addr sh_addr;
 Elf64_Off sh_offset;
 Elf64_Xword sh_size;
 Elf64_Word sh_link;
 Elf64_Word sh_info;
 Elf64_Xword sh_addralign;
 Elf64_Xword sh_entsize;
} Elf64_Shdr;

typedef struct {
 Elf64_Word st_name;
 unsigned char st_info;
 unsigned char st_other;
 Elf64_Section st_shndx;
 Elf64_Addr st_value;
 Elf64_Xword st_size;
} Elf64_Sym;
