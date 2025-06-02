#include "ec.h"
#include "ptab.h"
#include "string.h"

typedef enum {
    sys_print      = 1,
    sys_sum        = 2,
    sys_break      = 3,
    sys_thr_create = 4,
    sys_thr_yield  = 5,
} Syscall_numbers;

void Ec::syscall_handler (uint8 a)
{
    // Get access to registers stored during entering the system - see
    // entry_sysenter in entry.S
    Sys_regs * r = current->sys_regs();
    Syscall_numbers number = static_cast<Syscall_numbers> (a);

    switch (number) {
        case sys_print: {
            // Tisk řetězce na sériovou linku
            char *data = reinterpret_cast<char*>(r->esi);
            unsigned len = r->edi;
            for (unsigned i = 0; i < len; i++)
                printf("%c", data[i]);
            break;
        }
        case sys_sum: {
            // Naprosto nepotřebné systémové volání na sečtení dvou čísel
            int first_number = r->esi;
            int second_number = r->edi;
            r->eax = first_number + second_number;
            break;
        }
        case sys_break: {
            bool success = true;
            mword virt = break_current;
            mword new_break = r->esi;
            if (new_break == break_current || new_break == 0) {
                r->eax = break_current;
                break;
            }
            if (new_break < break_min || new_break > 0xBFFFF000) {
                r->eax = 0;
                break;
            }
            if (new_break > break_current) {
                while (virt < new_break) {
                    if ((virt & 0xFFF) != 0) {
                        mword phys = Ptab::get_mapping(virt) & ~PAGE_MASK;
                        if (!Ptab::insert_mapping(virt, phys, Ptab::RW | Ptab::PRESENT | Ptab::USER)) {
                            success = false;
                            printf("1\n");
                            break;
                        }
                        memset(reinterpret_cast<void*>(virt), 0x0, ((virt & 0xFFFFF000) + PAGE_SIZE) - virt);
                    } else {
                        void *page = Kalloc::allocator.alloc_page(1, Kalloc::FILL_0);
                        if (!page) {
                            success = false;
                            printf("2\n");
                            break;
                        }
                        mword phys = Kalloc::virt2phys(page);
                        if (!Ptab::insert_mapping(virt, phys, Ptab::RW | Ptab::PRESENT | Ptab::USER)) {
                            Kalloc::allocator.free_page(page);
                            success = false;
                            printf("3\n");
                            break;
                        }
                        memset(reinterpret_cast<void*>(virt), 0x0, PAGE_SIZE);
                    }
                    virt = (virt & 0xFFFFF000) + PAGE_SIZE;
                }
                while (!success && virt >= break_current) {
                    virt = (virt & 0xFFFFF000) - PAGE_SIZE;
                    if (virt >= break_current) {
                        mword phys = Ptab::get_mapping(virt) & ~PAGE_MASK;
                        if (!Ptab::insert_mapping(virt, 0, 0)) {
                            printf("4\n");
                            break;
                        }
                        Kalloc::allocator.free_page(reinterpret_cast<void*>(Kalloc::phys2virt(phys)));
                    }
                }
            }
            else if (new_break < break_current) {
                while (virt >= new_break) {
                    if (virt == break_current && (virt & 0xFFF) == 0) {
                        virt -= PAGE_SIZE;
                        continue;
                    }
                    virt = virt & 0xFFFFF000;
                    if (virt >= new_break) {
                        mword phys = Ptab::get_mapping(virt) & ~PAGE_MASK;
                        if (!Ptab::insert_mapping(virt, 0, 0)) {
                            success = false;
                            printf("5\n");
                            break;
                        }
                        Kalloc::allocator.free_page(reinterpret_cast<void*>(Kalloc::phys2virt(phys)));
                    }
                    virt -= PAGE_SIZE;
                }
            }
            if (success) {
                r->eax = break_current;
                break_current = new_break;
            } else {
                r->eax = 0;
            }
            break;
        }
        default:
            printf ("unknown syscall %d\n", number);
            break;
    }
    ret_user_sysexit();
}
