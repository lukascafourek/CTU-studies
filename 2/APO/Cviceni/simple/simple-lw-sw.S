.globl _start
.option norelax

.text

_start:
loop:
	// load the word from absolute address
	lw     x2, 0x400(x0)
	// store the word to absolute address
	sw     x2, 0x404(x0)
	// stop execution wait for debugger/user
	// ebreak

	// ensure that continuation does not
	// interpret random data
	beq    x0, x0, loop
	nop
	nop
	ebreak

.data
.org 0x400

src_val:
	.word  0x12345678
dst_val:
	.word  0
