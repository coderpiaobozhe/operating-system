BUILD:= ./build
SRC:= ./srcfile
SPEC:= build/kernel.bin build/system.bin
ENTRY:=0X10000
$(filter-out $(SPEC), $(BUILD)/%.bin):$(SRC)/bootloader/%.asm
	nasm $< -o $@
$(BUILD)/%.o:$(SRC)/kernel/%.asm
	nasm -f elf32 $< -o $@
$(BUILD)/kernel.bin:$(BUILD)/start.o
	ld -m elf_i386 -static $^ -o $@ -Ttext $(ENTRY)
$(BUILD)/system.bin:$(BUILD)/kernel.bin
	objcopy -O binary $< $@
$(BUILD)/system.map:$(BUILD)/kernel.bin
	nm $< | sort > $@
$(BUILD)/master.img:$(BUILD)/boot.bin \
	$(BUILD)/loader.bin \
	$(BUILD)/system.bin \
	$(BUILD)/system.map
ifeq ("$(wildcard $(BUILD)/master.img)","")
	bximage -q -hd=16 -mode=create -sectsize=512 -imgmode=flat $@
endif
	dd if=$(BUILD)/boot.bin of=$@ bs=512 count=1 conv=notrunc
	dd if=$(BUILD)/loader.bin of=$@ bs=512 count=4 seek=2 conv=notrunc
	dd if=$(BUILD)/system.bin of=$@ bs=512 count=200 seek=10 conv=notrunc
.PHONY:bochs
bochs:$(BUILD)/master.img
	rm -rf bx_enh_dbg.ini
	bochs -q
test:$(BUILD)/master.img
.PHONY:clean
clean:
	rm -rf $(BUILD)/*
	rm bx_enh_dbg.ini