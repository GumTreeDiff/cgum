TARGET=cgumtester

$(TARGET): tester.ml
	ocamlc -o $@ unix.cma str.cma tester.ml

tehuti: $(TARGET)
	./$(TARGET) /var/linuxes/linux-next drivers/net/ethernet/tehuti/tehuti.c \
	tehuti

f_acm: $(TARGET)
	./$(TARGET) /var/linuxes/linux-next drivers/usb/gadget/f_acm.c \
	f_acm

radio-cadet: $(TARGET)
	./$(TARGET) /var/linuxes/linux-next drivers/media/radio/radio-cadet.c \
	radio-cadet
