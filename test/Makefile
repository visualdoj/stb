default : all;

ifeq ($(OS),Windows_NT)
  EXEEXT := .exe
else
  EXEEXT :=
endif

TESTS := test_sprintf

test_%$(EXEEXT) : test_%.pas ../stb_%.pas
	fpc -gl $< -FE. -FUtemp -o$@

TESTS_EXECUTABLES := $(addsuffix $(EXEEXT),$(TESTS))
all : $(TESTS_EXECUTABLES);

CLEANUP_FILES := $(wildcard temp/*.ppu) \
	             $(wildcard temp/*.o) \
                 $(TESTS_EXECUTABLES)
ifeq ($(OS),Windows_NT)
clean :
	cd temp && del /Q /F * || echo
	del /Q /F $(TESTS_EXECUTABLES)
else
clean :
	rm -f temp/*.ppu temp/*.o $(TESTS_EXECUTABLES)
endif
