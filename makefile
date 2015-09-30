PETSC_DIR_ARCH = ${PETSC_DIR}/arch-darwin-c-debug

SLEPC_DIR_ARCH = ${SLEPC_DIR}/arch-darwin-c-debug
SLEPC_DIR_ARCH_INCLUDE = ${SLEPC_DIR_ARCH}/include

SRCDIR = ${CURDIR}/src/Numerical/SLEPc
SRCPARDIR = ${CURDIR}/src/Numerical
CBITS = ${CURDIR}/src/cbits
TESTDIR = ${CURDIR}/test
LIBDIR = ${CURDIR}/lib


main:
	make step1
	make step2a
	make step2b
	make step3

step1:
	ghc  ${SRCDIR}/Raw/Internal.hs ${SRCDIR}/Raw/InlineC.hs -isrc/

step2a:
	cc -c ${SRCDIR}/Raw/Internal.c -o ${LIBDIR}/Internal_c.o -I${SLEPC_DIR_ARCH}/include -I${SLEPC_DIR}/include 

step2b:
	cc -c ${SRCDIR}/Raw/InlineC.c -o ${LIBDIR}/InlineC_c.o -I${SLEPC_DIR_ARCH}/include -I${SLEPC_DIR}/include -I${PETSC_DIR}/include -I${PETSC_DIR_ARCH}/include

step3:
	ghci ${SRCDIR}/TestMain.hs ${SRCDIR}/Raw/InlineC.hs  ${LIBDIR}/InlineC_c.o  ${LIBDIR}/Internal_c.o -isrc/ -L${SLEPC_DIR_ARCH}/lib -L${PETSC_DIR_ARCH}/lib -lslepc -lmpich



