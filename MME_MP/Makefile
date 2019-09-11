# Makefile for compiling TMB models

all: tunaDelay.so
	# Comment here preserves the prior tab
tunaDelay.so: tunaDelay.cpp
	Rscript --vanilla -e "TMB::compile('tunaDelay.cpp','-O0 -g')"
clean:
	rm -rf *o
