#!/bin/sh
ocamlbuild -Is calculus,datastructure,interfaces,parser-hotptp,toplevel toplevel/leo.native
cp -L leo.native ../bin/leo
rm leo.native

