(* # generated by ./configure --prefix /Users/jeremy/llama-install *)
let prefix = "/Users/jeremy/llama-install";;
let bindir = prefix^"/bin";;
let libdir = prefix^"/lib/ocaml";;
let stublibdir = libdir^"/stublibs";;
let mandir = prefix^"/man";;
let manext = "1";;
let ranlib = "ranlib";;
let ranlibcmd = "ranlib";;
let sharpbangscripts = true;;
let bng_arch = "amd64";;
let bng_asm_level = "1";;
let pthread_link = "-cclib -lpthread";;
let x11_includes = "-I/usr/X11R6/include";;
let x11_link = "-L/usr/X11R6/lib -lX11";;
let dbm_includes = "";;
let dbm_link = "";;
let tk_defs = " "^x11_includes;;
let tk_link = " -ltk8.5 -ltcl8.5  "^x11_link;;
let libbfd_link = "";;
let bytecc = "gcc";;
let bytecccompopts = " -fno-defer-pop -no-cpp-precomp -Wall -D_FILE_OFFSET_BITS=64 -D_REENTRANT";;
let bytecclinkopts = "";;
let bytecclibs = "   -lcurses -lpthread";;
let byteccrpath = "";;
let exe = "";;
let supports_shared_libraries = true;;
let sharedcccompopts = "";;
let mksharedlibrpath = "";;
let natdynlinkopts = "";;
(* SYSLIB=-l"^1^" *)
let syslib x = "-l"^x;;

(* ## *)
(* MKLIB=ar rc "^1^" "^2^"; ranlib "^1^" *)
let mklib out files opts = Printf.sprintf "ar rc %s %s %s; ranlib %s" out opts files out;;
let arch = "amd64";;
let model = "default";;
let system = "macosx";;
let nativecc = "gcc";;
let nativecccompopts = " -D_FILE_OFFSET_BITS=64 -D_REENTRANT";;
let nativeccprofopts = " -D_FILE_OFFSET_BITS=64 -D_REENTRANT";;
let nativecclinkopts = "";;
let nativeccrpath = "";;
let nativecclibs = "  ";;
let asm = "as -arch x86_64";;
let aspp = "gcc -arch x86_64 -c";;
let asppprofflags = "-DPROFILING";;
let profiling = "prof";;
let dynlinkopts = "";;
let otherlibraries = "unix num dynlink bigarray systhreads threads graph dbm labltk";;
let debugger = "ocamldebugger";;
let cc_profile = "-pg";;
let systhread_support = true;;
let partialld = "ld -r -arch x86_64";;
let packld = partialld^" "^nativecclinkopts^" -o\ ";;
let dllcccompopts = "";;

let o = "o";;
let a = "a";;
let so = "so";;
let ext_obj = ".o";;
let ext_asm = ".s";;
let ext_lib = ".a";;
let ext_dll = ".so";;
let extralibs = "";;
let ccomptype = "cc";;
let toolchain = "cc";;
let natdynlink = true;;
let cmxs = "cmxs";;
let mkexe = bytecc;;
let mkdll = "gcc -bundle -flat_namespace -undefined suppress";;
let mkmaindll = "gcc -bundle -flat_namespace -undefined suppress";;
