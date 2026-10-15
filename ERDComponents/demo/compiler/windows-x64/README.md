# Windows TSVM compiler for the demo

`erd-compiler.exe` is a Windows x64 console executable. It can serve either a Win32 or Win64 demo on 64-bit Windows. No Node.js or separate MinGW runtime installation is required.

Rebuild the Delphi packages and demo, open the Code page, and click **Connect TSVM...**. The demo searches this directory beside its executable and up to three parent directories, covering the standard `demo/Win32/Release` output. If no bundled compiler is found, it opens a file picker.

Completion uses **Ctrl+J** (or Ctrl+Space). Parameter hints appear on `(` and `,`; **Ctrl+Shift+Space** requests one manually, **Alt+Up/Down** switches overloads, and **Esc** closes it.

The binary includes the LSP endpoint and embedded compiler bytecode. This compiler-focused profile excludes TLS, AI, USB and canvas backends. Windows raw NDB TCP transport and iconv-dependent legacy CJK decoding are unavailable. This is a demo compiler build, not a complete device runtime distribution.

Source and checksum are recorded in `manifest.json`. Source branch: `fix/windows-compiler-demo` in ERD-TSVM. To reproduce with MinGW-w64 installed and TSVM's documented bytecode-generation prerequisites available:

```sh
cmake -S erdtsvm -B erdtsvm/build/windows-compiler-demo \
  -C toolchain/cmake/windows-compiler-demo.cmake \
  -DCMAKE_TOOLCHAIN_FILE="$PWD/toolchain/cmake/windows-x64.cmake" \
  -DPLATFORM=desktop -DERD_CANVAS_BACKEND=none \
  -DCMAKE_BUILD_TYPE=Release -DCMAKE_CXX_FLAGS_RELEASE='-O2 -DNDEBUG'
cmake --build erdtsvm/build/windows-compiler-demo --target erd-compiler -j4
x86_64-w64-mingw32-strip erdtsvm/build/windows-compiler-demo/erd-compiler.exe
```

MinGW cross-compilation and executable-level LSP tests under Wine passed, including Unicode framing, completion, diagnostics, hover, definitions, signature help and shutdown. Delphi/VCL compilation and real Windows execution remain for Windows testing. The executable is unsigned.
