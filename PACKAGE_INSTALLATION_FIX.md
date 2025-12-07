# Package Installation Fix

## Problem
When attempting to install the DesignTime package, users encountered the error:
**"Kan opgegeven module niet vinden"** (Dutch for "Cannot find specified module")

This error occurred because the Skia DLL (`libskia.dll`) required by Skia4Delphi was not accessible when the package was being loaded.

## Root Cause
The packages were configured to output BPL files to non-standard locations:
- **DesignTime.dpk**: No explicit BPL output path configured, defaulting to project directory
- **RunTime.dpk**: Hardcoded path `Z:\Projects\Delphi\OBD\Delphi-OBD\Packages\`

When installed in these locations, the Delphi IDE couldn't find the Skia DLL because:
1. The Skia4Delphi packages and their DLLs are installed in the standard Delphi BPL directory
2. The OBD packages were in a different location
3. Windows DLL search path didn't include the Skia DLL location

## Solution
Both packages have been reconfigured to output their BPL files to Delphi's standard package directory using the MSBuild variable `$(BDSCOMMONDIR)\Bpl`.

### Changes Made

#### DesignTime.dproj
- Added `<DCC_BplOutput>$(BDSCOMMONDIR)\Bpl</DCC_BplOutput>` to:
  - Base configuration (applies to all platforms)
  - Base_Win32 configuration
  - Base_Win64 configuration (outputs to `\Bpl\Win64` subdirectory)
  - Base_Win64x configuration (outputs to `\Bpl\Win64x` subdirectory)
  - Cfg_1_Win32 configuration (Debug build)

#### RunTime.dproj
- Changed `<DCC_BplOutput>` from hardcoded `Z:\Projects\Delphi\OBD\Delphi-OBD\Packages\` to `$(BDSCOMMONDIR)\Bpl` in:
  - Base configuration
  - Base_Win32 configuration
  - Base_Win64x configuration (outputs to `\Bpl\Win64x` subdirectory)
  - Cfg_1_Win32 configuration (Debug build)

### What `$(BDSCOMMONDIR)\Bpl` Resolves To
This MSBuild variable typically resolves to:
```
C:\Users\Public\Documents\Embarcadero\Studio\[Version]\Bpl
```

For example:
- Delphi 11: `C:\Users\Public\Documents\Embarcadero\Studio\22.0\Bpl`
- Delphi 12: `C:\Users\Public\Documents\Embarcadero\Studio\23.0\Bpl`

## Benefits
1. **Packages install in the standard location** where Delphi IDE expects them
2. **Skia DLL is accessible** because Skia4Delphi packages are in the same directory
3. **No manual DLL copying** required
4. **Platform-independent** configuration using MSBuild variables
5. **Consistent with Delphi best practices**
6. **Works across different Delphi versions** automatically

## Installation Instructions
1. **Install Skia4Delphi** via GetIt Package Manager (required dependency)
2. **Compile and install RunTime.dpk** first
3. **Compile and install DesignTime.dpk** second
4. The packages will automatically output to the correct directory
5. Restart Delphi IDE if components don't appear in Tool Palette

## Verification
After installation, you should see the BPL files in:
```
C:\Users\Public\Documents\Embarcadero\Studio\[YourVersion]\Bpl\
├── RunTime.bpl
├── DesignTime.bpl
├── Skia.Package.RTL.bpl    (from Skia4Delphi)
├── Skia.Package.VCL.bpl    (from Skia4Delphi)
└── libskia.dll             (from Skia4Delphi)
```

## Troubleshooting
If you still encounter issues after this fix:
1. Verify Skia4Delphi is properly installed via GetIt Package Manager
2. Check that all BPL files are in the same directory
3. Ensure `libskia.dll` is present in the BPL directory
4. Try uninstalling and reinstalling both packages
5. Restart Delphi IDE

## Technical Notes
- The fix does not change any source code, only project configuration
- Both 32-bit (Win32) and 64-bit ARM (Win64x) platforms are supported
- The Win64 configuration is included for future compatibility
- The configuration uses MSBuild variables for portability
