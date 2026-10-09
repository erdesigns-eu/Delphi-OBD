# Run from a RAD Studio command prompt; actual compilation requires Delphi.
[CmdletBinding()]
param(
    [ValidateSet('Win32', 'Win64')][string]$Platform = 'Win32',
    [ValidateSet('Debug', 'Release')][string]$Config = 'Release'
)
$ErrorActionPreference = 'Stop'
$repoRoot = Split-Path -Parent $PSScriptRoot

function Invoke-CheckedNative {
    param([string]$Program, [string[]]$Arguments)
    & $Program @Arguments
    if ($LASTEXITCODE -ne 0) { throw "$Program failed with exit code $LASTEXITCODE" }
}

Push-Location $repoRoot
try {
    if (-not $env:BDS -or -not (Test-Path "$env:BDS\Bin\CodeGear.Delphi.Targets")) {
        throw 'Initialize the RAD Studio environment using rsvars.bat before running this script.'
    }
    if (-not $env:DUNITX_SOURCE -or -not (Test-Path "$env:DUNITX_SOURCE\DUnitX.TestFramework.pas")) {
        throw 'Set DUNITX_SOURCE to the DUnitX Source directory.'
    }
    $null = Get-Command msbuild -ErrorAction Stop
    Invoke-CheckedNative -Program 'msbuild' -Arguments @(
        'packages\DelphiOBD_RT.dproj', '/t:Rebuild', "/p:Config=$Config", "/p:Platform=$Platform")
    if ($Platform -eq 'Win32') {
        Invoke-CheckedNative -Program 'msbuild' -Arguments @(
            'packages\DelphiOBD_DT.dproj', '/t:Rebuild', "/p:Config=$Config", '/p:Platform=Win32')
    } else {
        Write-Host 'IDE package is tested in the separate Win32 run; Win64 builds runtime and tests.'
    }
    Invoke-CheckedNative -Program 'msbuild' -Arguments @(
        'tests\DelphiOBD_Tests.dproj', '/t:Rebuild', "/p:Config=$Config", "/p:Platform=$Platform")
    $resultFile = "tests\nunit-$Platform-$Config.xml"
    if (Test-Path $resultFile) { Remove-Item $resultFile }
    Invoke-CheckedNative -Program ".\tests\$Platform\$Config\DelphiOBD_Tests.exe" -Arguments @(
        "--xmloutput=$resultFile")
    if (-not (Test-Path $resultFile)) { throw 'DUnitX did not produce its XML result file.' }
    [xml]$results = Get-Content $resultFile
    if ([int]$results.DocumentElement.GetAttribute('total') -le 0) {
        throw 'DUnitX reported no tests; this is not a successful validation.'
    }
    Write-Host "Delphi $Platform $Config build and DUnitX completed. Results: $resultFile"
} catch {
    Write-Error $_
    exit 1
} finally {
    Pop-Location
}
