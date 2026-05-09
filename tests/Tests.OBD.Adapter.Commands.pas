//------------------------------------------------------------------------------
//  Tests.OBD.Adapter.Commands
//
//  DUnitX coverage for FormatCommand and TOBDAdapterCommandCatalog.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Phase 3 initial.
//------------------------------------------------------------------------------

unit Tests.OBD.Adapter.Commands;

interface

uses
  DUnitX.TestFramework;

type
  /// <summary>FormatCommand placeholder coverage.</summary>
  [TestFixture]
  TFormatCommandTests = class
  public
    /// <summary>%d substitutes a decimal integer.</summary>
    [Test] procedure DecimalIntegerPlaceholder;
    /// <summary>%xx substitutes two hex digits, zero-padded, lower-case.</summary>
    [Test] procedure TwoHexDigitsLowerCase;
    /// <summary>%XX substitutes two hex digits, zero-padded, upper-case.</summary>
    [Test] procedure TwoHexDigitsUpperCase;
    /// <summary>%xxxx substitutes four hex digits.</summary>
    [Test] procedure FourHexDigits;
    /// <summary>%s substitutes a string.</summary>
    [Test] procedure StringPlaceholder;
    /// <summary>Multiple placeholders substitute in order.</summary>
    [Test] procedure MultiplePlaceholders;
    /// <summary>Too few parameters raises EOBDConfig.</summary>
    [Test] procedure TooFewParamsRaises;
  end;

  /// <summary>Catalogue lookup coverage.</summary>
  [TestFixture]
  TCommandCatalogTests = class
  public
    /// <summary>Built-in ATZ is registered.</summary>
    [Test] procedure ATZRegistered;
    /// <summary>Lookup is case-insensitive.</summary>
    [Test] procedure LookupCaseInsensitive;
    /// <summary>ST commands carry acSTCommands as a required
    /// capability.</summary>
    [Test] procedure STCommandsRequireCapability;
    /// <summary>CommandSupportedBy returns missing capability name.</summary>
    [Test] procedure CommandSupportedByReportsMissingCap;
    /// <summary>Empty verb on Register raises.</summary>
    [Test] procedure RegisterEmptyVerbRaises;
  end;

implementation

uses
  System.SysUtils,
  OBD.Types,
  OBD.Adapter.Types,
  OBD.Adapter.Commands;

procedure TFormatCommandTests.DecimalIntegerPlaceholder;
begin
  Assert.AreEqual('AT SP 6', FormatCommand('AT SP %d', [6]));
end;

procedure TFormatCommandTests.TwoHexDigitsLowerCase;
begin
  Assert.AreEqual('ATPP 0a OFF', FormatCommand('ATPP %xx OFF', [10]));
end;

procedure TFormatCommandTests.TwoHexDigitsUpperCase;
begin
  Assert.AreEqual('ATST 7F', FormatCommand('ATST %XX', [$7F]));
end;

procedure TFormatCommandTests.FourHexDigits;
begin
  Assert.AreEqual('ATCRA 07E8', FormatCommand('ATCRA %XXXX', [$7E8]));
end;

procedure TFormatCommandTests.StringPlaceholder;
begin
  Assert.AreEqual('hello world',
    FormatCommand('%s %s', ['hello', 'world']));
end;

procedure TFormatCommandTests.MultiplePlaceholders;
begin
  Assert.AreEqual('STPP 0a SV 7F',
    FormatCommand('STPP %xx SV %XX', [10, $7F]));
end;

procedure TFormatCommandTests.TooFewParamsRaises;
begin
  Assert.WillRaise(
    procedure
    begin
      FormatCommand('AT SP %d', []);
    end,
    EOBDConfig);
end;

procedure TCommandCatalogTests.ATZRegistered;
var
  Cmd: TOBDAdapterCommand;
begin
  Assert.IsTrue(TOBDAdapterCommandCatalog.Default.TryFind('ATZ', Cmd));
  Assert.AreEqual(Ord(ckAT), Ord(Cmd.Kind));
  Assert.AreEqual('ATZ', Cmd.Verb);
end;

procedure TCommandCatalogTests.LookupCaseInsensitive;
var
  Cmd: TOBDAdapterCommand;
begin
  Assert.IsTrue(TOBDAdapterCommandCatalog.Default.TryFind('atz', Cmd));
  Assert.IsTrue(TOBDAdapterCommandCatalog.Default.TryFind(' ATZ ', Cmd));
end;

procedure TCommandCatalogTests.STCommandsRequireCapability;
var
  Cmd: TOBDAdapterCommand;
begin
  Assert.IsTrue(TOBDAdapterCommandCatalog.Default.TryFind('STI', Cmd));
  Assert.IsTrue(acSTCommands in Cmd.RequiredCapabilities);
end;

procedure TCommandCatalogTests.CommandSupportedByReportsMissingCap;
var
  Cmd: TOBDAdapterCommand;
  Missing: string;
begin
  TOBDAdapterCommandCatalog.Default.TryFind('STI', Cmd);
  Assert.IsFalse(CommandSupportedBy(Cmd, [], Missing));
  Assert.IsNotEmpty(Missing);
  // Should mention 'STCommands' (the capability name without the 'ac' prefix).
  Assert.IsTrue(Pos('ST', Missing) > 0);
end;

procedure TCommandCatalogTests.RegisterEmptyVerbRaises;
var
  Cmd: TOBDAdapterCommand;
begin
  Cmd.Kind := ckAT;
  Cmd.Verb := '';
  Cmd.Description := '';
  Cmd.RequiredCapabilities := [];
  Assert.WillRaise(
    procedure
    begin
      TOBDAdapterCommandCatalog.Default.Register(Cmd);
    end,
    EOBDConfig);
end;

initialization
  TDUnitX.RegisterTestFixture(TFormatCommandTests);
  TDUnitX.RegisterTestFixture(TCommandCatalogTests);

end.
