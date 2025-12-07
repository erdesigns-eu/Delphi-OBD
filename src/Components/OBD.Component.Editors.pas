//------------------------------------------------------------------------------
// UNIT           : OBD.Component.Editors.pas
// CONTENTS       : Design-time property editors for component bindings
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : OpenAI Assistant
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 05/02/2025
//------------------------------------------------------------------------------
unit OBD.Component.Editors;

interface

uses
  System.Classes, System.TypInfo, DesignIntf, DesignEditors,
  OBD.Connection.Component, OBD.Protocol.Component, OBD.CircularGauge;

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Property editor that lists available <c>TOBDConnectionComponent</c>
  ///   instances on the current designer surface.
  /// </summary>
  TOBDConnectionComponentProperty = class(TComponentProperty)
  public
    /// <summary>
    ///   Returns the supported editor attributes, enabling a sorted value list
    ///   in the Object Inspector.
    /// </summary>
    function GetAttributes: TPropertyAttributes; override;
    /// <summary>
    ///   Enumerates compatible connection components for selection.
    /// </summary>
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  /// <summary>
  ///   Property editor that lists available <c>TOBDProtocolComponent</c>
  ///   instances on the designer surface for binding convenience.
  /// </summary>
  TOBDProtocolComponentProperty = class(TComponentProperty)
  public
    /// <summary>
    ///   Returns the supported editor attributes for the protocol component
    ///   lookup.
    /// </summary>
    function GetAttributes: TPropertyAttributes; override;
    /// <summary>
    ///   Enumerates protocol components on the form or data module.
    /// </summary>
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  /// <summary>
  ///   Property editor that lists available <c>TOBDCircularGauge</c> instances
  ///   for controller bindings.
  /// </summary>
  TOBDGaugeComponentProperty = class(TComponentProperty)
  public
    /// <summary>
    ///   Returns the supported editor attributes for gauge lookup.
    /// </summary>
    function GetAttributes: TPropertyAttributes; override;
    /// <summary>
    ///   Enumerates circular gauge controls available on the designer surface.
    /// </summary>
    procedure GetValues(Proc: TGetStrProc); override;
  end;

//------------------------------------------------------------------------------
// PROCEDURES
//------------------------------------------------------------------------------
/// <summary>
///   Register the property editors with the Delphi IDE.
/// </summary>
procedure Register;

implementation

//------------------------------------------------------------------------------
// CONNECTION COMPONENT PROPERTY
//------------------------------------------------------------------------------
function TOBDConnectionComponentProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paValueList, paSortList, paMultiSelect];
end;

procedure TOBDConnectionComponentProperty.GetValues(Proc: TGetStrProc);
begin
  if not Assigned(Designer) then
    Exit;
  Designer.GetComponentNames(GetTypeData(TypeInfo(TOBDConnectionComponent)), Proc);
end;

//------------------------------------------------------------------------------
// PROTOCOL COMPONENT PROPERTY
//------------------------------------------------------------------------------
function TOBDProtocolComponentProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paValueList, paSortList, paMultiSelect];
end;

procedure TOBDProtocolComponentProperty.GetValues(Proc: TGetStrProc);
begin
  if not Assigned(Designer) then
    Exit;
  Designer.GetComponentNames(GetTypeData(TypeInfo(TOBDProtocolComponent)), Proc);
end;

//------------------------------------------------------------------------------
// GAUGE COMPONENT PROPERTY
//------------------------------------------------------------------------------
function TOBDGaugeComponentProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paValueList, paSortList, paMultiSelect];
end;

procedure TOBDGaugeComponentProperty.GetValues(Proc: TGetStrProc);
begin
  if not Assigned(Designer) then
    Exit;
  Designer.GetComponentNames(GetTypeData(TypeInfo(TOBDCircularGauge)), Proc);
end;

//------------------------------------------------------------------------------
// REGISTER
//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TOBDConnectionComponent), nil, 'ConnectionComponent',
    TOBDConnectionComponentProperty);
  RegisterPropertyEditor(TypeInfo(TOBDProtocolComponent), nil, 'ProtocolComponent',
    TOBDProtocolComponentProperty);
  RegisterPropertyEditor(TypeInfo(TOBDCircularGauge), nil, 'Gauge', TOBDGaugeComponentProperty);
end;

end.
