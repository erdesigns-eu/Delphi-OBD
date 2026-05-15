//------------------------------------------------------------------------------
//  OBD.Service.VINInspector
//
//  TOBDVINInspector - thin non-visual component over the
//  TOBDVINDecoder static class. Drop one on a form / data
//  module, set DumpFile or set VIN directly, and read the
//  decoded breakdown from <c>Info</c> (or any of the
//  shorthand properties: WMI, Country, Manufacturer,
//  ModelYear, ...).
//
//  This is the offline-decode side of VIN. For reading the
//  VIN over OBD-II / UDS from a vehicle, see
//  <see cref="OBD.Service.VIN"/>'s <c>TOBDVIN</c> - that one
//  needs a connected protocol; this one is pure string-in,
//  details-out.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT - see LICENSE
//
//  History     :
//    2026-05-10  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.Service.VINInspector;

interface

uses
  System.SysUtils,
  System.Classes,
  Data.Bind.Components,
  OBD.Service.VINDecoder,
  OBD.Service.VINDecoder.Types;

type
  /// <summary>Fired after a successful decode (Info.Valid = True).</summary>
  TOBDVINInspectorDecodedEvent = procedure(Sender: TObject;
    const AInfo: TOBDVINInfo) of object;

  /// <summary>Fired when the input couldn't be decoded.
  /// <c>AReason</c> is the same string that lands in
  /// <c>Info.InvalidReason</c>.</summary>
  TOBDVINInspectorInvalidEvent = procedure(Sender: TObject;
    const AReason: string) of object;

  /// <summary>Non-visual component that wraps
  /// <see cref="TOBDVINDecoder"/>. Set <see cref="VIN"/> (or
  /// call <see cref="DecodeNow"/>); read the decoded data via
  /// <see cref="Info"/> or the per-field shortcut properties.
  /// </summary>
  TOBDVINInspector = class(TComponent)
  strict private
    FVIN:        string;
    FAutoDecode: Boolean;
    FCatalogDir: string;
    FInfo:       TOBDVINInfo;
    FOnDecoded:  TOBDVINInspectorDecodedEvent;
    FOnInvalid:  TOBDVINInspectorInvalidEvent;
    procedure SetVIN(const AValue: string);
    procedure SetCatalogDir(const AValue: string);
    function  GetValid:              Boolean;
    function  GetWMI:                string;
    function  GetVDS:                string;
    function  GetVIS:                string;
    function  GetRegionName:         string;
    function  GetCountryName:        string;
    function  GetManufacturerName:   string;
    function  GetCheckDigit:         Char;
    function  GetCheckDigitValid:    Boolean;
    function  GetModelYear:          Word;
    function  GetYearCode:           Char;
    function  GetPlantName:          string;
    function  GetPlantCity:          string;
    function  GetPlantCountry:       string;
    function  GetSerial:             string;
    function  GetVehicleTypeText:    string;
    function  GetEngineDisplacement: string;
    function  GetEngineType:         string;
    function  GetBodyStyle:          string;
    function  GetDriveType:          string;
    function  GetTransmission:       string;
    function  GetRestraintSystem:    string;
    function  GetIsCommercial:       Boolean;
  public
    constructor Create(AOwner: TComponent); override;

    /// <summary>Forces a decode against <see cref="VIN"/> using
    /// the catalogues at <see cref="CatalogDir"/> (when set) or
    /// the global default. Updates <see cref="Info"/>, fires
    /// <see cref="OnDecoded"/> on success or
    /// <see cref="OnInvalid"/> on failure. Returns the decoded
    /// record so callers can bind in one expression.</summary>
    function DecodeNow: TOBDVINInfo;

    /// <summary>Last decoded result. Default-initialised
    /// (<c>Valid</c> = False) before the first decode.</summary>
    property Info: TOBDVINInfo read FInfo;

    /// <summary>Shortcut for <c>Info.Valid</c>.</summary>
    property Valid:              Boolean read GetValid;

    /// <summary>Shortcut for <c>Info.WMI</c>.</summary>
    property WMI:                string  read GetWMI;
    /// <summary>Shortcut for <c>Info.VDS</c>.</summary>
    property VDS:                string  read GetVDS;
    /// <summary>Shortcut for <c>Info.VIS</c>.</summary>
    property VIS:                string  read GetVIS;

    /// <summary>Shortcut for <c>Info.Region.Name</c>.</summary>
    property RegionName:         string  read GetRegionName;
    /// <summary>Shortcut for <c>Info.Country.Name</c>.</summary>
    property CountryName:        string  read GetCountryName;
    /// <summary>Shortcut for <c>Info.Manufacturer.Name</c>.</summary>
    property ManufacturerName:   string  read GetManufacturerName;

    /// <summary>Shortcut for <c>Info.CheckDigit</c>.</summary>
    property CheckDigit:         Char    read GetCheckDigit;
    /// <summary>Shortcut for <c>Info.CheckDigitValid</c>.</summary>
    property CheckDigitValid:    Boolean read GetCheckDigitValid;

    /// <summary>Shortcut for <c>Info.ModelYear</c>.</summary>
    property ModelYear:          Word    read GetModelYear;
    /// <summary>Shortcut for <c>Info.YearCode</c>.</summary>
    property YearCode:           Char    read GetYearCode;

    /// <summary>Shortcut for <c>Info.Plant.Name</c>.</summary>
    property PlantName:          string  read GetPlantName;
    /// <summary>Shortcut for <c>Info.Plant.City</c>.</summary>
    property PlantCity:          string  read GetPlantCity;
    /// <summary>Shortcut for <c>Info.Plant.Country</c>.</summary>
    property PlantCountry:       string  read GetPlantCountry;

    /// <summary>Shortcut for <c>Info.Serial</c>.</summary>
    property Serial:             string  read GetSerial;

    /// <summary>String form of <c>Info.Features.VehicleType</c>
    /// (e.g. "PassengerCar", "Truck", "SUV") for direct
    /// data-binding into a UI label.</summary>
    property VehicleTypeText:    string  read GetVehicleTypeText;
    /// <summary>Shortcut for <c>Info.Features.EngineDisplacement</c>.</summary>
    property EngineDisplacement: string  read GetEngineDisplacement;
    /// <summary>Shortcut for <c>Info.Features.EngineType</c>.</summary>
    property EngineType:         string  read GetEngineType;
    /// <summary>Shortcut for <c>Info.Features.BodyStyle</c>.</summary>
    property BodyStyle:          string  read GetBodyStyle;
    /// <summary>Shortcut for <c>Info.Features.DriveType</c>.</summary>
    property DriveType:          string  read GetDriveType;
    /// <summary>Shortcut for <c>Info.Features.Transmission</c>.</summary>
    property Transmission:       string  read GetTransmission;
    /// <summary>Shortcut for <c>Info.Features.RestraintSystem</c>.</summary>
    property RestraintSystem:    string  read GetRestraintSystem;
    /// <summary>Shortcut for <c>Info.Features.IsCommercial</c>.</summary>
    property IsCommercial:       Boolean read GetIsCommercial;
  published
    /// <summary>VIN to decode. Setting this re-decodes when
    /// <see cref="AutoDecode"/> is True (the default).</summary>
    property VIN: string read FVIN write SetVIN;

    /// <summary>When True (default), assigning <see cref="VIN"/>
    /// triggers an immediate <see cref="DecodeNow"/>. Set False
    /// to batch updates and call <see cref="DecodeNow"/>
    /// manually.</summary>
    property AutoDecode: Boolean
      read FAutoDecode write FAutoDecode default True;

    /// <summary>Override the catalogue directory just for this
    /// component. Empty (default) = use the global
    /// <see cref="TOBDVINDecoder.CatalogDir"/>.</summary>
    property CatalogDir: string
      read FCatalogDir write SetCatalogDir;

    /// <summary>Fired after a successful decode.</summary>
    property OnDecoded: TOBDVINInspectorDecodedEvent
      read FOnDecoded write FOnDecoded;

    /// <summary>Fired when the input couldn't be decoded
    /// (<c>Info.Valid</c> = False).</summary>
    property OnInvalid: TOBDVINInspectorInvalidEvent
      read FOnInvalid write FOnInvalid;
  end;

implementation

{ TOBDVINInspector ------------------------------------------------------------}

constructor TOBDVINInspector.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAutoDecode := True;
end;

procedure TOBDVINInspector.SetVIN(const AValue: string);
begin
  if FVIN = AValue then Exit;
  FVIN := AValue;
  // Skip the auto-decode at design-time too — the IDE would
  // otherwise hit the catalog files every time the property is
  // touched in the Object Inspector.
  if FAutoDecode and
     not (csLoading   in ComponentState) and
     not (csDesigning in ComponentState) then
    DecodeNow;
end;

procedure TOBDVINInspector.SetCatalogDir(const AValue: string);
begin
  FCatalogDir := AValue;
  // Push to the global decoder lazily on next decode.
end;

function TOBDVINInspector.GetValid:              Boolean; begin Result := FInfo.Valid;                       end;
function TOBDVINInspector.GetWMI:                string;  begin Result := FInfo.WMI;                         end;
function TOBDVINInspector.GetVDS:                string;  begin Result := FInfo.VDS;                         end;
function TOBDVINInspector.GetVIS:                string;  begin Result := FInfo.VIS;                         end;
function TOBDVINInspector.GetRegionName:         string;  begin Result := FInfo.Region.Name;                 end;
function TOBDVINInspector.GetCountryName:        string;  begin Result := FInfo.Country.Name;                end;
function TOBDVINInspector.GetManufacturerName:   string;  begin Result := FInfo.Manufacturer.Name;           end;
function TOBDVINInspector.GetCheckDigit:         Char;    begin Result := FInfo.CheckDigit;                  end;
function TOBDVINInspector.GetCheckDigitValid:    Boolean; begin Result := FInfo.CheckDigitValid;             end;
function TOBDVINInspector.GetModelYear:          Word;    begin Result := FInfo.ModelYear;                   end;
function TOBDVINInspector.GetYearCode:           Char;    begin Result := FInfo.YearCode;                    end;
function TOBDVINInspector.GetPlantName:          string;  begin Result := FInfo.Plant.Name;                  end;
function TOBDVINInspector.GetPlantCity:          string;  begin Result := FInfo.Plant.City;                  end;
function TOBDVINInspector.GetPlantCountry:       string;  begin Result := FInfo.Plant.Country;               end;
function TOBDVINInspector.GetSerial:             string;  begin Result := FInfo.Serial;                      end;
function TOBDVINInspector.GetEngineDisplacement: string;  begin Result := FInfo.Features.EngineDisplacement; end;
function TOBDVINInspector.GetEngineType:         string;  begin Result := FInfo.Features.EngineType;         end;
function TOBDVINInspector.GetBodyStyle:          string;  begin Result := FInfo.Features.BodyStyle;          end;
function TOBDVINInspector.GetDriveType:          string;  begin Result := FInfo.Features.DriveType;          end;
function TOBDVINInspector.GetTransmission:       string;  begin Result := FInfo.Features.Transmission;       end;
function TOBDVINInspector.GetRestraintSystem:    string;  begin Result := FInfo.Features.RestraintSystem;    end;
function TOBDVINInspector.GetIsCommercial:       Boolean; begin Result := FInfo.Features.IsCommercial;       end;

function TOBDVINInspector.GetVehicleTypeText: string;
begin
  case FInfo.Features.VehicleType of
    vtPassengerCar: Result := 'PassengerCar';
    vtTruck:        Result := 'Truck';
    vtSUV:          Result := 'SUV';
    vtVan:          Result := 'Van';
    vtBus:          Result := 'Bus';
    vtMotorcycle:   Result := 'Motorcycle';
    vtElectric:     Result := 'Electric';
    vtHybrid:       Result := 'Hybrid';
  else              Result := '';
  end;
end;

function TOBDVINInspector.DecodeNow: TOBDVINInfo;
begin
  if FCatalogDir <> '' then
    TOBDVINDecoder.LoadCatalogs(FCatalogDir);
  FInfo := TOBDVINDecoder.Decode(FVIN);
  // LiveBindings refresh — fires before the event handlers so a
  // bound TLinkPropertyToField sees the new FInfo even if the
  // host handler shows a dialog (which would block the refresh
  // until dismissed).
  try
    TBindings.Notify(Self, '');
  except
  end;
  if FInfo.Valid then
  begin
    if Assigned(FOnDecoded) then FOnDecoded(Self, FInfo);
  end
  else
    if Assigned(FOnInvalid) then
      FOnInvalid(Self, FInfo.InvalidReason);
  Result := FInfo;
end;

end.
