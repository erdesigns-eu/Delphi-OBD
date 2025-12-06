//------------------------------------------------------------------------------
// UNIT           : OBD.CustomControl.Constants.pas
// CONTENTS       : OBD CustomControl Constants
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 02/04/2024
//------------------------------------------------------------------------------
unit OBD.CustomControl.Constants;

interface

uses Vcl.Graphics;

//------------------------------------------------------------------------------
// CONSTANTS: HEADER
//------------------------------------------------------------------------------
const
  /// <summary>
  ///   Default back button caption
  /// </summary>
  DEFAULT_BACK_BUTTON_CAPTION = 'Back';
  /// <summary>
  ///   Default back button width
  /// </summary>
  DEFAULT_BACK_BUTTON_WIDTH = 60;
  /// <summary>
  ///   Default back button border color
  /// </summary>
  DEFAULT_BACK_BUTTON_BORDER_COLOR = $00ACACAC;
  /// <summary>
  ///   Default back button normal color from
  /// </summary>
  DEFAULT_BACK_BUTTON_NORMAL_COLOR_FROM = $00F5F5F5;
  /// <summary>
  ///   Default back button normal color to
  /// </summary>
  DEFAULT_BACK_BUTTON_NORMAL_COLOR_TO = $00C1C1C2;
  /// <summary>
  ///   Default back button hot color from
  /// </summary>
  DEFAULT_BACK_BUTTON_HOT_COLOR_FROM = $00F0F0F0;
  /// <summary>
  ///   Default back button hot color to
  /// </summary>
  DEFAULT_BACK_BUTTON_HOT_COLOR_TO = $00B9B9BB;
  /// <summary>
  ///   Default back button pressed color from
  /// </summary>
  DEFAULT_BACK_BUTTON_PRESSED_COLOR_FROM = $00E5E5E5;
  /// <summary>
  ///   Default back button pressed color to
  /// </summary>
  DEFAULT_BACK_BUTTON_PRESSED_COLOR_TO = $00B0B0B3;
  /// <summary>
  ///   Default back button disabled color from
  /// </summary>
  DEFAULT_BACK_BUTTON_DISABLED_COLOR_FROM = $00E2E2E2;
  /// <summary>
  ///   Default back button disabled color to
  /// </summary>
  DEFAULT_BACK_BUTTON_DISABLED_COLOR_TO = $00BFBFBF;

  /// <summary>
  ///   Default action button width
  /// </summary>
  DEFAULT_ACTION_BUTTON_WIDTH = 80;
  /// <summary>
  ///   Default action button caption
  /// </summary>
  DEFAULT_ACTION_BUTTON_TEXT = 'Connect';
  /// <summary>
  ///   Default action button border color
  /// </summary>
  DEFAULT_ACTION_BUTTON_BORDER_COLOR = $00F27900;
  /// <summary>
  ///   Default action button normal color from
  /// </summary>
  DEFAULT_ACTION_BUTTON_NORMAL_COLOR_FROM = $00EC7600;
  /// <summary>
  ///   Default action button normal color to
  /// </summary>
  DEFAULT_ACTION_BUTTON_NORMAL_COLOR_TO = $00FFA346;
  /// <summary>
  ///   Default action button hot color from
  /// </summary>
  DEFAULT_ACTION_BUTTON_HOT_COLOR_FROM = $00DD6F00;
  /// <summary>
  ///   Default action button hot color to
  /// </summary>
  DEFAULT_ACTION_BUTTON_HOT_COLOR_TO = $00FF870F;
  /// <summary>
  ///   Default v button pressed color from
  /// </summary>
  DEFAULT_ACTION_BUTTON_PRESSED_COLOR_FROM = $009D4F00;
  /// <summary>
  ///   Default action button pressed color to
  /// </summary>
  DEFAULT_ACTION_BUTTON_PRESSED_COLOR_TO = $00E17100;
  /// <summary>
  ///   Default action button disabled color from
  /// </summary>
  DEFAULT_ACTION_BUTTON_DISABLED_COLOR_FROM = $00DF7000;
  /// <summary>
  ///   Default action button disabled color to
  /// </summary>
  DEFAULT_ACTION_BUTTON_DISABLED_COLOR_TO = $00DF7000;

  /// <summary>
  ///   Default caption font size
  /// </summary>
  DEFAULT_CAPTION_FONT_SIZE = 12;

  /// <summary>
  ///   Default tab width
  /// </summary>
  DEFAULT_TAB_WIDTH = 100;

  /// <summary>
  ///   Default battery indicator color (0-25%)
  /// </summary>
  DEFAULT_BATTERY_COLOR_25 = $004864FF;
  /// <summary>
  ///   Default battery indicator color (26-50%)
  /// </summary>
  DEFAULT_BATTERY_COLOR_50 = $0052B1F8;
  /// <summary>
  ///   Default battery indicator color (51-75%)
  /// </summary>
  DEFAULT_BATTERY_COLOR_75 = $000CE076;
  /// <summary>
  ///   Default battery indicator color (75-100%)
  /// </summary>
  DEFAULT_BATTERY_COLOR_100 = $00FF870F;

//------------------------------------------------------------------------------
// CONSTANTS: SUBHEADER
//------------------------------------------------------------------------------
const
  /// <summary>
  ///   Default background from color
  /// </summary>
  DEFAULT_BACKGROUND_FROM = $00EBECEC;
  /// <summary>
  ///   Default background to color
  /// </summary>
  DEFAULT_BACKGROUND_TO = $00EFEFEF;

  /// <summary>
  ///   Default border from color
  /// </summary>
  DEFAULT_BORDER_FROM = $00BFBFBF;
  /// <summary>
  ///   Default border to color
  /// </summary>
  DEFAULT_BORDER_TO = $00CBCCCD;
  /// <summary>
  ///   Default border height
  /// </summary>
  DEFAULT_BORDER_HEIGHT = 2;

  /// <summary>
  ///   Default battery indicator size
  /// </summary>
  DEFAULT_BATTERY_INDICATOR_SIZE = 16;
  /// <summary>
  ///   Default battery indicator border color
  /// </summary>
  DEFAULT_BATTERY_INDICATOR_BORDER_COLOR = clBlack;
  /// <summary>
  ///   Default battery indicator label format
  /// </summary>
  DEFAULT_BATTERY_INDICATOR_LABEL_FORMAT = '%.1fV';
  /// <summary>
  ///   Default battery indicator from color
  /// </summary>
  DEFAULT_BATTERY_INDICATOR_FROM_COLOR = $00F5F5F5;
  /// <summary>
  ///   Default battery indicator to color
  /// </summary>
  DEFAULT_BATTERY_INDICATOR_TO_COLOR = $00C1C1C2;

  /// <summary>
  ///   Default VCI indicator size
  /// </summary>
  DEFAULT_VCI_INDICATOR_SIZE = 16;
  /// <summary>
  ///   Default VCI indicator border color
  /// </summary>
  DEFAULT_VCI_INDICATOR_BORDER_COLOR = clBlack;
  /// <summary>
  ///   Default VCI indicator from color
  /// </summary>
  DEFAULT_VCI_INDICATOR_FROM_COLOR = $00F5F5F5;
  /// <summary>
  ///   Default VCI indicator to color
  /// </summary>
  DEFAULT_VCI_INDICATOR_TO_COLOR = $00C1C1C2;
  /// <summary>
  ///   Default VCI indicator caption
  /// </summary>
  DEFAULT_VCI_INDICATOR_CAPTION = 'Not connected';

  /// <summary>
  ///   Default Internet connection indicator size
  /// </summary>
  DEFAULT_INTERNET_CONNECTION_INDICATOR_SIZE = 12;
  /// <summary>
  ///   Default Internet connection border color
  /// </summary>
  DEFAULT_INTERNET_CONNECTION_INDICATOR_BORDER_COLOR = clBlack;
  /// <summary>
  ///   Default Internet connection from color
  /// </summary>
  DEFAULT_INTERNET_CONNECTION_INDICATOR_FROM_COLOR = $00F5F5F5;
  /// <summary>
  ///   Default Internet connection to color
  /// </summary>
  DEFAULT_INTERNET_CONNECTION_INDICATOR_TO_COLOR = $00C1C1C2;
  /// <summary>
  ///   Default Internet connection caption
  /// </summary>
  DEFAULT_INTERNET_CONNECTION_INDICATOR_CAPTION = 'No internet access';

  /// <summary>
  ///   Default protocol indicator size
  /// </summary>
  DEFAULT_PROTOCOL_INDICATOR_SIZE = 16;
  /// <summary>
  ///   Default protocol border color
  /// </summary>
  DEFAULT_PROTOCOL_INDICATOR_BORDER_COLOR = clBlack;
  /// <summary>
  ///   Default protocol from color
  /// </summary>
  DEFAULT_PROTOCOL_INDICATOR_FROM_COLOR = $00F5F5F5;
  /// <summary>
  ///   Default protocol to color
  /// </summary>
  DEFAULT_PROTOCOL_INDICATOR_TO_COLOR = $00C1C1C2;
  /// <summary>
  ///   Default protocol caption
  /// </summary>
  DEFAULT_PROTOCOL_INDICATOR_CAPTION = 'Not connected';

//------------------------------------------------------------------------------
// CONSTANTS: STATUSBAR
//------------------------------------------------------------------------------
const
  /// <summary>
  ///   Default sizegrip color
  /// </summary>
  DEFAULT_SIZEGRIP_COLOR = $00909090;

implementation

end.
