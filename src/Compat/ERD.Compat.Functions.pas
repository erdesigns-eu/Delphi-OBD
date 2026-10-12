// ------------------------------------------------------------------------------
// ERD.Compat.Functions
// Managed function-reference signatures for Free Pascal 3.3.1 or later.
// Author: ERDesigns and Delphi-OBD contributors
// License: see LICENSE
// ------------------------------------------------------------------------------
unit ERD.Compat.Functions;
{$IFDEF FPC}
{$IF FPC_FULLVERSION < 30301}
{$FATAL Full nonvisual runtime requires FPC 3.3.1 or later}
{$ENDIF}
{$MODE DELPHI}
{$MODESWITCH FUNCTIONREFERENCES}
{$MODESWITCH ANONYMOUSFUNCTIONS}
{$ENDIF}

interface

{$IFDEF FPC}

type
  TProc = reference to procedure;
  TProc<T> = reference to procedure(AValue: T);
  TProc<T1, T2> = reference to procedure(AFirst: T1; ASecond: T2);
  TProc<T1, T2, T3> = reference to procedure(AFirst: T1; ASecond: T2;
    AThird: T3);
  TFunc<TResult> = reference to function: TResult;
  TFunc<T, TResult> = reference to function(AValue: T): TResult;
  TFunc<T1, T2, TResult> = reference to function(AFirst: T1;
    ASecond: T2): TResult;
  TFunc<T1, T2, T3, TResult> = reference to function(AFirst: T1; ASecond: T2;
    AThird: T3): TResult;
{$ENDIF}

implementation

end.
