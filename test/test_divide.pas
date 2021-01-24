{$MODE FPC}
{$MODESWITCH DEFAULTPARAMETERS}
{$MODESWITCH OUT}
{$MODESWITCH RESULT}

uses
  stb_divide in '../stb_divide.pas';

begin
  if stb_divide_run_test <> 0 then
    Halt(1);
end.
