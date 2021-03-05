{$MODE FPC}
{$MODESWITCH DEFAULTPARAMETERS}
{$MODESWITCH OUT}
{$MODESWITCH RESULT}

uses
  stb_rect_pack in '../stb_rect_pack.pas';

var
  TestCase: record
    Name: AnsiString;
    W, H: PtrInt;
    N: PtrInt;
    Rects: array of record
      W, H: PtrInt;
    end;
  end;
  context: stbrp_context;
  nodes: array of stbrp_node;
  rects: array of stbrp_rect;

function ReadS(var F: TextFile; out S: AnsiString): Boolean;
begin
{$I-}
  Readln(F, S);
{$I+}
  if IOResult <> 0 then begin
    Writeln(stderr, 'ERROR: could not read ', TestCase.Name);
    Exit(False);
  end;
  Exit(True);
end;

function ReadI(var F: TextFile; out I: PtrInt): Boolean;
begin
{$I-}
  Read(F, I);
{$I+}
  if IOResult <> 0 then begin
    Writeln(stderr, 'ERROR: could not read ', TestCase.Name);
    Exit(False);
  end;
  Exit(True);
end;

function ReadTestCase(const FileName: AnsiString): Boolean;
var
  F: TextFile;
  S: AnsiString;
  I: LongInt;
begin
  TestCase.Name := FileName;
  Assign(F, FileName);
{$I-}
  Reset(F);
{$I+}
  if IOResult <> 0 then begin
    Writeln(stderr, 'ERROR: could not open ', FileName);
    Exit(False);
  end;

  repeat
    if not ReadS(F, S) then
      Exit(False);
  until S = '';

  if (not ReadI(F, TestCase.W))
  or (not ReadI(F, TestCase.H))
  or (not ReadI(F, TestCase.N))
  then
    Exit(False);

  SetLength(TestCase.Rects, TestCase.N);
  for I := 0 to TestCase.N - 1 do begin
    if (not ReadI(F, TestCase.Rects[I].W))
    or (not ReadI(F, TestCase.Rects[I].H))
    then
      Exit(False);
  end;

  Close(F);
  Exit(True);
end;

function PackedByIndex(Index: LongInt): Boolean;
var
  I: LongInt;
begin
  for I := 0 to High(rects) do begin
    if rects[I].id = 45670000 + Index then begin
      if (rects[I].w <> TestCase.Rects[I].W) or (rects[I].h <> TestCase.Rects[I].H) then
        Exit(False);
      Exit(True);
    end;
  end;
  Exit(False);
end;

function Intersects1(A, B, C, D: PtrInt): Boolean;
begin
  if B <= C then
    Exit(False);
  if D <= A then
    Exit(False);
  Exit(True);
end;

function Intersects(A, B: Pstbrp_rect): Boolean;
begin
  Result := Intersects1(A^.x, A^.x + A^.w, B^.x, B^.x + B^.w)
        and Intersects1(A^.y, A^.y + A^.h, B^.y, B^.y + B^.h);
end;

function CheckResultIsCorrect: Boolean;
var
  I, J: LongInt;
begin
  Result := True;
  for I := 0 to High(rects) do begin
    if not PackedByIndex(I) then begin
      Writeln('FAILURE:', TestCase.Name, ': rect ', I, ' not found in result');
      Result := False;
    end;

    if not rects[I].was_packed then begin
      Writeln('FAILURE:', TestCase.Name, ': rect ', rects[I].id - 45670000, ' was not packed');
      Result := False;
    end;
    if (PtrInt(rects[I].x) < 0) or (PtrInt(rects[I].y) < 0) or (rects[I].x + rects[I].w > TestCase.W) or (rects[I].y + rects[I].h > TestCase.H) then begin
      Writeln('FAILURE:', TestCase.Name, ': rect ', rects[I].id - 45670000, ' does not fit into sheet');
      Result := False;
    end;

    for J := I + 1 to High(rects) do begin
      if Intersects(@rects[I], @rects[J]) then begin
        Writeln('FAILURE:', TestCase.Name, ': rects ', rects[I].id - 45670000, ' and ', rects[J].id - 45670000, ' overlapped');
        Result := False;
      end;
    end;
  end;
end;

function RunTestCase(const FileName: AnsiString): Boolean;
var
  I: LongInt;
begin
  if not ReadTestCase('data/rect_pack/example.txt') then
    Exit(False);

  SetLength(nodes, TestCase.W);
  stbrp_init_target(@context, TestCase.W, TestCase.H, @nodes[0], Length(nodes));

  SetLength(rects, TestCase.N);
  for I := 0 to TestCase.N - 1 do begin
    rects[I].id := 45670000 + I;
    rects[I].w := TestCase.Rects[I].W;
    rects[I].h := TestCase.Rects[I].H;
  end;
  if not stbrp_pack_rects(@context, @rects[0], Length(rects)) then begin
    Writeln(stderr, 'FAILURE: could not solve ', TestCase.Name);
    Exit(False);
  end;

  if not CheckResultIsCorrect then begin
    Writeln(stderr, 'FAILURE: incorrect result ', TestCase.Name);
    Exit(False);
  end;

  Exit(True);
end;

var
  all_ok: Boolean = True;
begin
  all_ok := all_ok and RunTestCase('data/rect_pack/example.txt');

  if not all_ok then
    Halt(1);
end.
