unit stb_rect_pack;

// stb_rect_pack.h - v1.00 - public domain - rectangle packing
// Sean Barrett 2014
//
// Useful for e.g. packing rectangular textures into an atlas.
// Does not do rotation.
//
// Not necessarily the awesomest packing method, but better than
// the totally naive one in stb_truetype (which is primarily what
// this is meant to replace).
//
// Has only had a few tests run, may have issues.
//
// More docs to come.
//
// No memory allocations; uses qsort() and assert() from stdlib.
// Can override those by defining STBRP_SORT and STBRP_ASSERT.
//
// This library currently uses the Skyline Bottom-Left algorithm.
//
// Please note: better rectangle packers are welcome! Please
// implement them to the same API, but with a different init
// function.
//
// Credits
//
//  Library
//    Sean Barrett
//  Pascal port
//    Doj
//  Minor features
//    Martins Mozeiko
//    github:IntellectualKitty
//
//  Bugfixes / warning fixes
//    Jeremy Jaussaud
//    Fabian Giesen
//
// Version history:
//
//     1.00  (2019-02-25)  avoid small space waste; gracefully fail too-wide rectangles
//     0.99  (2019-02-07)  warning fixes
//     0.11  (2017-03-03)  return packing success/fail result
//     0.10  (2016-10-25)  remove cast-away-const to avoid warnings
//     0.09  (2016-08-27)  fix compiler warnings
//     0.08  (2015-09-13)  really fix bug with empty rects (w=0 or h=0)
//     0.07  (2015-09-13)  fix bug with empty rects (w=0 or h=0)
//     0.06  (2015-04-15)  added STBRP_SORT to allow replacing qsort
//     0.05:  added STBRP_ASSERT to allow replacing assert
//     0.04:  fixed minor bug in STBRP_LARGE_RECTS support
//     0.01:  initial release
//
// LICENSE
//
//   See end of file for license information.

{$MODE FPC}
{$MODESWITCH DEFAULTPARAMETERS}
{$MODESWITCH OUT}
{$MODESWITCH RESULT}

interface

const
  STB_RECT_PACK_VERSION = 1;

type
{$IF Defined(STBRP_LARGE_RECTS)}
  stbrp_coord = PtrInt;
{$ELSE}
  stbrp_coord = UInt16;
{$ENDIF}

stbrp_heuristic = (
   STBRP_HEURISTIC_Skyline_BL_sortHeight = 0,
   STBRP_HEURISTIC_Skyline_BF_sortHeight
);

const
  STBRP_HEURISTIC_Skyline_default = STBRP_HEURISTIC_Skyline_BL_sortHeight;

//////////////////////////////////////////////////////////////////////////////
//
// the details of the following structures don't matter to you, but they must
// be visible so you can handle the memory allocations for them

type
PPstbrp_node = ^Pstbrp_node;
Pstbrp_node = ^stbrp_node;
stbrp_node = record
  x,y: stbrp_coord;
  next: Pstbrp_node;
end;

Pstbrp_context = ^stbrp_context;
stbrp_context = record
  width: PtrInt;
  height: PtrInt;
  align: PtrInt;
  init_mode: PtrInt;
  heuristic: stbrp_heuristic;
  num_nodes: PtrInt;
  active_head: Pstbrp_node;
  free_head: Pstbrp_node;
  extra: array[0 .. 2 - 1] of stbrp_node; // we allocate two extra nodes so optimal user-node-count is 'width' not 'width+2'
end;

Pstbrp_rect = ^stbrp_rect;
stbrp_rect = record
  // reserved for your use:
  id: PtrUInt;

  // input:
  w, h: stbrp_coord;

  // output:
  x, y: stbrp_coord;
  was_packed: Boolean;  // non-zero if valid packing
end; // 16 bytes, nominally



function stbrp_pack_rects (context: Pstbrp_context; rects: Pstbrp_rect; num_rects: SizeUInt): Boolean;
// Assign packed locations to rectangles. The rectangles are of type
// 'stbrp_rect' defined above, stored in the array 'rects', and there
// are 'num_rects' many of them.
//
// Rectangles which are successfully packed have the 'was_packed' flag
// set to a non-zero value and 'x' and 'y' store the minimum location
// on each axis (i.e. bottom-left in cartesian coordinates, top-left
// if you imagine y increasing downwards). Rectangles which do not fit
// have the 'was_packed' flag set to 0.
//
// You should not try to access the 'rects' array from another thread
// while this function is running, as the function temporarily reorders
// the array while it executes.
//
// To pack into another rectangle, you need to call stbrp_init_target
// again. To continue packing into the same rectangle, you can call
// this function again. Calling this multiple times with multiple rect
// arrays will probably produce worse packing results than calling it
// a single time with the full rectangle array, but the option is
// available.
//
// The function returns 1 if all of the rectangles were successfully
// packed and 0 otherwise.


procedure stbrp_init_target (context: Pstbrp_context; width, height: SizeUInt; nodes: Pstbrp_node; num_nodes: SizeUInt);
// Initialize a rectangle packer to:
//    pack a rectangle that is 'width' by 'height' in dimensions
//    using temporary storage provided by the array 'nodes', which is 'num_nodes' long
//
// You must call this function every time you start packing into a new target.
//
// There is no "shutdown" function. The 'nodes' memory must stay valid for
// the following stbrp_pack_rects() call (or calls), but can be freed after
// the call (or calls) finish.
//
// Note: to guarantee best results, either:
//       1. make sure 'num_nodes' >= 'width'
//   or  2. call stbrp_allow_out_of_mem() defined below with 'allow_out_of_mem = 1'
//
// If you don't do either of the above things, widths will be quantized to multiples
// of small integers to guarantee the algorithm doesn't run out of temporary storage.
//
// If you do #2, then the non-quantized algorithm will be used, but the algorithm
// may run out of temporary storage and be unable to pack some rectangles.

procedure stbrp_setup_allow_out_of_mem (context: Pstbrp_context; allow_out_of_mem: Boolean);
// Optionally call this function after init but before doing any packing to
// change the handling of the out-of-temp-memory scenario, described above.
// If you call init again, this will be reset to the default (false).


procedure stbrp_setup_heuristic (context: Pstbrp_context; heuristic: stbrp_heuristic);
// Optionally select which packing heuristic the library should use. Different
// heuristics will produce better/worse results for different data sets.
// If you call init again, this will be reset to the default.

implementation

type
  TRectCompare = function (A, B: Pstbrp_rect): PtrInt;
procedure STBRP_SORT(rects: Pstbrp_rect; num_rects: SizeUInt; compare: TRectCompare);
var
  M, L, R: Pstbrp_rect;
  Temp: stbrp_rect;
begin
  while num_rects > 1 do begin
    L := rects;
    R := rects + (num_rects - 1);
    M := rects + (num_rects div 2);
    repeat
      while compare(L, M) = -1 do Inc(L);
      while compare(M, R) = -1 do Dec(R);
      if L <= R then begin
        Temp := L^;
        L^ := R^;
        R^ := Temp;
        if L = M then begin
          M := R;
        end else if R = M then
          M := L;
        Inc(L);
        Dec(R);
      end;
    until L > R;
    if L < rects + num_rects then
      STBRP_SORT(L, (rects + num_rects - L) div SizeOf(stbrp_rect), compare);
    // explicit tail recursion
    num_rects := (R - rects) div SizeOf(stbrp_rect) + 1;
  end;
end;

procedure STBRP_ASSERT(Condition: Boolean);
begin
  // TODO
  if not Condition then begin
    Writeln(stderr, 'STBRP_ASSERT FAILED');
  end;
end;

procedure STBRP__NOTUSED(var v); inline;
begin
end;

const
  STBRP__INIT_skyline = 1;

procedure stbrp_setup_heuristic(context: Pstbrp_context; heuristic: stbrp_heuristic);
begin
  case context^.init_mode of
    STBRP__INIT_skyline: begin
        STBRP_ASSERT((heuristic = STBRP_HEURISTIC_Skyline_BL_sortHeight) or (heuristic = STBRP_HEURISTIC_Skyline_BF_sortHeight));
        context^.heuristic := heuristic;
      end;
  else
    STBRP_ASSERT(False);
  end;
end;

procedure stbrp_setup_allow_out_of_mem (context: Pstbrp_context; allow_out_of_mem: Boolean);
begin
  if allow_out_of_mem then begin
    // if it's ok to run out of memory, then don't bother aligning them;
    // this gives better packing, but may fail due to OOM (even though
    // the rectangles easily fit). @TODO a smarter approach would be to only
    // quantize once we've hit OOM, then we could get rid of this parameter.
    context^.align := 1;
  end else begin
    // if it's not ok to run out of memory, then quantize the widths
    // so that num_nodes is always enough nodes.
    //
    // I.e. num_nodes * align >= width
    //                  align >= width / num_nodes
    //                  align = ceil(width/num_nodes)

    context^.align := (context^.width + context^.num_nodes - 1) div context^.num_nodes;
  end;
end;

procedure stbrp_init_target (context: Pstbrp_context; width, height: SizeUInt; nodes: Pstbrp_node; num_nodes: SizeUInt);
var
  i: PtrInt;
begin
{$IF not Defined(STBRP_LARGE_RECTS)}
  STBRP_ASSERT((width <= $ffff) and (height <= $ffff));
{$ENDIF}

  for i := 0 to num_nodes - 2 do
    nodes[i].next := @nodes[i + 1];
  nodes[num_nodes - 1].next := nil;
  context^.init_mode := STBRP__INIT_skyline;
  context^.heuristic := STBRP_HEURISTIC_Skyline_default;
  context^.free_head := @nodes[0];
  context^.active_head := @context^.extra[0];
  context^.width := width;
  context^.height := height;
  context^.num_nodes := num_nodes;
  stbrp_setup_allow_out_of_mem(context, False);

  // node 0 is the full width, node 1 is the sentinel (lets us not store width explicitly)
  context^.extra[0].x := 0;
  context^.extra[0].y := 0;
  context^.extra[0].next := @context^.extra[1];
  context^.extra[1].x := stbrp_coord(width);
{$IF Defined(STBRP_LARGE_RECTS)}
  context^.extra[1].y := 1 shl 30;
{$ELSE}
  context^.extra[1].y := 65535;
{$ENDIF}
  context^.extra[1].next := nil;
end;

// find minimum y position if it starts at x1
function stbrp__skyline_find_min_y(c: Pstbrp_context; first: Pstbrp_node; x0: stbrp_coord; width: SizeUInt; pwaste: PSizeUInt): stbrp_coord;
var
  node: Pstbrp_node;
  x1, min_y, visited_width: stbrp_coord;
  waste_area: SizeUInt;
  under_width: stbrp_coord;
begin
  node := first;
  x1 := x0 + width;

  STBRP__NOTUSED(c);

  STBRP_ASSERT(first^.x <= x0);

{$IF False}
  // skip in case we're past the node
  while node^.next^.x <= x0 do
    Inc(node);
{$ELSE}
  STBRP_ASSERT(node^.next^.x > x0); // we ended up handling this in the caller for efficiency
{$ENDIF}

  STBRP_ASSERT(node^.x <= x0);

  min_y := 0;
  waste_area := 0;
  visited_width := 0;
  while node^.x < x1 do begin
    if node^.y > min_y then begin
      // raise min_y higher.
      // we've accounted for all waste up to min_y,
      // but we'll now add more waste for everything we've visted
      Inc(waste_area, visited_width * (node^.y - min_y));
      min_y := node^.y;
      // the first time through, visited_width might be reduced
      if node^.x < x0 then begin
        Inc(visited_width, node^.next^.x - x0);
      end else
        Inc(visited_width, node^.next^.x - node^.x);
    end else begin
      // add waste area
      under_width := node^.next^.x - node^.x;
      if under_width + visited_width > width then
        under_width := width - visited_width;
      Inc(waste_area, under_width * (min_y - node^.y));
      Inc(visited_width, under_width);
    end;
    node := node^.next;
  end;

  pwaste^ := waste_area;
  Exit(min_y);
end;

type
stbrp__findresult = record
  x, y: PtrInt;
  prev_link: PPstbrp_node;
end;

function stbrp__skyline_find_best_pos(c: Pstbrp_context; width, height: SizeUInt): stbrp__findresult;
var
  best_waste: SizeUInt;
  best_x, best_y: UInt32;
  fr: stbrp__findresult;
  prev, best: PPstbrp_node;
  node, tail: Pstbrp_node;
  y: stbrp_coord;
  xpos: PtrInt;
  waste: SizeUInt;
begin
  best_waste := 1 shl 30;
  best_y := 1 shl 30;
  best := nil;

  // align to multiple of c^.align
  width := width + c^.align - 1;
  Dec(width, width mod c^.align);
  STBRP_ASSERT(width mod c^.align = 0);

  // if it can't possibly fit, bail immediately
  if (width > c^.width) or (height > c^.height) then begin
    fr.prev_link := nil;
    fr.x := 0;
    fr.y := 0;
    Exit(fr);
  end;

  node := c^.active_head;
  prev := @c^.active_head;
  while node^.x + width <= c^.width do begin
    y := stbrp__skyline_find_min_y(c, node, node^.x, width, @waste);
    if c^.heuristic = STBRP_HEURISTIC_Skyline_BL_sortHeight then begin // actually just want to test BL
      // bottom left
      if y < best_y then begin
        best_y := y;
        best := prev;
      end;
    end else begin
      // best-fit
      if y + height <= c^.height then begin
        // can only use it if it first vertically
        if (y < best_y) or ((y = best_y) and (waste < best_waste)) then begin
          best_y := y;
          best_waste := waste;
          best := prev;
        end;
      end;
    end;
    prev := @node^.next;
    node := node^.next;
  end;

  if best = nil then begin
    best_x := 0;
  end else
    best_x := (best^)^.x;

  // if doing best-fit (BF), we also have to try aligning right edge to each node position
  //
  // e.g, if fitting
  //
  //     ____________________
  //    |____________________|
  //
  //            into
  //
  //   |                         |
  //   |             ____________|
  //   |____________|
  //
  // then right-aligned reduces waste, but bottom-left BL is always chooses left-aligned
  //
  // This makes BF take about 2x the time

  if c^.heuristic = STBRP_HEURISTIC_Skyline_BF_sortHeight then begin
    tail := c^.active_head;
    node := c^.active_head;
    prev := @c^.active_head;
    // find first node that's admissible
    while tail^.x < width do
      tail := tail^.next;
    while tail <> nil do begin
      xpos := tail^.x - width;
      STBRP_ASSERT(xpos >= 0);
      // find the left position that matches this
      while node^.next^.x <= xpos do begin
        prev := @node^.next;
        node := node^.next;
      end;
      STBRP_ASSERT((node^.next^.x > xpos) and (node^.x <= xpos));
      y := stbrp__skyline_find_min_y(c, node, xpos, width, @waste);
      if y + height <= c^.height then begin
        if y <= best_y then begin
          if (y < best_y) or (waste < best_waste) or ((waste = best_waste) and (xpos < best_x)) then begin
            best_x := xpos;
            STBRP_ASSERT(y <= best_y);
            best_y := y;
            best_waste := waste;
            best := prev;
          end;
        end;
      end;
      tail := tail^.next;
    end;
  end;

  fr.prev_link := best;
  fr.x := best_x;
  fr.y := best_y;
  Exit(fr);
end;

function stbrp__skyline_pack_rectangle(context: Pstbrp_context; width, height: stbrp_coord): stbrp__findresult;
var
  res: stbrp__findresult;
  node, cur, next: Pstbrp_node;
  count: LongInt;
begin
   // find best position according to heuristic
   res := stbrp__skyline_find_best_pos(context, width, height);

   // bail if:
   //    1. it failed
   //    2. the best node doesn't fit (we don't always check this)
   //    3. we're out of memory
   if (res.prev_link = nil) or (res.y + height > context^.height) or (context^.free_head = nil) then begin
     res.prev_link := nil;
     Exit(res);
   end;

   // on success, create new node
   node := context^.free_head;
   node^.x := stbrp_coord(res.x);
   node^.y := stbrp_coord(res.y + height);

   context^.free_head := node^.next;

   // insert the new node into the right starting point, and
   // let 'cur' point to the remaining nodes needing to be
   // stiched back in

   cur := res.prev_link^;
   if cur^.x < res.x then begin
     // preserve the existing one, so start testing with the next one
     next := cur^.next;
     cur^.next := node;
     cur := next;
   end else begin
     res.prev_link^ := node;
   end;

   // from here, traverse cur and free the nodes, until we get to one
   // that shouldn't be freed
   while (cur^.next <> nil) and (cur^.next^.x <= res.x + width) do begin
     next := cur^.next;
     // move the current node to the free list
     cur^.next := context^.free_head;
     context^.free_head := cur;
     cur := next;
   end;

   // stitch the list back in
   node^.next := cur;

   if cur^.x < res.x + width then
      cur^.x := stbrp_coord(res.x + width);

{$IF Defined(_DEBUG)}
  cur := context^.active_head;
  while cur^.x < context^.width do begin
    STBRP_ASSERT(cur^.x < cur^.next^.x);
    cur := cur^.next;
  end;
  STBRP_ASSERT(cur^.next = nil);

  begin
    count := 0;
    cur := context^.active_head;
    while cur <> nil do begin
      cur := cur^.next;
      Inc(count);
    end;
    cur := context^.free_head;
    while cur <> nil do begin
      cur := cur^.next;
      Inc(count);
    end;
    STBRP_ASSERT(count = context^.num_nodes + 2);
  end;
{$ELSE}
  STBRP__NOTUSED(count);
{$ENDIF}

  Exit(res);
end;

function rect_height_compare(p, q: Pstbrp_rect): PtrInt;
begin
  if p^.h > q^.h then
    Exit(-1);
  if p^.h < q^.h then
    Exit( 1);
  if p^.w > q^.w then begin
    Exit(-1);
  end else begin
    Exit(Ord(p^.w < q^.w));
  end;
end;

function rect_original_order(p, q: Pstbrp_rect): PtrInt;
begin
  if Ord(p^.was_packed) < Ord(q^.was_packed) then begin
    Exit(-1);
  end else
    Exit(Ord(Ord(p^.was_packed) > Ord(q^.was_packed)));
end;

{$IF Defined(STBRP_LARGE_RECTS)}
const
  STBRP__MAXVAL = $ffffffff;
{$ELSE}
const
  STBRP__MAXVAL = $ffff;
{$ENDIF}

function stbrp_pack_rects (context: Pstbrp_context; rects: Pstbrp_rect; num_rects: SizeUInt): Boolean;
var
  i: PtrInt;
  fr: stbrp__findresult;
  all_rects_packed: Boolean;
begin
  all_rects_packed := True;

  // we use the 'was_packed' field internally to allow sorting/unsorting
  for i := 0 to num_rects - 1 do begin
    rects[i].was_packed := True;
  end;

  // sort according to heuristic
  STBRP_SORT(rects, num_rects, @rect_height_compare);

  for i := 0 to num_rects - 1 do begin
    if (rects[i].w = 0) or (rects[i].h = 0) then begin
      rects[i].x := 0;
      rects[i].y := 0;  // empty rect needs no space
    end else begin
      fr := stbrp__skyline_pack_rectangle(context, rects[i].w, rects[i].h);
      if fr.prev_link <> nil then begin
        rects[i].x := stbrp_coord(fr.x);
        rects[i].y := stbrp_coord(fr.y);
      end else begin
        rects[i].x := STBRP__MAXVAL;
        rects[i].y := STBRP__MAXVAL;
      end;
    end;
  end;

  // unsort
  STBRP_SORT(rects, num_rects, @rect_original_order);

  // set was_packed flags and all_rects_packed status
  for i := 0 to num_rects - 1 do begin
    rects[i].was_packed := not ((rects[i].x = STBRP__MAXVAL) and (rects[i].y = STBRP__MAXVAL));
    if not rects[i].was_packed then
      all_rects_packed := False;
  end;

  // return the all_rects_packed status
  Exit(all_rects_packed);
end;

end.

{
------------------------------------------------------------------------------
This software is available under 2 licenses -- choose whichever you prefer.
------------------------------------------------------------------------------
ALTERNATIVE A - MIT License
Copyright (c) 2017 Sean Barrett
Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:
The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
------------------------------------------------------------------------------
ALTERNATIVE B - Public Domain (www.unlicense.org)
This is free and unencumbered software released into the public domain.
Anyone is free to copy, modify, publish, use, compile, sell, or distribute this
software, either in source code form or as a compiled binary, for any purpose,
commercial or non-commercial, and by any means.
In jurisdictions that recognize copyright laws, the author or authors of this
software dedicate any and all copyright interest in the software to the public
domain. We make this dedication for the benefit of the public at large and to
the detriment of our heirs and successors. We intend this dedication to be an
overt act of relinquishment in perpetuity of all present and future rights to
this software under copyright law.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
------------------------------------------------------------------------------
}
