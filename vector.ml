(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Jean-Christophe Filliatre                               *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

module Narray = Nullable_array

type 'a t = {
  mutable size:  int;
  mutable data:  'a Narray.t; (* 0 <= size <= Narray.length data *)
}

let make n v =
  if n < 0 || n > Narray.max_length then invalid_arg "Vector.make";
  { size = n; data = Narray.make_some n v; }

let create () =
  { size = 0; data = Narray.empty_array }

let init n f =
  if n < 0 || n > Narray.max_length then invalid_arg "Vector.init";
  { size = n; data = Narray.init_some n f; }

let length a =
  a.size

let get a i =
  if i < 0 || i >= a.size then invalid_arg "Vector.get";
  Narray.unsafe_get_some a.data i

let set a i v =
  if i < 0 || i >= a.size then invalid_arg "Vector.set";
  Narray.unsafe_set_some a.data i v

let unsafe_get a i =
  Narray.unsafe_get_some a.data i

let unsafe_set a i v =
  Narray.unsafe_set_some a.data i v

(* shrink that assumes [0 <= s < a.size] *)
let unsafe_shrink a s =
  let n = Narray.length a.data in
  if 4 * s < n then (* reallocate into a smaller array *)
    a.data <- Narray.sub a.data 0 s
  else begin
    for i = s to a.size - s do
      Narray.clear a.data i
    done
  end;
  a.size <- s

let shrink a s =
  if s < 0 then invalid_arg "Vector.shrink";
  if s < a.size then unsafe_shrink a s

(* expansion that creates an uninitialised suffix *)
let unsafe_expand a s =
  if s > a.size then begin
    let n = Narray.length a.data in
    if s > n then begin
      let n' = min (max (2 * n) s) Narray.max_length in
      let a' = Narray.make n' in
      Narray.blit a.data 0 a' 0 a.size;
      a.data <- a'
    end;
    a.size <- s
  end

let resize a s v =
  if s < 0 then invalid_arg "Vector.resize";
  if s <= a.size then
    unsafe_shrink a s
  else begin
    let n = Narray.length a.data in
    (* grow *)
    if s > n then begin (* reallocate into a larger array *)
      if s > Narray.max_length then invalid_arg "Vector.resize: cannot grow";
      let n' = min (max (2 * n) s) Narray.max_length in
      let a' = Narray.make_some n' v in
      Narray.blit a.data 0 a' 0 a.size;
      a.data <- a'
    end;
    a.size <- s
  end

(** stack interface *)

let is_empty a =
  length a = 0

let clear a =
  shrink a 0

let push a v =
  let n = a.size in
  unsafe_expand a (n+1);
  Narray.unsafe_set_some a.data n v

exception Empty

let top a =
  let n = length a in
  if n = 0 then raise Empty;
  Narray.unsafe_get_some a.data (n - 1)

let pop a =
  let n = length a - 1 in
  if n < 0 then raise Empty;
  let r = Narray.unsafe_get_some a.data n in
  shrink a n;
  r

(** array interface *)

let append a1 a2 =
  let n1 = length a1 in
  let n2 = length a2 in
  unsafe_expand a1 (n1 + n2);
  for i = 0 to n2 - 1 do unsafe_set a1 (n1 + i) (unsafe_get a2 i) done

let copy a =
  { size = a.size;
    data = Narray.copy a.data; }

let sub a ofs len =
  if len < 0 || ofs > length a - len then invalid_arg "Vector.sub";
  { size = len; data = Narray.sub a.data ofs len }

let fill a ofs len v =
  if ofs < 0 || len < 0 || ofs > length a - len then invalid_arg "Vector.fill";
  for i = ofs to ofs + len - 1 do unsafe_set a i v done

let blit a1 ofs1 a2 ofs2 len =
  if len < 0 || ofs1 < 0 || ofs1 > length a1 - len
             || ofs2 < 0 || ofs2 > length a2 - len
  then invalid_arg "Vector.blit";
  if ofs1 <= ofs2 then
    for i = len - 1 downto 0 do
      unsafe_set a2 (ofs2 + i) (unsafe_get a1 (ofs1 + i))
    done
  else
    for i = 0 to len - 1 do
      unsafe_set a2 (ofs2 + i) (unsafe_get a1 (ofs1 + i))
    done

let iter f a =
  for i = 0 to length a - 1 do f (unsafe_get a i) done

let map f a =
  { size = a.size; data = Narray.map_some f a.data }

let iteri f a =
  for i = 0 to length a - 1 do f i (unsafe_get a i) done

let mapi f a =
  { size = a.size; data = Narray.mapi_some f a.data }

let to_list a =
  let rec tolist i res =
    if i < 0 then res else tolist (i - 1) (unsafe_get a i :: res) in
  tolist (length a - 1) []

let of_list l =
  let a = Narray.of_list l in
  { size = Narray.length a; data = a }

let to_array a =
  Array.init a.size (fun i -> Narray.unsafe_get_some a.data i)

let of_array a =
  { size = Array.length a; data = Narray.of_array a }

let fold_left f x a =
  let r = ref x in
  for i = 0 to length a - 1 do r := f !r (unsafe_get a i) done;
  !r

let fold_right f a x =
  let r = ref x in
  for i = length a - 1 downto 0 do r := f (unsafe_get a i) !r done;
  !r

