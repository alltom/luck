
exception Inconsistent_buffer_sizes
exception Nonexistent_input

class type ugen =
  object
    method connect : string -> ugen -> unit
    method disconnect : string -> ugen -> unit
    method has_input : string -> bool
    method last : float array
    method parents : ugen list
    method tick : float -> int -> unit
  end

class gain : ugen
