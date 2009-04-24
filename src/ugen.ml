
exception Inconsistent_buffer_sizes
exception Nonexistent_input

class type ugen =
  object
    method tick : float -> int -> unit (* now, samples *)
    method last : float array
    method has_input : string -> bool
    method connect : string -> ugen -> unit
    method disconnect : string -> ugen -> unit
    method parents : ugen list
  end

class in_hole =
  object
    val mutable inputs : ugen list = []
    method add ugen = inputs <- ugen :: inputs
    method remove ugen = inputs <- List.filter (fun u -> u = ugen) inputs
    method inputs = inputs
    method sum samples =
      let add_to to_arr from_arr =
        Array.iteri (fun i ugen -> to_arr.(i) <- to_arr.(i) +. from_arr.(i)) to_arr
      in
      List.fold_left
        (fun sum ugen -> add_to sum ugen#last; sum)
        (Array.make samples 0.0)
        inputs
  end

class gain : ugen =
  object
    val default_input = new in_hole
    val mutable buffer = Array.make 0 0.0
    method tick now samples = buffer <- default_input#sum samples
    method last = buffer
    method has_input name = name = "default"
    method connect input_name ugen =
      if input_name = "default" then
        default_input#add ugen
      else raise Nonexistent_input
    method disconnect input_name ugen =
      if input_name = "default" then
        default_input#remove ugen
      else raise Nonexistent_input
    method parents = default_input#inputs
  end
