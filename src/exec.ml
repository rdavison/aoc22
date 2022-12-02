open! Import

module Part = struct
  type 'a t =
    { name : string
    ; run : unit -> 'a
    ; to_string : 'a -> string
    }
end

let many ?(no_time = false) (parts : 'a Part.t list) =
  List.iter parts ~f:(fun part ->
      printf "-- Running: %s\n" part.name;
      let output, span = timeit part.run in
      if not no_time then printf "-- Time: %f\n" (Core_private.Span_float.to_sec span);
      printf "-- Result:\n";
      printf "%s\n" (part.to_string output))
;;
