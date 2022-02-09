module type GENERAL = sig
  type input = Tast_iterator.iterator

  val metric_id : string
  val run : Compile_common.info -> input -> input
  val collect_result : string -> unit -> unit
  val reset : unit -> unit
end
