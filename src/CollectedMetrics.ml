open Base
open Caml.Format
open Utils

let metric_notes : string Queue.t = Queue.create ()
let add_note = Queue.enqueue metric_notes

let metric_results : (string * int) Queue.t = Queue.create ()
let add_result metric_id res = Queue.enqueue metric_results (metric_id, res)

let report verbose () =
    Queue.iter metric_results ~f:(fun (metric_id, res) ->
        Format.printf "%s: %d\n" metric_id res
    );
    let report_notes () =
        Format.printf "\n";
        Queue.iter metric_notes ~f:(fun s ->
            Format.printf "%s\n" s
        );
    in
      if verbose () then
        report_notes ()
;;
