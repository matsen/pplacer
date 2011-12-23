open Subcommand
open Guppy_cmdobjs
open Ppatteries

class cmd () =
object (self)
  inherit subcommand () as super
  inherit output_cmd ~prefix_required:true () as super_output

  val n_groups = flag "--groups"
    (Formatted (4, "The number of groups to collect sequences into. default: %d"))

  method specl =
    super_output#specl
  @ [
    int_flag n_groups;
  ]

  method desc = "group sequences"
  method usage = "usage: group [options] in.fasta"

  method action = function
    | [infile] ->
      let prefix = self#single_prefix ~requires_user_prefix:true ()
      and in_aln = Alignment.upper_list_of_any_file infile in
      let n_sites = List.hd in_aln |> snd |> String.length in
      Seq_group.group (fv n_groups) in_aln
        |> Array.iteri
            (fun i sl ->
              Alignment.to_fasta
                (Array.of_list sl)
                (Printf.sprintf "%s%d.fasta" prefix i))

    | l ->
      List.length l
      |> Printf.sprintf "group takes exactly one placefile (%d given)"
      |> failwith

end
