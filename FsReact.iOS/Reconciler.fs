namespace FsReact

open System.Collections.Generic
open System.Linq

type Dict<'k, 'v> = Dictionary<'k, 'v>


(*
    todo: encapsulate dictionary away to make it immutable outside of the reconciler
*)

module Dict = 
    let ofList (l : _ list) = l.ToDictionary(fst, snd)

    let private deconstruct (kvp:KeyValuePair<_, _>) = kvp.Key, kvp.Value
    let toSeq (d:Dict<_,_>) = d |> seq |> Seq.map deconstruct

(* 
    A key-based dictionary reconciler.

    todo:
        - support actual partial specification of reconcile functions.
        - call update only if the value has changed?
*)

module Reconciler =

    type ReconcileFunctions<'k, 'v, 'g> = 
        { 
            add: int -> 'k -> 'v -> 'g; 
            update: int -> 'k -> 'g -> 'v -> 'g; 
            remove : 'k -> 'g -> unit 
        }

    let reconcile (functions: ReconcileFunctions<'k, 'v, 'g>) (cur: Dict<'k, 'g>) (next: ('k * 'v) list) : Dict<'k, 'g> = 
        let indexTable = Dict<'k, int>(cur.Count)
        next 
        |> List.iteri (fun i (k, _) -> indexTable.Add(k, i))

        // First remove the old ones, so that 
        // - at no point in time, new and old elements are mixed
        // - and our add indices match the target lists position
        
        for kv in cur do
            let k = kv.Key
            match indexTable.TryGetValue k |> fst with
            | false -> functions.remove k kv.Value
            | _ -> ()

        // The process updates and adds in order 
        // This ensures that if the caller organizes elements in a list, 
        // they will always match up with the new list.

        let r = Dict<'k, 'g>(cur.Count)

        for k, v in next do
            let i = indexTable.[k]
            match cur.TryGetValue k with
            | (true, g) -> 
                let g = functions.update i k g v
                r.Add (k, g)
            | (false, _) ->
                let g = functions.add i k v 
                r.Add (k, g)

        r