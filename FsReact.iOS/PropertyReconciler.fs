namespace FsReact

open System.Collections.Generic

type Dict<'k, 'v> = Dictionary<'k, 'v>


(* 
    A key-based dictionary reconciler.

    todo:
        - support actual partial specification of reconcile functions.
        - call update only if the value has changed?
*)

[<AutoOpen>]
module Reconciler =

    type ReconcileFunctions<'k, 'v> = 
        { 
            add: 'k -> 'v -> unit; 
            update: 'k -> 'v -> unit; 
            remove : 'k -> 'v -> unit 
        }

    let reconcile (functions: ReconcileFunctions<'k, 'v>) (cur: Dict<'k, 'v>) (next: ('k * 'v) list) : Dict<'k, 'v> = 
        let r = Dict<'k, 'v>(cur.Count)
        for k,v in next do
            match cur.TryGetValue k with
            | (true, v) -> 
                functions.update k v
                r.Add (k, v)
            | (false, _) ->
                functions.add k v 
                r.Add (k, v)

        for kv in cur do
            let k = kv.Key
            match r.TryGetValue k |> fst with
            | false -> functions.remove k kv.Value
            | _ -> ()

        r

(*
    A property reconcilder that 
        - is bound to a specific target
        - has a set of properties defined by the writer
        - stores the actual properties.

    todo:
        - reuse keys when calling the writer
*)

type PropertyReconciler<'target>(writer : PropertyWriter<'target>, target : 'target) = 

    let mutable _properties = Dict<string, obj>()
    
    let reconcile = 
        let functions = 
            {
                add = fun _ v -> writer.add target v;
                update = fun _ v -> writer.update target v;
                remove = fun _ v -> writer.update target v;
            }

        reconcile functions

    member this.update properties = 
        let namedProperties = properties |> List.map (fun p -> (p.GetType().Name, p))
        _properties <- reconcile _properties namedProperties
