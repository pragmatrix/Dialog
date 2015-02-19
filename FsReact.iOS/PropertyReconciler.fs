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
        - stores the MountedProperty instances of the writer

    todo:
        - reuse keys when calling the writer
        - combine _values and _properties dictionary
*)

type PropertyReconciler<'target>(writer : PropertyWriter<'target>, target : 'target) = 

    let mutable _values = Dict<string, obj>()
    let _properties = Dict<string, MountedProperty<'target>>()
    
    let reconcile = 
        let functions = 
            {
                add = fun k v -> 
                    let mounted = writer.mount target v
                    _properties.Add(k, mounted)

                update = fun k v -> 
                    let mounted = _properties.[k]
                    match mounted.update with
                    | Some u -> u v
                    | None ->
                    // update fallback is remove and add
                    mounted.remove()
                    _properties.Remove k |> ignore
                    let mounted = writer.mount target v
                    _properties.Add(k, mounted)
                    
                remove = fun k v -> 
                    _properties.[k].remove()
                    _properties.Remove k |> ignore
            }

        reconcile functions

    member this.update properties = 
        let namedProperties = properties |> List.map (fun p -> (p.GetType().Name, p))
        _values <- reconcile _values namedProperties
