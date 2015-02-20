namespace FsReact

open Reconciler

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

    let mutable _properties = Dict<string, MountedProperty<'target>>()
    
    let reconcile = 
        let functions = 
            {
                add = fun _ v -> 
                    writer.mount target v

                update = fun _ mounted v -> 
                    match mounted.update with
                    | Some u -> 
                        u v
                        mounted
                    | None ->
                    // update fallback is remove and add
                    mounted.remove()
                    writer.mount target v
                    
                remove = fun _ mounted -> 
                    mounted.remove()
            }

        reconcile functions

    member this.update properties = 
        let namedProperties = properties |> List.map (fun p -> (p.GetType().Name, p))
        _properties <- reconcile _properties namedProperties
