﻿namespace Dialog

open Reconciler

(*
    A property reconciler that 
        - is bound to a specific target
        - has a set of properties defined by the writer
        - stores the actual properties.
        - stores the MountedProperty instances of the writer

    todo:
        - reuse keys when calling the writer
        - combine _values and _properties dictionary
*)

type PropertyReconciler<'target>(writer : PropertyAccessor<'target>, target : 'target, identity: string) = 

    let mutable _properties = Dict<string, MountedProperty>()
    
    let mountProperty v = writer.mount target identity v

    let reconcile = 
        let functions = 
            {
                insert = fun _ _ v -> mountProperty v
                update = fun _ _ mounted v -> mounted.update v
                remove = fun _ mounted -> mounted.unmount()
            }

        reconcile functions

    member this.update properties = 
        let namedProperties = properties |> List.map (fun p -> (p.GetType().Name, p))
        _properties <- reconcile _properties namedProperties
