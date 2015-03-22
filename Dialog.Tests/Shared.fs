module Shared

open Core.Printf

let printfn fmt =
    kprintf (fun (str:string) -> System.Diagnostics.Debug.WriteLine str) fmt

