namespace Dialog

open ComponentDOM

type Scanner = MountedElement -> MountedElement list

module Scanners = 

    type ScanInclusion = 
        | IncludeAndScanNested
        | ScanNestedAndInclude
        | ScanNested
        | Include
        | Skip

    let dontScan _ = []

    (*
        A type scanner that only tests native names for inclusion and 
        continues scanning nested elements of components
    *)

    let recursiveNativeNameScanner nameTest : Scanner = 

        let rec scan mounted = 
            let nestedNames = mounted.orderedKeys
            [
                for name in nestedNames do
                    let nested = mounted.nested.[name]
                    match nested.state with
                    | ComponentState _ ->
                        yield! scan nested
                    | ServiceState (n, _) ->
                    match nameTest n with
                    | IncludeAndScanNested ->
                        yield nested
                        yield! scan nested
                    | ScanNestedAndInclude ->
                        yield! scan nested
                        yield nested
                    | ScanNested ->
                        yield! scan nested
                    | Include ->
                        yield nested
                    | Skip -> ()
            ]

        scan
        
    