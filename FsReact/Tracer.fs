namespace FsReact

#if DEBUG

module Tracer =

    let mutable private tracer = System.Diagnostics.Debug.WriteLine

    let set t = tracer <- t

    let trace str = tracer str 

module Trace = 

    let private trace = Tracer.trace

    let renderingComponent componentName componentKey = 
        "rendering " + componentName + componentKey
        |> trace
    
    let mountingProperty key propertyName property = 
        sprintf "+ %s='%A' %s" propertyName property key
        |> trace

    let updatingProperty key propertyName property =
        sprintf "* %s='%A' %s" propertyName property key
        |> trace

    let unmountedProperty key propertyName = 
        sprintf "- %s %s" propertyName key
        |> trace

    let mountingService ancestorKey index nestedKey =
        sprintf "%s[%d] += %s" ancestorKey index nestedKey
        |> trace
        
    let unmountingService ancestorKey nestedKey = 
        sprintf "%s[?] -= %s" ancestorKey nestedKey 
        |> trace

#else

module Trace = 
    let renderingComponent _ _ = ()
    let mountingProperty _ _ _ = ()
    let updatingProperty _ _ _ = ()
    let unmountedProperty _ _  = ()
    let mountingService _ _ _ = ()
    let unmountingService _ _ = ()

#endif
 