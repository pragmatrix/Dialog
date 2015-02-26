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
    
    let mountingProperty resourceKey propertyName property = 
        sprintf "+ %s='%A' %s" propertyName property resourceKey
        |> trace

    let updatingProperty resourceKey propertyName property =
        sprintf "* %s='%A' %s" propertyName property resourceKey
        |> trace

    let unmountedProperty resourceKey propertyName = 
        sprintf "- %s %s" propertyName resourceKey
        |> trace

    let mountingResource ancestorKey index nestedKey =
        sprintf "%s[%d] += %s" ancestorKey index nestedKey
        |> trace
        
    let unmountedResource ancestorKey nestedKey = 
        sprintf "%s[?] -= %s" ancestorKey nestedKey 
        |> trace

#else

module Trace = 
    let render componentName = ()
    let mountProperty resourceKey propertyName value = ()
    let unmountProperty resourceKey propertyName = ()
    let updateProperty resourceKey propertyName value = ()
    let mountNested ancestorKey nestedKey index = ()
    let unmountNested ancestorKey nestedKey index = ()

#endif
 