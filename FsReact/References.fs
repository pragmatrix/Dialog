namespace FsReact

type Reference = 
    abstract get : ('property -> 'value) -> 'value

[<AutoOpen>]
module ReferenceExtensions = 
    type Reference with
        static member withProperties properties = 
            {
                new Reference with
                    member this.get decon = 
                        Properties.get decon properties
            }

        static member empty = Reference.withProperties []
