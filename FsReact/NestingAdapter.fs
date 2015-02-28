namespace FsReact


(*
    A nesting adapter describes how to mount and unmount nested native elements.
*)


type NestingAdapter<'target>(mounter: 'target -> int -> obj -> unit, unmounter: 'target -> obj -> unit) =
    member this.mount ancestor index nested = mounter ancestor index nested
    member this.unmount ancestor nested = unmounter ancestor nested
    abstract canMount : obj -> bool
    default this.canMount o = false

    static member invalid<'target>() = 
        let invalidMounter target _ _ = 
            failwithf "target %s does not support nested elements" (target.ToString())
            
        let invalidUnmounter target _ =
            failwithf "target %s does not support nested elements" (target.ToString())

        NestingAdapter<'target>(invalidMounter, invalidUnmounter)

type NestingAdapter<'target, 'nested>(mounter: 'target -> int -> 'nested -> unit, unmounter: 'target -> 'nested -> unit) = 
    inherit NestingAdapter<'target>((fun t i n -> mounter t i (unbox n)), (fun t n -> unmounter t (unbox n)))

    member this.mount ancestor index nested = mounter ancestor index nested
    member this.unmount ancestor nested = unmounter ancestor nested
    override this.canMount obj = obj :? 'nested


