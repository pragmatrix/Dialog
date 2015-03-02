namespace FsReact


(*
    A nesting adapter describes how to mount and unmount nested native elements.
*)


type NestingAdapter<'target>(mounter: 'target -> int -> obj -> unit, unmounter: 'target -> obj -> unit) =
    member this.mount ancestor index nested = mounter ancestor index nested
    member this.unmount ancestor nested = unmounter ancestor nested

    static member agnostic<'target>() = 
        let emptyMounter target _ _ = ()
        let emptyUnmounter target _ = ()
        NestingAdapter<'target>(emptyMounter, emptyUnmounter)

type NestingAdapter<'target, 'nested>(mounter: 'target -> int -> 'nested -> unit, unmounter: 'target -> 'nested -> unit) = 
    inherit NestingAdapter<'target>((fun t i n -> mounter t i (unbox n)), (fun t n -> unmounter t (unbox n)))

    member this.mount ancestor index nested = mounter ancestor index nested
    member this.unmount ancestor nested = unmounter ancestor nested


