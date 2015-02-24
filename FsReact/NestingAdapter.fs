namespace FsReact


(*
    A nesting adapter describes how to mount and unmount nested native elements.
*)

type NestingAdapter<'target, 'nested>(mounter: 'target -> int -> 'nested -> unit, unmounter: 'target -> 'nested -> unit) = 
    member this.mount ancestor index element = mounter ancestor index element
    member this.unmount ancestor element = unmounter ancestor element

    member this.promoteTarget<'target>() = 
        let promotedMounter target index nested = 
            mounter (target |> box |> unbox) index nested

        let promotedUnmounted target nested = 
            unmounter (target |> box |> unbox) nested

        NestingAdapter<'target, 'nested>(promotedMounter, promotedUnmounted)

    member this.promoteNested<'nested'>() = 
        let promotedMounter target index nested = 
            mounter target index (nested :> obj :?> 'nested)

        let promotedUnmounter target nested = 
            unmounter target (nested :> obj :?> 'nested)

        NestingAdapter<'target, 'nested'>(promotedMounter, promotedUnmounter)

    static member invalid<'target, 'nested>() = 
        let invalidMounter target _ _ = 
            failwithf "target %s does not support nested elements" (target.ToString())
            
        let invalidUnmounter target _ =
            failwithf "target %s does not support nested elements" (target.ToString())

        NestingAdapter<'target, 'nested>(invalidMounter, invalidUnmounter)
    


    


