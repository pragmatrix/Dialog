namespace FsReact

(*

*)

type PropertyReader<'target> = 
    {
        readers: Map<string, 'target -> obj>
    }
    with
    member this.read (target: 'target) (decon: 'property -> 'value) : 'value = 
        let name = typedefof<'property>.Name
        match this.readers.TryFind name with
        | Some reader -> reader target |> unbox |> decon
        | None ->
        failwithf "type '%s' does not support reading property '%s'" typedefof<'target>.Name name

    member this.extend (parent: PropertyReader<'parent>) : PropertyReader<'target> =
        let promoteReader f = 
            fun t -> t |> box |> unbox |> f

        let promotedReaders = 
            parent.readers 
            |> Map.toSeq 
            |> Seq.map (fun (s, f) -> (s, promoteReader f) )

        { readers = promotedReaders |> Seq.fold (fun m (k, v) -> m.Add(k, v)) this.readers }
       
(* 
    Defines property modification functions for a specific target

    todo:
        - use Dictionaries instead of maps for performance
        - we might need readers to read out properties directly from the components.
        - default values should also be set when the target is instantiated.
*)

type MountedProperty =
    abstract update: obj -> MountedProperty
    abstract unmount: unit -> unit

type PropertyAccessor<'target> = 
    { 
        mounters: Map<string, 'target -> obj -> (unit -> unit)>;
        writers: Map<string, 'target -> obj -> unit>;
        defaultValues: Map<string, obj>;
    }

    with 
   
    member this.mount target identity property =
        let name = property.GetType().Name
        match this.mounters.TryFind name with
        | Some mounter ->
            let remount = this.mount target identity
            let current = ref property
            Trace.mountingProperty identity name property
            let unmounter = mounter target property
            
            {
                new MountedProperty with
                    member mp.update property =
                        if (property = !current) then
                            mp
                        else
                        match this.writers.TryFind name with
                        | Some writer -> 
                            Trace.updatingProperty identity name property
                            writer target property; mp                            
                        | None -> mp.unmount(); remount property
                    member mp.unmount() = 
                        unmounter()
                        Trace.unmountedProperty identity name
            }
        | None ->
        match this.writers.TryFind name with
        | Some writer -> 
            let current = ref property
            Trace.mountingProperty identity name property
            writer target property
            { 
                new MountedProperty with
                    member mp.update property =
                        if !current <> property then
                            Trace.updatingProperty identity name property
                            writer target property
                            current := property
                        mp
                    member mp.unmount() =
                        match this.defaultValues.TryFind name with
                        | Some value -> 
                            if !current <> value then
                                Trace.updatingProperty identity name property
                                writer target value
                        | None -> ()
                        Trace.unmountedProperty identity name
            }
        | None ->
            failwithf "native type '%s' does not support mounting / writing property '%s'" typedefof<'target>.Name name

(*
    member this.derived() : PropertyWriter<'derived> = 
        let dMap = 
            this.map
            |> Map.map (fun _ f -> (fun (d:'derived) p -> f (d :> obj :?> 'target) p))
        { map = dMap }
*)

module PropertyAccessor =

    let inline ( -- ) l r = l r

    // reader

    let readerFor<'target> : PropertyReader<'target> = 
        { readers = Map.empty }

    let reader (f: 'target -> 'property) (this: PropertyReader<'target>) =
        let name = typedefof<'property>.Name
        let reader target = f target |> box
        { this with readers = this.readers.Add(name, reader) }

    // writer

    let accessor<'target> : PropertyAccessor<'target> = 
        { mounters = Map.empty; writers = Map.empty; defaultValues = Map.empty }
    
    let mounter (f: 'target -> 'property -> (unit -> unit)) (this: PropertyAccessor<'target>) = 
        let name = typedefof<'property>.Name
        let mounter target property = f target (unbox property)
        { this with mounters = this.mounters.Add(name, mounter) }
    
    let writer (f: 'target -> 'property -> unit) (this: PropertyAccessor<'target>) = 
        let name = typedefof<'property>.Name
        let writer target property = f target (unbox property)
        { this with writers = this.writers.Add(name, writer) }

    let defaultValue (v:'property) (this : PropertyAccessor<'target>) = 
        let name = v.GetType().Name
        { this with defaultValues = this.defaultValues.Add(name, box v) }