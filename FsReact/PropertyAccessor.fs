namespace FsReact

(*

*)


module Map = 
    let joinSeq (left: Map<'a, 'b>) (right: ('a*'b) seq) = 
        right
        |> Seq.fold (fun (m:Map<_,_>) (k, v) -> m.Add(k, v)) left

    let join (left:Map<'a,'b>) (right:Map<'a,'b>) = 
        joinSeq left (right |> Map.toSeq)

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

type PropertyWriter<'target> = 
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

    member this.extend (parent: PropertyWriter<'parent>) : PropertyWriter<'target> =
        let promote f t = t |> box |> unbox |> f

        let promotedWriters = 
            parent.writers 
            |> Map.toSeq 
            |> Seq.map (fun (s, f) -> (s, promote f) )

        let promotedMounters = 
            parent.mounters 
            |> Map.toSeq 
            |> Seq.map (fun (s, f) -> (s, promote f) )

        { this with 
            writers = Map.joinSeq this.writers promotedWriters
            mounters = Map.joinSeq this.mounters promotedMounters
            defaultValues = Map.join this.defaultValues parent.defaultValues
            }

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

    let writerFor<'target> : PropertyWriter<'target> = 
        { mounters = Map.empty; writers = Map.empty; defaultValues = Map.empty }
    
    let mounter (f: 'target -> 'property -> (unit -> unit)) (this: PropertyWriter<'target>) = 
        let name = typedefof<'property>.Name
        let mounter target property = f target (unbox property)
        { this with mounters = this.mounters.Add(name, mounter) }
    
    let writer (f: 'target -> 'property -> unit) (this: PropertyWriter<'target>) = 
        let name = typedefof<'property>.Name
        let writer target property = f target (unbox property)
        { this with writers = this.writers.Add(name, writer) }

    let defaultValue (v:'property) (this : PropertyWriter<'target>) = 
        let name = v.GetType().Name
        { this with defaultValues = this.defaultValues.Add(name, box v) }