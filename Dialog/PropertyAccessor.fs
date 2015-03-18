namespace Dialog

(* 
    A PropertyAccessor defines property modification functions for a specific target.

    todo:
        - use Dictionaries instead of maps for performance
        - default values should also be set when the target is instantiated.
*)

module Map = 
    let joinSeq (left: Map<'a, 'b>) (right: ('a*'b) seq) = 
        right
        |> Seq.fold (fun (m:Map<_,_>) (k, v) -> m.Add(k, v)) left

    let join (left:Map<'a,'b>) (right:Map<'a,'b>) = 
        joinSeq left (right |> Map.toSeq)
       

type MountedProperty =
    abstract update: obj -> MountedProperty
    abstract unmount: unit -> unit


type PropertyAccessor<'target> = 
    { 
        readers: Map<string, 'target -> obj>;
        writers: Map<string, 'target -> obj -> unit>;

        mounters: Map<string, 'target -> Reference -> obj -> (unit -> unit)>;
    }

    member this.read (target: 'target) (decon: 'property -> 'value) : 'value = 
        let name = typedefof<'property>.Name
        match this.readers.TryFind name with
        | Some reader -> reader target |> unbox |> decon
        | None ->
        failwithf "type '%s' does not support reading property '%s'" typedefof<'target>.Name name
   
    member this.mount target identity property =
        let name = property.GetType().Name
        let reference = 
            {
                new Reference with
                    member __.get p = this.read target p
            }
        match this.mounters.TryFind name with
        | Some mounter ->
            let current = ref property
            let remount = this.mount target identity
            Trace.mountingProperty identity name property
            let unmounter = mounter target reference property

            
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
            let read = this.readers.[name]
            let previous = read target
            Trace.mountingProperty identity name property
            writer target property
            { 
                new MountedProperty with
                    member mp.update property =
                        // need to read back (value may be changed)
                        // this may have a performance impact, but handling this problem later in the
                        // context of a general invaliation based system makes more sense.
                        
                        // also note, if tracing would be disabled, a read would not be required.
                        
                        let current = read target
                        if current <> property then
                            Trace.updatingProperty identity name property
                            writer target property
                        mp
                    member mp.unmount() =
                        let current = read target
                        if current <> previous then
                                Trace.updatingProperty identity name property
                                writer target previous
                        Trace.unmountedProperty identity name
            }
        | None ->
            failwithf "native type '%s' does not support mounting / writing property '%s'" typedefof<'target>.Name name

    member this.extend (parent: PropertyAccessor<'parent>) : PropertyAccessor<'target> =
        let promote f t = t |> box |> unbox |> f

        let promotedReaders = 
            parent.readers 
            |> Map.toSeq 
            |> Seq.map (fun (s, f) -> (s, promote f) )

        let promotedWriters = 
            parent.writers 
            |> Map.toSeq 
            |> Seq.map (fun (s, f) -> (s, promote f) )

        let promotedMounters = 
            parent.mounters 
            |> Map.toSeq 
            |> Seq.map (fun (s, f) -> (s, promote f) )

        { this with 
            readers = Map.joinSeq this.readers promotedReaders
            writers = Map.joinSeq this.writers promotedWriters
            mounters = Map.joinSeq this.mounters promotedMounters
            }

module PropertyAccessor =

    let inline ( -- ) l r = l r

    let accessorFor<'target> : PropertyAccessor<'target> = 
        { readers = Map.empty; mounters = Map.empty; writers = Map.empty }

    // reader

    let reader (f: 'target -> 'property) (this: PropertyAccessor<'target>) =
        let name = typedefof<'property>.Name
        let reader target = f target |> box
        { this with readers = this.readers.Add(name, reader) }

    // modifications

    let mounter (f: 'target -> 'property -> (unit -> unit)) (this: PropertyAccessor<'target>) = 
        let name = typedefof<'property>.Name
        let mounter target reference property = f target (unbox property)
        { this with mounters = this.mounters.Add(name, mounter) }

    let eventMounter (f: 'target -> Reference -> 'property -> (unit -> unit)) (this: PropertyAccessor<'target>) = 
        let name = typedefof<'property>.Name
        let mounter target reference property = f target reference (unbox property)
        { this with mounters = this.mounters.Add(name, mounter) }
    
    let writer (f: 'target -> 'property -> unit) (this: PropertyAccessor<'target>) = 
        let name = typedefof<'property>.Name
        let writer target property = f target (unbox property)
        { this with writers = this.writers.Add(name, writer) }