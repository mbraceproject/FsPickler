#r "bin/Release/FsPickler.dll"

open Nessos.FsPickler
open Nessos.FsPickler.Combinators


let bench loops (value : 'T) (pickler : Pickler<'T>) =
    for i = 1 to loops do
        pickle pickler value |> unpickle pickler |> ignore


type Person =
    {
        Address : string
        Age : int
        Name : string
    }

let makePerson name age address =
    {
        Address = address
        Age = age
        Name = name
    }

// reflection-based generation
let p = Pickler.auto<Person>

// incremental
let p' =
    Pickler.product makePerson
    ^+ Pickler.field (fun p -> p.Name) Pickler.string
    ^+ Pickler.field (fun p -> p.Age) Pickler.int
    ^. Pickler.field (fun p -> p.Address) Pickler.string

// straightforward wrap
let p'' = 
    Pickler.triple Pickler.string Pickler.int Pickler.string 
    |> Pickler.wrap 
        (fun (addr,age,name) -> { Address = addr ; Age = age ; Name = name }) 
        (fun p -> p.Address, p.Age, p.Name)

#time

let instance = { Address = "Athens" ; Age = 28 ; Name = "eirik" }

bench 100000 instance p // ~ 470ms
bench 100000 instance p' // ~ 680ms
bench 100000 instance p'' // ~ 490ms