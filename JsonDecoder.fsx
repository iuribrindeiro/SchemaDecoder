open System.Text.Json
open System

let printInfo e =
    printfn "%A" e
    e

module Decode =

    type ErrorType =
        | Custom of msg: string
        | BadType of given: JsonValueKind * expected: JsonValueKind
        | EmptyValue of given: JsonValueKind * expected: JsonValueKind
        | FieldError of FieldError
        | FieldNotFound of fieldName: string
        | Multiple of ErrorType list

    and FieldError = { Error: ErrorType; FieldName: string }

    and FieldSchemaType = { FieldName: string; Type: DecoderSchemaType }

    and ObjectSchemaType = { Name: string; Fields: FieldSchemaType list }

    and DecoderSchemaType =
        | Raw of kind: JsonValueKind
        | Array of DecoderSchemaType
        | Optional of DecoderSchemaType
        | Object of ObjectSchemaType

    type FieldDecoder<'a> = { Type: FieldSchemaType; Decode: JsonElement -> Result<'a, ErrorType> }

    type Decoder<'a> = { 
        Scheme: DecoderSchemaType
        Decode:  JsonElement -> Result<'a, ErrorType> }

    let decode d (json: string) =
        json
        |> JsonDocument.Parse
        |> _.RootElement
        |> d.Decode

    let getScheme d =
        d.Scheme 

    let fieldError fieldName error =
        { Error = error; FieldName = fieldName } |> FieldError

    let fieldNotFound fieldName = FieldNotFound fieldName

    let string =
        let decodeStr (json: JsonElement) =
            match json.ValueKind with
            | JsonValueKind.String -> json.GetString() |> Ok
            | JsonValueKind.Null | JsonValueKind.Undefined -> 
                EmptyValue(json.ValueKind, JsonValueKind.String) |> Error
            | _ -> BadType (json.ValueKind, JsonValueKind.String) |> Error

        { Scheme = Raw JsonValueKind.String; Decode = decodeStr }

    let int =
        let decodeInt (json: JsonElement) =
            match json.ValueKind with
            | JsonValueKind.Number -> json.GetInt32() |> Ok
            | JsonValueKind.Null | JsonValueKind.Undefined -> 
                EmptyValue(json.ValueKind, JsonValueKind.Number) |> Error
            | _ -> BadType (json.ValueKind, JsonValueKind.Number) |> Error
        { Scheme = Raw JsonValueKind.Number; Decode = decodeInt }

    let option decoder =
        let decodeOpt (json: JsonElement) =
            let handleOptField =
                function
                | Error(FieldNotFound _) 
                | Error(EmptyValue _) -> Ok None
                | Error e -> Error e
                | Ok e -> Some e |> Ok

            match json.ValueKind with
            | JsonValueKind.Null
            | JsonValueKind.Undefined -> Ok None
            | _ -> decoder.Decode json |> handleOptField

        { Scheme = Optional decoder.Scheme; Decode = decodeOpt }

    let field (fieldName: string) decoder : FieldDecoder<'a> =
        let decodeField (json: JsonElement) =
            match json.ValueKind with
            | JsonValueKind.Object ->
                match json.TryGetProperty fieldName with
                | true, value ->
                    value |> decoder.Decode |> Result.mapError (fieldError fieldName)
                | false, _ -> fieldNotFound fieldName |> Error
            | JsonValueKind.Null | JsonValueKind.Undefined -> 
                EmptyValue(json.ValueKind, JsonValueKind.Object) |> Error
            | _ -> BadType (json.ValueKind, JsonValueKind.Object) |> Error
        
        { Type = { FieldName = fieldName; Type = decoder.Scheme }; Decode = decodeField }

    let toUnit _ = ()

    let obj2 name f (decoder1: FieldDecoder<'a>) (decoder2: FieldDecoder<'b>) =
        let decodeFields (json: JsonElement) =
            match decoder1.Decode json, decoder2.Decode json with
            | Ok v1, Ok v2 -> f v1 v2 |> Ok
            | r1, r2 -> 
                [r1 |> Result.map toUnit; r2 |> Result.map toUnit;] 
                |> List.choose (function Ok _ -> None | Error e -> Some e) 
                |> Multiple
                |> Error

        { Scheme = Object { Name = name; Fields = [decoder1.Type; decoder2.Type] }; Decode = decodeFields }

type Leader = { name: string; salary: int }
type Customer = { name: string; age: int option; leader: Leader }

let rec collectErr e =
            match e with
            | Decode.Multiple errs -> 
                errs |> List.collect collectErr
            | e -> [e]

type ObjectDecoderBuilderState<'a> = { Decoder: Decode.Decoder<'a> }

type ObjectDecoderBuilder(name: string) =

    //called when merging two fields as one decoder (let! .. and! .. return)
    member this.MergeSources(source1: Decode.FieldDecoder<'a>, source2: Decode.FieldDecoder<'b>): ObjectDecoderBuilderState<'a * 'b> =
        let decodeFields (json: JsonElement) =
            match source1.Decode json, source2.Decode json with
            | Ok v1, Ok v2 -> (v1, v2) |> Ok
            | r1, r2 -> 
                [r1 |> Result.map Decode.toUnit; r2 |> Result.map Decode.toUnit;] 
                |> List.collect (function Ok _ -> [] | Error e -> collectErr e) 
                |> Decode.Multiple
                |> Error

        {
            Decoder = { Scheme = Decode.Object { Name = name; Fields = [source1.Type; source2.Type] } 
                        Decode = decodeFields }
        }

    //called when merging a new field into a decoder (let! .. and! .. and! .. return)
    member this.MergeSources(source1: Decode.FieldDecoder<'a>, source2: ObjectDecoderBuilderState<'b>): ObjectDecoderBuilderState<'a * 'b> =
        let decodeFields (json: JsonElement) =
            match source1.Decode json, source2.Decoder.Decode json with
            | Ok v1, Ok v2 -> (v1, v2) |> Ok
            | r1, r2 -> 
                [r1 |> Result.map Decode.toUnit; r2 |> Result.map Decode.toUnit;] 
                |> List.collect (function Ok _ -> [] | Error e -> collectErr e) 
                |> Decode.Multiple 
                |> Error

        let fieldsTypes =
            match source2.Decoder.Scheme with
            | Decode.Object { Fields = fields } -> fields
            | _ -> []

        {
            Decoder = { Scheme = Decode.Object { Name = name; Fields = source1.Type :: fieldsTypes }
                        Decode = decodeFields }
        }

    //called when mapping a list of fields into a decoder
    member this.BindReturn(source: ObjectDecoderBuilderState<'a>, f: 'a -> 'b): Decode.Decoder<'b> =
        {
            Scheme = source.Decoder.Scheme
            Decode = fun json -> source.Decoder.Decode json |> Result.map f
        }

    //called when mapping a single field into a decoder
    member this.BindReturn(source: Decode.FieldDecoder<'a>, f: 'a -> 'b): Decode.Decoder<'b> =
        {
            Scheme = Decode.Object { Name = name; Fields = [source.Type] }
            Decode = fun json -> source.Decode json |> Result.map f
        }

open Decode

let objectDecoder(name: string) =
    ObjectDecoderBuilder name

let leaderDecoder =
    objectDecoder "Leader" {
        let! name = string |> field "name" 
        and! salary = int |> field "salary"
        
        return { name = name; salary = salary }
    }

let customerDecoder =
    objectDecoder "Customer" {
        let! age = option int |> field "age"
        // and! name = string |> field "name"
        and! leader = leaderDecoder |> field "leader"
        
        return { age = age; name = "name"; leader = leader }
    }

(*
    let leaderCodec =
        codec "Leader" {
            let! name  = 
                string "name" 
                |> title "Name" 
                |> description "Leader name" 
                |> example "John"
                |> encodeFrom _.name
                
            and! salary =
                decimal "salary"
                |> encodeFrom _.salary

            return { name = name; salary = salary }
        }

    let customerCodec =
        codec "Customer" {
            let! age =
                int |> field "age"
                |> title "Age" 
                |> description "Customer age" 
                |> example 29
                |> encodeFrom _.age

            and! name = 
                string |> field "name" 
                |> title "Name"
                |> description "Customer name" 
                |> example "Iuri"
                |> encodeFrom _.name

            and! leader = 
                object |> field "leader" leaderCodec.Schema
                |> title "Leader"
                |> description "Customer leader"
                |> example leaderCodec.Schema.Example
                |> encodeFrom _.leader

            return { age = age; name = name; leader = leader }
        }

    let leaderEncoder =
        encoder "Leader" {
            encode _.name <| (string |> field "name")
                
            encode _.salary
                decimal 
                |> field "salary"
                |> title "Salary"
                |> description "Leader salary" 
                |> example 30
        }

    let leaderDecoder =
        decoder "Leader" {
            let! name = string |> field "name"
            
            and! salary = 
                int 
                |> field "salary" 
                |> title "Salary"
                |> description "Leader salary" 
                |> example 30
            
            return { name = name; salary = salary }
        }

    let encodeLeader =
        Codec.encodeWith leaderCodec

    let decodeLeader =
        Codec.decodeWith leaderCodec
*)

type Field<'a, 'b> = { Name: string; Encoder: 'a -> string; Getter: 'b -> 'a }

type ObjectEncoderBuilder() =
    member val private Fields = [] with get, set
    
    [<CustomOperation("field")>]
    member this.Field(name: string, encoder: 'b -> string, getter: 'a -> 'b) =
        let l = { Name = name; Encoder = encoder; Getter = getter } :: this.Fields
        this.Fields <- l

    member this.Delay(f: unit -> Field<'a, 'b>) =
        let field = f()
        let l = field :: this.Fields
        this.Fields <- l
        this

let objectEncoder =
    ObjectEncoderBuilder()

let customerEncoder =
    objectEncoder {
        field "name" Encode.string _.name
        field "age" Encode.string _.age >> string
    }

customerDecoder
|> getScheme
|> printInfo


"""{"name": "Iuri", "age": 29, "leader": {"name": "John", "salary": 30}}"""
|> decode customerDecoder
|> Result.mapError printInfo
|> Result.iter (printfn "%A")