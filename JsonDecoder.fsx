open System.Text.Json
open System

let printInfo e =
    printfn "%A" e
    e

[<RequireQualifiedAccess>]
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

    let scheme d =
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
                    match value.ValueKind with
                    | JsonValueKind.Null | JsonValueKind.Undefined -> 
                        FieldNotFound fieldName |> Error
                    | _ ->
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
type Customer = { name: string; age: int; leader: Leader }

let rec collectErr e =
            match e with
            | Decode.Multiple errs -> 
                errs |> List.collect collectErr
            | e -> [e]


type ObjectDecoderBuilder(name: string) =

    member this.Bind(fieldDecoder: Decode.FieldDecoder<'a>, f: 'a -> Decode.Decoder<'b>): Decode.Decoder<'b> =
        
        let decoder json = 
            fieldDecoder.Decode json
            |> Result.map f
            |> Result.bind (fun d -> d.Decode json)
            
        {
            Scheme = Decode.Object { Name = name; Fields = [fieldDecoder.Type] }
            Decode = decoder
        }

        
    member this.MergeSources(source1: Decode.FieldDecoder<'a>, source2: Decode.FieldDecoder<'b>): Decode.Decoder<'a * 'b> =
        let decodeFields (json: JsonElement) =
            match source1.Decode json, source2.Decode json with
            | Ok v1, Ok v2 -> (v1, v2) |> Ok
            | r1, r2 -> 
                [r1 |> Result.map Decode.toUnit; r2 |> Result.map Decode.toUnit;] 
                |> List.collect (function Ok _ -> [] | Error e -> collectErr e) 
                |> Decode.Multiple
                |> Error

        { Scheme = Decode.Object { Name = name; Fields = [source1.Type; source2.Type] }
          Decode = decodeFields }

    member this.MergeSources(source1: Decode.FieldDecoder<'a>, source2: Decode.Decoder<'b>): Decode.Decoder<'a * 'b> =
        let decodeFields (json: JsonElement) =
            match source1.Decode json, source2.Decode json with
            | Ok v1, Ok v2 -> (v1, v2) |> Ok
            | r1, r2 -> 
                [r1 |> Result.map Decode.toUnit; r2 |> Result.map Decode.toUnit;] 
                |> List.collect (function Ok _ -> [] | Error e -> collectErr e) 
                |> Decode.Multiple 
                |> Error

        let fieldsTypes =
            match source2.Scheme with
            | Decode.Object { Fields = fields } -> fields
            | _ -> []

        { Scheme = Decode.Object { Name = name; Fields = source1.Type :: fieldsTypes }
          Decode = decodeFields }
    member this.BindReturn(source: Decode.Decoder<'a>, f: 'a -> 'b): Decode.Decoder<'b> =
        {
            Scheme = source.Scheme
            Decode = fun json -> source.Decode json |> Result.map f
        }
    

    member this.Return(value: 'a): Decode.Decoder<'a> =
        {
            Scheme = Decode.Object { Name = name; Fields = [] }
            Decode = fun _ -> Ok value
        }


let objectDecoder(name: string) =
    ObjectDecoderBuilder name

let leaderDecoder =
    objectDecoder "Leader" {
        let! name = Decode.field "name" Decode.string
        and! salary = Decode.field "salary" Decode.int
        
        return { name = name; salary = salary }
    }

let customerDecoder =
    objectDecoder "Customer" {
        let! age =  Decode.int |> Decode.field "age"
        and! name = Decode.string |> Decode.field "name" 
        and! leader = leaderDecoder |> Decode.field "leader"

        return { age = age; name = name; leader = leader }
    }

customerDecoder
|> Decode.scheme
|> printInfo


"""{"names": "Iuri", "age": 29, "leader": {"name": "John", "sa": 30}}"""
|> Decode.decode customerDecoder
|> Result.mapError printInfo
|> Result.iter (printfn "%A")