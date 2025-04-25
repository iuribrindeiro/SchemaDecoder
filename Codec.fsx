#r "nuget: FsToolkit.ErrorHandling"

open System.Text.Json
open System.Text.Json.Nodes


type DecodeError =
    | InvalidType of expect: JsonValueKind * actual: JsonValueKind
    | InvalidFormat of string
    | FieldError of fieldName: string * error: FieldError

and FieldError =
    | Missing
    | DecodeError of DecodeError list

type DecodeStrError =
    | DecodeError of DecodeError list
    | JsonParseError of exn

type Encoder<'a> = 'a -> JsonNode

type Decoder<'a> = JsonElement -> Result<'a, DecodeError list>

type Schema =
    | Object of ObjectSchema
    | Array of Schema
    | Primitive of PrimitiveSchemaType

and ObjectSchema =
    { Name: string
      Fields: (string * Schema) list }

and PrimitiveSchemaType =
    { Type: JsonValueKind
      Example: obj option
      Description: string option
      Required: bool }

type Codec<'a> = { Decoder: Decoder<'a>; Encoder: Encoder<'a>; Schema: Schema }

type SchemaDescription<'a> = { Description: string option; Example: 'a option }

[<RequireQualifiedAccess>]
module JsonValue =
    let toJsonNode (json: JsonValue): JsonNode =
        json

[<RequireQualifiedAccess>]
module JsonObject =
    let toJsonNode (json: JsonObject): JsonNode =
        json

let toSchema schemaDesc type' =
        { Type = type'
          Example = schemaDesc.Example |> Option.map box
          Description = schemaDesc.Description
          Required = true }

let toCodec schemaDesc type' decoder encoder =
    { Decoder = decoder; Encoder = encoder; Schema = toSchema schemaDesc type' |> Primitive }

type SchemaDescriptionBuilder<'a>(type', encoder: 'a -> JsonValue, decoder: Decoder<'a>) =
    [<CustomOperation("description")>]
    member _.Description(field: SchemaDescription<'a>, description: string) =
        { field with
            Description = Some description }

    [<CustomOperation("example")>]
    member _.Example(field: SchemaDescription<'a>, example: 'a) = { field with Example = Some example }

    member _.Yield(_) =
        { Description = None
          Example = None }

    member _.Run(f: SchemaDescription<'a>) = 
            toCodec f type' decoder (encoder >> JsonValue.toJsonNode)

[<RequireQualifiedAccess>]
module Encoder =

    let addField fieldName (v: JsonNode) (jObject: JsonObject) = 
        jObject
        |> _.Add(fieldName, v)
        jObject

    let createObjWithField (fieldName: string) (v: JsonNode) : JsonObject =
        JsonObject()
        |> addField fieldName v

[<RequireQualifiedAccess>]
module Decoder =

    let string: Decoder<string> = fun json -> 
                match json.ValueKind with
                | JsonValueKind.String -> Ok(json.GetString())
                | _ -> 
                    (JsonValueKind.String, json.ValueKind)
                    |> InvalidType
                    |> List.singleton
                    |> Error

    let decimal: Decoder<decimal> = 
        fun json ->
                match json.ValueKind with
                | JsonValueKind.Number ->
                    match json.TryGetDecimal() with
                    | true, value -> Ok value
                    | false, _ -> 
                        "Could not convert the value to a decimal"
                        |> InvalidFormat
                        |> List.singleton
                        |> Error
                | _ -> 
                    (JsonValueKind.Number, json.ValueKind)
                    |> InvalidType
                    |> List.singleton
                    |> Error

    let int: Decoder<int> = 
        fun json ->
                match json.ValueKind with
                | JsonValueKind.Number ->
                    match json.TryGetInt32() with
                    | true, value -> Ok value
                    | false, _ -> 
                        "Could not convert the value to an int"
                        |> InvalidFormat
                        |> List.singleton
                        |> Error
                | _ -> 
                    (JsonValueKind.Number, json.ValueKind)
                    |> InvalidType
                    |> List.singleton
                    |> Error


[<RequireQualifiedAccess>]
module Codec =
    let objectToNode (jObject: JsonObject): JsonNode =
        jObject

    let string = SchemaDescriptionBuilder<string>(JsonValueKind.String, JsonValue.Create, Decoder.string)

    let decimal = SchemaDescriptionBuilder<decimal>(JsonValueKind.Number, JsonValue.Create, Decoder.decimal)

    let int = SchemaDescriptionBuilder<int>(JsonValueKind.Number, JsonValue.Create, Decoder.int)

    let decoder (json: JsonElement) (fieldName: string) ({ Decoder = decoder }: Codec<'a>) =
        let fieldError err =
            FieldError(fieldName, err)
            |> List.singleton

        match json.ValueKind with
        | JsonValueKind.Object ->
            match json.TryGetProperty fieldName with
            | true, value -> 
                decoder value
                |> Result.mapError (FieldError.DecodeError >> fieldError)
            | false, _ ->
                Missing
                |> fieldError
                |> Error
        | _ ->
            InvalidType (JsonValueKind.Object, json.ValueKind)
            |> List.singleton
            |> Error

    let toJsonString (json: JsonNode): string =
        json.ToJsonString()

    let toJsonElement (jsonStr: string) =
        try
            JsonDocument.Parse(jsonStr).RootElement
            |> Ok
        with
        | ex ->
            Error ex

    let encode codecState =
        codecState.Encoder

    let decode codecState =
        codecState.Decoder

    let decodeStr codecState =
        toJsonElement 
        >> Result.mapError JsonParseError 
        >> Result.bind (decode codecState >> Result.mapError DecodeError)

    [<AutoOpen>]
    module CE =
        type ObjectCodecState<'s, 't> =
                { ObjectEncoder: 's -> JsonObject
                  CurrentDecoder: Decoder<'t>
                  CurrentSchema: ObjectSchema }

        let field fieldName codec =
            fieldName, codec

        let encodeFrom (getter: 's -> 't) (fieldName, codec: Codec<'t>) =
            codec, fieldName, getter

        type CodecObjectBuilder(name: string) =
            //used in let! .. and! .. and! .. return
            member _.BindReturn
                (
                    (fieldCodec, fieldName, getter): Codec<'t> * string * ('s -> 't),
                    f: 't -> 's
                ): ObjectCodecState<'s, 's> =

                { ObjectEncoder = getter >> encode fieldCodec >> Encoder.createObjWithField fieldName
                  CurrentSchema = { Name = name; Fields = [ fieldName, fieldCodec.Schema ] }
                  CurrentDecoder = fieldCodec.Decoder >> Result.map f }

            //used in let! .. and! .. return
            member _.BindReturn(source: ObjectCodecState<'s, 't>, f: 't -> 's) =
                { ObjectEncoder = source.ObjectEncoder
                  CurrentSchema = source.CurrentSchema
                  CurrentDecoder = source.CurrentDecoder >> Result.map f }

            //used in let! .. and! .. return
            member _.MergeSources(
                (fieldCodec1, fieldName1, getter1): Codec<'t1> * string * ('s -> 't1),
                (fieldCodec2, fieldName2, getter2): Codec<'t2> * string * ('s -> 't2)): ObjectCodecState<'s, 't1 * 't2> =
                let encodeFields s =
                    let t1 = getter1 s |> encode fieldCodec1
                    let t2 = getter2 s |> encode fieldCodec2
                    
                    t1
                    |> Encoder.createObjWithField fieldName1
                    |> Encoder.addField fieldName2 t2

                let decodeFields (json: JsonElement) : Result<('t1 * 't2), DecodeError list> =
                    match fieldCodec1 |> decoder json fieldName1, fieldCodec2 |> decoder json fieldName2 with
                    | Ok value1, Ok value2 -> Ok (value1, value2)
                    | r1, r2 ->
                        [r1 |> Result.map ignore; r2 |> Result.map ignore]
                        |> List.collect (function Ok _ -> [] | Error e -> e)
                        |> Error

                { ObjectEncoder = encodeFields
                  CurrentSchema =
                      { Name = name; Fields = [ fieldName1, fieldCodec1.Schema; fieldName2, fieldCodec2.Schema ] }
                  CurrentDecoder = decodeFields }

            //used in let! .. and! .. and! .. return
            member _.MergeSources(
                (fieldCodec1, fieldName1, getter1): Codec<'t1> * string * ('s -> 't1),
                source: ObjectCodecState<'s, 't2>): ObjectCodecState<'s, 't1 * 't2> =
                let encodeFields s =
                    let t1 = getter1 s |> encode fieldCodec1
                    source.ObjectEncoder s
                    |> Encoder.addField fieldName1 t1
                
                let decodeFields (json: JsonElement) : Result<('t1 * 't2), DecodeError list> =
                    match fieldCodec1 |> decoder json fieldName1, source.CurrentDecoder json with
                    | Ok value1, Ok value2 -> Ok (value1, value2)
                    | r1, r2 ->
                        [r1 |> Result.map ignore; r2 |> Result.map ignore]
                        |> List.collect (function Ok _ -> [] | Error e -> e)
                        |> Error

                { ObjectEncoder = encodeFields
                  CurrentSchema = { source.CurrentSchema with Fields = [ fieldName1, fieldCodec1.Schema ] @ source.CurrentSchema.Fields }
                  CurrentDecoder = decodeFields }

            member _.Run(f: ObjectCodecState<'s, 's>) =
                { Encoder = f.ObjectEncoder >> JsonObject.toJsonNode
                  Schema = Object f.CurrentSchema
                  Decoder = f.CurrentDecoder }

    let object = CodecObjectBuilder

open Codec.CE

type Country = { CountryName: string; Digit: int }
type Address = { Street: string; City: string; Country: Country }
type Person = { Name: string; Salary: decimal; Age: int; Address: Address }

let countryCodec =
    Codec.object "Country" {
        let! name = 
            Codec.string {
                description "Country name"
                example "USA"
            } |> field "name" |> encodeFrom _.CountryName

        and! digit = 
            Codec.int {
                description "Country digit"
                example 1
            } |> field "digit" |> encodeFrom _.Digit

        return { CountryName = name; Digit = digit }
    }

let addressCodec =
    Codec.object "Address" {
        let! street = 
            Codec.string {
                description "Street name"
                example "Main St"
            } |> field "street" |> encodeFrom _.Street

        and! city = 
            Codec.string {
                description "City name"
                example "New York"
            } |> field "city" |> encodeFrom _.City

        and! country =
            countryCodec |> field "country" |> encodeFrom _.Country

        return { Street = street; City = city; Country = country }
    }

let personCodec =
    Codec.object "Person" {
        let! name = 
            Codec.string {
                description "Person name"
                example "John"
            } |> field "name" |> encodeFrom _.Name

        and! salary = 
            Codec.decimal {
                description "Person salary"
                example 30.42m
            } |> field "salary" |> encodeFrom _.Salary

        and! age = 
            Codec.int {
                description "Person age"
                example 28
            } |> field "age" |> encodeFrom _.Age

        and! address =
            addressCodec |> field "address" |> encodeFrom _.Address
        
        return { Name = name; Salary = salary; Age = age; Address = address }
    }
let decodePerson =
    personCodec
    |> Codec.decodeStr

let encodePerson =
    personCodec
    |> Codec.encode

let encodeAddress =
    addressCodec
    |> Codec.encode

printfn "Encoding Person: %A" personCodec.Schema

let person = { Name = "John"; Salary = 30.42m; Age = 28; Address = { Street = "Main St"; City = "New York"; Country = { CountryName = "Brazil"; Digit = 55 } } }
let json = person |> encodePerson |> Codec.toJsonString

printfn "Encoded JSON: %s" json

let decoded = 
    json
    |> decodePerson

printfn "Decoded Person: %A" decoded

let deserialized = json |> JsonSerializer.Deserialize<Person>
