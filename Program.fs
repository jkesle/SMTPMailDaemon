module MailDaemon

open System
open System.IO
open System.Net
open System.Net.Sockets
open System.Text

type HeaderOfInterest =
    | From
    | To
    | Subject
    | Date
    | MessageID
    | Cc
    | ReplyTo
    | DKIMSignature

type Header =
    | OfInterest of HeaderOfInterest
    | Other of string

let handleClient: TcpClient -> unit = 
    fun client -> 
        use stream: NetworkStream = client.GetStream ()
        use reader: StreamReader = new StreamReader (stream, Encoding.UTF8)
        use writer: StreamWriter = new StreamWriter (stream, Encoding.UTF8)
        writer.NewLine <- "\r\n"
        writer.AutoFlush <- true

        let write: string -> unit =
            fun line ->
                writer.WriteLine line
                printfn "S: %s" line

        let read: unit -> string = fun () -> reader.ReadLine ()

        write "220 joshkesler.com SMTP Server Ready"

        let mutable from: string = ""
        let mutable toAddress: string = ""
        let mutable dataBuffer = StringBuilder()

        let rec loop: unit -> unit =
            fun () -> 
                let line = read ()
                printfn "C: %s" line
                match line with
                | s when s.StartsWith "HELO" || s.StartsWith "EHLO" -> write "250 Hello"
                | s when s.StartsWith "MAIL FROM:" -> 
                    from <- (s.Substring 10).Trim('<', '>', ' ')
                    write "250 OK"
                | "DATA" ->
                    write "354 End with <CR><LF>.<CR><LF>"
                    let rec readData: unit -> unit =
                        fun () ->  
                            let line: string = read ()
                            if line = "." then ()
                            else dataBuffer.AppendLine line |> ignore
                            readData ()
                    readData ()
                    write "250 Message accepted for delivery"
                    let timestamp = DateTime.UtcNow.ToString("yyyy-MM-ddTHH-mm-ss")
                    let maildir = "/var/mail/josh"
                    let filepath = Path.Combine(maildir, $"{timestamp}.eml")
                    if not (Directory.Exists maildir) then Directory.CreateDirectory maildir |> ignore
                    let content = $"From: {from}\r\nTo: {toAddress}\r\n\r\n{dataBuffer.ToString ()}"
                    File.WriteAllText(filepath, content)
                    dataBuffer.Clear () |> ignore
                | "Quit" -> write "221 Bye"
                | _ ->  
                    write "250 OK" 
                    loop ()
        loop ()
        client.Close ()
        
let startServer: unit -> Async<unit> =
    fun () -> 
        let listener = new TcpListener (IPAddress.Any, 25)
        listener.Start ()
        printfn "SMTP server listening on port 25..."

        let rec acceptLoop: unit -> Async<unit> =
            fun () -> async {
                let! client = listener.AcceptTcpClientAsync() |> Async.AwaitTask
                printfn "Accepted connection from %A" client.Client.RemoteEndPoint

                Async.Start(async {
                    try do! async { handleClient client }
                    with ex -> printfn "Error in client handler: %s" ex.Message
                })

                return! acceptLoop ()
            }

        acceptLoop ()

module Dkim =

    open System.Security.Cryptography
    open Org.BouncyCastle.OpenSsl
    open Org.BouncyCastle.Security
    open Org.BouncyCastle.Crypto
    open Org.BouncyCastle.Crypto.Parameters
        
    type Canonicalization =
        | Simple
        | Relaxed

    type DKIMSignature = { Version: int; Algorithm: string; Canonicalization: Canonicalization * Canonicalization; SigningDomain: string; Selector: string; HeadersToSign: Header list}

    let relaxHeader: string -> string -> string =
        fun name -> 
            fun value -> 
                let unfolded = value.Replace("\r\n", "").Replace("\n", "").Replace("\r", "")
                let normalized = System.Text.RegularExpressions.Regex.Replace(unfolded, @"\s+", " ").Trim()
                $"{name}:{normalized}\r\n"

    let canonicalizeHeaders: (string * string) list -> string list -> (string * string) =
        fun headers -> 
            fun headersToSign -> 
                let signedHeaders =
                    headersToSign
                    |> List.choose (fun header -> 
                        headers
                        |> List.tryFindBack (fun (name, _) -> name.ToLowerInvariant() = header.ToLowerInvariant())
                        |> Option.map (fun (name, value) -> header.ToLowerInvariant(), relaxHeader name value)
                    )
                let htag = signedHeaders |> List.map fst |> String.concat ":"
                let canonBlock = signedHeaders |> List.map snd |> String.concat ""
                htag, canonBlock

    let relaxBody: string -> string =
        fun body -> 
            let lines = body.Split([|"\r\n"; "\n"; "\r"|], StringSplitOptions.None)
            let canonLines =
                    lines
                    |> Array.map (fun line -> 
                        let collapsed = System.Text.RegularExpressions.Regex.Replace(line, @"\s+", " ")
                        collapsed.TrimEnd() + "\r\n")
                    |> Array.toList

            let trimmedLines = 
                canonLines
                |> List.rev
                |> List.skipWhile (fun l -> l.Trim () = "")
                |> List.rev
                
            String.Concat trimmedLines

    let computeBodyHash: string -> string =
        fun canonBody -> 
            let bytes = Encoding.UTF8.GetBytes canonBody
            use sha = SHA256.Create()
            let hash = sha.ComputeHash bytes
            hash 
            |> Convert.ToBase64String

    let buildDkimHeaderStub: string -> string -> string list -> string -> string =
        fun domain -> 
            fun selector -> 
                fun headersToSign -> 
                    fun bodyHash -> 
                        let headerVal = String.concat ":" headersToSign
                        [
                            "v=1"
                            "a=rsa-sha256"
                            "c=relaxed/relaxed"
                            $"d={domain}"
                            $"s={selector}"
                            $"h={headerVal}"
                            $"bh={bodyHash}"
                            "b="
                        ] |> String.concat "; "
                          |> fun content -> "DKIM-Signature: " + content

    let loadRsaKey: string -> RSACryptoServiceProvider =
        fun pemPath -> 
            use reader = new StringReader(File.ReadAllText(pemPath))
            let pemReader = new PemReader(reader)
            let keyObject = pemReader.ReadObject ()

            match keyObject with
            | :? AsymmetricCipherKeyPair as kp ->
                let privateKeyParams = kp.Private :?> RsaPrivateCrtKeyParameters
                let rsaParams = DotNetUtilities.ToRSAParameters(privateKeyParams)
                let rsa = new RSACryptoServiceProvider()
                rsa.ImportParameters(rsaParams)
                rsa
            | _ -> failwith "Invalid/Unsupported PEM key format"

    let signWithRsa: RSACryptoServiceProvider -> string -> string =
        fun rsa -> 
            fun data -> 
                let bytes = System.Text.Encoding.UTF8.GetBytes(data)
                rsa.SignData(bytes, CryptoConfig.MapNameToOID("SHA256")) |> Convert.ToBase64String

    let buildFinalDkimHeader: string -> string -> string =
        fun dkimStub -> 
            fun signature -> dkimStub + signature

    let generateDkimSignature: (string * string) list -> string -> string -> string -> string list -> RSACryptoServiceProvider -> string =
        fun headers -> 
            fun body -> 
                fun domain ->
                    fun selector -> 
                        fun headersToSign -> 
                            fun rsa ->
                                let canonBody = relaxBody body
                                let bodyHash = computeBodyHash canonBody
                                let htag, canonHeaderBlock = canonicalizeHeaders headers headersToSign
                                let dkimStub = buildDkimHeaderStub domain selector headersToSign bodyHash
                                let canonicalizedDkimHeader = relaxHeader "DKIM-Signature" dkimStub
                                let fullDataToSign = canonHeaderBlock + canonicalizedDkimHeader
                                let signature = signWithRsa rsa fullDataToSign
                                dkimStub + signature

[<EntryPoint>]
let main: string array -> int = 
    fun _ ->
        let headers = 
            ["From", "josh@joshkesler.com"];
             ["To", "you@someone.com"];
             ["Subject", "Testing DKIM"];
             ["Date", "Sat, 1 Jun 2025 16:10:00 -0500"];

        let body = "This is the body of the email.\r\nWith some lines.\r\n\r\n\r\n"

        let rsa = Dkim.loadRsaKey "/etc/dkim/private.key"

        let dkimHeader = Dkim.generateDkimSignature headers body "joshkesler.com" "mail" ["from"; "to"; "subject"; "date"] rsa

        printfn "%s" dkimHeader
        try startServer () |> Async.RunSynchronously
            0
        with ex -> 
            printfn "Fatal error: %s" ex.Message
            1



    

