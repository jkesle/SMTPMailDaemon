module MailDaemon

open System
open System.IO
open System.Net
open System.Net.Sockets
open System.Text

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
let startServer: unit -> unit =
    fun () -> 
        let listener = new TcpListener (IPAddress.Any, 25)
        listener.Start ()
        printfn "SMTP server listening on port 25..."

        while true do
            let client = listener.AcceptTcpClient ()
            async { handleClient client } |> Async.Start

[<EntryPoint>]
let main: string array -> int = 
    fun args ->
        startServer ()
        0



