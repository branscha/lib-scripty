/*
 * Scripty Programming Language
 * Copyright (C) 2010-2011 Bruno Ranschaert, S.D.I.-Consulting BVBA
 * http://www.sdi-consulting.be
 * mailto://info@sdi-consulting.be
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */

package com.sdicons.repl.repl;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.ServerSocket;
import java.net.Socket;

public class ReplServer 
{
    private ServerSocket server;
    private IReplFactory factory;

    /**
     * Construct the server, you have to call the serveClients method 
     * for the server to start serving clients.
     */
    public ReplServer(int aPort, IReplFactory aFact) 
    {                
        try 
        {
            factory = aFact;
            
            // Plain server.
            server = new ServerSocket(aPort);
        } 
        catch (IOException e) 
        {
            e.printStackTrace();
        }
    }

    /**
     * Start serving clients. This method will never return.
     *
     */
    public void serveClients() 
    {   
        // An infinite loop, waiting for client requests, spawning handlers and 
        // wait for new requests.
        while (true) 
        {
            try 
            {
                // This call will block until a client contacts this server.
                Socket socket = server.accept();
                // Set the session timeout.
                // After 30 min of inactivity the socket will be closed.
                socket.setSoTimeout(1000 * 1800);
                
                // We just got a connection from a client.
                // We will create a separate handler for this client request (separate thread)
                // and we will immediately keep listening for other clients to connect our server port.
                // So this call starts the handling process, but it does not wait for it to finish.
                new RequestHandler(socket, factory);
            } 
            catch (IOException e) 
            {
                e.printStackTrace();
            }
        }
    }
    
    public static void main(String[] aArgs)
    {
        System.out.println("Starting REPL server on port 5596.");
        ReplServer lServer = new ReplServer(5596, new ReplFactory());
        lServer.serveClients();
    }
}

/**
 * A request handler, it is created by the server.
 * It will always run in a separate thread from the server so that the
 * main server can keep listening for incoming connections.
 *
 */
class RequestHandler 
implements Runnable 
{
    private Socket socket;
    private IReplFactory factory;


    /*
     * Create the handler, and start the processing of the request
     * in a separate thread.
     */
    public RequestHandler(Socket aSocket, IReplFactory aFact) 
    {
        this.socket = aSocket;
        this.factory = aFact;
        Thread workerThread = new Thread(this);
        workerThread.start();
    }

    public void run() 
    {
        try 
        {             
            // Get the streams.
            InputStream lIn = socket.getInputStream();
            OutputStream lOut = socket.getOutputStream();
            
            // Start the REPL.
            IRepl lRepl = factory.createRepl(lIn, lOut);                        
            lRepl.start();          
            
            // Close streams and sockets.
            // In production code, closing stuff should *ALWAYS* be 
            // in a finally handler! For simplicity we keep them here.
            lIn.close();
            lOut.close();
            socket.close();            
        } 
        catch (Exception e) 
        {
            e.printStackTrace();
        } 
    }        
}