/* ******************************************************************************
 * The MIT License
 * Copyright (c) 2012 Bruno Ranschaert
 * lib-scripty
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 * LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 ******************************************************************************/
package branscha.scripty;

import branscha.scripty.repl.ReplEngine;

import java.io.*;
import java.net.ServerSocket;
import java.net.Socket;

/**
 * A text based scripty REPL that can be embedded in an application and then accessed over the network layer.
 * In this way an application can provide console access for debugging, monitoring or configuration.
 * It is a proof-of-concept for the REPL, a production implementation will need a security layer.
 */
@SuppressWarnings("UnusedDeclaration")
public class ScriptyEmbeddedRepl
extends ScriptyCapable {

    public ScriptyEmbeddedRepl() {
    }

    public ScriptyEmbeddedRepl(ScriptyCapable aScriptyFacade) {
        setReplEngine(aScriptyFacade.getReplEngine());
    }

    public void startLoop(final int aPort, final String aUid, final String aPwd) {
        final Thread lServerThread = new Thread() {
            @Override
            public void run() {
                final ReplServer lServer = new ReplServer(aPort, getReplEngine());
                lServer.serveClients(aUid, aPwd);
            }
        };
        lServerThread.setDaemon(true);
        lServerThread.start();
    }
}

class ReplServer {
    private ServerSocket server;
    private ReplEngine scripty;

    /**
     * Construct the server, you have to call the serveClients method
     * for the server to start serving clients.
     *
     * @param aPort   The port where the server will listen for connections.
     * @param aEngine A REPL engine.
     */
    public ReplServer(int aPort, ReplEngine aEngine) {
        try {
            scripty = aEngine;

            // Plain server.
            server = new ServerSocket(aPort);
        }
        catch (IOException e) {
            e.printStackTrace();
        }
    }

    /**
     * Start serving clients. This method will never return.
     *
     * @param aUid A user id.
     * @param aPwd A password.
     */
    public void serveClients(String aUid, String aPwd) {
        // An infinite loop, waiting for client requests, spawning handlers and 
        // wait for new requests.
        //noinspection InfiniteLoopStatement
        while (true) {
            try {
                // This call will block until a client contacts this server.
                Socket socket = server.accept();
                // Set the session timeout.
                // After 30 min of inactivity the socket will be closed.
                socket.setSoTimeout(1000 * 1800);

                // We just got a connection from a client.
                // We will create a separate handler for this client request (separate thread)
                // and we will immediately keep listening for other clients to connect our server port.
                // So this call starts the handling process, but it does not wait for it to finish.
                new RequestHandler(socket, scripty, aUid, aPwd);
            }
            catch (IOException e) {
                e.printStackTrace();
            }
        }
    }
}

/**
 * A request handler, it is created by the server.
 * It will always run in a separate thread from the server so that the
 * main server can keep listening for incoming connections.
 */
class RequestHandler implements Runnable {

    private Socket socket;
    private ReplEngine scripty;
    private String uid;
    private String pwd;

    /*
     * Create the handler, and start the processing of the request
     * in a separate thread.
     */
    public RequestHandler(Socket aSocket, ReplEngine aEngine, String aUid, String aPwd) {
        this.socket = aSocket;
        this.scripty = aEngine;
        uid = aUid;
        pwd = aPwd;

        Thread workerThread = new Thread(this);
        workerThread.start();
    }

    public boolean login(InputStream aIn, OutputStream aOut, String aUid, String aPwd) {
        try {
            LineNumberReader input = new LineNumberReader(new InputStreamReader(aIn));
            PrintWriter out = new PrintWriter(new OutputStreamWriter(aOut));

            int count = 0;
            while (count++ < 5) {
                out.print("user: ");
                out.flush();
                String lUid = input.readLine();

                out.print("password: ");
                out.flush();
                String lPwd = input.readLine();

                if (lUid != null && lUid.trim().equals(aUid) && lPwd != null && lPwd.trim().equals(aPwd)) return true;
                else {
                    out.println("Login failed.");
                    out.flush();
                }
            }
            return false;
        }
        catch (Exception e) {
            return false;
        }
    }

    public void run() {
        InputStream lIn = null;
        OutputStream lOut = null;

        try {
            // Get the streams.
            lIn = socket.getInputStream();
            lOut = socket.getOutputStream();

            if (!login(lIn, lOut, uid, pwd)) return;

            // Start the REPL.
            scripty.startInteractive(lIn, lOut, lOut);
        }
        catch (Exception e) {
            e.printStackTrace();
        }
        finally {
            if (lIn != null) try {
                lIn.close();
            }
            catch (Exception ignored) {
            }
            if (lOut != null) try {
                lOut.close();
            }
            catch (Exception ignored) {
            }
            try {
                socket.close();
            }
            catch (Exception ignored) {
            }
        }
    }
}
