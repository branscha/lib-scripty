/*
 * Scripty Programming Language
 * Copyright (C) 2010-2012 Bruno Ranschaert, S.D.I.-Consulting BVBA
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

package com.sdicons.scripty;

import com.sdicons.scripty.repl.ReplEngine;

import javax.swing.*;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.*;

public class ScriptyGuiRepl
{
    private static LineNumberReader input;
    private static PrintWriter output;
    private static PrintWriter error;
    
    private static ReplEngine replEngine = new ReplEngine();
   
    
    public static void main(String[] args)
    {
        
       

//        // Provide the input and output streams in the context.
//        IContext lCtx = new BasicContext();
//
//
//
//
//
//        // Initialize our expression evaluator.
//        eval = new Eval(lCtx);
        
//        // TODO TODO
//        // GENERALIZE MECHANISM
//        replEngine.registerCommand("print", new PrintCmd());
//        // For demo purposes only.
//        replEngine.registerCommand("uuid", new UuidCmd());
//        replEngine.registerCommand("choose-file", new FileChooserCmd(FileChooserCmd.FileChooserType.files, FileChooserCmd.FileChooserMode.open));
//        replEngine.registerCommand("choose-dir", new FileChooserCmd(FileChooserCmd.FileChooserType.dirs, FileChooserCmd.FileChooserMode.open));
//        replEngine.registerCommand("exit", new ExitCmd());
//        replEngine.registerCommand("load", new LoadCmd(LoadCmd.LoadCmdType.load));
//        replEngine.registerCommand("edit-rec", new RecEditCmd());
//        replEngine.registerCommand("format", new StrCmd(StrCmd.StrCmdType.strFormat));
        
        final GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
        final boolean lIsHeadless = ge.isHeadless();
        
        if(lIsHeadless)
        {
            input = new LineNumberReader(new InputStreamReader(System.in));
            output = new PrintWriter(new OutputStreamWriter(System.out));
            error = new PrintWriter(new OutputStreamWriter(System.out));

//            lCtx.getRootContext().defBinding(INPUT, input);
//            lCtx.getRootContext().defBinding(OUTPUT, output);
//            lCtx.getRootContext().defBinding(ERROR, error);

            // TODO TODO TODO
            // START THE COMMAND LINE REPL
           // ....

        }
        else
        {
            Writer lWriter = buildGuiConsole();
            
            input = null;
            output = new PrintWriter(lWriter);
            error = new PrintWriter(lWriter);
        }

        

    }

    // TODO : - exit application ... be careful, see demo app.
    // - Position in middle of the screen
    // - commands to create menus.
    // - execute cmds in worker thread, show interrupt dialog box after x sec. to interrupt the command.
    //
    private static Writer buildGuiConsole()
    {
        final JFrame lFrame = new JFrame("Test");

        // Install the closing mechanism on the frame when the user
        // wants to close the frame by clicking the X button.
        lFrame.setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
        lFrame.addWindowListener(new WindowAdapter()
        {
            public void windowClosing(WindowEvent e)
            {
                for(Frame lAppFrm : JFrame.getFrames()) lAppFrm.dispose();
            }
        });

        // Calculate the middle of the screen.
        final Rectangle lScreenRect = GraphicsEnvironment.getLocalGraphicsEnvironment().getDefaultScreenDevice().getDefaultConfiguration().getBounds();
        // Using the 'golden ratio' 1.618 for the width/height ratio.
        final Rectangle lFrameRect = new Rectangle(Math.min(647, lScreenRect.width), Math.min(400, lScreenRect.height));
        // Set the window size and location.
        lFrame.setLocation((lScreenRect.width - lFrameRect.width) / 2,(lScreenRect.height - lFrameRect.height) / 2);
        lFrame.setSize(lFrameRect.width, lFrameRect.height);

        final JMenuBar lMainMenu = new JMenuBar();
        lFrame.setJMenuBar(lMainMenu);
        
        final JToolBar lToolBar = new JToolBar();
        lFrame.add(lToolBar, BorderLayout.NORTH);
        
        final JTextPane lOutputPane = new JTextPane();
        lOutputPane.setEditable(false);
        final JScrollPane lOutScroller = new JScrollPane(lOutputPane);
        // lOutScroller.setBorder(new TitledBorder("Output"));
        final TextPaneWriter lWriter = new TextPaneWriter(lOutputPane);

        final JTextArea lInputPane = new JTextArea();
        final JScrollPane lInScroller = new JScrollPane(lInputPane);
        // lInScroller.setBorder(new TitledBorder("Command Panel").set);
        
        final JSplitPane lSplitter = new JSplitPane(JSplitPane.VERTICAL_SPLIT);
        lSplitter.add(lInScroller, JSplitPane.TOP);
        lSplitter.add(lOutScroller, JSplitPane.BOTTOM);
        lFrame.add(lSplitter);

        final SimpleAttributeSet lErrAttrs = new SimpleAttributeSet();
        lErrAttrs.addAttribute(StyleConstants.CharacterConstants.Bold, Boolean.TRUE);
        lErrAttrs.addAttribute(StyleConstants.CharacterConstants.Italic, Boolean.TRUE);
        StyleConstants.setForeground(lErrAttrs, Color.RED);

        Action lExecuteAction = new AbstractAction("Run")
        {
            public void actionPerformed(ActionEvent e)
            {
                try
                {
                    lFrame.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
                    String lCmd = lInputPane.getText();
                    replEngine.startNonInteractive(new StringReader(lCmd), output, error);
                    lInputPane.grabFocus();
                    lInputPane.selectAll();
                } 
                catch (Exception e1)
                {
                    Document lDoc = lOutputPane.getDocument();
                    try
                    {
                        lDoc.insertString(lDoc.getLength(), e1.getMessage() + "\n", lErrAttrs);
                        lOutputPane.setCaretPosition(lOutputPane.getDocument().getLength());
                    }
                    catch (BadLocationException e2)
                    {
                        e2.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
                    }
                } 
                finally
                {
                    lFrame.setCursor(Cursor.getDefaultCursor());
                }
            }
        };
        
        Action lClearAction = new AbstractAction("Clear")
        {
            public void actionPerformed(ActionEvent e)
            {
                lOutputPane.setText("");
            }
        };

        JMenu lMenu = new JMenu("Console");
        lMainMenu.add(lMenu);
        
        JMenuItem lExecuteItem = new JMenuItem(lExecuteAction);
        JMenuItem lClearItem = new JMenuItem(lClearAction);
        lMenu.add(lExecuteItem);
        lMenu.add(lClearItem);

        lToolBar.add(lExecuteAction);
        lToolBar.add(lClearAction);

        lFrame.setVisible(true);
        lSplitter.setDividerLocation(1 - 1.0/1.618);
        
        // CTRL+ENTER = Execute
        lFrame.getRootPane().getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke("ctrl ENTER"), "execute");
        lFrame.getRootPane().getActionMap().put("execute", lExecuteAction);
        // Command window gets focus.
        lInputPane.grabFocus();
        
        return lWriter;
    }
}


final class TextPaneWriter extends Writer
{

    private final JTextPane textArea;

    public TextPaneWriter(final JTextPane textArea) {
        this.textArea = textArea;
    }

    @Override
    public void flush(){ }

    @Override
    public void close(){ }

    @Override
    public void write(final char[] cbuf, final int off, final int len) throws IOException {
        Document lDoc = textArea.getDocument();
        try
        {
            lDoc.insertString(lDoc.getLength(), new String(cbuf, off, len), null);
            textArea.setCaretPosition(textArea.getDocument().getLength());
        } catch (BadLocationException e)
        {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        }

    }
}