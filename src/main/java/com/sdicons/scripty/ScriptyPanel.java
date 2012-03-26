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

import javax.swing.*;
import javax.swing.border.TitledBorder;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringReader;
import java.io.Writer;

public class ScriptyPanel
extends JPanel
{
    private static class PanelEngine 
    extends ScriptyCapable
    {
    }
    
    private PanelEngine engine = new PanelEngine();

    public ScriptyPanel()
    {
        this.setLayout(new BorderLayout());
        init();
    }
    
    public ScriptyPanel(ScriptyCapable aFacade)
    {
        this();
        engine.setReplEngine(aFacade.getReplEngine());
    }

//    public ScriptyCapable asScriptyCapable()
//    {
//        return engine;
//    }
    
    private void init()
    {
        final JToolBar lToolBar = new JToolBar();
        this.add(lToolBar, BorderLayout.NORTH);

        final JTextPane lOutputPane = new JTextPane();
        lOutputPane.setEditable(false);
        final JScrollPane lOutScroller = new JScrollPane(lOutputPane);
        lOutScroller.setBackground(Color.white);
        lOutScroller.setBorder(new TitledBorder("Output"));
        final TextPaneWriter lWriter = new TextPaneWriter(lOutputPane);

        final JTextArea lInputPane = new JTextArea();
        final JScrollPane lInScroller = new JScrollPane(lInputPane);
        lInScroller.setBackground(Color.white);
        lInScroller.setBorder(new TitledBorder("Command Panel"));

        final JSplitPane lSplitter = new JSplitPane(JSplitPane.VERTICAL_SPLIT);
        lSplitter.add(lInScroller, JSplitPane.TOP);
        lSplitter.add(lOutScroller, JSplitPane.BOTTOM);
        this.add(lSplitter);

        final SimpleAttributeSet lErrAttrs = new SimpleAttributeSet();
        lErrAttrs.addAttribute(StyleConstants.CharacterConstants.Bold, Boolean.TRUE);
        lErrAttrs.addAttribute(StyleConstants.CharacterConstants.Italic, Boolean.TRUE);
        StyleConstants.setForeground(lErrAttrs, Color.RED);

        Action lExecuteAction = new AbstractAction("Run")
        {
            public void actionPerformed(ActionEvent e)
            {
                JFrame lFrame = (JFrame)SwingUtilities.getAncestorOfClass(JFrame.class, ScriptyPanel.this);

                try
                {
                    lFrame.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
                    String lCmd = lInputPane.getText();

                    engine.getReplEngine().startNonInteractive(new StringReader(lCmd), new PrintWriter(lWriter), new PrintWriter(lWriter));
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

        this.addComponentListener(new ComponentAdapter()
        {
            boolean firstTime = true;
            public void componentResized(ComponentEvent e) {
                if(firstTime)
                {
                    firstTime=false;
                    lSplitter.setDividerLocation(1 - 1.0/1.618);
                }
            }
        });

        lToolBar.add(lExecuteAction);
        lToolBar.add(lClearAction);

        // CTRL+ENTER = Execute
        this.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke("ctrl ENTER"), "execute");
        this.getActionMap().put("execute", lExecuteAction);
        // Command window gets focus.
        lInputPane.grabFocus();
    }

    private static class TextPaneWriter extends Writer
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
            }
            catch (BadLocationException e)
            {
                e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
            }
        }
    }
}
