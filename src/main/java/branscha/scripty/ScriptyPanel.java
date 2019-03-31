/*******************************************************************************
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

public class ScriptyPanel extends JPanel {
    private static class PanelEngine extends ScriptyCapable {
    }

    private PanelEngine engine = new PanelEngine();
    private JSplitPane splitter;
    private boolean horizontalLayout = false;

    public ScriptyPanel() {
        this.setLayout(new BorderLayout());
        splitter = new JSplitPane(horizontalLayout ? JSplitPane.HORIZONTAL_SPLIT : JSplitPane.VERTICAL_SPLIT);
        buildPanel();
    }

    public ScriptyPanel(ScriptyCapable aFacade) {
        this();
        engine.setReplEngine(aFacade.getReplEngine());
    }

    public boolean isHorizontalLayout() {
        return horizontalLayout;
    }

    public void setHorizontalLayout() {
        this.horizontalLayout = true;
        if (splitter != null) splitter.setOrientation(JSplitPane.HORIZONTAL_SPLIT);
    }

    public boolean isVerticalLayout() {
        return !horizontalLayout;
    }

    public void setVerticalLayout() {
        this.horizontalLayout = false;
        if (splitter != null) splitter.setOrientation(JSplitPane.VERTICAL_SPLIT);
    }

    private void buildPanel() {
        final JToolBar lToolBar = new JToolBar();
        this.add(lToolBar, BorderLayout.NORTH);

        final JTextPane lOutputPane = new JTextPane();
        lOutputPane.setEditable(false);
        final JScrollPane lOutScroller = new JScrollPane(lOutputPane);
        lOutScroller.setBackground(Color.white);
        lOutScroller.setBorder(new TitledBorder("Output"));
        final TextPaneWriter lWriter = new TextPaneWriter(lOutputPane);

        final JTextPane lInputPane = new JTextPane();
        final JScrollPane lInScroller = new JScrollPane(lInputPane);
        lInScroller.setBackground(Color.white);
        lInScroller.setBorder(new TitledBorder("Command Panel"));

        splitter.add(lInScroller, JSplitPane.TOP);
        splitter.add(lOutScroller, JSplitPane.BOTTOM);
        this.add(splitter);

        final SimpleAttributeSet lErrAttrs = new SimpleAttributeSet();
        lErrAttrs.addAttribute(StyleConstants.CharacterConstants.Bold, Boolean.TRUE);
        lErrAttrs.addAttribute(StyleConstants.CharacterConstants.Italic, Boolean.TRUE);
        StyleConstants.setForeground(lErrAttrs, Color.RED);

        final Action lExecuteAction = new AbstractAction("Run") {
            public void actionPerformed(ActionEvent e) {
                // Find top level frame.
                final JFrame lFrame = (JFrame) SwingUtilities.getAncestorOfClass(JFrame.class, ScriptyPanel.this);

                try {
                    // Set wait cursor.
                    lFrame.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
                    //
                    final String lCmd = lInputPane.getText();
                    engine.getReplEngine().startNonInteractive(new StringReader(lCmd), new PrintWriter(lWriter), new PrintWriter(lWriter));
                    // 
                    lInputPane.grabFocus();
                    lInputPane.selectAll();
                } catch (Exception e1) {
                    try {
                        final Document lDoc = lOutputPane.getDocument();
                        lDoc.insertString(lDoc.getLength(), e1.getMessage() + "\n", lErrAttrs);
                        lOutputPane.setCaretPosition(lOutputPane.getDocument().getLength());
                    } catch (BadLocationException ignored) {
                    }
                } finally {
                    lFrame.setCursor(Cursor.getDefaultCursor());
                }
            }
        };

        final Action lClearAction = new AbstractAction("Clear") {
            public void actionPerformed(ActionEvent e) {
                lOutputPane.setText("");
            }
        };

        this.addComponentListener(new ComponentAdapter() {
            boolean firstTime = true;

            public void componentResized(ComponentEvent e) {
                if (firstTime) {
                    firstTime = false;
                    splitter.setDividerLocation(1 - 1.0 / 1.618);
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

    private static class TextPaneWriter extends Writer {
        private final JTextPane textArea;

        public TextPaneWriter(final JTextPane textArea) {
            this.textArea = textArea;
        }

        @Override
        public void flush() {
        }

        @Override
        public void close() {
        }

        @Override
        public void write(final char[] cbuf, final int off, final int len)
        throws IOException {
            final Document lDoc = textArea.getDocument();
            try {
                lDoc.insertString(lDoc.getLength(), new String(cbuf, off, len), null);
                textArea.setCaretPosition(textArea.getDocument().getLength());
            } catch (BadLocationException ignored) {
            }
        }
    }
}
