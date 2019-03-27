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
package branscha.scripty.cmdlib;

import branscha.scripty.annot.*;
import branscha.scripty.parser.CommandException;
import branscha.scripty.parser.Parser;
import branscha.scripty.parser.Token;
import branscha.scripty.annot.*;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

@ScriptyLibrary(type=ScriptyLibraryType.STATIC)
@ScriptyNamedArgLists(std = {@ScriptyStdArgList(name="content + title", optional = {@ScriptyArg(name="content", type="String")}, named = {@ScriptyArg(name="title", type="String", optional = true, value = "Text Editor")})})
public class EditLibrary
{
    @ScriptyCommand(name="edit")
    @ScriptyRefArgList(ref = "content + title")
    public static String edit(@ScriptyParam("content") String aContent, @ScriptyParam("title") String aTitle)
    throws CommandException
    {
        // Not available in head less mode.
        if(GraphicsEnvironment.isHeadless())
            throw new CommandException("Java VM is running in headless mode.");

        JFrame lParent = null;
        final EditDialog lDiag = new EditDialog(lParent, aTitle, aContent);
        lDiag.setLocationRelativeTo(lParent);
        
        lDiag.setVisible(true);
        final String lEdited = lDiag.getContent();

        return lEdited;
    }

    @ScriptyCommand(name="edit-expr")
    @ScriptyRefArgList(ref = "content + title")
    public static Object editExpr(@ScriptyParam("content") String aContent, @ScriptyParam("title") String aTitle)
    throws CommandException
    {
        final String lEdited = edit(aContent, aTitle);
        if(lEdited != null)
        {
            Parser lParser = new Parser();
            Object lRes =  lParser.parseExpression(lEdited);
            if(lRes instanceof Token)
            {
                Token lResTok = (Token) lRes;
                if(lResTok.isErroneous()) throw new CommandException(lResTok.getValue());
            }
            return lRes;
        }
        else return null;
    }
}

class EditDialog
extends JDialog
{
    private String content = null;
    private JTextArea txtArea;

    public EditDialog(Frame aParent, String lTitle, String aContent)
    {
        this(aParent, lTitle);
        txtArea.setText(aContent);
    }

    public EditDialog(Frame aParent, String lTitle)
    {
        super(aParent, lTitle);

        setModal(true);
        setSize(400, 300);

        Container lCont = getContentPane();
        lCont.setLayout(new GridBagLayout());
        txtArea = new JTextArea();

        final Font lFont = new Font("Monospaced", Font.PLAIN, 12);
        txtArea.setFont(lFont);

        GridBagConstraints lConstraints = new GridBagConstraints();
        lConstraints.gridx = 0;
        lConstraints.gridy = 0;
        lConstraints.gridwidth = 3;
        lConstraints.gridheight = 1;
        lConstraints.fill = GridBagConstraints.BOTH;
        lConstraints.insets = new Insets(5,5,5,5);
        lConstraints.weightx = 1.0;
        lConstraints.weighty = 1.0;
        lCont.add(new JScrollPane(txtArea), lConstraints);

        JButton lOkButton = new JButton("Ok");
        JButton lCancelButton = new JButton("Cancel");

        lConstraints = new GridBagConstraints();
        lConstraints.gridwidth = 2;
        lConstraints.gridheight = 1;
        lConstraints.gridx = 0;
        lConstraints.gridy = 1;
        lConstraints.insets = new Insets(0,5,5,5);
        lConstraints.weightx = 1.0;
        lConstraints.anchor = GridBagConstraints.EAST;
        lCont.add(lOkButton, lConstraints);

        lConstraints.gridwidth = 1;
        lConstraints.gridx = 2;
        lConstraints.gridy = 1;
        lConstraints.weightx = 0.0;
        lCont.add(lCancelButton, lConstraints);

        lCancelButton.addActionListener(new ActionListener()
        {
            public void actionPerformed(ActionEvent e)
            {
                content = null;
                EditDialog.this.setVisible(false);
            }
        });

        lOkButton.addActionListener(new ActionListener()
        {
            public void actionPerformed(ActionEvent e)
            {
                content = txtArea.getText();
                EditDialog.this.setVisible(false);
            }
        });
    }

    public String getContent()
    {
        return content;
    }
}
