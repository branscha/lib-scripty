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

package com.sdicons.repl.cmdlib;

import com.sdicons.repl.parser.*;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

public class EditCmd
implements ICommand
{
    public static enum EditCmdType {txt, cmd};
    private EditCmdType type;

    public static void registerCommands(IRegistry aReg)
    {
        aReg.registerCommand("edit", new EditCmd(EditCmdType.txt));
        aReg.registerCommand("edit-expr", new EditCmd(EditCmdType.cmd));
    }

    public EditCmd(EditCmdType aType)
    {
        type = aType;
    }

    public Object execute(IEval aEval, IContext aCtx, Object[] aArgs)
    throws CommandException
    {
        // Not available in head less mode.
	    if(GraphicsEnvironment.isHeadless())
	    	throw new CommandException("ERROR: Java VM is running in headless mode.");

        String lTitle = "Text Editor";
        String lContents = "";
        Frame lParent = (Frame) aCtx.getBinding("*frame");

        // Get the arguments.
        if(aArgs.length > 1)
        {
            if(aArgs[1] instanceof String) lTitle = (String) aArgs[1];
            if(aArgs.length > 2)
            {
                lContents = (String) aArgs[2];
            }
        }

        final EditDialog lDiag = new EditDialog(lParent, lTitle, lContents);
        lDiag.setLocationRelativeTo(lParent);
        lDiag.setVisible(true);
        final String lEdited = lDiag.getContent();

        if(lEdited == null || type == EditCmdType.txt) return lEdited;
        else if(lEdited != null && type == EditCmdType.cmd)
        {
            Parser lParser = new Parser();
            Object lRes =  lParser.parseExpression(lEdited);
            if(lRes instanceof  Token)
            {
                Token lResTok = (Token) lRes;
                if(lResTok.isErroneous()) throw new CommandException(lResTok.getValue());
            }
            return lRes;
        }
        return null;
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
