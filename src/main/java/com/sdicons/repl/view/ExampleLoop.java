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

package com.sdicons.repl.view;

import java.io.File;
import java.util.LinkedList;
import java.util.List;

import com.sdicons.repl.cmdlib.*;
import com.sdicons.repl.cmdlib.FileChooserCmd.FileChooserMode;
import com.sdicons.repl.cmdlib.FileChooserCmd.FileChooserType;
import com.sdicons.repl.cmdlib.LoadCmd.LoadCmdType;
import com.sdicons.repl.parser.CommandException;
import com.sdicons.repl.parser.ICommand;
import com.sdicons.repl.repl.Repl;

public class ExampleLoop
{
    public static void main(String[] aArgs)
    {
        // Create the repl.
        System.out.println("-- SDI Super REPL --");
        Repl lRepl = new Repl(System.in, System.out, System.out);
 
        // Register the file view commands.
        lRepl.registerCommand("print", new PrintCmd());
        lRepl.registerCommand("choose-file", new FileChooserCmd(FileChooserType.files, FileChooserMode.open));
        lRepl.registerCommand("choose-dir", new FileChooserCmd(FileChooserType.dirs, FileChooserMode.open));
        lRepl.registerCommand("exit", new ExitCmd());
        
        // Add string manipulation commands.
        StrCmd.registerCommands(lRepl);
        
        // Load commands.
        LoadCmd.registerCommands(lRepl);
        
        // Add all list commands.
        LispCmd.registerCommands(lRepl);
        
        // Add calculating commands.
        MathCmd.registerCommands(lRepl);
        
        // Add pair commands.
        //PairCmd.registerCommands(lRepl.getEval().getCommands());
        
        // Add hash commands.
        MapCmd.registerCommands(lRepl);
                      
        
        // System stuff
        SysCmd.registerCommands(lRepl);
        
        // Bean dir beta stuff.
        BeanCmd.registerCommands(lRepl);
        
        // Bean dir beta stuff.
        FileSysCmd.registerCommands(lRepl);
        
        // Httpd beta stuff.
        HttpdCmd.registerCommands(lRepl);

        // Debugger.
        DbgCmd.registerCommands(lRepl);
        
        // File autoloading. 
        final List<File> lAutoLoad = new LinkedList<File>();
        // Try the home directory.
        String lHome = System.getProperty("user.home");
        if(lHome != null) 
            lAutoLoad.add(new File(lHome, "exampleloop.txt"));

        // Try to autoload.
        for(File lFile : lAutoLoad)
        {
            if(lFile.exists() && lFile.canRead() )
            {                
                try
                {
                    final String lPath = lFile.getAbsolutePath();
                    System.out.println("Loading " + lPath + ".");
                    lRepl.exec("(load " + lPath + ")");
                }
                catch (CommandException e)
                {
                    final String lMsg = CmdUtil.concatExceptionMessages(e);
                    System.out.println(lMsg);
                }                
            }            
        }
        
        // Start the interaction.
        lRepl.start();        
    }
}
