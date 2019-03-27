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

import java.awt.Frame;
import java.awt.GraphicsEnvironment;
import java.awt.Rectangle;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import javax.swing.JFrame;

public class ScriptyGuiRepl
extends ScriptyCapable    
{
    private JFrame frame;
    private String title = "Scripty";
    private ScriptyPanel scriptyPanel;

    public ScriptyGuiRepl()
    {
    }

    public ScriptyGuiRepl(ScriptyCapable aFacade)
    {
        super(aFacade);
    }

    public String getTitle()
    {
        return title;
    }

    public void setTitle(String title)
    {
        this.title = title;
        if(frame != null) frame.setTitle(title);
    }

    private void buildGuiConsole()
    {
        frame = new JFrame(title);

        // Install the closing mechanism on the frame when the user
        // wants to close the frame by clicking the X button.
        frame.setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
        frame.addWindowListener(new WindowAdapter()
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
        frame.setLocation((lScreenRect.width - lFrameRect.width) / 2,(lScreenRect.height - lFrameRect.height) / 2);
        frame.setSize(lFrameRect.width, lFrameRect.height);
        //
        scriptyPanel = new ScriptyPanel(this);
        frame.add(scriptyPanel);
    }

    public void startLoop()
    {
        buildGuiConsole();
        if(frame != null) frame.setVisible(true);
    }
}


