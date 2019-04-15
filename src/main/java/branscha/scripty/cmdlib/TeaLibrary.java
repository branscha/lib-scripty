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
package branscha.scripty.cmdlib;

import branscha.scripty.annot.*;
import branscha.scripty.parser.CommandException;

import java.math.BigInteger;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;

@ScriptyStdArgList(name = "text and password", fixed = {@ScriptyArg(name = "text", type = "String")}, named = {@ScriptyArg(name = "password", type = "String", optional = true, value = "")})
public class TeaLibrary {
    @ScriptyCommand(name = "tea-encrypt")
    @ScriptyRefArgList(ref = "text and password")
    public static String encrypt(@ScriptyParam("text") String aPlainText, @ScriptyParam("password") String aPassword)
    throws CommandException {
        TeaEncrypter lTea;

        if (aPassword == null || aPassword.length() <= 0) lTea = new TeaEncrypter(); // Standard IBM EGL password.
        else lTea = new TeaEncrypter(md5(aPassword)); // User provided password.

        return lTea.encrypt(aPlainText);
    }

    @ScriptyCommand(name = "tea-decrypt")
    @ScriptyRefArgList(ref = "text and password")
    public static String decrypt(@ScriptyParam("text") String aCipher, @ScriptyParam("password") String aPassword)
    throws CommandException {
        TeaEncrypter lTea;

        if (aPassword == null || aPassword.length() <= 0) lTea = new TeaEncrypter(); // Standard IBM EGL password.
        else lTea = new TeaEncrypter(md5(aPassword)); // User provided password.

        return lTea.decrypt(aCipher);
    }

    private static byte[] md5(String aString)
    throws CommandException {
        try {
            MessageDigest digest = java.security.MessageDigest.getInstance("MD5");
            digest.update(aString.getBytes());
            return digest.digest();
        }
        catch (NoSuchAlgorithmException e) {
            throw new CommandException("Cannot calculate MD5 hash.");
        }
    }

}

class TeaEncrypter {
    private Tea tea;

    public TeaEncrypter(byte[] aKey) {
        tea = new Tea(aKey);
    }

    public TeaEncrypter() {
        tea = new Tea((new BigInteger("687d44e4a3a912230909823be3e2455", 16)).toByteArray());
    }

    public String encrypt(String aPlainText) {
        char lPlainChars[] = aPlainText.toCharArray();
        byte lPlainBytes[] = new byte[lPlainChars.length * 2];
        for (int i = 0; i < lPlainChars.length; i++)
            lPlainBytes[i] = (byte) (lPlainChars[i] & 0xff);

        final int lEncodedBytes[] = tea.encode(lPlainBytes, lPlainBytes.length);
        final StringBuilder lCipherText = new StringBuilder();
        for (int j = 0; j < lEncodedBytes.length; j++) {
            final String s2 = Integer.toHexString(lEncodedBytes[j]);
            if (s2.length() % 2 != 0) lCipherText.append("0");
            lCipherText.append(s2);
        }
        return lCipherText.toString();
    }

    public String decrypt(String aCipherText) {
        final ArrayList<Long> lLongCipherPieces = new ArrayList<Long>();
        final int lCipherLen = aCipherText.length();

        int j;
        for (int k = 0; k < lCipherLen; k = j) {
            j = k + 8;
            if (j >= lCipherLen)
                j = lCipherLen;
            String lTxtCipherPiece = aCipherText.substring(k, j);
            try {
                lLongCipherPieces.add(Long.decode("0x" + lTxtCipherPiece));
            }
            catch (NumberFormatException numberformatexception) {
                return "";
            }
        }

        int lIntCipherPieces[] = new int[lLongCipherPieces.size()];
        for (int l = 0; l < lIntCipherPieces.length; l++)
            lIntCipherPieces[l] = ((Long) lLongCipherPieces.get(l)).intValue();

        byte lDecodedBytes[] = tea.decode(lIntCipherPieces);
        char lPlainChars[] = new char[lDecodedBytes.length];
        int i1 = 0;
        for (int j1 = 0; j1 < lPlainChars.length; j1++) {
            lPlainChars[j1] = (char) lDecodedBytes[j1];
            if (lPlainChars[j1] == 0) i1++;
        }
        return new String(lPlainChars, 0, lPlainChars.length - i1);
    }
}

class Tea {
    private int key[];
    private int padding;

    public Tea(int aKey[]) {
        key = aKey;
    }

    public Tea(byte aKey[]) {
        int i = aKey.length;
        key = new int[4];
        if (i != 16)
            throw new ArrayIndexOutOfBoundsException(getClass().getName() + ": Key is not 16 bytes: " + i);
        int k = 0;
        for (int j = 0; j < i; ) {
            key[k] = aKey[j] << 24 | (aKey[j + 1] & 0xff) << 16 | (aKey[j + 2] & 0xff) << 8 | aKey[j + 3] & 0xff;
            j += 4;
            k++;
        }
    }

    int[] encode(byte aPlainBytes[], int aPlainBytesLen) {
        int l = aPlainBytesLen;
        byte abyte1[] = aPlainBytes;

        padding = l % 8;
        if (padding != 0) {
            padding = 8 - l % 8;
            abyte1 = new byte[l + padding];
            System.arraycopy(aPlainBytes, 0, abyte1, 0, l);
            l = abyte1.length;
        }
        int i1 = l / 4;
        int ai[] = new int[2];
        int ai1[] = new int[i1];
        int k = 0;
        for (int j = 0; j < l; ) {
            ai[0] = abyte1[j] << 24 | (abyte1[j + 1] & 0xff) << 16 | (abyte1[j + 2] & 0xff) << 8 | abyte1[j + 3] & 0xff;
            ai[1] = abyte1[j + 4] << 24 | (abyte1[j + 5] & 0xff) << 16 | (abyte1[j + 6] & 0xff) << 8 | abyte1[j + 7] & 0xff;
            encipher(ai);
            ai1[k] = ai[0];
            ai1[k + 1] = ai[1];
            j += 8;
            k += 2;
        }

        return ai1;
    }

    public int[] encipher(int ai[]) {
        int i = ai[0];
        int j = ai[1];
        int k = 0;
        int l = 0x9e3779b9;
        int i1 = key[0];
        int j1 = key[1];
        int k1 = key[2];
        int l1 = key[3];
        for (int i2 = 32; i2-- > 0; ) {
            k += l;
            i += (j << 4) + i1 ^ j + k ^ (j >>> 5) + j1;
            j += (i << 4) + k1 ^ i + k ^ (i >>> 5) + l1;
        }

        ai[0] = i;
        ai[1] = j;
        return ai;
    }

    public byte[] decode(int lEncodedInts[]) {
        int i = lEncodedInts.length;
        byte lDecodedBytes[] = new byte[i * 4];
        int ai1[] = new int[2];
        int k = 0;
        for (int j = 0; j < i; ) {
            ai1[0] = lEncodedInts[j];
            ai1[1] = lEncodedInts[j + 1];
            decipher(ai1);
            lDecodedBytes[k] = (byte) (ai1[0] >>> 24);
            lDecodedBytes[k + 1] = (byte) (ai1[0] >>> 16);
            lDecodedBytes[k + 2] = (byte) (ai1[0] >>> 8);
            lDecodedBytes[k + 3] = (byte) ai1[0];
            lDecodedBytes[k + 4] = (byte) (ai1[1] >>> 24);
            lDecodedBytes[k + 5] = (byte) (ai1[1] >>> 16);
            lDecodedBytes[k + 6] = (byte) (ai1[1] >>> 8);
            lDecodedBytes[k + 7] = (byte) ai1[1];
            j += 2;
            k += 8;
        }
        return lDecodedBytes;
    }

    public int[] decipher(int ai[]) {
        int i = ai[0];
        int j = ai[1];

        int k = 0xc6ef3720;
        int l = 0x9e3779b9;

        int i1 = key[0];
        int j1 = key[1];
        int k1 = key[2];
        int l1 = key[3];

        for (int i2 = 32; i2-- > 0; ) {
            j -= (i << 4) + k1 ^ i + k ^ (i >>> 5) + l1;
            i -= (j << 4) + i1 ^ j + k ^ (j >>> 5) + j1;
            k -= l;
        }

        ai[0] = i;
        ai[1] = j;
        return ai;
    }
}
