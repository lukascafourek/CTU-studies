/*
 * File name: Lab06.java
 * Date:      2014/08/26 21:39
 * Author:    @author
 */

package cz.cvut.fel.pjv;

import java.util.Objects;

// Implementace z ak. roku 2022/2023 (z minuleho letniho semestru) z duvodu opakovani predmetu PJV
public class Lab02 {

   public void homework() {
     TextIO text = new TextIO();
     String line;
     double [] array = {0,0,0,0,0,0,0,0,0,0};
     int counter = 0;
     int numOfLines = 1;
     double input = 0;
     double average;
     double deviation = 0;
     while (!Objects.equals(line = text.getLine(), "")) {
         if (TextIO.isInteger(line) || TextIO.isFloat(line) || TextIO.isDouble(line)) {
             array[counter] = Double.parseDouble(line);
             input += Double.parseDouble(line);
             counter += 1;
         } else {
             System.err.println("A number has not been parsed from line " + numOfLines);
         }
         if (counter == 10) {
             average = input / counter;
             for (int i = 0; i < counter; ++i) {
                 deviation += Math.pow(array[i] - average, 2);
             }
             deviation = Math.sqrt(deviation / counter);
             System.out.printf("%2d %.3f %.3f\n", counter, average, deviation);
             deviation = 0;
             input = 0;
             counter = 0;
         }
         numOfLines += 1;
     }
     System.err.println("End of input detected!");
     if (counter > 1) {
         average = input / counter;
         for (int i = 0; i < counter; ++i) {
             deviation += Math.pow(array[i] - average, 2);
         }
         deviation = Math.sqrt(deviation / counter);
         System.out.printf("%2d %.3f %.3f\n", counter, average, deviation);
     }
   }
}
