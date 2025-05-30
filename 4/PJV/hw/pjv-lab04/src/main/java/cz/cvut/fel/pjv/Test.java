package cz.cvut.fel.pjv;

// Implementace z ak. roku 2022/2023 (z minuleho letniho semestru) z duvodu opakovani predmetu PJV

public class Test {
    
    public void start() {
        String password = "abcdaaaddb";
        BruteForceAttacker attacker = new BruteForceAttacker();
        attacker.init(new char[]{'a', 'b', 'c', 'd'}, password);
        
        System.out.println("Trying to break password...");
        attacker.breakPassword(password.length());
        
        if (attacker.isOpened()) {
            System.out.println("[VAULT] opened, password is " + password);
        } else {
            System.out.println("[VAULT] is still closed");
        }
    }
}