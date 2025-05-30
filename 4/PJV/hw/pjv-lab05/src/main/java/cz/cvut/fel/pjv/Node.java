package cz.cvut.fel.pjv;

// Implementace z ak. roku 2022/2023 (z minuleho letniho semestru) z duvodu opakovani predmetu PJV

/**
 * Interface Node represents one node of binary tree. It contains integer value.
 */
public interface Node {
	
	/**
	 * @return left child of this node, or null
	 */
	public Node getLeft();
	
	/**
	 * @return right child of this node, or null
	 */
	public Node getRight();
	
	/**
	 * @return value of this node
	 */
	public int getValue();
}
