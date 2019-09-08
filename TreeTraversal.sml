(* CSC 345: ML Project - Traversal of Binary Trees *)
(* Giuseppe Barretta *)

datatype 'a BT = empty | bTree of 'a * 'a BT * 'a BT ;

(* Don't use NIL for the empty tree as we want it for the empty list *) 


(* a new type -- X, to test the polymorhism on *) 
datatype X = A|B|C|D|E|F|G|H;



(*  TEST DATA - BINARY TREES   *)

val t1 =  bTree(1, 
	       bTree(2,bTree(3,empty, empty), bTree(4,empty, empty)),
	       bTree(5,bTree(6,empty, empty),bTree(7,empty, empty)));

val t2 =  bTree(A, 
	       bTree(B,bTree(D,empty, empty), bTree(E,empty, empty)),
	       bTree(C,bTree(F,empty, empty),bTree(G,empty, empty)));

val t3 =  bTree(1.22, bTree(2.33,empty, empty), bTree(3.44,empty,empty));

val t4 =  bTree("A", 
	       bTree("B",
		    bTree("C",bTree("E",empty,empty), empty), 
		    bTree("D",
			 bTree("F",empty,empty), 
			 bTree("G",bTree("H",empty,empty), empty))),
	       bTree("I",
		    bTree("J",empty, bTree("K",
				      bTree("L",empty, empty),
				      bTree("M",empty, empty))),
		    empty));

(* print functions *)

fun printInt n = print(Int.toString n);

fun printReal n = print(Real.toString n);

fun printX A = print "A"
  | printX B = print "B"
  | printX C = print "C"
  | printX D = print "D"
  | printX E = print "E"
  | printX F = print "F"
  | printX G = print "G"
  | printX H = print "H";

val indentLevel = 2;

(* recursively add spaces until the tab amount is exhausted *)
fun tab n = if n <= 0 then
		print " "
            else
		(print " "; tab(n-1));

fun displayNode (a, printType, tabAmount) = (tab(indentLevel*tabAmount);
                                             printType(a);
                                             print "\n");
 
fun dash n = (tab(indentLevel*n);
	      print "  -\n");
 
fun displayTreeIndent (empty,_,_) = ()
  | displayTreeIndent (bTree(a, empty, empty), printType, n) = displayNode(a, printType, n)
  | displayTreeIndent (bTree(a, empty, right), printType, n) = (displayNode(a, printType, n);
                                                                dash n;
                                                                displayTreeIndent(right, printType, (n+1)))
  | displayTreeIndent (bTree(a, left, empty), printType, n) = (displayNode(a, printType, n);
                                                               displayTreeIndent(left, printType, (n+1));
                                                               dash n)
  | displayTreeIndent (bTree(a, left, right), printType, n) = (displayNode(a, printType, n);
                                                               displayTreeIndent(left, printType, (n+1));
                                                               displayTreeIndent(right, printType, (n+1)));
                                                                 
fun displayTree (btree, printType) = displayTreeIndent(btree, printType, 0);

(* Binary Tree Traversals *)

(* pre order binary tree traversal *)
fun preOrder empty = []
  | preOrder (bTree (a, left, right)) = [a] @ preOrder left @ preOrder right;
 
(* in order binary tree traversal*)
fun inOrder empty = []
  | inOrder (bTree ( a, left, right)) = inOrder left @ [a] @ inOrder right;
 
(* post order binary tree traversal *)
fun postOrder empty = []
  | postOrder (bTree (a, left, right)) = postOrder left @ postOrder right @ [a];
						     
(*End of File*)
