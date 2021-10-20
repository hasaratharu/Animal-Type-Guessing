%To Start the system type start.
% Name : - Hasara Tharudini

:- use_module(library(jpl)).
start :-sleep(0.4),	
		write('-----------------------------------------------------------------'),nl,
		sleep(0.4),
		write('*****************************************************************'),nl,
		sleep(0.2),
		write("###################||| EXPERT SYSTEM |||#########################"),nl,
		sleep(0.4),
		write('*****************************************************************'),nl,
		sleep(0.4),
		write('-----------------------------------------------------------------'),nl,nl,nl,
		
		
        /*write("Hi. How are you? First of all tell me your name Please : "),
        read(Patient),*/
		
		
		interface2.
		
		
       /* hypothesis(Patient,Disease),
        write(Patient),write(', you '), write(' probably have '),write(Disease),write('.'),undo,
		nl,nl,nl,
		sleep(0.7),
		write('*****************************************************************'),nl,
		sleep(0.4),
		write("################||| THANK YOU FOR USE ME |||#####################"),nl,
		sleep(0.4),
		write('*****************************************************************'),nl.*/
        
        
    symptom(Patient,ear_bones) :- verify(Patient," has three middle ear bones (y/n) ?").
 
    symptom(Patient,glands) :- verify(Patient," has mammary glands (y/n) ?").
  
    symptom(Patient,hair) :- verify(Patient," has hair (y/n) ?").

    symptom(Patient,neocortex) :- verify(Patient," has a neocortex (y/n) ?").
    
    symptom(Patient,feathers) :- verify(Patient," has feathers (y/n) ?").
    
    symptom(Patient,toothless) :- verify(Patient," has be toothless (y/n) ?").
	
    symptom(Patient,beaked_jaws) :- verify(Patient," has beaked jaws (y/n) ?").
 
    symptom(Patient,eggs) :- verify(Patient," has laying of hard-shelled eggs (y/n) ?").
   
    symptom(Patient,heart) :- verify(Patient," has a four-chambered heart (y/n) ?").
  
    symptom(Patient,skelton_system) :- verify(Patient," has a light weight skelton system (y/n) ?").
   
	symptom(Patient,joined_legs) :- verify(Patient," has joinned legs(8-legs) (y/n) ?").
	
	symptom(Patient,invertebrate) :- verify(Patient," has be the invertebrate animal (y/n) ?").
	
	/*symptom(_,"Sorry, I don't seem to be able to diagnose the disease.").*/

        
    hypothesis(Patient,mammal) :-
        symptom(Patient,ear_bones),
        symptom(Patient,glands),
        symptom(Patient,hair),
		symptom(Patient,neocortex).
    
    hypothesis(Patient,bird) :-
        symptom(Patient,feathers),
        symptom(Patient,toothless),
        symptom(Patient,beaked_jaws),
        symptom(Patient,eggs),
		symptom(Patient,heart),
		symptom(Patient,skelton_system).
        
    hypothesis(Patient,arachnid) :-
        symptom(Patient,joined_legs),
        symptom(Patient,invertebrate).    
        
   
        
	hypothesis(_,"a type. But I'm Sorry, I don't seem to be able to diagnose the type of the animal").
	
    response(Reply) :-
        read(Reply),
        write(Reply),nl.
		
ask(Patient,Question) :-
	write(Patient),write(', does the animal '),write(Question),
	/*read(N),
	( (N == yes ; N == y)
      ->
       assert(yes(Question)) ;
       assert(no(Question)), fail),*/
	
	interface(', does the animal ',Patient,Question),
	write('Loading.'),nl,
	sleep(1),
	write('Loading..'),nl,
	sleep(1),
	write('Loading...'),nl,
	sleep(1),
    nl.
	
:- dynamic yes/1,no/1.		
	
verify(P,S) :-
   (yes(S) 
    ->
    true ;
    (no(S)
     ->
     fail ;
     ask(P,S))).
	 
undo :- retract(yes(_)),fail. 
undo :- retract(no(_)),fail.
undo.


pt(Patient):- 

		hypothesis(Patient,Disease),
		interface3(Patient,', the animal is ',Disease,'.'),
        write(Patient),write(', the animal is '),write(Disease),write('.'),undo,end.

end :-
		nl,nl,nl,
		sleep(0.7),
		write('*****************************************************************'),nl,
		sleep(0.4),
		write("################||| THANK YOU FOR USE ME |||#####################"),nl,
		sleep(0.4),
		write('*****************************************************************'),nl.

interface(X,Y,Z) :-
	atom_concat(Y,X, FAtom),
	atom_concat(FAtom,Z,FinalAtom),
	jpl_new('javax.swing.JFrame', ['Expert System'], F),
	jpl_new('javax.swing.JLabel',['--- GUESS ANIMAL TYPE EXPERT SYSTEM ---'],LBL),
	jpl_new('javax.swing.JPanel',[],Pan),
	jpl_call(Pan,add,[LBL],_),
	jpl_call(F,add,[Pan],_),
	jpl_call(F, setLocation, [400,300], _),
	jpl_call(F, setSize, [400,300], _),
	jpl_call(F, setVisible, [@(true)], _),
	jpl_call(F, toFront, [], _),
	jpl_call('javax.swing.JOptionPane', showInputDialog, [F,FinalAtom], N),
	jpl_call(F, dispose, [], _), 
	write(N),nl,
	( (N == yes ; N == y)
      ->
       assert(yes(Z)) ;
       assert(no(Z)), fail).
	   		
interface2 :-
	jpl_new('javax.swing.JFrame', ['Expert System'], F),
	jpl_new('javax.swing.JLabel',['--- GUESS ANIMAL TYPE EXPERT SYSTEM ---'],LBL),
	jpl_new('javax.swing.JPanel',[],Pan),
	jpl_call(Pan,add,[LBL],_),
	jpl_call(F,add,[Pan],_),
	jpl_call(F, setLocation, [400,300], _),
	jpl_call(F, setSize, [400,300], _),
	jpl_call(F, setVisible, [@(true)], _),
	jpl_call(F, toFront, [], _),
	jpl_call('javax.swing.JOptionPane', showInputDialog, [F,'Hi. How are you? First of all tell me your name please'], N),
	jpl_call(F, dispose, [], _), 
	/*write(N),nl,*/
	(	N == @(null)
		->	write('you cancelled'),interface3('you cancelled. ','Thank you ','for use ','me.'),end,fail
		;	write("Hi. How are you? First of all tell me your name please : "),write(N),nl,pt(N)
	).
	
	
interface3(P,W1,D,W2) :-
	atom_concat(P,W1, A),
	atom_concat(A,D,B),
	atom_concat(B,W2,W3),
	jpl_new('javax.swing.JFrame', ['Expert System'], F),
	jpl_new('javax.swing.JLabel',['--- GUESS ANIMAL TYPE EXPERT SYSTEM ---'],LBL),
	jpl_new('javax.swing.JPanel',[],Pan),
	jpl_call(Pan,add,[LBL],_),
	jpl_call(F,add,[Pan],_),
	jpl_call(F, setLocation, [400,300], _),
	jpl_call(F, setSize, [400,300], _),
	jpl_call(F, setVisible, [@(true)], _),
	jpl_call(F, toFront, [], _),
	jpl_call('javax.swing.JOptionPane', showMessageDialog, [F,W3], N),
	jpl_call(F, dispose, [], _), 
	/*write(N),nl,*/
	(	N == @(void)
		->	write('')
		;	write("")
	).
	
help :- write("To start the expert system please type 'start.' and press Enter key").