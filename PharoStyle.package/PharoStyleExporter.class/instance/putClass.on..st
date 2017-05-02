as yet unclassified
putClass: example on: aStream
	| allMethods intents |

	intents := '&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;'.
	allMethods := example selectors.

	aStream lf;
		nextPutAll: example superclass name, example kindOfSubclass, example name.
	aStream nextPutAll: '<br/>', intents; nextPutAll: 'instanceVariableNames: '; 
		store: example instanceVariablesString.
	aStream nextPutAll: '<br/>', intents;  nextPutAll: 'classVariableNames: '; 
		store: example classVariablesString.
	aStream nextPutAll: '<br/>', intents; nextPutAll: 'poolDictionaries: '; 
		store: example sharedPoolsString.
	aStream nextPutAll: '<br/>', intents; nextPutAll: 'category: '; 
		store: example category asString; cr; cr.
	
	example organization classComment isNotEmpty ifTrue: [ 
		aStream nextPutAll: '"' , example organization classComment , '"'; lf.
	].
	
	allMethods isNotEmpty ifTrue: [ 
		allMethods do: [ self putNoObject: example on: aStream ]
	]