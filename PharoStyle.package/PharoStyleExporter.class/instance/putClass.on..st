as yet unclassified
putClass: example on: aStream
	| allMethods |

	allMethods := example selectors.

	aStream
		lf;
		nextPutAll: '##### ' , example name; lf.
	example organization classComment isNotEmpty ifTrue: [ 
		aStream nextPutAll: '*' , example organization classComment , '*'; lf.
	].
	
	allMethods isNotEmpty ifTrue: [ 
		allMethods do: [ self putNoObject: example on: aStream ]
	]