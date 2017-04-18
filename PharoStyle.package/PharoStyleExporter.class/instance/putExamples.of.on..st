as yet unclassified
putExamples: examples of: aRule on: aStream
	examples
		do: [ :ex | 
			(ex hasPragmaNamed: #reExampleObject)
				ifTrue: [ self putObject: (aRule executeMethod: ex) of: aRule on: aStream ]
				ifFalse: [ self putNoObject: ex on: aStream ] ]