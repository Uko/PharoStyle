as yet unclassified
putObject: example of: aRule on: aStream
	
	aRule class checksMethod
		ifTrue: [ self putNoObject: example on: aStream ]
		ifFalse: [ 
			aRule class checksClass ifTrue: [ self putClass: example on: aStream ] 
			]