as yet unclassified
putExamplesOf: aRule on: aStream


	aRule class goodExamples ifNotEmpty: [ :examples |
		aStream lf;
		nextPutAll: '#### Good Examples'.
		self putExamples: examples of: aRule on: aStream ].
	
	aRule class badExamples ifNotEmpty: [ :examples |
		aStream lf;
		nextPutAll: '#### Bad Examples'.
		self putExamples: examples of: aRule on: aStream ].