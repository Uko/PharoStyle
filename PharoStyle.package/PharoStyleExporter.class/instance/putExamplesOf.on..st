as yet unclassified
putExamplesOf: aRule on: aStream


	aRule class goodExamples ifNotEmpty: [ :examples |
		aStream lf; nextPutAll: '#### Good Exmaples'.
		self putExamples: examples on: aStream ].
	
	aRule class badExamples ifNotEmpty: [ :examples |
		aStream lf; nextPutAll: '#### Bad Exmaples'.
		self putExamples: examples on: aStream ].