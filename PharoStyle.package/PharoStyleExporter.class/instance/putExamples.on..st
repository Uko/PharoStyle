as yet unclassified
putExamples: examples on: aStream

	examples do: [ :ex |
		aStream
			lf;
			nextPutAll:  '```'; lf;
			nextPutAll: ex sourceCode; lf;
			nextPutAll: '```' ] 