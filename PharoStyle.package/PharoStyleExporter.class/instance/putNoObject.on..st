as yet unclassified
putNoObject: example on: aStream

	aStream
		lf;
		nextPutAll: '```'; lf;
		nextPutAll: example sourceCode; lf;
		nextPutAll: '```' 