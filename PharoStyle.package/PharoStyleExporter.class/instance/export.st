as yet unclassified
export

	'style.md' asFileReference writeStreamDo: [ :str |
		str nextPutAll: '# Pharo Style Guidelines'; lf.
		((RBLintRule allSubclasses
			select: [ :r | r checksMethod and: [ r isVisible ] ] thenCollect: [ :r | r new ])
			groupedBy: [ :r | r group ])
				valuesDo: [ :group |
					str
						nextPutAll: '## ';
						nextPutAll: group anyOne group capitalized;
						nextPutAll: ' Rules';
						lf.
					group do: [ :r |
						str
							nextPutAll: '### ';
							nextPutAll: r name;
							lf;
							nextPutAll: r rationale;
							lf;lf.]
						]. ].